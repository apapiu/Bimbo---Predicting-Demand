#using xgboost directly on numeric features:

library(data.table)
library(dplyr)
library(feather)

#Read in the data:
train <- fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv",
               select = c('Semana','Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK', 'Demanda_uni_equil'))

test <- fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv")
town_state = fread("/Users/alexpapiu/Documents/Data/Bimbo/town_state.csv")    


#this to deal with the 
town_state$Town = sapply(strsplit(town_state$Town, " "), function(x) x[[1]])
train$Demanda_uni_equil = log1p(train$Demanda_uni_equil) #transform target variable since rmsle is the metric
setkey(train, Producto_ID, Town)

train %>% 
    merge(town_state, all.x = TRUE, by = "Agencia_ID") -> train

test %>% 
    merge(town_state, all.x = TRUE, by = "Agencia_ID") -> test

train[,.N, by = .(Town)] -> counts #same towns in test and train.

sm_train = train[Town %in% counts$Town[20:30]] #use this for validation.

#look at weeks 3 and 4 and aggregate by product:

prod_counts = train[Semana<= 4][,.(mean_prod = mean(Demanda_uni_equil)), by = .(Producto_ID)] 

prod_counts = train[Semana<= 4][,.(q1 = quantile(Demanda_uni_equil, 0.25),
                                   q2 = mean(Demanda_uni_equil),
                                   q3 = quantile(Demanda_uni_equil, 0.75)),
                                   by = .(Producto_ID)] 
write_feather(prod_counts, "/Users/alexpapiu/Documents/Data/Bimbo/prod_counts.feather")
#write_feather(sm_train, "/Users/alexpapiu/Documents/Data/Bimbo/val.csv")

merge(prod_counts, train, all.y = TRUE, by = "Producto_ID") -> train

#fixing some NA's:
total_mean = mean(train$Demanda_uni_equil)
quart_1 = quantile(train$Demanda_uni_equil, 0.25)
quart_3 = quantile(train$Demanda_uni_equil, 0.75)

train %>% 
    mutate(q1 = coalesce(q1, quart_1),
           q2 = coalesce(q2, total_mean),
           q3 = coalesce(q3, quart_3)) %>% 
    as.data.table() -> train

train = train[Semana > 4]

#looks like aggregating and taking means doesn't help too much...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this is the train with 20 towns or so:
train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.csv"))

products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv")

merge(train, products, all.x = TRUE, by = "Producto_ID") -> train


train %>% count(Town) -> town_counts
train %>% count(Agencia_ID) %>% arrange(n) -> agencia_counts


#the key is to split based on some geographical element - town seems like a nice middle-ground:
towns = town_counts$Town[1:3]
agencia = agencia_counts$Agencia_ID

town = towns[1]

sapply(towns, function(town) {
    
    train[Town == town] -> one_town
    #one_town[Agencia_ID != 1222] -> one_town

    #let's xgboost it:
    library(xgboost)
    
    X = cbind(as.numeric(as.factor(one_town$Agencia_ID)), 
              as.numeric(as.factor(one_town$Semana)), 
              as.numeric(as.factor(one_town$Ruta_SAK)), 
              as.numeric(as.factor(one_town$Cliente_ID)), 
              as.numeric(as.factor(one_town$Producto_ID)),
              as.numeric(as.factor(one_town$brand)),
              one_town$weight,
              one_town$nCliente_ID,
              one_town$nProducto_ID,
              one_town$Ruta_SAK,
              one_town$pieces,
              one_town$mean_prod,
              one_town$mean_client,
              as.numeric(one_town$has_choco),
              as.numeric(one_town$has_vanilla),
              as.numeric(one_town$has_multigrain))
    
    X[, 7][is.na(X[, 7])] = median(X[, 7], na.rm = TRUE)
    
    y = one_town$Demanda_uni_equil
    
    #use the 9th week as validation - it is encoded from 1 to 7 (not 3 - 9)
    semana = X[,2] < 7
    semana_val = X[,2] == 7
    
    X_train = X[semana,]
    X_val = X[semana_val,]
    
    data = xgb.DMatrix(X_train, label = y[semana], missing="NAN")
    data_val = xgb.DMatrix(X_val, label = y[semana_val], missing="NAN")
    
    
    model = xgb.train(data = data,
              nrounds = 100,
              watchlist = list(eval = data_val, train = data),
              max_depth = 10,
              eta = 0.5,
              verbose = 1,
              maximize = FALSE,
              early.stop.round = 8)
    
    #0.485293
    #0.478233 with 500 trees and 0.1 eta - you gain almost 0.01 from 
    
    #xgb.importance(feature_names = colnames(X), model = model)
    #importance: Producto, Agencia, Cliente, Ruta, Semana in this order.
    
    c(model$bestScore, model$bestInd)
}) -> scores

#so roughly +0.003 for using the mean CV.

#One issue is to pick the right number of trees: seems like lots of trees needed.

scores_df = data.frame(Agencia_ID = agencia, rmsle =  scores[1,], optimal_treee = scores[2,])
scores_df = merge(scores_df, agencia_counts, all.x = TRUE, by = "Agencia_ID")
sum(scores_df$rmsle*scores_df$n)/sum(scores_df$n)


scores_df = data.frame(Town = towns, rmsle =  scores[1,], optimal_treee = scores[2,])
scores_df = merge(scores_df, town_counts, all.x = TRUE, by = "Town")
sum(scores_df$rmsle*scores_df$n)/sum(scores_df$n)



#0.4708754 by agencia - 0.4728816 by town - hmm so agencia seems a bit better.
#but what if we put all agencia's under 100k in one group?
#we get 0.585657 - hmmm no real improvment here.
#what if we just do mean based on product/client on this data:

train[Semana <9][,.(mean = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID)] %>% 
    merge(one_town[Semana == 9], all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) %>% 
    mutate(mean = coalesce(mean, mean(train$Demanda_uni_equil))) -> one_temp

Metrics::rmse(one_temp$Demanda_uni_equil, one_temp$mean)
#0.6003551 using the client/product - so even for the bad ones xgboost deos better - 0.58.



#2087 - 0.460625
#2080 - 0.484351


#nrounds = 140