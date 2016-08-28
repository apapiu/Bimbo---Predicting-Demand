#separate validation set:
library(dtplyr)
library(dplyr)
library(data.table)
library(feather)
library(xgboost)

train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.csv"))
train$id = 1:nrow(train)

products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv")
merge(train, products, all.x = TRUE, by = "Producto_ID") -> train

setkey(train, Producto_ID, Cliente_ID, Semana)
val = train[Semana == 9]
train = train[Semana != 9]

#adding some lagged feats:
lagged = train[,.(lag_1 = mean(Demanda_uni_equil)), by = .(Cliente_ID, Producto_ID, Semana)][,Semana := Semana + 1]


merge(lagged, train, all.y = TRUE, by= c("Cliente_ID", "Producto_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> val

lagged[,Semana := Semana + 1][,lag_2 := lag_1][,lag_1 := NULL]

merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> val


lagged[,Semana := Semana + 1][,lag_3 := lag_2][,lag_2 := NULL]

merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> val


lagged[,Semana := Semana + 1][,lag_4 := lag_3][,lag_3 := NULL]

merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> val


lagged[,Semana := Semana + 1][,lag_5 := lag_4][,lag_4 := NULL]

merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> val


#adding more for lag 1 - product ID:


lagged = train[,.(lag_1_2 = mean(Demanda_uni_equil)), by = .(Cliente_ID, Semana)][,Semana := Semana + 1]

merge(lagged, train, all.y = TRUE, by= c("Cliente_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Cliente_ID", "Semana")) -> val


lagged = train[,.(lag_1_3 = mean(Demanda_uni_equil)), by = .(Ruta_SAK, Semana)][,Semana := Semana + 1]

merge(lagged, train, all.y = TRUE, by= c("Ruta_SAK", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Ruta_SAK", "Semana")) -> val


lagged = train[,.(lag_1_4 = mean(Demanda_uni_equil)), by = .(Agencia_ID, Semana)][,Semana := Semana + 1]

merge(lagged, train, all.y = TRUE, by= c("Agencia_ID", "Semana")) -> train
merge(lagged, val, all.y = TRUE, by = c("Agencia_ID", "Semana")) -> val


#train$lag_1_3 = NULL
#val$lag_1_3 = NULL


train %>% 
    mutate(lag_1_2 = coalesce(lag_1_2, 0),
           lag_1_3 = coalesce(lag_1_3, 0),
           lag_1_4 = coalesce(lag_1_4, 0),
           lag_1 = coalesce(lag_1, 0),
           lag_2 = coalesce(lag_2, 0),
           lag_3 = coalesce(lag_3, 0),
           lag_4 = coalesce(lag_4, 0),
           lag_5 = coalesce(lag_5, 0)) -> train



val %>% 
    mutate(lag_1_2 = coalesce(lag_1_2, 0),
           lag_1_3 = coalesce(lag_1_3, 0),
           lag_1_4 = coalesce(lag_1_4, 0),
           lag_1 = coalesce(lag_1, 0),
           lag_2 = coalesce(lag_2, 0),
           lag_3 = coalesce(lag_3, 0),
           lag_4 = coalesce(lag_4, 0),
           lag_5 = coalesce(lag_5, 0)) -> val

#~~~~~~~~~~~~~~~~~~
#adding the count - this helps, and it suffices to do it on last week of data.
train = train[Semana == 8]


train[, .(count_1 = .N), by = "Producto_ID"] %>% 
    merge(train, all.y = TRUE, by = "Producto_ID") -> train

train[, .(count_1 = .N), by = "Producto_ID"] %>% 
    merge(val, all.y = TRUE, by = "Producto_ID") -> val


train[, .(count_2 = .N), by = "Cliente_ID"] %>% 
    merge(train, all.y = TRUE, by = "Cliente_ID") -> train

train[, .(count_2 = .N), by = "Cliente_ID"] %>% 
    merge(val, all.y = TRUE, by = "Cliente_ID") -> val


train[, .(count_3 = .N), by = "Ruta_SAK"] %>% 
    merge(train, all.y = TRUE, by = "Ruta_SAK") -> train

train[, .(count_3 = .N), by = "Ruta_SAK"] %>% 
    merge(val, all.y = TRUE, by = "Ruta_SAK") -> val



train[, .(count_4 = .N), by = .(Cliente_ID, Producto_ID)] %>% 
    merge(train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> train

train[, .(count_4 = .N), by = .(Cliente_ID, Producto_ID)] %>% 
    merge(val, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> val



train[, .(count_5 = .N), by = "Agencia_ID"] %>% 
    merge(train, all.y = TRUE, by = "Agencia_ID") -> train

train[, .(count_5 = .N), by = "Agencia_ID"] %>% 
    merge(val, all.y = TRUE, by = "Agencia_ID") -> val


train %>% 
    mutate(count_1 = coalesce(as.numeric(count_1), 0),
           count_2 = coalesce(as.numeric(count_2), 0),
           count_3 = coalesce(as.numeric(count_3), 0),
           count_4 = coalesce(as.numeric(count_4), 0)) -> train


val %>% 
    mutate(count_1 = coalesce(as.numeric(count_1), 0),
           count_2 = coalesce(as.numeric(count_2), 0),
           count_3 = coalesce(as.numeric(count_3), 0),
           count_4 = coalesce(as.numeric(count_4), 0)) -> val

#train = train[Semana == 8]

keep_train = train
train = keep_train

#train[Agencia_ID == "1220"] -> one_town
#val[Agencia_ID == "1220"] -> one_val

train = train[Semana >= 8] #SO USING ONLY LAST WEEK HERE MAKES SENSE!

all_town = rbind(train, val)

#all_town = rbind(one_town, one_val)

create_matrix = function(one_town) {
    
    X = cbind(one_town$Semana, #1
              as.numeric(as.factor(one_town$Ruta_SAK)), #2
              #as.numeric(as.factor(one_town$Cliente_ID)), #3
              as.numeric(as.factor(one_town$Producto_ID)), #4
              as.numeric(as.factor(one_town$brand)), #5
              one_town$weight, #6 
              #one_town$Ruta_SAK, #7
              one_town$pieces, #8
              one_town$lag_1_2, #9
              one_town$lag_1_3, #10
              one_town$lag_1_4, #11
              one_town$lag_1, #12
              one_town$lag_2, #13
              one_town$lag_3, #14
              one_town$lag_4, #15
              one_town$lag_5, #16
              one_town$count_1, #17
              one_town$count_2, #18
              one_town$count_3, #19
              one_town$count_4, # 20
              one_town$count_5, #21
              as.numeric(one_town$has_choco), #22
              as.numeric(one_town$has_vanilla), #23
              as.numeric(one_town$has_multigrain)) #24
    
    return(X)
}

X = create_matrix(all_town)

X_train = X[1:nrow(train),]
X_val = X[-(1:nrow(train)), ]
y_train = train$Demanda_uni_equil
y_val = val$Demanda_uni_equil


#X_train = X[1:nrow(one_town),]
#X_val = X[-(1:nrow(one_town)), ]
#y_train = one_town$Demanda_uni_equil
#y_val = one_val$Demanda_uni_equil


data = xgb.DMatrix(X_train, label = y_train, missing="NAN")
data_val = xgb.DMatrix(X_val, label = y_val, missing="NAN")


model = xgb.train(data = data,
                  nrounds = 150,
                  #booster = "gblinear",
                  watchlist = list(train = data, eval = data_val),
                  max_depth = 8,
                  eta = 0.1, #0.3 originally, but adding lags makes xgb learn faster.
                  verbose = 1,
                  colsample_bytree = 0.75,
                  subsample = 0.75,
                  maximize = FALSE,
                  early.stop.round = 8)

xgb.importance(feature_names = colnames(X_train), model) %>% xgb.plot.importance()


preds = predict(model, X_val)


#0.453 using only week 8 - better and faster.

#0.454323 with all of train - around 386k elements - phew....
#0.4631 with one_town

xgb.importance(feature_names = as.character(1:23), model) -> importance
#lag_1, weight, count_1, count_2, lag_2, lag_3, , peices, Producto_ID,

xgb.plot.importance(importance)

towns = unique(train$Town)
#
sapply(towns, function(town) {
    
    train[Town == town] -> one_town
    val[Town == town] -> one_val
    
    
    all_town = rbind(one_town, one_val)
    
    create_matrix = function(one_town) {
        
        X = cbind(as.numeric(as.factor(one_town$Semana)), 
                  as.numeric(as.factor(one_town$Ruta_SAK)), 
                  as.numeric(as.factor(one_town$Cliente_ID)), 
                  as.numeric(as.factor(one_town$Producto_ID)),
                  as.numeric(as.factor(one_town$brand)),
                  one_town$weight,
                  one_town$Ruta_SAK,
                  one_town$pieces,
                  #one_town$lag_1_1, #this nope
                  one_town$lag_1_2, #this helps
                  one_town$lag_1_3,
                  one_town$lag_1_4,
                  one_town$lag_1,
                  one_town$lag_2,
                  one_town$lag_3,
                  one_town$lag_4,
                  one_town$lag_5,
                  one_town$count_1,
                  one_town$count_2,
                  one_town$count_3,
                  one_town$count_4,
                  as.numeric(one_town$has_choco),
                  as.numeric(one_town$has_vanilla),
                  as.numeric(one_town$has_multigrain))
        
        return(X)
    }
    
    X = create_matrix(all_town)
    
    
    
    X_train = X[1:nrow(one_town),]
    X_val = X[-(1:nrow(one_town)), ]
    y_train = one_town$Demanda_uni_equil
    y_val = one_val$Demanda_uni_equil
    
    
    
    data = xgb.DMatrix(X_train, label = y_train, missing="NAN", weights = train$Semana)
    data_val = xgb.DMatrix(X_val, label = y_val, missing="NAN", weights = val$Semana)
    
    
    model = xgb.train(data = data,
                      nrounds = 120,
                      watchlist = list(train = data, eval = data_val),
                      max_depth = 10,
                      eta = 0.1, #0.3 originally, but adding lags makes xgb learn faster.
                      verbose = 1,
                      #colsample_bytree = 0.7,
                      maximize = FALSE,
                      early.stop.round = 8,
                      tree_method = 'exact')
    

    
    c(model$best_score, model$best_iteration)
}) -> scores

town_count = count(train, Town)

scores_df = data.frame(Town = towns, rmsle =  scores[1,], optimal_treee = scores[2,])
scores_df = merge(scores_df, town_count, all.x = TRUE, by = "Town")
sum(scores_df$rmsle*scores_df$n)/sum(scores_df$n)


