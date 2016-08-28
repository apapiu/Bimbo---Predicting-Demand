#xgboost numeric preds:

library(data.table)
library(dplyr)
library(feather)

#Read in the data:
train <- fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv",
               #select = c('Semana','Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK', 'Demanda_uni_equil')
               )

products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv")

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)

train[, .N, by = .(Agencia_ID)] -> agencia_counts
agencias = agencia_counts$Agencia_ID


#run this tonight::::

sapply(agencias[1:10], function(agencia) {
    train[Agencia_ID == agencia] -> one_town

    #let's xgboost it:
    library(xgboost)
    
    X = cbind(as.numeric(as.factor(one_town$Agencia_ID)), 
              as.numeric(as.factor(one_town$Semana)), 
              as.numeric(as.factor(one_town$Ruta_SAK)), 
              as.numeric(as.factor(one_town$Cliente_ID)), 
              as.numeric(as.factor(one_town$Producto_ID))
    
    y = one_town$Demanda_uni_equil
    
    #use the 9th week as validation - it is encoded from 1 to 7 (not 3 - 9)
    semana = X[,2] < 7
    semana_val = X[,2] == 7
    
    X_train = X[semana,]
    X_val = X[semana_val,]
    
    data = xgb.DMatrix(X_train, label = y[semana], missing="NAN")
    data_val = xgb.DMatrix(X_val, label = y[semana_val], missing="NAN")
    
    
    model = xgb.train(data = data,
                      nrounds = 200,
                      watchlist = list(eval = data_val, train = data),
                      max_depth = 10,
                      eta = 0.5,
                      verbose = 1,
                      maximize = FALSE,
                      early.stop.round = 8)
    
    #xgb.importance(feature_names = colnames(X), model = model)
    #importance: Producto, Agencia, Cliente, Ruta, Semana in this order.
    
    c(model$bestScore, model$bestInd)
}) -> scores

#let's just go over all agencias and see what hapens...
scores_df = data.frame(Agencia_ID = agencias[1:10], rmse = scores[1,],
                       rounds = scores[2,])
scores_df = merge(scores_df, agencia_counts, all.x = TRUE, by = "Agencia_ID")
sum(scores_df$rmse*scores_df$N)/sum(scores_df$N)
#you get around 0.47 on LB....blahhhh.


