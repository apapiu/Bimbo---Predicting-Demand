#means_xgboost as blending:

library(dtplyr)
library(dplyr)
library(data.table)
library(feather)
library(xgboost)
library(Metrics)
library(Matrix)
library(splines)
library(tidyr)
library(ggplot2)

train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.csv"))

#first model:
#use mean by Cliente+Prod:

val = train[Semana == 9]


train[Semana <9][,.(mean_prod_client = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID, Agencia_ID)] %>% 
    merge(val, all.y =TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID")) -> val
train[Semana <9][,.(mean_prod = mean(Demanda_uni_equil)), by = .(Producto_ID, Ruta_SAK)] %>% 
    merge(val, all.y =TRUE, by = c("Producto_ID", "Ruta_SAK")) -> val
train[Semana <9][,.(mean_client = mean(Demanda_uni_equil)), by = .(Cliente_ID)] %>% 
    merge(val, all.y =TRUE, by = c("Cliente_ID")) -> val
#train[Semana <9][,.(mean_prod_2 = mean(Demanda_uni_equil)), by = .(Producto_ID)] %>% 
#    merge(val, all.y =TRUE, by = c("Producto_ID")) -> val


val %>% select(mean_client, mean_prod, mean_prod_client) %>% 
    apply(MARGIN = 1, function(x) weighted.mean(x, w = c(1,3,13), na.rm = TRUE)) -> val$preds

val$preds = coalesce(val$preds, mean(train$Demanda_uni_equil))

rmse(val$preds, val$Demanda_uni_equil) #0.4755387 with weighted mean, up to 0.5105819 ignoring week = 8
rmse(coalesce(val$mean_prod_client,val$mean_prod, val$mean_client,mean(train$Demanda_uni_equil)),
     val$Demanda_uni_equil) #0.4976893 up to 0.5455037 ignoring week = 8


#using xgboost:
rec_train = train[Semana >= 8]

train[Semana <7][,.(mean_prod_client = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID, Agencia_ID)] %>% 
    merge(rec_train, all.y =TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID")) -> rec_train
train[Semana <7][,.(mean_prod = mean(Demanda_uni_equil)), by = .(Producto_ID, Ruta_SAK)] %>% 
    merge(rec_train, all.y =TRUE, by = c("Producto_ID", "Ruta_SAK")) -> rec_train
train[Semana <7][,.(mean_client = mean(Demanda_uni_equil)), by = .(Cliente_ID)] %>% 
    merge(rec_train, all.y =TRUE, by = c("Cliente_ID")) -> rec_train
train[Semana <7][,.(mean_prod_2 = mean(Demanda_uni_equil)), by = .(Producto_ID, Ruta_SAK)] %>% 
    merge(rec_train, all.y =TRUE, by = c("Producto_ID", "Ruta_SAK")) -> rec_train


val = rec_train[Semana == 9]
train = rec_train[Semana == 8]

train %>% select(mean_prod, mean_client, mean_prod_client) %>% as.matrix() -> X_train 
val %>% select(mean_prod, mean_client, mean_prod_client) %>% as.matrix() -> X_val 


y_train = train$Demanda_uni_equil
y_val = val$Demanda_uni_equil

data = xgb.DMatrix(X_train, label = y_train, missing="NAN")
data_val = xgb.DMatrix(X_val, label = y_val, missing="NAN")


model = xgb.train(data = data,
                  nrounds = 100,
                  watchlist = list(train = data, eval = data_val),
                  max_depth = 9,
                  #booster = "gblinear",
                  eta = 0.3, #0.3
                  #tree_method='approx',
                  gamma = 0,
                  verbose = 1,
                  #subsample = 0.7,
                  #colsample_bytree = 0.7, #- this trips it up!
                  maximize = FALSE,
                  early.stop.round = 8)
#with xgboost: 0.467907 - decreasing to 0.485670 with ignoring week 7.

preds_xgb = predict(model, data_val)
rmse(preds_xgb, val$Demanda_uni_equil)

rmse(0.7*preds_xgb + 0.3*val$preds, val$Demanda_uni_equil) #this gets 0.478670 with weighted bagging
