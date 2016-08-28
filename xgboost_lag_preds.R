#creating lags for entire training data:
library(feather)
library(data.table)

train = fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
select = c('Semana','Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK', 'Demanda_uni_equil', 'Canal_ID'))
test = fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv", nrows = 1e6)

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)
keep_train = train
setkey(train, Producto_ID, Cliente_ID, Semana)

#compute lags on entire data:
lagged = train[,.(lag_1 = mean(Demanda_uni_equil)), by = .(Cliente_ID, Producto_ID, Semana)][,Semana := Semana + 1]
write_feather(lagged, "/Users/alexpapiu/Documents/Data/Bimbo/lagged.feather")


train = train[Semana >= 8] #you only use these in the model - 20 millions datapoints:
merge(lagged, train, all.y = TRUE, by= c("Cliente_ID", "Producto_ID", "Semana")) -> train

lagged[,Semana := Semana + 1][,lag_2 := lag_1][,lag_1 := NULL]
merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train

lagged[,Semana := Semana + 1][,lag_3 := lag_2][,lag_2 := NULL]
merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train

lagged[,Semana := Semana + 1][,lag_4 := lag_3][,lag_3 := NULL]
merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train

lagged[,Semana := Semana + 1][,lag_5 := lag_4][,lag_4 := NULL]
merge(lagged, train, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> train

train %>% filter(Cliente_ID == "146030", Producto_ID == "41")

#now more:

lagg = keep_train[,.(lag_1_1 = mean(Demanda_uni_equil)), by = .(Producto_ID, Semana)][,Semana := Semana + 1]
merge(lagg, train, all.y = TRUE, by= c("Producto_ID", "Semana")) -> train

lagg = keep_train[,.(lag_1_2 = mean(Demanda_uni_equil)), by = .(Cliente_ID, Semana)][,Semana := Semana + 1]
merge(lagg, train, all.y = TRUE, by= c("Cliente_ID", "Semana")) -> train

lagg = keep_train[,.(lag_1_3 = mean(Demanda_uni_equil)), by = .(Ruta_SAK, Semana)][,Semana := Semana + 1]
merge(lagg, train, all.y = TRUE, by= c("Ruta_SAK", "Semana")) -> train

lagg = train[,.(lag_1_4 = mean(Demanda_uni_equil)), by = .(Agencia_ID, Semana)][,Semana := Semana + 1]
merge(lagg, train, all.y = TRUE, by= c("Agencia_ID", "Semana")) -> train

write_feather(train, "/Users/alexpapiu/Documents/Data/Bimbo/lagged_train.feather")
train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged_train.feather"))


#~~~~~~~
#COUNTS:
train[, .(count_1 = .N), by = "Producto_ID"] %>% 
    merge(train, all.y = TRUE, by = "Producto_ID") -> train

train[, .(count_2 = .N), by = "Cliente_ID"] %>% 
    merge(train, all.y = TRUE, by = "Cliente_ID") -> train

train[, .(count_3 = .N), by = "Ruta_SAK"] %>% 
    merge(train, all.y = TRUE, by = "Ruta_SAK") -> train

train[, .(count_4 = .N), by = .(Cliente_ID, Producto_ID)] %>% 
    merge(train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> train

train[, .(count_5 = .N), by = "Agencia_ID"] %>% 
    merge(train, all.y = TRUE, by = "Agencia_ID") -> train

write_feather(train, "/Users/alexpapiu/Documents/Data/Bimbo/lagged_train_count.feather")


train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged_train_count.feather"))

#fixing NA's - we can do this all in one go:
#train[is.na(train)] = 0 #this says if you can't find the demand the previous week set it to 0


#adding product information:

fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv") %>%
    select(Producto_ID, brand, weight, pieces) -> products

merge(train, products, all.x = TRUE, by = "Producto_ID") -> train

#merge with prod_client means - computed elsewhere:
merge(train, mean_prod_clt, all.x = TRUE, by = c("Producto_ID", "Cliente_ID")) -> train


create_matrix = function(one_town) {
    
    X = cbind(one_town$Semana, #1
              #as.numeric(as.factor(one_town$Ruta_SAK)), #2
              #as.numeric(as.factor(one_town$Cliente_ID)), #3
              #as.numeric(as.factor(one_town$Producto_ID)), #4
              #as.numeric(as.factor(one_town$brand)), #5
              #as.numeric(as.factor(one_town$Ruta_SAK)), #2
              one_town$mean_client_prod,
              #one_town$Cliente_ID, #3
              #one_town$Agencia_ID,
              #one_town$Canal_ID,
              #one_town$Producto_ID, #4
              #one_town$Ruta_SAK, #2
              #as.numeric(as.factor(one_town$brand)),
              #one_town$weight, #6 
              #one_town$pieces, #8
              one_town$lag_1_1,
              #one_town$lag_1_2, #9
              one_town$lag_1_3, #10
              one_town$lag_1_4, #11
              one_town$lag_1, #12
              #one_town$lag_2, #13
              #one_town$lag_3, #14
              #one_town$lag_4, #15
              #one_town$lag_5, #16
              one_town$count_1, #17
              one_town$count_2, #18
              one_town$count_3, #19
              one_town$count_4, # 20
              one_town$count_5) #21
              #as.numeric(one_town$has_choco), #22
              #as.numeric(one_town$has_vanilla), #23
              #as.numeric(one_town$has_multigrain)) #24
    
    return(X)
}

X = create_matrix(train)

mask = X[,1] == 8

X_train = X[mask,]
X_val = X[-mask, ]
y_train = train$Demanda_uni_equil[mask]
y_val = train$Demanda_uni_equil[-mask]

library(xgboost)
set.seed(1231)
mask = sample(x = 1:dim(X_val)[1],size = 0.1*dim(X_val)[1])
X_val = X_val[mask,]
y_val = y_val[mask]

data = xgb.DMatrix(X_train, label = y_train, missing="NAN")
data_val = xgb.DMatrix(X_val, label = y_val, missing="NAN")


model = xgb.train(data = data,
                  nrounds = 50,
                  #booster = "gblinear",
                  watchlist = list(train = data, eval = data_val),
                  max_depth = 6,
                  eta = 0.1, #0.3 originally, but adding lags makes xgb learn faster.
                  verbose = 1,
                  #colsample_bytree = 0.75,
                  subsample = 0.75,
                  tree_method = "exact",
                  maximize = FALSE,
                  early.stop.round = 8)

xgb.importance(feature_names = as.character(1:24), model) -> importance
xgb.plot.importance(importance) #if you add mean_client_prod then it dominates by a LOT

#0.46077 with 80  trees 


#ideas to do on smaller dataset:
# - play with encodings of categorical feats
# - use other aggreagtes from week 3 - 7
# - look only at a few towns 
# - look at week 6 + 7?