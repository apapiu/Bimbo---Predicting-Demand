library(feather)
library(data.table)
library(xgboost)
library(dplyr)

test = fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv")
lagged = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged.feather"))

merge(lagged, test, all.y = TRUE, by= c("Cliente_ID", "Producto_ID", "Semana")) -> test

lagged[,Semana := Semana + 1][,lag_2 := lag_1][,lag_1 := NULL]
merge(lagged, test, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> test

lagged[,Semana := Semana + 1][,lag_3 := lag_2][,lag_2 := NULL]
merge(lagged, test, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> test

lagged[,Semana := Semana + 1][,lag_4 := lag_3][,lag_3 := NULL]
merge(lagged, test, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> test

lagged[,Semana := Semana + 1][,lag_5 := lag_4][,lag_4 := NULL]
merge(lagged, test, all.y = TRUE, by = c("Cliente_ID", "Producto_ID", "Semana")) -> test



#add COUNTS::
train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged_train.feather"))

train[, .(count_1 = .N), by = "Producto_ID"] %>% 
    merge(test, all.y = TRUE, by = "Producto_ID") -> test

train[, .(count_2 = .N), by = "Cliente_ID"] %>% 
    merge(test, all.y = TRUE, by = "Cliente_ID") -> test

train[, .(count_3 = .N), by = "Ruta_SAK"] %>% 
    merge(test, all.y = TRUE, by = "Ruta_SAK") -> test

train[, .(count_4 = .N), by = .(Cliente_ID, Producto_ID)] %>% 
    merge(test, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> test

train[, .(count_5 = .N), by = "Agencia_ID"] %>% 
    merge(test, all.y = TRUE, by = "Agencia_ID") -> test

#fixing NA's:
test[is.na(test)] = 0


fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv") %>%
    select(Producto_ID, brand, weight, pieces) -> products

merge(test, products, all.x = TRUE, by = "Producto_ID") -> test

write_feather(test, "/Users/alexpapiu/Documents/Data/Bimbo/lagged_test.csv")


#ok we're ready to do the xgboost:
train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged_train_count.feather"))



merge(train, products, all.x = TRUE, by = "Producto_ID") -> train
train %>% select(-lag_1_1, -lag_1_2, -lag_1_3, -lag_1_4) -> train


train = train[Semana == 9]
test$Demanda_uni_equil = 0 #this is so we can rbind them for matrix creation
train$id = 0

data = rbind(train, test)
data[is.na(data)] = 0


create_matrix = function(one_town) {
    
    X = cbind(as.numeric(as.factor(one_town$Semana)), 
              as.numeric(as.factor(one_town$Ruta_SAK)), 
              as.numeric(as.factor(one_town$Cliente_ID)), 
              as.numeric(as.factor(one_town$Producto_ID)),
              as.numeric(as.factor(one_town$brand)),
              one_town$weight,
              one_town$pieces, #one_town$lag_1_1, #this nope
              #one_town$lag_1_2, #this helps
              #one_town$lag_1_3,
              #one_town$lag_1_4,
              one_town$lag_1,
              one_town$lag_2,
              one_town$lag_3,
              one_town$lag_4,
              one_town$lag_5,
              one_town$count_1,
              one_town$count_2,
              one_town$count_3,
              one_town$count_4,
              one_town$count_5,
              as.numeric(one_town$has_choco),
              as.numeric(one_town$has_vanilla),
              as.numeric(one_town$has_multigrain))
    
    return(X)
}
X = create_matrix(data)

X_train = X[1:nrow(train),]
X_test = X[-(1:nrow(train)),]
y_train = train$Demanda_uni_equil

dtrain = xgb.DMatrix(X_train, label = y_train, missing="NAN")


model = xgb.train(data = dtrain, #took 20 minutes.
                  nrounds = 3,
                  #booster = "gblinear",
                  max_depth = 10,
                  eta = 0.1, #0.3 originally, but adding lags makes xgb learn faster.
                  verbose = 0,
                  #tree_method = "exact",
                  metrics = "rmse",
                  colsample_bytree = 0.75,
                  subsample = 0.75)

preds = predict(model, X_test)
preds = expm1(preds)

solution = data.frame(id = test$id, Demanda_uni_equil = preds)
write.csv(solution, "/Users/alexpapiu/Documents/Data/Bimbo/xgboost_lag_preds_2.csv", row.names = FALSE)
