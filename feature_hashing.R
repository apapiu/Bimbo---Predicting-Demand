#using feature hasing:
library(dplyr)
library(FeatureHashing)
library(Matrix)

one_town %>% 
        mutate(Producto_ID = as.factor(Producto_ID),
               Agencia_ID = as.factor(Agencia_ID),
               Ruta_SAK = as.factor(Ruta_SAK),
               Cliente_ID = Cliente_ID) -> one_town

y = one_town$Demanda_uni_equil

one_town %>% select(-Demanda_uni_equil, - product_name, -V1, - State, - Town) -> one_townz

one_townz[is.na(one_townz)] = ""

sapply(one_townz, function(x) length(unique(x)))

X = sparse.model.matrix( ~.,  data = one_townz)

X = hashed.model.matrix(~., data = one_townz, 2^16)


#use the 9th week as validation - it is encoded from 1 to 7 (not 3 - 9)
semana = X[,2] < 7
semana_val = X[,2] == 7

semana = one_town$Semana < 7
semana_val = one_town$Semana == 7

X_train = X[semana,]
X_val = X[semana_val,]

data = xgb.DMatrix(X_train, label = y[semana])
data_val = xgb.DMatrix(X_val, label = y[semana_val])


model = xgb.train(data = data,
                  nrounds = 200,
                  watchlist = list(eval = data_val, train = data),
                  max_depth = 10,
                  booster = "gblinear",
                  eta = 0.1,
                  verbose = 1,
                  maximize = FALSE,
                  early.stop.round = 8)

#gblinear doesn't work here :(