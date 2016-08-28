#instead of numeric use OHE - integer encoding is better.
library(Matrix)
library(xgboost)

towns = counts$Town[10:20]
town = towns[2]

train[Town == town] -> one_town

X = sparse.model.matrix( ~ Semana + 
                           as.factor(Ruta_SAK) + 
                           as.factor(Cliente_ID) +
                           as.factor(Producto_ID) - 1,
                         data = one_town)

y = one_town$Demanda_uni_equil

semana = X[,1] < 9
semana_val = X[, 1] == 9

X_train = X[semana,]
X_val = X[semana_val,]

data = xgb.DMatrix(X_train, label = y[semana])
data_val = xgb.DMatrix(X_val, label = y[semana_val])


model = xgb.train(data = data,
                  nrounds = 200,
                  watchlist = list(eval = data_val, train = data),
                  max_depth = 16,
                  #booster = "gblinear",
                  alpha = 0,
                  eta = 0.7,
                  verbose = 1,
                  maximize = FALSE,
                  early.stop.round = 4)

#so OHE seems to do worse than integer encoding!
#it does slightly better 0.486 for gblinear but for the tree its bad!