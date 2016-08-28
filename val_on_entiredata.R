#building model on the entire dataset:

train = fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv")

agencias = train$Agencia_ID %>% unique

train = train[Agencia_ID %in% agencias[50:100]]


products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv")
merge(train, products, all.x = TRUE, by = "Producto_ID") -> train

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)



setkey(train, Producto_ID, Cliente_ID, Semana)
val = train[Semana == 9]
train = train[Semana != 9]

#run the stuff.

all_town = rbind(train, val)



create_matrix = function(one_town) {
    
    X = cbind(as.numeric(as.factor(one_town$Semana)), 
              as.numeric(as.factor(one_town$Ruta_SAK)), 
              #as.numeric(as.factor(one_town$Cliente_ID)), 
              as.numeric(as.factor(one_town$Producto_ID)),
              as.numeric(as.factor(one_town$brand)),
              one_town$weight,
              #one_town$Ruta_SAK,
              one_town$pieces, #one_town$lag_1_1, #this nope
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
              one_town$count_5,
              as.numeric(one_town$has_choco),
              as.numeric(one_town$has_vanilla),
              as.numeric(one_town$has_multigrain))
    
    return(X)
}
X = create_matrix(all_town)


X_train = X[1:nrow(train),]
X_val = X[-(1:nrow(train)), ]

y_train = train$Demanda_uni_equil
y_val = val$Demanda_uni_equil


data = xgb.DMatrix(X_train, label = y_train, missing="NAN")
data_val = xgb.DMatrix(X_val, label = y_val, missing="NAN")



model = xgb.train(data = data,
                  nrounds = 150,
                  #booster = "gblinear",
                  watchlist = list(train = data, eval = data_val),
                  max_depth = 10,
                  eta = 0.1, #0.3 originally, but adding lags makes xgb learn faster.
                  verbose = 1,
                  colsample_bytree = 0.75,
                  subsample = 0.75,
                  maximize = FALSE,
                  early.stop.round = 8)

#0.457645 on validation set with NA's left untouched.  