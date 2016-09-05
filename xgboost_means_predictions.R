library(dtplyr)

train = fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
              select = c('Semana','Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK', 'Demanda_uni_equil', 'Canal_ID'))
test = fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv")


train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)

setkey(train, Producto_ID, Cliente_ID, Semana)

train$id = 0
test$Demanda_uni_equil = 0


products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv") %>% select(Producto_ID,weight, pieces)
merge(train, products, all.x = TRUE, by = "Producto_ID") -> train
merge(test, products, all.x = TRUE, by = "Producto_ID") -> test

train$tst = 0
test$tst = 1
rec_train = rbind(train[Semana == 9], test) #this is what we will merge with.

n = 8


train[Semana <= n][, .(mean_client_prod = mean(Demanda_uni_equil), 
                          count_client_prod = .N),
                      by = .(Producto_ID, Cliente_ID, Agencia_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID")) -> rec_train


train[Semana <= n][, .(mean_prod = mean(Demanda_uni_equil),
                          count_prod = .N,
                          sd_prod = sd(Demanda_uni_equil)),
                      by = .(Producto_ID, Ruta_SAK)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Producto_ID", "Ruta_SAK")) -> rec_train


train[Semana <= n][, .(mean_prod_2 = mean(Demanda_uni_equil),
                          count_prod_2 = .N,
                          sd_prod_2 = sd(Demanda_uni_equil)),
                      by = .(Producto_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Producto_ID")) -> rec_train


train[Semana <= n][, .(mean_cliente = mean(Demanda_uni_equil),
                          count_cliente = .N),
                      by = .(Cliente_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Cliente_ID")) -> rec_train


train[Semana <= n][, .(mean_agencia = mean(Demanda_uni_equil), #not that important.
                          count_agencia = .N),
                      by = .(Agencia_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Agencia_ID")) -> rec_train


train[Semana <= n][, .(mean_ruta = mean(Demanda_uni_equil),
                          count_ruta = .N),
                      by = .(Ruta_SAK)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Ruta_SAK")) -> rec_train


write_feather(rec_train, "/Users/alexpapiu/Documents/Data/Bimbo/rec_train.feather")
rec_train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/rec_train.feather"))

rec_train = as.data.table(rec_train)

#rm(train)


X_train = rec_train[tst == 0] %>% select(- Demanda_uni_equil, -id, - tst)
X_test = rec_train[tst == 1] %>% select(- Demanda_uni_equil, -id, - tst)

y_train = rec_train$Demanda_uni_equil[rec_train$tst == 0] #this is not ideal since things get shuffled in the merges


set.seed(313)
wltst=sample(1:nrow(X_train),30000) 


    
dval = xgb.DMatrix(as.matrix(X_train[wltst,]), label = y_train[wltst])
dtrain = xgb.DMatrix(as.matrix(X_train[-wltst,]), label = y_train[-wltst])


watchlist = list(val=dval)

model = xgb.train(data = dtrain,
                  tree_method='exact',
                  nrounds = 200,
                  watchlist = watchlist,
                  max_depth = 9,
                  eta = 0.1)


preds = predict(model, as.matrix(X_test))
preds = expm1(preds)
preds[preds < 0] = 0

id = rec_train[tst == 1]$id

solution = data.frame(id = as.integer(id), Demanda_uni_equil = preds)
write.csv(solution, "/Users/alexpapiu/Documents/Data/Bimbo/xgboost_mean_preds_3.csv", row.names = FALSE)
