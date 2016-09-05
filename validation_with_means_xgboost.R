library(dtplyr)
library(dplyr)
library(data.table)
library(feather)
library(xgboost)
library(Metrics)
library(Matrix)
library(splines)
library(tidyr)

train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.csv"))

train$id = 1:nrow(train)

train %>% select( - State, -Town) -> train


setkey(train, Producto_ID, Cliente_ID, Semana)

products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv") %>%
    select(Producto_ID,weight, pieces, has_choco, has_vanilla, has_multigrain)
merge(train, products, all.x = TRUE, by = "Producto_ID") -> train

#playing with lags and correlations:
#train[Semana <= 7][, .(mean_client_prod = mean(Demanda_uni_equil)), #lags
#                   by = .(Producto_ID, Cliente_ID, Semana)] %>% 
#    spread(key = Semana, value = mean_client_prod, fill = NA) -> lagz


#lagz %>% select(`3`:`7`) %>% as.matrix() -> lags

#apply(lags, 1, function(x) cov(1:5, x)/2.5) -> correlations

#lagz$slope = correlations

#lagz %>% 
#    select(Producto_ID, Cliente_ID, slope) %>% 
#    merge(train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> train

#train$Demanda_uni_equil = expm1(train$Demanda_uni_equil)

m = c(8, 7)

#m = 8


mask = !(train$Semana %in% c(m, 9))


train[mask][, .(mean_client_prod = mean(Demanda_uni_equil), #bloating this up..
                       count_client_prod = .N),
                       #sd_cp = sd(Demanda_uni_equil)),
                   by = .(Producto_ID, Cliente_ID)] %>% 
    merge(train[!mask], all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> rec_train


train[mask][, .(mean_prod = mean(Demanda_uni_equil),
                       count_prod = .N,
                       #q1_prod = quantile(Demanda_uni_equil, probs = 0.25),
                       #q3_prod = quantile(Demanda_uni_equil, probs = 0.75),
                       sd_prod = sd(Demanda_uni_equil)),
                   by = .(Producto_ID, Ruta_SAK)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Producto_ID", "Ruta_SAK")) -> rec_train



train[mask][, .(mean_prod_2 = mean(Demanda_uni_equil),
                       count_prod_2 = .N,
                       sd_prod_2 = sd(Demanda_uni_equil)),
                   by = .(Producto_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Producto_ID")) -> rec_train


train[mask][, .(mean_cliente = mean(Demanda_uni_equil),
                       count_cliente = .N),
                   by = .(Cliente_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Cliente_ID")) -> rec_train


train[mask][, .(mean_agencia = mean(Demanda_uni_equil),
                       count_agencia = .N),
                   by = .(Agencia_ID)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Agencia_ID")) -> rec_train


train[mask][, .(mean_ruta = mean(Demanda_uni_equil),
                       count_ruta = .N),
                   by = .(Ruta_SAK)] %>% 
    merge(rec_train, all.y = TRUE, by = c("Ruta_SAK")) -> rec_train


#rec_train %>% summary
#245888/1159354 #around 20% don't have the combo.

#rec_train %>% 
#    mutate(mean_client_prod = coalesce(mean_client_prod, mean_prod_2)) -> rec_train



val = rec_train[Semana == 9]
#train = rec_train[Semana == m]

train = rec_train[Semana == 7]



#xgboost:
#fitting a linear model on each week 3-7 demand doesn't work..
#adding lags 1-5 doesn't help, 
#adding town and country doesn't help
#adding NA's as 0 doesn't help
#integer encoding doesn't help.
#adding counts from val as well doesn't help
#using 2 weeks for xgboost doesnt help.
#using week n, n <8 then fitting on week n doesn't help - week 8 fit is best.
#however combinging 0.7*week_8 + 0.3*week_9 - plus 0.003
##ID's help very marrginally, weight and pieces -meh.


#~~~~~~~~~~~~~~~~~~~~~~~~~
train %>% select(-Demanda_uni_equil, - id) %>% as.matrix() -> X_train 
val %>% select(-Demanda_uni_equil, -id) %>% as.matrix() -> X_val 


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

#0.464873 with only a few feats
#maybe I should try to use this model as one of my picks?

xgb.importance(feature_names = colnames(X_train), model) %>% xgb.plot.importance()

#0.4480
preds_w_8 = predict(model, data_val)

val$preds = predict(model, X_val)
val$diff = (val$preds - val$Demanda_uni_equil)^2
#the model messes up when actual demand is 0

val %>% 
    sample_n(10000) %>% 
    ggplot(aes(x = Demanda_uni_equil, y = preds)) +
    geom_point(aes(size = diff, alpha = 0.4))

solution = data.frame(id = val$id, preds = preds_w_8)
rmse(preds_w_8, val$Demanda_uni_equil)


#0.448132



#filling the preds up for when we didnt have prod+client combo in semana 3-7 but we have it in 8:


train[Semana == 8][, .(mean_client_prod_now = mean(Demanda_uni_equil)),
                   by = .(Producto_ID, Cliente_ID)] %>% 
    merge(val, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> val


val = merge(val, solution, by = "id")

val$mean_client_prod_now + val$preds[1:10]
val$new_preds = val$preds

mask = !(is.na(val$mean_client_prod_now))
val$new_preds[mask] = 0.145*val$mean_client_prod_now[mask] + 0.85*val$preds[mask]

rmse(val$Demanda_uni_equil, val$new_preds)

#changing preds:
val$change_preds = !(is.na(val$mean_client_prod_now)) & is.na(val$mean_client_prod)

val %>% select(id, change_preds, mean_client_prod_now) %>% 
    merge(solution, by = "id") -> solution

val_score = data.frame(id = val$id, val$Demanda_uni_equil)


merge(val_score, solution, by = "id") -> solution

solution$preds[solution$change_preds == TRUE] = solution$mean_client_prod_now[solution$change_preds == TRUE]

solution %>% filter(change_preds == TRUE) -> sol_change


rmse(sol_change$val.Demanda_uni_equil, sol_change$preds)
#0.4519704
rmse(sol_change$val.Demanda_uni_equil, 0.3*sol_change$mean_client_prod_now + 0.7*sol_change$preds)
#0.436 if you add them 0.3/0.7 hmmm.


#linear models: 


preds = coalesce(val$mean_client_prod, val$mean_prod, val$mean_agencia, mean(train$Demanda_uni_equil))
rmse(val$Demanda_uni_equil, preds) #0.546179
#this loses about a 0.03 from skipping one week.

formula = Demanda_uni_equil ~ mean_agencia  +  ns(mean_cliente, 3) + mean_ruta + mean_prod + ns(mean_client_prod,3)

model = lm(formula, train)
preds = predict(model, val) 
preds = coalesce(preds, val$mean_prod, mean(train$Demanda_uni_equil))
rmse(val$Demanda_uni_equil, preds) #0.5034218