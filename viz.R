#visualize lags + linear models:

train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/lagged_train_count.feather"))

train %>% select(lag_1, lag_2, lag_3, lag_4, lag_5) %>% 
    filter( is.na(lag_1) == 0, is.na(lag_2) == 0, is.na(lag_3) == 0, is.na(lag_4) == 0, is.na(lag_5) == 0) -> business

business[130,]   %>%  as.numeric %>% plot


head(train)

sm_train = val %>% sample_n(30000)

plot(sm_train$lag_1, sm_train$Demanda_uni_equil, xlim = c(0, 8)) #really nice correlation.
plot(sm_train$lag_2, sm_train$Demanda_uni_equil, xlim = c(0, 8))
plot(sm_train$lag_3, sm_train$Demanda_uni_equil)

plot(sm_train$lag_1_1, sm_train$Demanda_uni_equil)
plot(sm_train$lag_1_2, sm_train$Demanda_uni_equil)

val = train[Semana == 9]
train = train[Semana != 9]


val %>% 
    mutate(lagz = coalesce(lag_1, lag_2, lag_3, lag_4, lag_5)) -> val

plot(sm_train$lagz, sm_train$Demanda_uni_equil)

sm_train %>% 
    ggplot(aes(x = lagz, y = Demanda_uni_equil)) + geom_smooth()


#checking the rmse for some lag:
mask = !(is.na(val$lagz))
rmse(actual = val$Demanda_uni_equil[mask], predicted = val$lagz[mask])

#lag_1 does best but is still 0.56 
#Question: how did the logmean script score 0.48 then?


#let's take mean by client/prod for weeks 3 - 8(or 7) and then use it to predict val:

train <- fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
               select = c('Cliente_ID', 'Producto_ID', 'Demanda_uni_equil', 'Semana'))

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)
setkey(train, Producto_ID, Cliente_ID)


train[Semana <= 7][, .(mean_client_prod = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID)] -> mean_prod_clt

mean_prod_clt %>% 
    merge(val, all.y = TRUE, by = c('Cliente_ID', 'Producto_ID')) -> val

mask = !(is.na(val$mean_client_prod))
rmse(actual = val$Demanda_uni_equil[mask], predicted = val$mean_client_prod[mask])

#0.4804987 - w/ Semana <= 8  much better than the lag!
#doing only week <= 7  0.4948772 - this is what week 11 would look like.
sm_train = val %>% sample_n(30000)

plot(sm_train$mean_client_prod, sm_train$Demanda_uni_equil, xlim = c(0, 8))


sm_train %>% 
    ggplot(aes(x =lag_3, y = lag_1)) + geom_bin2d() + geom_smooth(method = "lm") 


train[, .(mean_client_prod = mean(Demanda_uni_equil)), #bloating this up..
                            by = .(Producto_ID, Cliente_ID, Semana)] %>% 
    spread(key = Semana, value = mean_client_prod, fill = NA) -> weekly

