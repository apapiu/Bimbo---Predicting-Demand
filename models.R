#can we get other models going?

library(data.table)
library(dplyr)
library(Metrics)
library(feather)

#the idea is to use medians/standard deviations/counts as features in a new model:

#TO DO: split the training set into two parts randomly: first part to do the counts
#and the second to join the counts and do the models - you don't need that much data for this -
#roughly around 200k examples seem to be doing quite well.

#ALSO: add more features.

source("/Users/alexpapiu/Documents/Kaggle/Bimbo/func.R")
train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/sm_train.feather"))
val = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.feather"))

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil)
val$Demanda_uni_equil = log1p(val$Demanda_uni_equil)

setkey(train, Producto_ID, Cliente_ID)



#create two train sets:
set.seed(1321)
sam = sample(1:nrow(train), 1e6)
train_xgb = train[sam]
tain = train[-sam]

#counting:
mean_total = mean(train$Demanda_uni_equil)

counts_client = train[,.(client_mean = mean(Demanda_uni_equil), 
                         client_sd = sd(Demanda_uni_equil), #sd slows things a bit
                         client_count = .N),
                      by = .(Cliente_ID)]

counts_product = train[,.(product_mean = mean(Demanda_uni_equil), 
                          product_sd = sd(Demanda_uni_equil),
                          product_count = .N),
                       by = .(Producto_ID)]

counts_ruta = train[,.(ruta_mean = mean(Demanda_uni_equil), 
                       ruta_sd = sd(Demanda_uni_equil),
                       ruta_count = .N),
                    by = .(Ruta_SAK)]

counts_agencia = train[,.(ag_mean = mean(Demanda_uni_equil), 
                          ag_sd = sd(Demanda_uni_equil),
                          ag_count = .N),
                       by = .(Agencia_ID)]



#counts_client = count_by_level(train, "Cliente_ID")
#counts_product = count_by_level(train, "Producto_ID")

#X = cbind(join_by_count(train, counts_client),
#          join_by_count(train, counts_product))
#X_val = cbind(join_by_count(val, counts_client),
#              join_by_count(val, counts_product))


#now merge the counts from train with train_xgb - that way no data leakcage:
add_counts = function(train) { #this train should be not used for counts.
    train %>% 
        merge(counts_product, all.x = TRUE, by = "Producto_ID") %>% 
        merge(counts_client, all.x = TRUE, by = "Cliente_ID") %>% 
        merge(counts_ruta, all.x = TRUE, by = "Ruta_SAK") %>% 
        #merge(counts_client_ruta, all.x = TRUE, by = c("Producto_ID", "Cliente_ID")) %>% 
        merge(counts_agencia, all.x = TRUE, by = "Agencia_ID")-> merged
    
    #cleaning NA's:
    merged %>% 
        mutate(product_mean = coalesce(product_mean, mean_total),
               client_mean = coalesce(client_mean, mean_total),
               ruta_mean = coalesce(ruta_mean, mean_total),
               ag_mean = coalesce(ag_mean, mean_total)) -> merged
    
    return(merged)
}

merged = add_counts(train_xgb)
merged_val = add_counts(val)

#~~~~~~~~~
#xgboost:
#~~~~~~~~

#There is overfitting going on here I think.

library(xgboost)

create_xgboost_matrix = function(merged) {
    #merged %>% select(has_choco:ag_count) %>% as.matrix() -> X #selects columns
    merged %>% select(product_mean:ag_count) %>% as.matrix() -> X #selects columns
    X[is.na(X)] = 0 #this sets counts and sd to zero - which makes sense
    return(X) 
}

X = create_xgboost_matrix(merged)
X_val = create_xgboost_matrix(merged_val)

y = merged$Demanda_uni_equil
y_val = merged_val$Demanda_uni_equil

#making a smaller val set:
set.seed(3123)
sam = sample(1:dim(X)[1], 2e5)
X_small = X[sam,]
y_small = y[sam]

#trying to add new features - numeric instead of factors
X = cbind(X, weight = merged$weight)
X_val = cbind(X_val, weight = merged_val$weight)

X[,"weight"][is.na(X[,"weight"])] = 0
X_val[,"weight"][is.na(X_val[,"weight"])] = 0


data_xgb = xgb.DMatrix(data = X_small, label = y_small) #setting missing here=NaN here leads to bad rez
data_xgb_val = xgb.DMatrix(X_val, label = y_val)


model_xgb = xgb.train(data  = data_xgb,
                      watchlist = list(valdation = data_xgb_val, train = data_xgb),
                      eta = 0.3,
                      max_depth = 6,
                      nrounds = 50)

#0.547793 with 200k points
#0.537970 with 1 million points, max_depth = 8

#~~~~~~~~~~~~~~~
#Linear models:
#~~~~~~~~~~~~~~

model = lm(log1p(Demanda_uni_equil) ~
               median_Cliente_ID +
               median_Producto_ID +
               sd_Producto_ID, data = X)

preds = predict(model, X_val)
preds[preds < 0] = 0

rmsle(actual = y_val,predicted =  preds)

rmse(log1p(y_val), preds)


