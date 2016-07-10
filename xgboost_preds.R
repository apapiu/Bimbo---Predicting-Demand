#xgboost preds:

library(data.table)
library(dplyr)

#Read in the data:
train <- fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv",
               select = c('Semana','Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK', 'Demanda_uni_equil'))
test <- fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv", 
              select = c('id', 'Producto_ID','Agencia_ID', 'Cliente_ID', 'Ruta_SAK'))

train$Demanda_uni_equil = log1p(train$Demanda_uni_equil) #transform target variable since rmsle is the metric


#splitting it up - but how?
mask = (nrow(train) - 1e6):nrow(train)
train_xgb = train[mask] #use the last 1 million rows for xgboost.
train = train[-mask] #use this for computing medians.

#computing mean, sd and count:
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


#merging:
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
merged_test = add_counts(test)

#xgboost:
library(xgboost)

create_xgboost_matrix = function(merged) {
    merged %>% select(product_mean:ag_count) %>% as.matrix() -> X #selects columns
    X[is.na(X)] = 0 #this sets counts and sd to zero if they are NAN - which makes sense
    return(X) 
}

X = create_xgboost_matrix(merged)
X_test = create_xgboost_matrix(merged_test)

y = merged$Demanda_uni_equil

data_xgb = xgb.DMatrix(data = X, label = y) #setting missing here=NaN here leads to bad rez
model_xgb = xgboost(data  = data_xgb,
                    eta = 0.3,
                    max_depth = 6,
                    nrounds = 50)

preds = predict(model_xgb, X_test)

#MAKE sure to transform it back to original scale:
preds = expm1(preds)

#the distribution is too smooth - averaging with the other mean preds would make sense.

submission = data.frame(id = merged_test$id, Demanda_uni_equil = preds)
write.csv(submission, "/Users/alexpapiu/Documents/Data/Bimbo/xgboost_base.csv", row.names = FALSE)


