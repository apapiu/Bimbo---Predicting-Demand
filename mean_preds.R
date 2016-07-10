library(data.table)
library(dplyr)

#Read in the data:
train <- fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
               select = c('Cliente_ID', 'Agencia_ID' 'Producto_ID', 'Demanda_uni_equil'))
test <- fread("/Users/alexpapiu/Documents/Data/Bimbo/test.csv", 
              select = c('id', 'Producto_ID','Agencia_ID', 'Cliente_ID'))

train$log_demand = log1p(train$Demanda_uni_equil) #transform target variable since rmsle is the metric

#set a table key to enable fast aggregations
setkey(train, Producto_ID, Cliente_ID)
setkey(test, Producto_ID, Cliente_ID)

mean_total <- mean(train$log_demand) #overall mean
mean_Prod <- train[, .(mean_prod = mean(log_demand)),
                   by = .(Producto_ID, Agencia_ID)] #mean by product
mean_Client_Prod <- train[, .(mean_client_prod = mean(log_demand)),
                          by = .(Producto_ID, Cliente_ID)] #mean by product and client

#merge test set with aggregate means:
submit <- merge(test, mean_Client_Prod, all.x = TRUE)
submit <- merge(submit, mean_Prod, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))

#use cross_feature if available, if not use mean product, if not use overall mean.
preds = coalesce(submit$mean_client_prod,
                 submit$mean_prod,
                 mean_total)

preds = expm1(preds) #transform the preds back.

submission = data.frame(id = submit$id, Demanda_uni_equil = preds)
fwrite(submission, "/Users/alexpapiu/Documents/Data/Bimbo/cross_feat.csv")

#submission in under 1 minute.

write.csv(ro)