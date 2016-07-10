library(data.table)
library(dplyr)
library(ggplot2)
library(Metrics)

n = 15e6

train = fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
              #select = c("Semana", "Cliente_ID", "Producto_ID", "Demanda_uni_equil"),
              nrow = n)

prod_id = fread("/Users/alexpapiu/Documents/Data/Bimbo/producto_tabla.csv")
client_id = fread("/Users/alexpapiu/Documents/Data/Bimbo/cliente_tabla.csv")
town_state = fread("/Users/alexpapiu/Documents/Data/Bimbo/town_state.csv")    

joins = function(train) {
    train %>% 
        left_join(town_state) %>% 
        #left_join(client_id) %>% there are a few client_id's that are doubles.
        left_join(prod_id) -> train
    return(train) 
}

train$id = 1:nrow(train)

val = train[train$Semana == 4]
train = train[train$Semana %in% c(3)]

str_count(train$NombreProducto[1:100], "p")



n = 200
train %>% count(Producto_ID) %>% arrange(desc(n)) -> products
top_products = products$Producto_ID[1:n]
top_products = as.character(top_products)

clean = function(train, top_products = top_products) {
    
    train %>% 
        mutate(Agencia_ID = as.character(Agencia_ID),
               Canal_ID = as.character(Canal_ID),
               Producto_ID = as.character(Producto_ID),
               Ruta_SAK = as.character(Ruta_SAK),
               Cliente_ID = as.character(Cliente_ID)) -> train
    
    #set lesser used products as generic
    train$Producto_ID[!(train$Producto_ID %in% top_products)] = "generic"
    return(train)
}

train = clean(train)
val = clean(val)


create_matrix = function(train) {
    X = model.matrix( ~ Canal_ID + Producto_ID, data = train)[-1,]
    return(X)
}

X = create_matrix(train)



#~~~~~~~~~~~~
train$Demanda_uni_equil = as.numeric(train$Demanda_uni_equil)
setkey(train, Cliente_ID, Producto_ID)

prod_client_id = train[,.(median = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID, Producto_ID)] 
product_id = train[,.(median = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID)]

new_val = left_join(val, prod_client_id, by = c("Cliente_ID", "Producto_ID"))
new_val = left_join(new_val, product_id, by = "Producto_ID")

preds = new_val$median.x
preds[is.na(preds)] = new_val$median.y[is.na(preds)]
preds[is.na(preds)] = median(train$Demanda_uni_equil)

new_val$preds = preds

rmsle(new_val$preds, new_val$Demanda_uni_equil)
 
y = new_val$Demanda_uni_equil

rmsle(y, preds)
# 0.5451922 with semana 7,8 predicting semana 9 (1 million ample) - TO BEAT
#0.551299 with semana 3 predicting semana 4
#0.6 using ony 10 mils as the train data - I think client_ID needs a lot of data to work.
#0.5131832 on semana 3:8 predicting semana 9

rmsle(y[1:10000], preds[1:10000])

errors =sapply(1:1e4, function(i)msle(preds[i], y[i]))
sqrt(mean(errors))

results = data.frame(real = y[1:1e4], pred = preds[1:1e4], errors)
#what screws you over is when you predict 1 and it's 8 or vice versa
#not so much predicting 150 when it's 300


condition_1 = preds %% 10 == 1 & preds > 10
#condition_2 = preds %% 10 == 9 & preds > 1
preds[condition_1] = preds[condition_1] - 1
#preds[condition_2] = preds[condition_2] + 1
rmsle(y, preds)

hist(preds[preds < 100])

rmsle(ceiling(preds),new_val$Demanda_uni_equil )


#~~~~~~~~~~~~~~~~~~~
#looking at different single features versus rmsle:
#~~~~~~~~~~~~~~~~~~~
sapply(names(train), eval_feat, USE.NAMES = TRUE) -> feature_scores
#Semana      Agencia_ID Canal_ID    Ruta_SAK  Cliente_ID Producto_ID 
#0.8400049   0.7813288   0.7861610   0.7834331   0.7427504   0.6622240


#~~~~~~~~~~~~
#trying to use the mean on a log transformed target:
#~~~~~~~~~~~~~
train$new <- log(1+ train$Demanda_uni_equil)
val$new <- log(1 + val$Demanda_uni_equil)

prod_client_id = train[,.(median = mean(new)), by = .(Cliente_ID, Producto_ID)] 
product_id = train[,.(median = mean(new)), by = .(Producto_ID)]

new_val = left_join(val, prod_client_id, by = c("Cliente_ID", "Producto_ID"))
new_val = left_join(new_val, product_id, by = "Producto_ID")


preds_2 = new_val$median.x
preds_2[is.na(preds_2)] = new_val$median.y[is.na(preds_2)]
preds_2[is.na(preds_2)] = mean(train$new)

preds_2 = exp(preds_2) - 1
rmsle(preds_2, y) 
#0.5548306 - worse than using median

