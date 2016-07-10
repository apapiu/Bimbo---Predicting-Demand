library(data.table)
library(feather)
library(dplyr)


train = fread("/Users/alexpapiu/Documents/Data/Bimbo/train.csv", 
              select = c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK",
                         "Cliente_ID", "Producto_ID", "Demanda_uni_equil"))

train$id = 1:nrow(train)


prod_id = fread("/Users/alexpapiu/Documents/Data/Bimbo/producto_tabla.csv")
client_id = fread("/Users/alexpapiu/Documents/Data/Bimbo/cliente_tabla.csv")
town_state = fread("/Users/alexpapiu/Documents/Data/Bimbo/town_state.csv") 
cleaned_products = fread("/Users/alexpapiu/Documents/Data/Bimbo/preprocessed_products.csv")


set.seed(428)
sam = sample(1:nrow(train), n)
train = train[sam]

val = train[train$Semana == 9] #split like in the kaggle set-up
train = train[train$Semana < 9]

val = joins(val)
train = joins(train)

train = clean(train)
val = clean(val)

#still need to to the cleaning for nombre producto.

write_feather(train, "/Users/alexpapiu/Documents/Data/Bimbo/sm_train.feather")
write_feather(val, "/Users/alexpapiu/Documents/Data/Bimbo/val.feather")

#one issue with sampling is that client_id's are so numerous - subsampling 20% of the data decreases 
#the val rmsle by a lot. perhaps a better idea would just be to use seamana 8 to predict semana 9?

#~~~~~~~~
#new_val:
#~~~~~~~
set.seed(412)
val = train[train$Semana == 9]
val = sample_n(val, 1e6)

train = train[train$Semana %in% c(7,8)]

train = joins(train)
val = joins(val)

write_feather(train, "/Users/alexpapiu/Documents/Data/Bimbo/sm_train.feather")
write_feather(val, "/Users/alexpapiu/Documents/Data/Bimbo/val.feather")

