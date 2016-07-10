#eval:

library(data.table)
library(dplyr)
library(ggplot2)
library(Metrics)
library(feather)

train = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/sm_train.feather"))
val = as.data.table(read_feather("/Users/alexpapiu/Documents/Data/Bimbo/val.feather"))


#score for each mdeian
sapply(names(train)[-c(7,8,9,10,14)], eval_feat, USE.NAMES = TRUE) -> feature_scores

#Demanda_uni_equil       Producto_ID        Cliente_ID        Agencia_ID 
#0.02972167              0.69260075        0.76254821         0.79985826 
#Ruta_SAK          Canal_ID              Town             State 
#0.80004470        0.80163941            0.83696480       0.85114099 
#Semana 
#0.86475433 

#worse than the val using semana 3 as train and semana 4 as val - this makes some sense however since
#in the semana 3/4 case you're using all the data on semana 3 - if you use all the training data you 
#results similar to the leaderboard.


#with median - no improvment.
eval_feat("Cliente_ID")
