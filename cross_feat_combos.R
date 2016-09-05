#ok how to make the search for good cross-features more systematic?
# maybe look at the standard deviation of the groups?
#but this might not work since in many cases there are so few examples withtin the given group
#of course using a validation set is fine I guess but not very statistically sound.

#so use total_mean < product_mean < best product+other_feat_1 < best product+other_feat1 + other_feat_2

total_mean = mean(train$Demanda_uni_equil)
prod_means = train[, .(mean_prod = mean(Demanda_uni_equil)), by = .(Producto_ID)]
prod_means_clients = train[, (mean_prod_clients = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID)]

#product and X:
feat = "Ruta_SAK"

three_feat_val = function(feat) {
prod_X_means = train[, .(mean_cross = mean(Demanda_uni_equil)),
                     by = .(Producto_ID, Cliente_ID, get(feat))]
names(prod_X_means)[3] = feat

val %>% 
    merge(prod_X_means, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", feat)) %>% 
    merge(prod_means_clients, all.x = TRUE, by = c("Producto_ID", "Cliente_ID")) %>% 
    merge(prod_means, all.x = TRUE, by = "Producto_ID") -> merged

preds = coalesce(merged$mean_cross, merged$mean_prod, total_mean)
return(rmse(preds, merged$Demanda_uni_equil))}

three_feat_val("Ruta_SAK")

sapply(names(train)[c(2:5,7:19)], three_feat_val) %>% sort()

#so doing the three way cross has little benefit - some of the feats overlap or 
#are subclasses of one another.

#two val feat:
#Demanda_uni_equil        Cliente_ID          Ruta_SAK        Agencia_ID 
#0.1040733                0.5492940           0.6143919       0.6210070 
#Town          Canal_ID             State            Semana 
#0.6419724     0.6426528            0.6563925        0.6821010 
#id                V1      product_name             brand 
#0.6821010         0.6821010         0.6821010      0.6821010 
#weight            pieces  weight_per_piece         has_choco 
#0.6821010         0.6821010         0.6821010         0.6821010 
#has_vanilla    has_multigrain 
#0.6821010         0.6821010 


#three val feat - not a big difference from the two one:
#Agencia_ID              Town             State                V1 
#0.5490295         0.5490295         0.5490908         0.5492940 
#product_name             brand            weight            pieces 
#0.5492940         0.5492940         0.5492940         0.5492940 
#weight_per_piece         has_choco       has_vanilla    has_multigrain 
#0.5492940         0.5492940         0.5492940         0.5492940 
#Canal_ID          Ruta_SAK Demanda_uni_equil            Semana 
#0.5495499         0.5498530         0.6209163         0.6821010 
#id 
#0.6821010 


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#trying to do it two by two:
#~~~~~~~~~~~~~~~~~~~~~~~~~

total_mean = mean(train$Demanda_uni_equil)
prod_means = train[, .(mean_prod = mean(Demanda_uni_equil)), by = .(Producto_ID)]
prod_ruta_means = train[, .(mean_prod_ruta = mean(Demanda_uni_equil)), by = .(Producto_ID, Ruta_SAK)]
prod_client =  train[, .(mean_prod_client = mean(Demanda_uni_equil)), by = .(Producto_ID, Cliente_ID)]
prod_agencia = train[, .(mean_prod_agencia = mean(Demanda_uni_equil)), by = .(Producto_ID, Agencia_ID)]

val %>% 
    merge(prod_means, all.x = TRUE, by = "Producto_ID") %>% 
    merge(prod_ruta_means, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK")) %>% 
    merge(prod_client, all.x = TRUE, by = c("Producto_ID", "Cliente_ID")) %>% 
    merge(prod_agencia, all.x = TRUE, by = c("Producto_ID", "Agencia_ID")) -> merged

preds = coalesce(merged$mean_prod_client,
                 #merged$mean_prod_agencia, #putting agencia above works best.
                 merged$mean_prod_ruta,
                 merged$mean_prod, total_mean)

rmse(merged$Demanda_uni_equil, preds)


#~~~~~~~~~~
#the ideas above work ok when the number of features is small but we're looking at 2^n comparisions here
#where n is the number of features.
#another idea - similar to forward stepwise linear regression: try to see which one single feature gives 
#best validation error - F_1, then add another one F_2 - picked so it minimizes validation rmse, then
#keep going like this - this takes at most n^2 comparisons.

#Ensembling the results:
#another good idea is that when we have all these means for grouping by Producto, Cliente, Producto+Cliente
#we can use them as inputs in a new model. The big issue here is overfitting however.