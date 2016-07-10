#functions:
library(data.table)
library(dplyr)
library(ggplot2)
library(Metrics)

#make a function to test a feature by grouping on it and predictng the median and computing rmlse

eval_feat = function(feat) {
    grouped_data = train[,.(median = expm1(mean(log1p(Demanda_uni_equil)))),
                         by = .(get(feat))] #get(feat) since data.table doesn't like strings
    names(grouped_data)[1] = feat #to make the join work
    new_val = left_join(val, grouped_data, by = feat)
    preds = new_val$median
    preds[is.na(preds)] = median(train$Demanda_uni_equil)
    return(rmsle(new_val$Demanda_uni_equil, preds))
}


plot_it = function(train, feat, tresh = 25) {
    train %>% 
        filter(Demanda_uni_equil < tresh) %>% 
        ggplot(aes_string(x = "Demanda_uni_equil", fill = feat)) +
        geom_bar(position = "fill")
}


joins = function(train) {
    train %>% 
        left_join(town_state) %>% 
        #left_join(client_id) %>% there are a few client_id's that are doubles.
        left_join(cleaned_products) -> train
    return(train) 
}

clean = function(train) {
    
    train %>% 
        mutate(Agencia_ID = as.character(Agencia_ID),
               Canal_ID = as.character(Canal_ID),
               Producto_ID = as.character(Producto_ID),
               Ruta_SAK = as.character(Ruta_SAK),
               Cliente_ID = as.character(Cliente_ID)) -> train
    
    train = joins(train)
    
    return(train)
}

#takes a high cardinality factor with a numeric target and outputs the median, sd, and count grouped 
#by factor levels, these can be used as numeric features in other models:
count_by_level = function(train, feat) { 
    counts = train[,.(median = mean(Demanda_uni_equil), 
                      sd = sd(Demanda_uni_equil),
                      count = .N),
                   by = .(get(feat))] #you need get here since data.table doesn't like strings
    
    counts$sd[is.na(counts$sd)] = 0 #setting count = 1 sd's equal to zero(good idea?)
    names(counts)[1] = feat #so we can do the join
    names(counts)[-1] = paste0(names(counts)[-1], "_", feat)
    return(counts)
}


join_by_count = function(df, counts) {
    feat = names(counts)[1]
    df = left_join(df, counts, by = feat) #it messes up the order here
    df %>% arrange(id) -> df #this to put the examples back in order..meh...not the best example.
    len = length(names(df)) 
    return(df[,(len-4):len, with = FALSE])
}


fix_NAs = function(X_val) { #replaces NA's caused by joining on the test set
    X_val$median_Cliente_ID[is.na(X_val$median_Cliente_ID)] = median(train$Demanda_uni_equil)
    X_val$median_Producto_ID[is.na(X_val$median_Producto_ID)] = median(train$Demanda_uni_equil)
    
    X_val$sd_Producto_ID[is.na(X_val$sd_Producto_ID)] = 0
    X_val$sd_Cliente_ID[is.na(X_val$sd_Cliente_ID)] = 0
    
    
    X_val$count_Producto_ID[is.na(X_val$count_Producto_ID)] = 0
    X_val$count_Cliente_ID[is.na(X_val$count_Cliente_ID)] = 0
    return(X_val)
}

