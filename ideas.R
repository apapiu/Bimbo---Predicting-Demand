#ideas that didn't work:


#trying to split and predict median based on the other half, to prevent overfitting:
#this will not work since there are so many Client ID's - you get too many NA's, a better 
#option would be to compute medians leaveig only the specific train example out but that would be costly. Also the median will not be too 
#influenced by one example so for now we'll just do it on the entire dataset.

#a better idea would probably be to aggregate and compute medians on half the dataset and then use
#the other half for xgboost.

split_half = function(train){
    set.seed(152)
    sam = sample(1:nrow(train), nrow(train)/2)
    
    count_by_level(train[sam], "Cliente_ID") %>% 
        join_by_count(train[-sam], .) -> count_client_1
    
    count_by_level(train[-sam], "Cliente_ID") %>% 
        join_by_count(train[sam], .) -> count_client_2
    
    count_client = rbind(count_client_1, count_client_2)
    
    
    #product:
    count_by_level(train[sam], "Producto_ID") %>% 
        join_by_count(train[-sam], .) -> count_1
    
    count_by_level(train[-sam], "Producto_ID") %>% 
        join_by_count(train[sam], .) -> count_2
    
    count_product = rbind(count_1, count_2)
    
    X = cbind(count_product, count_client) #id and demand are double here.
    return(X)
}
X_val = split_half(val)
#fixing the NA's
X = fix_NAs(X)
X_val = fix_NAs(X_val)