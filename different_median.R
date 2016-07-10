#trying to figure out how to use the fact that for some product id's/clients only certain values
#appear for the demand:

train$prod_client = paste(train$Cliente_ID, train$Producto_ID)

train %>% count(prod_client, sort = TRUE) -> prod_clients


train %>% 
    filter(prod_client == prod_clients$prod_client[sample(1:nrow(prod_clients), 1)],
           Demanda_uni_equil < 5000) -> one_combo

ggplot(data = one_combo,aes(x = Demanda_uni_equil)) + geom_bar()

View(one_combo)

sum(prod_clients$n[1:1000])
#the vast majority of client, product combos have very few instances, less than 10

View(new_val[new_val$Producto_ID == "31187"]) 
#so the median already figures out that only some values appear - this is because the median is always
#either a value in the sequence or the mean between two values in the sequence.

View(new_val[new_val$Producto_ID == "32335"]) 

#is there a function that returns the median iff it's in the original set?
#let's make one:
median_ = function(seq) {
    if (length(seq) %% 2 == 1) return(median(seq))
        else return(median(seq[-1]))
}


median_(1:1e5)

system.time(
median(1:1e4))    


