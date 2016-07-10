#some eda for the script:

library(ggplot2)
library(plotly)

#load(val)

sm_train = train[1:2e5,]


#so what exactly is this dataset?
View(sample_n(train, 100))

train %>% count(Cliente_ID) %>% arrange(desc(n))

#let's look at one client:
train %>% filter(Cliente_ID == 653039) -> one_client

one_client %>% count(Producto_ID) -> prod_counts 
#lots of prodcuts appear many times, probably there is a daily or even more than daily delivery.

one_client %>% count()

#
sapply(train, function(x) length(unique(x)))



#very skewed distribution of demand.
ggplot(data = train, aes(x = Demanda_uni_equil)) + geom_bar()

hist(train$Demanda_uni_equil[train$Demanda_uni_equil < 100], breaks = 100)

train %>% count(Demanda_uni_equil) -> demands
#first observation: there are more demands at integers close to 10's and 100's

#tryin to look at different categorical features versus the demand
#one tricky part is that the demand in only integers....
#maybe perturb it a bit? but then I'd lose the integer patterns


#function plotting different features:
plot_it = function(train, feat, tresh = 25) {
    train %>% 
        filter(Demanda_uni_equil < tresh) %>% 
        ggplot(aes_string(x = "Demanda_uni_equil", fill = feat)) +
        geom_bar(position = "fill")
}

names = names(train)[1:6]

#Agencia ID:
plot_it(sm_train, names[2], 25)

plot_it(sm_train, names[2], 25) + geom_bar() 

plot_it(sm_train, names[2], 25) + geom_bar(position = "identity", alpha = 0.5) 

plot_it(sm_train, names[2], 25) + geom_bar(position = "dodge") 


plot_it(sm_train, names[2], 25) +
    geom_bar() +
    facet_wrap( ~ Agencia_ID)#zero seems somewhat iportant but meh


#Canal ID:
plot_it(sm_train, names[3], 10) +
    geom_bar() +
    facet_wrap( ~ Canal_ID)
    #zero looks different but meh

#Route:
plot_it(sm_train, names[4], 10) + theme(legend.position = "none") #too many levels.

sm_train %>% count(Ruta_SAK) %>% arrange(desc(n)) -> routes

sm_train %>% 
    filter(Ruta_SAK %in% routes$Ruta_SAK[1:20]) %>% 
    plot_it(names[4], 10) + 
    geom_bar() + 
    facet_wrap( ~ Ruta_SAK)


#Client:

sm_train %>% count(Cliente_ID) %>% arrange(desc(n)) -> clients
top_clients = clients$Cliente_ID[1:20]

sm_train %>% 
    filter(Cliente_ID %in% top_clients) %>% 
    plot_it("Cliente_ID", 50) + 
    geom_bar(position = "identity") +
    facet_wrap(~ Cliente_ID)



#Product
sm_train %>% count(Producto_ID) %>% arrange(desc(n)) -> counts

sm_train %>% 
    filter(Producto_ID %in% counts$Producto_ID[13:14]) %>% 
    plot_it(names[6], 20) + 
    geom_bar(position = "identity", alpha = 0.6)


sm_train %>% 
    filter(Producto_ID %in% counts$Producto_ID[1:6]) %>% 
    plot_it(names[6], 20) + 
    geom_bar(position = "fill", alpha = 1)
    

sm_train %>% 
    filter(Producto_ID %in% counts$Producto_ID[1:16]) %>% 
    plot_it(names[6], 20) + 
    geom_bar() +
    facet_wrap(~ Producto_ID) +
    theme(legend.position = "none")


#EDA on the preprocessed products:

cor(train$Demanda_uni_equil[1:1000], train$weight[1:1000])

sample_n(train, 1e4) %>% 
    ggplot(aes(x = Demanda_uni_equil, y = weight)) +
    geom_point() +
    ylim(0, 1000) +
    xlim(0, 50) +
    geom_smooth()

sample_n(train, 5e6) %>% 
    plot_it("brand") +
    geom_bar() +
    facet_wrap( ~ brand)
