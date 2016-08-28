
#a look at which clients fit into which of the other variables:
train[,.(agencia = length(unique(Agencia_ID)),
         prod = length(unique(Producto_ID)),
         town = length(unique(Town)),
         ruta = length(unique(Ruta_SAK))), by = Cliente_ID] -> temp

sapply(train, function(x) length(unique(x)))

#so there are 254 towns.

train[,.(prod = length(unique(Producto_ID)),
         client = length(unique(Cliente_ID)),
         count = .N), by = Town] -> prod_client_counts_by_town

#so more popular town will have more clients but not more than 10000 - 
#copare this to 800000 total clients.

sum(prod_client_counts_by_town$client) =
    
#896845 so most clients are just in one town.
    
#how about Demand by week? we will preict on semana 11.
