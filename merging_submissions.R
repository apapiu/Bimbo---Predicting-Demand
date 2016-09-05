library(data.table)
library(dplyr)
library(dtplyr)

sub_1  = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/xgboost_mean_preds_3.csv")
sub_2 = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/submit_mean_by_Agency_Ruta_Client.csv")
sub_3 = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/results1.csv")

sub_2 = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/mean_BEST.csv")


sub_1 %>% 
    rename(my_xgb_dem = Demanda_uni_equil) -> sub_1


sub_2 %>% 
    rename(my_xgb_mean = Demanda_uni_equil) -> sub_2

sub_3 %>% 
    rename(my_xgb_lag = Demanda_uni_equil) -> sub_3



merge(sub_1, sub_2, by = "id") -> predz
    #merge(sub_3, by = "id") ->
    predz

predz %>% 
    #mutate(Demanda_uni_equil = 0.75*my_xgb_dem + 0.1*Demanda_uni_equil + 0.15*my_xgb_lag) %>% 
    mutate(Demanda_uni_equil = 0.75*my_xgb_dem + 0.25*my_xgb_mean) %>% 
    select(id, Demanda_uni_equil) %>% 
    write.csv("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/BEST_average_xgb.csv", row.names = FALSE)


cor(predz$my_xgb_dem, predz$Demanda_uni_equil)

sub_4 = fread("/Users/alexpapiu/Downloads/results1.csv")


merge(sub_3, sub_4, by = "id") -> temp

cor(temp$my_xgb_lag, temp$Demanda_uni_equil)