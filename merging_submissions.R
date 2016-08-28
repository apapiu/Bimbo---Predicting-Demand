
sub_1  = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/xgboost_mean_preds_2.csv")
sub_2 = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/submit_mean_by_Agency_Ruta_Client.csv")
sub_3 = fread("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/xgboost_lag_preds_2.csv")


sub_1 %>% 
    rename(my_xgb_dem = Demanda_uni_equil) -> sub_1


sub_3 %>% 
    rename(my_xgb_lag = Demanda_uni_equil) -> sub_3



merge(sub_1, sub_2, by = "id") %>% merge(sub_3, by = "id") -> predz

predz %>% 
    mutate(Demanda_uni_equil = 0.6*my_xgb_dem + 0.2*Demanda_uni_equil + 0.2*my_xgb_lag) %>% 
    select(id, Demanda_uni_equil) %>% 
    write.csv("/Users/alexpapiu/Documents/Data/Bimbo/Submissions/blend_solution_4.csv", row.names = FALSE)


cor(predz$my_xgb_lag, predz$my_xgb_dem)
