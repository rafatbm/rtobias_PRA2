
#Carrega de llibreries
library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)


#Carrega de fitxers
financial_commit <- read.csv("data_in/Financial Commitments.csv", sep = ",")
missile_attacks_daily <- read.csv("data_in/missile_attacks_daily.csv", sep = ",")
missile_uav <- read.csv("data_in/missiles_and_uav.csv", sep = ",")
equipment_losses <- read.csv("data_in/russia_losses_equipment.csv", sep = ",")
equipment_losses_corr <- read.csv("data_in/russia_losses_equipment_correction.csv", sep = ",")
personnel_losses <- read.csv("data_in/russia_losses_personnel.csv", sep = ",")


#PREPARACIÓ DE DADES
#Compromissos financers
top5_finan <- financial_commit %>% 
        arrange(desc(Financial.commitments...billion.)) %>%
        head(5) %>% 
        select(Country,Financial.commitments...billion.)

top5_human <- financial_commit %>% 
        arrange(desc(Humanitarian.commitments...billion.)) %>%
        head(5)%>% 
        select(Country,Humanitarian.commitments...billion.)

top5_militar <- financial_commit %>% 
        arrange(desc(Military.commitments...billion.)) %>%
        head(5)%>% 
        select(Country,Military.commitments...billion.)
        

#Objectius
#Per simplificar l'anàlisi ens centrarem en les regions i no tan en les ciutats
missile_attacks_daily_mod <- missile_attacks_daily %>%
  select(1:9) %>%
  mutate(
    time_start = as.POSIXct(time_start, format = "%Y-%m-%d %H:%M"),
    date_start = as.Date(time_start),
    time_start2 = format(time_start, format = "%H:%M"),
    time_end = as.POSIXct(time_end, format = "%Y-%m-%d %H:%M"),
    date_send = as.Date(time_end),
    time_end2 = format(time_end, format = "%H:%M")
  ) %>%
  separate(target, into = c("target1", "target2", "target3", "target4"), sep = " and ", fill = "right") %>%
  mutate(
    target1 = case_when(
      target1 == "Cherkasy oblast" ~ "Cherkasy",
      target1 == "Chernihiv oblast" ~ "Chernihiv",
      target1 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Dnipro",
      target1 == "Donetsk oblast" ~ "Donetsk",
      target1 == "Kharkiv oblast" ~ "Kharkiv",
      target1 == "Kherson oblast" ~ "Kherson",
      target1 == "Khmelnytskyi oblast" ~ "Khmelnytskyi",
      target1 == "Kirovohrad oblast" ~ "Kirovohrad",
      target1 == "Kursk oblast" ~ "Kursk",
      target1 == "Kyiv oblast" ~ "Kyiv",
      target1 == "Kyiv oblast (south)" ~ "Kyiv",
      target1 == "Mykolaiv oblast" ~ "Mykolaiv",
      target1 == "Odesa oblast" ~ "Odesa",
      target1 == "Odesa oblast, Black Sea" ~ "Odesa",
      target1 == "Poltava oblast" ~ "Poltava",
      target1 == "Poltava oblast, Myrhorod Raion" ~ "Poltava",
      target1 == "Sumy oblast" ~ "Sumy",
      target1 == "Vinnytsia oblast" ~ "Vinnytsia",
      target1 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      target1 == "Lviv oblast" ~ "Lviv",
      target1 == "Dnipropetrovsk oblast, Dnipro Raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Novomoskovsk Raion" ~ "Dnipro",
      target1 == "Starokostiantyniv" ~ "Khmelnytskyi",
      target1 == "Ochakiv" ~ "Mikolaiv",
      target1 == "Kolomyia" ~ "Ivano-Frankivsk",
      target1 == "Kryvyi Rih" ~ "Dnipro",
      target1 == "Kramatorsk" ~ "Donetsk",
      TRUE ~ target1
    ),
    target2 = case_when(
      target2 == "Cherkasy oblast" ~ "Cherkasy",
      target2 == "Chernihiv oblast" ~ "Chernihiv",
      target2 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Dnipro",
      target2 == "Donetsk oblast" ~ "Donetsk",
      target2 == "Kharkiv oblast" ~ "Kharkiv",
      target2 == "Kherson oblast" ~ "Kherson",
      target2 == "Khmelnytskyi oblast" ~ "Khmelnytskyi",
      target2 == "Kirovohrad oblast" ~ "Kirovohrad",
      target2 == "Kursk oblast" ~ "Kursk",
      target2 == "Kyiv oblast" ~ "Kyiv",
      target2 == "Kyiv oblast (south)" ~ "Kyiv",
      target2 == "Mykolaiv oblast" ~ "Mykolaiv",
      target2 == "Odesa oblast" ~ "Odesa",
      target2 == "Odesa oblast, Black Sea" ~ "Odesa",
      target2 == "Poltava oblast" ~ "Poltava",
      target2 == "Poltava oblast, Myrhorod Raion" ~ "Poltava",
      target2 == "Sumy oblast" ~ "Sumy",
      target2 == "Vinnytsia oblast" ~ "Vinnytsia",
      target2 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      target2 == "Lviv oblast" ~ "Lviv",
      target2 == "Starokostiantyniv" ~ "Khmelnytskyi",
      TRUE ~ target2
    ),
    target3 = case_when(
      target3 == "Cherkasy oblast" ~ "Cherkasy",
      target3 == "Chernihiv oblast" ~ "Chernihiv",
      target3 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Dnipro",
      target3 == "Donetsk oblast" ~ "Donetsk",
      target3 == "Kharkiv oblast" ~ "Kharkiv",
      target3 == "Kherson oblast" ~ "Kherson",
      target3 == "Khmelnytskyi oblast" ~ "Khmelnytskyi",
      target3 == "Kirovohrad oblast" ~ "Kirovohrad",
      target3 == "Kursk oblast" ~ "Kursk",
      target3 == "Kyiv oblast" ~ "Kyiv",
      target3 == "Kyiv oblast (south)" ~ "Kyiv",
      target3 == "Mykolaiv oblast" ~ "Mykolaiv",
      target3 == "Odesa oblast" ~ "Odesa",
      target3 == "Odesa oblast, Black Sea" ~ "Odesa",
      target3 == "Poltava oblast" ~ "Poltava",
      target3 == "Poltava oblast, Myrhorod Raion" ~ "Poltava",
      target3 == "Sumy oblast" ~ "Sumy",
      target3 == "Vinnytsia oblast" ~ "Vinnytsia",
      target3 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      target3 == "Lviv oblast" ~ "Lviv",
      TRUE ~ target3
    ),
    target4 = case_when(
      target4 == "Cherkasy oblast" ~ "Cherkasy",
      target4 == "Chernihiv oblast" ~ "Chernihiv",
      target4 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Dnipro",
      target4 == "Donetsk oblast" ~ "Donetsk",
      target4 == "Kharkiv oblast" ~ "Kharkiv",
      target4 == "Kherson oblast" ~ "Kherson",
      target4 == "Khmelnytskyi oblast" ~ "Khmelnytskyi",
      target4 == "Kirovohrad oblast" ~ "Kirovohrad",
      target4 == "Kursk oblast" ~ "Kursk",
      target4 == "Kyiv oblast" ~ "Kyiv",
      target4 == "Kyiv oblast (south)" ~ "Kyiv",
      target4 == "Mykolaiv oblast" ~ "Mykolaiv",
      target4 == "Odesa oblast" ~ "Odesa",
      target4 == "Odesa oblast, Black Sea" ~ "Odesa", 
      target4 == "Poltava oblast" ~ "Poltava", 
      target4 == "Poltava oblast, Myrhorod Raion" ~ "Poltava", 
      target4 == "Sumy oblast" ~ "Sumy", 
      target4 == "Vinnytsia oblast" ~ "Vinnytsia", 
      target4 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      target4 == "Lviv oblast" ~ "Lviv",
      TRUE ~ target4),
    launch_place = str_replace_all(launch_place, ",", " and")) %>% 
        separate(launch_place, into = paste0("launch_place", 1:6), sep = " and ", fill = "right") %>% 
        mutate(launch_place1 = case_when(
                launch_place1 == "Bryansk oblast" ~ "Bryansk",
                launch_place1 == "Primorsko-Akhtarsk" ~ "Krasnodar Krai",
                launch_place1 == "Yeysk" ~ "Krasnodar Krai",
                launch_place1 == "Belgorod oblast" ~ "Belgorod",
                launch_place1 == "Tula oblast" ~ "Tula",
                launch_place1 == "Taganrog" ~ "Rostov",
                launch_place1 == "Morozovsk" ~ "Rostov",
                launch_place1 == "Olenya" ~ "Murmansk",
                launch_place1 == "Engels-2" ~ "Saratov",
                launch_place1 == "Olenegorsk" ~ "Murmansk",
                launch_place1 == "Dzhankoi" ~ "Crimea",
                launch_place1 == "Sevastopol" ~ "Crimea",
                launch_place1 == "Cape Tarkhankut" ~ "Crimea",
                launch_place1 == "Chauda" ~ "Crimea",
                launch_place1 == "Balaklava" ~ "Crimea",
                launch_place1 == "Voronezh oblast" ~ "Voronezh",
                launch_place1 == "Kursk oblast" ~ "Kursk",
                launch_place1 == "Oryol oblast" ~ "Oryol",
                launch_place1 == "Donetsk oblast" ~ "Donetsk",
                launch_place1 == "Rostov oblast" ~ "Rostov",
                launch_place1 == "Soltsy-2" ~ "Novgorod",
                launch_place1 == "Savasleyka" ~ "Novgorod",
                launch_place1 == "Luhansk oblast" ~ "Luhansk",
                launch_place1 == "Tambov oblast" ~ "Tambov",
                launch_place1 == "Volgograd oblast" ~ "Volgograd",
                launch_place1 == "Astrakhan oblast" ~ "Astrakhan",
                launch_place1 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                launch_place1 == "Tokmak" ~ "Zaporizhzhia",
                launch_place1 == "Kherson oblast" ~ "Kherson",
                launch_place1 == "Lipetsk oblast" ~ "Lipetsk",
                launch_place1 == "Ryazan oblast" ~ "Ryazan",
                launch_place1 == "Saratov oblast" ~ "Saratov",
                launch_place1 == "eastern coast of Sea of Azov" ~ "Sea of Azov",
                TRUE ~ launch_place1),
               launch_place2 = case_when(
                       launch_place2 == "Oryol oblast" ~ "Oryol",
                       launch_place2 == "Bryansk oblast" ~ "Bryansk",
                       launch_place2 == "Seshcha" ~ "Bryansk",
                       launch_place2 == "Chauda" ~ "Crimea",
                       launch_place2 == "Yeysk" ~ "Krasnodar Krai",
                       launch_place2 == "Engels-2" ~ "Saratov",
                       launch_place2 == "Taganrog" ~ "Volgodonsk",
                       launch_place2 == "Kursk oblast" ~ "Kursk",
                       launch_place2 == "Donetsk oblast" ~ "Donetsk",
                       launch_place2 == "Sevastopol" ~ "Crimea",
                       launch_place2 == "Tokmak" ~ "Zaporizhzhia",
                       launch_place2 == "Belgorod oblast" ~ "Belgorod",
                       launch_place2 == "Tula oblast" ~ "Tula",
                       launch_place2 == "Voronezh oblast" ~ "Voronezh",
                       launch_place2 == "Volgodonsk" ~ "Rostov",
                       launch_place2 == "Feodosia" ~ "Crimea",
                       launch_place2 == "Balaklava" ~ "Crimea",
                       launch_place2 == "Shaykovka" ~ "Kaluga",
                       launch_place2 == "Millerovo" ~ "Rostov",
                       launch_place2 == "Olenya" ~ "Murmansk",
                       launch_place2 == "Yalta" ~ "Crimea",
                       launch_place2 == "Lipetsk oblast" ~ "Lipetsk",
                       launch_place2 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                       launch_place2 == "Tambov oblast" ~ "Tambov",
                       TRUE ~ launch_place2),
               launch_place3 = case_when(
                       launch_place3 == "Millerovo" ~ "Rostov",
                       launch_place3 == "Dzhankoi" ~ "Crimea",
                       launch_place3 == "Belbek" ~ "Crimea",
                       launch_place3 == "Bryansk oblast" ~ "Bryansk",
                       launch_place3 == "Belgorod oblast" ~ "Belgorod",
                       launch_place3 == "Voronezh oblast" ~ "Voronezh",
                       launch_place3 == "Kursk oblast" ~ "Kursk",
                       launch_place3 == "Oryol oblast" ~ "Oryol",
                       launch_place3 == "Yeysk" ~ "Krasnodar Krai",
                       launch_place3 == "Balaklava" ~ "Crimea",
                       TRUE ~ launch_place3),
               launch_place4 = case_when(
                       launch_place4 == "Oryol oblast" ~ "Oryol",
                       launch_place4 == "Millerovo" ~ "Rostov",
                       launch_place4 == "Rostov oblast" ~ "Rostov",
                       launch_place4 == "Bryansk oblast" ~ "Bryansk",
                       launch_place4 == "Kursk oblast" ~ "Kursk",
                       launch_place4 == "Yeysk" ~ "Krasnodar Krai",
                       launch_place4 == "Seshcha" ~ "Bryansk",
                       launch_place4 == "Balaklava" ~ "Crimea",
                       TRUE ~ launch_place4),
               launch_place5 = case_when(
                       launch_place5 == "Oryol oblast" ~ "Oryol",
                       launch_place5 == "Millerovo" ~ "Rostov",
                       launch_place5 == "Yeysk" ~ "Krasnodar Krai",
                       TRUE ~ launch_place5),
               launch_place6 = ifelse(launch_place6 == "Kursk oblast","Kursk",launch_place6))
               
        
        
        
atacs_regio1 <- missile_attacks_daily_mod %>%
        group_by(target1) %>% 
        count(target1)

atacs_regio2 <- missile_attacks_daily_mod %>%
        group_by(target2) %>% 
        count(target2) %>% 
        na.omit()

atacs_regio3 <- missile_attacks_daily_mod %>%
        group_by(target3) %>% 
        count(target3) %>% 
        na.omit()

atacs_regio4 <- missile_attacks_daily_mod %>%
        group_by(target4) %>% 
        count(target4) %>% 
        na.omit()

atacs_per_region <- atacs_regio1 %>% 
        left_join(atacs_regio2, by = c("target1" = "target2")) %>% 
        left_join(atacs_regio3, by = c("target1" = "target3")) %>% 
        left_join(atacs_regio4, by = c("target1" = "target4")) %>% 
        mutate(across(everything(), ~replace_na(., 0)),
               total = n.x + n.y + n.x.x + n.y.y) %>% 
        select(target1, total)


launch_place1 <- missile_attacks_daily_mod %>%
        group_by(launch_place1) %>% 
        count(launch_place1) %>% 
        na.omit()

launch_place2 <- missile_attacks_daily_mod %>%
        group_by(launch_place2) %>% 
        count(launch_place2) %>% 
        na.omit()

launch_place3 <- missile_attacks_daily_mod %>%
        group_by(launch_place3) %>% 
        count(launch_place3) %>% 
        na.omit()

launch_place4 <- missile_attacks_daily_mod %>%
        group_by(launch_place4) %>% 
        count(launch_place4) %>% 
        na.omit()

launch_place5 <- missile_attacks_daily_mod %>%
        group_by(launch_place5) %>% 
        count(launch_place5) %>% 
        na.omit()

launch_place6 <- missile_attacks_daily_mod %>%
        group_by(launch_place6) %>% 
        count(launch_place6) %>% 
        na.omit()

launch_per_regions <- launch_place1 %>% 
        left_join(launch_place2, by = c("launch_place1" = "launch_place2")) %>% 
        left_join(launch_place3, by = c("launch_place1" = "launch_place3")) %>% 
        left_join(launch_place4, by = c("launch_place1" = "launch_place4")) %>%
        left_join(launch_place5, by = c("launch_place1" = "launch_place5")) %>%
        left_join(launch_place6, by = c("launch_place1" = "launch_place6")) %>%
        mutate(across(everything(), ~replace_na(., 0)),
               total = n.x + n.y + n.x.x + n.y.y + n.x.x.x + n.y.y.y) %>% 
        select(launch_place1, total)

write.xlsx(atacs_per_region,"atacs_per_regio.xlsx", rowNames = FALSE)  
write.xlsx(launch_per_regions,"launch_per_region.xlsx", rowNames = FALSE) 

tipus_missils <- missile_attacks_daily %>% 
        group_by(model) %>% 
        summarise(launched = sum(launched),
                  destroyed = sum(destroyed),
                  not_reach_goal = sum(not_reach_goal))


        mutate(model = str_replace_all(model, "/", " and ")) %>% 
        separate(model, into = paste0("model", 1:4), sep = " and ", fill = "right") %>% 
        select(model1,model2,model3,model4)
        

tipus_missils1 <- tipus_missils %>%
        group_by(model1) %>% 
        count(model1) %>% 
        na.omit()

tipus_missils2 <- tipus_missils %>%
        group_by(model2) %>% 
        count(model2) %>% 
        na.omit()

tipus_missils3 <- tipus_missils %>%
        group_by(model3) %>% 
        count(model3) %>% 
        na.omit()

tipus_missils4 <- tipus_missils %>%
        group_by(model4) %>% 
        count(model4) %>% 
        na.omit()

tipus_missil_total <- tipus_missils1 %>% 
        left_join(tipus_missils2, by = c("model1" = "model2")) %>% 
        left_join(tipus_missils3, by = c("model1" = "model3")) %>% 
        left_join(tipus_missils4, by = c("model1" = "model4")) %>%
        mutate(across(everything(), ~replace_na(., 0)),
               total = n.x + n.y + n.x.x + n.y.y) %>% 
        select(model1, total)


#Segons la web del Banc Mundial (https://datos.bancomundial.org/indicador/SP.POP.TOTL?locations=UA) la pobráció d'Ucraïna al 2022,
#any de l'inici de la invasió era de 41.048.766 habitants 

poblacio_ucraina <- 41048766
print(paste0("La poblacio a Ucraïna al 2022 era de ",poblacio_ucraina,"habitants."))

#Segons dades de Nacions Unides le nombre total de refugiats
refugiats_totals <- read_excel("C:/Users/rafat/Documents/UOC/Master en Ciencia de Dades/Visualització de dades/PRA2/rtobias_PRA2/data_in/statistic_id1312584_number-of-ukrainian-refugees-2024-by-country.xlsx", sheet = "Data_mod")

refugiats <- sum(refugiats_totals$`Number of refugees`)
print(paste0("El total de refugiats és de ",refugiats,"."))

#Revisarem quina es la proporció de població refugiada respecte el total
percentatge_refugiats <- refugiats/poblacio_ucraina
print(paste0("El ",round(percentatge_refugiats*100,2), "% de problació ha fugit del país"))

total_bombardejos <- sum(atacs_per_region$total) - 28
        
print(paste0("El total de bombardejos ha sigut de ",total_bombardejos, "."))
