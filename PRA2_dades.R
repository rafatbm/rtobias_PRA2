
#Carrega de llibreries
library(tidyverse)
library(dplyr)


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
missile_attacks_daily <- missile_attacks_daily %>%
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
  separate(
    launch_place,
    into = c("launch_place1", "launch_place2", "launch_place3", "launch_place4", "launch_place5", "launch_place6"),
    sep = " and ", fill = "right"
  ) %>%
  mutate(
    target1 = case_when(
      target1 == "Cherkasy oblast" ~ "Cherkasy",
      target1 == "Chernihiv oblast" ~ "Chernihiv",
      target1 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target1 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
      target1 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
      target1 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
      target1 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
      target1 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
      target1 == "Sumy oblast" ~ "Sumy",
      target1 == "Vinnytsia oblast" ~ "Vinnytsia",
      target1 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      TRUE ~ target1
    ),
    target2 = case_when(
      target2 == "Cherkasy oblast" ~ "Cherkasy",
      target2 == "Chernihiv oblast" ~ "Chernihiv",
      target2 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target2 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
      target2 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
      target2 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
      target2 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
      target2 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
      target2 == "Sumy oblast" ~ "Sumy",
      target2 == "Vinnytsia oblast" ~ "Vinnytsia",
      target2 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      TRUE ~ target2
    ),
    target3 = case_when(
      target3 == "Cherkasy oblast" ~ "Cherkasy",
      target3 == "Chernihiv oblast" ~ "Chernihiv",
      target3 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target3 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
      target3 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
      target3 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
      target3 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
      target3 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
      target3 == "Sumy oblast" ~ "Sumy",
      target3 == "Vinnytsia oblast" ~ "Vinnytsia",
      target3 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      TRUE ~ target3
    ),
    target4 = case_when(
      target4 == "Cherkasy oblast" ~ "Cherkasy",
      target4 == "Chernihiv oblast" ~ "Chernihiv",
      target4 == "Dnipropetrovsk oblast" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
      target4 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
      target4 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
      target4 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
      target4 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
      target4 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod", 
      target4 == "Sumy oblast" ~ "Sumy", 
      target4 == "Vinnytsia oblast" ~ "Vinnytsia", 
      target4 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
      TRUE ~ target4))

atacs_regio <- missile_attacks_daily %>%
        group_by(target1,target2,target3,target4) %>% 
        count(target1,target2,target3,target4) %>%
        mutate(target1 = case_when(
                target1 == "Cherkasy oblast" ~ "Cherkasy",
                target1 == "Chernihiv oblast" ~ "Chernihiv",
                target1 == "Dnipropetrovsk oblast" ~ "Dnipro",
                target1 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
                target1 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
                target1 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
                target1 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
                target1 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
                target1 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
                target1 == "Sumy oblast" ~ "Sumy",
                target1 == "Vinnytsia oblast" ~ "Vinnytsia",
                target1 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                TRUE ~ target1
        ),
        target2 = case_when(
                target2 == "Cherkasy oblast" ~ "Cherkasy",
                target2 == "Chernihiv oblast" ~ "Chernihiv",
                target2 == "Dnipropetrovsk oblast" ~ "Dnipro",
                target2 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
                target2 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
                target2 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
                target2 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
                target2 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
                target2 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
                target2 == "Sumy oblast" ~ "Sumy",
                target2 == "Vinnytsia oblast" ~ "Vinnytsia",
                target2 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                TRUE ~ target2
        ),
        target3 = case_when(
                target3 == "Cherkasy oblast" ~ "Cherkasy",
                target3 == "Chernihiv oblast" ~ "Chernihiv",
                target3 == "Dnipropetrovsk oblast" ~ "Dnipro",
                target3 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
                target3 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
                target3 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
                target3 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
                target3 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
                target3 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
                target3 == "Sumy oblast" ~ "Sumy",
                target3 == "Vinnytsia oblast" ~ "Vinnytsia",
                target3 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                TRUE ~ target3
        ),
        target4 = case_when(
                target4 == "Cherkasy oblast" ~ "Cherkasy",
                target4 == "Chernihiv oblast" ~ "Chernihiv",
                target4 == "Dnipropetrovsk oblast" ~ "Dnipro",
                target4 == "Dnipropetrovsk oblast, Dnipro raion" ~ "Dnipro",
                target4 == "Dnipropetrovsk oblast, Kamianske Raion" ~ "Kamianske",
                target4 == "Dnipropetrovsk oblast, Kryvyi Rih Raion" ~ "Kryvyi Rih",
                target4 == "Dnipropetrovsk oblast, Pavlohrad Raion" ~ "Pavlohrad",
                target4 == "Dnipropetrovsk oblast, Synelnykove Raion" ~ "Synelnykove",
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
                target4 == "Poltava oblast, Myrhorod Raion" ~ "Myrhorod",
                target4 == "Sumy oblast" ~ "Sumy",
                target4 == "Vinnytsia oblast" ~ "Vinnytsia",
                target4 == "Zaporizhzhia oblast" ~ "Zaporizhzhia",
                TRUE ~ target4
        ))


