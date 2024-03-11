




library(tidyverse)
Baza_razem4 <-  read_csv("Baza razem - 21 lutego 2022.csv")


source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")
Bazy_export <- Baza_razem4 %>% 
  mutate(Obszar_terapeutyczny = plyr::mapvalues(Obszar_terapeutyczny, 
                                                Obszary_old, Obszary_new)) %>% 
  select(Obserwacja_nr, everything())



filtr_obszary <- c(
  "Rare diseases",
  "Musculoskeletal system diseases",
  "Dermatology",
  "Diabetology",
  "Diagnostics",
  "Nephrology",
  "Ophthalmology",
  "Oncology",
  "Psychiatry",
  "Urology",
  "Virology"
)


library(xlsx)

Bazy_export2 <- Bazy_export %>% 
  # filter(Obszar_terapeutyczny %in% filtr_obszary) %>% 
  group_by(Obszar_terapeutyczny) %>% 
  nest() %>% 
  mutate(
    name = str_c("Dane tylko dla obszaru ", Obszar_terapeutyczny),
    file = str_c("./Dane - wybrane obszary/", name, ".xlsx"),
    x = map(data, as.data.frame)
    ) %>% 
  ungroup() 

Bazy_export2 %>% 
  select(x, file) %>% 
  pwalk(write.xlsx)
  

Bazy_export2$x[[1]] %>% 
  write.xlsx("jajo.xlsx")



