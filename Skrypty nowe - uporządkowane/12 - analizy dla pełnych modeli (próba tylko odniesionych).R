
# Krok 12 - analizy dla pełnych modeli ------------------------------------

library(epitools)
  library(rcompanion)
library(modelr)
library(caret)
library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")

source("./Funkcje/Krok 12 - Funkcje pomocnicze.R")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")




# Wektory pomocnicze ------------------------------------------------------


odds_names <- c(
  "Kraj",
  "n",
  "Korzyść kliniczna",
  "n",
  "Bezpieczeństwo",
  "n",
  "Efektywność kosztowa",
  "n",
  "Wpływ na budżet"
  )


# Modele - wszystkie obserwacje -------------------------------------------




rekomendacje_nested <- Analizy_korzyść %>% 
  group_by(Kraj) %>% 
  nest() %>% 
  filter(!Kraj %in% c("Szwecja"))





glm(Rekomendacja2 ~ Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2,
    family = "binomial", data = rekomendacje_nested$data[[11]])


formuły <- c(
  Polska = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  `Wielka Brytania` = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2 ",
  Szkocja = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
  Walia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  Irlandia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
  Francja = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
  Holandia = "Kat1_Korzyść_kliniczna2 + Kat1_Wpływ_budżet2",
  Niemcy = "Kat1_Korzyść_kliniczna2",
  Norwegia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  Australia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 ",
  `Nowa Zelandia` = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
  Kanada = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2"
)




model_full <- rekomendacje_nested %>%
  ungroup() %>% 
  mutate(
    Predyktory = formuły,
    full_model = map2(data, Predyktory, possibly(model_log, otherwise = NA)),
    full_model2 = map(full_model, possibly(tidy_full, otherwise = NA)),
    n = map_int(full_model, possibly(tidy_nobs, otherwise = NA))
  )

model_full

Analizy_korzyść %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2 , Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2, Kat1_Wpływ_budżet2) %>% 
  pivot_longer(-1) %>% 
  drop_na() %>% 
  select(Kraj, name) %>% 
  table()




model_full %>% 
  select(Kraj, full_model2, n) %>% 
  unnest(full_model2) %>% 
  pivot_wider(names_from = "term", values_from = "estimate2")


model_full %>% 
  mutate(acc = map2(data, full_model, possibly(add_accuracy, otherwise = ""))  )

# McNemmar


rekomendacje_nested %>% 
  mutate(
    n_korzyść_klincizna = map_int(data, odds_nMap, x = "Kat1_Korzyść_kliniczna2"),
    odds_korzyść_klincizna = map_chr(data, odds_map, x = "Kat1_Korzyść_kliniczna2"),
    n_bezpieczenstwo = map_int(data, odds_nMap, x = "Kat1_Bezpieczeństwo2"),
    odds_bezpieczenstwo = map_chr(data, odds_map, x = "Kat1_Bezpieczeństwo2"),
    n_efektywnosc_kosztowa = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Efektywność_kosztowa2"),
    odds_efektywnosc_kosztowa = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Efektywność_kosztowa2"),
    n_wpływ_budżet = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Wpływ_budżet2"),
    odds_wpływ_budżet = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Wpływ_budżet2")
  )



# Modele - pomniejszona próba ---------------------------------------------




rekomendacje_nested2 <- Analizy_pomniejszone %>% 
  group_by(Kraj) %>% 
  nest() %>% 
  filter(!Kraj %in% c("Szwecja"))



glm(Rekomendacja2 ~ Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2,
    family = "binomial", data = rekomendacje_nested2$data[[11]])


formuły2 <- c(
  Polska = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  `Wielka Brytania` = "Kat1_Korzyść_kliniczna2 +  Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 ",
  Szkocja = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
  Walia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  Irlandia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
  Francja = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
  Holandia = "Kat1_Korzyść_kliniczna2 + Kat1_Wpływ_budżet2",
  Niemcy = "Kat1_Korzyść_kliniczna2",
  Norwegia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
  Australia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
  `Nowa Zelandia` = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
  Kanada = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2"
)





model_full2 <- rekomendacje_nested2 %>%
  ungroup() %>% 
  mutate(
    Predyktory = formuły2,
    full_model = map2(data, Predyktory, possibly(model_log, otherwise = NA)),
    full_model2 = map(full_model, possibly(tidy_full, otherwise = NA)),
    n = map_int(full_model, possibly(tidy_nobs, otherwise = NA)),
  )


model_full2 %>% 
  select(Kraj, full_model2, n) %>% 
  unnest(full_model2) %>% 
  pivot_wider(names_from = "term", values_from = "estimate2")



Analizy_pomniejszone %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2 , Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2, Kat1_Wpływ_budżet2) %>% 
  pivot_longer(-1) %>% 
  drop_na() %>% 
  select(Kraj, name) %>% 
  table()



model_full2 %>% 
  mutate(acc = map2(data, full_model, possibly(add_accuracy, otherwise = ""))  )

# Mcnemmar


rekomendacje_nested2 %>% 
  mutate(
    n_korzyść_klincizna = map_int(data, odds_nMap, x = "Kat1_Korzyść_kliniczna2"),
    odds_korzyść_klincizna = map_chr(data, odds_map, x = "Kat1_Korzyść_kliniczna2"),
    n_bezpieczenstwo = map_int(data, odds_nMap, x = "Kat1_Bezpieczeństwo2"),
    odds_bezpieczenstwo = map_chr(data, odds_map, x = "Kat1_Bezpieczeństwo2"),
    n_efektywnosc_kosztowa = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Efektywność_kosztowa2"),
    odds_efektywnosc_kosztowa = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Efektywność_kosztowa2"),
    n_wpływ_budżet = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Wpływ_budżet2"),
    odds_wpływ_budżet = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Wpływ_budżet2")
  )





# Pomniejszona próba i "nie można stwierdzić" jako NA ---------------------


rekomendacje_nested2 %>% 
  mutate(
    n_korzyść_klincizna = map_int(data, odds_nMap, x = "Kat1_Korzyść_kliniczna3"),
    odds_korzyść_klincizna = map_chr(data, odds_map, x = "Kat1_Korzyść_kliniczna3"),
    n_bezpieczenstwo = map_int(data, odds_nMap, x = "Kat1_Bezpieczeństwo3"),
    odds_bezpieczenstwo = map_chr(data, odds_map, x = "Kat1_Bezpieczeństwo3"),
    n_efektywnosc_kosztowa = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Efektywność_kosztowa3"),
    odds_efektywnosc_kosztowa = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Efektywność_kosztowa3"),
    n_wpływ_budżet = map_int(data, possibly(odds_nMap, otherwise = NA), x = "Kat1_Wpływ_budżet3"),
    odds_wpływ_budżet = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Wpływ_budżet3")
  )



Analizy_pomniejszone %>% 
  filter(Kraj == "Wielka Brytania") %>% 
  count(Kat1_Efektywność_kosztowa2, Rekomendacja) 
 

Analizy_pomniejszone %>% 
  filter(Kraj == "Wielka Brytania") %>% 
  select(Kat1_Efektywność_kosztowa2, Rekomendacja) %>% 
  drop_na() %>% 
  table()


lista <- list()

for (i in 1:nrow(rekomendacje_nested2)) {
  
  lista[[i]] <- rekomendacje_nested2$data[[i]] %>% 
    select(Kat1_Efektywność_kosztowa2, Rekomendacja) %>% 
    drop_na() %>%
    table()
    
  
}



lista[[1]] %>% 
  kable()


cat("\n\n\n\n")
