# Krok X - wstępne zabawy z modelami logistycznymi ------------------------

library(modelr)
library(caret)
library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")

source("./Funkcje/Krok 12 - Funkcje pomocnicze.R")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")


#https://stats.stackexchange.com/questions/254472/what-is-a-high-standard-error-in-logistic-regression


# Modele dwu-zmiennowe włączająć "Nie można stwierdzić"


rekomendacje_nested <- Analizy_korzyść %>% 
  group_by(Kraj) %>% 
  nest() %>% 
  filter(!Kraj %in% c("Szwecja"))




Modele1 <- rekomendacje_nested  %>% 
  mutate(
    mod_korzysc = map2(data,"Kat1_Korzyść_kliniczna2", safely(model_log)) %>% map(1),
    mod_bezpieczenstwo = map2(data,"Kat1_Bezpieczeństwo2", safely(model_log)) %>% map(1),
    mod_efektywnosc = map2(data,"Kat1_Efektywność_kosztowa2", safely(model_log)) %>% map(1),
    mod_budzet = map2(data,"Kat1_Wpływ_budżet2", safely(model_log)) %>% map(1)
    ) 


Modele1_se <- Modele1 %>% 
  mutate(
    Korzyść_kliniczna = map_chr(mod_korzysc, possibly(tidy_IV, otherwise = "")),
    Bezpieczeństwo = map_chr(mod_bezpieczenstwo, possibly(tidy_IV, otherwise = "")),
    Efektywność_kosztowa = map_chr(mod_efektywnosc, possibly(tidy_IV, otherwise = "")),
    Wpływ_budżet = map_chr(mod_budzet, possibly(tidy_IV, otherwise = ""))
  )



Modele1_cf <- Modele1 %>% 
  mutate(
    Korzyść_kliniczna = map_chr(mod_korzysc, conf.int = T,  possibly(tidy_IV, otherwise = "")),
    Bezpieczeństwo = map_chr(mod_bezpieczenstwo, conf.int = T, possibly(tidy_IV, otherwise = "")),
    Efektywność_kosztowa = map_chr(mod_efektywnosc, conf.int = T, possibly(tidy_IV, otherwise = "")),
    Wpływ_budżet = map_chr(mod_budzet, conf.int = T, possibly(tidy_IV, otherwise = ""))
  )



# Dlaczego takie dziwne standard error ------------------------------------



Modele1_1$mod_efektywnosc[[2]] %>% broom::glance()
Modele1_1$mod_efektywnosc[[2]] %>% broom::tidy(conf.int = T)

Analizy_korzyść %>% 
  filter(Kraj == "Holandia") %>% 
  count(Rekomendacja, Kat1_Bezpieczeństwo2)


glm(Rekomendacja2 ~ Kat1_Efektywność_kosztowa2, family = "binomial",
  data = Analizy_korzyść %>% 
    filter(Kraj == "Wielka Brytania")) %>% broom::tidy()




# Ocena czułości, swoistości i precyzji modeli ----------------------------



accuracy1 <- Modele1_1 %>% 
  mutate(pred_korzysc = map2(data, mod_korzysc, add_predictions, type = "response")) %>% 
  select(pred_korzysc)

accuracy1$pred_korzysc[[2]] %>% 
  select(pred) %>% 
  summary()

accuracy2 <- accuracy1$pred_korzysc[[2]] %>% 
  select(Rekomendacja2, pred) %>% 
  mutate(predicted = if_else(pred > 0.5, 1, 0)) %>% 
  select(predicted,Rekomendacja2) %>% 
  table()

accuracy2

 accuracy1$pred_korzysc[[10]] %>% 
  select(Rekomendacja2, pred) %>% 
  mutate(predicted = if_else(pred > 0.5, 1, 0)) %>% 
  select(predicted,Rekomendacja2) %>% 
  table() 
  

accuracy2$overall[1]
accuracy2$byClass[1:2]




Modele1 %>% 
  filter(Kraj %in% c("Polska")) %>% 
  mutate(pred_korzysc = map2(data, mod_korzysc, add_accuracy)) 

Modele1$data[[2]] %>% 
  add_accuracy(Modele1$mod_korzysc[[2]])

Modele1 %>% 
  mutate(pred_korzysc = map2(data, mod_korzysc, possibly(add_accuracy, otherwise = ""))  )

Modele1$data[[10]] %>% 
  add_accuracy(Modele1$mod_korzysc[[10]])

# Modele pełne ------------------------------------------------------------



Analizy_korzyść %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2 , Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2, Kat1_Wpływ_budżet2) %>% 
  pivot_longer(-1) %>% 
  drop_na() %>% 
  select(Kraj, name) %>% 
  table()
  

glm(Rekomendacja2 ~ Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2,
    family = "binomial", data = rekomendacje_nested$data[[11]])


formuły <- c(
  Polska = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    `Wielka Brytania` = "Kat1_Korzyść_kliniczna2 +  Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 ",
    Szkocja = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    Walia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    Irlandia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2",
    Francja = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
    Holandia = "Kat1_Korzyść_kliniczna2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    Niemcy = "Kat1_Korzyść_kliniczna2",
    Norwegia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    Australia = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2",
    `Nowa Zelandia` = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
    Kanada = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2 + Kat1_Efektywność_kosztowa2 + Kat1_Wpływ_budżet2"
)


Modele1_1
model_full
rekomendacje_nested 


model_full <- rekomendacje_nested %>%
  ungroup() %>% 
  mutate(
    formulas = formuły,
    full_model = map2(data, formulas, possibly(model_log, otherwise = NA)),
    full_model2 = map(full_model, possibly(tidy_full, otherwise = NA))
    )

model_full %>% 
  select(Kraj, full_model2) %>% 
  unnest() %>% 
  pivot_wider(names_from = "term", values_from = "estimate2")


model_full %>% 
  mutate(acc = map2(data, full_model, possibly(add_accuracy, otherwise = ""))  )


model_full$data[[12]] %>% 
  add_accuracy(model_full$full_model[[12]])


model_full$full_model[[1]] %>% 
  broom::tidy()




