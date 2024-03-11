

# Krok 13 - Modele i częstości dla odniesienia ----------------------------



library(epitools)
library(rcompanion)
library(modelr)
library(caret)
library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")

source("./Funkcje/Krok 12 - Funkcje pomocnicze.R")
source("./Funkcje/Krok 13 - Funkcja częstości with 0.R")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")




# Parameters --------------------------------------------------------------

add.one <- F
baza.filter <- 0


# Przekształcam do postaci odniesienie vs brak odniesienia ----------------



Baza_odniesienie <-  Baza_analizy1 %>% 
  mutate(across(starts_with("Kat"), ~if_else(is.na(.) == T, "Brak odniesienia", "Odniesienie") %>% factor(), 
                .names = "{.col}_2")) %>% 
  mutate(across(starts_with("Kat"), ~if_else(is.na(.) == T, 0, 1) %>% factor(), 
                .names = "{.col}_3")) %>% 
  mutate(Rekomendacja2 = plyr::mapvalues(Rekomendacja, c("Pozytywna", "Negatywna"), 1:0) %>% as.double()) 


Baza_odniesienie <- Baza_odniesienie %>% 
  filter(liczba_agencji >= baza.filter)

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




odniesienia_nested <- Baza_odniesienie %>% 
  group_by(Kraj) %>% 
  nest() %>% 
  filter(!Kraj %in% c("Szwecja"))


formuły <- c(
  Polska = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  `Wielka Brytania` = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Szkocja = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Walia = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Irlandia = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Francja = "Kat1_Korzyść_kliniczna2 + Kat1_Bezpieczeństwo2",
  Holandia = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Niemcy = "Kat1_Korzyść_kliniczna2",
  Norwegia = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Australia = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  `Nowa Zelandia` = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2",
  Kanada = "Kat1_Korzyść_kliniczna_2 + Kat1_Bezpieczeństwo_2 + Kat1_Efektywność_kosztowa_2 + Kat1_Wpływ_budżet_2"
)




model_full <- odniesienia_nested %>%
  ungroup() %>% 
  mutate(
    Predyktory = formuły,
    full_model = map2(data, Predyktory, possibly(model_log, otherwise = NA)),
    full_model2 = map(full_model, possibly(tidy_full, otherwise = NA)),
    n = map_int(full_model, possibly(tidy_nobs, otherwise = NA))
  )



Baza_odniesienie %>% 
  select(Kraj, Kat1_Korzyść_kliniczna_2 , Kat1_Bezpieczeństwo_2, Kat1_Efektywność_kosztowa_2, Kat1_Wpływ_budżet_2) %>% 
  pivot_longer(-1) %>% 
  drop_na() %>% 
  select(Kraj, name) %>% 
  table()




modele_whole <- model_full %>% 
  select(Kraj, full_model2, n) %>% 
  unnest(full_model2) %>% 
  pivot_wider(names_from = "term", values_from = "estimate2")  %>% 
  select(-`NA`)


model_full %>% 
  mutate(acc = map2(data, full_model, possibly(add_accuracy, otherwise = ""))  )

# McNemmar




oddsy_whole <- odniesienia_nested %>% 
  mutate(
    n = map_int(data, odds_nMap, x = "Kat1_Korzyść_kliniczna_2"),
    odds_korzyść_klincizna = map_chr(data, odds_map, x = "Kat1_Korzyść_kliniczna_2", add.one = add.one),
    odds_bezpieczenstwo = map_chr(data, odds_map, x = "Kat1_Bezpieczeństwo_2", add.one = add.one),
    odds_efektywnosc_kosztowa = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Efektywność_kosztowa_2", add.one = add.one),
    odds_wpływ_budżet = map_chr(data, possibly(odds_map, otherwise = "Brak danych"), x = "Kat1_Wpływ_budżet_2", add.one = add.one)
  ) %>% 
  ungroup() %>% 
  select(-data) %>% 
  mutate(ID = 1:12)



# Tabele częstości

frequencies_whole <- odniesienia_nested %>% 
  mutate(
    korzysc = map(data, czestosci_all, "Kat1_Korzyść_kliniczna_2", add.one = add.one),
    bezpieczenstwo = map(data, czestosci_all, "Kat1_Bezpieczeństwo_2", add.one = add.one),
    efektywnosc = map(data, czestosci_all, "Kat1_Efektywność_kosztowa_2", add.one = add.one),
    budzet = map(data, czestosci_all, "Kat1_Wpływ_budżet_2", add.one = add.one)
    ) %>% 
  unnest(korzysc, bezpieczenstwo, efektywnosc, budzet) %>% 
  ungroup() %>% 
  select(-c(data, Rekomendacja1, Rekomendacja2, Rekomendacja3)) %>% 
  mutate(ID = rep(1:12, 2) %>% sort(),
         n = NA)


names(oddsy_whole)[3:6] <- names(frequencies_whole)[seq(3,9,2)]

tabela_full <- frequencies_whole %>% 
  add_row(oddsy_whole)  %>% 
  arrange(ID) %>% 
  select(Kraj, n, everything()) %>% 
  mutate(Rekomendacja = replace_na(Rekomendacja, "Odds Ratio"))





# Wykresy  ----------------------------------------------------------------



Baza_odniesienie %>% 
  filter(Kraj != "Szwecja") %>% 
  rstatix::freq_table(Kraj, Kat1_Korzyść_kliniczna_2) %>% 
  ggplot(aes(Kraj, prop, fill = Kat1_Korzyść_kliniczna_2)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  coord_flip()
  



graph_odniesienia <- function(x, kategoria) {
  
  x %>% 
    filter(Kraj != "Szwecja") %>% 
    rstatix::freq_table(Kraj, .data[[kategoria]]) %>% 
    ggplot(aes(Kraj, prop, fill = fct_rev(.data[[kategoria]]))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), alpha = 2/3) +
    coord_flip() +
    ggsci::scale_fill_lancet() +
    theme(legend.position = "bottom") +
    labs(x = "", fill = "", y = "Procent") +
    scale_y_continuous(breaks = seq(0,100,20), labels = str_c(seq(0,100,20), "%"))
    
}

Baza_odniesienie %>% 
  graph_odniesienia("Kat1_Bezpieczeństwo_2")





wektor_kategorie <- c(
  "Kat1_Korzyść_kliniczna_2",
  "Kat1_Bezpieczeństwo_2",
  "Kat1_Efektywność_kosztowa_2",
  "Kat1_Wpływ_budżet_2"
)



graphs_list <- map(wektor_kategorie, graph_odniesienia, x = Baza_odniesienie)



