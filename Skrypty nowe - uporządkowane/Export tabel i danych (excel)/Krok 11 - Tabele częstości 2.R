library(tidyverse)
library(ggsci)
library(ggthemes)
library(flextable)
Sys.setlocale("LC_ALL", "Polish")
Baza_analizy1 <- read_csv("Baza danych - 21 lutego 2022.csv") 

source("./Funkcje/Krok 11 - Funkcje.R", encoding = "UTF-8")


# Przygotowanie zmiennych -------------------------------------------------


Analizy_częstości <-  Baza_analizy1 %>% mutate(Kat1_Korzyść_kliniczna2 = fct_recode(Kat1_Korzyść_kliniczna, 
                                                                                    "Korzystna" = "Wykazano przewagę" ,
                                                                                    "Niekorzystna"  = "Nie wykazano przewagi" ,
                                                                                    "Niekorzystna"  = "Nie można stwierdzić jednoznacznie" 
) ,
Kat1_Bezpieczeństwo2 = fct_recode(Kat1_Bezpieczeństwo, 
                                  "Korzystna" = "Lepszy profil bezpieczeństwa" ,
                                  "Korzystna" = "Porównywalny profil bezpieczeństwa",
                                  "Niekorzystna" = "Gorszy profil bezpieczeńśtwa",
                                  "Niekorzystna" = "Nie można stwierdzić" 
)  ,
Kat1_Efektywność_kosztowa2 = fct_recode(Kat1_Efektywność_kosztowa, 
                                        "Korzystna"  = "Wykazano przewagę",
                                        "Niekorzystna"  = "Nie dowiedziono przewagi",
                                        "Niekorzystna"  = "Nieefektywny kosztowo",
                                        "Niekorzystna"  = "Niepewność",
                                        "Niekorzystna"  = "Nie można stwierdzić"
)  ,
Kat1_Wpływ_budżet2 = fct_recode(Kat1_Wpływ_budżet, 
                                "Korzystna" = "Oszczędności",
                                "Korzystna" = "Neutralny",
                                "Niekorzystna" = "Dodatkowe obciążenie",
                                "Niekorzystna" = "Niepewność",
                                "Niekorzystna" = "Nie można stwierdzić"
)   ) %>% 
  select(Kraj, Rekomendacja, liczba_agencji, Obszar_terapeutyczny,
         Kat1_Korzyść_kliniczna2, Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2,  Kat1_Wpływ_budżet2,
         Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo, Kat1_Efektywność_kosztowa,  Kat1_Wpływ_budżet
  )





# Przygotowanie nested dataset --------------------------------------------



nested_freq <- tibble(
  Filtr = rep(c(0,6), 2),
  zmienna = c("Kraj", "Kraj", "Obszar_terapeutyczny",  "Obszar_terapeutyczny"),
  data = list(Analizy_częstości)
)



# -------------------------------------------------------------------------


nested_freq2 <- nested_freq %>% 
  mutate(
    data2 = map2(data, Filtr, function(x, filtr) {x %>% filter(liczba_agencji >= filtr)}),
    Kategoria1 = map2(data2, zmienna, filtr_kategorie, kategoria = 1),
    Kategoria2 = map2(data2, zmienna, filtr_kategorie, kategoria = 2),
    nested_full = map2(Kategoria1, Kategoria2, join_nested)
    )

filtr_kategorie(Analizy_częstości, zmienna = "Obszar_terapeutyczny")


Kategorie1 <- filtr_kategorie(Analizy_częstości)
Kategorie2 <- filtr_kategorie(Analizy_częstości, kategoria = 2)

nested_full <- join_nested(Kategorie1, Kategorie2)
nested_full
nested_freq2

nested_freq2$nested_full[[1]]$Ocena_pełna
nested_freq2$nested_full[[1]]$Korzystność
nested_freq2$nested_full


nested_freq3 <- nested_freq2
nested_freq3$nested_full[[i]]

zmienna2 <- nested_freq2$zmienna %>% as_vector()


for (i in 1:nrow(nested_freq2)) {
  
  nested_freq3$nested_full[[i]] <- nested_freq2$nested_full[[i]] %>% 
    mutate(
      ocena_freq = map(Ocena_pełna, freq_nested  ),
      korzystność_freq = map(Korzystność, freq_nested  ),
      ocena_freq2 = map(ocena_freq, freq_transform),
      korzystność_freq2 = map(korzystność_freq, freq_transform ),
      freq_all = map2(ocena_freq2, korzystność_freq2, function(x, y) {x %>% left_join(y) %>% tibble(.name_repair = "universal")}),
      freq_all2 = map2(freq_all, name, kryterium_order, zmienna = zmienna2[[i]])
    )  
  
}

nested_full3
nested_full3$freq_all[[1]]
nested_freq3$nested_full[[1]]$freq_all[[1]]


# -------------------------------------------------------------------------


nested_full
nested_full <- nested_full %>% 
  mutate(
    ocena_freq = map(Ocena_pełna, freq_nested  ),
    korzystność_freq = map(Korzystność, freq_nested  )
  )  

nested_full2 <- nested_full %>% 
  mutate(
    ocena_freq2 = map(ocena_freq, freq_transform),
    korzystność_freq2 = map(korzystność_freq, freq_transform )
  ) 
nested_full2

nested_full3 <-   nested_full2 %>% 
  mutate(
    freq_all = map2(ocena_freq2, korzystność_freq2, function(x, y) {x %>% left_join(y) %>% tibble(.name_repair = "universal")}),
    freq_all = map(freq_all, kraje_kolejność),
    freq_all2 = map2(freq_all, name, kryterium_order),
    flextabela = map(freq_all2, flextable),
    Nazwa_tabeli = str_c("Częstości dla wszystkich leków - rekomendacja ", Rekomendacja, " i kryterium ", name)
  ) 


nested_full3



# Zapisywanie -------------------------------------------------------------


nested_freq4 <- nested_freq3 %>% 
  select(Filtr, zmienna, nested_full)

nested_freq4$nested_full

#Excel

nested_freq5 <- nested_freq4 %>% 
  mutate(
    Pozytywne = map2(nested_full, zmienna, function(x, zmienna) {x %>% filter(Rekomendacja == "Pozytywna") %>% select(freq_all2) }),
    Negatywne = map2(nested_full, zmienna, function(x, zmienna) {x %>% filter(Rekomendacja == "Negatywna") %>% select(freq_all2) })
    )


pozytywne_reduced <- list()
negatywne_reduced <- list()



for (i in 1:nrow(nested_freq5)) {
  pozytywne_reduced[[i]] <-  nested_freq5$Pozytywne[[i]]$freq_all2 %>% 
    reduce(left_join, by = zmienna2[i])
  negatywne_reduced[[i]] <-  nested_freq5$Negatywne[[i]]$freq_all2 %>% 
    reduce(left_join, by = zmienna2[i])
}


nested_freq6 <- nested_freq5 %>% 
  mutate(
    pozytywne_reduced = pozytywne_reduced,
    negatywne_reduced = negatywne_reduced,
    output_pozytywne = str_c("./Frontiers/Excel/Analiza dla pozytywnych (min. agencji = ", Filtr, " zmienna = )", zmienna, ".xlsx"),
    output_negatywne = str_c("./Frontiers/Excel/Analiza dla negatywnych (min. agencji = ", Filtr, " zmienna = )", zmienna, ".xlsx")
    )


nested_freq6 %>% 
  select(x = pozytywne_reduced, file = output_pozytywne)  %>% 
  pwalk(write_excel_csv)

nested_freq6 %>% 
  select(x = negatywne_reduced, file = output_negatywne)  %>% 
  pwalk(write_excel_csv)

