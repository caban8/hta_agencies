library(rstatix)
library(tidyverse)
library(ggsci)
library(ggthemes)
library(flextable)
Sys.setlocale("LC_ALL", "Polish")
Baza_analizy1 <- read_csv("Baza danych - 21 lutego 2022.csv") 


source("./Skrypty nowe - uporządkowane/Krok 11_5 - Funkcje.R")


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
  select(Obszar_terapeutyczny, Rekomendacja, liczba_agencji,
         Kat1_Korzyść_kliniczna2, Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2,  Kat1_Wpływ_budżet2,
         Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo, Kat1_Efektywność_kosztowa,  Kat1_Wpływ_budżet
  )






# Zawężona próba - minimum 6 rekomendacji ---------------------------------


Analizy_zawężone <- Analizy_częstości %>% 
  filter(liczba_agencji >= 6) %>% 
  select(-liczba_agencji)

Kategorie1 <- filtr_kategorie(Analizy_zawężone)
Kategorie2 <- filtr_kategorie(Analizy_zawężone, kategoria = 2)

nested_full <- join_nested(Kategorie1, Kategorie2)



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


nested_full3 <-   nested_full2 %>% 
  mutate(
    freq_all = map2(ocena_freq2, korzystność_freq2, function(x, y) {x %>% left_join(y) %>% tibble(.name_repair = "universal")}),
    freq_all2 = map2(freq_all, name, kryterium_order),
    flextabela = map(freq_all2, flextable),
    Nazwa_tabeli = str_c("Częstości dla leków zawężonych - rekomendacja ", Rekomendacja, " i kryterium ", name)
  ) 

nested_full3 %>% 
  filter(Rekomendacja == "Negatywna", name == "Wpływ budżet") %>% 
  select(freq_all2) %>% 
  unnest(freq_all2)

nested_full3 %>% 
  filter(Rekomendacja == "Negatywna") %>% 
  select(freq_all2) %>% 
  unnest(freq_all2)



nested_full3 %>% 
  filter(Rekomendacja == "Negatywna")   %>% 
  select(freq_all2)



# Sprawdzam poprawność tabelki --------------------------------------------

Analizy_zawężone %>% 
  filter(Rekomendacja == "Pozytywna") %>% 
  freq_table(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna)

Analizy_zawężone %>% 
  filter(Rekomendacja == "Negatywna") %>% 
  freq_table(Obszar_terapeutyczny, Kat1_Bezpieczeństwo)


# Zapisywanie -------------------------------------------------------------






full_tabele <- nested_full3 %>% 
  select(Nazwa_tabeli, flextabela) 



for (i in 1:nrow(full_tabele)) {
  
  save_as_docx(full_tabele$flextabela[[i]], path = str_c("./Tabele częstości/Zawężona próba/", full_tabele$Nazwa_tabeli[[i]], ".docx"))
  
}




nested_full4 <- nested_full3 %>% 
  ungroup() %>% 
  mutate(ID = c(1,3,2,4) %>% rep(2)) %>% 
  arrange(ID)



negatywne_reduced <- nested_full4 %>% 
  filter(Rekomendacja == "Negatywna")   %>% 
  select(freq_all2)

negatywne_reduced$freq_all2 %>% 
  reduce(left_join, by = "Obszar_terapeutyczny") %>% 
  write_excel_csv("./Tabel częstości 2 (obszar terapeutyczny)/Obszary terapeutyczne - negatywne.xlsx")


pozytywne_reduced <- nested_full4 %>% 
  filter(Rekomendacja == "Pozytywna")   %>% 
  select(freq_all2)

pozytywne_reduced$freq_all2 %>% 
  reduce(left_join, by = "Obszar_terapeutyczny") %>% 
  write_excel_csv("./Tabel częstości 2 (obszar terapeutyczny)/Obszary terapeutyczne - pozytywne.xlsx")






