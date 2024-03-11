
# DF - Analizy dla kategorii kryteriów: korzystne vs niekorzystne ---------


source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")

Baza_analizy1 <- read_csv("Baza danych - 21 lutego 2022.csv") 


# Numeracja 4 - Nawiązano / nie nawiązano ---------------------------------


Baza_analizy1 %>% 
  mutate(across(starts_with("Kat"), ~if_else(is.na(.) == T, "Brak odniesienia", "Odniesienie") %>% factor(), 
                .names = "{.col}4"))

Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Rekomendacja) %>% drop_na() %>% 
  count(Obszar_terapeutyczny) %>% 
  arrange(desc(n))

# Rekoduję kategorie na korzystne i niekorzystne --------------------------


# numeracja 2 - z wykluczeniem "Nie można stwierdzić" jako brak danych






Analizy_korzyść <- Baza_analizy1 %>% 
  mutate(Kat1_Korzyść_kliniczna2 = fct_recode(Kat1_Korzyść_kliniczna,
                                              "Korzystna" = "Wykazano przewagę" ,
                                              "Niekorzystna"  = "Nie wykazano przewagi" ,
                                              NULL  = "Nie można stwierdzić jednoznacznie"
                                              ),
         Kat1_Bezpieczeństwo2 = fct_recode(Kat1_Bezpieczeństwo, 
                                  "Korzystna" = "Lepszy profil bezpieczeństwa" ,
                                  "Korzystna" = "Porównywalny profil bezpieczeństwa",
                                  "Niekorzystna" = "Gorszy profil bezpieczeńśtwa",
                                  NULL = "Nie można stwierdzić" 
                                  ),
         Kat1_Efektywność_kosztowa2 = fct_recode(Kat1_Efektywność_kosztowa, 
                                        "Korzystna"  = "Wykazano przewagę",
                                        "Niekorzystna"  = "Nie dowiedziono przewagi",
                                        "Niekorzystna"  = "Nieefektywny kosztowo",
                                        "Niekorzystna"  = "Niepewność",
                                        NULL  = "Nie można stwierdzić"
                                        ),
         Kat1_Wpływ_budżet2 = fct_recode(Kat1_Wpływ_budżet, 
                                "Korzystna" = "Oszczędności",
                                "Korzystna" = "Neutralny",
                                "Niekorzystna" = "Dodatkowe obciążenie",
                                "Niekorzystna" = "Niepewność",
                                NULL = "Nie można stwierdzić"
                                )
         )





# numeracja 3 - z uwzględnieniem "Nie można stwierdzić" jako niekorzystne
  # UWAGA - tutaj numeracja pytań to 3, a wcześniej 2
  # Dlatego miałem mind-fucka, że poniżej nadpisuję baze danych, jesus!

Analizy_korzyść <- Analizy_korzyść %>% mutate(Kat1_Korzyść_kliniczna3 = fct_recode(Kat1_Korzyść_kliniczna, 
                                                                                 "Korzystna" = "Wykazano przewagę" ,
                                                                                 "Niekorzystna"  = "Nie wykazano przewagi" ,
                                                                                 "Niekorzystna"  = "Nie można stwierdzić jednoznacznie" 
) ,
Kat1_Bezpieczeństwo3 = fct_recode(Kat1_Bezpieczeństwo, 
                                  "Korzystna" = "Lepszy profil bezpieczeństwa" ,
                                  "Korzystna" = "Porównywalny profil bezpieczeństwa",
                                  "Niekorzystna" = "Gorszy profil bezpieczeńśtwa",
                                  "Niekorzystna" = "Nie można stwierdzić" 
)  ,
Kat1_Efektywność_kosztowa3 = fct_recode(Kat1_Efektywność_kosztowa, 
                                        "Korzystna"  = "Wykazano przewagę",
                                        "Niekorzystna"  = "Nie dowiedziono przewagi",
                                        "Niekorzystna"  = "Nieefektywny kosztowo",
                                        "Niekorzystna"  = "Niepewność",
                                        "Niekorzystna"  = "Nie można stwierdzić"
)  ,
Kat1_Wpływ_budżet3 = fct_recode(Kat1_Wpływ_budżet, 
                                "Korzystna" = "Oszczędności",
                                "Korzystna" = "Neutralny",
                                "Niekorzystna" = "Dodatkowe obciążenie",
                                "Niekorzystna" = "Niepewność",
                                "Niekorzystna" = "Nie można stwierdzić"
),
Rekomendacja2 = plyr::mapvalues(Rekomendacja, c("Pozytywna", "Negatywna"), 1:0) %>% as.double()
)



# Próba pomniejszona ------------------------------------------------------


Analizy_pomniejszone <- Analizy_korzyść %>%
  filter(liczba_agencji >= 6) 

Analizy_korzyść2 <- Analizy_korzyść %>% 
  mutate(
    Kraj = plyr::mapvalues(Kraj, kraje_pl, kraje_ang) %>% factor(levels = kraje_ang),
    Obszar_terapeutyczny = plyr::mapvalues(Obszar_terapeutyczny, Obszary_old, Obszary_new),
    Rekomendacja = plyr::mapvalues(Rekomendacja, c("Pozytywna", "Negatywna"), c("Positive", "Negative"))
  )  


