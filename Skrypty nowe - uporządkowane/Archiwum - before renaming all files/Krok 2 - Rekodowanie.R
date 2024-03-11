
# Wypełniam brakujące wartości, których nie zostały zaimportowane  --------

Baza_razem2 <- Baza_razem %>% 
  fill(Nazwa_miedzynarodowa, .direction = "down") %>% 
  fill(ID, .direction = "down") #Tutaj dodatkowo usuwam zbędne zmienne





# Dodaje informację o liczbie ocenionych agencji --------------------------

ID_agencje <- Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj) %>% 
  unique() %>% 
  count(ID, Agencja_kraj) %>% 
  group_by(ID) %>% 
  summarise(liczba_agencji = sum(n)) 




Baza_razem2 <- Baza_razem2 %>% 
  left_join(ID_agencje)


Baza_razem2 %>% 
  select(ID, Nazwa_miedzynarodowa, liczba_agencji, Rekomendacja, Year) %>% 
  filter(is.na(liczba_agencji)) %>% 
  print(n = 100)


Baza_razem2 %>% 
  select(ID, Nazwa_miedzynarodowa, liczba_agencji, Rekomendacja, Year, Agencja_kraj) %>% 
  filter(liczba_agencji == 4) %>% 
  print(n = 100)

# Ustalam nazewnictwo kolumn ----------------------------------------------


nazwy1 <- str_c("Kat1_",names(Baza_razem2)[18:26])
nazwy2 <- str_c("Kat2_", names(Baza_razem2)[27:35]) %>% str_replace("_II", "")
nazwy <- c(nazwy1, nazwy2)
nazwy
names(Baza_razem2)[18:35] <- nazwy




# Sprawdzam popraność kodowania -------------------------------------------



Baza_razem2 %>% mutate(across(starts_with("Kat"), ~factor(.))) %>% 
  mutate(across(c(Kat1_Inne, Kat1_Komentarz, Kat2_Inne, Kat2_Komentarz), ~as.character(.))) %>% 
  select(starts_with("Kat")) %>% 
  sapply(levels)



# Sprowadzam zmienne do analizy do postaci factors ------------------------



Baza_razem3 <- Baza_razem2 %>% 
  mutate(across(starts_with("Kat"), ~factor(.))) %>%
  mutate(across(c(Kat1_Inne, Kat1_Komentarz, Kat2_Inne, Kat2_Komentarz), ~as.character(.)))
  

Baza_razem3 %>% select(Kat1_Populacja, Kat2_Populacja) %>% sapply(levels) #Jakiś problem z Kat2_populacje
Baza_razem3 %>% select(Kat1_Komparator, Kat2_Komparator) %>% sapply(levels)



# Rekoduję wartości czynników do postaci słownej --------------------------




Baza_razem4 <- Baza_razem3 %>% mutate(Kat1_Populacja = fct_recode(Kat1_Populacja, "On Label" ="0",
                                                   "Missing" = "9",
                                                   "Missing" = "nd",
                                                     "On Label" = "1",
                                                     "Restricted Label" = "2",
                                                   "Off Label" = "3",),
                       Kat1_Komparator = fct_recode(Kat1_Komparator, "Dobrze wybrany" = "1",
                                                    "Źle wybrany" = "-1",
                                                    "Missing" = "nd",
                                                    "Missing" = "9"),
                       Kat1_Korzyść_kliniczna = fct_recode(Kat1_Korzyść_kliniczna, "Wykazano przewagę" = "1",
                                                    "Nie wykazano przewagi" = "-1",
                                                    "Nie można stwierdzić jednoznacznie" = "0",
                                                    "Missing" = "nd",
                                                    "Missing" = "9"),
                       Kat1_Bezpieczeństwo = fct_recode(Kat1_Bezpieczeństwo, "Lepszy profil bezpieczeństwa" = "1",
                                                           "Porównywalny profil bezpieczeństwa" = "-1",
                                                          "Gorszy profil bezpieczeńśtwa" = "-2",
                                                           "Missing" = "nd",
                                                        "Nie można stwierdzić  " = "0",
                                                           "Missing" = "9"),
                       Kat1_RSS = fct_recode(Kat1_RSS, "Jest" = "1",
                                                    "Nie ma" = "0",
                                                    "Missing" = "nd",
                                                    "Missing" = "9",
                                             "Missing" = "99"),
                       Kat1_Efektywność_kosztowa = fct_recode(Kat1_Efektywność_kosztowa, "Wykazano przewagę" = "1",
                                                              "Nie dowiedziono przewagi" = "-1",
                                                              "Nieefektywny kosztowo" = "-2",
                                                              "Niepewność" = "2",
                                             "Nie można stwierdzić" = "0",
                                             "Missing" = "nd",
                                             "Missing" = "9"),
                       Kat1_Wpływ_budżet = fct_recode(Kat1_Wpływ_budżet, "Oszczędności" = "1",
                                                              "Dodatkowe obciążenie" = "-1",
                                                      "Neutralny" = "0",
                                                              "Niepewność " = "2",
                                                      "Nie można stwierdzić " = "-2",
                                                              "Missing" = "nd",
                                                              "Missing" = "9"),
                       Rekomendacja = factor(Rekomendacja, labels = c("Negatywna", "Pozytywna")),
                       dni_do_rekomendacji = difftime(Data_rekomendacji,Data_rejestracji, units = "day"),
                       Kraj = str_extract(Agencja_kraj, "[_].*$|[ ].*$") %>% 
                         str_replace("[_]|[ ]", "") %>% 
                         str_replace("BA[_]", "")) %>% 
  mutate(Is_Orphan = plyr::mapvalues(Is_Orphan, 1, 20),
         Biosimilar = plyr::mapvalues(Biosimilar, 1, 5),
         Kategoria_lek = Is_Orphan + Is_Generic + Biosimilar,
         Kategoria_lek = factor(Kategoria_lek, labels = c("Inne / Oryginalne", "Generic", "Biosimilar", "Orphan", "Unknown")),
         Obszar_terapeutyczny = Obszar_terapeutyczny %>% str_to_sentence())








# Sprawdzam poprawność rekodowania ----------------------------------------



Baza_razem4$Agencja_kraj %>% table()
Baza_razem4$Kraj %>% table()

Baza_razem4 %>% 
  select(starts_with("Kat1")) %>% 
  select(-c(Kat1_Inne, Kat1_Komentarz)) %>% 
sapply(table)
  #W ramach pierwszego komparatora populacja dwie dziwne wartości
  #W ramach RSS jest 7 0

Baza_razem4 %>% filter(Kat1_Komparator == 0)


Baza_razem4 %>% 
  count(Kategoria_lek)
  #Jeden lek jest unknown

Baza_razem4 %>% filter(Kategoria_lek == "Unknown")
Baza_razem4 %>% count(Kategoria_lek) 
Baza_razem4 %>% filter(Kategoria_lek == "Unknown") %>% count(Nazwa_miedzynarodowa)
Baza_razem2 %>% filter(Nazwa_miedzynarodowa == "Pemetrexed Hospira UK Limited")


Baza_razem4$Kat1_RSS %>% table()
Baza_razem4 %>% count(Nazwa_miedzynarodowa) %>% arrange(desc(n))
Baza_razem3$Kat1_RSS %>% table()

Baza_razem3 %>% count(Rekomendacja)
Baza_razem4 %>% count(Rekomendacja)
Baza_razem4 %>% count(Obszar_terapeutyczny) %>% print(n = 30)


Baza_razem4 %>% 
  filter(Kat1_Populacja %in% c("??", "???"))

# Naprawiam powyższe ------------------------------------------------------


Baza_razem4 <- Baza_razem4 %>% 
  mutate(
    Kat1_Komparator = plyr::mapvalues(Kat1_Komparator, 0, NA),
    Kategoria_lek = if_else(Nazwa_miedzynarodowa == "Pemetrexed Hospira UK Limited", "Generic", Kategoria_lek %>% 
                              as.character() ) %>% factor(),
    Kat1_Populacja = plyr::mapvalues(Kat1_Populacja, c("??", "???"), c("On Label", "On Label"))
    ) %>% 
  mutate(Obserwacja_nr = row_number())




# Zapisuję dwie bazy ------------------------------------------------------



Baza_analizy1 <- Baza_razem4 %>% 
  select(Kategoria_lek, Obszar_terapeutyczny,  Year, Kraj, starts_with("Kat1"), liczba_agencji,
         Rekomendacja, -c(Kat1_Inne, Kat1_Komentarz)) %>% 
  mutate(across(everything(), ~na_if(., "Missing")))


Baza_razem4 %>% 
  write_csv("Baza razem - 21 lutego 2022.csv")

Baza_analizy1 %>% 
  write_csv("Baza danych - 21 lutego 2022.csv")




