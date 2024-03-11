

#6b Analiza - rekomendacja a ustalona wartość w ramach rozpatrywanych kategorii / kryterium. W podziale obszar terapeutyczny




# Wpierw, tworzę funkcje pomocnicze ---------------------------------------



#Funkcja do zliczania częstości
funkcja_czestosci <- function(x, grouping, kategoria, filtr = NULL, mode = 1) {
  
  
  if (mode == 1) {
    x <- x %>% dplyr::filter({{grouping}} %in% filtr)
  } else if (mode == 2) {
    x <- x %>% dplyr::filter(!{{grouping}} %in% filtr)
  }  else {}
  
  
  x %>% 
    select({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    drop_na() %>%
    count({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    group_by({{grouping}}, .data[[kategoria]]) %>% 
    mutate(percent = n / sum(n) * 100) %>% 
    filter(Rekomendacja == "Pozytywna") 
}


funkcja_czestosci(Baza_analizy1,  Obszar_terapeutyczny, "Kat1_Populacja", mode = 3)




#Funkcja do obliczenia chi-kwadrat
funkcja_chiKwadrat <- function(x, grouping, kategoria, filtr = NULL, mode = 1) {
 
  
  if (mode == 1) {
    x <- x %>% dplyr::filter({{grouping}} %in% filtr)
  } else if (mode == 2) {
    x <- x %>% dplyr::filter(!{{grouping}} %in% filtr)
  }  else {}
  
  
  
x %>% 
    select({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    drop_na() %>% 
    group_by({{grouping}}) %>% 
    summarise(chisq_test(.data[[kategoria]], Rekomendacja))
  
}

funkcja_chiKwadrat(Baza_analizy1,  Obszar_terapeutyczny, "Kat1_Populacja", mode = 3)

#Funkcja połączenia
funkcja_join <- function(x, y, kategoria, filtr) {
  left_join(x ,y, by = "Obszar_terapeutyczny") %>% 
    mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
           p2 = if_else(.data[[kategoria]] == filtr, p2, ".") %>% na_if("."),
           p = replace_na(p, 1.1),
           significant = if_else(p < 0.05, T, F),
           Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))
}

#Funkcja do wykresu
funkcja_ggplot <- function(x, kategoria) {
  
  x %>% 
    ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = .data[[kategoria]], alpha = significant)) +
    geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
    scale_alpha_discrete(range = c(0.25, 0.75)) +
    guides(alpha = F) +
    coord_flip() + 
    scale_fill_calc() +
    scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
    labs(y = "", fill = "", x = "") +
    geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
    theme_economist() +
    theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
    guides(fill = guide_legend(reverse = T))
  
  
}



# Wektory referencyjne ----------------------------------------------------

#Do zaktualizowania w związku z nowymi danymi

filtr_populacja1 <- c("Choroby krwi i narządów krwiotwórczych","Choroby rzadkie","Choroby układu mięśniowo-szkieletowego",
                      "Choroby zakaźne i infekcyjne","Dermatologia","Diabetologia",
                      "Gastroenterologia","Ginekologia","Immunologia",
                      "Kardiologia","Nefrologia","Neurologia",
                      "Okulistyka","Onkologia","Psychiatria",
                      "Pulmonologia","Urologia") 
filtr_populacja2 <- c("Choroby krwi i narządów krwiotwórczych","Choroby rzadkie","Choroby układu mięśniowo-szkieletowego",
                      "Choroby zakaźne i infekcyjne","Dermatologia","Diabetologia",
                      "Gastroenterologia","Immunologia",
                      "Kardiologia","Nefrologia","Neurologia",
                      "Okulistyka","Onkologia","Psychiatria",
                      "Pulmonologia","Urologia")
filtr_komparator1 <- c("Choroby rzadkie" , "Neurologia" , "Immunologia" , "Onkologia")
filtr_komparator2 <- c("Onkologia")
filtr_bezpieczeństwo12 <- c("Choroby krwi i narządów krwiotwórczych","Choroby rzadkie","Choroby zakaźne i infekcyjne",
                            "Diabetologia","Immunologia","Kardiologia",
                            "Nefrologia","Neurologia","Okulistyka",
                            "Onkologia","Psychiatria","Pulmonologia",
                            "Urologia"    ) 
filtr_RSS1 <- c("Kardiologia" , "Neurologia" , "Choroby zakaźne i infekcyjne" , "Choroby rzadkie" , "Immunologia" , "Onkologia")
filtr_RSS2 <- c("Kardiologia" , "Neurologia" , "Choroby zakaźne i infekcyjne" , "Onkologia")
filtr_Wpływbudżet12 <- c("Okulistyka" , "Psychiatria" , "Kardiologia" , "Choroby krwi i narządów krwiotwórczych" , 
                        "Diabetologia" , "Immunologia" , "Pulmonologia" , "Neurologia" , "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" ,
                        "Onkologia")
filtr_korzyśćKliniczna1 <- c("Dermatologia" , "Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                             "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , 
                             "Immunologia" ,"Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")
filtr_korzyśćKliniczna2 <- c("Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                             "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , 
                             "Immunologia" ,"Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")
filtr_efektywnośćKosztowa12 <- c("Nefrologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , "Choroby krwi i narządów krwiotwórczych" ,
                                "Kardiologia" ,"Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" , "Choroby rzadkie" ,
                                "Choroby zakaźne i infekcyjne" , "Onkologia")


filtr_join <- c("On Label", "Dobrze wybrany", "Nie można stwierdzić  ", "Jest", "Oszczędności", "Nie wykazano przewagi", "Wykazano przewagę")


# Listy referencyjne ------------------------------------------------------

lista_kategorie <- c(Populacja = "Kat1_Populacja",
                     Komparator = "Kat1_Komparator",
                     Bezpieczeństwo = "Kat1_Bezpieczeństwo",
                     RSS = "Kat1_RSS",
                     Wpływ_budżet = "Kat1_Wpływ_budżet",
                     Korzyść_kliniczna = "Kat1_Korzyść_kliniczna",
                     Efektywność_kosztowa = "Kat1_Efektywność_kosztowa") 
lista_czestosci <- list(filtr_populacja1, filtr_komparator1, filtr_bezpieczeństwo12, filtr_RSS1, filtr_Wpływbudżet12, filtr_korzyśćKliniczna1,
                        filtr_efektywnośćKosztowa12)
lista_chikwadrat <- list(filtr_populacja2, filtr_komparator2, filtr_bezpieczeństwo12, filtr_RSS2, filtr_Wpływbudżet12, filtr_korzyśćKliniczna2,
                        filtr_efektywnośćKosztowa12)




# Analizy chi-kwadrat -----------------------------------------------------





# Utworzenie list częstości i chi-kwadrat ---------------------------------



obszar_czestosci <-  map2(lista_kategorie, lista_czestosci, funkcja_czestosci, x = Baza_analizy1, grouping = Obszar_terapeutyczny)
obszar_chikwadrat <- map2(lista_kategorie, lista_chikwadrat, funkcja_chiKwadrat, x = Baza_analizy1, grouping = Obszar_terapeutyczny)



# Uzupełniam, gdzie trzeba, brakujące obserwacje dla chi-kwadrat ----------

  #Do zaktualizowania w związku z nowymi danymi

obszar_chikwadrat$Populacja <- obszar_chikwadrat$Populacja %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Ginekologia"), n = replace_na(n, 13))


obszar_chikwadrat$Komparator <- obszar_chikwadrat$Komparator %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Immunologia"), n = replace_na(n, 15)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Neurologia"), n = replace_na(n, 13))


obszar_chikwadrat$RSS <- obszar_chikwadrat$RSS %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Choroby rzadkie"), n = replace_na(n, 27)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Immunologia"), n = replace_na(n, 30))


obszar_chikwadrat$Korzyść_kliniczna <- obszar_chikwadrat$Korzyść_kliniczna %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Dermatologia"), n = replace_na(n, 12))




# Łącze częstości z chi-kwadrat -------------------------------------------

lista_join <- list(obszar_czestosci, obszar_chikwadrat, lista_kategorie, filtr_join)

obszar_joined <-  pmap(lista_join, funkcja_join)



# Tworzę wykresy ----------------------------------------------------------

#Będę musiał jeszcze dodać 1) argument z kolorami, 2) argument z liczbą rows

map2(obszar_joined, lista_kategorie, funkcja_ggplot)
funkcja_ggplot()





# -------------------------------------------------------------------------














# 1. Populacja ------------------------------------------------------------

filtr_populacja2
Obszar_populacja1 <- funkcja_czestosci(Baza_analizy1, Obszar_terapeutyczny, Kat1_Populacja, filtr_populacja1, mode = 1)

Obszar_populacja2 <- funkcja_chiKwadrat(Baza_analizy1, Obszar_terapeutyczny, Kat1_Populacja, filtr_populacja2, mode = 1) %>% 
  add_case() %>% 
  mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Ginekologia"), n = replace_na(n, 13))


funkcja_join(Obszar_populacja1, Obszar_populacja2, Kat1_Populacja, "On Label") %>% 
  funkcja_ggplot(kategoria = Kat1_Populacja) +
  labs(title = "Procent pozytywnych rekomendacji w zależności od proponowanego wskazania\n refundacyjnego (populacja) w podziale na obszar terapeutyczny") 






# 1. Kategoria Populacja
Wykres_Obszar_terapeutyczny_Populacja  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Populacja, Rekomendacja) %>% 
  filter(!Obszar_terapeutyczny %in% c("Chirurgia" , "Diagnostyka" , "Leczenie bólu" , "Wirusologia" )) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Populacja, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Populacja) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 

Wykres_Obszar_terapeutyczny_Populacja %>% 
  print(n = 33)

Wykres_Obszar_terapeutyczny_Populacja %>% print(n = 303)

Chi_Obszar_terapeutyczny_Populacja <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Populacja, Rekomendacja) %>% 
  filter(!Obszar_terapeutyczny %in% c("Chirurgia" , "Diagnostyka" , "Leczenie bólu" , "Wirusologia" , "Ginekologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Populacja, Rekomendacja)) %>% 
  add_case() %>% 
  mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Ginekologia"), n = replace_na(n, 13))

Chi_Obszar_terapeutyczny_Populacja



Wykres_Obszar_terapeutyczny_Populacja2 <-  left_join(Wykres_Obszar_terapeutyczny_Populacja ,Chi_Obszar_terapeutyczny_Populacja, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Populacja == "On Label", p2, ".") %>% na_if("."),
         p = replace_na(p, 1.1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))

Wykres_Obszar_terapeutyczny_Populacja2 %>% 
  print(n = 40)

Wykres_Obszar_terapeutyczny_Populacja2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Populacja, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_calc() +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od proponowanego wskazania\n refundacyjnego (populacja) w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T))


ggsave("Analiza 6B - populacja a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 

# 2. Kategoria Komparator
## 2. Komparator
## Słabo z liczbą obserwacji i różnorodnością wyników

Wykres_Obszar_terapeutyczny_Komparator  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Komparator, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Choroby rzadkie" , "Neurologia" , "Immunologia" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Komparator, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Komparator) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 

Wykres_Obszar_terapeutyczny_Komparator

Chi_Obszar_terapeutyczny_Komparator <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Komparator, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Komparator, Rekomendacja)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Immunologia"), n = replace_na(n, 15)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Neurologia"), n = replace_na(n, 13))



Wykres_Obszar_terapeutyczny_Komparator2 <-  left_join(Wykres_Obszar_terapeutyczny_Komparator ,Chi_Obszar_terapeutyczny_Komparator, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Komparator == "Dobrze wybrany", p2, ".") %>% na_if("."),
         p = replace_na(p, 1.1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_Komparator2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Komparator, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_calc() +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od komparatora w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T))

ggsave("Analiza 6B - komparator a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 



## 3. Bezpieczeńśtwo
## Szkocja wykluczona, bo wszędzie taka sama wartość + n < 10

Wykres_Obszar_terapeutyczny_Bezpieczeństwo  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Bezpieczeństwo, Rekomendacja) %>% 
  filter(!Obszar_terapeutyczny %in% c("Chirurgia" , "Diagnostyka" , "Ginekologia" , "Leczenie bólu" , 
                                     "Choroby układu mięśniowo-szkieletowego" , "Dermatologia" , "Wirusologia" , "Gastroenterologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Bezpieczeństwo, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Bezpieczeństwo) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 

Wykres_Obszar_terapeutyczny_Bezpieczeństwo

Chi_Obszar_terapeutyczny_Bezpieczeństwo <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Bezpieczeństwo, Rekomendacja) %>% 
  filter(!Obszar_terapeutyczny %in% c("Chirurgia" , "Diagnostyka" , "Ginekologia" , "Leczenie bólu" , 
                                      "Choroby układu mięśniowo-szkieletowego" , "Dermatologia" , "Wirusologia" , "Gastroenterologia")) %>% 
    drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Bezpieczeństwo, Rekomendacja))



Wykres_Obszar_terapeutyczny_Bezpieczeństwo2 <-  left_join(Wykres_Obszar_terapeutyczny_Bezpieczeństwo ,Chi_Obszar_terapeutyczny_Bezpieczeństwo, by = "Obszar_terapeutyczny") %>% 
  mutate(significant = if_else(p < 0.05, T, F),
         p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Bezpieczeństwo == "Nie można stwierdzić  ", p2, ".") %>% na_if("."),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_Bezpieczeństwo2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Bezpieczeństwo, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_brewer(palette = "RdBu") +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od profilu bezpieczeńśtwa w podziale\n na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 13)) +
  guides(fill = guide_legend(reverse = T, nrow = 2))


ggsave("Analiza 6B - bezpieczeństwo a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 


##4. RSS



Wykres_Obszar_terapeutyczny_RSS  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_RSS, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Kardiologia" , "Neurologia" , "Choroby zakaźne i infekcyjne" , "Choroby rzadkie" , "Immunologia" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_RSS, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_RSS) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 


Chi_Obszar_terapeutyczny_RSS <- Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_RSS, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Kardiologia" , "Neurologia" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_RSS, Rekomendacja)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Choroby rzadkie"), n = replace_na(n, 27)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Immunologia"), n = replace_na(n, 30))



Wykres_Obszar_terapeutyczny_RSS2 <-  left_join(Wykres_Obszar_terapeutyczny_RSS ,Chi_Obszar_terapeutyczny_RSS, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_RSS == "Jest", p2, ".") %>% na_if("."),
         p = replace_na(p, 1.1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_RSS2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_RSS, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_manual(values = mycolors3) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od RSS w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T))


ggsave("Analiza 6B - RSS a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 


Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_RSS, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c( "Immunologia")) %>% 
  select(Kat1_RSS, Rekomendacja) %>% summarise(chisq_test(Kat1_RSS, Rekomendacja))



## 5. Wpływ Budżet
## Wielka Brytania wykluczona z chi-kwadrat, bo wszędzie taka sama wartość + n < 10

Wykres_Obszar_terapeutyczny_Wpływ_budżet  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Wpływ_budżet, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Okulistyka" , "Psychiatria" , "Kardiologia" , "Choroby krwi i narządów krwiotwórczych" , 
  "Diabetologia" , "Immunologia" , "Pulmonologia" , "Neurologia" , "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Wpływ_budżet, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Wpływ_budżet) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 


Chi_Obszar_terapeutyczny_Wpływ_budżet <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Wpływ_budżet, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Okulistyka" , "Psychiatria" , "Kardiologia" , "Choroby krwi i narządów krwiotwórczych" , 
                                     "Diabetologia" , "Immunologia" , "Pulmonologia" , "Neurologia" , "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Wpływ_budżet, Rekomendacja)) 





Wykres_Obszar_terapeutyczny_Wpływ_budżet2 <-  left_join(Wykres_Obszar_terapeutyczny_Wpływ_budżet ,Chi_Obszar_terapeutyczny_Wpływ_budżet, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Wpływ_budżet == "Oszczędności", p2, ".") %>% na_if("."),
         p = replace_na(p, 1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_Wpływ_budżet2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Wpływ_budżet, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_manual(values = mycolors5) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od wpływu na budżet w podziale\n na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T, nrow = 2))


ggsave("Analiza 6B - wplyw budżet a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 



##6. Korzyść kliniczna



Wykres_Obszar_terapeutyczny_Korzyść_kliniczna  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Dermatologia" , "Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                                     "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" ,
                                     "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 


Chi_Obszar_terapeutyczny_Korzyść_kliniczna <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                                     "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" ,
                                     "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Korzyść_kliniczna, Rekomendacja)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Dermatologia"), n = replace_na(n, 12))



Wykres_Obszar_terapeutyczny_Korzyść_kliniczna2 <-  left_join(Wykres_Obszar_terapeutyczny_Korzyść_kliniczna ,Chi_Obszar_terapeutyczny_Korzyść_kliniczna, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Korzyść_kliniczna == "Nie wykazano przewagi", p2, ".") %>% na_if("."),
         p = replace_na(p, 1.1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_Korzyść_kliniczna2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Korzyść_kliniczna, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_manual(values = mycolors3) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od korzyści klinicznej w podziale\n na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T))


ggsave("Analiza 6B - korzyść kliniczna a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 


## 7. Efektywność_kosztowa


Wykres_Obszar_terapeutyczny_Efektywność_kosztowa  <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Efektywność_kosztowa, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Nefrologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" ,
                                     "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" , "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Efektywność_kosztowa, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Kat1_Efektywność_kosztowa) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") 


Chi_Obszar_terapeutyczny_Efektywność_kosztowa <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Efektywność_kosztowa, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Nefrologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" ,
                                     "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" , "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Efektywność_kosztowa, Rekomendacja))





Wykres_Obszar_terapeutyczny_Efektywność_kosztowa2 <-  left_join(Wykres_Obszar_terapeutyczny_Efektywność_kosztowa ,Chi_Obszar_terapeutyczny_Efektywność_kosztowa, by = "Obszar_terapeutyczny") %>% 
  mutate(significant = if_else(p < 0.05, T, F),
         p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p3 = if_else(Kat1_Efektywność_kosztowa == "Wykazano przewagę", p2, ".") %>% na_if("."),
         p4 = if_else(Obszar_terapeutyczny == "Nowa Zelandia" & Kat1_Efektywność_kosztowa == "Nie dowiedziono przewagi", p2, p3),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))




Wykres_Obszar_terapeutyczny_Efektywność_kosztowa2 %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Efektywność_kosztowa, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_manual(values = mycolors5) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p4), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od efektywności kosztowej w podziale\n na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T, nrow = 2))

ggsave("Analiza 6B - efektywność kosztowa a rekomendacje - w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 





##6. Korzyść kliniczna - wersja alternatywna



Wykres_Obszar_terapeutyczny_Korzyść_kliniczna_ALT   <- Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Dermatologia" , "Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                                     "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" ,
                                     "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>%
  count(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  group_by(Obszar_terapeutyczny, Rekomendacja) %>% 
  mutate(percent = n / sum(n) * 100) %>%
  filter(Rekomendacja == "Pozytywna") 



Chi_Obszar_terapeutyczny_Korzyść_kliniczna <-  Baza_analizy1 %>% 
  select(Obszar_terapeutyczny, Kat1_Korzyść_kliniczna, Rekomendacja) %>% 
  filter(Obszar_terapeutyczny %in% c("Nefrologia" , "Urologia" , "Gastroenterologia" , "Okulistyka" , "Psychiatria" , 
                                     "Choroby krwi i narządów krwiotwórczych" , "Kardiologia" , "Pulmonologia" , "Diabetologia" , "Neurologia" , "Immunologia" ,
                                     "Choroby rzadkie" , "Choroby zakaźne i infekcyjne" , "Onkologia")) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  summarise(chisq_test(Kat1_Korzyść_kliniczna, Rekomendacja)) %>% 
  add_case() %>% mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, "Dermatologia"), n = replace_na(n, 12))



Wykres_Obszar_terapeutyczny_Korzyść_kliniczna2_ALT <-  left_join(Wykres_Obszar_terapeutyczny_Korzyść_kliniczna_ALT ,Chi_Obszar_terapeutyczny_Korzyść_kliniczna, by = "Obszar_terapeutyczny") %>% 
  mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
         p2 = if_else(Kat1_Korzyść_kliniczna == "Wykazano przewagę", p2, ".") %>% na_if("."),
         p = replace_na(p, 1.1),
         significant = if_else(p < 0.05, T, F),
         Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))


Wykres_Obszar_terapeutyczny_Korzyść_kliniczna2_ALT %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = Kat1_Korzyść_kliniczna, alpha = significant)) +
  geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
  scale_alpha_discrete(range = c(0.25, 0.75)) +
  guides(alpha = F) +
  coord_flip() + 
  scale_fill_manual(values = mycolors3) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  labs(y = "", fill = "", x = "") +
  geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
  theme_economist() +
  labs(title = "Procent pozytywnych rekomendacji w zależności od korzyści klinicznej w podziale\n na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = T))


ggsave("Analiza 6B - korzyść kliniczna a rekomendacje - w podziale na obszar terapeutyczny - WERSJA ALT.png", device = "png", path = "./Wykresy/Wykresy 6b",
       width = 12, height = 8, dpi = 300) 

