#Wstępne analizy


library(Cabix)
library(ggsci)
library(viridis)
library(ggthemes)
library(RColorBrewer)
biblioteki()

source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")

# Ustalanie parametrów dla wykresów ---------------------------------------


scales::show_col(few_pal()(7))

mycolors <- brewer.pal(9, "Pastel1")


fill_color <- "#FAA43A"
border_color <-  "#5DA5DA"
fill_2colors <- c( "#5DA5DA","#FAA43A")


alpha1 = 2/3
outlier.alpha1 = 0.6
outlier.size1 = 3
size1 = 1



# Wczytuję bazy danych do analiz ------------------------------------------




Baza_razem4 <-  read_csv("Baza razem - 21 lutego 2022.csv")
Baza_analizy1 <-  read_csv("Baza danych - 21 lutego 2022.csv")






Baza_razem4 <-   Baza_razem4 %>% 
    mutate(
      Is_Orphan = plyr::mapvalues(Is_Orphan, 1, 20),
       Biosimilar = plyr::mapvalues(Biosimilar, 1, 5),
       Kategoria_lek = Is_Orphan + Is_Generic + Biosimilar,
       Kategoria_lek = factor(Kategoria_lek, labels = c("Other / Original", "Generic", "Biosimilar", "Orphan", "Unknown")),
      Kraj = plyr::mapvalues(Kraj, kraje_pl, kraje_ang),
      Obszar_terapeutyczny = plyr::mapvalues(Obszar_terapeutyczny, Obszary_old, Obszary_new)
         )


# Eksport bazy 
# Baza_razem4 %>% 
#  select(Obserwacja_nr, everything()) %>% 
#  write_csv("Baza razem english - 22.09.2023.csv")
# Baza_razem4 %>% 
#  select(Obserwacja_nr, everything()) %>% 
#  xlsx::write.xlsx("Baza razem english - 22.09.2023.xlsx")


# Dodaję filtr dla wybranych dat - wykresy z datami -----------------------

library(readxl)
# Czasy do usunięcia
filtr_data <- read_excel("Daty filtr - do usunięcia w ramach wykresów czasu.xlsx") 

# Czasy ze zaktualizowanymi datami
obserwacje_update <- read_excel("Aktualizacja wybranych obserwacji dla dat.xlsx") 

obserwacje_update <- obserwacje_update %>% 
  mutate(
    dni_do_rekomendacji = difftime(Data_rekomendacji,Data_rejestracji, units = "day"),
    dni_do_rekomendacji = as.double(dni_do_rekomendacji)
  ) %>% 
  select(-1)

# Przygotowuję bazę z kolumnami do filtrowania
filtr_data2 <- filtr_data %>% 
  select(ID, Agencja_kraj, Data_rejestracji, Data_rekomendacji, Link_do_rekomendacji, Rekomendacja, dni_do_rekomendacji) %>% 
  mutate(
    Kraj = str_extract(Agencja_kraj, "[_].*$|[ ].*$") %>% 
      str_replace("[_]|[ ]", "") %>% 
      str_replace("BA[_]", "") %>% 
      plyr::mapvalues(kraje_pl, kraje_ang)
  ) %>% 
  mutate(
    across(c(Data_rekomendacji, Data_rejestracji), ~as.Date(.))
  ) %>% 
  mutate(
    filtr = paste(
      ID,  Kraj, dni_do_rekomendacji, Rekomendacja, Link_do_rekomendacji
    ) # tworzę zmienną filtrującą
  ) 

# Stary filtr
  # ID, Data_rekomendacji, Data_rejestracji, Kraj, Link_do_rekomendacji, Rekomendacja


# Sprawdzam, które obserwacje się duplikują
filtr_data2[duplicated(filtr_data2), ]
id_duplicated <- filtr_data2[duplicated(filtr_data2), ] %>% 
  pull(ID) # Brak duplikatów po zaktualizowaniu filtra o link do rekomendacji


# Wyodrębniam zmienną filtrującą
filtr_czasy <- filtr_data2 %>% pull(filtr)

# Sprawdzam, czy są duplikaty w ramach filtru
unique(filtr_czasy)
filtr_czasy[duplicated(filtr_czasy)]

# Tworzę bazę dates z analogiczną zmienną filtrującą
Baza_razem4_dates <- Baza_razem4 %>% 
  mutate(
    filtr = paste(
      ID,  Kraj, dni_do_rekomendacji, Rekomendacja, Link_do_rekomendacji)
  ) 

# Sprawdzam, czy w pełnej bazie są powtórzenia również z uwzględnieniem linku
Baza_razem4_dates %>% 
  count(ID, filtr) %>% 
  filter(n > 1) # Po uwzględnieniu jeszcze rekomendacji, jest jedna obserwacja, trudno



Baza_razem4_dates <- Baza_razem4_dates %>% 
  filter(
    !filtr %in% filtr_czasy
  ) %>% 
  filter(!is.na(Rekomendacja))  %>%  # Nakładam filtr na tylko obserwacje z wydanymi rekom.
  filter(dni_do_rekomendacji >= 0) %>%   # Nakładam filtr na tylko dodatnie czasowo rekom.
  filter(!Obserwacja_nr %in% obserwacje_update$Obserwacja_nr)  %>% 
  left_join(obserwacje_update) 

# Usuwam kraje europejskie z czasem powyżej 1500 
filtr_powyzej1500 <- Baza_razem4_dates %>% 
  filter(dni_do_rekomendacji >= 1500 & !Kraj %in% c("Poland", "Canada", "Australia", "New Zealand")) %>% 
  pull(Obserwacja_nr)

Baza_razem4_dates <- Baza_razem4_dates %>% 
  filter(!Obserwacja_nr %in% filtr_powyzej1500) %>% 
  group_by(Kraj) %>% 
  mutate(
    Kraj_n = length(Kraj),
    Kraj_n = str_c(Kraj, " (n = ", Kraj_n, ")")
  ) %>% 
  group_by(Obszar_terapeutyczny) %>% 
  mutate(
    Obszar_terapeutyczny_n = length(Obszar_terapeutyczny),
    Obszar_terapeutyczny_n = str_c(Obszar_terapeutyczny, " (n = ", Obszar_terapeutyczny_n, ")")
  ) %>%
  group_by(Kategoria_lek) %>% 
  mutate(
    Kategoria_lek_n = length(Kategoria_lek),
    Kategoria_lek_n = str_c(Kategoria_lek, " (n = ", Kategoria_lek_n, ")"),
    Kraj = plyr::mapvalues(Kraj, "Netherlands", "The Netherlands")
  ) %>% 
  ungroup()

Baza_razem4_dates %>% 
  count(Kategoria_lek, Kategoria_lek_n)

Baza_razem4_dates %>% 
  count(Obszar_terapeutyczny, Obszar_terapeutyczny_n)



# Baza_razem4_dates %>% 
#   select(Obserwacja_nr, everything()) %>% 
#   xlsx::write.xlsx("Baza z filtrem na dates 22.09.2023.xlsx")


# 1)	Liczba ocenionych leków w latach 2014-2020 ---------------------------



wykres1 <- Baza_razem4 %>% 
    select(Nazwa_miedzynarodowa, Year) %>% 
  group_by(Year) %>% 
  summarise(liczba = unique(Nazwa_miedzynarodowa)) %>% 
  ungroup() %>% 
  count(Year) %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), n)) +
  geom_bar(stat = "identity",  fill = fill_color, alpha = alpha1, width = 0.5) +
  labs(title = "Liczba ocenionych leków w latach 2014 - 2019+", fill = "", x = "", y = "") +
    geom_label(aes(label = n)) +
  theme(plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) +
  coord_flip() 

Baza_razem4$Nazwa_miedzynarodowa %>% unique() %>% length()

wykres1

# 2)	Liczba ocenionych rekomendacji w latach 2014-2020 -------------------------



wykres2 <-  Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  count(Year) %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), n)) +
  geom_bar(stat = "identity",  color = "white", fill = fill_color, alpha = alpha1, width = 0.5) +
  labs(title = "Liczba ocenionych rekomendacji w latach 2014 - 2019+", fill = "", x = "", y = "") +
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) +
  coord_flip()

wykres2

# 2.5)	Liczba ocenionych wskazań w latach 2014-2020 -------------------------



wykres2_5 <-  Baza_razem4 %>% 
  select(ID, Year) %>% 
  unique() %>% 
  count(Year) %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), n)) +
  geom_bar(stat = "identity",  color = "white", fill = fill_color, alpha = alpha1, width = 0.5) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019+", fill = "", x = "", y = "") +
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) +
  coord_flip()

wykres2_5




# 3)	Liczba leków w podziale ze względu na przynajmniej jedną uzys --------




wykres3 <- Baza_razem4 %>%
  filter(!is.na(Rekomendacja))  %>% 
  mutate(Rekomendacja2 = fct_recode(Rekomendacja, 
                                    'Oceniona' = 'Negatywna',
                                    'Oceniona' = 'Pozytywna' ),
         Rekomendacja2 = fct_explicit_na(Rekomendacja2, "Nieoceniona")) %>% 
  count(Rekomendacja2, Nazwa_substancji_czynnej) %>% 
  group_by(Nazwa_substancji_czynnej) %>% 
  mutate(percent =  n / sum(n)) %>% 
  arrange(desc(percent)) %>% 
  filter(Rekomendacja2 == "Nieoceniona") %>% 
  mutate(Stosunek = if_else(percent == 1, "Brak uzyskanych rekomendacji", "Przynajmniej jedna rekomendacja")) %>% 
  ungroup() %>% 
  count(Stosunek) %>% 
  ggplot(aes(Stosunek, n)) +
  geom_bar(stat = "identity",  color = "white", fill = fill_color, alpha = alpha1, width = 0.5) +
  labs(title = "Liczba leków w podziale ze względu na przynajmniej jedną uzyskaną rekomendację", fill = "", x = "", y = "") +
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) +
  coord_flip()



# 4)	Liczba ocenionych wskazań w latach 2014-2020 w podziale na kr --------


# WERSJA  SŁUPKOWA
 wykres4_a <- Baza_razem4 %>% 
   count(Year, Kraj) %>% 
   filter(Kraj != "Sweden") %>% 
   ggplot(aes(factor(Year), n)) +
   geom_bar(stat = "identity", color = "white", alpha = alpha1, fill = fill_color) +
   labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj", fill = "") +
   theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
   facet_wrap(~Kraj,  scales = "free_x") 

# Wersja liniowa
wykres4_a_line <- Baza_razem4 %>% 
   count(Year, Kraj) %>% 
   filter(Kraj != "Sweden") %>% 
   ggplot(aes(x = factor(Year), y = n,  color = Kraj, group = Kraj)) +
   geom_line() +
   labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj", fill = "") +
   theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) 

wykres4_a_line %>% 
  ggsave(device = "png", filename = "Frontiers/Graphs/Graph 4 line.png")


wykres4_b <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  count(Year, Kraj) %>% 
  complete(Year, Kraj, fill = list(n = 0)) %>% 
  ggplot(aes(factor(Year), n )) +
  geom_bar(stat = "identity", color = "white", alpha = alpha1, fill = fill_color) +
  geom_text(aes(label = n)) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj \ntylko wydane rekomendacje", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  facet_wrap(~Kraj, scales = "free_x")


wykres4_b + ggExtra::removeGrid()  + theme(panel.background = element_rect(fill = "white"))
wykres4_b + theme_few()  + theme(strip.background =element_rect(fill="gray"))


# 5)	Liczba ocenionych wskazań w latach 2014-2020 w podziale na ob --------


wykres5 <- Baza_razem4 %>%
  filter(!is.na(Rekomendacja))  %>% 
  mutate(Obszar_terapeutyczny = Obszar_terapeutyczny %>% str_to_sentence()) %>% 
  count(Year, Obszar_terapeutyczny) %>% 
  complete(Year, Obszar_terapeutyczny, fill = list(n = 0)) %>% 
  drop_na() %>% 
  ggplot(aes(factor(Year), n)) +
  geom_bar(stat = "identity", color = "white", alpha = alpha1, fill = fill_color) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na obszar terapeutyczny", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 11)) +
  facet_wrap(~Obszar_terapeutyczny, scales = "free_x")


wykres5

# 6)	Liczba ocenionych wskazań w latach 2014-2020 w podziale na ka --------

wykres6 <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  count(Year, Kategoria_lek) %>% 
  complete(Year, Kategoria_lek, fill = list(n = 0)) %>% 
  filter(Kategoria_lek != "Unknown") %>% 
  ggplot(aes(factor(Year), n )) +
  geom_bar(stat = "identity", color = "white", fill = fill_color, alpha = alpha1) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kategorię leku", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~Kategoria_lek, scales = "free_x") 


wykres6

# 7)	Średnia liczba dni od daty rejestracji do daty rekomendacji - kraj

wykres7 <- Baza_razem4_dates %>% 
  group_by(Kraj) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>%
  drop_na() %>% 
  ggplot(aes(Kraj %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "white", fill = fill_color, alpha = alpha1) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  coord_flip() +
  geom_label(aes(label = Mean %>% round())) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
  ) 


wykres7_skrzynka <- Baza_razem4_dates %>% 
  ggplot(aes(Kraj_n %>% fct_reorder(dni_do_rekomendacji), dni_do_rekomendacji )) +
  geom_boxplot(color = border_color, fill = fill_color, alpha = alpha1, outlier.alpha = outlier.alpha1, 
               outlier.size = outlier.size1, size = size1) + 
  coord_flip() +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
  ) 





# 8)	Średnia liczba dni od daty rejestracji do daty rekomendacji - obszar 

wykres8 <-  Baza_razem4_dates %>% 
  mutate(Obszar_terapeutyczny = str_to_sentence(Obszar_terapeutyczny)) %>% 
  filter(Obszar_terapeutyczny != "Leczenie bólu") %>% 
  group_by(Obszar_terapeutyczny) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>%
  drop_na() %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "white", fill = fill_color, alpha = alpha1) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  coord_flip() +
  geom_label(aes(label = Mean %>% round())) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 14, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) 

wykres8_skrzynka <-  Baza_razem4_dates %>% 
  ggplot(aes(Obszar_terapeutyczny_n %>% fct_reorder(dni_do_rekomendacji) , dni_do_rekomendacji )) +
  geom_boxplot(color = border_color, fill = fill_color, alpha = alpha1, outlier.alpha = outlier.alpha1, 
               outlier.size = outlier.size1, size = size1) + 
  coord_flip() +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
  ) 


wykres8_skrzynka

# 9)	Średnia liczba dni od daty rejestracji do daty rekomendacji - lek



wykres9 <- Baza_razem4_dates  %>% 
  filter(Kategoria_lek != "Unknown") %>% 
  group_by(Kategoria_lek) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>% 
  ggplot(aes(Kategoria_lek %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "white", fill = fill_color, alpha = alpha1, width = 0.5) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  coord_flip() +
  geom_label(aes(label = Mean %>% round()), nudge_y = -10) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na kategorię leku") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) 


wykres9_skrzynka <- Baza_razem4_dates  %>% 
  ggplot(aes(Kategoria_lek_n %>% fct_reorder(dni_do_rekomendacji) , dni_do_rekomendacji )) +
  geom_boxplot(color = border_color, fill = fill_color, alpha = alpha1, outlier.alpha = outlier.alpha1, 
               outlier.size = outlier.size1, size = size1) + 
  coord_flip() +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na kategorię leku") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) 



wykres9_skrzynka

# Wykresy pozostałe -------------------------------------------------------




Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Kraj, Rekomendacja) %>% filter(!is.na(Rekomendacja)) %>% 
  group_by(Kraj) %>% mutate(percent = n / sum(n) * 100) %>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Kraj %>% fct_reorder(percent), percent)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black", alpha = 0.4) +
  coord_flip() + labs(y = "Procent pozytywnych rekomendacji", x = "") + theme_calc() +
  scale_y_continuous(breaks =  seq(0,100,10), labels = str_c(seq(0,100,10), "%"))




Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Year, Kraj, Rekomendacja) %>%  filter(!is.na(Rekomendacja)) %>% 
  group_by(Year, Kraj) %>% mutate(percent = n / sum(n) * 100)%>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Kraj, percent, fill = Year %>% factor() %>% fct_rev() )) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge()) + scale_fill_viridis(discrete = T) + theme_economist() +
  labs(fill = "", x = "", y = "Procent pozytywnych rekomendacji") + coord_flip()














# Etykiety dla wykresów ---------------------------------------------------

etykiety <- list(
  wykres1 = list(x = "", y = "", title = "The number of evaluated drugs"),
  wykres2 = list(x = "", y = "", title = "The number of evaluated recommendations"),
  wykres2_5 = list(x = "", y = "", title = "The number of evaluated indications"),
  wykres3 = list(x = "", y = "", title = "The number of evaluated drugs with at least one recommendation"),
  wykres4_a = list(x = "", y = "", title = "The number of recommendations by country"),
  wykres4_b = list(x = "", y = "", title = "The number of evaluated recommendations by country"),
  wykres5 = list(x = "", y = "", title = "The number of recommendations by therapeutic area"),
  wykres6 = list(x = "", y = "", title = "The number of recommendations by drug category"),
  wykres7 = list(x = "", y = "", title = "The mean number of days from registration to recommendation by country"),
  wykres8 = list(x = "", y = "", title = "The mean number of days from registration to recommendation by therapuetic area"),
  wykres9 = list(x = "", y = "", title = "The mean number of days from registration to recommendation by drug category")
)




# Motyw dla wykresów ------------------------------------------------------


  
add_theme <- function(p, labs) {
  
  
  p +  
    ggExtra::removeGrid()  + 
    theme(panel.background = element_rect(fill = "white")) + 
    labs(x = labs[[1]], y = labs[[2]], title = labs[[3]])
}





# Kompletowanie -----------------------------------------------------------


wykresy <- list(wykres1, wykres2, wykres2_5, wykres3, wykres4_a ,wykres4_b, wykres5, wykres6, wykres7, wykres8, wykres9)


df_wykresy <- tibble(
  output = str_c("./Frontiers/Graphs/Graph ", c(1, 2, "2_5", 3, "4a", "4b", 5:9), ".png"),
  wykresy = wykresy,
  etykiety = etykiety) 

df_wykresy$etykiety
df_wykresy <- df_wykresy %>% 
  mutate(
    wykresy_output = map2(wykresy, etykiety, add_theme)
  )

df_wykresy



# Zapisywanie wykresów ----------------------------------------------------

df_wykresy %>% 
  select(filename = output, plot = wykresy_output) %>% 
  pwalk(ggsave, device = "png", dpi = 300, width = 10, height = 10)




# Wykresy skrzynkowe ------------------------------------------------------


wykresy_skrzynkowe <- list(
  wykres7_skrzynka,
  wykres8_skrzynka,
  wykres9_skrzynka
)

etykiety_skrzynkowe <- list(
  wykres7_skrzynka = list(x = "", y = "", title = "The number of days from registration to recommendation by country"),
  wykres8_skrzynka = list(x = "", y = "", title = "The number of days from registration to recommendation by therapuetic area"),
  wykres9_skrzynka = list(x = "", y = "", title = "The number of days from registration to recommendation by drug category")
  )


add_theme2 <- function(x, labs) {
  x +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = "white"), 
          plot.background = element_rect(fill = "white", color = "white")) + 
    labs(x = labs[[1]], y = labs[[2]], title = labs[[3]])
  
}


df_wykresy_skrzynkowe <- tibble(
  output = str_c("./Frontiers/Graphs/Graph boxplot", c(7:9) , ".png"),
  wykresy = wykresy_skrzynkowe,
  etykiety = etykiety_skrzynkowe) %>% 
  mutate(
    wykresy_output = map2(wykresy, etykiety, add_theme2)
  )
  


df_wykresy_skrzynkowe$wykresy_output

df_wykresy_skrzynkowe %>% 
  select(filename = output, plot = wykresy_output) %>% 
  pwalk(ggsave, device = "png", dpi = 300, width = 10, height = 10)





# Wykres liczba oceniających agencji --------------------------------------


Baza_razem4 %>% 
  filter(!is.na(Rekomendacja)) %>% 
  filter(ID == 881) %>% 
  count(Wskazanie_rejestracyjne)


Baza_razem4 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj) %>% 
  unique() %>% 
  count(ID, Agencja_kraj) %>% 
  group_by(ID) %>% 
  summarise(liczba_agencji = sum(n)) %>% 
  count(liczba_agencji) %>% 
  ggplot(aes(liczba_agencji, n)) +
  geom_bar(stat = "identity", fill = fill_color,  alpha = alpha1) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = seq(0,70,5)) +
  coord_cartesian(ylim = c(0,70)) +
  ggExtra::removeGrid()  + 
  theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Number of evaluating agencies", y = "Number of recommendations", title = "Number of recommendations by number of evaluating agencies") 
 
 ggsave(filename = "./Frontiers/Graphs/(K4) Number of recommendations by number of evaluating agencies.png",
        device = "png", dpi = 300, width = 10, height = 6)

 
 
 

# Tabele dla średniego czasu  ---------------------------------------------


# Tworzę funkcję do zmiany formatowania big numbers
 
no_commas <- function(df) {
  df %>% 
    mutate(
      across(where(is.numeric), ~formatC(., big.mark = ""))
    )
}
 
 
 tabele_czas <- list()
 wektor_tabele <- c("Kraj", "Obszar_terapeutyczny", "Kategoria_lek")
 
 
 
 for (i in 1:length(wektor_tabele)) {
   tabele_czas[[i]] <-  Baza_razem4_dates %>% 
     group_by(!!sym(wektor_tabele[i])) %>% 
     get_summary_stats(dni_do_rekomendacji) %>% 
     mutate(mean = round(mean, 1)) %>% 
     select(-c(variable, q1, q3, mad, sd, se, ci)) %>% 
     arrange(desc(median))
 }
 
 
 
for (i in 1:length(wektor_tabele)) {
  tabele_czas[[i]] %>% 
    no_commas() %>% 
    flextable() %>% 
    save_as_docx(path = str_c("Frontiers/Tabele czas/", wektor_tabele[i], ".docx"))
}
 
 
 # Dodaję tabelę
 Baza_razem4_dates %>% 
   group_by(Kraj) %>% 
   get_summary_stats(dni_do_rekomendacji) %>% 
   mutate(mean = round(mean, 1)) %>% 
   select(-c(variable, q1, q3, iqr, mad, sd, se, ci)) %>%
   
   flextable()
 
 Baza_razem4_dates$Obszar_terapeutyczny %>% 
   unique()
 
 
 
 
# Dodaję tabelę kraj i obszar
 Baza_razem4_dates %>% 
   filter(
     Obszar_terapeutyczny %in% c( "Oncology", "Rare diseases", "Immunology", "Neurology", "Diabetology",
                                  "Cardiology", "Pulmonology", "Infectious diseases")
     )  %>% 
   group_by(Kraj, Obszar_terapeutyczny) %>% 
   get_summary_stats(dni_do_rekomendacji) %>% 
   mutate(mean = round(mean, 1)) %>% 
   select(-c(variable, q1, q3, mad, sd, se, ci)) %>% 
   arrange(Kraj, desc(median)) %>% 
   select(Kraj, Obszar_terapeutyczny, everything()) %>% 
   no_commas() %>% 
   flextable() %>% 
   save_as_docx(path = str_c("Frontiers/Tabele czas/Kraj i obszar 1.docx"))
 
Baza_razem4_dates %>% 
   filter(
     Obszar_terapeutyczny %in% c( "Oncology", "Rare diseases", "Immunology", "Neurology", "Diabetology",
                                  "Cardiology", "Pulmonology", "Infectious diseases")
     )  %>% 
   group_by(Kraj, Obszar_terapeutyczny) %>% 
   get_summary_stats(dni_do_rekomendacji) %>% 
   mutate(mean = round(mean, 1)) %>% 
   select(-c(variable, q1, q3, mad, sd, se, ci)) %>% 
   arrange(Obszar_terapeutyczny, desc(median)) %>% 
   no_commas() %>% 
   flextable() %>% 
   save_as_docx(path = str_c("Frontiers/Tabele czas/Kraj i obszar 2.docx"))
 
 
 
 
  
 
