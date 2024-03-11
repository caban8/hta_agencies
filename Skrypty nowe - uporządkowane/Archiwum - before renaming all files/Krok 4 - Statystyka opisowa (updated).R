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


wykres4_a <- Baza_razem4 %>% count(Year, Kraj) %>% 
  ggplot(aes(factor(Year), n)) +
  geom_bar(stat = "identity", color = "white", alpha = alpha1, fill = fill_color) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  facet_wrap(~Kraj,  scales = "free_x") 

wykres4_a


wykres4_b <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  count(Year, Kraj) %>% 
  complete(Year, Kraj, fill = list(n = 0)) %>% 
  ggplot(aes(factor(Year), n )) +
  geom_bar(stat = "identity", color = "white", alpha = alpha1, fill = fill_color) +
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

wykres7 <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
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


wykres7_skrzynka <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  ggplot(aes(Kraj %>% fct_reorder(dni_do_rekomendacji), dni_do_rekomendacji )) +
  geom_boxplot(color = border_color, fill = fill_color, alpha = alpha1, outlier.alpha = outlier.alpha1, 
               outlier.size = outlier.size1, size = size1) + 
  coord_flip() +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
  ) 




# 8)	Średnia liczba dni od daty rejestracji do daty rekomendacji - obszar 

wykres8 <-  Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
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
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) 

wykres8_skrzynka <-  Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  ggplot(aes(Obszar_terapeutyczny %>% fct_reorder(dni_do_rekomendacji) , dni_do_rekomendacji )) +
  geom_boxplot(color = border_color, fill = fill_color, alpha = alpha1, outlier.alpha = outlier.alpha1, 
               outlier.size = outlier.size1, size = size1) + 
  coord_flip() +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
  ) 


wykres8_skrzynka

# 9)	Średnia liczba dni od daty rejestracji do daty rekomendacji - lek



wykres9 <- Baza_razem4  %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(Kategoria_lek != "Unknown") %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  group_by(Kategoria_lek) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>% 
  ggplot(aes(Kategoria_lek %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "white", fill = fill_color, alpha = alpha1, width = 0.5) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  coord_flip() +
  geom_label(aes(label = Mean %>% round()), nudge_y = -10) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na kategorię leku") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        ) 


wykres9_skrzynka <- Baza_razem4  %>% 
  filter(!is.na(Rekomendacja))  %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  ggplot(aes(Kategoria_lek %>% fct_reorder(dni_do_rekomendacji) , dni_do_rekomendacji )) +
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
