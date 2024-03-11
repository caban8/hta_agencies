
library(rstatix)
library(tidyverse)
library(ggsci)
library(ggthemes)

Baza_analizy1 <- read_csv("Baza danych - 16 lutego 2022.csv")

# Krok 8.1 - Analizy rekomendacje a kraj - inna perspektywa ---------------



# Pobieram funkcje z innych skryptów

source("./Funkcje/Kroki funkcje.R", encoding = "UTF-8")


# Zawężenie bazy danych ---------------------------------------------------



DF_analiza6a <-  Baza_analizy1 %>%
  pivot_longer(cols = starts_with("Kat1"), values_to = "Odpowiedz", names_to = "Kategoria") %>% 
  mutate(Kategoria = str_replace(Kategoria,"Kat1[_]", "") %>% 
           str_replace("[_]", " ")) 


nazwy_kraje <- Baza_analizy1 %>% 
  select(Kraj) %>% 
  unique() %>% 
  as_vector()


lista_bazy <- list()


for (i in 1:length(nazwy_kraje)) {
  
  lista_bazy[[i]] <- DF_analiza6a %>% 
    filter(Kraj == nazwy_kraje[[i]])
  
}


names(lista_bazy) <- nazwy_kraje

lista_bazy2 <- lista_bazy[-10] #Usuwam Szwecję, dla której nie ma żadnych Rekomendacji...
lista_bazy2





# Funkcje -----------------------------------------------------------------

  #Wektor referencyjny dla funkcji chi-kwadrat

  Kategorie <- DF_analiza6a$Kategoria %>% unique()


#Funkcja chi-kwadrat

chi_All <-  function(x, grouping = Kategoria, kategoria = "Odpowiedz") {
  
  
  filtr <- funkcja_filtr(x, {{grouping}}, kategoria)
  difference <- setdiff(Kategorie, x$Kategoria)
  
  
  x %>% 
    filter({{grouping}} %in% filtr) %>% 
  group_by(Kategoria) %>% 
    summarise(chisq_test(Odpowiedz, Rekomendacja)) %>% 
    ungroup() %>% 
    mutate( p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
            significant = if_else(p < 0.05, T,F)) %>% 
    select(Kategoria, n, p2, significant)
  
  
}

chi_All(lista_bazy2$Szkocja)



map(lista_bazy2, chi_All)



  
  
  



# Funkcja wykres ----------------------------------------------------------

  
#Wektor referencyjny 
  
  
  Kategorie_odpowiedzi <- DF_analiza6a %>% 
    count(Kategoria, Rekomendacja, Odpowiedz) %>% 
    filter(!is.na(Odpowiedz), !is.na(Rekomendacja)) %>%
    group_by(Kategoria, Odpowiedz) %>% 
    mutate(percent = n / sum(n) * 100) %>% 
    filter(Rekomendacja == "Pozytywna") %>% 
    ungroup() %>% 
    select(Kategoria, Odpowiedz)
  
  

    funkcja_czestosci2 <- function(x) {
      
      DF <- x %>% 
        count(Kategoria, Rekomendacja, Odpowiedz) %>% 
        filter(!is.na(Odpowiedz), !is.na(Rekomendacja)) %>%
        group_by(Kategoria, Odpowiedz) %>% 
        mutate(percent = n / sum(n) * 100) %>% 
        filter(Rekomendacja == "Pozytywna") %>% 
        ungroup() 
      
      differnce <- setdiff(Kategorie_odpowiedzi, 
              DF %>% select(Kategoria, Odpowiedz))
      
      
      differnce2 <- differnce %>% add_column(Rekomendacja = "Pozytywna", n = 0, percent = 0)  
      
      DF %>% 
        add_row(differnce2) %>% 
        arrange(Kategoria) %>% 
        mutate(lp = c(rep(4, 4), rep(5, 5), rep(2, 2), rep(3,3), rep(1,2), rep(6,2), rep(7,5)),
               Odpowiedz = tidytext::reorder_within(Odpowiedz, by = percent, within = Kategoria))
      
    }
  
   
    
    funkcja_join2 <- function(freq, chi) {
      
      left_join(freq, chi, by = "Kategoria") %>% 
        arrange(Kategoria, desc(percent)) %>% 
        mutate(p2 = if_else(is.na(p2), "n.s.", p2),
          p2 = c(p2[1], rep(NA, 3), p2[5], rep(NA, 4), p2[10], NA, p2[12], rep(NA, 2), p2[15], NA, p2[17], NA, p2[19], rep(NA, 4)),
          significant = if_else(is.na(significant), FALSE, TRUE)
               ) %>% 
        arrange(lp)
      
    }
    
    

# -------------------------------------------------------------------------

    
    
  freq1 <-    funkcja_czestosci2(lista_bazy2$Szkocja) 
  chi1 <- chi_All(lista_bazy2$Szkocja)
    
     funkcja_join2(freq1, chi1)
  


# Funkcja ggplot ----------------------------------------------------------


     funkcja_ggplot2 <- function(x, label.kraj) {
       
       x %>% 
         ggplot(aes(Odpowiedz, percent, fill = Kategoria %>% fct_reorder(lp) )) +
         geom_bar(stat = "identity", position = position_dodge(), width = 0.95, color = "black", alpha = 0.5) +
         coord_flip() +
         scale_fill_uchicago() + 
         labs(x = "", y = "", fill = "") + 
         scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
         tidytext::scale_x_reordered() +
         facet_grid(Kategoria %>% fct_reorder(lp) ~ ., scales = "free_y", space = "free") +
         theme_fivethirtyeight() +
         geom_text(aes(label = p2), hjust = 1, color = "black") +
         labs(title = str_c( "Procent pozytywnych rekomendacji oddzielnie dla każdego kryterium - \n", label.kraj)) +
         theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
               strip.text = element_blank(), panel.spacing.y = unit(10, "pt")) 
     }
         
    

# Obliczenia --------------------------------------------------------------


lista_freq2 <- map(lista_bazy2, funkcja_czestosci2)
lista_chi2 <- map(lista_bazy2, chi_All)
lista_joined2 <- map2(lista_freq2, lista_chi2, funkcja_join2)

lista_wykresy2 <- map2(lista_joined2, names(lista_bazy2), funkcja_ggplot2)
lista_wykresy2

pliki_nazwy2 <- str_c("Procent pozytywnych rekomendacji oddzielnie dla każdego kryterium - ", names(lista_bazy2), ".png")
pliki_nazwy2
     


walk2(pliki_nazwy2, lista_wykresy2, ggsave, device = "png", path = "./Wykresy/Wykresy 6a w podziale na kraj",
      width = 12, height = 8, dpi = 300)




# Sprawdzamy wybrane wykresy ----------------------------------------------



Baza_analizy1 %>% 
  filter(Kraj == "Wielka Brytania") %>% 
  count(Kat1_Wpływ_budżet, Rekomendacja)




# -------------------------------------------------------------------------



    
      funkcja_join2(freq1, chi1) %>% 
    ggplot(aes(Odpowiedz, percent, fill = Kategoria %>% fct_reorder(lp) )) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.95, color = "black", alpha = 0.5) +
    coord_flip() +
    scale_fill_uchicago() + 
    labs(x = "", y = "", fill = "") + 
    scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
    tidytext::scale_x_reordered() +
    facet_grid(Kategoria %>% fct_reorder(lp) ~ ., scales = "free_y", space = "free") +
    theme_fivethirtyeight() +
    geom_text(aes(label = p2), hjust = 1, color = "black") +
    labs(title = "Procent pozytywnych rekomendacji oddzielnie dla każdego kryterium") +
    theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
          strip.text = element_blank(), panel.spacing.y = unit(10, "pt")) 
  
  

# Analiza -----------------------------------------------------------------



Baza_analizy1 %>% 
  pivot_longer(cols = starts_with("Kat1"), values_to = "Odpowiedz", names_to = "Kategoria") %>% 
  mutate(Kategoria = str_replace(Kategoria,"Kat1[_]", "") %>% 
           str_replace("[_]", " ")) %>% 
  count(Kategoria, Rekomendacja, Odpowiedz) %>% 
  filter(!is.na(Odpowiedz), !is.na(Rekomendacja)) %>%
  group_by(Kategoria, Odpowiedz) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") %>% 
  ungroup() %>% 
  mutate(lp = c(rep(4, 4), rep(5, 5), rep(2, 2), rep(3,3), rep(1,2), rep(6,2), rep(7,5)),
         Odpowiedz = tidytext::reorder_within(Odpowiedz, by = percent, within = Kategoria)) %>% 
  left_join(Chi_wszystkie, by = "Kategoria") 
    
    
  arrange(Kategoria, desc(percent)) %>% 
  mutate(p2 = c(p2[1], rep(NA, 3), p2[5], rep(NA, 4), p2[10], NA, p2[12], rep(NA, 2), p2[15], NA, p2[17], NA, p2[19], rep(NA, 4))) %>% 
  arrange(lp) %>% 
  ggplot(aes(Odpowiedz, percent, fill = Kategoria %>% fct_reorder(lp)  )) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.95, color = "black", alpha = 0.5) +
  coord_flip() +
  scale_fill_uchicago() + 
  labs(x = "", y = "", fill = "") + 
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  tidytext::scale_x_reordered() +
  facet_grid(Kategoria %>% fct_reorder(lp) ~ ., scales = "free_y", space = "free") +
  theme_fivethirtyeight() +
  geom_text(aes(label = p2), hjust = 1, color = "black") +
  labs(title = "Procent pozytywnych rekomendacji oddzielnie dla każdego kryterium") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        strip.text = element_blank(), panel.spacing.y = unit(10, "pt")) 


  
  
  