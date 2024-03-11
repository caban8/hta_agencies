

# Krok 9.5 - Pozytywne i negatywne rekomendacje - minimum 6 oceniających agencji


library(tidyverse)
library(ggsci)

Baza_analizy1 <- read_csv("Baza danych - 21 lutego 2022.csv") 



Baza_analizy1_filtered <- Baza_analizy1 %>% 
  filter(liczba_agencji >= 6)
  

# Funkcje -----------------------------------------------------------------



funkcja_czestosci2 <- function(x, grouping, kategoria) {
  
  
  
  #Zliczam wszystkie częstości
  frequencies <- x %>% 
    select({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    drop_na() %>%
    count({{grouping}}, .data[[kategoria]], Rekomendacja)
  
  
  
  #Ustanawiam filtr, żeby wyselekcjonować tylko te warunki, dla których było przynajmniej 10 obserwacji
  filtr <- funkcja_filtr2(x, {{grouping}}, kategoria)
  
  #Zliczam częstości ogólne, żeby potem wykorzystać je w ramach filter join
  frequencies_total <- frequencies %>% 
    dplyr::filter({{grouping}} %in% filtr) %>% 
    group_by({{grouping}}) %>% 
    summarise(n = sum(n)) %>% 
    rename(Grupa = {{grouping}})
  
  
  frequencies2 <- frequencies %>%  
    dplyr::filter({{grouping}} %in% filtr) %>% 
    group_by({{grouping}}, .data[[kategoria]]) %>% 
    mutate(percent = n / sum(n) * 100) 
  
  results <- list(filtr, frequencies2, frequencies_total)
  names(results) <- c("Filtr_freq", "Freq", "N")
  results
} 





funkcja_ggplot2 <- function(x, kategoria, grouping) {
  
  x$Freq %>% 
    mutate(n = as.double(n),
           n = if_else(Rekomendacja == "Negatywna", (n * -1), n)) %>% 
    ggplot(aes({{grouping}}, n, fill = .data[[kategoria]] %>% fct_rev(), alpha = Rekomendacja)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_alpha_discrete(range = c(0.6, 0.9)) +
    facet_grid(~Rekomendacja,  scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0), legend.position = "bottom") +
    scale_y_continuous(labels = c(seq(30, 800, 30) %>% rev(), seq(0, 800, 30)), breaks = c(seq(-780, -30,30), seq(0, 800, 30))) +
    guides(alpha = F) +
    labs(x = "", y = "Liczba wskazań", fill = "")  +
    scale_fill_jco()
}


funkcja_title <-  function(x, title) {
  x +
    labs(title = title)
}


# Wektory referencyjne ----------------------------------------------------


lista_kategorie <- c(Populacja = "Kat1_Populacja",
                     Komparator = "Kat1_Komparator",
                     Bezpieczeństwo = "Kat1_Bezpieczeństwo",
                     RSS = "Kat1_RSS",
                     Wpływ_budżet = "Kat1_Wpływ_budżet",
                     Korzyść_kliniczna = "Kat1_Korzyść_kliniczna",
                     Efektywność_kosztowa = "Kat1_Efektywność_kosztowa") 


nazwy_kategorie <- str_replace(lista_kategorie, "Kat1_", "") %>% 
  str_replace_all("_", " ") %>% 
  str_to_lower()

# Obliczenia --------------------------------------------------------------



czestosci_kraje <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered, grouping = Kraj)
wykresy_kraje <- map2(czestosci_kraje,  lista_kategorie, funkcja_ggplot2, grouping = Kraj)
tytuły_kraje <- str_c("Rozkład pozytywnych i negatywnych rekomendacji ze względu na ", nazwy_kategorie)
wykresy_kraje2 <- map2(wykresy_kraje, tytuły_kraje, funkcja_title)
wykresy_kraje2
pliki_kraje <- str_c("Krok - 9_5 Rozkład pozytywnych i negatywnych rekomendacji ze względu na ", 
                     nazwy_kategorie, "(Kraje).png")

walk2(pliki_kraje, wykresy_kraje2, ggsave, device = "png", path = "./Wykresy/Wykresy 9_5",
      width = 12, height = 8, dpi = 300)




czestosci_obszar <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered, grouping = Obszar_terapeutyczny)
wykresy_obszar <- map2(czestosci_obszar,  lista_kategorie, funkcja_ggplot2, grouping = Obszar_terapeutyczny)
wykresy_obszar2 <- map2(wykresy_obszar, tytuły_kraje, funkcja_title)

wykresy_obszar2

pliki_obszar <- str_c("Krok - 9_5 Rozkład pozytywnych i negatywnych rekomendacji ze względu na ", 
                      nazwy_kategorie, "(Obszary terapeutyczne).png")

walk2(pliki_obszar, wykresy_obszar2, ggsave, device = "png", path = "./Wykresy/Wykresy 9_5",
      width = 12, height = 8, dpi = 300)

