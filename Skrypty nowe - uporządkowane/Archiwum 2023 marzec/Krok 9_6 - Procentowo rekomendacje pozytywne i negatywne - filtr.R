

# Krok 9.6 - Pozytywne i negatywne rekomendacje - minimum 6 oceniających agencji
# W procentach


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
    mutate(percent = n / sum(n) * 100) %>% 
    ungroup() %>% 
    group_by({{grouping}}, Rekomendacja) %>% 
    mutate(percent2 = n / sum(n) * 100) %>% 
    ungroup()
  
  results <- list(filtr, frequencies2, frequencies_total)
  names(results) <- c("Filtr_freq", "Freq", "N")
  results
} 


czestosci_kraje <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered, grouping = Kraj)

wynik1 <-  funkcja_czestosci2(Baza_analizy1_filtered, Kraj, "Kat1_Populacja")
wynik1$Freq


funkcja_czestosci2(Baza_analizy1_filtered, Obszar_terapeutyczny, "Kat1_Populacja")


# Funkcja wykresy ---------------------------------------------------------




funkcja_ggplot2 <- function(x, kategoria, grouping) {
  
  x$Freq %>% 
    mutate(percent2 = as.double(percent2),
           percent2 = if_else(Rekomendacja == "Negatywna", (percent2 * -1), percent2)) %>% 
    ggplot(aes({{grouping}}, percent2, fill = .data[[kategoria]] %>% fct_rev(), alpha = Rekomendacja)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_alpha_discrete(range = c(0.6, 0.9)) +
    facet_grid(~Rekomendacja,  scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0), legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = str_c( c(seq(10, 100, 10) %>% rev(), seq(0, 100, 10)), "%"),
                       breaks = c(seq(-100, -10,10), seq(0, 100, 10))) +
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



# Obliczenia ze względu na kraje ------------------------------------------





czestosci_kraje <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered, grouping = Kraj)
wykresy_kraje <- map2(czestosci_kraje,  lista_kategorie, funkcja_ggplot2, grouping = Kraj)
wykresy_kraje



tytuły_kraje <- str_c("Rozkład procentowy pozytywnych i negatywnych rekomendacji ze względu na ", nazwy_kategorie)
wykresy_kraje2 <- map2(wykresy_kraje, tytuły_kraje, funkcja_title)
wykresy_kraje2
pliki_kraje <- str_c("Krok - 9_6 Rozkład procentowy pozytywnych i negatywnych rekomendacji ze względu na ", 
                     nazwy_kategorie, "(Kraje).png")

walk2(pliki_kraje, wykresy_kraje2, ggsave, device = "png", path = "./Wykresy/Wykresy 9_6 - procentowo",
      width = 12, height = 8, dpi = 300)


# Obliczenia ze względu na obszar -----------------------------------------




czestosci_obszar <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered, grouping = Obszar_terapeutyczny)
wykresy_obszar <- map2(czestosci_obszar,  lista_kategorie, funkcja_ggplot2, grouping = Obszar_terapeutyczny)
wykresy_obszar2 <- map2(wykresy_obszar, tytuły_kraje, funkcja_title)

wykresy_obszar2

pliki_obszar <- str_c("Krok - 9_6 Rozkład procentowy pozytywnych i negatywnych rekomendacji ze względu na ", 
                      nazwy_kategorie, "(Obszary terapeutyczne).png")

walk2(pliki_obszar, wykresy_obszar2, ggsave, device = "png", path = "./Wykresy/Wykresy 9_6 - procentowo",
      width = 12, height = 8, dpi = 300)



# Obliczenia ze względu na obszar - wybrane obszary -----------------------



Baza_analizy1_filtered2 <- Baza_analizy1_filtered %>% 
  filter(Obszar_terapeutyczny %in% c("Pulmonologia", "Onkologia", "Neurologia", "Kardiologia",
                                     "Immunologia", "Diabetologia","Choroby zakaźne i infekcyjne" ,
                                     "Choroby rzadkie" ))

tytuły_kraje_2 <- str_c("Rozkład procentowy pozytywnych i negatywnych rekomendacji ze względu na ", nazwy_kategorie, "\n wybrane obszary")
czestosci_obszar_2 <- map(lista_kategorie, funkcja_czestosci2, x = Baza_analizy1_filtered2, grouping = Obszar_terapeutyczny)
wykresy_obszar_2 <- map2(czestosci_obszar_2,  lista_kategorie, funkcja_ggplot2, grouping = Obszar_terapeutyczny)
wykresy_obszar2_2 <- map2(wykresy_obszar_2, tytuły_kraje_2, funkcja_title)

wykresy_obszar2_2

pliki_obszar_2 <- str_c("Krok - 9_6 Rozkład procentowy pozytywnych i negatywnych rekomendacji ze względu na ", 
                      nazwy_kategorie, "(Obszary terapeutyczne - pomniejszone).png")

walk2(pliki_obszar_2, wykresy_obszar2_2, ggsave, device = "png", path = "./Wykresy/Wykresy 9_6 - procentowo",
      width = 12, height = 8, dpi = 300)

