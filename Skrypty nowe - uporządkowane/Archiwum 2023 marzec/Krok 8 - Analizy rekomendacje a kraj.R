
Baza_analizy1 <- read_csv("Baza danych - 16 lutego 2022.csv")

# Krok 8 - Analizy rekomendacje a kraj ------------------------------------





# Funkcja połączenia ------------------------------------------------------

funkcja_joinKraj <- function(freq, chi, kategoria, filtr) {
  
  dod_obserwacje <- setdiff(freq$Filtr_freq, chi$Filtr_chi)
  dod_n <- freq$N %>% filter(Grupa %in% dod_obserwacje) %>% select(n) %>% as_vector()
  
  
  if (length(dod_obserwacje) > 0) {
    
    for (i in 1:length(dod_obserwacje)) {
      
      chi$Chi <- chi$Chi %>% 
        add_case() %>% 
        mutate(Kraj = replace_na(Kraj, dod_obserwacje[[i]]), n = replace_na(n, dod_n[[i]]))
    }
  }
  
  left_join(freq$Freq,chi$Chi, by = "Kraj") %>% 
    mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
           p2 = if_else(.data[[kategoria]] == filtr, p2, ".") %>% na_if("."),
           p = replace_na(p, 1.1),
           significant = if_else(p < 0.05, T, F),
           Kraj = str_c(Kraj, " (n = ", n.y, ")"))
  
  
  
}





# Wektory referencyjne ----------------------------------------------------



filtr_join <- c("On Label", "Dobrze wybrany", "Nie można stwierdzić  ", "Jest", "Oszczędności", "Nie wykazano przewagi", "Wykazano przewagę")



lista_kategorie <- c(Populacja = "Kat1_Populacja",
                     Komparator = "Kat1_Komparator",
                     Bezpieczeństwo = "Kat1_Bezpieczeństwo",
                     RSS = "Kat1_RSS",
                     Wpływ_budżet = "Kat1_Wpływ_budżet",
                     Korzyść_kliniczna = "Kat1_Korzyść_kliniczna",
                     Efektywność_kosztowa = "Kat1_Efektywność_kosztowa") 





# Obliczenia --------------------------------------------------------------





lista_freqKraj <-  map(lista_kategorie, funkcja_czestosci, x = Baza_analizy1, grouping = Kraj)
lista_chiKraj <-  map(lista_kategorie, funkcja_chiKwadrat, x = Baza_analizy1, grouping = Kraj)
lista_freqKraj

# Łączenie baz ------------------------------------------------------------

lista_joinKraj <- list(
  freq = lista_freqKraj,
  chi = lista_chiKraj,
  kategoria = lista_kategorie,
  filtr = filtr_join
)



lista_połączoneKraj <- pmap(lista_joinKraj, funkcja_joinKraj) 
lista_połączoneKraj



# Wykresy -----------------------------------------------------------------

wykresy_nazwyKraj <- str_c("Procent pozytywnych rekomendacji w zależności od ", names(lista_połączoneKraj), " w podziale\n na kraj")
pliki_nazwyKraj <- str_c("Analiza 6d - ",  names(lista_połączoneKraj)," a rekomendacje - w podziale na kraj.png")
pliki_nazwyKraj


lista_wykresyKraj <- map2(lista_połączoneKraj, lista_kategorie,  funkcja_ggplot, grouping = "Kraj")
lista_wykresyKraj2 <- map2(lista_wykresyKraj, wykresy_nazwyKraj, funkcja_title)
lista_wykresyKraj2




walk2(pliki_nazwyKraj, lista_wykresyKraj2, ggsave, device = "png", path = "./Wykresy/Wykresy 6d",
      width = 12, height = 8, dpi = 300)







