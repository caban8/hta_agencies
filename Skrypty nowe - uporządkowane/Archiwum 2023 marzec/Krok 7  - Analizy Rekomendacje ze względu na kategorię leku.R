
# Krok 7 - Analizy ze względu na kategorię leku ---------------------------




# Funkcja połączenia ------------------------------------------------------

funkcja_joinKat <- function(freq, chi, kategoria, filtr) {
  
  dod_obserwacje <- setdiff(freq$Filtr_freq, chi$Filtr_chi)
  dod_n <- freq$N %>% filter(Grupa %in% dod_obserwacje) %>% select(n) %>% as_vector()
  
  
  if (length(dod_obserwacje) > 0) {
    
    for (i in 1:length(dod_obserwacje)) {
      
      chi$Chi <- chi$Chi %>% 
        add_case() %>% 
        mutate(Kategoria_lek = replace_na(Kategoria_lek, dod_obserwacje[[i]]), n = replace_na(n, dod_n[[i]]))
    }
  }
  
  left_join(freq$Freq,chi$Chi, by = "Kategoria_lek") %>% 
    mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
           p2 = if_else(.data[[kategoria]] == filtr, p2, ".") %>% na_if("."),
           p = replace_na(p, 1.1),
           significant = if_else(p < 0.05, T, F),
           Kategoria_lek = str_c(Kategoria_lek, " (n = ", n.y, ")"))
  
  
  
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



Baza_analizy1$Kategoria_lek

lista_freqKat <-  map(lista_kategorie, funkcja_czestosci, x = Baza_analizy1, grouping = Kategoria_lek)
lista_chiKat <-  map(lista_kategorie, funkcja_chiKwadrat, x = Baza_analizy1, grouping = Kategoria_lek)
lista_freqKat

# Łączenie baz ------------------------------------------------------------

lista_joinKat <- list(
  freq = lista_freqKat,
  chi = lista_chiKat,
  kategoria = lista_kategorie,
  filtr = filtr_join
)



lista_połączoneKat <- pmap(lista_joinKat, funkcja_joinKat) 
lista_połączoneKat



# Wykresy -----------------------------------------------------------------

wykresy_nazwyKat <- str_c("Procent pozytywnych rekomendacji w zależności od ", names(lista_połączoneKat), " w podziale\n na kategorię leku")
pliki_nazwyKat <- str_c("Analiza 6c - ",  names(lista_połączoneKat)," a rekomendacje - w podziale na kategorię leku.png")
pliki_nazwyKat


lista_wykresyKat <- map2(lista_połączoneKat, lista_kategorie,  funkcja_ggplot, grouping = "Kategoria_lek")
lista_wykresyKat2 <- map2(lista_wykresyKat, wykresy_nazwyKat, funkcja_title)
lista_wykresyKat2




walk2(pliki_nazwyKat, lista_wykresyKat2, ggsave, device = "png", path = "./Wykresy/Wykresy 6c",
      width = 12, height = 8, dpi = 300)






