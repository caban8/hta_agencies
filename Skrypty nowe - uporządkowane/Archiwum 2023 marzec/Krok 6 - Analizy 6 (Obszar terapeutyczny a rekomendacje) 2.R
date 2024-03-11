
# Funkcja połączenia ------------------------------------------------------

funkcja_join <- function(freq, chi, kategoria, filtr) {
  
  dod_obserwacje <- setdiff(freq$Filtr_freq, chi$Filtr_chi)
  dod_n <- freq$N %>% filter(Grupa %in% dod_obserwacje) %>% select(n) %>% as_vector()
  
  
  if (length(dod_obserwacje) > 0) {
    
    for (i in 1:length(dod_obserwacje)) {
      
      chi$Chi <- chi$Chi %>% 
        add_case() %>% 
        mutate(Obszar_terapeutyczny = replace_na(Obszar_terapeutyczny, dod_obserwacje[[i]]), n = replace_na(n, dod_n[[i]]))
    }
  }
  
  left_join(freq$Freq,chi$Chi, by = "Obszar_terapeutyczny") %>% 
    mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
           p2 = if_else(.data[[kategoria]] == filtr, p2, ".") %>% na_if("."),
           p = replace_na(p, 1.1),
           significant = if_else(p < 0.05, T, F),
           Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))
  
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



freq_pop2 <- funkcja_czestosci(Baza_analizy1,  Obszar_terapeutyczny, "Kat1_Populacja")
chi_pop2 <- funkcja_chiKwadrat(Baza_analizy1,  Obszar_terapeutyczny, "Kat1_Populacja")




lista_freq <-  map(lista_kategorie, funkcja_czestosci, x = Baza_analizy1, grouping = Obszar_terapeutyczny)
lista_chi <-  map(lista_kategorie, funkcja_chiKwadrat, x = Baza_analizy1, grouping = Obszar_terapeutyczny)


# Łączenie baz ------------------------------------------------------------

lista_join <- list(
  freq = lista_freq,
  chi = lista_chi,
  kategoria = lista_kategorie,
  filtr = filtr_join
)



lista_połączone <- pmap(lista_join, funkcja_join) 



# Wykresy -----------------------------------------------------------------

wykresy_nazwy <- str_c("Procent pozytywnych rekomendacji w zależności od ", names(lista_połączone), " w podziale\n na obszar terapeutyczny")
pliki_nazwy <- str_c("Analiza 6B - ",  names(lista_połączone)," a rekomendacje - w podziale na obszar terapeutyczny.png")
pliki_nazwy


lista_wykresy <- map2(lista_połączone, lista_kategorie,  funkcja_ggplot, grouping = "Obszar_terapeutyczny")
lista_wykresy2 <- map2(lista_wykresy, wykresy_nazwy, funkcja_title)
lista_wykresy2




walk2(pliki_nazwy, lista_wykresy2, ggsave, device = "png", path = "./Wykresy/Wykresy 6b",
      width = 12, height = 8, dpi = 300)





