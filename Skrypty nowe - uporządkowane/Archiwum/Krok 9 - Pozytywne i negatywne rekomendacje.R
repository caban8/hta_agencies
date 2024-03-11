
# Archiwum ----------------------------------------------------------------



Baza_analizy1 <- read_csv("Baza danych - 16 lutego 2022.csv")




# Krok 9 - Alternatywne przedstawienie pozytywnych rekomendacji -----------
#Kraje

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



# Proba z wykresami -------------------------------------------------------


lista_kategorie

kraj1 <-  funkcja_czestosci2(Baza_analizy1, grouping = Kraj, "Kat1_Wpływ_budżet") 

kraj1$Freq %>% 
  ggplot(aes(Rekomendacja, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range = c(0.6, 0.9)) + 
  coord_flip() +
  facet_grid(Kraj ~ .,  scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  guides(alpha = F)


kraj1$Freq %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(Kraj ~ .,  scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))



kraj1$Freq %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(Kraj ~ .,  scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))



# Warianty z facet grid dla Rekomendacji ----------------------------------


kraj1$Freq %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(~Rekomendacja,  scales = "free_y", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  guides(alpha = F)




kraj1$Freq %>% 
  mutate(n = as.double(n),
    n = if_else(Rekomendacja == "Negatywna", (n * -1), n)) %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(~Rekomendacja,  scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_y_continuous(labels = c(seq(0, 400, 50) %>% rev(), seq(0, 400, 50)), breaks = c(seq(-400, 0,50), seq(0, 400, 50))) +
  guides(alpha = F)




kraj1$Freq %>% 
  mutate(n = as.double(n),
         n = if_else(Rekomendacja == "Negatywna", (n * -1), n)) %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Populacja, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(Rekomendacja ~ .,  scales = "free_x", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  coord_flip()




# Pracujemy nad funkcją do wybranego wykresu ------------------------------

kraj1$Freq %>% 
  mutate(n = as.double(n),
         n = if_else(Rekomendacja == "Negatywna", (n * -1), n)) %>% 
  ggplot(aes(Kraj, n, fill = Kat1_Wpływ_budżet, alpha = Rekomendacja)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(~Rekomendacja,  scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top") +
  scale_y_continuous(labels = c(seq(0, 400, 50) %>% rev(), seq(0, 400, 50)), breaks = c(seq(-400, 0,50), seq(0, 400, 50))) +
  guides(alpha = F) +
  labs(x = "", y = "Liczba wskazań", fill = "")  +
  scale_fill_jco()

JCO
LANCET

funkcja_ggplot2 <- function(x, kategoria) {
  
  x %>% 
  mutate(n = as.double(n),
         n = if_else(Rekomendacja == "Negatywna", (n * -1), n)) %>% 
    ggplot(aes(Kraj, n, fill = .data[[kategoria]], alpha = Rekomendacja)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_alpha_discrete(range = c(0.6, 0.9)) +
    facet_grid(~Rekomendacja,  scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0), legend.position = "top") +
    scale_y_continuous(labels = c(seq(0, 400, 50) %>% rev(), seq(0, 400, 50)), breaks = c(seq(-400, 0,50), seq(0, 400, 50))) +
    guides(alpha = F) +
    labs(x = "", y = "Liczba wskazań", fill = "")  +
    scale_fill_aaas()
}

  

kraj1$Freq %>% 
  funkcja_ggplot2("Kat1_Populacja")
