
library(tidyverse)


source("./Funkcje/Krok 12 - Funkcje pomocnicze.R")
source("./Funkcje/Krok 13 - Funkcja częstości with 0.R")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")
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





# Przygotowanie danych ----------------------------------------------------




Baza_odniesienie <-  Baza_analizy1 %>% 
  mutate(across(starts_with("Kat"), ~if_else(is.na(.) == T, "No reference", "Reference") %>% factor(), 
                .names = "{.col}_2")) %>% 
  mutate(across(starts_with("Kat"), ~if_else(is.na(.) == T, 0, 1) %>% factor(), 
                .names = "{.col}_3")) %>% 
  mutate(Rekomendacja2 = plyr::mapvalues(Rekomendacja, c("Pozytywna", "Negatywna"), 1:0) %>% as.double()) %>% 
  mutate(
    Kraj = plyr::mapvalues(Kraj, kraje_pl, kraje_ang),
    Kraj = factor(Kraj, levels = kraje_ang_lvls),
    Obszar_terapeutyczny = plyr::mapvalues(Obszar_terapeutyczny, Obszary_old, Obszary_new)
  )  %>% 
  filter(Kraj != "Szwecja", !is.na(Rekomendacja))






# Przygotowanie funkcji ---------------------------------------------------


 
  

graph_odniesienia2 <- function(x) {
  
  x %>% 
  select(Kraj, all_of(wektor_kategorie)) %>% 
    pivot_longer(-1) %>% 
    filter(Kraj != "Szwecja") %>% 
    rstatix::freq_table(Kraj, name, value) %>% 
    ggplot(aes(Kraj, prop, fill = value)) +
    geom_bar(stat = "identity",  alpha = 2/3) +
    coord_flip() +
    ggthemes::scale_fill_few() +
    theme(legend.position = "bottom") +
    facet_wrap(~name) +
    labs(x = "", fill = "", y = "") +
    scale_y_continuous(breaks = seq(0,100,20), labels = str_c(seq(0,100,20), "%")) +
    ggExtra::removeGrid()  + 
    theme(panel.background = element_rect(fill = "white"))
}




graph_odniesienia2 <- function(x, grupowanie) {
  
  x %>% 
    filter(Kraj != "Szwecja", !is.na(Rekomendacja)) %>% 
    select(.data[[grupowanie]], all_of(wektor_kategorie)) %>% 
    pivot_longer(-1) %>% 
    rstatix::freq_table(grupowanie, name, value) %>% 
    ggplot(aes(.data[[grupowanie]], prop, fill = value)) +
    geom_bar(stat = "identity",  alpha = alpha1) +
    coord_flip() +
    scale_fill_manual(values = fill_2colors) +
    theme(legend.position = "bottom") +
    facet_wrap(~name %>% factor(levels = c("Clinical efficacy" , "Safety", "Cost effectiveness",  "Budget impact"))) +
    labs(x = "", fill = "", y = "") +
    scale_y_continuous(breaks = seq(0,100,20), labels = str_c(seq(0,100,20), "%")) +
    ggExtra::removeGrid()  + 
    theme(panel.background = element_rect(fill = "white"))
}





# Wykresy -----------------------------------------------------------------

Graphs_references <- tibble(
  Dataset = c("Full", "Restricted","Full", "Restricted"),
  Title = c(
    "The percent of references by countries - whole sample", 
    "The percent of references by countries - recommendations by at least 6 agencies",
    "The percent of references by therapeutic area - whole sample", 
    "The percent of references by therapeutic area - recommendations by at least 6 agencies"
  ),
  graphs = list(
    Full_kraj = Baza_odniesienie %>% graph_odniesienia2("Kraj"),
    Restricted_kraj = Baza_odniesienie %>% filter(liczba_agencji >= 6) %>%   graph_odniesienia2("Kraj"),
    Full_obszar = Baza_odniesienie %>% graph_odniesienia2("Obszar_terapeutyczny"),
    Restricted_obszar = Baza_odniesienie %>% filter(liczba_agencji >= 6) %>%   graph_odniesienia2("Obszar_terapeutyczny")
  )
) %>% 
  mutate(
    graphs2 = map2(graphs, Title, function(x, title) {x + ggtitle(title)}),
    filename = str_c("./Frontiers/Graphs/",  Title, ".png"),
    graphs3 = map(graphs2, function(x) {x + geom_label(aes(label = prop), 
                                                       data = . %>% filter(value == "Reference"),
                                                       size = 2.5, 
                                                       fill = fill_2colors[2])}),
    filename2 = str_c("./Frontiers/Graphs/",  Title, "(with labels).png"),
  )


Graphs_references %>% 
  select(filename, plot = graphs2) %>% 
  pwalk(ggsave, device = "png", dpi = 300, width = 10, height = 10)


# Wersja z labels

Baza_odniesienie %>% 
  graph_odniesienia2("Kraj") +
  geom_label(aes(label = prop), data = . %>% filter(value == "Reference"), size = 2.5, fill = fill_2colors[2]) +
  guides(label = "none")

Graphs_references %>% 
  select(filename = filename2, plot = graphs3) %>% 
  pwalk(ggsave, device = "png", dpi = 300, width = 10, height = 10)


# Wykresy reversed  -------------------------------------------------------



graph_reversed <- function(x, grupowanie) { 
  x %>% 
    filter(Kraj != "Szwecja", !is.na(Rekomendacja)) %>% 
    select(.data[[grupowanie]], all_of(wektor_kategorie)) %>% 
    pivot_longer(-1) %>% 
    rstatix::freq_table(grupowanie, name, value) %>% 
    ggplot(aes( name %>% factor(levels = kryteria_ang) %>% fct_rev(),
                prop, fill = value)) +
    geom_bar(stat = "identity",  alpha = alpha1) +
    coord_flip() +
    scale_fill_manual(values = fill_2colors) +
    theme(legend.position = "bottom") +
    facet_wrap(~.data[[grupowanie]]
                ) +
    labs(x = "", fill = "", y = "") +
    scale_y_continuous(breaks = seq(0,100,20), labels = str_c(seq(0,100,20), "%")) +
    ggExtra::removeGrid()  + 
    theme(panel.background = element_rect(fill = "white"))
}



Graphs_reversed <- tibble(
  Dataset = c("Full", "Restricted","Full", "Restricted"),
  Title = c(
    "The percent of references by countries - whole sample (reversed)", 
    "The percent of references by countries - recommendations by at least 6 agencies (reversed)",
    "The percent of references by therapeutic area - whole sample (reversed)", 
    "The percent of references by therapeutic area - recommendations by at least 6 agencies (reversed)"
  ),
  graphs = list(
    Full_kraj = Baza_odniesienie %>% graph_reversed("Kraj"),
    Restricted_kraj = Baza_odniesienie %>% filter(liczba_agencji >= 6) %>%   graph_reversed("Kraj"),
    Full_obszar = Baza_odniesienie %>% graph_reversed("Obszar_terapeutyczny"),
    Restricted_obszar = Baza_odniesienie %>% filter(liczba_agencji >= 6) %>%   graph_reversed("Obszar_terapeutyczny")
  )
) %>% 
  mutate(
    graphs2 = map2(graphs, Title, function(x, title) {x + ggtitle(title)}),
    filename = str_c("./Frontiers/Graphs/",  Title, ".png"),
  )

Graphs_reversed$graphs2

Graphs_reversed %>% 
  select(filename, plot = graphs2) %>% 
  pwalk(ggsave, device = "png", dpi = 300, width = 10, height = 10)


