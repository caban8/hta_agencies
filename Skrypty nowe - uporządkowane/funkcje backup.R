funkcja_czestosci_2 <- function(x, grouping, kategoria, filtr = NULL, mode = 1) {
  
  
  if (mode == 1) {
    x <- x %>% dplyr::filter({{grouping}} %in% filtr)
  } else if (mode == 2) {
    x <- x %>% dplyr::filter(!{{grouping}} %in% filtr)
  }  else {}
  
  
  x %>% 
    select({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    drop_na() %>%
    count({{grouping}}, .data[[kategoria]], Rekomendacja) %>% 
    group_by({{grouping}}, .data[[kategoria]]) %>% 
    mutate(percent = n / sum(n) * 100) %>% 
    filter(Rekomendacja == "Pozytywna") 
}

funkcja_czestosci(Baza_analizy1, Obszar_terapeutyczny, Kat1_Populacja, filtr_populacja1, mode = 1)
funkcja_czestosci_2(Baza_analizy1, Obszar_terapeutyczny, "Kat1_Populacja", filtr_populacja1, mode = 1)


#Mamy rozwiązanie!!





# Funkcje stare - bez opcji character vector ------------------------------


funkcja_czestosci <- function(x, grouping, kategoria, filtr = NULL, mode = 1) {
  
  
  if (mode == 1) {
    x <- x %>% dplyr::filter({{grouping}} %in% filtr)
  } else if (mode == 2) {
    x <- x %>% dplyr::filter(!{{grouping}} %in% filtr)
  }  else {}
  
  
  x %>% 
    select({{grouping}}, {{kategoria}}, Rekomendacja) %>% 
    drop_na() %>%
    count({{grouping}}, {{kategoria}}, Rekomendacja) %>% 
    group_by({{grouping}}, {{kategoria}}) %>% 
    mutate(percent = n / sum(n) * 100) %>% 
    filter(Rekomendacja == "Pozytywna") 
}





funkcja_chiKwadrat <- function(x, grouping, kategoria, filtr = NULL, mode = 1) {
  
  
  if (mode == 1) {
    x <- x %>% dplyr::filter({{grouping}} %in% filtr)
  } else if (mode == 2) {
    x <- x %>% dplyr::filter(!{{grouping}} %in% filtr)
  }  else {}
  
  
  
  x %>% 
    select({{grouping}}, {{kategoria}}, Rekomendacja) %>% 
    drop_na() %>% 
    group_by({{grouping}}) %>% 
    summarise(chisq_test({{kategoria}}, Rekomendacja))
  
}


funkcja_join <- function(x, y, kategoria, filtr) {
  left_join(x ,y, by = "Obszar_terapeutyczny") %>% 
    mutate(p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
           p2 = if_else({{kategoria}} == filtr, p2, ".") %>% na_if("."),
           p = replace_na(p, 1.1),
           significant = if_else(p < 0.05, T, F),
           Obszar_terapeutyczny = str_c(Obszar_terapeutyczny, " (n = ", n.y, ")"))
}


funkcja_ggplot <- function(x, kategoria) {
  
  x %>% 
    ggplot(aes(Obszar_terapeutyczny %>% fct_reorder2(percent, p), percent, fill = {{kategoria}}, alpha = significant)) +
    geom_bar(stat = "identity", position = position_dodge(0.8, preserve = "single"), width = 0.8) +
    scale_alpha_discrete(range = c(0.25, 0.75)) +
    guides(alpha = F) +
    coord_flip() + 
    scale_fill_calc() +
    scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
    labs(y = "", fill = "", x = "") +
    geom_label(aes(label = p2), fill = "white", size = 3.5, nudge_y = -10) +
    theme_economist() +
    theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
    guides(fill = guide_legend(reverse = T))
  
  
}


