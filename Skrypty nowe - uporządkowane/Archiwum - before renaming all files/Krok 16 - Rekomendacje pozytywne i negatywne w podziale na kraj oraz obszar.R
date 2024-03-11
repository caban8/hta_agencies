
# 16 - Pozytywne i negatywne w zależności od kraju i obszaru terap --------

library(flextable)
library(officer)
library(tidyverse)
library(rstatix)
source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")



# Tworzę funkcję flextable ------------------------------------------------


flex_freq <- function(x, IV) { x %>% 
    flextable() %>% 
    autofit() %>% 
    delete_part() %>% 
    add_header_row(values = c(IV, "Positive", "Negative", "Total")) %>% 
    border(border.bottom = fp_border(width = 1.5), border.top = fp_border(width = 1.5), part = "header") %>% 
    border(border.bottom = fp_border(width = 1.5), i = nrow(x)) %>% 
    font(fontname = "Times New Roman", part = "all") %>% 
    align(j = -1, align = "center") %>% 
    align( align = "center", part = "header") 
}




# Obliczam częstości dla krajów -------------------------------------------




Analizy_korzyść2 %>% 
  freq_table(Kraj, Rekomendacja) %>% 
  mutate(freq = Cabix::str_stat(n, prop, percent = T, comma = F)) %>% 
  group_by(Kraj) %>% 
  mutate(Total = sum(n)) %>% 
  ungroup() %>% 
  select(Kraj, Rekomendacja, freq, Total) %>% 
    pivot_wider(names_from = Rekomendacja, values_from = freq) %>% 
  select(Kraj, Positive, Negative, Total) %>% 
  flex_freq("Agency") %>% 
  save_as_docx(path = "./Frontiers/Tables/Rekomendacje a kraj - częstości.docx")

  



Analizy_korzyść2 %>% 
  freq_table(Obszar_terapeutyczny, Rekomendacja) %>% 
  mutate(freq = Cabix::str_stat(n, prop, percent = T, comma = F)) %>% 
  group_by(Obszar_terapeutyczny) %>% 
  mutate(Total = sum(n)) %>% 
  ungroup() %>% 
  select(Obszar_terapeutyczny, Rekomendacja, freq, Total) %>% 
    pivot_wider(names_from = Rekomendacja, values_from = freq) %>% 
  select(Obszar_terapeutyczny, Positive, Negative, Total) %>% 
  mutate(Negative = if_else(is.na(Negative), "0 (0%)", Negative)) %>% 
  flex_freq("Therapeutic Area") %>% 
  save_as_docx(path = "./Frontiers/Tables/Rekomendacje a obszar terapeutyczny - częstości.docx")



