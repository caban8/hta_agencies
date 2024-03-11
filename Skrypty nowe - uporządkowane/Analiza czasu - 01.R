
# Porównanie krajów pod względem szybkości wydawania rekomendacji ---------

pacman::p_load(readxl, tidyverse)

df <- read_excel("Baza filtr na dates 2024-03-04.xlsx") 

df <- df %>% 
  select(-c(...1, ...2))



# Sprawdzam czystość obserwacji -------------------------------------------



df %>% 
  count(ID, Agencja_kraj) %>% 
  filter(n != 1)




# Porównanie krajów pod kątem szybkości wydawania rek ---------------------


recommendation_order <- df %>% 
  select(ID, Kraj, dni_do_rekomendacji) %>% 
  group_by(ID) %>% 
  arrange(ID, dni_do_rekomendacji) %>% 
  mutate(recommendation_order = row_number()) %>% 
  ungroup()

 
# Sprawdźmy pod kątem statystyk opisowych
recommendation_order %>% 
  group_by(Kraj) %>% 
  summarise(
    order_median = median(recommendation_order)
  ) %>% 
  arrange(order_median)



df %>% 
  count(ID, Kraj) %>% 
  select(-n) %>% 
  unique() %>% 
  count(ID) %>% 
  filter(n >= 12)
