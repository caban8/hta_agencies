
library(tidyverse)
library(flextable)
source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")
source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")



# Ustalam bazę ------------------------------------------------------------


Analizy_korzyść2 <- Analizy_korzyść %>% 
  mutate(
    Kraj = plyr::mapvalues(Kraj, kraje_pl, kraje_ang),
    Obszar_terapeutyczny = plyr::mapvalues(Obszar_terapeutyczny, Obszary_old, Obszary_new),
    Rekomendacja = plyr::mapvalues(Rekomendacja, c("Pozytywna", "Negatywna"), c("Positive", "Negative"))
  )  


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





# Krok 15 - Rekomendacje a kraje w podziale na obszary terapeutycz --------

obszar_kraj <- Analizy_korzyść2 %>% 
  select(Obszar_terapeutyczny, Kraj, Rekomendacja) %>% 
  drop_na() %>% 
  group_by(Obszar_terapeutyczny) %>% 
  nest() %>% 
  mutate(freq = map(data, function(x) {x %>% rstatix::freq_table(Kraj, Rekomendacja, na.rm = F)})) 




obszar_kraj %>% 
  select(Obszar_terapeutyczny, freq) %>% 
  unnest() %>% 
  ggplot(aes(Kraj, prop, fill = Rekomendacja)) +
  geom_bar(stat = "identity", alpha = alpha1) +
  facet_wrap(~Obszar_terapeutyczny) +
  scale_fill_manual(values = fill_2colors) +
  labs(x = "", fill = "", y = "") +
  scale_y_continuous(breaks = seq(0,100,20), labels = str_c(seq(0,100,20), "%")) +
  ggExtra::removeGrid()  + 
  theme(panel.background = element_rect(fill = "white")) +
  coord_flip()


ggsave(filename = "./Frontiers/Graphs/Recommendations by country and therapuetic area.png",
       device = "png", dpi = 300, width = 10, height = 10)





# Częstości ---------------------------------------------------------------


obszar_kraj_freq <- obszar_kraj %>% 
  select(Obszar_terapeutyczny, freq) %>% 
  unnest() %>% 
  mutate(n_prop = str_c(n, " (", prop, "%)")) %>% 
  select(-c(n, prop)) %>% 
  pivot_wider(names_from = Kraj, values_from = n_prop) 


obszar_kraj_freq <- obszar_kraj_freq %>% # Dodaję puste wiersze dla negative w ramach virology i surgery, gdyż oba obszary nie miały żadnych negative
  ungroup() %>% 
  add_row(tibble(Obszar_terapeutyczny = "Virology", Rekomendacja = "Negative")) %>% 
  add_row(tibble(Obszar_terapeutyczny = "Surgery", Rekomendacja = "Negative")) 

obszar_kraj_freq %>% 
  write_excel_csv("./Frontiers/Excel/Rekomendacje a kraj w podziale na obszar.xlsx")
  
  



# Tabelka word sformatowana zgodnie z ogólnym szablonem -------------------


obszary_unique <- unique(obszar_kraj_freq$Obszar_terapeutyczny) %>% 
  sort()


obszar_kraj_freq2 <- obszar_kraj_freq %>% 
  arrange(Obszar_terapeutyczny, desc(Rekomendacja)) %>% 
  select(-Obszar_terapeutyczny) 



j <- seq(0, nrow(obszar_kraj_freq2), 2)

for (i in 1:length(obszary_unique)) {
  obszar_kraj_freq2 <- add_row(obszar_kraj_freq2, tibble(Rekomendacja = obszary_unique[i]), .before = j[i] + i)
}




j2 <- seq(1, nrow(obszar_kraj_freq2), 3)
  
obszar_kraj_freq2 %>% 
  dplyr::mutate(across(-Rekomendacja, ~replace_na(., "0"))) %>% 
  flextable() %>% 
  autofit() %>% 
  padding(j = 1, i = setdiff(1:nrow(obszar_kraj_freq2), j2), padding.left = 15) %>% 
  align(j = -1, align = "center", part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>% 
  save_as_docx(path = "./Frontiers/Tables/Rekomendacje a kraj w podziale na obszar.docx")
