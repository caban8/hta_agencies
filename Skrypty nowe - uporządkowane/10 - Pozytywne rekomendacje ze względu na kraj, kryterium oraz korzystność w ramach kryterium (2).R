library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")



source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")
source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")

#https://stackoverflow.com/questions/57807436/how-to-fill-grouped-dodged-bars-with-another-variable-in-ggplot2



# Ustalam parametry dla wykresów ------------------------------------------


scales::show_col(few_pal()(7))

mycolors <- brewer.pal(9, "Pastel1")

fill_color <- "#FAA43A"
border_color <-  "#5DA5DA"
fill_4colors <- c( "#5DA5DA","#FAA43A", "#60BD68", "#F17CB0")


alpha1 = 2/3
outlier.alpha1 = 0.6
outlier.size1 = 3
size1 = 1





# Defininuje obserwację ---------------------------------------------------



Analizy_korzyść2 <- Analizy_korzyść %>% 
  filter(Rekomendacja == "Pozytywna")  %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2, Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2,  Kat1_Wpływ_budżet2) %>% 
  pivot_longer(2:5) %>% 
  drop_na() %>% 
  mutate(
    name = str_sub(name, 6) %>% str_replace_all("_", " ") %>% str_replace_all("2", ""),
    value = fct_relevel(value, "Korzystna", "Niekorzystna")
         )  %>% 
  count(Kraj, name, value) 


kryteria_pl
Analizy_korzyść3 <- Analizy_korzyść2 %>% 
  add_row(
    tibble(
      Kraj = c("Francja", "Francja", "Niemcy", "Niemcy"),
      name = c("Efektywność kosztowa", "Wpływ budżet", "Efektywność kosztowa", "Wpływ budżet"),
      value = rep("Korzystna", 4),
      n = rep(0, 4)
      
    )) %>% 
  mutate(
    Kraj = plyr::mapvalues(Kraj, kraje_pl, kraje_ang),
    name = plyr::mapvalues(name, kryteria_pl, kryteria_ang) %>% factor(levels = kryteria_ang)
      )


# Opcja 2 -----------------------------------------------------------------



ggplot(data = Analizy_korzyść3, mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(2/3, 0.2)) +
  scale_fill_manual(values = fill_4colors) +
  facet_wrap(~ Kraj, strip.position = "bottom", scales = "free_x", nrow = 1) +
      theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        strip.background = element_blank(),
                  legend.position = "bottom",
        panel.background = element_rect(color = "white"), 
        plot.background = element_rect(fill = "white", color = "white")) + 
  guides(alpha = "none") +
  labs(fill = "", y = "") +
  ggtitle("The ratio of favourable to unfavourable evaluations within the positive recommendations", 
          "By country and criterium") +
  ggExtra::removeGrid()

ggsave("(K10) Graph 10 - Ratio korzystnych do niekorzystnych ocen w ramach pozytywnych rekomendacji - w podziale na kraj i kryterium.png",
       device = "png", dpi = 300, width = 20, height = 10, path = "./Frontiers/Graphs")
  
  
  
  

