
library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")



source("./Skrypty nowe - uporządkowane/Przygotowanie baz/Analizy - korzystne a niekorzystne.R", encoding = "UTF-8")
source("./Wektory referencyjne/Krok 13 - Wektory referencyjne.R", encoding = "UTF-8")





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





Analizy_korzyść2 <- Analizy_korzyść %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2, Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2,  Kat1_Wpływ_budżet2, Rekomendacja) %>% 
  pivot_longer(2:5) %>% 
  drop_na() %>% 
  mutate(
    name = str_sub(name, 6) %>% str_replace_all("_", " ") %>% str_replace_all("2", ""),
    value = fct_relevel(value, "Korzystna", "Niekorzystna")
  )  %>% 
  count(Kraj, name, Rekomendacja, value) %>% 
  complete(Kraj, name, Rekomendacja, value, fill = list(n = 0)) %>% 
  mutate(n = if_else(Rekomendacja == "Negatywna", n * -1, n %>% as.double()))


Analizy_korzyść2 %>% filter(Kraj == "Niemcy")

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
    name = plyr::mapvalues(name, kryteria_pl, kryteria_ang) %>% factor(levels = kryteria_ang),
    Rekomendacja = plyr::mapvalues(Rekomendacja, rekomendacja_pl,rekomendacja_ang)
  )





# Tworze wykres odniesienia w celu ustalenia równych skal y ---------------

pozytywna <- Analizy_korzyść3 %>% 
  filter(Rekomendacja == "Positive")

pozytywna2 <-  pozytywna %>% 
  add_row(pozytywna %>% mutate(Rekomendacja = "Negative")) %>% 
  mutate(n = if_else(Rekomendacja == "Negative", n * -1, n %>% as.double()))

plot_reference <- pozytywna2 %>% 
ggplot(mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(2/3, 0.2)) +
  scale_fill_manual(values = fill_4colors) +
  facet_grid(fct_rev(Rekomendacja) ~ Kraj,  scales = "free", switch = "x") +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        strip.background = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(color = "white"), 
        plot.background = element_rect(fill = "white", color = "white")) + 
  guides(alpha = "none") +
  scale_y_continuous(breaks = seq(-200,200,25), labels =  c(seq(0,200,25) %>% rev(), seq(25,200,25)) ) +
  ggExtra::removeGrid()

plot_reference_flipped <- pozytywna2 %>% 
ggplot(mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(2/3, 0.2)) +
  scale_fill_manual(values = fill_4colors) +
  facet_grid(Kraj ~ Rekomendacja,  scales = "free", switch = "x") +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        strip.background = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(color = "white"), 
        plot.background = element_rect(fill = "white", color = "white")) + 
  guides(alpha = "none") +
  scale_y_continuous(breaks = seq(-200,200,25), labels =  c(seq(0,200,25) %>% rev(), seq(25,200,25)) ) +
  ggExtra::removeGrid() +
  coord_flip()


# Opcja 2 -----------------------------------------------------------------



old_reference <- plot_reference %>% ggplot_build()
old_reference_flipped <- plot_reference_flipped %>% ggplot_build()




plot1 <- Analizy_korzyść3 %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), 
            fill="white", alpha=1, color = "black") +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(2/3, 0.2)) +
  scale_fill_manual(values = fill_4colors) +
  facet_grid(fct_rev(Rekomendacja) ~ Kraj,  scales = "free", switch = "x") +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.background = element_rect( color = "black"), 
        plot.background = element_rect(fill = "white", color = "white")) + 
  guides(alpha = "none") +
  scale_y_continuous(breaks = seq(-200,200,25), labels =  c(seq(0,200,25) %>% rev(), seq(25,200,25)) ) +
  labs(fill = "", y = "") + 
  ggExtra::removeGrid() +
    ggtitle("The ratio of favourable to unfavourable evaluations separately for the positive and negative recommendations", 
          "By country and criterium") 


plot1_flipped <- Analizy_korzyść3 %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), 
            fill="white", alpha=1, color = "black") +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(2/3, 0.2)) +
  scale_fill_manual(values = fill_4colors) +
  facet_grid(Kraj ~ Rekomendacja, scales = "free", switch = "x") +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        strip.text = element_text(size = 8),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.background = element_rect( color = "black"), 
        plot.background = element_rect(fill = "white", color = "white")) + 
  guides(alpha = "none") +
  scale_y_continuous(breaks = seq(-200,200,25), labels =  c(seq(0,200,25) %>% rev(), seq(25,200,25)) ) +
  labs(fill = "", y = "") +
  ggExtra::removeGrid() +
  ggtitle("The ratio of favourable to unfavourable evaluations separately for the positive and negative recommendations", 
          "By country and criterium") +
  coord_flip()




old_plot <- plot1 %>% ggplot_build()



old_plot$layout <- old_reference$layout


plot(ggplot_gtable(old_plot))

png("./Frontiers/Graphs/(K10_5) Graph 10_5 - Ratio korzystnych do niekorzystnych ocen w ramach pozytywnych i negatywnych rekomendacji - w podziale na kraj i kryterium.png",
    res = 300, units = "in", width = 20, height = 10)
plot(ggplot_gtable(old_plot))

dev.off()



## Flipped


old_plot_flipped <- plot1_flipped %>% ggplot_build()

old_plot_flipped$layout <- old_reference_flipped$layout

plot(ggplot_gtable(old_plot_flipped))

png("./Frontiers/Graphs/(K10_5) Graph 10_5 - Ratio korzystnych do niekorzystnych ocen w ramach pozytywnych i negatywnych rekomendacji - w podziale na kraj i kryterium (flipped).png",
    res = 300, units = "in", width = 20, height = 10)
plot(ggplot_gtable(old_plot_flipped))
dev.off()

