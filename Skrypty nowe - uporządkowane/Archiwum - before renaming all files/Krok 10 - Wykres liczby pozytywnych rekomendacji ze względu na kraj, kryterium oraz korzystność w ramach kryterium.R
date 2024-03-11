library(tidyverse)
library(ggsci)
library(ggthemes)
Sys.setlocale("LC_ALL", "Polish")

Baza_analizy1 <- read_csv("Baza danych - 21 lutego 2022.csv") 


Baza_analizy1 %>% 
  filter(Rekomendacja == "Pozytywna") %>% 
  filter(liczba_agencji >= 6)  %>% 
  select(Kraj, Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo, Kat1_Wpływ_budżet, Kat1_Efektywność_kosztowa) 

Baza_analizy1

#https://stackoverflow.com/questions/57807436/how-to-fill-grouped-dodged-bars-with-another-variable-in-ggplot2


# Rekoduję kategorie na korzystne i niekorzystne --------------------------


Analizy_korzyść <- Baza_analizy1 %>% mutate(Kat1_Korzyść_kliniczna2 = fct_recode(Kat1_Korzyść_kliniczna, 
                                          "Korzystna" = "Wykazano przewagę" ,
                                          "Niekorzystna"  = "Nie wykazano przewagi" ,
                                          "Niekorzystna"  = "Nie można stwierdzić jednoznacznie" 
                                           ) ,
       Kat1_Bezpieczeństwo2 = fct_recode(Kat1_Bezpieczeństwo, 
                                        "Korzystna" = "Lepszy profil bezpieczeństwa" ,
                                        "Korzystna" = "Porównywalny profil bezpieczeństwa",
                                        "Niekorzystna" = "Gorszy profil bezpieczeńśtwa",
                                        "Niekorzystna" = "Nie można stwierdzić" 
                                        )  ,
       Kat1_Efektywność_kosztowa2 = fct_recode(Kat1_Efektywność_kosztowa, 
                                            "Korzystna"  = "Wykazano przewagę",
                                            "Niekorzystna"  = "Nie dowiedziono przewagi",
                                            "Niekorzystna"  = "Nieefektywny kosztowo",
                                            "Niekorzystna"  = "Niepewność",
                                            "Niekorzystna"  = "Nie można stwierdzić"
                                              )  ,
       Kat1_Wpływ_budżet2 = fct_recode(Kat1_Wpływ_budżet, 
                                      "Korzystna" = "Oszczędności",
                                      "Korzystna" = "Neutralny",
                                      "Niekorzystna" = "Dodatkowe obciążenie",
                                      "Niekorzystna" = "Niepewność",
                                      "Niekorzystna" = "Nie można stwierdzić"
                                      )   )
       

Analizy_korzyść2 <- Analizy_korzyść %>% 
  filter(Rekomendacja == "Pozytywna")  %>% 
  filter(liczba_agencji >= 6)  %>% 
  select(Kraj, Kat1_Korzyść_kliniczna2, Kat1_Bezpieczeństwo2, Kat1_Efektywność_kosztowa2,  Kat1_Wpływ_budżet2) %>% 
  pivot_longer(2:5) %>% 
  drop_na() %>% 
  mutate(
    name = str_sub(name, 6) %>% str_replace_all("_", " ") %>% str_replace_all("2", ""),
    value = fct_relevel(value, "Korzystna", "Niekorzystna")
         )  %>% 
  count(Kraj, name, value) 

Analizy_korzyść2

Analizy_korzyść3 <- Analizy_korzyść2 %>% 
  add_row(
    tibble(
      Kraj = c("Francja", "Francja", "Niemcy", "Niemcy"),
      name = c("Efektywność kosztowa", "Wpływ budżet", "Efektywność kosztowa", "Wpływ budżet"),
      value = rep("Korzystna", 4),
      n = rep(0, 4)
      
    ))

# Opcja 1 -----------------------------------------------------------------



Analizy_korzyść3 %>% 
  ggplot(aes(Kraj, n)) +
  geom_bar(aes(fill = name), alpha = ifelse(Analizy_korzyść2$value == "Korzystna", 1, 0.4),
             position = position_dodge(preserve = "single", width = 1.5), stat = "identity", width = 1.5) +
  coord_flip() +
  scale_fill_jco()
  
  
Analizy_korzyść3 %>% 
  filter(Kraj == "Australia")
  
Baza_analizy3 %>% 
  filter(Kraj == "Francja", Rekomendacja == "Pozytywna") %>% 
  count(Kat1_Bezpieczeństwo) #Zgadza się.

Baza_analizy1 %>% 
  filter(Kraj == "Australia", Rekomendacja == "Pozytywna") %>% 
  count(Kat1_Efektywność_kosztowa)

Analizy_korzyść3 %>% 
  filter(name == "Wpływ budżet")


# Opcja 2 -----------------------------------------------------------------



ggplot(data = Analizy_korzyść3, mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = position_stack(reverse = T), stat = "identity") +
  scale_alpha_manual("value", values = c(1, 0.3)) +
  facet_wrap(~ Kraj, strip.position = "bottom", scales = "free_x", nrow = 1) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        strip.background = element_blank(),
        
              legend.position = "bottom") + 
  guides(alpha = "none") +
  labs(fill = "", y = "") +
  scale_fill_uchicago() +
  ggtitle("Stosunek ocen korzystnych do niekorzystnych w ramach pozytywnych rekomendacji", "W podziale na kraj i kryterium")

ggsave("Wykres 10 - Stosunek korzystnych do niekorzystnych ocen w ramach pozytywnych rekomendacji - w podziale na kraj i kryterium.png",
       device = "png", dpi = 300, width = 20, height = 10, path = "./Wykresy")
  
  
  
  




ggplot(data = Analizy_korzyść3, mapping = aes(x = interaction(Kraj, name), y = n)) +
  geom_bar(aes(fill = name, alpha = factor(value)), 
           position = "stack", stat = "identity") +
  scale_alpha_manual("value", values = c(0.3, 1)) +
  facet_grid(~ Kraj,  scales = "free_x", space = "free") +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        strip.background = element_blank())
