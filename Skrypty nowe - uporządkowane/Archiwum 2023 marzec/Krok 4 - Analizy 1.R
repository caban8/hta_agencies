#Wstępne analizy


library(Cabix)
library(ggsci)
library(viridis)
library(ggthemes)
library(RColorBrewer)
biblioteki()

##Ustalenie kolorów do fill

source("./Skrypty nowe - uporządkowane/Krok 1 - skompletowanie danych pod analizy 2.R", encoding = "UTF-8")
source("./Skrypty nowe - uporządkowane/Krok 2 - Rekodowanie.R", encoding = "UTF-8")

mycolors <- brewer.pal(9, "RdBu")
mycolors5 <- mycolors[c(1,2,7,8,9)]
mycolors4 <- mycolors[c(1,2,8,9)]
mycolors3 <- mycolors[c(1,8,9)]


#Najpierw przypisuję braki danych (docelowo, powinienem to zrobić w skrypcie wcześniej)
#Do usunięcia


Baza_analizy1 <- Baza_razem4 %>% 
  select(Kategoria_lek, Obszar_terapeutyczny,  Year, Kraj, starts_with("Kat1"), liczba_agencji,
         Rekomendacja, -c(Kat1_Inne, Kat1_Komentarz)) %>% 
  mutate(across(everything(), ~na_if(., "Missing")))
 

Baza_analizy1 %>% 
  write_csv("Baza danych - 21 lutego 2022.csv")

#tabele krzyżowe
with(Baza_analizy1, chi.cross(Kat1_Populacja, Rekomendacja))


#Kraj a pozytywne rekomendacje

with(Baza_analizy1, chi.cross(Kraj, Rekomendacja))
with(Baza_analizy1, table(Kraj, Rekomendacja))

with(Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")), chi.cross(Kraj, Rekomendacja))


Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Kraj, Rekomendacja) %>% filter(!is.na(Rekomendacja)) %>% 
  group_by(Kraj) %>% mutate(percent = n / sum(n) * 100) %>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Kraj %>% fct_reorder(percent), percent)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black", alpha = 0.4) +
  coord_flip() + labs(y = "Procent pozytywnych rekomendacji", x = "") + theme_calc() +
  scale_y_continuous(breaks =  seq(0,100,10), labels = str_c(seq(0,100,10), "%"))


Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Year, Kraj, Rekomendacja) %>%  filter(!is.na(Rekomendacja)) %>% 
group_by(Year, Kraj) %>% mutate(percent = n / sum(n) * 100)%>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Year, n, fill = Kraj %>% fct_reorder(n))) +
  geom_area(color = "white", alpha = 0.7) + scale_fill_viridis(discrete = T) + theme_calc() +
  labs(fill = "", x = "", y = "Liczba pozytywnych rekomendacji") #Aneta nie chce takich
  
Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Year, Kraj, Rekomendacja) %>%  filter(!is.na(Rekomendacja)) %>% 
group_by(Year, Kraj) %>% mutate(percent = n / sum(n) * 100)%>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev, n, fill = Kraj %>% fct_reorder(n))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge()) + scale_fill_viridis(discrete = T) + theme_calc() +
  labs(fill = "", x = "", y = "Liczba pozytywnych rekomendacji") + coord_flip()
  

Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Year, Kraj, Rekomendacja) %>%  filter(!is.na(Rekomendacja)) %>% 
  group_by(Year, Kraj) %>% mutate(percent = n / sum(n) * 100)%>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), percent, fill = Kraj %>% fct_reorder(n))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge()) + scale_fill_viridis(discrete = T) + theme_calc() +
  labs(fill = "", x = "", y = "Procent pozytywnych rekomendacji") + coord_flip() #Problem z procentami jest taki, że Nowa Zelandia wydawała mało rekomdencaji, a w latach 2017-2018, tylko jedną


Baza_analizy1 %>% filter(!Kraj %in% c("IQWiG_Niemcy", "SBU Szwecja")) %>% count(Year, Kraj, Rekomendacja) %>%  filter(!is.na(Rekomendacja)) %>% 
  group_by(Year, Kraj) %>% mutate(percent = n / sum(n) * 100)%>% filter(Rekomendacja == "Pozytywna") %>% 
  ggplot(aes(Kraj, percent, fill = Year %>% factor() %>% fct_rev() )) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge()) + scale_fill_viridis(discrete = T) + theme_economist() +
  labs(fill = "", x = "", y = "Procent pozytywnych rekomendacji") + coord_flip()





#2)     Analizujemy ile leków zostało w sumie ocenionych w latach 2014-2018
#a.      Jak to się zmienia na przestrzeni lat
#b.      Wymienić liczbę leków w podziale na liczbę wskazań 


Baza_razem4 %>% count(Nazwa_substancji_czynnej) %>% arrange(desc(n))
Baza_razem4 %>% count(Nazwa_miedzynarodowa) %>% arrange(desc(n))
Baza_razem4 %>% filter(Nazwa_miedzynarodowa == "Aerivio Spiromax") %>% count(Year)

Baza_razem4 %>% select(Nazwa_substancji_czynnej, Year) %>% group_by(Year) %>% summarise(liczba = unique(Nazwa_substancji_czynnej)) %>% ungroup() %>% 
  count(Year) %>% ggplot(aes(desc(Year), n)) +
  geom_bar(stat = "identity", fill = "turquoise", alpha = 0.6, color = "black", width = 0.6) +
  coord_flip() + labs(y = "Liczba rozważanych leków", x = "") + theme_calc()

Baza_razem4 %>% select(Nazwa_miedzynarodowa, Year) %>% group_by(Year) %>% summarise(liczba = unique(Nazwa_miedzynarodowa)) %>% ungroup() %>% 
  count(Year) %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), n)) +
  geom_bar(stat = "identity",  color = "black", fill = mycolors[3], alpha = 0.75, width = 0.5) +
  labs(title = "Liczba ocenionych leków w latach 2014 - 2019+", fill = "", x = "", y = "") +
  theme_economist_white() + 
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        plot.background = element_rect(fill = "slategray1")) +
  coord_flip() 



ggsave("Analiza 2a - Liczba ocenionych leków w latach 2014 - 2019.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 
  

#3. Analizujemy ile wskazań zostało w sumie ocenionych w latach 2014-2019 (liczba wszystkich wierszy w bazie, 100%) 

n_leki <-  Baza_razem4 %>% count(Year) %>% 
  ggplot(aes(Year %>% factor() %>% fct_rev(), n)) +
  geom_bar(stat = "identity",  color = "black", fill = mycolors[3], alpha = 0.75, width = 0.5) +
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019", fill = "", x = "", y = "") +
  theme_economist_white() + 
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        plot.background = element_rect(fill = "slategray1")) +
  coord_flip()


   


Baza_razem4 %>% count(Kraj) %>% 
  summarise(sum(n))

Baza_razem4 %>% count(Agencja_kraj) 




# Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kra --------



Baza_razem4 %>% count(Year, Kraj) %>% ggplot(aes(factor(Year), n, fill = Kraj %>% fct_reorder(desc(n)))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge(preserve = "single")) +
  scale_fill_simpsons() + 
  theme_fivethirtyeight() + 
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 2))


ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj - wersja 1 slupkowy.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 
  
n_rekomendacje <- Baza_razem4 %>% 
  filter(!is.na(Rekomendacja))  %>% 
  count(Year, Kraj) %>% ggplot(aes(factor(Year), n, fill = Kraj %>% fct_reorder(desc(n)))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge(preserve = "single")) +
  scale_fill_simpsons() + 
  theme_fivethirtyeight() + 
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj \ntylko wydane rekomendacje", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 2))



# -------------------------------------------------------------------------



Baza_razem4 %>% count(Year, Kraj) %>% ggplot(aes(Year, n, fill = Kraj %>% fct_reorder(n))) +
  geom_area(color = "white", alpha = 0.7) +
  scale_fill_simpsons() + 
  labs(fill = "", y = "Liczba wskazań", x = "") +
  theme_fivethirtyeight() + 
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 2))


ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj - wersja 2 area.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 


n_wskazania_obszar <- Baza_razem4 %>% mutate(Obszar_terapeutyczny = Obszar_terapeutyczny %>% str_to_sentence()) %>% 
  count(Year, Obszar_terapeutyczny) %>% 
  drop_na() %>% 
  ggplot(aes(Year, n, fill = Obszar_terapeutyczny %>% fct_reorder(desc(n)))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge(preserve = "single")) +
  scale_fill_viridis(discrete = T) + 
  theme_fivethirtyeight() + 
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na obszar terapeutyczny", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 11)) +
  guides(fill = guide_legend(nrow = 5))



n_wskazania <- Baza_razem4 %>% count(Year, Kategoria_lek) %>% 
  filter(Kategoria_lek != "Unknown") %>% 
  ggplot(aes(Year, n, fill = Kategoria_lek %>% fct_reorder(desc(n)))) +
  geom_bar(stat = "identity", color = "white", alpha = 0.7, position = position_dodge(preserve = "single")) +
  scale_fill_simpsons()  + 
  theme_fivethirtyeight() + 
  labs(title = "Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kategorię leku", fill = "") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1))


#4)     Z całkowitej puli pokazujemy ilość ocenionych i ilość nieocenionych (dla których nie opublikowano ani jednej rekomendacji)  
#a.      Z puli ocenionych pokazujemy ilość (N, %) w podziale na obszary terapeutyczne 
#b.      Z puli ocenionych pokazujemy ilość (N, %) w podziale na orphan, generic, biosimilar 


Baza_razem4 %>% 
  mutate(Rekomendacja2 = fct_recode(Rekomendacja, 
                                    'Oceniona' = 'Negatywna',
                                    'Oceniona' = 'Pozytywna' ),
         Rekomendacja2 = fct_explicit_na(Rekomendacja2, "Nieoceniona")) %>% 
  count(Rekomendacja2, Nazwa_substancji_czynnej) %>% 
  group_by(Nazwa_substancji_czynnej) %>% 
  mutate(percent =  n / sum(n)) %>% 
  arrange(desc(percent)) %>% 
  filter(Rekomendacja2 == "Nieoceniona") %>% 
  mutate(Stosunek = if_else(percent == 1, "Brak uzyskanych rekomendacji", "Przynajmniej jedna rekomendacja")) %>% 
  ungroup() %>% 
  count(Stosunek) %>% 
  ggplot(aes(Stosunek, n)) +
  geom_bar(stat = "identity",  color = "black", fill = mycolors[3], alpha = 0.75, width = 0.5) +
  labs(title = "Liczba leków w podziale ze względu na przynajmniej jedną uzyskaną rekomendację", fill = "", x = "", y = "") +
  theme_economist_white() + 
  geom_label(aes(label = n)) +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        plot.background = element_rect(fill = "slategray1")) +
  coord_flip()



ggsave("Analiza 4 - Liczba leków z przynajmniej jedną uzyskaną rekomendacją.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300)   


#5. Analizujemy czas od momentu rejestracji do wydania rekomendacji w podziale na:
#a.      obszary terapeutyczne 
#b.      na orphan, generic, biosimilar 
#Uwaga - wyłączam z analizy te leki, które mają ujemną wartość dni do rekomendacji


Baza_razem4 %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  mutate(Obszar_terapeutyczny = str_to_sentence(Obszar_terapeutyczny)) %>% 
  filter(Obszar_terapeutyczny != "Leczenie bólu") %>% 
  group_by(Obszar_terapeutyczny) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>%
  drop_na() %>% 
   ggplot(aes(Obszar_terapeutyczny %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "black", fill = mycolors[3], alpha = 0.75) + 
  labs(y = "Średnia liczba dni od rejestracji do rekomendacji") +
  scale_y_continuous(breaks = seq(0,1000,100)) +
  coord_flip() +
  theme_economist_white() +
  geom_label(aes(label = Mean %>% round())) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        plot.background = element_rect(fill = "slategray1")) 



ggsave("Analiza 5a Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 


Baza_razem4 %>% mutate(Is_Orphan = plyr::mapvalues(Is_Orphan, 1, 20),
                       Biosimilar = plyr::mapvalues(Biosimilar, 1, 5),
                       Kategoria_lek = Is_Orphan + Is_Generic + Biosimilar,
                       Kategoria_lek = factor(Kategoria_lek, labels = c("Inne / Oryginalne", "Generic", "Biosimilar", "Orphan", "Unknown"))) %>% 
  filter(Kategoria_lek != "Unknown") %>% 
  filter(dni_do_rekomendacji >= 0) %>% 
  group_by(Kategoria_lek) %>% summarise(Mean = mean(dni_do_rekomendacji, na.rm = T)) %>% 
  ggplot(aes(Kategoria_lek %>% fct_reorder(Mean), Mean )) +
  geom_bar(stat = "identity", color = "black", fill = mycolors[3], alpha = 0.75, width = 0.5) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  labs(y = "Średnia liczba dni od rejestracji do rekomendacji") +
  coord_flip() +
  theme_economist_white() +
  geom_label(aes(label = Mean %>% round()), nudge_y = -10) +
  labs(x = "", y = "",title = "Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na kategorię leku") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
              plot.background = element_rect(fill = "slategray1")) 

ggsave("Analiza 5b Średnia liczba dni od daty rejestracji do daty rekomendacji w podziale na kategorię leku.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 




Baza_razem4 %>% select(Is_Generic, Is_Orphan) %>% table()
Baza_razem4 %>% select(Biosimilar, Is_Orphan) %>% table()
Baza_razem4 %>% select(Biosimilar, Is_Generic) %>% table()

Baza_razem4 %>% filter(Is_Generic == 1, Is_Orphan == 1) #orphan dajemy 0


# 6. NAJWAŻNIEJSZE - Analizujemy jak każde kryterium rozpatrywane osobno przekładało się na typ wydanej rekomendacji refundacyjnej
# Porównanie wszystkich




Chi_wszystkie <-  Baza_analizy1 %>%
  pivot_longer(cols = starts_with("Kat1"), values_to = "Odpowiedz", names_to = "Kategoria") %>% 
  mutate(Kategoria = str_replace(Kategoria,"Kat1[_]", "") %>% 
           str_replace("[_]", " ")) %>% 
  group_by(Kategoria) %>% 
  summarise(chisq_test(Odpowiedz, Rekomendacja)) %>% 
  ungroup() %>% 
  mutate( p2 = if_else(p < 0.001, "p < 0.001", str_c("p = ", round(p, 3)) ),
          significant = if_else(p < 0.05, T,F)) 



Baza_analizy1 %>% 
   pivot_longer(cols = starts_with("Kat1"), values_to = "Odpowiedz", names_to = "Kategoria") %>% 
  mutate(Kategoria = str_replace(Kategoria,"Kat1[_]", "") %>% 
           str_replace("[_]", " ")) %>% 
  count(Kategoria, Rekomendacja, Odpowiedz) %>% 
  filter(!is.na(Odpowiedz), !is.na(Rekomendacja)) %>%
  group_by(Kategoria, Odpowiedz) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  filter(Rekomendacja == "Pozytywna") %>% 
  ungroup() %>% 
  mutate(lp = c(rep(4, 4), rep(5, 5), rep(2, 2), rep(3,3), rep(1,2), rep(6,2), rep(7,5)),
         Odpowiedz = tidytext::reorder_within(Odpowiedz, by = percent, within = Kategoria)) %>% 
  left_join(Chi_wszystkie, by = "Kategoria") %>% 
  arrange(Kategoria, desc(percent)) %>% 
  mutate(p2 = c(p2[1], rep(NA, 3), p2[5], rep(NA, 4), p2[10], NA, p2[12], rep(NA, 2), p2[15], NA, p2[17], NA, p2[19], rep(NA, 4))) %>% 
  arrange(lp) %>% 
  ggplot(aes(Odpowiedz, percent, fill = Kategoria %>% fct_reorder(lp)  )) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.95, color = "black", alpha = 0.5) +
  coord_flip() +
  scale_fill_uchicago() + 
  labs(x = "", y = "", fill = "") + 
  scale_y_continuous(breaks = seq(0, 100, 20), labels = str_c(seq(0, 100, 20), "%")) +
  tidytext::scale_x_reordered() +
  facet_grid(Kategoria %>% fct_reorder(lp) ~ ., scales = "free_y", space = "free") +
  theme_fivethirtyeight() +
  geom_text(aes(label = p2), hjust = 1, color = "black") +
  labs(title = "Procent pozytywnych rekomendacji oddzielnie dla każdego kryterium") +
  theme(legend.position = "bottom", plot.title = element_text(size = 15, hjust = 0.5, face = "plain", margin=margin(0,0,30,0)), legend.text = element_text(size = 14),
        strip.text = element_blank(), panel.spacing.y = unit(10, "pt")) 


ggsave("Analiza 6a - procent pozytywnych rekomendacji ogolnie.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 







# Zapisywanie wykresów ----------------------------------------------------



ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300)


ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kraj - tylko wydane rekomendajce.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 



ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na obszar terapeutyczny.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 



ggsave("Analiza 3 - Liczba ocenionych wskazań w latach 2014 - 2019 w podziale na kategorię leku.png", device = "png", path = "./Wykresy",
       width = 12, height = 8, dpi = 300) 
