#Sprawdzam, jak wygląda rozłożenie wartości w ramach lęku z największą liczbą rekomendacji

Baza_Opdivo <- Baza_razem2 %>%
  filter(!is.na(Rekomendacja), Nazwa_miedzynarodowa == "Opdivo") 

Baza_Opdivo  %>% 
  select(Nazwa_miedzynarodowa, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  print(n = 39)


Baza_Opdivo  %>% 
  count(Wskazanie_rejestracyjne, Agencja_kraj) %>% 
  arrange(desc(n))
  

#Piewotny skrypt mi zginął :(
#Zaktualizować, cos tutaj nie gra (nie zgadza się z wykresem), pewnie unique

Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(Nazwa_miedzynarodowa, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(Nazwa_miedzynarodowa, Wskazanie_rejestracyjne) %>% 
  arrange(desc(n)) 

Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(Nazwa_miedzynarodowa, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(Nazwa_miedzynarodowa, Wskazanie_rejestracyjne) %>% 
  count(n)

Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(Nazwa_substancji_czynnej, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(Nazwa_substancji_czynnej, Wskazanie_rejestracyjne) %>% 
  count(n) 




Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(ID, Wskazanie_rejestracyjne) %>% 
  count(n) 


#Wykres ostateczny


Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(ID, Wskazanie_rejestracyjne) %>% 
  count(n) %>% 
  ggplot(aes(n, nn)) +
  geom_bar(stat = "identity", fill = "wheat3", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous(breaks = seq(0,70,5)) +
  labs(x = "Liczba agencji, która brała udział w ewaluacji danego zgłoszenia", 
       title = "Liczba analizowanych rekomendacji w zależności od liczby agencji") +
  coord_cartesian(ylim = c(0,70))

ggsave("Liczba analizowanych rekomendacji w zależności od liczby agencji.png", device = "png", dpi = 300,path = "./Wykresy/")
       

  



Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(Nazwa_substancji_czynnej, Agencja_kraj, Wskazanie_rejestracyjne) %>% 
  unique() %>% 
  count(Nazwa_substancji_czynnej, Wskazanie_rejestracyjne) %>% 
  count(n) %>% 
  ggplot(aes(n, nn)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous(breaks = seq(0,70,5)) +
  labs(x = "Liczba agencji, która brała udział w ewaluacji danego zgłoszenia", 
       title = "Liczba analizowanych leków w zależności od liczby agencji") +
  coord_cartesian(ylim = c(0,70))

ggsave("Liczba analizowanych leków w zależności od liczby agencji.png", device = "png", dpi = 300,path = "./Wykresy/")


#Jedna taka alternatywna do liczby analizowanych rekomendacji

Baza_razem2 %>% 
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj)
  
Baza_razem2 %>%
  filter(!is.na(Rekomendacja)) %>% 
  select(ID, Agencja_kraj) %>% 
  unique() %>% 
  count(ID, Agencja_kraj) %>% 
  group_by(ID) %>% 
  summarise(liczba_agencji = sum(n)) %>% 
  count(liczba_agencji) %>% 
ggplot(aes(liczba_agencji, n)) +
  geom_bar(stat = "identity", fill = "wheat3", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous(breaks = seq(0,70,5)) +
  labs(x = "Liczba agencji, która brała udział w ewaluacji danego zgłoszenia", 
       title = "Liczba analizowanych rekomendacji w zależności od liczby agencji", y = "") +
  coord_cartesian(ylim = c(0,70))

ggsave("Liczba analizowanych rekomendacji w zależności od liczby agencji - final.png", device = "png", dpi = 300,path = "./Wykresy/")




