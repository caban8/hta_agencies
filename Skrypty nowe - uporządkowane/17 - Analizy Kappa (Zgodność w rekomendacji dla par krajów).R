library(rstatix)
library(tidyverse)
library(ggsci)
library(ggthemes)


# Zrobić aplikacje shiny
  # Tak, żeby pokazywała: czystą proporcję dla rekomendacji bądź wybranej kategorii dla dwóch krajów
  # Tak, żeby dodatkowo pokazywała proporcję dla rekomendacji w oparciu o wybraną kategorię
  # Może też dodatkowo raport KAPPA


# Analizy Kappa -----------------------------------------------------------




Baza_analizy1 <- read_csv("Baza razem - 21 lutego 2022.csv")



# Funkcje -----------------------------------------------------------------



#### Przygotowanie funkcji ####



# Tworzę funkcję do zliczania Kappa
kappa_fun <- function(x, y) {
  
  # Wykonaj pełne obliczenie
  result <- irr::kappa2(data.frame(x, y)) 
  
  # Wyciągnij wartości kappa i p.value
  kappa <- round(result$value, 2) %>% as.character()
  p <- result$p.value
  n <- result$subjects
  
  # Dodaj gwiazdkę
  output <- case_when(
    p < 0.05 & p >= 0.01 ~ paste0(kappa, "*"),
    p < 0.01 & p >= 0.001 ~ paste0(kappa, "**"),
    p < 0.001 ~ paste0(kappa, "***"),
    p >= 0.05 ~ kappa
  )
  
  
  # Doaj liczbę obserwacji
  output <- str_c(output, " (n = ", n, ")")
  
  
  return(output)
  
}

# Tworzę funkcję do stworzenia matrycy korelacji
kappa_matrix <- function (df, X, Y, del_diag = F) 
{
  # Sprowadzam obie listy zmiennych do dataframes
  df1 <- dplyr::select(df, {{X}})
  df2 <- dplyr::select(df, {{Y}})
  
  # Tworzę pustą dataframe do wypełnienia przez pętlę
  korelacje <- data.frame(matrix(nrow = length(df1), ncol = length(df2)))
  
  # Uruchamiam pętlę do zliczenia wszystkich współczynników kappa 
  for (i in 1:length(df1)) {
    for (j in 1:length(df2)) {
      korelacje[i, j] <- kappa_fun(df1[[i]], df2[[j]])
    }
  }
  
  
  # Usuwam powtórzone korelacje, jeśli potrzeba
  if (del_diag) {
    korelacje[upper.tri(korelacje)] <- NA 
  }
  
  # Nadaję nazwy zmiennym
  korelacje <- cbind(names(df1), korelacje)
  names(korelacje) <- c(" ", names(df2))
  
  korelacje <- as_tibble(korelacje)
  
  
  return(korelacje)
}


#### Test funkcji ####

kappa_result <- irr::kappa2(select(mtcars, am, vs))
kappa_result$subjects
kappa_result$p.value

kappa_fun(mtcars$am, mtcars$vs)





# Analiza - rekomendacje --------------------------------------------------



#### Przygotowanie danych ####



# Wybieram zmienne do analizy Kappa
df_kappa <- Baza_analizy1 %>% 
  select(ID, Rekomendacja, Kraj, Nazwa_miedzynarodowa) %>% 
  filter(Kraj != "Szwecja")

# Tworze bazę relacyjną z liczbą wydanych rekomendacji dla każdego wskazania ze względu na kraj
repeated_filter <- df_kappa %>% 
  count(ID, Kraj) %>% 
  mutate(unique = if_else(n == 1, TRUE, FALSE))

# Wykluczam obserwacje, które w ramach danego wskazania były ocenione więcej niż raz przez danych kraj
df_kappa2 <- df_kappa %>% 
  left_join(repeated_filter) %>% 
  filter(unique)


# Porządkuję bazę danych pod kątem analiz kappa
pivot_kappa <- df_kappa2 %>% 
  mutate(
    Rekomendacja = factor(Rekomendacja, levels = c("Negatywna", "Pozytywna"))
  ) %>% 
  pivot_wider(names_from = Kraj, values_from = Rekomendacja) 



#### Wykonanie analizy ####

# Wektor określający kraje do analizy
kraje_analiza <- pivot_kappa %>% 
  select(-c(1:4)) %>% 
  names()



results_rec <- kappa_matrix(pivot_kappa, kraje_analiza, kraje_analiza, del_diag = T) 
results_rec


# Sprawdzam wynik NA
irr::kappa2(select(pivot_kappa, `Nowa Zelandia`, Walia))
irr::kappa2(select(pivot_kappa, `Wielka Brytania`, Norwegia))


# Problem z incomplete information 
pivot_kappa %>% 
  select(`Wielka Brytania`, Norwegia) %>% 
  drop_na()

pivot_kappa %>% 
  select(Holandia, Norwegia) %>% 
  drop_na() # Holandia w tym zestawieniu ma tylko pozytywne oceny






# Analiza - kategorie -----------------------------------------------------



  # W ramach każdej analizy chcę wykluczyć te obserwacje, które nie wydały rekomendacji
  # Mogę to teoretycznie zrobić krok przed complete cases, wybierając !(is.na) dla Rekomendacji
    # Druga opcja jednak odpada, gdyż przecież wcześniej usunąłem w ramach niektórych rekomendacji powtórzenia
  # Missing oznacza brak kompletny w kategorii, tj. wtedy, gdy nie było też wydanej rekomendacji



#### Przygotowuję dane ogólne ####
  # Zarówno do analizy na odniesieniach, jak i korzyści

# Wybieram zmienne do analizy
df_kategorie <- Baza_analizy1 %>% 
  select(ID, Rekomendacja, Kraj, 
         Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo,
         Kat1_Efektywność_kosztowa, Kat1_Wpływ_budżet
  ) %>% 
  left_join(repeated_filter) %>% 
  filter(unique)



# Wypełniam wartości krajów tak, żeby w ramach każdego ID mieć tyle samo krajów
df_kategorie2 <- df_kategorie %>% 
  complete(Kraj, ID, fill = list(Rekomendacja = NA)) %>% 
  mutate(
    across(
      starts_with("Kat"),
      ~if_else(is.na(Rekomendacja), "No Recommendation", .) # Wprowadzam rozróżnienie między NA w kategorii a obserwacji z brakami rekomendacji
    )
  ) 
  # mutate(across(where(is.character), ~na_if(., "Missing"))) # Poprawiam missing na NA


# Sprawdzam, czy missing dotyczy tylko obserwacji z brakami rekomendacji
df_kategorie2 %>% 
  filter(if_any(starts_with("Kat"), ~str_detect(., "Missing"))) %>% 
  count(Rekomendacja)

df_kategorie2 %>% 
  filter(if_any(starts_with("Kat"), ~str_detect(., "No Recommendation"))) %>% 
  count(Rekomendacja)

df_kategorie2 %>% 
  filter(if_any(starts_with("Kat"), ~str_detect(., "Missing"))) %>% 
  filter(!is.na(Rekomendacja))

# Systematyzuję zmienne pod kątem analizy
df_kategorie3 <- df_kategorie2 %>% 
  nest(
    KK = Kat1_Korzyść_kliniczna, Bez = Kat1_Bezpieczeństwo, 
    EK = Kat1_Efektywność_kosztowa, WB = Kat1_Wpływ_budżet,
    .by = Kraj
  ) %>% 
  mutate(across(-Kraj, ~map(., as_vector)))

# Izoluję Polskę jako stałą wartość odniesienia
Polska_kategorie <- df_kategorie3 %>% 
  filter(Kraj == "Polska") %>% 
  select(-Kraj) %>% 
  as.list()

# Łączę bazy danych 
df_kategorie4 <- df_kategorie3 %>% 
  filter(Kraj != "Polska") %>% 
  mutate(
    KK_pol = Polska_kategorie$KK,
    Bez_pol = Polska_kategorie$Bez,
    EK_pol = Polska_kategorie$EK,
    WB_pol = Polska_kategorie$WB
  ) %>% 
  rename_with(~str_c(., "_main"), .cols = c(KK, Bez, EK, WB)) 


# Sprowadzam do postaci podłużnej, żeby łatwiej przeprowadzić analizę iteracyjną
pivot_kategorie <- df_kategorie4 %>% 
  pivot_longer(-Kraj, names_sep = "_", names_to = c("set", ".value"))




pivot_kategorie %>% 
  unnest()

pivot_kategorie %>% 
  unnest() %>% 
  nest(main = main, pol = pol, .by = c(Kraj, set))



#### Analiza dla odniesień ####

  # UWAGA - wpierw upewnić się, że obserwacje bez wydanej rekomendacji zostały wykluczone
  # Żeby przypadkiem nie potraktował ich jako brak odniesień

pivot_kategorie %>% 
  unnest() %>% 
  filter(if_any(c(main, pol), ~str_detect(., "No Recommendation")))


pivot_kategorie %>% 
  mutate(Kappa_kk = map2_chr(main, pol, possibly(kappa_fun, NA)))


#### Analiza dla korzyści ####

load("Wektory referencyjne/codebooks.RData")

# Wybieram tylko unikalne obserwacje do rekodowania, żeby nie wypluwał mi error
code_korzysc2 <- code_korzysc %>% 
  select(Korzysc1, Value) %>% 
  unique()

# Ustalam wartości do analizy: korzystne vs niekorzystne
  # Uwzględniam odpowiedzi "nie można stwierdzić" jako niekorzystne
df_korzysc <- pivot_kategorie %>% 
  unnest() %>% 
  mutate(
    across(
      c(main, pol), 
      ~plyr::mapvalues(., 
                       from = code_korzysc2$Value,
                       to = code_korzysc2$Korzysc1) %>% 
        factor(levels = c("Niekorzystna", "Korzystna"))
      )) 


# Sprowadzam wyniki z powrotem do postaci nested
results_korzysc <- df_korzysc %>% 
  nest(main = main, pol = pol, .by = c(Kraj, set)) %>% 
  mutate(across(c(main, pol), ~map(., as_vector))) %>%  
  mutate(Kappa_kk = map2_chr(main, pol, possibly(kappa_fun, NA))) 

# Sprawdzam, dlaczego wyszły wyniki NA 
results_korzysc %>% 
  filter(is.na(Kappa_kk)) %>% 
  unnest() %>% 
  filter(!is.na(main)) %>% 
  count(Kraj, set, main, pol) %>% 
  Cabix2::head_more()
  # Krótko mówiąc, wyniki nie wychodziły albo ze względu na brak jakichkolwiek danych w jednej grupie
  # albo incomplete information


# Przypisuję odpowiednie oznaczenie dla kategorii NA
relational_na1 <- results_korzysc %>% # Lack of data for one of the countries
  filter(is.na(Kappa_kk)) %>% 
  unnest() %>% 
  count(Kraj, set, main) %>% 
  group_by(Kraj, set) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  mutate(NA_category = str_c("No data ", Kraj)) %>% 
  select(Kraj, set, NA_category)
  
relational_na2 <- results_korzysc %>% # Incomplete information
  filter(is.na(Kappa_kk)) %>% 
  unnest() %>% 
  filter(!is.na(main)) %>% 
  count(Kraj, set, main, pol) %>% 
  Cabix2::head_more() %>% 
  select(Kraj, set) %>% 
  unique() %>% 
  mutate(NA_category = "Incomplete information")

relational_na <- relational_na1 %>% 
  add_row(relational_na2)

# Dodaję informację o brakach
results_korzysc2 <- results_korzysc %>% 
  left_join(relational_na) %>% 
  mutate(Kappa_kk = coalesce(Kappa_kk, NA_category)) %>% 
  select(-NA_category)


# Sprowadzam wyniki do czytelnej tabelki
results_korzysc3 <- results_korzysc2 %>% 
  select(-c(main, pol)) %>% 
  pivot_wider(names_from = c(set), values_from = "Kappa_kk")
  





# -------------------------------------------------------------------------



df_kategorie %>% 
  select(-c(n, Rekomendacja, unique)) %>% 
  pivot_wider(names_from = Kraj, 
              values_from = c(Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo,
                              Kat1_Efektywność_kosztowa, Kat1_Wpływ_budżet)) 


Baza_analizy1 %>% 
  filter(ID == 28) %>% 
  select(Rekomendacja, Kraj, starts_with("Kat1")) 
  



df_kategorie %>% 
  select(-c(n, Rekomendacja, unique)) %>% 
  nest(Kat1 = Kat1_Korzyść_kliniczna, Kat2 = Kat1_Bezpieczeństwo, .by = Kraj)
  
