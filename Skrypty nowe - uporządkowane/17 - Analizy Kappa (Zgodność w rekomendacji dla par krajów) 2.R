library(rstatix)
library(tidyverse)
library(ggsci)
library(ggthemes)


# Zrobić aplikacje shiny
  # Tak, żeby pokazywała: czystą proporcję dla rekomendacji bądź wybranej kategorii dla dwóch krajów
  # Tak, żeby dodatkowo pokazywała proporcję dla rekomendacji w oparciu o wybraną kategorię
  # Może też dodatkowo raport KAPPA


# Loaduję wektory referencyjne
source("Wektory referencyjne/Relational database for recoding character values.R", 
       encoding = "UTF-8")

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
  output <- str_c(output, " \n(n = ", n, ")")
  
  
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
    korelacje[upper.tri(korelacje, diag = T)] <- "." 
    korelacje[upper.tri(korelacje, diag = F)] <- NA 
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

tibble(mtcars$am, mtcars$vs)





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
    Rekomendacja = factor(Rekomendacja, levels = c("Negatywna", "Pozytywna")),
    Kraj = plyr::mapvalues(Kraj, code_kraje$Agencies_old, code_kraje$Agencies_short)
  ) %>% 
  pivot_wider(names_from = Kraj, values_from = Rekomendacja) 



#### Wykonanie analizy ####

# Wektor określający kraje do analizy
kraje_analiza <- pivot_kappa %>% 
  select(-c(1:4)) %>% 
  names()



results_rec <- kappa_matrix(pivot_kappa, kraje_analiza, kraje_analiza, del_diag = T) 

# Dodaję wzmianke o incomplete information
IC_index <- lower.tri(results_rec[-1]) & is.na(results_rec[-1]) 
results_rec[-1][IC_index] <- "I.I."



# Sprawdzam wynik NA
irr::kappa2(select(pivot_kappa, NZ, `GB-WLS`))
irr::kappa2(select(pivot_kappa, UK, NO))

# Problem z incomplete information 
pivot_kappa %>% 
  select(UK, NO) %>% 
  drop_na()







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
    KK = Kat1_Korzyść_kliniczna, 
    Bez = Kat1_Bezpieczeństwo, 
    EK = Kat1_Efektywność_kosztowa, 
    WB = Kat1_Wpływ_budżet,
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


# Aktualizuję wartości do wystąpienia odniesienia lub braku
pivot_odniesienia <- pivot_kategorie %>% 
  unnest(c(main, pol)) %>% 
  mutate(
    across(c(main, pol), ~replace_na(., "No"))
  ) %>% 
  mutate(
    across(
      c(main, pol), # error dotyczy braku "Missing" w pol
      ~fct_collapse(.,
        miss = "No Recommendation",
        "No" = c("No", "Missing"),
        other_level = "Yes"
      )
    )
  ) %>% 
  mutate(
    across(c(main, pol), ~na_if(., "miss") %>% factor(levels = c("No", "Yes")))
  )
  
  
# Obliczam współczynniki kappa 
pivot_odniesienia2 <- pivot_odniesienia %>% 
  nest(main = main, pol = pol, .by = c(Kraj, set)) %>% 
  mutate(across(c(main, pol), ~map(., as_vector))) %>% 
  mutate(Kappa_kk = map2_chr(main, pol, possibly(kappa_fun, NA))) 


# Sprawdzam, z czego wynikają NA
odniesienia_na <- pivot_odniesienia2 %>% 
  filter(is.na(Kappa_kk)) %>% 
  unnest() %>% 
  count(Kraj, set, main, pol) 

# Wyodrębniam obserwacje dla których było incomplete information
relational_odniesienia1 <- odniesienia_na %>% 
  filter(if_all(c(main, pol), ~!is.na(.))) %>% 
  select(Kraj, set) %>% 
  unique() %>% 
  mutate(NA_category = "I.I.")

# Wyodrębniam obserwacje, dla których było lack of data
odniesienia_na %>% 
  select(Kraj, set) %>% 
  unique() # Mamy powtórzenie, zatem dla wszystkich jest incomplete information


# Uzupełniam dane o informacje o incomplete information
pivot_odniesienia3 <- pivot_odniesienia2 %>% 
  left_join(relational_odniesienia1) %>% 
  mutate(Kappa_kk = coalesce(Kappa_kk, NA_category)) %>% 
  select(-NA_category) 

  

# Tworzę ostateczną tabelkę 
results_odniesienia <- pivot_odniesienia3 %>% 
  select(-c(main, pol)) %>% 
  pivot_wider(names_from = set, values_from = Kappa_kk) %>% 
  rename_with(~str_c(., "_odniesienie"), .cols = -Kraj)



#### Analiza dla korzyści ####



# Wybieram tylko unikalne obserwacje do rekodowania, żeby nie wypluwał mi error
code_korzysc2 <- code_korzysc %>% 
  select(Korzysc2, Value) %>% 
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
                       to = code_korzysc2$Korzysc2) %>% 
        factor(levels = c("Niekorzystna", "Korzystna"))
      )) 

# Sprawdzam, czy poprawnie "No Recommendation" usunęło jako NA
pivot_kategorie %>% 
  unnest() %>% 
  filter(if_any(c(main, pol), ~str_detect(., "No Recommendation")))
df_korzysc


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
  count(Kraj, set, main, pol)
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
  select(Kraj, set) %>% 
  unique() %>% 
  mutate(NA_category = "I.I.")

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
  pivot_wider(names_from = c(set), values_from = "Kappa_kk") %>% 
  rename_with(~str_c(., "_korzysc"), .cols = -Kraj)
  




# Łącznie wyniki z analiza rekomendacje i analiza kategorie ---------------

results_kryteria <- results_odniesienia %>% 
  left_join(results_korzysc3) %>% 
  mutate(Kraj = plyr::mapvalues(Kraj, code_kraje$Agencies_old, code_kraje$Agencies_short)) 
  




# Zapisuję wszystkie wyniki -----------------------------------------------

library(flextable)
library(officer)

code_kryteria

# Parametry 
doc_par <- prop_section(page_size = page_size(orient = "landscape"))

results_rec %>% 
  select(-ncol(results_rec)) %>% 
  flextable() %>% 
  # delete_part() %>% 
  # add_header_row(values = c("", 1:(nrow(results_rec) - 1)))
  Cabflex2::flex_general() %>% 
  set_caption("Cohen's Kappas for Recommendation") %>% 
  fontsize(size = 9, part = "all") %>% 
  Cabflex2::fit_flex(pgwidth = 10) %>% 
  save_as_docx(path = "Frontiers/Tabele Cohen's Kappa/Cohen's Kappas for Recommendation.docx",
               pr_section = doc_par)

results_kryteria

# Wersja 1 - najpierw odniesienia, potem korzyśc
results_kryteria %>% 
  flextable() %>% 
  delete_part() %>% 
  add_header_row(values = c("Country", code_kryteria$en, code_kryteria$en)) %>% 
  add_header_row(values = c("", "Reference", rep("", 3), "Value", rep("", 3))) %>% 
  Cabflex2::flex_general(head.rows = 2) %>% 
  border(i = 1, j = -1, border.bottom = fp_border(), part = "header") %>% 
  merge_at(i = 1, j = 2:5, part = "header") %>% 
  merge_at(i = 1, j = 6:9, part = "header") %>% 
  set_caption("Cohen's Kappas for Criteria - version 1") %>% 
  fontsize(size = 9, part = "all") %>% 
  Cabflex2::fit_flex(pgwidth = 10) %>% 
  save_as_docx(path = "Frontiers/Tabele Cohen's Kappa/Cohen's Kappas for Criteria - version 1.docx",
               pr_section = doc_par)


# Wersja 2 - najpierw odniesienia, potem korzyśc
kryteria_char <-map(code_kryteria$en, function(x) append(x, "")) %>% 
  unlist()

results_kryteria %>% 
  select(Kraj, starts_with("KK"), starts_with("Bez"), starts_with("EK"), starts_with("WB")) %>% 
  flextable() %>% 
  delete_part() %>% 
  add_header_row(values = c("Country", rep(c("Reference", "Value"), 4))) %>% 
  add_header_row(values = c("", kryteria_char)) %>% 
  Cabflex2::flex_general(2) %>% 
  border(i = 1, j = -1, border.bottom = fp_border(), part = "header") %>% 
  {
    ft <- .
    for (i in seq(2, 9, 2)) {
      ft <- merge_at(ft, i = 1, j = i:(i + 1), part = "header")
    }
    ft
  } %>% 
  set_caption("Cohen's Kappas for Criteria - version 2") %>% 
  fontsize(size = 9, part = "all") %>% 
  Cabflex2::fit_flex(pgwidth = 10) %>% 
  save_as_docx(path = "Frontiers/Tabele Cohen's Kappa/Cohen's Kappas for Criteria - version 2.docx",
               pr_section = doc_par)



                 
                 