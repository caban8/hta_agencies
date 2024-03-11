
# Rzeczy do zrobienia !!! -------------------------------------------------
#Naprawić wszystkie daty!!!




# Loadowanie bibliotek ----------------------------------------------------
library(readxl)
library(tidyverse)
library(lubridate)

# Zaimportowanie plików excel ---------------------------------------------


sheet <- c(str_c("201", 4:8), "2019")
zmienne_nazwy <- c(
  "Nazwa_miedzynarodowa",	"Nazwa_substancji_czynnej",	"Wskazanie_rejestracyjne",	"ATC_code",	"MAH",	"Data_rejestracji",	"Is_Orphan",	"Is_Generic",	
  "Biosimilar",	"Obszar_terapeutyczny",	"Agencja_kraj",	"Link_do_rekomendacji", 	"Data_rekomendacji", "Rekomendacja", 	"Rekomendowane_wskazanie",				
  "Unknown1", "Unknown2", "Unknown3",
  "Populacja",	"Komparator",	"Korzyść_kliniczna",	"Bezpieczeństwo",	"RSS",	"Efektywność_kosztowa",	"Wpływ_budżet",	"Inne",	"Komentarz",	"Populacja_II",
  "Komparator_II",	"Korzyść_kliniczna_II",	"Bezpieczeństwo_II",	"RSS_II",	"Efektywność_kosztowa_II",	"Wpływ_budżet_II",	"Inne_II",	"Komentarz_II"
  
  
) #Uwaga - zmienne Unknown dodaje specjalnie, gdyż z jakiegoś powodu ładuje trzy puste kolumny, których normalnie w arkuszu nie widać




Bazy_lista <- map(sheet, read_excel,  path = "Final updated 2022.xlsx", na = "9",  skip = 5, col_names = F, guess_max = 1000000)
Bazy_lista



names(Bazy_lista) <- str_c("BAZA_201", 4:9)


# Dodaję empty columns, żeby zgadzałą się liczba kolumn w każdej b --------

na_columns <- rep(NA, 36)

funkcja_NAcol <- function(df) {
  
  for (i in length(df):36) {
    df[i] <- na_columns[i]
    
  }
  df
}


Bazy_lista2 <- map(Bazy_lista, funkcja_NAcol)
Bazy_lista
Bazy_lista2



# Nadaję nazwy zmiennym w ramach wszystkich baz ---------------------------



for (i in 1:length(Bazy_lista2)) {
  
  names(Bazy_lista2[[i]]) <- zmienne_nazwy
  
}




Bazy_lista2$BAZA_2015$Data_rejestracji %>% class()

# Sprawdzam, czy poprawnie pogrupowałem daty ------------------------------

extract_date <- function(x) {
  str_extract(x$Data_rejestracji, "([0-9]*)$") %>% table()
}

map_df(Bazy_lista2, extract_date)
str_extract(Bazy_lista2$BAZA_2018$Data_rejestracji, "[0-9]*") %>% table()
extract_date(Bazy_lista2$BAZA_2014) %>% table()
extract_date(Bazy_lista2$BAZA_2015) %>% table()




# Porządkuję w bazach zmienne o datach ------------------------------------


Bazy_test_daty <- Bazy_lista2

funkcja_rok <- function(x, rok) {
  x %>% mutate(Year = rep(rok, nrow(x)))
}

funkcja_daty <- function(x, rok) {
  x %>% mutate(Data_rejestracji = dmy(Data_rejestracji))
}

lata_baza <- c(2014:2019)

Bazy_lista2 <-  map2(Bazy_lista2, lata_baza, funkcja_rok)



Bazy_lista2[1:4] <- map(Bazy_lista2[1:4], funkcja_daty)

Bazy_lista$BAZA_2017
Bazy_lista2$BAZA_2017
Bazy_lista2$BAZA_2017 %>% 
  filter(is.na(Data_rejestracji))

Bazy_lista2$BAZA_2015 %>% 
  filter(is.na(Data_rejestracji))


Bazy_test_daty$BAZA_2017 %>% 
  filter(Nazwa_substancji_czynnej == "alectinib") #Problem z poprawnością wprowadzonych dat







# Naprawiam problem z powtórzeniem się wielokrotnym "Aerivio Spiro --------


lista_counts <- list()
names(lista_counts) <- names(Bazy_lista2)

for (i in 1:length(Bazy_lista2)) {

lista_counts[[i]] <- count(Bazy_lista2[[i]], Nazwa_miedzynarodowa) %>% arrange(desc(n))
  
}

lista_counts




    #Zamieniam wszystkie aerivio na braki, gdyż w danych z 2015, 2017, 2018 i 2019 wkradł się błąd i
#oryginalnie nie było takiego leku

Bazy_lista2$BAZA_2015 <- Bazy_lista2$BAZA_2015 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax")) 

Bazy_lista2$BAZA_2017 <- Bazy_lista2$BAZA_2017 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax")) 

Bazy_lista2$BAZA_2018 <- Bazy_lista2$BAZA_2018 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax")) 
Bazy_lista2$BAZA_2019 <- Bazy_lista2$BAZA_2019 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax")) 
    

Bazy_lista2$BAZA_2016  %>% count(Nazwa_miedzynarodowa) %>% arrange(desc(n))
Bazy_lista2$BAZA_2016  %>% filter(str_detect(Wskazanie_rejestracyjne, "^Aerivio")) %>% 
  count(Nazwa_miedzynarodowa) %>% arrange(desc(n))

    #Naprawiam liczbę obserwacji, które dotyczyły leku Aerivio Spiromax
Bazy_lista2$BAZA_2016 <- Bazy_lista2$BAZA_2016 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax"),
         Nazwa_miedzynarodowa = if_else(str_detect(Wskazanie_rejestracyjne, "^Aerivio"), "Aerivio Spiromax", Nazwa_miedzynarodowa)) 



# Importuje daty z danych w formacie csv ----------------------------------
  #Format excela gubi poprawność części dat, stąd ta zmiana


bazy_names <- str_c("Rok 201", 4:9, " test.csv")

bazy_csv <- list()

for (i in 1:length(bazy_names)) {
  
  bazy_csv[[i]] <- read.csv(bazy_names[[i]], skip = 4) %>%
    as_tibble() %>%
    select(X.4, X.11) %>% 
    mutate(Rok = str_c(201, 3 + i))
  
}

bazy_csv[[2]]

bazy_csv[[1]]$X.4 %>% data.frame()

bazy_csv2 <- bazy_csv %>% 
  reduce(add_row) %>% 
  mutate(
    across(, ~na_if(., 9)),
    ID = 1:nrow(.)
  ) 


wektor_daty <- bazy_csv2 %>% 
  mutate(X.4 = dmy(X.4)) %>% 
  filter(is.na(X.4)) %>% 
  select(ID) %>% 
  as_vector()


daty1 <- bazy_csv2 %>% 
  filter(ID %in% wektor_daty) %>% 
  mutate(
    X.4 = ymd(X.4),
    X.11 = ymd(X.11))

daty2 <- bazy_csv2 %>% 
  filter(!ID %in% wektor_daty) %>% 
  mutate(
    X.4 = dmy(X.4),
    X.11 = ymd(X.11))

daty_razem <- daty1 %>% 
  add_row(daty2) %>% 
  arrange(ID) %>% 
  rename(
    Data_rejestracji2 = X.4,
    Data_rekomendacji2 = X.11) %>% 
  select(-c(Rok, ID))

daty_razem

# Łączę bazy danych -------------------------------------------------------


Baza_razem <- Bazy_lista2 %>% 
  reduce(add_row) %>%  #Funkcja reduce, bardzo ważna sprawa. 
  add_column(daty_razem)       #Zapewnia możliwość połączenia elementów danego wektora zgodnie z algorytmem danej funkcji


  
  #Sprawdzam poprawność dat
  intersect(Baza_razem$Data_rejestracji2, Baza_razem$Data_rejestracji %>% as_date())
  setdiff(Baza_razem$Data_rejestracji2, Baza_razem$Data_rejestracji %>% as_date())
  
  setdiff(Baza_razem$Data_rekomendacji %>% as_date(), Baza_razem$Data_rekomendacji2)
  
  
  Baza_razem %>% 
    count(Data_rejestracji2) %>% 
    arrange(desc(n)) #To wyjaśnia dlaczego intersekcji i różnic łącznie jest mniej niż liczby ocenionych leków. 
                      #Wiele ma powtórzone daty

  Baza_razem %>% 
    filter(Data_rejestracji2 == ymd("2015-01-15")) %>% 
    print(n = 150) #Tutaj upewniam się, czy na pewno różne leki mogą mieć tą samą datę rejestracji




Baza_razem <- Baza_razem %>% 
  mutate(Data_rejestracji = Data_rejestracji2) %>% 
  select(-c(Data_rejestracji2, Data_rekomendacji2))

Baza_razem %>% 
  filter(is.na(Data_rejestracji))


# Dodaję zmienną identyfikującą grupę rekomendacyjną ----------------------
  #Poprawić określenie językowe


Baza_razem <- Baza_razem %>% 
  mutate(ID = 1:nrow(Baza_razem) %>% as.integer(),
         ID2 = if_else(!is.na(Nazwa_miedzynarodowa), ID, rep(0, nrow(Baza_razem)) %>% as.integer()),
         ID2 = na_if(ID2, 0)) %>% 
  select(-c(ID, Unknown1, Unknown2, Unknown3)) %>% 
  rename(ID = ID2) %>% 
  select(ID, Year, everything())



Baza_razem$ID %>% is.na() %>% sum()
Baza_razem$Nazwa_miedzynarodowa %>% is.na() %>% sum()


map(Bazy_lista2, nrow) %>% as_vector() %>% sum()

