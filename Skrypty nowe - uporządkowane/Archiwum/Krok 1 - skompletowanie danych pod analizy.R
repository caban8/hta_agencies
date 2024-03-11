# Loadowanie bibliotek ----------------------------------------------------
library(readxl)
library(tidyverse)
library(lubridate)

# Zaimportowanie plików excel ---------------------------------------------


sheet <- str_c("201", 4:9)
zmienne_nazwy <- c(
  "Nazwa_miedzynarodowa",	"Nazwa_substancji_czynnej",	"Wskazanie_rejestracyjne",	"ATC_code",	"MAH",	"Data_rejestracji",	"Is_Orphan",	"Is_Generic",	
  "Biosimilar",	"Obszar_terapeutyczny",	"Agencja_kraj",	"Link_do_rekomendacji", 	"Data_rekomendacji", "Rekomendacja", 	"Rekomendowane_wskazanie",				
  "Populacja",	"Komparator",	"Korzyść_kliniczna",	"Bezpieczeństwo",	"RSS",	"Efektywność_kosztowa",	"Wpływ_budżet",	"Inne",	"Komentarz",	"Populacja_II",
  "Komparator_II",	"Korzyść_kliniczna_II",	"Bezpieczeństwo_II",	"RSS_II",	"Efektywność_kosztowa_II",	"Wpływ_budżet_II",	"Inne_II",	"Komentarz_II",
  "Unknown1", "Unknown2", "Unknown3"
  
)
Bazy_lista <- map(sheet, read_excel,  path = "Final updated 2022.xls", na = "9",  skip = 5)

length(Bazy_lista)

for (i in 1:length(Bazy_lista)) {
  names(Bazy_lista[[i]]) <- zmienne_nazwy[1:ncol(Bazy_lista[[i]])]
}





#Stworzyć pętle loadującą odpowienią liczbę nazw zmiennych dla każdej bazy




names(Bazy_lista) <- str_c("BAZA_201", 4:9)
Bazy_lista %>% str()



# Sprawdzam, czy poprawnie pogrupowałem daty ------------------------------

extract_date <- function(x) {
  str_extract(x$Data_rejestracji, "([0-9]*)$") %>% table()
}

map_df(Bazy_lista, extract_date)
str_extract(Bazy_lista$BAZA_2018$Data_rejestracji, "[0-9]*") %>% table()
extract_date(Bazy_lista$BAZA_2014) %>% table()
extract_date(Bazy_lista$BAZA_2015) %>% table()




# Porządkuję w bazach zmienne o datach ------------------------------------


funkcja_rok <- function(x, rok) {
  x %>% mutate(Year = rep(rok, nrow(x)))
}

funkcja_daty <- function(x, rok) {
  x %>% mutate(Data_rejestracji = dmy(Data_rejestracji))
}

lata_baza <- c(2014:2019)

Bazy_lista2 <-  map2(Bazy_lista, lata_baza, funkcja_rok)



Bazy_lista2[1:4] <- map(Bazy_lista2[1:4], funkcja_daty)

Bazy_lista2$BAZA_2017 %>% 
  filter(is.na(Data_rejestracji))

Bazy_lista$BAZA_2017 %>% 
  filter(Nazwa_substancji_czynnej == "alectinib") #Problem z poprawnością wprowadzonych dat



Bazy_lista2$BAZA_2018$Data_rejestracji <- Bazy_lista$BAZA_2018$Data_rejestracji #Tutaj wstawiam starą z powrotem, gdyż u niej data od początku była poprawnie sformatowana



# Ustalam wspólne typy dla wszystkich zmiennych ---------------------------


Bazy_lista2$BAZA_2017$...18 <- Bazy_lista2$BAZA_2017$...18 %>% as.double() 


# Naprawiam problem z powtórzeniem się wielokrotnym "Aerivio Spiro --------


Bazy_lista2$BAZA_2015  %>% count(Nazwa_miedzynarodowa) %>% arrange(desc(n))


    #Zamieniam wszystkie aerivio na braki, gdyż w danych z 2015 wkradł się błąd i oryginalnie nie było takiego leku
Bazy_lista2$BAZA_2015 <- Bazy_lista2$BAZA_2015 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax")) 
    

Bazy_lista2$BAZA_2016  %>% count(Nazwa_miedzynarodowa) %>% arrange(desc(n))
Bazy_lista2$BAZA_2016  %>% filter(str_detect(Wskazanie_rejestracyjne, "^Aerivio")) %>% 
  count(Nazwa_miedzynarodowa) %>% arrange(desc(n))

    #Naprawiam liczbę obserwacji, które dotyczyły leku Aerivio Spiromax
Bazy_lista2$BAZA_2016 <- Bazy_lista2$BAZA_2016 %>% 
  mutate(Nazwa_miedzynarodowa = na_if(Nazwa_miedzynarodowa, "Aerivio Spiromax"),
         Nazwa_miedzynarodowa = if_else(str_detect(Wskazanie_rejestracyjne, "^Aerivio"), "Aerivio Spiromax", Nazwa_miedzynarodowa)) 


# Łączę bazy danych -------------------------------------------------------

Baza_razem <- Bazy_lista2 %>% 
  reduce(add_row) #Funkcja reduce, bardzo ważna sprawa. 
                  #Zapewnia możliwość połączenia elementów danego wektora zgodnie z algorytmem danej funkcji

Baza_razem$Nazwa_miedzynarodowa %>% is.na()



# Dodaję zmienną identyfikującą grupę rekomendacyjną ----------------------
  #Poprawić określenie językowe


Baza_razem <- Baza_razem %>% 
  mutate(ID = 1:nrow(Baza_razem) %>% as.integer(),
         ID2 = if_else(!is.na(Nazwa_miedzynarodowa), ID, rep(0, nrow(Baza_razem)) %>% as.integer()),
         ID2 = na_if(ID2, 0)) %>% 
  select(-ID) %>% 
  rename(ID = ID2) %>% 
  select(ID, everything())



Baza_razem$ID %>% is.na() %>% sum()
Baza_razem$Nazwa_miedzynarodowa %>% is.na() %>% sum()
