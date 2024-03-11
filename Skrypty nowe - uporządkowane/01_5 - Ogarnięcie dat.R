
# Sprawdzam problem z datami ----------------------------------------------

df_na <- Bazy_lista2 %>% 
  reduce(add_row) %>% 
  mutate(ID = 1:nrow(.)) %>% 
  filter(is.na(Data_rejestracji)) %>% 
  select(ID)

Bazy_test_daty2 <- Bazy_test_daty %>% map(function(x) {x %>% 
    mutate(Data_rejestracji = as.character(Data_rejestracji),
    Data_rekomendacji = as.character(Data_rekomendacji))
  }
    ) %>% 
  reduce(add_row) %>% 
  mutate(ID = 1:nrow(.)) 
  
df_na2 <- df_na %>%  left_join(Bazy_test_daty2 %>% select(Data_rejestracji, Data_rekomendacji, ID)) 

Bazy_test_daty2

df_na2



# Próbuję z formatem csv --------------------------------------------------


baza_test2017 <- read.csv("Rok 2017 test.csv", skip = 4) %>% as_tibble()

baza_test2017$X.4
baza_test2017 %>% filter(X == "alectinib") %>% 
  select(X.4)




# Pseudokod ---------------------------------------------------------------


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

daty1 %>% 
  add_row(daty2) %>% 
  arrange(ID) %>% 
  rename(
    Data_rejestracji = X.4,
    Data_rekomendacji = X.11)




#https://stackoverflow.com/questions/13764514/how-to-change-multiple-date-formats-in-same-column

#Importuje bazy w formacie csv
#Łączę w jedną i dodaje do wszystkich ID
#Pobieram tylko daty
#Na podstawie ID dodaję je następnie do starej bazy, na miejsce starych


