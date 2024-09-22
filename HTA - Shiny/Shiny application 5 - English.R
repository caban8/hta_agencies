# Aplikacja 

library(rstatix)
library(tidyverse)
library(shiny)

# Zrobić analogiczną apkę, ale z liczbą zgodnych obserwacji
# Albo - dodać drugi graph obok

# Pobieram wektory referencyjne do rekodowania zmiennych
source("Relational database for recoding character values.R", 
       encoding = "UTF-8")

code_korzysc

# Obrabiam odpowiednio bazę danych ----------------------------------------


# df <- read_csv("Baza razem - 21 lutego 2022.csv")
df <- read_csv("Baza razem english - 14.09.2023.csv")
# df <- read_csv("Baza razem english - 22.09.2023.xlsx")


df <- df %>% 
  select(ID, Data_rejestracji, Data_rekomendacji, Nazwa_miedzynarodowa, Obszar_terapeutyczny, 
         Kraj, Rekomendacja,
         Kat1_Bezpieczeństwo, Kat1_Wpływ_budżet, Kat1_Efektywność_kosztowa, Kat1_Korzyść_kliniczna )

# Tworzę filtr do wyrzucenia obserwacji, które mają więcej niż jedną rekomendację w ramach danego wskazania
filtr_duplicated <- df %>% 
  count(Kraj, ID) %>% 
  mutate(unique = if_else(n == 1, TRUE, FALSE)) %>% 
  select(-n)

df <- df %>%  
  left_join(filtr_duplicated) %>% 
  filter(unique) %>% 
  complete(Kraj, ID) %>% 
  select(-unique) %>% 
  mutate(Data_rekomendacji = as.Date(Data_rekomendacji))


# Sprowadzam zmienne kryterialne do postaci dychotomicznej

df <- df %>% 
  mutate(
    across(starts_with("Kat"),
           ~plyr::mapvalues(., from = code_korzysc$Value, to = code_korzysc$Korzysc2)) 
  ) %>% 
  mutate(
    across(starts_with("Kat"), ~factor(., 
                                       levels = c("Niekorzystna", "Korzystna"),
                                       labels = c("Unfavourable", "Favourable")
                                       ))
  ) %>% 
  mutate(
    Rekomendacja = factor(Rekomendacja, 
                          levels = c("Negatywna", "Pozytywna"),
                          labels = c("Negative", "Positive")
                          ),
    Data_rekomendacji = as.character(Data_rekomendacji) %>% lubridate::ymd()
  )



# Ustalam wektory dla select Input ----------------------------------------



criteria <- df %>% 
  select(Kat1_Korzyść_kliniczna, Kat1_Bezpieczeństwo, Kat1_Efektywność_kosztowa, Kat1_Wpływ_budżet) %>% 
  names() %>% 
  c(NA) %>% 
  setNames(c("Clinical benefit", "Safety", "Cost-effectiveness", "Budget impact", "None"))


countries <- unique(df$Kraj)

stats <- setNames(c("n", "prop"), c("Frequencies", "Proportion"))


recommendation <- setNames(c("Rekomendacja", NA), c("Yes", "No"))

main_vars <- c(
  "ID", "Data_rekomendacji", "Nazwa_miedzynarodowa", 
  "Obszar_terapeutyczny", "Kraj"
)

wider_vars <- c("Data_rekomendacji")



# Parametry dla graphu ----------------------------------------------------

graph_opt <- list(
  geom_bar(
    stat = "identity", 
    position = position_dodge(preserve = "single", width = 0.7),
    width = 0.6),
  theme(legend.position = "top")
)


kolory_zgodnosc <- c("#CB2027", "#059748")
kolory_ocena <- c("#323232", "#265DAB")



# Parametry dla tabel -----------------------------------------------------

# Tabela PABAK
kappa_labs <- c("PABAK", "Lower Bound", "Upper Bound", "Number of recommendations")

# Tabela obserwacje
obs_pre <- c(
  criteria, 
  main_vars
  )
obs_post <- c(
  names(criteria),
  "ID", "Recommendation's Date", "International Name", 
  "Therapeutic indication category", "Kraj"
  
  )


# Własne funkcje ----------------------------------------------------------



# Tworzę funkcję do zliczania Kappa
kappa_shiny <- function(x, y) {
  
  
  # Przygotuj bazę i usuń braki 
  if (is.data.frame(x)) {
    df <- x %>% 
      tidyr::drop_na()
  } else {
    df <- data.frame(x, y) %>% 
      tidyr::drop_na()
  }
  
 
  
  # Wykonaj pełne obliczenie
  result <- df %>% 
    table() %>% 
    epiR::epi.kappa() %>% 
    {.$pabak} %>% 
    mutate(sample = nrow(df)) %>% 
    setNames(c("Kappa", "Dolna granica", "Górna granica", "Liczba wskazań")) %>% 
    mutate(across(everything(), ~round(., 2))) 
    
  
  
    # If small sample size
  if (nrow(df) == 0) {return("No data")}
  
  
  return(result)
  
}




# User interface ----------------------------------------------------------



ui <- fixedPage(
  theme = bslib::bs_theme(
    bootswatch = "flatly"
  ),
  tabsetPanel(
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "recommendation", "Recommendation", choices = recommendation, selected = "Yes"
                 ),
                 selectInput(
                   "aspect", "Aspect", choices = criteria, selected = NA
                 ),
                 selectInput(
                   "country1", "First country", choices = countries, selected = "Poland"
                 ),
                 selectInput(
                   "country2", "Second country", choices = countries, selected = "Australia"
                 ),
                 selectInput(
                   "stat", "Statistic", choices = stats, selected = "Frequencies"
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(12,
                          h5("Number of recommendations depending on the country - ",
                             textOutput("tytul2"),
                             style = "text-align: center;"
                             ),
                          plotOutput("plot")
                   )
                 ),
                 fluidRow(
                   column(6,
                          h5(
                            textOutput("kraje"),
                            style = "text-align: center; font-size: 18px;"
                          ),
                          tableOutput("results"),
                          p(" ",
                            style = "font-size: 10px;")
                   ),
                   column(6,
                          h5(
                            "Distribution of agreements and disagreements - ", 
                            textOutput("tytul"),
                            style = "text-align: center;"
                          ),
                          plotOutput("plot2")
                   )
                 ),
               )
             )
    ),
    tabPanel("Observations",
             dataTableOutput("Observations")
    )
  )
)



# Server ------------------------------------------------------------------




server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  # Wyodrębniam wektor do selekcji zmiennych
  vars <- reactive({
    vars <-  c(input$recommendation, input$aspect)
    vars[vars != "NA"]
  })
  
  
  # Update country2 choices when country1 is selected
  observe({
    selected_country1 <- input$country1
    current_country2 <- isolate(input$country2)  # Preserve the current selection
    
    # Update country2 options, remove the option that is equal to country1
    updated_countries <- countries[countries != selected_country1]
    
    # If the current country2 is still valid, keep it selected, otherwise, set to NULL
    new_country2 <- ifelse(current_country2 %in% updated_countries, current_country2, NULL)
    
    updateSelectInput(session, "country2",
                      choices = updated_countries,
                      selected = new_country2)
  })
  
  
 
 
  
  
  ## Wyodrębniam wektor do nazewnictwa
  #vars2 <- reactive({
  #  vars2 <-  str_replace(vars(), "Kat1_", "") %>% 
  #    str_replace_all("_", " ")
  #})
  
  # Wyodrębniam wektor do nazewnictwa
  vars2 <- reactive({
    vars2 <-  plyr::mapvalues(
      vars(), 
      c("Rekomendacja", "Kat1_Korzyść_kliniczna", "Kat1_Bezpieczeństwo", "Kat1_Efektywność_kosztowa", "Kat1_Wpływ_budżet"),
      c("Recommendation", "Clinical benefit", "Safety", "Cost-effectiveness", "Budget impact")
      ) 
  })
  
  
  
  # Tworzę wektor do wymienienia krajów w tytule
  output$kraje <- renderText({
    if (length(vars2() ) == 1) {
      paste(
        "Consistency between", 
        input$country1, "and", input$country2, 
        "within", str_to_lower(vars2()) 
      )
    } else if (length(vars2() ) == 2) {
      paste("Consistency between", input$country1, "and", input$country2, 
            "depending on the concordance within", 
            str_to_lower(vars2()[2]))
    } else {""}
    
    
   
    
    })
  
  # Tworzę wektor do wymienienia zmiennych w tytułach
  tytul <- reactive({
    tytul <- if (length(vars2() ) == 1) {
      vars2()
    } else if (length(vars2() ) == 2) {
      paste(vars2()[1], "and", vars2()[2])
    } else {""}
    
    str_to_lower(tytul)
  })
  
  output$tytul <- renderText(tytul())
  output$tytul2 <- renderText(tytul())
  
  # Wyodrębniam bazę danych do analizy
  df2 <- reactive({
    df2 <- df %>% 
      filter(Kraj %in% c(input$country1, input$country2)) %>% 
      select(all_of(c(main_vars, vars())))
    
    filter_id <- df2 %>% 
      count(across(c("ID", "Kraj", vars()))) %>% 
      drop_na() %>% 
      count(ID) %>% 
      filter(n == 2) %>% 
      pull(ID)
    
    df2 %>% 
      filter(ID %in% filter_id)
  })
  
  
  
  # Obliczam częstości i proporcje do wykresu
  proportion <- reactive({
    df2() %>% 
      freq_table(vars = c("Kraj", rev(vars() ))) %>% 
      complete(!!!rlang::syms(c("Kraj", vars() )), fill = list(n = 0, prop = 0)) 
    
    # In this example, rlang::syms() is used to convert the character vector vars_to_complete 
    # into a list of symbols, and !!! (bang-bang-bang) is used to splice the list of symbols as 
    # individual arguments 
    
  })
  
  
 
  # Wyodrębniam tytuł
  tit <- reactive(
    str_c(vars2(), collapse = " oraz ") %>% 
      str_c(" w zależności od kraju") %>% 
      str_to_sentence()
  )
  

  
  
  # Tworzę tabelkę zestawiającą kolumny obserwacji dla dwóch krajów
  tab <- reactive({
    df3 <- df2() %>% 
      pivot_wider(
        names_from = Kraj, 
        values_from = c(Data_rekomendacji, vars()) 
      ) %>% 
      select(-ID)  
    
      # Dodaję zmienne o liczbie zgodnych i niezgodnych opinii 
      if (length(vars()) == 1) {
        df3 %>% mutate(agreement = df3[5] == df3[6])
      } else if (length(vars()) == 2) {
        df3 %>% mutate(
              agreement = df3[5] == df3[6],
              agreement2 = df3[7] == df3[8]
            )
      } else {df3}
      
  })
  
  # Tworzę tabelkę zestawiającą liczbę zgodnych i niezgodnych opinii
  tab_agree <- reactive({
    
   
    tab2 <- tab() %>% 
      freq_table(rev(starts_with("agreement"))) %>% 
      complete(
        !!!rlang::syms(names(.)[-c((ncol(.) - 1):ncol(.))]), 
        fill = list(n = 0, prop = 0)
      ) %>% 
      mutate(
        across(
          starts_with("agreement"),
          ~plyr::mapvalues(., c(FALSE, TRUE), c("Disagreement", "Agreement")) %>%  # tu poprawić
          factor(levels = c("Disagreement", "Agreement"))
        )
      ) %>% 
      setNames(c(rev(vars()), "n", "prop"))
    
     if (ncol(tab2) == 4) {
       tab2[[1]] <- paste(tab2[[1]], vars2()[2])
       tab2[[2]] <- paste(tab2[[2]], vars2()[1])
     }
    
    tab2
    
  })
  
  
  
  # Wyodrębniam input dla rodzaju statystyki
  y <- reactive(input$stat)
  y_lab <- reactive(ifelse(y() == "n", "Frequency", "Percent"))
  
  # Tworzę wykres dla liczebności
  output$plot <- renderPlot({
    if (length(vars()) == 1) {
      proportion() %>% 
        ggplot(aes(x = Kraj, y = get(y()), fill = get(vars() ))) +
        graph_opt +
        labs(
          fill = vars2(),
          y = y_lab(), 
          x = ""
        ) +
        scale_fill_manual(values = kolory_ocena)
    } else if (length(vars()) == 2) {
      proportion() %>% 
        ggplot(aes(x = Kraj, y = get(y()), fill = get(vars()[1]))) +
        graph_opt +
        facet_wrap(~get(vars()[2] )) +
        labs(
          fill = vars2()[1],
          y = y_lab(), 
          x = ""
          
        ) +
        scale_fill_manual(values = kolory_ocena)
    } else {print("none")}
  }, res = 96)
  
  
  # Tworzę wykres zgodności
  output$plot2 <- renderPlot(
    if (length(vars()) == 1) {
      tab_agree() %>% 
        ggplot(aes(x = get(vars() ), y = get(y()), fill = get(vars() ))) +
        geom_bar(stat = "identity", width = 0.6, alpha = 4/5) +
        labs(
          x = "",
          y = y_lab()
        ) +
        scale_fill_manual(values = kolory_zgodnosc) +
        guides(fill = "none") 
      
    } else if (length(vars()) == 2) {
      tab_agree() %>% 
        ggplot(aes(x = get(vars()[1] )  %>% fct_rev(), y = get(y()), fill = get(vars()[1])  %>% fct_rev())) +
        graph_opt +
        labs(
          x = "", 
          fill = "",
          y = y_lab(), 
        ) +
        scale_fill_manual(values = kolory_zgodnosc) +
        facet_wrap(~fct_rev(get(vars()[2]) ), scales = "free_x") +
        scale_x_discrete(labels = NULL)
       # guides(fill = "none")
    } else {print("none")}, res = 96
  )
  
  
  # Obliczam wskaźnik Kappa
  kappa_result <- reactive({
    if (length(vars()) == 1) {
      tab() %>% 
        select(starts_with(vars())) %>% 
        kappa_shiny() %>% 
        pivot_longer(everything()) %>% 
        setNames(c("Statistic", vars2())) %>% 
        mutate(Statistic = kappa_labs)
      
    } else if (length(vars()) == 2) {
      
      kappa_df <- tab() %>% 
        select(agreement2, starts_with("Rekomendacja")) 
      
      zgodny_kappa <- kappa_df %>% 
        filter(agreement2) %>% 
        select(-agreement2) %>% 
        kappa_shiny()
      
      niezgodny_kappa <- kappa_df %>% 
        filter(!agreement2) %>% 
        select(-agreement2) %>% 
        kappa_shiny()
      
      zgodny_kappa %>% 
        add_row(niezgodny_kappa) %>% 
        t() %>% 
        as.data.frame() %>% 
        setNames(c("Agreement", "Disagreement")) %>% 
        mutate(Statistic = kappa_labs) %>% 
        select(Statistic, Disagreement, Concordance) 
        
        
    } else {print(" ")}
    
  })
  
  
  
  # Output dla Kappa
  output$results <- renderTable(kappa_result())
  
  output$Observations <- renderDataTable({
    tab_obs <- tab() %>% 
      select(-starts_with("agreement")) 
    
    tab_labs <- plyr::mapvalues(names(tab_obs), obs_pre, obs_post) %>% 
      str_replace("Data_rekomendacji", "Recommendation's Date") %>%
      str_replace("Kat1_Bezpieczeństwo", "Safety") %>%
      str_replace("Kat1_Wpływ_budżet", "Budget impact") %>%
      str_replace("Kat1_Korzyść_kliniczna", "Clinical benefit") %>%
      str_replace("Kat1_Efektywność_kosztowa", "Cost-effectiveness") %>%
      str_replace("Rekomendacja", "Recommendation") %>% 
      str_replace_all("_", " ") 
    
    tab_obs %>% 
      setNames(tab_labs) 
    
  })
  
  
}

shinyApp(ui, server)
