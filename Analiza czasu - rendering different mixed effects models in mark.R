
# Analiza czasu - rendering different mixed effects models in mark --------

library(tidyverse)

params <- expand_grid(
  transformation = c("none", "logarithmic"),
  country_fixed = c(TRUE, FALSE)
  ) %>% 
  mutate(
    params = list(list(transformation = transformation, country_fixed = country_fixed)),
    output = str_c(
      "./Projekt czas do refundacji/Models comparison/Analysis for ", 
      transformation, 
      " ",
      country_fixed,
      ".html" )
  ) 


params %>% 
  select(output_file = output, params) %>% 
  pwalk(rmarkdown::render, input = "./Skrypty nowe - uporzadkowane/Analiza czasu - comparing different models with params.Rmd")
