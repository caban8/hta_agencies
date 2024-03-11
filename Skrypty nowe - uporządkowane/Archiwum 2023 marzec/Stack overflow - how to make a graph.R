library(tidyverse)

set.seed(10)

DF <- tibble(
  Recommendation = c(rep( "Positive", 20), rep("Negative", 20)),
  Country = rep(sample(letters, 10), 4),
  Category = c(rep( "Better", 10), rep("Worse", 10)) %>% rep(2),
  n = c(sample(200:400, 20), sample(50:150, 20))
)
DF %>% print(n = 40)

DF %>% 
  mutate(n = as.double(n),
         n = if_else(Recommendation == "Negative", (n * -1), n)) %>% 
  ggplot(aes(Country, n, fill = Category, alpha = Recommendation)) +
  geom_bar(stat = "identity") +
  scale_alpha_discrete(range = c(0.6, 0.9)) +
  facet_grid(~Recommendation,  scales = "free_x", space = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  coord_flip() +
  guides(alpha = F) +
  theme(legend.position = "bottom")



