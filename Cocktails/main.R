library(tidyverse)
library(ggthemes)
library(cowplot)


cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


top_glasses <- 
  cocktails %>% 
  mutate(glass = str_to_lower(glass)) %>% 
  count(glass, sort = TRUE) %>% 
  top_n(5)


top_ing <- boston_cocktails %>%
  count(ingredient, sort = TRUE) %>% 
  top_n(5)


bar_plot <- boston_cocktails %>% 
  filter(ingredient %in% top_ing$ingredient) %>% 
  ggplot(aes(ingredient, fill = category)) +
  geom_bar() + 
  scale_x_discrete(limits = top_ing$ingredient) +
  geom_text(aes(label = ..count..), stat='count',
            position = position_stack(0.5), size=3) +
  theme_wsj() +
  labs(
    title = 'In what kind of cocktails are the\n5 most used ingredients found?',
    fill = 'Category'
  ) +
  theme(axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ylab('Count') 
  
  


bar_plot

ggdraw(bar_plot) +
ggsave(paste0("cocktail_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), width = 8, height = 9)


