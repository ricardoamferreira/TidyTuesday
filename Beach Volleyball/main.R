library(tidyverse)
library(ggthemes)
library(lubridate)
library(cowplot)
library(gghighlight)

# Get the data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# Overview
vb_matches %>% skimr::skim()


player_wins <- vb_matches %>% 
  pivot_longer(cols = c(w_player1, w_player2), values_to = 'player') %>% 
  select(date, player) %>% 
  arrange(date) %>% 
  group_by(player) %>% 
  mutate(wins = row_number()) %>% 
  ungroup()


win_plot <- player_wins %>% 
  mutate(playerwins = paste0(player,': ',wins)) %>% 
  ggplot(aes(date, wins)) +
  geom_line(aes(colour=player), size = 1.2) +
  gghighlight(player %in% c('Kerri Walsh Jennings', 'Phil Dalhausser', 'Jake Gibb'), 
              label_key = playerwins, unhighlighted_params = list(alpha = 0.5, size = 0.33),
              label_params = list(fill = "white", direction = "x", nudge_x = -1250)) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 1200), expand = c(0,0)) +
  labs(
    title = 'AVP and FIVB Wins between 2000 and 2019',
    subtitle = 'The iconic three-time Olympic Gold and one-time Bronze Medalist Kerri Walsh Jennings clearly leads the field\nThe three-time Olympian, and one-time Olympic Gold Medalist Phil Dalhausser follows her in 2nd\nThe three-time Olympian Jake Gibb closes the top 3'
  ) +
  theme(
    plot.title = element_text(vjust = 0, hjust=0.5, size = 20),
    plot.subtitle  = element_text(vjust = 0, hjust=0.5),
    panel.grid.major  = element_blank(),
    axis.line = element_line(),
    axis.title = element_text()
  ) +
  ylab('Wins') +
  xlab('Year')
  

ggdraw(win_plot) +
  draw_image('img/661.png', scale = 0.30, x = 0.42, y = -0.274) +
  draw_image('img/663.png', scale = 0.35, x = 0.32, y = -0.249) +
  draw_image('img/367.png', scale = 0.40, x = 0.22, y = -0.224) +
  ggsave('img1.png', height = 8, width = 9.6, dpi=320)








            