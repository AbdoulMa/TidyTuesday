
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggforce)
library(ggtext)
library(cowplot)

# Data Wrangling ----------------------------------------------------------
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

# Compute rating proportions for 1000 released games
ratings %>% 
  mutate(average_fct = cut(average, breaks = c(0, 4.5, 5.25, 6, 6.75, 7.5, 10), labels = str_c("Level ", 1:6))) %>% 
  count(average_fct) %>% 
  mutate(n = MESS::round_percent(prop.table(n))*10) %>% 
  pmap_dfr(~tibble(average_fct = rep(.x, .y))) %>% 
  mutate(
    row_num = row_number()-1, 
    # Circles centers coordinates
    x = row_num %% 20, 
    y = row_num %/% 20
  ) -> ratings_circles

# Graphic -----------------------------------------------------------------
plot <- ratings_circles %>% 
  ggplot() + 
  geom_circle(aes(x0= x, y0 =y, r = .5, fill = average_fct)) + 
  coord_equal(expand = F, clip = "off") + 
  scale_fill_manual(
    values = c(
      "Level 1" = "#5aba47",
      "Level 2" = "#ff0052", 
      "Level 3" = "#d2da25", 
      "Level 4" = "#00d0e0",
      "Level 5" = "#ff009d",
      "Level 6" = "#611e86"
    ),
    guide= "none"
  ) + 
  annotate(geom = "text", x = -1, y = 0.5, label = " lower than 4.5, 2.72%", size = 6.5, family = "Ve Bold", color = "#FFFFFF",hjust = 1) + 
  annotate(geom = "text", x = -1, y = 3, label = "between 4.5 and 5.25 - 7.3%", size = 6.5, family = "Ve Bold", color = "#FFFFFF", hjust = 1) + 
  annotate(geom = "text", x = -1, y = 10, label = "between 5.25 and 6 - 21%", size = 6.5, family = "Ve Bold", color = "#FFFFFF", hjust = 1) + 
  annotate(geom = "text", x = -1, y = 24, label = "between 6 and 6.75 - 32.9%",size = 6.5, family = "Ve Bold", color = "#FFFFFF", hjust = 1) + 
  annotate(geom = "text", x = -1, y = 38, label = "between 6.75 and 7.5 - 24.7%", size = 6.5, family = "Ve Bold", color = "#FFFFFF", hjust = 1) + 
  annotate(geom = "text", x = -1, y = 46, label = "higher than 7.5, 11.4%", size = 6.5, family = "Ve Bold", color = "#FFFFFF", hjust = 1) + 
  annotate(geom = "segment", x= 20, xend = 24, y = 0, yend = 0, color = "#FFFFFF") + 
  annotate(geom = "text", x = 24, y = 0, label = "Games like Tic-Tac-Toe, \nMonopoly and Bingo.", family = "M G1 Semibold Italic", color="#FFFFFF", size = 6.5, hjust = 0) + 
  annotate(geom = "segment", x= 20, xend = 28, y = 46, yend = 46, color="#FFFFFF") + 
  annotate(geom = "segment", x= 28, xend = 28, y = 46, yend = 44, color="#FFFFFF") + 
  annotate(geom = "text", x = 32, y = 44, label = "There are games like Carcassonne,\n7 wonders, Dominions and Codename.", family = "M G1 Semibold Italic", color="#FFFFFF", size = 6.5, vjust = 1.1) + 
  labs(title = str_to_upper("Boards Games"), 
       subtitle = "For 1000 games released, what are their average ratings?", 
       caption = "Data from  Kaggle by way of Board Games Geek\n with h/t to David and Georgios
        Tidytuesday Week-4 2022 · Abdoul ISSA BIDA") + 
  theme_minimal() + 
  theme(
    text = element_text(color = "#FFFFFF"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = rel(4.5), family = "Ve Black", margin = margin(t = 15, b=15)),
    plot.subtitle = element_text(size = rel(2), family = "M G1 Bold",  margin = margin(b=15)),
    plot.caption = element_text(size = rel(1.5), family = "Ve Bold", hjust = 1, margin = margin(t = 25))
  )

# Edit background
ggdraw(plot) + 
  theme(
    plot.background = element_rect(fill = "#2d2d2e", color = NA), 
    plot.margin = margin(b = 15, l=15)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w4", "tidytuesday_2022_w4")
ggsave(filename = glue::glue("{path}.png"), width = 12, height = 12, device = ragg::agg_png, dpi = 640)
