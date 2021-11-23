# Load libraries ----------------------------------------------------------
library(tidyverse)

# Data Reading and Wrangling ----------------------------------------------
imdb <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv")

imdb <- imdb %>% 
  arrange(season, ep_num) %>% 
  mutate(overall_ep_num = row_number(), 
         season_fct = factor(season)) %>% 
  relocate(overall_ep_num)

seasons <- imdb %>% 
  group_by(season_fct) %>% 
  summarise(
    median_ep_num = median(overall_ep_num)
  )

# The Episode "Orphan" directed by Lee Haven Jones and 
# written by Ed Hime was not good at all.


# Episodes Evolution ------------------------------------------------------
bad_episode_label <- 'The Episode "Orphan" directed by \n Lee Haven Jones and 
written by Ed Hime\n was not good at all.'
imdb %>% 
  ggplot(aes(overall_ep_num, rating, color = season_fct)) + 
  geom_text(data = seasons, aes(x = median_ep_num, label = paste0("Season ",season_fct)), y = 10) + 
  geom_point(size = 2.5) + 
  geom_smooth(se = F, size = .75) + 
  annotate(geom = "segment", x = 140, xend = 150, y = 3.9, yend = 3.9, size = .25) +
  # TODO rich text 
  annotate(geom = "text", x = 138,  y = 3.9, label = bad_episode_label, hjust = 1) +
  scale_color_discrete(
    guide = "none"
  )


# Episodes Ratings 
imdb %>% 
  mutate(rating_filling = cut(rating, breaks = c(3,5,6.5,8,9,10))
         %>%  factor(labels = c("Garbage","Bad","Regular", "Good", "Great"))
  ) %>% 
  ggplot(aes(ep_num, season)) + 
  geom_tile(aes( fill = rating_filling), 
            width = 1, height = 1,
            size = .25, color = "white") +
  geom_text(aes(label = rating)) + 
  scale_y_reverse(
    name = "SEASON",
    breaks = 1:12
  ) +
  scale_x_continuous(
    name = "EPISODE",
    position = "top",
    breaks = 0:14
  ) +
  guides(
    fill = guide_legend(
      reverse = T,
      title = "",
      label.position = "right"
    )
  ) + 
  scale_fill_manual(
    values = c("#AAAAAA","#FF4136","#FF851B","#FFDC00","#2ECC40")
  ) + 
  coord_cartesian(expand = F) + 
  theme_minimal() + 
  theme(
    legend.position = "top", 
    panel.grid = element_blank()
  )


paletteer::paletteer_d("rcartocolor::Bold") %>% 
  scales::show_col()

# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
