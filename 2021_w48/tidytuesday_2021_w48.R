# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(patchwork)

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

# Graphic -----------------------------------------------------------------

# Episodes Ratings Heatmap 
(heatmap_plot <- imdb %>% 
   mutate(rating_filling = cut(rating, breaks = c(3,5,6.5,8,9,10))
          %>%  factor(labels = c("Garbage","Bad","Regular", "Good", "Great"))
   ) %>% 
   ggplot(aes(ep_num, season)) + 
   geom_tile(aes( fill = rating_filling), 
             width = 1, height = 1,
             size = .25, color = "black") +
   geom_text(aes(label = rating), family = "Gotham Medium", size = 7.5, fontface = "bold") + 
   labs(

     subtitle = "Heatmap of average IMDb ratings"
   ) + 
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
       label.position = "right",
       keyheight = unit(1, "cm"),
       keywidth = unit(1, "cm")
     )
   ) + 
   scale_fill_manual(
     values = c("#AAAAAA","#FF4136","#FF851B","#FFDC00","#2ECC40")
   ) + 
   coord_cartesian(expand = F) + 
   theme_minimal() + 
   theme(
     legend.position = "top", 
     panel.grid = element_blank(),
     axis.title = element_text(margin = margin(b = 1, unit = "cm")),
     axis.text = element_text(size = rel(2)),
     legend.text = element_text(family = "Mercury Display", face = "bold", size = rel(1.5))
   )
)

# Ratings Trends Plot 
bad_episode_label <- 'The episode "Orphan" directed by \n Lee Haven Jones and 
written by Ed Hime\n was not good at all.'
blink_episode_label <- 'The episode "Blink"\n  which aired on June 9,  2007\n is the highest rated with 
an average 9.8  by 19,688 viewers.'
season_9_killer_label <- "Season 9 \nRating Killer"
(overtime_plot <- imdb %>% 
    ggplot(aes(overall_ep_num, rating, color = season_fct)) + 
    geom_richtext(data = seasons, aes(x = median_ep_num, label = paste0(" Season ",season_fct, " "), fill = season_fct), 
                  size = 6.5, family = "Gotham Black", color = "white",  y = 11, 
                  label.padding = unit(.5, "cm"), label.r = unit(0,"cm")) + 
    geom_point(size = 7.5) + 
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 4), se = F, size = 6) + 
    annotate(geom = "segment", x = 140, xend = 150, y = 3.9, yend = 3.9, size = .75) +
    # TODO rich text 
    annotate(geom = "text", x = 138,  y = 3.9,family = "Mercury", color = "#111111", size = 7.5, fontface ="bold.italic",lineheight = .9, label = bad_episode_label, hjust = 1) +
    annotate(geom = "segment", x = 34, xend = 37, y = 9.8, yend = 9.8, size = .75) +
    annotate(geom = "text", x = 33,  y = 9.8, family = "Mercury", color = "#111111", size = 7.5, fontface ="bold.italic",lineheight = .9,label = blink_episode_label, hjust = 1) +
    annotate(geom = "text", x = 120,  y = 6.1, family = "Mercury",color = "#111111",size = 7.5, fontface ="bold.italic",lineheight = .9, label = season_9_killer_label , hjust = .5) +
    labs(
      subtitle = "Evolution of the ratings over time and season",
      x = str_to_upper("Episode Number"),
      y = str_to_upper("IMBd Rating")
    ) + 
    scale_x_continuous(
      breaks = 0:15*10
    ) + 
    scale_y_continuous(
      breaks = 4:10,
      limits = c(3.5, 11.5)
    ) + 
    scale_color_manual(
      values = paletteer::paletteer_d("rcartocolor::Prism"),
      guide = "none"
    ) + 
    scale_fill_manual(
      values = paletteer::paletteer_d("rcartocolor::Prism"),
      guide = "none"
    ) + 
    coord_cartesian(expand = F, clip = "off") + 
    theme_minimal() + 
    theme(
      panel.grid.minor = element_blank(), 
      panel.grid.major.x = element_blank(),
      axis.text = element_text(size = rel(1.75))
    )
)

# Plots Combine
heatmap_plot / overtime_plot + 
  plot_annotation(
    title = "- Dr. Who TV Show -",
    caption = "Inspired  by Jim Vallandingham and Kevin Wu.\nData from {datardis} package by way of Jonathan Kitt. \n Tidytuesday Week-48 2021 · Abdoul ISSA BIDA."
  ) & 
  theme(
    text= element_text(family = "Gotham Medium",color = "#111111"),
    axis.title = element_text(size = rel(2)),
    plot.title = element_text(family = "Gotham Black",hjust = .5, size = rel(5)),
    plot.subtitle = element_text(family = "Mercury", face = "bold", size = rel(3.5), hjust = .5, margin = margin(t = .5, b = 0.5, unit = "cm")),
    plot.caption = element_text(size = rel(2), margin = margin(b = .5, unit = "cm")), 
    plot.margin = margin(t = 1.5,r = .5, b = 1, l = .5, unit = "cm"),
    plot.background = element_rect(fill = "#F3F5F7", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w48", "tidytuesday_2021_w48")
ggsave(filename = glue::glue("{path}.pdf"), width = 27.5, height = 32.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 640
)
