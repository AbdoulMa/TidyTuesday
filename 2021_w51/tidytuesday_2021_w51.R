# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggfittext)
library(patchwork)
library(ggtext)
# Data Reading and Wrangling ----------------------------------------------

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')


categories_breaks <- cross_df(
  list(
    start = seq(1.5,5.25, by = 1.25), 
    breaks = 0:5*.2
  )
) %>% 
  mutate(cat_breaks = start + breaks) %>% 
  arrange(cat_breaks) %>% 
  mutate(
    category = rep(c("Danceability", "Energy", "Valence", "Liveness"), each = 6)
  )

axis_labels <- tibble(
  category = c("Danceability", "Energy", "Valence", "Liveness"), 
  x = seq(1.5,5.25, by = 1.25) + .5
)

# https://www.tableau.com/fr-fr/community/music/heavy-metal

top_liveness_label <- '"Who Do You Think You Are?" is by is by far the song with the more liveness. Released in November 1996, the song is heavily influenced by early 1990s dance-pop, and has a nu-disco-style beat that resembles the music of the late 1970s.' %>% 
  str_wrap(width = 50)
n_songs <- nrow(studio_album_tracks) # 

colors_palette <- c(
  "Danceability"= "green", 
  "Energy" = "yellow", 
  "Valence" = "orange", 
  "Liveness" = "pink")

common_theme <- function() {
  theme_minimal() + 
    theme(
      strip.text = element_blank(), 
      axis.text = element_blank(), 
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_markdown(size = rel(7.5), face = "bold",  color = "red", hjust = .5),
      plot.subtitle = element_markdown(size = rel(2.5), hjust = .5, margin = margin(t = .25, b = .25, unit = "cm"))
    )
}


(plot1 <- studio_album_tracks %>% 
    select(danceability, energy, valence, liveness) %>% 
    summarise(
      Danceability = mean(danceability),
      Energy = mean(energy),
      Valence = mean(valence),
      Liveness = mean(liveness)
    ) %>% 
    pivot_longer(everything(), names_to = "category", values_to = "category_mean") %>% 
    mutate(
      category = fct_relevel(category, c("Danceability","Energy", "Valence", "Liveness"))
    ) %>% 
    ggplot(aes(x = 3, fill = category)) + 
    geom_col(
      aes(y= category_mean),
      color = NA
    ) + 
    geom_col(
      aes(y = 1),
      alpha = .35, 
      color = NA
    ) + 
    ggtext::geom_richtext(
      aes(.2, 0, 
          label = glue::glue("<span style ='font-size: 25px;'><b>{str_to_title(category)}</b></span><br>{round(category_mean*100,2)}%"),
          color = category
      ),
      fill = NA,
      label.size = 0, 
      label.color = NA,
      lineheight = 1.5
    ) + 
    labs(
      title = "Spice Girls",
      subtitle = "Global Audio Features"
    ) +
    scale_x_continuous(
      limits = c(0.2, 3 + 0.5) # this 3 links to the 3 above. tweak 0.2 and 0.5 to see what it does to the shape
    ) +
    scale_fill_manual(
      values = colors_palette,
      guide = "none"
    ) + 
    scale_color_manual(
      values = colors_palette,
      guide = "none"
    ) + 
    coord_polar(theta = "y") + 
    facet_wrap(vars(category), nrow = 1) + 
    common_theme()
)

(plot2 <- studio_album_tracks %>% 
    mutate(row_num = row_number()) %>% 
    ggplot() + 
    geom_text(aes(x = -.35, y = row_num*2, label = album_name), hjust = 0, size = 5.5) + 
    geom_fit_text(aes(x = .85 , y = row_num*2, label = track_name), place = "left") + 
    geom_text(aes(x = 1.5 + danceability, y = row_num*2),  label = "𝄞", size = 7.5, color = "green") + 
    geom_segment(aes(x = 1.5, xend = 1.5 + danceability, y = row_num*2, yend = row_num*2),color = "green", size = 2) + 
    geom_text(aes(x = 2.75 + energy + 0.025, y = row_num*2),  label = "♬", size = 7.5, color = "yellow") + 
    geom_segment(aes(x = 2.75, xend = 2.75 + energy, y = row_num*2, yend = row_num*2), color = "yellow",size = 2) + 
    geom_text(aes(x = 4 + valence, y = row_num*2), label = "♪", size = 7.5, color = "orange") + 
    geom_segment(aes(x = 4, xend = 4 + valence, y = row_num*2, yend = row_num*2), color = "orange",size = 2) + 
    geom_text(aes(x = 5.21 + liveness, y = row_num*2), label = "𝅘𝅥𝅱", size = 7.5, hjust = 0, color = "pink") + 
    geom_segment(aes(x = 5.25, xend = 5.25 + liveness, y = row_num*2, yend = row_num*2), color = "pink",size = 2) + 
    # Categories Breaks 
    geom_segment(data = categories_breaks, aes(x = cat_breaks, xend = cat_breaks, color = category), y = -.25, yend = .25, size = .25) + 
    geom_segment(data = categories_breaks, aes(x = cat_breaks, xend = cat_breaks, color = category), y = (n_songs+1)*2 - .25, yend = (n_songs+1)*2 + .25, size = .25) + 
    geom_text(data = categories_breaks, aes(x = cat_breaks, label = glue::glue("{breaks*100}"), color = category), y = - 1.5 ) +
    geom_text(data = categories_breaks, aes(x = cat_breaks, label = glue::glue("{breaks*100}"), color = category), y = (n_songs+1)*2  + 1.5 ) +
    # Categories Labels
    geom_text(data = axis_labels, aes(x = x, label = category, color = category), y = -3) +
    annotate(geom= "text", x = -.35, y = (n_songs+1)*2 + 1, label = "Album", hjust = 0, size = 5 ) + 
    annotate(geom= "text", x = 0.4, y = (n_songs+1)*2 + 1, label = "Track", hjust = 0, size = 5 ) + 
    annotate(geom ="text", x = 5.90, y = 52, label = top_liveness_label, size = 2.5) + 
    labs(
      subtitle = "Each song Audio features"
    ) + 
    scale_x_continuous(limits = c(-0.5,6.5)) + 
    # scale_y_continuous(limits = c(-5,65)) + 
    scale_color_manual(
      values = colors_palette,
      guide = "none"
    ) + 
    coord_cartesian(clip = "off") + 
    common_theme() + 
    theme(
      plot.margin = margin( b = 1, unit = "cm")  
    )
)


plot1 / plot2 + 
  plot_layout(
    heights = c(1, 3)
  ) 


# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2021_w51", "tidytuesday_2021_w51")
ggsave(filename = glue::glue("{path}.pdf"), width = 18, height = 17.5, device = cairo_pdf)
