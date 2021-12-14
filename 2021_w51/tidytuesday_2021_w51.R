# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggfittext)
library(patchwork)
library(ggtext)
# Data Reading and Wrangling ----------------------------------------------

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')

# Convert duration in minutes

## The function to convert
milliseconds_to_min <- function(milliSec) {
  min <-   (milliSec %/% 60000)
  sec <-  round(((milliSec%%60000) /  1000))
  glue::glue("{min} : {sec}{ifelse(sec<=9, '0','')}")
}

## Apply the function 
studio_album_tracks <- studio_album_tracks %>% 
  mutate(duration_min = milliseconds_to_min(duration_ms)) 

# Breaks for each feature
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

# Features Labels Positions
axis_labels <- tibble(
  category = c("Danceability", "Energy", "Valence", "Liveness"), 
  x = seq(1.5,5.25, by = 1.25) + .5
)

# Graphic -----------------------------------------------------------------

top_liveness_label <- '"Who Do You Think You Are?" is by is by far the song with the more liveness. Released in November 1996, the song is heavily influenced by early 1990s dance-pop, and has a nu-disco-style beat that resembles the music of the late 1970s.' %>% 
  str_wrap(width = 50)

n_songs <- nrow(studio_album_tracks) 

colors_palette <- c(
  "Danceability"= "#01FF70", 
  "Energy" = "#FFDC00", 
  "Valence" = "#FF851B", 
  "Liveness" = "#F012BE")

common_theme <- function() {
  theme_minimal() + 
    theme(
      text = element_text(color = "#FFFFFF"),
      strip.text = element_blank(), 
      axis.text = element_blank(), 
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_markdown(family = "Lobster", size = rel(7.5), face = "bold",  color = "#EF476F", hjust = .5, margin = margin(t = 1,b = 1, unit = "cm")),
      plot.subtitle = element_markdown(size = rel(3), face = "bold", family = "Verlag",  hjust = .5, margin = margin(t = .25, b = .25, unit = "cm")), 
      plot.background = element_rect(fill = "#111111", color = NA)
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
          label = glue::glue("<span style ='font-size: 35px;'>{str_to_title(category)}</span><br><span style='font-size:27.5px;'>{round(category_mean*100,2)}%</span>"),
          color = category
      ),
      family = "Verlag Black",
      fill = NA,
      label.size = 0, 
      label.color = NA,
      lineheight = 1.5
    ) + 
    labs(
      title = "-·- Spice Girls -·-",
      subtitle = "Global audio features"
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
    common_theme() +
    theme(
      plot.margin = margin(t = .5,b = 1, unit = "cm")
    )
)

(plot2 <- studio_album_tracks %>% 
    mutate(row_num = row_number()) %>% 
    ggplot() + 
    geom_text(aes(x = -.35, y = row_num*2, label = str_to_upper(album_name)), color = "#FFFFFF", family = "Verlag", fontface = "bold", hjust = 0, size = 5.5) + 
    geom_fit_text(aes(x = .85 , y = row_num*2, label = glue::glue("{track_name} · {duration_min}")), color = "#FFFFFF", family = "MercuryDisplay-Semibold", place = "left", fontface = "bold.italic") + 
    geom_text(aes(x = 1.5 + danceability + .025, y = row_num*2),  label = "𝄞", size = 7.5, color = "#01FF70") + 
    geom_segment(aes(x = 1.5, xend = 1.5 + danceability, y = row_num*2, yend = row_num*2),color = "#01FF70", size = 3, lineend = "round") + 
    geom_text(aes(x = 2.75 + energy + 0.05, y = row_num*2),  label = "♬", size = 7.5, color = "#FFDC00") + 
    geom_segment(aes(x = 2.75, xend = 2.75 + energy, y = row_num*2, yend = row_num*2), color = "#FFDC00",size = 3, lineend = "round") + 
    geom_text(aes(x = 4 + valence + .025, y = row_num*2), label = "♪", size = 7.5, color = "#FF851B") + 
    geom_segment(aes(x = 4, xend = 4 + valence, y = row_num*2, yend = row_num*2), color = "#FF851B",size = 3, lineend = "round") + 
    geom_text(aes(x = 5.215 + liveness + .025, y = row_num*2), label = "𝅘𝅥𝅱", size = 7.5, hjust = 0, color =  "#F012BE") + 
    geom_segment(aes(x = 5.25, xend = 5.25 + liveness, y = row_num*2, yend = row_num*2), color =  "#F012BE",size = 3, lineend = "round") + 
    # Categories Breaks 
    geom_segment(data = categories_breaks, aes(x = cat_breaks, xend = cat_breaks, color = category), y = -.25, yend = .25, size = .25) + 
    geom_segment(data = categories_breaks, aes(x = cat_breaks, xend = cat_breaks, color = category), y = (n_songs+1)*2 - .25, yend = (n_songs+1)*2 + .25, size = .25) + 
    geom_text(data = categories_breaks, aes(x = cat_breaks, label = glue::glue("{breaks*100}"), color = category), family = "Verlag", y = - 1.5 ) +
    geom_text(data = categories_breaks, aes(x = cat_breaks, label = glue::glue("{breaks*100}"), color = category), family = "Verlag", y = (n_songs+1)*2  + 1.5 ) +
    # Features Labels
    geom_text(data = axis_labels, aes(x = x, label = category, color = category),y = -3, family = "Verlag", size = 7.5) +
    # Other annotations
    annotate(geom= "text", x = -.35, y = (n_songs+1)*2 + 1, label = "Album", family = "Verlag", fontface = "bold", hjust = 0, size = 7.5, color = "#FFFFFF") + 
    annotate(geom= "text", x = 0.4, y = (n_songs+1)*2 + 1, label = "Track · Duration", family = "Verlag", fontface = "bold", hjust = 0, size = 5.5, color = "#FFFFFF") + 
    annotate(geom ="text", x = 6, y = 53, label = top_liveness_label, size = 3, family = "MercuryDisplay-Semibold" ,fontface = "italic", color = "#FFFFFF", lineheight = .95) + 
    labs(
      subtitle = "Audio features of each song"
    ) + 
    scale_x_continuous(limits = c(-0.5,6.5)) + 
    scale_color_manual(
      values = colors_palette,
      guide = "none"
    ) + 
    coord_cartesian(clip = "off") + 
    common_theme() + 
    theme(
      plot.margin = margin(t = .5,b = 1, unit = "cm")
    )
)

# Plots Combine
plot1 / plot2 + 
  plot_annotation(
    caption = "*Durations in minute.\n Data comes from Spotify and Genius by way of Jacquie Tran. \n Tidytuesday Week-51 2021 · Abdoul ISSA BIDA inspired by Sean Miller."
  ) +
  plot_layout(heights = c(1, 3)) & 
  theme(
    plot.caption = element_text(size = rel(1.25), family = "Verlag", hjust = .5, color = "#FFFFFF", margin = margin(b = .25, unit = "cm")),
    plot.background = element_rect(fill = "#111111", color =NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w51", "tidytuesday_2021_w51")
ggsave(filename = glue::glue("{path}.pdf"), width = 18, height = 21, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 640
)
