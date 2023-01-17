
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv")

top_countries <- df |> 
  count(artist_nationality, sort = T) |> 
  pluck("artist_nationality") |> 
  head(3)

artist_by_country <-  df |> 
  mutate(
    artist_nationality = fct_other(artist_nationality, keep = top_countries)
  ) 

overall_nationality <- artist_by_country |> 
  count(artist_nationality)

artist_by_country <- artist_by_country |> 
  count(artist_nationality, year) |> 
  arrange(year) |> 
  complete(artist_nationality, year, fill = list(n = 0)) |> 
  group_by(year, artist_nationality) |> 
  summarise(
    count = sum(n)
  ) |> 
  ungroup()

# Graphic -----------------------------------------------------------------
plot_theme <- function(...) {
  theme_minimal() + 
    theme(
      axis.title =  element_blank(),
      axis.text =  element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      ...
    )
}

countries_pal <- c( 
  "Other" = "#E1161C", 
  "British" = "#FEAF16", 
  "French" = "#346C3B",
  "American" = "#167FB8"
)

font <- "TT Norms"
(overall_plot <- overall_nationality |> 
    ggplot() + 
    # Working on the geom, I will release it when ready 
    # Feel free to contact me if you really need me 
    geom_pie(aes(cat = artist_nationality, val = n, fill = artist_nationality),
             linewidth = 0.25,
             spotlight_max = T
    ) +
    annotate(geom = "text",family = font, x = 0, y = 1.45, label = "OVERALL", fontface = "bold", size = 10) + 
    annotate(geom = "text",family = font, x = 0, y = 1.25, label = "The most represented is at top", size = 5.5) + 
    annotate(geom = "text",family = font, fontface = "bold", x = 0, y = .5, label = toupper("Others"), color = "white", size = 5.5) + 
    annotate(geom = "text",family = font, fontface = "bold", x = .025, y = -.65, label = toupper("British"),  size = 5.5) + 
    annotate(geom = "text",family = font, fontface = "bold", x = -.5, y = 0, label = toupper("American"), size = 5.5) + 
    annotate(geom = "text",family = font, fontface = "bold", x = .5, y = 0, label = toupper("French"),  color = "white", size = 5.5) + 
    annotate(geom = "segment", x = 0, y = 1.2, xend = 0, yend = 1.05, arrow = arrow(length = unit(5,'pt') ))  + 
    annotate(geom = "segment", x = -1.2, y = -1.65, xend = -1.2, yend = 1.65, size = 2) + 
    annotate(geom = "richtext", x = -1, y = - 1.30, label = "NATIONALITIES<br>OF  ARTISTS", 
             size = 15.5,
             family = font,
             fontface = "bold",
             hjust = 0,
             label.size = unit(0, 'pt'), 
             label.r = unit(0, 'pt'), 
             fill = NA
    ) + 
    scale_x_continuous(expand = expansion(mult = 0)) + 
    coord_equal(clip = "off")  + 
    scale_fill_manual(
      values = countries_pal
    ) + 
    plot_theme(
      plot.margin = margin(c(0,0,0,0), unit = "cm")
    )
)

(edition_plot <- artist_by_country |> 
    ggplot() + 
    geom_pie(aes(cat = artist_nationality, val = count, fill = artist_nationality),
             linewidth = 0.25,
             spotlight_max = T,
             max_position = "top"
    ) + 
    facet_wrap(vars(year), ncol = 5) + 
    scale_fill_manual(
      values = countries_pal
    ) + 
    coord_equal() + 
    plot_theme(
      strip.text = element_text(family = font, size = rel(1.25)),
      plot.margin = margin(c(0,0,0,0), unit = "cm")
    )
)
(final_plot <- overall_plot + edition_plot +
    plot_layout(widths = c(4/9, 5/9))
)


caption <- caption <- "Tidytuesday Week-03 2023\n Data from {arthistory} package\n Abdoul ISSA BIDA @issa_madjid."
cowplot::ggdraw(final_plot) + 
  cowplot::draw_label(
    x = 0.725, y = 0.1, label = caption,
    fontfamily = font, 
    fontface = "bold",
    size = 14.5,
    hjust = 0, 
    lineheight = 1.125
  ) + 
  theme(
    plot.background = element_rect(fill = "#C0DCF1")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w3", "tidytuesday_2023_w3")
ggsave(filename = glue::glue("{path}.png"), width = 9*16/9, height = 9, device = ragg::agg_png, dpi = 320)
