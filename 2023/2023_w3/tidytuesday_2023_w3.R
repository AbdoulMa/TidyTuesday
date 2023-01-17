
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv")

top_five <- df |> 
  count(artist_nationality, sort = T) |> 
  pluck("artist_nationality") |> 
  head(3)

nationality_by_dec <-  df |> 
  mutate(
    artist_nationality = fct_other(artist_nationality, keep = top_five),
    decade = (year %/%  10) * 10
  ) 

overall_nationality <- nationality_by_dec |> 
  count(artist_nationality) #|> 
  # mutate(
  #   artist_nationality = fct_reorder(artist_nationality, n)
  # )

nationality_by_dec <- nationality_by_dec |> 
count(artist_nationality, year) |> 
  arrange(year) |> 
  complete(artist_nationality, year, fill = list(n = 0)) |> 
  group_by(year, artist_nationality) |> 
  summarise(
    count = sum(n)
  ) |> 
  ungroup() # |> 
  # arrange(decade, desc(count)) 

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

overall_nationality
(overall_plot <- overall_nationality |> 
  # arrange(desc(n)) |> 
  ggplot() + 
  geom_pie(aes(cat = artist_nationality, val = n, fill = artist_nationality),
           linewidth = 0.25,
           spotlight_max = T,
           # max_position = "top"
           ) +
  annotate(geom = "text", x = 0, y = 1.55, label = "Most present at top", size = 5) + 
  annotate(geom = "text", x = 0, y = 1.15, label = "Other nationalities") + 
  annotate(geom = "richtext", x = 0, y = - 1.5, label = "ARTISTS <br> NATIONALITIES", 
             size = 15,
             label.size = unit(0, 'pt'), 
             label.r = unit(0, 'pt'), 
             fill = NA
             ) + 
  coord_equal(clip = "off") + 
  plot_theme()
)

(decade_plot <- nationality_by_dec |> 
  # arrange(decade, desc(count)) |> 
  ggplot() + 
  geom_pie(aes(cat = artist_nationality, val = count, fill = artist_nationality),
           spotlight_max = T,
           linewidth = 0.25,
           # max_position = "top"
           ) + 
  facet_wrap(vars(year), ncol = 5) + 
  coord_equal() + 
    plot_theme(
      plot.margin = margin(b = .5, unit = "cm")
    )
)
areas <- 
  "AAAABBBBB
   AAAABBBBB
   AAAABBBBB
  "
(final_plot <- overall_plot + decade_plot +
  plot_layout(design = areas)
)


caption <- caption <- "Tidytuesday Week-02 2023\n Data from  Project FeederWatch\n Abdoul ISSA BIDA @issa_madjid."
  cowplot::ggdraw(final_plot) + 
  cowplot::draw_label(
    x = 0.685, y = 0.1, label = caption,
    size = 11.5,
    hjust = 0, 
    lineheight = 1.125
  ) + 
  theme(
    plot.background = element_rect(fill = "#F7F8F9")
  )
# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w3", "tidytuesday_2023_w3")
ggsave(filename = glue::glue("{path}.png"), width = 13, height = 9, device = ragg::agg_png, dpi = 300)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

