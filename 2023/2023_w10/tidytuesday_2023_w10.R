
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv")

## Important: I will carefully comment the code on March 08th 
df <- df |> 
  mutate(
    dataResourceName = str_trunc(dataResourceName, 25, side = "right"),
    dataResourceName = fct_lump(dataResourceName, 9),
  )
resources_df <- df |> 
  count(dataResourceName) |> 
  mutate(
    index = row_number(),
    .before =  1L
  ) |> 
  mutate(
  n_rescaled = scales::rescale(n, to = c(2.5,10), from = range(resources_df$n))
  )

species_df <- df |> 
  distinct(dataResourceName,scientificName) |> 
  arrange(dataResourceName, scientificName)  |> 
  group_by(dataResourceName) |> 
  mutate(
    num = row_number()
  ) |> 
  ungroup() |> 
  left_join(resources_df) 


nb_resources <- nrow(resources_df)
margin <- 1 / 10 
inc1 <-  pi/8
part1_df <- resources_df |> 
  rowwise() |> 
  mutate(
  x = map(index, \(index) {c(index-1 + margin, index - margin , index - margin, index - 1 + margin)}), 
  y =  map(index, \(index) {c(nb_resources/3 - (index-1 + margin)*tan(inc1), nb_resources/3 - (index -margin)*tan(inc1), -n_rescaled,-n_rescaled)})
  )  |> 
  unnest_longer(c(x, y)) 

h2 <- 5
inc2 <- pi / 4
part2_df <- resources_df |> 
  mutate(
    x = map(index, \(index) {c(index-1 + margin, index - margin, index - margin + h2 * sin(inc2), index-1 + margin + h2*sin(inc2))}),
    y = map(index, \(index) {c(nb_resources/3 - (index-1 + margin)*tan(inc1), nb_resources/3 - (index -margin)*tan(inc1), nb_resources/3 - (index -margin)*tan(inc1) + h2*cos(inc2), nb_resources/3 - (index-1 + margin)*tan(inc1) + h2*cos(inc2))})
  ) |> 
  unnest_longer(c(x,y))

part3_df <- resources_df |> 
  mutate(
    x = index - 1 + 1/2, 
    y = nb_resources/3 - (index-1  + 1/2)*tan(inc1)
  )

# Graphic -----------------------------------------------------------------
caption <-  "Tidytuesday Week-10 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      Data from **Atlas of Living Australia**"
  ggplot() + 
  geom_polygon(data = part1_df, aes(x,y, group = as.factor(index)),  size = 0, fill = "black") + 
  geom_polygon(data = part2_df, aes(x,y, group = as.factor(index)),  size = 0, fill = "black") + 
  geom_text(data = part3_df, aes(x,y, label = dataResourceName), hjust = -0.1, angle = 45, color = "white", family = "UEFA Supercup", fontface = "bold") +
  geom_segment(data = part3_df, aes(x =x +  (h2-0.5)*cos(inc2),y = y +  (h2-0.5)*sin(inc2), xend =x +  (h2+0.75)*cos(inc2),yend = y +  (h2+0.75)*sin(inc2)), color = "grey") +
  geom_point(data = part3_df, aes(x +  (h2-0.5)*cos(inc2),y +  (h2-0.5)*sin(inc2)), shape = 21, size = 2, color = "white", fill = "black") +
  geom_text(data = part3_df, aes(x +  (h2+1.25)*cos(inc2),y +  (h2+1.25)*sin(inc2), label = n),  angle = 45, size = 5, family = "Iosevka Semibold") +
  ggforce::geom_circle(data = part3_df, aes(x0 = index -1/2, y0 = -n_rescaled, r = (1 -2*margin)/2), size = 0, fill = "black") + 
  ggforce::geom_circle(data = species_df, aes(x0 = index - 1/2, y0 = -n_rescaled + num -1, r = 0.35, fill = scientificName)) + 
  annotate( geom ="label", x = 0, y = -3.25, label = str_wrap("Myrmecobius fasciatus", 15), fill = "#EA4C50",
            family = "UEFA Supercup",
            fontface = "bold",
            label.r = unit(0,'pt'),
            label.padding = unit(5,'pt'),
            label.size = unit(0,'pt'),
            hjust = 1) + 
  annotate( geom ="label", x = 0, y = -2.15, label = str_wrap("Myrmecobius fasciatus rufus", 15), fill = "#FEDC2D", 
            family = "UEFA Supercup",
            fontface = "bold",
            label.r = unit(0,'pt'),
            label.padding = unit(5,'pt'),
            label.size = unit(0,'pt'),
            hjust = 1) + 
  annotate(geom = "richtext", x = -1.5, y = -10, label = caption, 
           family = "UEFA Supercup",
           hjust = 0,
           fill = NA,
           size = 5,
           label.r = unit(0,'pt'),
           label.padding = unit(5,'pt'),
           label.size = unit(0,'pt')
           ) + 
    labs(
      title = "Numbats sightings in Australia",
      
    ) + 
    scale_fill_manual(
      values = c(
        "Myrmecobius fasciatus" = "#EA4C50",
        "Myrmecobius fasciatus rufus" =  "#FEDC2D"
      ),
      guide = "none"
    ) + 
  coord_equal(clip = "off") +
  theme_minimal() + 
    theme(
      panel.grid = element_blank(), 
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      plot.title  = element_text(family = "UEFA Supercup", face = "bold", hjust = 0, size = rel(3.5), margin = margin(t = .25, b = .25, unit = "cm")),
      plot.caption = ggtext::element_markdown(hjust = 0, size = rel(1.25)),
      plot.background = element_rect(fill = "grey90", color = NA),
      plot.margin = margin(c(.25, -.125, .25, -.125), unit = "cm")
    )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w10", "tidytuesday_2023_w10")
ggsave(filename = glue::glue("{path}.png"), width = 10.5, height = 10.5, device = ragg::agg_png, dpi = 300)

