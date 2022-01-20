
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(treemapify)
library(grid)
library(patchwork)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Compute mean 
(mean <- mean(chocolate$rating))

# Locations ratings
location_ratings <- chocolate  %>%
  group_by(company_location) %>% 
  summarise(
    n = n(),
    min_rating = min(rating),
    max_rating = max(rating),
    avg_rating = mean(rating, na.rm = T)
  ) %>% 
  mutate(
    company_location = fct_reorder(company_location, avg_rating)
  ) %>% 
  filter(n > 3) %>% 
  mutate(rating_diff = avg_rating - mean)

# Chocolate per bean origin
chocolate_per_origin <- chocolate %>% 
  filter(country_of_bean_origin != "Blend") %>% 
  count(company_location, country_of_bean_origin)

# Graphic -----------------------------------------------------------------

# Average ratings plot ---- 
# Image reading 
cocoa_beans_img <- png::readPNG(here::here("2022_w3","chocolate.png"))
cocoa_beans_raster <- rasterGrob(cocoa_beans_img, interpolate = T)

uae_label <- "Even if United Arab Emirates are not the first ones that come to mind when it comes to chocolate, they can rely on the chocolate artisans of the Mirzam brand." %>% str_wrap(25)
france_label <- "Exceptional chocolate factories such as Morin and Bonnat contribute greatly to the quality of French products." %>% str_wrap(25)
switzerland_label <- "Switzerland is certainly not in the lead, but it is the home of chocolate by definition." %>% str_wrap(25)

(location_ratings_plot <- location_ratings %>% 
    ggplot() + 
    geom_col(aes(x = rating_diff, y = company_location, fill = rating_diff > 0), size = .25, color = "white" ) + 
    geom_text(aes(x = ifelse(rating_diff > 0, -.005, .005), y = company_location, label = company_location, 
                  hjust = ifelse(rating_diff > 0, 1, 0)), 
              family = "Go Condensed Bold", size = 5) + 
    annotate(geom = "text", x = -.25, y = 15, label = "Less than\n average", family = "G Bold", lineheight= .9, size = 10, hjust = 1) + 
    annotate(geom = "text", x = .15, y = 35, label = "More than\n average", family = "G Bold", lineheight= .9, size = 10, hjust = 0) + 
    annotate(geom = "segment", x = .2125, xend = .25, y = 50, yend = 50, linetype = "dotted") +
    annotate(geom = "text", x = .255, y = 50, hjust = 0, label = uae_label, family = "M G1 Bold Italic", size = 4, lineheight = .95) + 
    annotate(geom = "segment", x = .15, xend = .25, y = 44, yend = 44, linetype = "dotted") +
    annotate(geom = "text", x = .255, y = 44, hjust = 0, label = switzerland_label, family = "M G1 Bold Italic", size = 4, lineheight = .95) + 
    annotate(geom = "segment", x = -.05, xend = -.175, y = 35, yend = 35, linetype = "dotted") +
    annotate(geom = "text", x = -.18, y = 35, hjust = 1, label = france_label, family = "M G1 Bold Italic", size = 4, lineheight = .95) + 
    annotation_custom(cocoa_beans_raster, xmin = -.5, xmax = -.2, ymin = 40, ymax = 50) + 
    labs(subtitle = "Average rating per manufacturer place") + 
    scale_x_continuous(
      expand = expansion(add = c(0,.2)),
      sec.axis = sec_axis(trans = ~.,
                          breaks = seq(-.4,.2, by = .2),
                          labels = c(-.4,-.2,"AVERAGE\n RATING",.2)
      ),
      breaks = seq(-.4,.2, by = .2),
      labels = ~ifelse(.x == 0, "AVERAGE\n RATING", .x)
    ) + 
    scale_y_discrete(expand = c(.025, .025)) + 
    scale_fill_manual(
      values = c(
        "TRUE" = "#E11B4D",
        "FALSE" = "#8456BA"
        
      )
    ) + 
    coord_cartesian(clip = "off") +  
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      axis.text.y =  element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(family = "Inconsolata", face = "bold", size =rel(1.75), color = "#111111"),
      axis.ticks.x = element_line(size = .25), 
      axis.ticks.length.x = unit(.25, "cm")
    ) 
)

# TreeMap of beans origins ----
(origins_plot <- chocolate_per_origin %>% 
   ggplot(aes(area = n, label =country_of_bean_origin, subgroup = company_location)) + 
   geom_treemap(size = .5, fill = "#6D4434") + 
   geom_treemap_subgroup_border(color = "black") +
   geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.55, colour = "white",
                              family = "M G2 Bold",
                              fontface = "italic", min.size = 0) + 
   geom_treemap_text(colour = "white", family = "Go Condensed Bold", place = "topleft", reflow = T) + 
   labs(subtitle = "Shares of cocoa beans imports per manufacturer place")
)

# Plots combine ---- 
(location_ratings_plot + origins_plot ) +
  plot_annotation(
    title = "CHOCOLATE", 
    caption = "Data from Flavors of Cacao by way of **Georgios and Kelsey**.<br>
    Image credits to healthhubqld.com <br>
      Tidytuesday Week-3 2022 &bull; Abdoul ISSA BIDA &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) &
  theme(
    text = element_text(color = "#111111"),
    plot.title = element_text(hjust = .5, size =rel(3.5), family = "Go Black"),
    plot.subtitle = element_text(hjust = .5, size =rel(2), family = "M G2 Bold"),
    plot.caption = element_markdown(size = rel(1.5),family = "Go Bold", hjust = .5, margin = margin(t = 10)), 
    plot.margin = margin(t =1, r = .5, b =.5, unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2022_w3", "tidytuesday_2022_w3")
ggsave(filename = glue::glue("{path}.png"), width = 24, height = 15, device = ragg::agg_png, dpi = 640)

