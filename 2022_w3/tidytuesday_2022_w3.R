
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(treemapify)
library(grid)

# Data Wrangling ----------------------------------------------------------
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

cocoa_beans_img <- png::readPNG(here::here("2022_w3","chocolate.png"))
cocoa_beans_raster <- rasterGrob(cocoa_beans_img, interpolate = T)
chocolate %>% 
  filter(company_location == "Switzerland") %>% view()
  
  


chocolate %>% 
  count(country_of_bean_origin, sort = T) 
chocolate %>% 
  pull(company_location) %>% 
  unique()

french_manufacturers <- chocolate %>% 
  filter(company_location %in% c("France", "Switzerland", "Italy")) 

(mean <- mean(chocolate$rating))

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

uae_label <- "Even if United Arab Emirates are not the first ones that come to mind when it comes to chocolate, they can rely on the chocolate artisans of the Mirzam brand." %>% str_wrap(25)

france_label <- "Exceptional chocolate factories such as Morin and Bonnat contribute greatly to the quality of French products." %>% str_wrap(25)

switzerland_label <- "Switzerland is certainly not in the lead, but it is the home of chocolate by definition." %>% str_wrap(25)
location_ratings %>% 
  ggplot() + 
  geom_col(aes(x = rating_diff, y = company_location, fill = rating_diff > 0), size = .25, color = "white" ) + 
  geom_text(aes(x = ifelse(rating_diff > 0, -.005, .005), y = company_location, label = company_location, 
                hjust = ifelse(rating_diff > 0, 1, 0)), size = 3) + 
  annotate(geom = "text", x = -.25, y = 15, label = "Less than\n average", lineheight= .9, size = 10, hjust = 1) + 
  annotate(geom = "text", x = .15, y = 35, label = "More than\n average", lineheight= .9, size = 10, hjust = 0) + 
  annotate(geom = "segment", x = .2125, xend = .25, y = 50, yend = 50, linetype = "dotted") +
  annotate(geom = "text", x = .255, y = 50, hjust = 0, label = uae_label, size = 3, lineheight = .95) + 
  annotate(geom = "segment", x = .15, xend = .25, y = 44, yend = 44, linetype = "dotted") +
  annotate(geom = "text", x = .255, y = 44, hjust = 0, label = switzerland_label, size = 3, lineheight = .95) + 
  annotate(geom = "segment", x = -.1, xend = -.175, y = 35, yend = 35, linetype = "dotted") +
  annotate(geom = "text", x = -.18, y = 35, hjust = 1, label = france_label, size = 3, lineheight = .95) + 
  annotation_custom(cocoa_beans_raster, xmin = -.5, xmax = -.2, ymin = 40, ymax = 50) + 
  labs(
    subtitle = "Average rating per manufacturers place"
  ) + 
  scale_x_continuous(
    sec.axis = sec_axis(trans = ~.,
                        breaks = seq(-.4,.2, by = .2),
                        labels = c(-.4,-.2,"Average\n rating",.2)
                        ),
    labels = ~ifelse(.x == 0, "Average\n rating", .x)
  ) + 
  scale_y_discrete(expand = c(0, 0)) + 
  coord_cartesian(expand = F, clip = "off") + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.y =  element_blank(), 
    axis.ticks.x = element_line(size = .25), 
    axis.ticks.length.x = unit(.25, "cm")
  )

# TreeMap ----
# https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/
chocolate_per_origin <- chocolate %>% 
  filter(country_of_bean_origin != "Blend") %>% 
  count(company_location, country_of_bean_origin)

chocolate_per_origin %>% 
  ggplot(aes(area = n, label =country_of_bean_origin, subgroup = company_location)) + 
  geom_treemap(size = .5, fill = "#29130C") + 
  geom_treemap_subgroup_border(color = "black") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour = "white",
                             fontface = "italic", min.size = 0) + 
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) + 
  labs(
    subtitle = "Share of cocoa beans imports"
  )




# Other options ---- 

# location_ratings %>% 
#   pivot_longer(
#     cols = c(min_rating, max_rating, avg_rating), 
#     names_to = "x_variation_name",
#     values_to = "x_variation"
#   ) %>% 
#   ggplot(aes(x = x_variation, y = company_location, group = company_location)) + 
#   geom_line() +
#   geom_point(aes(color = x_variation_name), size = 3) + 
#   scale_y_discrete(
#     position = "right"
#   ) + 
#   coord_cartesian(expand = F, clip = "off") + 
#   theme_minimal() + 
#   theme(
#     axis.text.y = element_text(face = "bold"),
#     panel.grid.major.y = element_blank()
#   ) 


# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w3", "tidytuesday_2022_w3")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 12, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

