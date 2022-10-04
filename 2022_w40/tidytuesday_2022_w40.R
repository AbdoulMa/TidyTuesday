
# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------
product_hunt <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv")

interesting_tags <- c("DEVELOPER TOOLS", "WEB APP", "IPHONE", "FINTECH",  "DESIGN TOOLS", "HEALTH AND FITNESS", "SAAS", "ANDROID", "EDUCATION", "OPEN SOURCE") 
category_per_year <-
  product_hunt |> 
  mutate(year = lubridate::year(release_date)) |> 
  filter(year %in% 2017:2021) |> 
  mutate(
    category_tags = str_remove_all(category_tags, "[''\\[\\]]"),
    category_tags = str_split(category_tags, ",")) |> 
  unnest_longer(category_tags) |> 
  mutate(category_tags = str_squish(category_tags)) |> 
  filter(category_tags %in% interesting_tags) |> 
  count(category_tags, year, name = "number_of_tags") |>
  arrange(year, desc(number_of_tags))

# Graphic -----------------------------------------------------------------
tags_colors <- c("DEVELOPER TOOLS" = "#9FA7B6", 
                 "WEB APP" = "#F7334C", 
                 "IPHONE" = "#4A6DCB", 
                 "FINTECH" = "#8C82AF", 
                 "DESIGN TOOLS" = "#EA682C", 
                 "HEALTH AND FITNESS" = "#B6B19E", 
                 "SAAS"=  "#dd5082", 
                 "ANDROID" = "#2C8689", 
                 "EDUCATION"  = "#DBD8DB",
                 "OPEN SOURCE" = "#5C5C5E")

caption <- "Data from **components.one** by way of Data is Plural <br> Tidytuesday Week-40 2022 &bull;  Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid** <br> inspired by Nathan YAU."
category_per_year |> 
  ggplot() + 
  geom_col(aes(x = number_of_tags, y = tidytext::reorder_within(category_tags, number_of_tags, year), 
               fill = category_tags), width = .8) + 
  geom_col(aes(x = 2500, y = tidytext::reorder_within(category_tags, number_of_tags, year), 
               fill = category_tags, color = category_tags), width = .8, size = .5,   alpha = .5) + 
  geom_text(aes(x = 100, y = tidytext::reorder_within(category_tags, number_of_tags, year), 
                label = category_tags), family = "Gotham Narrow", hjust = 0) + 
  scale_x_continuous(expand = expansion(mult = 0)) + 
  tidytext::scale_y_reordered(expand = expansion(mult = c(.1, .075))) + 
  facet_wrap(vars(year), scales = "free_y", nrow = 1) + 
  labs(
    title = toupper("Main tech products tags on Product Hunt"),
    subtitle = "over the last 5 years", 
    caption = caption,
    x = toupper("Number of tags"), 
    y = NULL
  ) + 
  scale_fill_manual(
    values = tags_colors,
    guide = NULL
  ) +
  scale_color_manual(
    values = tags_colors,
    guide = NULL
  ) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "#111111", family = "UEFA Supercup"),
    plot.title = element_text(size = rel(2.75), face = "bold", hjust = .5, margin = margin(t = 1, unit = "cm")),
    plot.subtitle = element_text(size = rel(2.25), face = "bold", color = "grey25", hjust = .5, margin = margin(t = .25, b = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(size = rel(1.5), hjust = .5, margin = margin(t = .5, b = .5, unit = "cm")),
    strip.text = element_text(face = "bold", size = rel(1.75)),
    axis.text = element_text(size = rel(1.05)),
    axis.text.y = element_blank(), 
    axis.title.x = element_text(size = rel(1.5), face = "bold", margin = margin(t = .5, b = .5,  unit = "cm")),
    panel.grid.minor = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.spacing.x = unit(2, "cm"),
    panel.grid.major.x  = element_line(size = 0.5, color = "white"), 
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(.2, "cm"), 
    axis.line.x = element_line(),
    plot.margin = margin(r = 1, l= 1, unit = "cm")
  ) 

# Saving ------------------------------------------------------------------
path <- here::here("2022_w40", "tidytuesday_2022_w40")
ggsave(filename = glue::glue("{path}.pdf"), width = 15, height = 7.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}_polished.png"),
  dpi = 300
)

