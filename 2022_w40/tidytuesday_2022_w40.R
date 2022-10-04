
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
category_per_year |> 
  ggplot() + 
  geom_col(aes(x = number_of_tags, y = tidytext::reorder_within(category_tags, number_of_tags, year), 
               fill = category_tags), width = .8) + 
  geom_col(aes(x = 2500,y = tidytext::reorder_within(category_tags, number_of_tags, year), 
               fill = category_tags, color = category_tags), width = .8, size = .5,   alpha = .5) + 
  geom_text(aes(x = 100, y = tidytext::reorder_within(category_tags, number_of_tags, year), 
                label = category_tags), hjust = 0) + 
  scale_x_continuous(expand = expansion(mult = 0)) + 
  tidytext::scale_y_reordered(expand = expansion(mult = c(.1, .075))) + 
  facet_wrap(vars(year), scales = "free_y", nrow = 1) + 
  labs(
    title = "Main tech products tags on Product Hunt",
    subtitle = "", 
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
    text = element_text(color = "#111111"),
    plot.title = element_text(size = rel(2.5), hjust = .5, margin = margin(t = .5, b = .5, unit = "cm")),
    strip.text = element_text(face = "bold", size = rel(1.25)),
    axis.text = element_text(size = rel(1.05)),
    axis.text.y = element_blank(), 
    axis.title.x = element_text(size = rel(1.5), margin = margin(t = .5, b = .5,  unit = "cm")),
    panel.grid.minor = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.spacing.x = unit(1, "cm"),
    panel.grid.major.x  = element_line(size = 0.5, color = "white"), 
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(.2, "cm"), 
    axis.line.x = element_line()
  ) 
  
tidytricks::image_colors_summarizer("/home/abdoul-ma/Images/Data Viz/coronavirus-bill-gates-1.png", 5
                                    ) |> 
  scales::show_col()
# Saving ------------------------------------------------------------------
path <- here::here("2022_w40", "tidytuesday_2022_w40")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

