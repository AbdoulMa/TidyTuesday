
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)

# Data Wrangling ----------------------------------------------------------
drugs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv")
drugs <- drugs |>
  rename(
    holder_company_name = marketing_authorisation_holder_company_name
  ) |>
  mutate(
    holder_company_name = case_when(
      str_detect(holder_company_name, "Novartis") ~ "Novartis",
      str_detect(holder_company_name, "Pfizer") ~ "Pfizer",
      str_detect(holder_company_name, "Zoetis") ~ "Zoetis",
      str_detect(holder_company_name, "Zeneca") ~ "AstraZeneca AB",
      str_detect(holder_company_name, "Boehringer") ~ "Boehringer Ingelheim",
      str_detect(holder_company_name, "Merck") ~ "Merck",
      str_detect(holder_company_name, "Teva") ~ "Teva",
      str_detect(holder_company_name, "Intervet") ~ "Intervet",
      str_detect(holder_company_name, "Janssen") ~ "Janssen",
      str_detect(holder_company_name, "Bayer") ~ "Bayer",
      str_detect(holder_company_name, "Sanofi") ~ "Sanofi",
      TRUE ~ holder_company_name
    )
  )

# Select major companies
holders_df <- drugs |>
  filter(
    holder_company_name %in% c(
      "Novartis", "Pfizer", "Zoetis", "AstraZeneca AB",
      "Boehringer Ingelheim", "Merck", "Teva", "Intervet", "Janssen",
      "Bayer", "Sanofi"
    )
  ) |>
  count(holder_company_name, sort = T) |>
  mutate(
    holder_company_name = fct_reorder(holder_company_name, desc(n))
  )
# Graphic -----------------------------------------------------------------
# Drip shape df
r <- .325
m <- (pi * 60.5) / 180 # tangent lines
xo <- 115 / 10
yo <- -0.3

angles <- seq(m, (pi - m), length.out = 100)
drip_df <- tibble(
  x = xo + c(r * cos(angles), 0, r * cos(m)),
  y = yo + c(r * sin(angles), r / cos(m), r * sin(m))
)

# Plot
holders_df |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = n / 10, ymin = 0, ymax = 1), fill = "#add8e6", alpha = .65) +
  geom_text(aes(x = 95 / 10, y = .5, label = holder_company_name), color = "white", family = "Go Condensed Medium", fontface = "bold", size = 8.5, hjust = 1) +
  annotate(geom = "rect", xmin = 0, xmax = 100 / 10, ymin = 0, ymax = 1, color = "white", fill = NA) +
  annotate(geom = "rect", xmin = -5 / 10, xmax = 0, ymin = .35, ymax = .65, color = "white", fill = "white") +
  annotate(geom = "rect", xmin = -7 / 10, xmax = -5 / 10, ymin = .15, ymax = .85, color = "white", fill = "white") +
  annotate(geom = "rect", xmin = 100 / 10, xmax = 101 / 10, ymin = .35, ymax = .65, color = "white", fill = "white") +
  annotate(geom = "rect", xmin = 101 / 10, xmax = 102 / 10, ymin = .4, ymax = .6, color = "white", fill = "white") +
  annotate(geom = "segment", x = 102 / 10, xend = 115 / 10, y = .5, yend = .5, color = "white") +
  geom_polygon(data = drip_df, aes(x, y), fill = "white") +
  ggforce::geom_circle(data = tibble(x = xo, y = yo), aes(x0 = xo, y0 = yo, r = r), color = "white", fill = "#add8e6", alpha = .65, size = .85) +
  geom_text(aes(x = xo, yo, label = n), family = "Inconsolata Semibold", size = 7.5) +
  facet_wrap(vars(holder_company_name), ncol = 1) +
  labs(
    title = "The big pharmaceutical companies",
    subtitle = "Ranking of the numbers of authorizations held in Europe",
    caption = "Tidytuesday Week-11 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      Data from **European Medicines Agency**"
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(color = "white"),
    plot.title = element_text(family = "Go Condensed Medium", face = "bold", size = rel(4.5)),
    plot.subtitle = element_text(family = "Go Book", size = rel(1.75), margin = margin(b = .5, unit = "cm")),
    plot.caption = element_markdown(color = "white", family = "Go Book", size = rel(1.5)),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_blank(),
    panel.spacing.y = unit(.125, "cm"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#1B2129", color = NA),
    plot.margin = margin(t = .5, r = 1.5, b = .5, l = 1.5, unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w11", "tidytuesday_2023_w11")
ggsave(filename = glue::glue("{path}.png"), width = 9, height = 13.5, device = ragg::agg_png, dpi = 300)

