
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggimage)
# Data Wrangling ----------------------------------------------------------
big_tech_companies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv")
# Assign companies colors
big_tech_companies$clr <- c("#A2AAAD", "#FF0000", "#FF9900", "#1798c1", "#00BCEB", "#34A853", "#0530AD", "#0068B5", "#0668E1", "#F14F21", "#E50914", "#76B900", "#C74634", "#E31937")
# Companies logos (downloades in folder Logos)
big_tech_companies <- big_tech_companies |>
  mutate(
    lower_stock = tolower(stock_symbol),
    company_logo_path = paste0(here::here("2023/2023_w6/Logos/"), lower_stock, ".png")
  )

big_tech_stock_prices <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv")
big_tech_stock_prices <- big_tech_stock_prices |>
  left_join(big_tech_companies)

# Get company max closing value and min date
companies_logos_df <- big_tech_stock_prices |>
  group_by(stock_symbol, company_logo_path) |>
  summarise(
    first_date = min(date),
    max_adj_close = max(adj_close)
  ) |>
  ungroup()

# Graphic -----------------------------------------------------------------
# Main plot: companies facet
(main_plot <- ggplot() +
  geom_line(data = big_tech_stock_prices, aes(date, adj_close, color = clr, group = stock_symbol), size = .75) +
  geom_ribbon(data = big_tech_stock_prices, aes(date, ymin = 0, ymax = adj_close, fill = clr), alpha = .6) +
  geom_richtext(
    data = companies_logos_df, aes(x = first_date, y = max_adj_close, label = glue::glue("<img src='{company_logo_path}' width='45'/>")),
    hjust = 0,
    fill = NA,
    label.padding = unit(.125, "lines"),
    label.r = unit(0, "lines"),
    label.size = unit(0, "lines")
  ) +
  labs(
    title = "Big Tech companies",
    subtitle = "Evolution of stock prices between 2010 and 2022"
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_cartesian(clip = "off", expand = F) +
  facet_wrap(vars(stock_symbol), scales = "free", nrow = 5) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", face = "bold", family = "Go Black", size = rel(3.5), margin = margin(b = .5, unit = "cm")),
    plot.subtitle = element_text(color = "#444444", family = "Go Bold", size = rel(1.75), margin = margin(b = 1, unit = "cm")),
    strip.text = element_blank(),
    panel.spacing.x = unit(1, "cm"),
    panel.spacing.y = unit(1.5, "cm"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(),
    axis.text.x = element_text(color = "black", family = "Iosevka", size = rel(1.25)),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(.125, "cm"),
    axis.ticks.x = element_line(size = .25),
    plot.background = element_rect(fill = "white", color = NA)
  )
)

# Caption plot
(caption_plot <- ggplot() +
  annotate(
    geom = "richtext", x = 0, y = 0, label = "Tidytuesday Week-06 2023 <br>Data from: Yahoo Finance<br>Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**",
    family = "Iosevka Semibold",
    size = 5.5,
    fill = NA,
    label.size = unit(0, "pt"),
    label.r = unit(0, "pt")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
)

main_plot +
  patchwork::inset_element(caption_plot, left = 0.66, bottom = 0, right = 0.999, top = 0.15) &
  theme(
    plot.margin = margin(c(.25, .25, .25, .25), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w6", "tidytuesday_2023_w6")
ggsave(filename = glue::glue("{path}.png"), width = 11.5, height = 10.5, device = ragg::agg_png, dpi = 300)
