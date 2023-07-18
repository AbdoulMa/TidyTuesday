# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv")

df <- df |>
  filter(
    year == 2021, is.na(iso_code),
    country %in% c("Africa", "Asia", "Middle East (BP)", "South America", "Europe", "Oceania", "North America", "High-income countries", "Low-income countries")
  ) |>
  mutate(
    country = fct_recode(country, "Middle East" = "Middle East (BP)")
  )

region_mix <- df |>
  select(country, "Fossil" = fossil_electricity, "Nuclear Energy" = nuclear_electricity, "Renewable E." = renewables_electricity, "Coal" = coal_electricity, "Gas" = gas_electricity) |>
  pivot_longer(
    cols = -country,
    names_to = "energy_type",
    values_to = "energy_share"
  ) |>
  mutate(
    energy_share = prop.table(energy_share),
    .by = country
  )

order <- c("Fossil", "Nuclear Energy", "Renewable E.", "Coal", "Gas")
base_radar_df <- tibble(
  energy_type = order,
  energy_share = 1 / length(order)
)

# Graphic -----------------------------------------------------------------
(plot <- region_mix |>
  mutate(
    country = str_to_upper(country)
  ) |>
  ggplot(aes(energy_type, energy_share)) +
  geom_polygon(data = base_radar_df, group = 1, fill = "grey") +
  geom_polygon(aes(group = country), fill = "#E90B2E", color = "black") +
  labs(
    title = "World electrical energy mix",
    subtitle = "Shares of different energy types in total electrical energy generated",
    caption = "Tidytuesday Week-23 2023<br/>Data from **Our World In Data**<br/> Inspiration: Thomas Rahlf<br/>Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      "
  ) +
  scale_y_continuous(
    limits = c(0, 0.6)
  ) +
  scale_x_discrete(
    limits = order,
    labels = order,
    expand = expansion(mult = 0.1)
  ) +
  ggmulti::coord_radial(start = pi / 3, clip = "off") +
  facet_wrap(vars(country)) +
  theme_minimal() +
  theme(
    text = element_text(family = "I Sans Medium Pro"),
    axis.text = element_text(color = "#111111"),
    plot.title = element_text(family = "I Sans Semibold Pro", size = rel(3)),
    strip.text = element_text(family = "I Sans Semibold Pro", size = rel(0.95)),
    panel.spacing.x = unit(1, "cm"),
    panel.spacing.y = unit(1, "cm"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = rel(0.95)),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(margin = margin(b = 0.75, unit = "cm")),
    plot.caption = ggtext::element_markdown(hjust = 0.5, size = rel(0.875), margin = margin(t = 0.5, unit = "cm")),
    plot.margin = margin(c(0.5, 0.125, 0.125, 0), unit = "cm"),
    plot.background = element_rect(fill = "#F7F8F9", color = NA),
  )
)

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w23", "tidytuesday_2023_w23")
ggsave(filename = glue::glue("{path}.png"), width = 7.5, height = 8.75, device = ragg::agg_png, dpi = 240)

