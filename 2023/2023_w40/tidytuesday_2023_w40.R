# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)

# Data Wrangling ----------------------------------------------------------
grants <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv")

top_2023_grants <- grants |>
  count(year = format(posted_date, "%Y"), agency_name, wt = estimated_funding, name = "total") |>
  arrange(desc(year), desc(total)) |>
  slice(1:10)

top_2023_grants <- top_2023_grants |>
  mutate(
    agency_name = fct_recode(agency_name,
      "Federal Railroad Administration" = "DOT - Federal Railroad Administration",
      "Federal Highway Administration" = "DOT Federal Highway Administration",
      "Office of the Under Secretary for Policy" = "69A345 Office of the Under Secretary for Policy",
      "Department of Homeland Security" = "Department of Homeland Security - FEMA"
    )
  )

top_2023_grants <- top_2023_grants |>
  mutate(total_scaled = total / 1e9)

total_range <- range(top_2023_grants$total_scaled)

top_grants_final_df <- top_2023_grants |>
  mutate(total_scaled = scales::rescale(total_scaled, to = c(1, 6), from = total_range)) |>
  arrange(total) |>
  mutate(
    row_num = row_number(),
    .before = 1
  ) |>
  mutate(
    total_lag = lag(total_scaled, default = 0),
    agency_y = cumsum(total_lag) + (row_num - 1) * 0.25 + total_scaled / 2
  )


# Graphic -----------------------------------------------------------------
top_grants_final_df |>
  ggplot() +
  geom_segment(aes(x = 0, xend = 3.85, y = agency_y, yend = agency_y),
    linetype = "dashed", linewidth = 0.25, color = "#FFFFFF"
  ) +
  geom_circle(aes(x0 = 0, y0 = agency_y, r = total_scaled / 2), fill = "#EB494F", color = NA) +
  geom_text(aes(x = 0, y = agency_y, label = scales::dollar(total / 1e9, suffix = "\nBn"), size = total_scaled),
    family = "Inconsolata",
    fontface = "bold",
    color = "#FFFFFF",
    lineheight = 0.85
  ) +
  geom_text(aes(x = 4, y = agency_y, label = str_wrap(agency_name, 25)),
    hjust = 0, color = "#FFFFFF",
    family = "M G1 Semibold",
    lineheight = 0.85
  ) +
  labs(
    title = "Top 10 agencies",
    subtitle = "funded by US Government in 2023",
    caption = "Data from **grants.gov**<br/>**Tidytuesday Week-40 2023**<br/>Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**.",
  ) +
  scale_size(
    range = c(2, 8),
    guide = "none"
  ) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(
    plot.title = element_text(color = "#FFFFFF", family = "M G1 Bold", hjust = 0, size = rel(3.25)),
    plot.subtitle = element_text(color = "#FFFFFF", family = "M G1 Semibold", hjust = 0, size = rel(1.25)),
    plot.caption = element_markdown(color = "#FFFFFF", family = "Inconsolata", hjust = 0, size = rel(1.125)),
    plot.background = element_rect(fill = "#111111", color = NA),
    plot.margin = margin(c(0.5, 1, 0.5, -4.5), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w40", "tidytuesday_2023_w40")
ggsave(filename = glue::glue("{path}.png"), width = 6, height = 9, device = ragg::agg_png, dpi = 240)
