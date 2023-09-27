# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
rk_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv")

rk_df <- rk_df |>
  mutate(
    ep = paste0(ifelse(Episode == 1, "Ep/", ""), Episode),
    season = paste0("Season ", Season),
    ep = fct_inorder(ep)
  ) |>
  select(season, ep, count = F_count_RK)


# Graphic -----------------------------------------------------------------
(plot <- rk_df |>
  rowwise() |>
  mutate(
    fancy_label = paste0(rep("F*CK", count), collapse = "\n")
  ) |>
  ggplot() +
  geom_text(aes(label = fancy_label, color = count),
    x = 0, y = 0,
    family = "UEFA SuperCup", fontface = "bold", vjust = 0, size = 3.75, lineheight = 1
  ) +
  labs(
    title = toupper("Roy Kent's F*cking Counts"),
    caption = "Data from **Deepsha Menghani**<br/>**Tidytuesday Week-39 2023**<br/>Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**."
  ) +
  facet_grid(season ~ ep, switch = "y", scales = "free_y") +
  scale_x_continuous(
    expand = expansion(mult = c(1.5, .35))
  ) +
  viridis::scale_color_viridis(
    option = "viridis", discrete = FALSE, direction = -1,
    guide = "none", begin = 0.15
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    text = element_text(color = "#FFFFFF"),
    panel.spacing.y = unit(1.5, "cm"),
    plot.title = element_text(size = rel(3), family = "UEFA SuperCup", face = "bold", hjust = 0.5, margin = margin(b = 1, unit = "cm")),
    plot.caption = element_markdown(family = "UEFA SuperCup", size = rel(1.25), lineheight = unit(1.125, "cm"), hjust = 0.5, margin = margin(t = 0.5, unit = "cm")),
    strip.text.x = element_text(color = "#FFFFFF", family = "UEFA SuperCup", size = rel(1.75)),
    strip.text.y.left = element_text(angle = 0, color = "#FFFFFF", family = "M G2 Bold", vjust = 0.0325, size = rel(2.25)),
    plot.background = element_rect(fill = "#111111", colour = NA),
    plot.margin = margin(c(1, 1, 1, 1), unit = "cm")
  )
)

roy_image <- here::here("2023/2023_w39/roy_coach.png")
cowplot::ggdraw(plot) +
  cowplot::draw_image(roy_image, x = 0.05, y = 0.99, hjust = 0, vjust = 1, width = 0.2, height = 0.3) +
  cowplot::draw_label(
    x = 0.15, y = 0.775, vjust = 1, label = "\"We are in a shit fucking mood\nbecause we never fucking win\n and it sucks fucking shit.\"",
    fontfamily = "M G1 Semibold Italic",
    fontface = "italic", size = 16.5, color = "#FFFFFF"
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w39", "tidytuesday_2023_w39")
ggsave(filename = glue::glue("{path}.png"), width = 12.5, height = 15, dpi = 240, device = ragg::agg_png)
