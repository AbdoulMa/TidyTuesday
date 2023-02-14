
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv")
range(df$release_year)
final_df <- df |>
  pivot_longer(
    cols = c("character_1_gender", "character_2_gender"),
    names_to = "gender",
    values_to = "gender_value"
  ) |>
  filter(gender_value == "woman") |>
  mutate(
    age = case_when(
      str_detect(gender, "1") ~ actor_1_age,
      TRUE ~ actor_2_age
    )
  ) |>
  select(release_year, age) |>
  mutate(
    age = cut(age, breaks = c(-Inf, 24, 34, 49, Inf), labels = c("less than 25", "25 to 34", "35 to 49", "50 or more"), include.lowest = T),
    release_decade = paste0((release_year %/% 10) * 10, "s")
  ) |>
  count(release_decade, age, name = "nb_women", .drop = T) |>
  complete(release_decade, age, fill = list(nb_women = 0)) |>
  group_by(release_decade) |>
  mutate(
    prop = prop.table(nb_women)
  ) |>
  ungroup() |>
  arrange(release_decade)

# Graphic -----------------------------------------------------------------
final_df |>
  ggplot(aes(fill = age)) +
  annotate(geom = "rect", xmin = 0, xmax = .75, ymin = -.15, ymax = .15, fill = "#cecece", alpha = .75) +
  geom_rect(aes(xmax = prop), xmin = 0, ymin = -.15, ymax = .15) +
  geom_text(
    data = filter(final_df, prop != 0), aes(x = .1, y = 0, color = after_scale(prismatic::best_contrast(fill)), label = glue::glue("{round(prop*100, 0)}%")),
    family = "Iosevka Semibold", size = 6.5, hjust = 0
  ) +
  labs(
    title = "Representation of women by age in films",
    subtitle = "Share of women by age groups playing romantic roles in Hollywood\nbetween 1935 & 2022",
    caption = "Tidytuesday Week-07 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
    Data from **Hollywood Age Gap** via **Data Is Plural**"
  ) +
  facet_grid(release_decade ~ age, switch = "y") +
  coord_equal() +
  scale_fill_manual(
    values = c("#54AC5E", "#5B43CD", "#F5D04E", "#C9439A"),
    guide = "none"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(family = "Go Bold", size = rel(2.5), margin = margin(b = .5, unit = "cm")),
    plot.subtitle = element_text(family = "NY Bold Italic", size = rel(1.95), margin = margin(b = .5, unit = "cm")),
    plot.caption = element_markdown(family = "Go Book", size = rel(1.25), lineheight = .85, margin(t = 1, unit = "cm")),
    strip.text = element_text(family = "Go Bold"),
    strip.text.x = element_text(hjust = 1, size = rel(1.75)),
    strip.text.y.left = element_text(angle = 0, size = rel(2)),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(.75, .25, .75, .25, unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w7", "tidytuesday_2023_w7")
ggsave(filename = glue::glue("{path}.png"), width = 9, height = 10.5, device = ragg::agg_png, dpi = 300)
