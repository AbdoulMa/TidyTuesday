
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
afrisenti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv")
languages <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv")

languages <- languages |>
  mutate(
    language = case_when(
      language == "Algerian Arabic/Darja" ~ "Darja",
      language == "Moroccan Arabic/Darija" ~ "Darija",
      language == "Nigerian Pidgin" ~ "Pidgin",
      language == "Mozambican Portuguese" ~ "Mozambican",
      TRUE ~ language
    )
  )

afrisenti <- afrisenti |>
  filter(label != "neutral") |>
  left_join(languages)

positive_languages <- afrisenti |>
  group_by(language) |>
  summarise(pos_rate = sum(label == "positive") / n()) |>
  ungroup() |>
  mutate(
    language = fct_reorder(language, pos_rate)
  )

languages_levels <- levels(positive_languages$language)

# Graphic -----------------------------------------------------------------
df_afrisenti <- afrisenti |>
  mutate(language = fct_relevel(language, languages_levels)) |>
  group_by(language, label) |>
  summarize(count = n()) |>
  mutate(
    label.count = sum(count),
    prop = count / sum(count)
  ) |>
  ungroup()

df_afrisenti |>
  ggplot(aes(x = language, y = prop, width = label.count, fill = label)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  geom_text(aes(x = 0.125, y = .01, label = language, size = label.count), family = "TT Norms", fontface = "bold", hjust = 0, vjust = .025, angle = 90, stat = "unique") +
  labs(
    title = "Sentiments analysis in African languages",
    subtitle = "Percentages of tweets in 14 languages, and proportions of <span style='color:#E79213;'>**positive**</span> or <span style='color:#E73835;'>**negative**</span> labels assigned by native speakers.",
    caption = "Tidytuesday Week-09 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
    Data from **AfriSenti** via **@Shmuhammadd**"
  ) +
  facet_grid(cols = vars(language), scales = "free_x", space = "free_x") +
  coord_cartesian(clip = "off") +
  scale_fill_manual(
    values = c(
      "positive" = "#E79213",
      "negative" = "#E73835"
    ),
    guide = "none"
  ) +
  scale_size(
    range = c(2.75, 15),
    guide = "none"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "TT Norms", color = "white", face = "bold", size = rel(3.75), margin = margin(t = .35, b = .35, unit = "cm")),
    plot.subtitle = element_markdown(family = "TT Norms", color = "white", size = rel(1.45)),
    plot.caption = element_markdown(family = "TT Norms", color = "white", size = rel(1.25)),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#333333", color = NA),
    plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w9", "tidytuesday_2023_w9")
ggsave(filename = glue::glue("{path}.png"), width = 9 * 9 / 6, height = 9, dpi = 300, device = ragg::agg_png)
