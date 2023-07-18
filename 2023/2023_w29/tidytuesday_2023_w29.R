# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
detectors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv")

detectors_acc_df <- detectors |>
  filter(!is.na(native)) |>
  summarise(
    accuracy = sum(kind == .pred_class) / n(),
    .by = c(detector, native)
  ) |>
  arrange(detector)

detectors_logos_df <- tribble(
  ~detector, ~logo,
  "ZeroGPT", "zero_gpt.png",
  "Crossplag", "cross_plag.png",
  "GPTZero", "gpt_zero.png",
  "HFOpenAI", "hf.png",
  "Sapling", "sapling.png",
  "Quil", "quill.png",
  "OriginalityAI", "originality.png"
) |>
  mutate(
    logo = here::here("2023", "2023_w29", "detectors_logos", logo)
  )


# Graphic -----------------------------------------------------------------

best_no_native_detecors <- detectors_acc_df |>
  filter(native == "No") |>
  arrange(desc(accuracy)) |>
  pull(detector)

title <- "**Me**: Hey TidyTuesdayGPT, can you visualize me the accuracies of the detectors<br/>
whether the essays are written by a native English writer or not?"

subtitle <- "**TidytuesdayGPT**: Of course, here is a fairly quick visualization of the accuracy bias of  detectors depending <br/>
writers proficiency in  English, <span style='color:#FFC44E; '>**Native**</span> or <span style='color: #EA0054; '>**Not native**</span>."


caption <- "Tidytuesday Week-29 2023<br/>Data from **Simon Couch**<br/>Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>"
detectors_acc_df |>
  left_join(detectors_logos_df, by = join_by("detector" == "detector")) |>
  mutate(
    fancy_label = glue::glue("<img src='{logo}' width='50'/><br/><span>{detector}</span>"),
    detector = fct_relevel(detector, best_no_native_detecors) # ,
  ) |>
  arrange(detector) |>
  mutate(
    fancy_label = fct_inorder(fancy_label)
  ) |>
  ggplot() +
  geom_col(aes(native, accuracy, fill = native), position = "dodge") +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  geom_text(
    aes(native, accuracy, label = scales::label_percent()(round(accuracy, 2))),
    family = "DecimaMonoPro",
    size = rel(5.25),
    position = "dodge",
    vjust = -0.5
  ) +
  scale_y_continuous(
    name = "Accuracy",
    labels = scales::percent_format(),
    expand = expansion(mult = c(0.0, 0.15)),
    breaks = seq(0, 0.75, by = 0.25)
  ) +
  scale_fill_manual(
    values = c(
      "Yes" = "#FFC44E",
      "No" = "#EA0054"
    ),
    guide = "none"
  ) +
  facet_wrap(vars(fancy_label), nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(family = "Go Book"),
    panel.spacing.x = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.75),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black", size = rel(1.5), family = "DecimaMonoPro"),
    axis.title.y = element_text(angle = 0, size = rel(1.25), family = "DecimaMonoPro", vjust = 1, margin = margin(r = -50, unit = "pt")),
    strip.text = element_markdown(size = rel(1.125), face = "bold", vjust = 0),
    plot.title = element_markdown(size = rel(1.5), lineheight = unit(1.25, "cm"), hjust = 0.5, margin = margin(t = 0.75, b = 0.75, unit = "cm")),
    plot.subtitle = element_markdown(size = rel(1.5), lineheight = unit(1.25, "cm"), hjust = 0.5, margin = margin(b = 0.75, unit = "cm")),
    plot.caption = element_markdown(size = rel(1.125), lineheight = unit(1.125, "cm"), hjust = 0.5, margin = margin(t = 0.5, unit = "cm")),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin = margin(c(0.5, 1, 0.25, 1), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w29", "tidytuesday_2023_w29")
ggsave(filename = glue::glue("{path}.png"), width = 13.5, height = 9, dpi = 300, device = ragg::agg_png)
