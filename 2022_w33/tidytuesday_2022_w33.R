
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(waffle)

# Data Wrangling ----------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 33)
characters <- tuesdata$characters
psych_stats <- tuesdata$psych_stats

# Some personality traits selection
personalities_selected <-  c("charismatic", "eloquent", "extrovert", "demonic", "high IQ", "reasonable", "sensitive", "social", "technophile", "workaholic")

# Reorder names by notability score
ozark_characters <- characters |> 
  filter(uni_name =="Ozark") |> 
  mutate(
    name = gsub(" ","\n",name),
    name = fct_reorder(name, desc(notability)))


ozark_psych_stats <- psych_stats |> 
  filter(uni_name == "Ozark", personality %in% personalities_selected) |> 
  mutate(char_name = gsub(" ","\n", char_name))
  
# Append notability score to other personality traits score 
(ozark_overall_stats <- ozark_psych_stats |> 
    bind_rows(data.frame(
      char_name = ozark_characters$name,
      personality = "notability",
      avg_rating = ozark_characters$notability
    )) |> 
    select(char_name, personality, avg_rating) |> 
    mutate(avg_rating = round(avg_rating), 
           remaining_rating = 100 - avg_rating
    ) |> 
    pivot_longer(
      cols = c("avg_rating", "remaining_rating"), 
      names_to = "rating_type", 
      values_to = "rating"
    ) |> 
    mutate(
      personality_fill = ifelse(rating_type == "avg_rating", as.character(personality), "remaining")
    )
)

# Graphic -----------------------------------------------------------------
# Function to upper the first letter of personality trait
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

characters_names <- unique(ozark_overall_stats$char_name)
personalities <- unique(ozark_overall_stats$personality)

# Complete dataset to fill missing personalities score for characters
ozark_overall_stats <- ozark_overall_stats |> 
  complete(char_name = characters_names, personality = personalities) |> 
  replace_na(list(rating = 100, personality_fill = "no_data"))

caption <- "Data from **Open-Source Psychometrics Project courtesy of Tanya Shapiro** <br> Tidytuesday Week-33 2022 &bull;  Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
final_plot <- ozark_overall_stats |> 
  mutate(char_name = fct_relevel(gsub(" ","\n",char_name), levels = levels(ozark_characters$name)),
         personality = fct_relevel(firstup(personality), levels = firstup(c("notability", sort(personalities_selected))))) |> 
  ggplot(aes(fill = personality_fill, values = rating)) + 
  geom_waffle(color = "#111111", size = .01, n_rows = 10, flip = T) +
  scale_fill_manual(
    values = c("notability" = "#E22E2E",
               "charismatic"  = "#DF8B00",
               "demonic" = "#E3E300",
               "eloquent" = "#00B100",
               "extrovert" = "#00BDBD",
               "high IQ" = "#256EB3",
               "reasonable" = "#C161EB",
               "sensitive" = "#CAB9CA",
               "social" = "#E3E300",
               "technophile" = "#7D747C",
               "workaholic" = "#005800",
               "remaining" = "#FFFFFF",
               "no_data" = "#111111")
  ) + 
  labs(
    title = "OZARK", 
    subtitle = "Personality traits of the main characters", 
    caption = caption
  ) + 
  coord_equal(expand = F) + 
  facet_grid(char_name ~ personality, switch = "y") + 
  theme_minimal() +
  theme(
    text = element_text(color = "#FFFFFF"),
    plot.background = element_rect(fill = "#111111", color = NA),
    plot.title = element_text(family = "MercurySSm-Bold", size = rel(3.5), hjust = .5, margin = margin(b = 0.5, unit = "cm")),
    plot.subtitle = element_text(family = "Mercury Display", size = rel(2.5), hjust = .5, margin = margin(b = 1.25, unit = "cm")),
    plot.caption = ggtext::element_markdown(family = "Gotham Medium", color = "#FFFFFF", size = rel(.9), hjust = .5),
    plot.margin = margin(c(0.5, 0.5, 0.5, 0.5), unit = "cm"),
    strip.placement = "outside",
    strip.text = element_text(family = "Gotham Condensed", color = "#FFFFFF"),
    strip.text.y.left = element_text(angle = 0, size = rel(2)),
    strip.text.x = element_text( size = rel(1.5), margin = margin(b = .5, unit = "cm")),
    panel.grid = element_blank(),
    panel.spacing.x = unit(.5, "cm"),
    panel.spacing.y = unit(.2, "cm"),
    panel.border = element_rect(fill = NA, color ="white", size = .5),
    axis.text = element_blank(),
    legend.position = "none"
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w33", "tidytuesday_2022_w33")
ggsave(filename = glue::glue("{path}.pdf"), plot = final_plot, width = 12.5, height = 12.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

# Addtitional annotations with Illustrator

