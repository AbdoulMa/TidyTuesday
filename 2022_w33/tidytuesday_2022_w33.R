
# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2022, week = 33)
characters <- tuesdata$characters
psych_stats <- tuesdata$psych_stats

unique(characters$uni_name)

psych_stats |> 
  group_by(uni_name) |> 
  summarise(
    n_chars = n_distinct(char_id),
    .groups = "drop"
  ) |> view()


personalities_selected <-  c("charismatic", "eloquent", "extrovert", "demonic", "high IQ", "reasonable", "sensitive", "social", "technophile", "workaholic")

ozark_characters <- characters |> 
  filter(uni_name =="Ozark") |> 
  mutate(name = fct_reorder(name, desc(notability)))


ozark_psych_stats <- psych_stats |> 
  filter(uni_name == "Ozark", personality %in% personalities_selected)


ozark_overall_stats <- ozark_psych_stats |> 
  bind_rows(data.frame(
    char_name = ozark_characters$name,
    personality = "notability",
    avg_rating = ozark_characters$notability
  )) |> 
  select(char_name, personality, avg_rating) |> 
  mutate(char_name = fct_relevel(char_name, levels = levels(ozark_characters$name)),
         personality = fct_relevel(personality, levels = c("notability"), sort(personalities_selected)),
         avg_rating = round(avg_rating)) |> 
  arrange(char_name, personality)
  

# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w33", "tidytuesday_2022_w33")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

