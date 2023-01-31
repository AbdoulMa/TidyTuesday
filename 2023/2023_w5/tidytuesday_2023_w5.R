
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)


# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv")
  
# Graphic -----------------------------------------------------------------

df |> 
  ggplot() + 
  geom_point(aes(location_long, location_lat))

bi_weeks_df <-  df |> 
  mutate(
    yearw = as.integer(format(timestamp, '%U')),
    biweek = (yearw %/% 2) * 2,
    # %u :  week day
    # %U : year week 
    biweek = as.Date(paste(2017, biweek, 1, sep = "-"), format = "%Y-%U-%u"),
    .after = "timestamp"
  ) |> 
  # mutate(month = lubridate::month(timestamp), 
  #        .after = "timestamp") |> 
  count(biweek)
  # mutate(
  #   tag_id = fct_lump(tag_id, n = 10)
  # ) |> 
  # filter(tag_id != "Other") |> 
  # ggplot() + 
  # geom_bar(aes(month, fill = tag_id))

# as.Date(paste(2017, 22, 1, sep="-"), "%Y-%U-%u")
glimpse(bi_weeks_df)
bi_weeks_df |> 
  ggplot() + 
  geom_col(aes(biweek, n)) + 
  geom_text(aes(biweek, y = 0, label = biweek), angle = 90)

lubridate::rollbackward()
# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w5", "tidytuesday_2023_w5")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

