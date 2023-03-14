
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

drugs <- drugs |>
  rename(
    holder_company_name = marketing_authorisation_holder_company_name
  ) |> 
  mutate(
    holder_company_name = case_when(
      str_detect(holder_company_name, "Novartis") ~ "Novartis",
      str_detect(holder_company_name, "Pfizer") ~ "Pfizer",
      str_detect(holder_company_name, "Zoetis") ~ "Zoetis",
      str_detect(holder_company_name, "Zeneca") ~ "AstraZeneca AB",
      str_detect(holder_company_name, "Boehringer") ~ "Boehringer Ingelheim",
      str_detect(holder_company_name, "Merck") ~ "Merck",
      str_detect(holder_company_name, "Teva") ~ "Teva",
      str_detect(holder_company_name, "Intervet") ~ "Intervet",
      str_detect(holder_company_name, "Janssen") ~ "Janssen",
      str_detect(holder_company_name, "Bayer") ~ "Bayer",
      str_detect(holder_company_name, "Sanofi") ~ "Sanofi",
      TRUE ~ holder_company_name
    )
  )

holders_df <- drugs |>  
  filter(
    holder_company_name %in% c("Novartis", "Pfizer", "Zoetis", "AstraZeneca AB",
                               "Boehringer Ingelheim", "Merck", "Teva", "Intervet", "Janssen",
                               "Bayer", "Sanofi")) |> 
    count(holder_company_name, sort = T)
# Graphic -----------------------------------------------------------------
holders_df |>  
  
  mutate(
    holder_company_name = fct_reorder(holder_company_name, desc(n))
  ) |> 
  ggplot() + 
  geom_rect(aes(xmin = 0, xmax = n, ymin = 0, ymax = 1)) + 
  geom_text(aes(x = 95, y = .5, label = holder_company_name), hjust = 1) + 
  annotate( geom = "rect", xmin = 0, xmax = 100, ymin = 0, ymax = 1, color = "black", fill = NA) + 
  annotate( geom = "rect", xmin = -5, xmax = 0, ymin = .35, ymax = .65, color = "black", fill = "black") + 
  annotate( geom = "rect", xmin = -7, xmax = -5, ymin = .15, ymax = .85, color = "black", fill = "black") + 
  annotate( geom = "rect", xmin = 100, xmax = 101, ymin = .35, ymax = .65, color = "black", fill = "black") + 
  annotate( geom = "rect", xmin = 101, xmax = 102, ymin = .4, ymax = .6, color = "black", fill = "black") + 
  annotate( geom = "segment", x = 102, xend = 115, y = .5, yend = .5 , color = "black") + 
  facet_wrap(vars(holder_company_name), ncol = 1, strip.position = "left") + 
  theme_minimal() + 
  theme(
    panel.spacing.y = unit(1, "cm"), 
    panel.grid = element_blank()
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w11", "tidytuesday_2023_w11")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

