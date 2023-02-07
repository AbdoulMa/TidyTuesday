
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')

images_positions <- big_tech_stock_prices |> 
  group_by(stock_symbol) |> 
  summarise(
    max_adj_close = max(adj_close)
  ) |> 
  ungroup() |> 
  mutate(
    x_pos = ymd("2010-02-10")
  )


 # |> 
  # mutate(year = lubridate::year(date)) |> 
  # filter(year == 2022) |> 
  ggplot() + 
  geom_line(data = big_tech_stock_prices, aes(date, adj_close, color = stock_symbol, group = stock_symbol)) + 
  geom_text(data = images_positions, aes(x = x_pos, max_adj_close, label= stock_symbol )) +
  facet_wrap(vars(stock_symbol),scales = "free_y", nrow = 2)

# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w6", "tidytuesday_2023_w6")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

