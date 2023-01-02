
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- tibble::tribble(
  ~"food", ~"inflation", ~"img",
  "Beef & Veal", 5.47, "", # https://www.in2013dollars.com/Beef-and-veal/price-inflation/2021-to-2022?amount=20
  "Chuck Roast", 6.53, "", # https://fred.stlouisfed.org/series/APU0300703213
  "Bacon", 9.42, "", # https://www.in2013dollars.com/Bacon-and-related-products/price-inflation
  "Cheese", 8.52, "", # https://www.in2013dollars.com/Cheese-and-related-products/price-inflation
  "Butter", 18.74, "", # https://www.in2013dollars.com/Butter/price-inflation
  "Coffee", 14.06, "", # https://www.in2013dollars.com/Coffee/price-inflation
  "Chicken", 14.63, "", # https://www.in2013dollars.com/Chicken/price-inflation
  "Rice", 10.63, "", # https://www.in2013dollars.com/Rice/price-inflation
  "Flour", 18.33, "", # https://www.in2013dollars.com/Flour-and-prepared-flour-mixes/price-inflation
  "Potatoes", 10.17, "", # https://www.in2013dollars.com/Potatoes/price-inflation
  "Sugar", 9.95, "", # https://www.in2013dollars.com/Sugar-and-sweets/price-inflation
  "Beef Steak", 2.81, "", # https://www.in2013dollars.com/Uncooked-beef-steaks/price-inflation
  "Ground beef", 8.22, "", # https://www.in2013dollars.com/Uncooked-ground-beef/price-inflation
  "Rib Roast", 6.46, "", # https://www.in2013dollars.com/Other-pork-including-roasts,-steaks,-and-ribs/price-inflation
  "Corn Meal", 12.32, "", # https://www.in2013dollars.com/Rice,-pasta,-cornmeal/price-inflation/2021-to-2022?amount=0.74
  "Bread", 10.92, "", # https://www.in2013dollars.com/Bread/price-inflation
  "Ham", 9.14, "", # https://www.in2013dollars.com/Ham,-excluding-canned/price-inflation
  "Pork Chop", 7.23, "", # https://www.officialdata.org/Pork-chops/price-inflation/1965
  "Milk", 13.71, "", # https://www.in2013dollars.com/Milk/price-inflation
  "Eggs", 28.98, "", # https://www.in2013dollars.com/Eggs/price-inflation
  "Fish", 9.10, "", # https://www.in2013dollars.com/Fish-and-seafood/price-inflation
  "All articles", 9.57,"" # https://www.in2013dollars.com/Food/price-inflation
)

# Graphic -----------------------------------------------------------------
# Takes ti
df |> 
  mutate(
    x = glue::glue("<img src='/home/abdoul-ma/Documents/Programmes/R/TidyTuesday/2023/2023_w1/Images/cheese.png' width ='30' /><br/>{food}")
  ) |> 
  ggplot(aes(x =x , y = inflation) ) + 
  geom_point() + 
  theme_minimal() + 
  coord_flip() +
  theme(
    axis.text.y = ggtext::element_markdown()
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023_w1", "tidytuesday_2023_w1")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)
