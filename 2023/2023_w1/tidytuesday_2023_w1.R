# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- tibble::tribble(
  ~"food", ~"inflation", ~"img",
  "Beef & Veal", 5.47, "beef_veal.png", # https://www.in2013dollars.com/Beef-and-veal/price-inflation/2021-to-2022?amount=20
  "Chuck Roast", 6.53, "chuck_roast.png", # https://fred.stlouisfed.org/series/APU0300703213
  "Bacon", 9.42, "bacon.png", # https://www.in2013dollars.com/Bacon-and-related-products/price-inflation
  "Cheese", 8.52, "cheese.png", # https://www.in2013dollars.com/Cheese-and-related-products/price-inflation
  "Butter", 18.74, "butter.png", # https://www.in2013dollars.com/Butter/price-inflation
  "Coffee", 14.06, "coffee.png", # https://www.in2013dollars.com/Coffee/price-inflation
  "Chicken", 14.63, "chicken.png", # https://www.in2013dollars.com/Chicken/price-inflation
  "Rice", 10.63, "rice.png", # https://www.in2013dollars.com/Rice/price-inflation
  "Flour", 18.33, "flour.png", # https://www.in2013dollars.com/Flour-and-prepared-flour-mixes/price-inflation
  "Potatoes", 10.17, "potatoes.png", # https://www.in2013dollars.com/Potatoes/price-inflation
  "Sugar", 9.95, "sugar.png", # https://www.in2013dollars.com/Sugar-and-sweets/price-inflation
  "Beef Steak", 2.81, "beef_steak.png", # https://www.in2013dollars.com/Uncooked-beef-steaks/price-inflation
  "Ground beef", 8.22, "ground_beef.png", # https://www.in2013dollars.com/Uncooked-ground-beef/price-inflation
  "Rib Roast", 6.46, "rib_roast.png", # https://www.in2013dollars.com/Other-pork-including-roasts,-steaks,-and-ribs/price-inflation
  "Corn Meal", 12.32, "corn_meal.png", # https://www.in2013dollars.com/Rice,-pasta,-cornmeal/price-inflation/2021-to-2022?amount=0.74
  "Bread", 10.92, "bread.png", # https://www.in2013dollars.com/Bread/price-inflation
  "Ham", 9.14, "ham.png", # https://www.in2013dollars.com/Ham,-excluding-canned/price-inflation
  "Pork Chop", 7.23, "pork_chop.png", # https://www.officialdata.org/Pork-chops/price-inflation/1965
  "Milk", 13.71, "milk.png", # https://www.in2013dollars.com/Milk/price-inflation
  "Eggs", 28.98, "eggs.png", # https://www.in2013dollars.com/Eggs/price-inflation
  "Fish", 9.10, "fish.png", # https://www.in2013dollars.com/Fish-and-seafood/price-inflation
  "All foods", 9.57, "all_foods.png" # https://www.in2013dollars.com/Food/price-inflation
)

# Graphic -----------------------------------------------------------------
# Takes time but works
images_dir <- here::here("2023", "2023_w1")
caption <- "Each circle represents 1% inflation.<br/> Data from U.S. Bureau of Labor Statistics.<br/> Tidytuesday Week-01 2023 &bull;  Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
df |>
  mutate(
    food_img = here::here(images_dir, "Images", img),
    food_img = glue::glue("<img src='{food_img}' width ='18' />"),
    food_img = fct_reorder(food_img, inflation),
    food_label = glue::glue("{food} : + {inflation} % "),
    text_position = case_when(
      inflation %% 1 == 0 ~ (inflation - 1) + .5,
      inflation %% 1 < .5 ~ plyr::round_any(inflation, .5, ceiling),
      TRUE ~ plyr::round_any(inflation, .5, floor),
    )
  ) |>
  ggplot() +
  # geom package will be released soon but feel free to cantact if you really need it
  geom_pictorial_circles(aes(x = inflation, y = food_img, fill = str_equal(food, "All foods")), linewidth = .75, color = "#000000") +
  geom_text(aes(x = text_position + .5, y = food_img, label = food_label), color = "#151515", family = "Mabry Pro", fontface = "bold", hjust = 0, size = 3) +
  annotate(
    geom = "richtext",
    x = 33.5,
    y = 0.5,
    label = caption,
    family = "Mabry Pro",
    hjust = 1,
    vjust = 0,
    fill = NA,
    size = 3.5,
    label.size = unit(0, "cm")
  ) +
  labs(
    title = "Food Price Index Inflation between 2021 & 2022",
  ) +
  coord_equal(clip = "off") +
  scale_x_continuous(
    expand = expansion(add = 0)
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#1D6397",
      "FALSE" = "#F89708"
    ),
    guide = "none"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = ggtext::element_markdown(size = 10, color = "#000000", family = "Mabry Pro", face = "bold"),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(color = "#000000", family = "Mabry Pro", face = "bold", size = rel(2.5), margin = margin(t = .5, b = .5, unit = "cm")),
    plot.caption = element_markdown(color = "#000000"),
    plot.background = element_rect(fill = "#E4E4E4", color = NA),
    plot.margin = margin(c(.35, 0.25, .35, 0), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w1", "tidytuesday_2023_w1")
ggsave(filename = glue::glue("{path}.png"), width = 9.75, height = 6.5, device = ragg::agg_png, dpi = 300)
