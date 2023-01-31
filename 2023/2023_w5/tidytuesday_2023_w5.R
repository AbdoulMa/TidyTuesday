
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(waffle)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv")

# Graphic -----------------------------------------------------------------

df |>
  ggplot() +
  geom_point(aes(location_long, location_lat))

bi_weeks_df <- df |>
  mutate(
    yearw = as.integer(format(timestamp, "%U")),
    biweek = (yearw %/% 2) * 2,
    # %u :  week day
    # %U : year week
    biweek = as.Date(paste(2017, biweek, 1, sep = "-"), format = "%Y-%U-%u"),
    .after = "timestamp"
  ) |>
  count(biweek)

bi_weeks_df <- bi_weeks_df |>
  mutate(
    n_rounded = ceiling(n / 100)
  ) |>
  rowwise() |>
  mutate(
    cat_index = list(1:n_rounded)
  ) |>
  unnest_longer(
    col = cat_index
  )

bi_weeks_df$cat_letters <- sample(letters, size = nrow(bi_weeks_df), replace = T)
bi_weeks_df$values <- rep(1, nrow(bi_weeks_df))
(plot <- bi_weeks_df |>
  # select(-n) |>
  mutate(
    biweek_label = format(biweek, "%m/%d"),
    biweek_label = glue::glue("<span style='font-size: 25pt;'>**{biweek_label}**</span><br><span style='font-size: 12.5pt;font-color: grey;'>{scales::comma(n)}</span>"),
    biweek_label = as.factor(biweek_label)
  ) |>
  ggplot() +
  stat_waffle(
    aes(
      fill = cat_letters,
      label = cat_letters,
      values = values,
    ),
    n_rows = 2,
    size = 10.5,
    color = "black",
    geom = "text",
    family = "kitty cats tfb"
  ) +
  labs(
      # caption = "Each <span style=\"font-family:;\">b</span> represents approximatively 100 cats observed."
  ) + 
  facet_wrap(vars(biweek_label), ncol = 1, strip.position = "left") +
    scale_x_continuous(
      expand = expansion(mult = c(.05, .35))
    ) + 
    scale_y_continuous(
      expand = expansion(mult = 0)
    ) + 
    
  coord_equal(clip = "off") +
  theme_minimal() +
  theme(
    strip.text.y.left = element_markdown(angle = 0), 
    plot.caption = element_markdown(size = rel(1)),
    
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(.5, "cm"),
    plot.margin = margin(l = 0, unit = "cm")
  )
)

(title_plot <- ggplot() + 
  annotate(
    geom = "richtext", x= 0, y = 0, 
    label = "<span style='font-size: 40pt;'>**UK Cats**</span><br>
    <span style='font-size: 15pt;'>***Bi weekly-Observations***</span><br><br>
    Each <span style='font-family:kitty cats tfb;'>b</span> represents approximatively 100 cats observed. <br>
    Tidytuesday Week-05 2023\n Data from  Movebank for Animal Tracking Data<br> Abdoul ISSA BIDA", 
    angle = 90,
    fill = NA, 
    label.size = unit(0, 'pt'),
    label.r = unit(0,'pt')
  ) + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      axis.text =  element_blank(),
      axis.title =  element_blank()
    )
)  

plot + 
  patchwork::inset_element(title_plot,  left = 0.75, bottom = 0.05, right = 0.975, top = 0.9) + 
  theme(
    plot.margin = margin(l = 0, unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w5", "tidytuesday_2023_w5")
ggsave(filename = glue::glue("{path}.png"), width = 11, height = 12, device = ragg::agg_png, dpi = 320)

