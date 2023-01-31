
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(waffle)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv")

bi_monthly_df <- df |>
  mutate(
    yearw = as.integer(format(timestamp, "%U")),
    bimonthly = (yearw %/% 2) * 2,
    # %u :  week day
    # %U : year week
    bimonthly = as.Date(paste(2017, bimonthly, 1, sep = "-"), format = "%Y-%U-%u"),
    .after = "timestamp"
  ) |>
  count(bimonthly)

bi_monthly_df <- bi_monthly_df |>
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

bi_monthly_df$cat_letters <- sample(letters, size = nrow(bi_monthly_df), replace = T)
bi_monthly_df$values <- rep(1, nrow(bi_monthly_df))

# Graphic -----------------------------------------------------------------
fill_clr <- "#121212"
(plot <- bi_monthly_df |>
  mutate(
    bimonthly_label = format(bimonthly, "%m/%d"),
    bimonthly_label = glue::glue("<span style='color: white;font-size: 25pt;'>**{bimonthly_label}**</span><br><span style='font-size: 13.5pt;color: #D04F5A;'>**{scales::comma(n)}**</span>"),
    bimonthly_label = as.factor(bimonthly_label)
  ) |>
  ggplot() +
  stat_waffle(
    aes(
      fill = cat_letters,
      label = cat_letters,
      values = values,
    ),
    n_rows = 2,
    size = 11,
    geom = "text",
    family = "Cat font",
    
  ) +
  facet_wrap(vars(bimonthly_label), ncol = 1, strip.position = "left") +
    scale_x_continuous(
      expand = expansion(mult = c(.05, .35))
    ) + 
    scale_y_continuous(
      expand = expansion(mult = 0)
    ) + 
    
  coord_equal(clip = "off") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    strip.text.y.left = element_markdown(angle = 0, family = "Mabry Pro"), 
    panel.spacing.y = unit(.5, "cm"),
    plot.background = element_rect(fill = fill_clr, color = NA)
  )
)

(title_plot <- ggplot() + 
  annotate(
    geom = "richtext", x= 0, y = 0, 
    label = "<span style='font-size: 55pt;'>**UK C<span style='font-family: \"Cat font\"'>b</span>ts**</span><br>
    <span style='font-size: 25pt; color: #D04F5A;'>**Bi-monthly Tracking**</span><br><br>
    Each <span style='font-family: \"Cat font\"'>d</span> represents approximatively 100 tracked cats.<br>
    Tidytuesday Week-05 2023\n Data from  Movebank for Animal Tracking Data<br> 
    Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**", 
    angle = 90,
    color = "white",
    family = "Mabry Pro",
    size = 6.5,
    fill = NA, 
    label.size = unit(0, 'pt'),
    label.r = unit(0,'pt')
  ) + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill = fill_clr, color = NA),
      axis.text =  element_blank(),
      axis.title =  element_blank()
    )
)  

plot + 
  patchwork::inset_element(title_plot,  left = 0.725, bottom = 0.05, right = 0.975, top = 0.9) &
  theme(
    plot.margin = margin(c(.25, 0, .25, 0), unit = "cm"),
      plot.background = element_rect(fill = fill_clr, color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w5", "tidytuesday_2023_w5")
ggsave(filename = glue::glue("{path}.png"), width = 11, height = 12, device = ragg::agg_png, dpi = 320)

