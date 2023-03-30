
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)
library(cowplot)

cowplot::set_null_device("pdf")
cowplot::set_null_device("png")
cowplot::set_null_device("cairo")
cowplot::set_null_device("agg")

# Data Wrangling ----------------------------------------------------------
# Source: https://gist.github.com/mchoi2000/e5e0486c74abdbb624db43d7f0783255
world_cities <- read_csv("https://gist.githubusercontent.com/mchoi2000/e5e0486c74abdbb624db43d7f0783255/raw/0e713337d945e4d8859dfe6449f380810c40ca37/worldcities-20210313-population-50000+.csv")
time_zones <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv")

world_25_cities <- world_cities |>
  slice_max(order_by = population, n = 25) |>
  mutate(city_wo_accent = stringi::stri_trans_general(city, "LATIN-ASCII"))

time_zones_cities <- time_zones |>
  separate(col = "zone", remove = F, extra = "merge", into = c("continent", "city"), sep = "/") |>
  mutate(
    city = ifelse(str_detect(city, "/"), str_split_i(city, pattern = "/", 2), city),
    city = str_replace_all(city, "_", " ")
  ) |>
  select(1:3)


# Arbitrary date
today_date <- parse_date_time("2023-03-28 12:37:18", "%y-%m-%d %H:%M:%S")
world_25_cities_tz <- world_25_cities |>
  left_join(time_zones_cities, by = c("city_wo_accent" = "city")) |>
  mutate(
    # https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
    zone = case_match(city_wo_accent,
      "Delhi" ~ "Asia/Calcutta",
      "Mumbai" ~ "Asia/Calcutta",
      "Guangzhou" ~ "Asia/Shanghai",
      "Beijing" ~ "Asia/Chongqing",
      "Shenzhen" ~ "Asia/Shanghai",
      "Osaka" ~ "Asia/Tokyo",
      "Bangalore" ~ "Asia/Calcutta",
      .default = zone
    ),
    country = case_match(
      country,
      "United States" ~ "USA",
      .default = country
    )
  ) |>
  select(city, country, population, zone)

# Compute time zone offsets to arrange clocks (Don't work with mutate)
tz_offsets <- vector(mode = "integer", length = 25L)
for (i in seq_along(world_25_cities_tz$zone)) {
  city_date <- with_tz(today_date, world_25_cities_tz$zone[i])
  tz_offset <- as.numeric(format(city_date, "%z"))
  tz_offsets[i] <- tz_offset
}

world_25_cities_tz$tz_offset <- tz_offsets

world_25_cities_tz <- world_25_cities_tz |>
  mutate(city = fct_reorder(city, tz_offset)) |>
  arrange(city)

r <- 1
hangles <- seq(0, 2 * pi - 2 * pi / 12, by = 2 * pi / 12)
mangles <- seq(0, 2 * pi - 2 * pi / 60, by = 2 * pi / 60)

hours_df <- data.frame(
  x = (r - .05) * cos(hangles),
  y = (r - .05) * sin(hangles),
  xend = (r - .085) * cos(hangles),
  yend = (r - .085) * sin(hangles),
  label = c(3:1, 12:4)
)

minutes_df <- data.frame(
  x = (r - .05) * cos(mangles),
  y = (r - .05) * sin(mangles),
  xend = (r - .085) * cos(mangles),
  yend = (r - .085) * sin(mangles)
)




# Graphic -----------------------------------------------------------------
font <- "Mabry Pro"
font_black <- "Mabry Pro Black"
city_plot <- function(city, country, population, zone, tz_offset) {
  fancy_population <- scales::comma(population / 1000, big.mark = ".", suffix = " M")
  fancy_population <- ifelse(str_equal(city, "Mexico City"), paste0(fancy_population, "illions residents"), fancy_population)
  city_time <- with_tz(today_date, zone)
  hour <- as.numeric(format(city_time, "%H"))
  min <- as.numeric(format(city_time, "%M"))
  sec <- as.numeric(format(city_time, "%S"))
  hour_var <- hour %% 12
  min_var <- (min %% 60) / 60
  hour_var <- hour_var + min_var

  hour_angle <- -((hour_var * 2 * pi) / 12) + pi / 2
  min_angle <- -((min * 2 * pi) / 60) + pi / 2
  sec_angle <- -((sec * 2 * pi) / 60) + pi / 2

  ggplot() +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1, size = 1.5), fill = "white") +
    geom_segment(data = hours_df, aes(x, y, xend = xend, yend = yend), linewidth = .75, linejoin = "round", lineend = "round") +
    geom_segment(data = minutes_df, aes(x, y, xend = xend, yend = yend), linewidth = .25, linejoin = "round", lineend = "round") +
    geom_text(data = hours_df, aes(x = .85 * xend, y = .85 * yend, label = label), family = font, size = 4) +
    annotate(geom = "text", x = 0, y = 1.35, label = paste0(rep("0", 2 - nchar(hour)), hour, ":", rep("0", 2 - nchar(min)), min), family = font, size = 8.5) +
    annotate(geom = "text", x = 0, y = -1.4, label = paste0(city, ",", country), family = font_black, size = 5.25) +
    annotate(geom = "text", x = 0, y = -1.85, label = fancy_population, family = font, size = 4.5) +
    annotate(geom = "segment", x = 0, y = 0, xend = cos(sec_angle) * 0.8, yend = sin(sec_angle) * .8, color = "#B3807D", linewidth = .45, linejoin = "round", lineend = "round") +
    annotate(geom = "segment", x = 0, y = 0, xend = cos(sec_angle + pi) * 0.05, yend = sin(sec_angle + pi) * .05, color = "#B3807D", linewidth = .45, linejoin = "round", lineend = "round") +
    annotate(geom = "segment", x = cos(sec_angle + pi) * 0.05, y = sin(sec_angle + pi) * .05, xend = cos(sec_angle + pi) * 0.075, yend = sin(sec_angle + pi) * .075, color = "#B3807D", linewidth = .55, linejoin = "round", lineend = "round") +
    annotate(geom = "segment", x = 0, y = 0, xend = cos(hour_angle) * 0.5, yend = sin(hour_angle) * .5, linewidth = 1.25, linejoin = "round", lineend = "round") +
    annotate(geom = "segment", x = 0, y = 0, xend = cos(min_angle) * 0.8, yend = sin(min_angle) * .8, linewidth = 1.25, linejoin = "round", lineend = "round") +
    annotate(geom = "point", x = 0, y = 0,  size = 1.35, color = "#B3807D") + 
    annotate(geom = "point", x = 0, y = 0,  size = .25, color = "white") + 
    coord_equal(clip = "off") +
    scale_size_identity() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(c(.5, 0, .25, 0), unit = "cm")
    )
}


cities_plots <- pmap(world_25_cities_tz, city_plot)
clocks_plots_grid <- cowplot::plot_grid(plotlist = cities_plots, nrow = 5)

title <- ggdraw() +
  draw_label(
    "Time offset in the 25 most populated cities in the world",
    x = 0,
    hjust = 0,
    size = 27.5,
    fontface = "bold",
    fontfamily = font_black
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 5)
  )

caption <- ggdraw() +
  draw_label(
    label = "Tidytuesday Week-13 2023\nAbdoul ISSA BIDA\nData from IANA tz database",
    x = .95,
    y = 0,
    hjust = 1,
    vjust = 0,
    fontfamily = font
  ) +
  theme(
    plot.margin = margin(5, 0, 0, 0)
  )

final_plot <- plot_grid(title,
  clocks_plots_grid,
  caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.085, 1, 0.05)
)

ggdraw(final_plot) +
  theme(
    plot.background = element_rect(fill = "#C5C5C7", color = NA),
    plot.margin = margin(c(.5, .5, .5, .5), unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w13", "tidytuesday_2023_w13")
ggsave(filename = glue::glue("{path}.png"), width = 12, height = 13, device = ragg::agg_png, dpi = 300)
