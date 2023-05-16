
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(albersusa)

# Data Wrangling ----------------------------------------------------------
tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

tornadoes_sf <- tornados |> 
  select(yr, slat, slon) |> 
  st_as_sf(coords = c("slon", "slat"), crs = 4326) |> 
  st_transform(albersusa::us_laea_proj)

counties_sf <- albersusa::counties_sf(proj = "laea")
states_sf <- albersusa::usa_sf(proj = "laea")

tornados_by_county <- st_join(counties_sf, tornadoes_sf, left = T)

yearly_county_tornados <- tornados_by_county |> 
  count(county_fips, drop = F)


# Graphic -----------------------------------------------------------------
yearly_county_tornados |> 
  mutate(n = ggplot2::cut_interval(n, n = 10)) |> 
  ggplot() + 
  geom_sf(aes(fill = n), linewidth = 0.00125, color = NA) + 
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.15) +
  labs(
    title = "Tornados in USA", 
    subtitle = "The number of tornados observed in  each US County",
    caption = "Tidytuesday Week-20 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      Data from NOAA's National Weather Service Storm Prediction Center"
  )+ 
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1,
    begin = 0.05,
    end = 0.85, 
    labels = c("<- Less",rep("",7),"More ->"),
    guide = guide_colorsteps(
      title = "Number of tornaods observed",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      barwidth = unit(10, "cm")
      
    )
  ) + 
  theme_minimal()  +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 1, margin = margin(t = 1, unit = "cm")),
    plot.subtitle = element_text(hjust = 1, margin = margin(b = 1, unit = "cm")),
    plot.caption = element_markdown(hjust = 1, margin = margin(b = 1, unit = "cm")),
    plot.margin =  margin(0.5, unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w20", "tidytuesday_2023_w20")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

