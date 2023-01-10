
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(sf)
library(sp)

# Data Wrangling ----------------------------------------------------------
feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

# Only keep metropolitan states
feederwatch_df <- feederwatch |> 
  filter(startsWith(subnational1_code, "US"),  str_detect(subnational1_code, pattern = "(AK)|(HI)", negate = T)) |> 
  count(longitude, latitude, sort = T, name = "z") 

# USA States sf
states_to_remove <-  c("AK", "HI", "VI", "GU", "AS", "MP", "PR")
us_states <- albersusa::usa_sf() |> 
  st_transform(crs = st_crs("ESRI:102003"))

us_states <- us_states |> 
  filter(!iso_3166_2 %in% states_to_remove)

# Coords transformation to a better projection
feederwatch_df <- feederwatch_df |> 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |> 
  st_transform(crs = "ESRI:102003") |>
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2],
    .before = 1L
  )

# Remove geometry
feederwatch_df <- feederwatch_df |> 
  st_set_geometry(NULL) |> 
  mutate(
    x = longitude, 
    y = latitude, 
    z = z
  )

# Convert the map shapefile to a SpatialPolygonsDF 
states_map <- as(us_states, "Spatial")

# Convert feederwatch_df to SpatialPointsDF
sp::coordinates(feederwatch_df) <- ~ x + y 

# Create an empty grid where n is the total number of cells 
# use a lower n value for less system resources 
grd <- as.data.frame(sp::spsample(states_map, "regular", n = 1000000))

names(grd)  <-  c("x", "y") # rename columns to x and y in grid

coordinates(grd) <- c("x", "y") # convert grid dataframe to SpatialPoints
gridded(grd) <- TRUE  # Create SpatialPixel object 
fullgrid(grd) <- TRUE # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(feederwatch_df) <- proj4string(feederwatch_df) # Temp fix until new proj env is adopted
proj4string(grd)     <- proj4string(feederwatch_df)

# Interpolate the grid cells using a power value of 2 
P.idw <- gstat::idw(z ~ 1, feederwatch_df, newdata = grd, idp = 2.0)

# Convert to raster object then clip to the US Map
r       <- raster::raster(P.idw)
r.m     <- terra::mask(r, states_map)

# Create a new dataframe that is a raster points of the birds observation
df <- data.frame(raster::rasterToPoints(r.m))

colnames(df) <- c("lon", "lat", "z") # set column names to new df

final_df <- df |> 
  mutate(
    # Cut birds appareances 
    z = cut_number(z, 6)    
  )

# Graphic -----------------------------------------------------------------
bg <- "#19222B"
fc <- "#FFFFFF"
caption <- "Tidytuesday Week-02 2023<br/> Data from  **Project FeederWatch**<br/> Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
final_df |> 
  ggplot() + 
  geom_tile(aes(lon, lat, fill = z)) + 
  annotate(geom = "richtext", x = -2354705 + 1500000, y = -1333641 + 120000, 
           label = caption,
           family = "Mabry Pro",
           fill = NA,
           size = 3.5,
           label.color = fc,
           color = fc,
           label.r = unit(0, "pt"),
           label.padding = unit(rep(8,4), "pt"),
           hjust = 1, 
           vjust = 0) + 
  labs( 
    title = "BIRDS",
    subtitle = "Presence across the United States"
    ) + 
  scale_fill_manual(
    values = scales::gradient_n_pal(c("#338000","#FEB926", "#D00000"))(seq(0, 1, length.out = 6)), 
    labels = c("← Less", rep("", 4), "More →"), 
    guide = guide_legend(
      title = NULL, 
      nrow = 1,
      title.hjust = .5,
      title.position = "top",
      label.position = "top", 
      label.hjust = .5, 
      label.theme = element_text(color = fc, family = "Mabry Pro", size = rel(15)),
      keywidth = unit(1.5, "cm"),
      keyheight  = unit(.75, "cm"),
      direction = "horizontal"
    )    
  ) + 
  theme_minimal() + 
  theme(
    text = element_text(color = fc, family = "Mabry Pro"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(family = "Mabry Pro Black", size = rel(3.25), hjust = .5, margin = margin(b = .5, unit = "cm")),
    plot.subtitle = element_text(face = "bold", size = rel(2.25), hjust = .5),
    panel.grid = element_blank(), 
    legend.position = "top",
    legend.spacing.x = unit(0,"cm"), 
    plot.margin = margin(c(.5, .25, .5, .25), unit = "cm"),
    plot.background = element_rect(fill = bg, color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w2", "tidytuesday_2023_w2")
ggsave(filename = glue::glue("{path}.png"), width = 9, height = 8.5, device = ragg::agg_png, dpi = 300)


