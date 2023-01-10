
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(sf)
library(sp)
# Data Wrangling ----------------------------------------------------------
feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

# feederwatch <- feederwatch |> 
#   # keep only some states
#   count(species_code, sort = T)
feederwatch_df <- feederwatch |> 
  filter(startsWith(subnational1_code, "US"),  str_detect(subnational1_code, pattern = "(AK)|(HI)", negate = T)) |> 
  count(longitude, latitude, sort = T, name = "z") 

# feederwatch_df |> 
#   ggplot() + 
#   geom_point(aes(longitude, latitude))

# USA States 
states_to_remove <-  c("AK", "HI", "VI", "GU", "AS", "MP", "PR")
us_states <- albersusa::usa_sf() |> 
  st_transform(crs = st_crs("ESRI:102003"))

us_states <- us_states |> 
  filter(!iso_3166_2 %in% states_to_remove)

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
    z = cut_number(z, 5)    
  )

final_df |> 
  ggplot() + 
  geom_tile(aes(lon, lat, fill = z))

# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w2", "tidytuesday_2023_w2")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

