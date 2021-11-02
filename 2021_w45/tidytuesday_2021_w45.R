# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(spikemap) # remotes::install_github("rCarto/spikemap")
library(cartography)
library(spData)
# Data Reading and Wrangling ----------------------------------------------
glimpse(coffee_data)

afcon %>% 
  arrange(name)

afcon %>% 
  ggplot() + 
  geom_point(aes(x, y))
urban_agglomerations %>% 
  filter(country_or_area == "France") %>% view()
  ggplot() + 
  geom_sf()
  
sf_world <- st_as_sf(rworldmap::getMap(resolution = "low")) 
sf_africa <- sf_world %>% 
  filter(REGION == "Africa")

sf_africa <- sf_africa %>% 
  mutate(ADMIN = str_to_upper(ADMIN))
  ggplot() + 
  geom_sf(data = sf_africa) + 
    geom_point(data = afcon, aes(x,y))


glimpse(sf_africa)
path <- here::here("2021_w45", "tidytuesday_2021_w45")
cairo_pdf(bg="grey98", glue::glue("{path}.pdf"),width=7,height=7.5)

par(mar = c(0,0.2,3,0.2),omi=c(0,0.75,0.95,0.75))
  plot(st_geometry(sf_africa), col="#99aed1", border = "#e1e5eb",
       lwd = 0.2, bg = "#e1e5eb")  
  
   afcon_ed <- afcon %>% 
    mutate(name= as.character(name)) %>% 
    mutate(name = case_when(
      name == "THE GAMBIA" ~ "GAMBIA",
      name == "ZAIRE" ~ str_to_upper("Democratic Republic of the Congo"),
      name == "CONGO" ~ str_to_upper("Republic of the Congo"),
      name == "TANZANIA" ~ str_to_upper("United Republic of Tanzania"),
      TRUE ~ name
    ))
   
 
  afcon_geometry <-  sf_africa %>% 
     left_join(afcon_ed, by =  c( "ADMIN" = "name" )) %>% 
    mutate(ADMIN = str_wrap(ADMIN, 15))
 
  max(afcon_geometry$totcon, na.rm = T)
 plot(st_geometry(afcon_geometry), col="#99aed1", border = "#e1e5eb",
      lwd = 0.2, bg = "#e1e5eb")   
   spikemap(x = afcon_geometry[afcon_geometry$totcon <= 1000, ], var = "totcon", inches = .5, 
            fixmax = 5250,
            col = "#ffffff90", border = "#94000090",  lwd = .5,
            legend.pos = "x")
   spikemap(x = afcon_geometry[afcon_geometry$totcon > 1000, ], var = "totcon",
            inches = .5, fixmax = 5250,
            col = "#ffffff", border = "#940000", lwd = 1.1,
            legend.pos = c(779307.2, 6128000),
            legend.title.txt = "Population",
            legend.values.rnd = -3)
   
   afcon_existing <- afcon_geometry %>% 
     drop_na(totcon)
   
   
   lbl <- spikelabel(x = afcon_existing, var = "totcon",
                     inches = .5, fixmax = 5250)
   lbl <- lbl[order(lbl$totcon, decreasing = T),]
   # display only the 12 first, use various cex and halo
   labelLayer(lbl[1:12,], txt = "ADMIN",
              pos = 3, offset = .5,
              halo = T, bg = "#99aed150",
              cex = .45,
              col = "grey30",
              family = "Lato Black"
              )
   
   # barscale(size = 20, pos= c(629638.7 ,6136862.3), lwd = 1)
   north(pos = "topright", col = "grey60", x = afcon_geometry)
   
   layoutLayer(title = "",
               sources = "Data from spData package.",
               author = "Tidytuesday Week-44 2021 · Abdoul ISSA BIDA",
               frame = FALSE, scale = FALSE)
   mtext("Index of Total Conflict in Africa\n between 1966-78",
         side = 3, adj = 0.5, padj = 0, line = 2,
         cex = 2.5, font = 2, col = "#111111", family = "Lato Black")
   mtext("Source: ISSP Data Report Religious Attitudes and Religious Change",1,line=-5
         ,adj=.5,cex=1,font=1,outer=T)
   dev.off()
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
