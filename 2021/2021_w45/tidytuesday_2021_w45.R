# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(spikemap) # remotes::install_github("rCarto/spikemap")
library(cartography)
library(spData)

# Data Reading and Wrangling ----------------------------------------------
#   
sf_world <- st_as_sf(rworldmap::getMap(resolution = "low")) 
sf_africa <- sf_world %>% 
  filter(REGION == "Africa") %>% 
  # Mutate to upper case to join with countries index dataframe
  mutate(ADMIN = str_to_upper(ADMIN))

# Mutate names to join
afcon_cleaned <- afcon %>% 
  mutate(name= as.character(name)) %>% 
  mutate(name = case_when(
    name == "THE GAMBIA" ~ "GAMBIA",
    name == "ZAIRE" ~ str_to_upper("Democratic Republic of the Congo"),
    name == "CONGO" ~ str_to_upper("Republic of the Congo"),
    name == "TANZANIA" ~ str_to_upper("United Republic of Tanzania"),
    TRUE ~ name
  ))

# Join with the sf df 
afcon_geometry <-  sf_africa %>% 
  left_join(afcon_cleaned, by =  c("ADMIN" = "name" )) %>% 
  mutate(
    ADMIN = case_when(ADMIN  == str_to_upper("United Republic of Tanzania")~  "TANZANIA",
                      ADMIN  == str_to_upper("Republic of the Congo")~  "CONGO",
                      TRUE ~ ADMIN),
    ADMIN = str_wrap(ADMIN, 15))

# Just keep Countries with index
afcon_existing <- afcon_geometry %>% 
  drop_na(totcon) %>% 
  # Concatenate ADMIN with index for spikes labels
  mutate(ADMIN_TOTCON = paste0(ADMIN,"\n", scales::comma(totcon, big.mark = ",", accuracy = 4)))



# Graphic -----------------------------------------------------------------
# IMPORTANT I use Base R
path <- here::here("2021_w45", "tidytuesday_2021_w45")
cairo_pdf(bg="#F3F6F7", glue::glue("{path}.pdf"),width=7,height=7.5)

par(mar = c(0,0.2,3,0.2),omi=c(0,0.75,0.95,0.75))
plot(st_geometry(afcon_geometry), col="#99aed1", border = "#e1e5eb",
     lwd = 0.2, bg = "#F3F6F7")   
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


lbl <- spikelabel(x = afcon_existing, var = "totcon",
                  inches = .5, fixmax = 5250)
lbl <- lbl[order(lbl$totcon, decreasing = T),]

# display only those with totcon > 1000
labelLayer(lbl[lbl$totcon >=1000,], txt = "ADMIN_TOTCON",
           pos = 3, offset = .5,
           halo = T, bg = "#99aed150",
           cex = .4,
           col = "#111111",
           family = "Gotham Black"
)

# Plot north arrow
north(pos = "topright", col = "grey60", x = afcon_geometry)

mtext("Index of Total Conflict in Africa\n between 1966-78",
      side = 3, adj = 0.5, padj = 0, line = 2,
      cex = 2, font = 2, col = "#111111", family = "Gotham Black")
mtext("Data from {spData} package \nTidytuesday Week-45 2021 · Abdoul ISSA BIDA",1,line=-3,
      family = "Gotham Book",
      col = "#111111",
      adj=.5,cex=.65,font=2,outer=T)
dev.off()

# Saving ------------------------------------------------------------------
# Convert to PNG
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 640
)
