# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggforce) 
library(ggtext)
library(scales)

# Data Reading and Wrangling ----------------------------------------------
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')

captured_vs_farmed <- captured_vs_farmed %>% 
  rename(aquaculture = `Aquaculture production (metric tons)`, 
         capture = `Capture fisheries production (metric tons)`)


main_entities <- c("China", "Japan", "European Union", "Peru", "Indonesia", "United States", "India", "Chile", "Norway", "Philippines", "Russia", "South Korea", 
                   "Thailand", "Africa Western and Central", "Middle East & North Africa")

# Only keeps main_entities, and years between 2003 and 2018 
culture_evolutions <- captured_vs_farmed %>% 
  filter(Entity %in% main_entities, between(Year, 2003,2018)) %>%  
  arrange(Entity, Year) %>% 
  group_by(Entity) %>% 
  mutate(
    evolution = (aquaculture - lag(aquaculture)) / aquaculture,
    evolution = 100* evolution
  )  %>% 
  filter(!is.na(evolution)) %>% 
  ungroup()

# Compute proportions  of aquaculture
# in each entity total production
(culture_proportions <- culture_evolutions %>% 
    group_by(Entity) %>% 
    summarise(
      overall = sum(aquaculture, capture),
      aquaculture_prop  = sum(aquaculture) /  overall
    ) %>%
    
    mutate(
      overall = round(overall / 1000000, 2),
      Entity = fct_reorder(Entity, aquaculture_prop)
    )  %>% 
    arrange(Entity) %>% 
    mutate(index = row_number(), 
           index = index*2) %>%
    relocate(index) 
)

# Distribute annual changes
culture_evolutions <- culture_evolutions %>% 
  mutate(
    Entity = factor(Entity, levels = levels(culture_proportions$Entity)),
    evolution_progress = cut_number(evolution, 6)%>% 
        # Important to keep spaces of differents width
      factor(labels = c("Strongly\nDecrease", "", " ", 
                        "  ", "   ", "Strongly\nIncrease"))
  ) %>% 
  arrange(Entity) %>% 
group_by(Entity) %>% 
  mutate(index = group_indices(), 
         index = index*2) %>%
  relocate(index) 
  


# Graphic -----------------------------------------------------------------
  ggplot() + 
  geom_circle(data = culture_evolutions, aes(x0 = Year , y0=index , r =.4995 , fill = fct_rev(evolution_progress)), size = .25, color = NA) +
  geom_text(data = culture_evolutions, aes(x =Year, y = index, label = paste0(ifelse(evolution > 0, "+",""),glue::glue("{round(evolution,1)}%"))), size = 2, color = "white",
            family = "Gotham Black", fontface = "bold") + 
  geom_richtext(data = culture_proportions, aes(y =index, label = glue::glue("<span>**{Entity}**</span><br><span style=\"color: grey25;\">**{overall}M Tons**</span>")), 
                hjust = 1,x = 2003, fill =NA, label.color = NA,
                lineheight = .95,
                family = "Gotham Black"
                ) + 
  geom_rect(data = tibble(xmin = 2020, xmax = 2024,y = 1:15*2), aes(xmin = xmin, xmax = xmax, ymin = y -.35, ymax = y+.35), 
            fill = "#001f3f",
            alpha = .3) + 
  geom_rect(data = culture_proportions, aes(xmin = 2020, xmax = 2020+ aquaculture_prop*4, ymin = index -.35, ymax = index+.35),
            fill = "#001f3f"
            ) + 
  geom_text(data = culture_proportions, aes(x = 2024.5, y = index, label = glue::glue("{round(aquaculture_prop*100,2)}%")),
            size =4,
            family = "Gotham Medium",
            hjust = 0) +
  annotate(geom = "segment", x = 2020, xend = 2020, y = 1, yend = 31, size =.25) + 
  annotate(geom = "segment", x = 2022, xend = 2022, y = 1, yend = 31, size =.125, linetype = "dotted") + 
  annotate(geom = "segment", x = 2024, xend = 2024, y = 1, yend = 31, size = .25) + 
  annotate(geom = "text", x = 2022, y = 32.5, label = "Aquaculture\nProportion", size = 4.5, family = "Gotham Black") + 
  annotate(geom = "text", x = 2003, y = 32.5, label = "Countries\nFisheries Production", size = 4.5, hjust = 1,family = "Gotham Black") + 
  annotate(geom = "text", x = 2020, y = .5, label = "0%", size = 3,family = "Gotham Medium") + 
  annotate(geom = "text", x = 2022, y = .5, label = "50%", size = 3, family = "Gotham Medium") + 
  annotate(geom = "text", x = 2024, y = .5, label = "100%", size = 3, family = "Gotham Medium") + 
  annotate(geom = "segment", x = 2004, xend = 2004, y = 1.5 , yend = 0, size = .5) +   
  annotate(geom = "segment", x = 2018, xend = 2018, y = 1.5 , yend = 0, size = .5) +   
  annotate(geom= "point", x=2004, y = 1.5, pch =21, size = 2, fill = "white") +
  annotate(geom= "point", x=2018, y = 1.5, pch =21, size = 2, fill = "white") +
  annotate(geom = "segment", x = 2004, xend = 2004, y = 30.5 , yend = 31.5, size = .5) +   
  annotate(geom = "segment", x = 2004, xend = 2004.5, y = 31.5 , yend = 31.5, size = .5) +   
  annotate(geom = "text", x = 2004.6, y = 31.5, label = "Change comparatively\nto 2003 production.", hjust = 0, lineheight = .85, family = "Mercury Display", fontface = "bold.italic") + 
  annotate(geom= "point", x=2004, y = 30.5, pch =21, size = 2, fill = "white") +
  annotate(geom = "text", x = 2004, y = -.5, label = "2004",family = "Gotham Medium") + 
  annotate(geom = "text", x = 2018, y = -.5, label = "2018", family = "Gotham Medium") + 
  labs(
    title = "How Aquaculture is alleviating pressure on wild fish production?",
    subtitle = "Annual change in aquaculture (the practice of fish and seafood farming) production\n between 2004 and 2018.",
    caption = "Data from OurWorldinData.org.\n Tidytuesday Week-42 2021 · Abdoul ISSA BIDA."
  ) + 
  scale_fill_viridis_d(begin = .15, end = 0.8, direction = -1,
                       guide =  guide_legend(title = NULL, nrow = 1, 
                                             reverse = T, 
                                             label.position = "top", 
                                             keyheight = unit(15, "mm"),
                                             keywidth = unit(22.5, "mm"))) +
  coord_equal(clip = "off") + 
  theme_minimal() + 
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(),
    panel.grid  = element_blank(),
    plot.margin = margin(r = -3.5, unit = "cm"),
    plot.title = element_text(size = 20, margin = margin(t= 25,b = 15), hjust = .5, family = "Gotham Black"),
    plot.subtitle = element_text(size = 15, color = "grey5", margin = margin(b = 15), hjust = .5, family = "Mercury Display", face = "bold"),
    plot.caption = element_text(color = "black", size = rel(.95), family = "Gotham Medium", margin = margin(t = -15, b = 10, r = 10), hjust = 1),
    legend.position = "top", legend.box.spacing = unit(0.5, "mm"),
    legend.text = element_text(color = "white", 
                               size = 13,
                               face = "bold",
                               family = "Gotham Medium",
                               margin = margin(b = -40),
                               vjust = 0.5), 
    legend.spacing.x = unit(0,"mm")
  ) 

# Saving ------------------------------------------------------------------
path <-  here::here("2021_w42/tidytuesday_2021_w42")
ggsave(glue::glue("{path}.pdf"), width = 12.5, height = 15.5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)
