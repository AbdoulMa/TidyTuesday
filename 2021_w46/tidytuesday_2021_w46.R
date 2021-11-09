# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(ggtext)
library(cowplot)

# Data Reading and Wrangling ----------------------------------------------

polio_vaccine_coverage <- read_csv("2021_w46/polio-vaccine-coverage-of-one-year-olds.csv") %>% 
  rename(one_year_olds_immunized = `Pol3 (% of one-year-olds immunized)`)

paralytic_polio_cases <- read_csv("2021_w46/reported-paralytic-polio-cases-per-1-million-people.csv") %>% 
  rename(polio_cases = `Polio cases per 1 million population (WHO (2017))`)

#  Map Geometry Reading
sf_world <- st_as_sf(rworldmap::getMap(resolution = "low")) 
sf_africa <- sf_world %>% 
  filter(REGION == "Africa") %>% 
  # Select Just ISO_A3 for join 
  select(ISO_A3,ADMIN.1 )

polio_coverage_cases <-  polio_vaccine_coverage %>% 
  left_join(paralytic_polio_cases, by = c("Entity","Code", "Year")) %>% 
  # complete with missing year for each country
  complete(nesting(Entity, Code), Year) %>% 
  group_by(Entity) %>% 
  fill(polio_cases, .direction = "downup") %>% 
  fill(one_year_olds_immunized, .direction = "downup") %>% 
  ungroup() 

  
africa_polio_df <- sf_africa %>%  
  mutate(
    ISO_A3 = as.character(ISO_A3),    
    ISO_A3 = case_when(ISO_A3 == "ESH" ~ "MAR", # Western Sahara with Morocco
                      ISO_A3 == "SOL" ~ "SOM", # Somaliland with Somalia
                      TRUE ~ ISO_A3
                      )
  ) %>% 
  left_join(polio_coverage_cases, by = c("ISO_A3" = "Code")) %>% 
  group_by(Year) %>% 
  mutate(pct_immunized = mean(one_year_olds_immunized, na.rm = T)) %>% 
  ungroup()  




# Graphic -----------------------------------------------------------------
(africa_polio_plot <- africa_polio_df %>% 
  drop_na(polio_cases) %>% 
  filter(Year %in% seq(1980, 2015, by = 5)) %>% 
  mutate(
    fancy_strip_text = glue::glue("<span style='font-family:\"Gotham Black\";font-size:25px;'><b>{Year}</b></span><br><br><span style='font-family:\"Mercury\";font-size:20px;'><b>{round(pct_immunized,2)}% {ifelse(Year == 1980, 'of one-year olds <br>','')} immunized.</b></span>"),
    polio_cases = cut(polio_cases, breaks = c(-Inf,0:8,Inf))) %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = polio_cases),
          size = .125,
          color = "grey15") +
  labs(
    title = "The vaccine against polio\n in Africa",
    subtitle = "In 1988, 
    the World Health Assembly launched the Global Polio Eradication Initiative (GPEI) 
    which was tasked with eradicating the disease globally by the year 2000.
    33 years later, they have almost done it."
  ) + 
  scale_fill_manual(
    name = "Paralytic Polio Cases per 1 million people",
    values = paletteer::paletteer_d("ggsci::pink_material", n = 10) %>% as.character(),
    labels = c("0\n or negligible" , 1:7, "8\n or more"),
    guide = guide_colorsteps(
      title.position = "top", 
      title.hjust = .5,
      barheight = unit(0.5, "cm"),
      barwidth = unit(9, "cm")
    )
  ) + 
  facet_wrap(vars(fancy_strip_text)) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title =  element_text(size = rel(2.5), hjust = .5, family = "Gotham Black", face = "bold", margin = margin(t = 10, b = 15)),
    plot.subtitle =  element_text(size = rel(1.25), hjust = .5,family = "Mercury", face = "bold.italic", margin = margin(b = 15)),
    legend.title = element_text(family = "Gotham Medium", face = "bold", size = 12),
    legend.text = element_text(family = "Gotham Medium", size = 6.5),
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.spacing.x  = unit(1, "cm"),
    panel.spacing.y  = unit(0, "cm"),
    plot.margin = margin(20),
    strip.text = element_markdown(color = "white"),
    legend.position = "top"
  )
)

# Annotate with Week informations
ggdraw(africa_polio_plot) + 
  draw_label(x = 0.90, y = 0.1, label = "Data from ourworldindata.org\n Tidytuesday Week-46 2021 \n Abdoul ISSA BIDA.", hjust = 1,
             fontfamily = "Gotham Medium",
             color = "white", size = 15) + 
  theme(plot.background = element_rect(fill = "#111111", color = NA))

# Saving ------------------------------------------------------------------
path <- here::here("2021_w46", "tidytuesday_2021_w46")
ggsave(filename = glue::glue("{path}.pdf"), width = 12, height = 15, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}_twitter.png"),
  dpi = 320
)


