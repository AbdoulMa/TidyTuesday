
# Load libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(sf)
library(albersusa)
library(PNWColors)
library(patchwork)
library(here)

# Load Data & Wrangling ---------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 20)
broadband <- pluck(tuesdata, "broadband")

broadband <- broadband %>% 
  mutate(across(4:5, parse_double)) 

# Compute broadband means by state
states_broadband_mean <- broadband %>% 
  group_by(ST) %>%
  summarise(across(3:4, ~mean(.x, na.rm =T))) %>% 
  select(
    ST,
    mean_bb_per_ffc = `BROADBAND AVAILABILITY PER FCC`,
    mean_bb_usage = `BROADBAND USAGE`
  )  

broadband <- broadband %>%
  select(fips = `COUNTY ID`,
         bb_per_ffc = `BROADBAND AVAILABILITY PER FCC`,
         bb_usage = `BROADBAND USAGE`, everything())  %>% 
  mutate(
  fips = case_when(fips < 9999 ~ str_c("0",as.character(fips)), 
                   TRUE ~ as.character(fips))
  )

# Load counties simple features for the map
counties <- albersusa::counties_sf()
counties <- counties %>% 
  mutate(fips = as.character(fips))
# Joining with initial dataser  
broadband_sf <- counties %>%  
  left_join(broadband, by = "fips")

# Make a summary for get the percentage of area covered (after Cutting broadband usage rate)
census_area_summary <- broadband_sf %>%
    mutate(bb_usage_fct = cut(bb_usage, breaks = 2, labels = c("low", "high"))) %>%
    group_by(bb_usage_fct) %>%
    summarise(
      census_area_covered = sum(census_area) 
    ) %>% 
    mutate(
      pct_area_covered = census_area_covered/sum(census_area_covered),
      pct_area_covered = round(pct_area_covered*100,2)
    )

low_pct <- census_area_summary %>% 
  filter(bb_usage_fct == "low") %>% 
  pull(pct_area_covered)

high_pct <- census_area_summary %>% 
  filter(bb_usage_fct == "high") %>% 
  pull(pct_area_covered)

cut_value <- median(range(broadband_sf$bb_usage, na.rm = T))
cut_value <- round(cut_value*100,2)

# Make a summary
summary_text <-  glue::glue('As of <b>November 2019</b>, the United States can be divided into two. <br>
   A first part (more than <b>{low_pct}%</b> of the territory),
   where the percentage of population <br> using the internet at broadband speeds 
   does not exceed <b>{cut_value}%</b>. <br> And a second one (approximatively <b>{high_pct}%</b> of the territory), 
   where the use of higher speed is more regular.')



# Plotting ----------------------------------------------------------------
# Define a special theme for my 2 plots
my_theme <-  theme_minimal(base_family = "Source Sans Pro") + 
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text =  element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20,r =10,b = 20,l = 20),
    plot.title = element_markdown(size = rel(1.8), face = "bold", hjust = .5, margin = margin(b = 20)),
    plot.subtitle = element_markdown(size = rel(1.2), lineheight = 1.2, family = "Bembo Std",face = "italic", hjust = .5),
    plot.caption = element_markdown(family = "Source Sans Pro",size = rel(1))
  )
  
# First plot : The map
map <- broadband_sf %>%  
  ggplot() +
  geom_sf( aes(fill = bb_usage),color = "#b2b2b2", size = 0.1) +
  labs(
    title = "Percentage of people per county that use the internet at broadband speeds",
    subtitle = summary_text,
    fill = NULL
  ) + 
  scale_fill_gradientn(colours = pnw_palette("Bay",100), guide = "coloursteps",
                       breaks = c(.2,.4,.6,.8), 
                       labels = c("≤ 20%","","","≥ 80%")) +
  coord_sf(crs = "ESRI:102003") +
  my_theme +
  theme(legend.position = c(0.985, 0.5), 
        legend.text = element_text(face = "bold"))
# Display it
map


# Second plot : state broadband distribution
distribution <- states_broadband_mean  %>%
  arrange(mean_bb_per_ffc) %>%
  mutate(
    mean_bb_per_ffc = round(mean_bb_per_ffc*100,0)
  ) %>% 
  group_by(mean_bb_per_ffc) %>% 
  summarize(
    states = str_c(ST, collapse = ",")
  ) %>% 
  mutate(
    r_num = row_number(),
    vpos = case_when(r_num %% 2 == 0 ~ 0.95,
                     TRUE ~ 1.05),
    vjust = case_when(r_num %% 2 == 0 ~ 1,
                      TRUE ~ 0),
    label = glue::glue( '<span style = "color : black;"><span>{states}</span><br><b><span style = "font-size:12px;">{mean_bb_per_ffc}%</span></b></span>')
  ) %>% 
  ggplot() + 
  annotate(geom = "segment", x = 55, xend = 105, y = 1, yend = 1, size = 1.1, color = "grey75") +
  geom_richtext(aes( mean_bb_per_ffc, y = vpos,label = label, vjust = vjust ), fill = NA, label.colour = NA, family = "Source Sans Pro") +
  geom_segment(aes(x = mean_bb_per_ffc, xend = mean_bb_per_ffc, y = 1, yend = vpos ), color = "grey60") +
  geom_point(aes(x = mean_bb_per_ffc, y = 1), size = 4, color = "#DD4124") + 
  annotate(geom = "text", x = 97, y = 1.8,
           family = "Bembo Std",
           fontface = "italic",
           size = 4,
           label = "The inhabitants of the East Coast are clearly \nthe best served by fixed terrestrial broadband.")  +
  labs(
    title = "Average of people per state with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps",
    caption = "Data from  Microsoft.<br>
      Tidytuesday Week-20 2021 | <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**"
  ) + 
  scale_y_continuous(
  name = NULL,
  limits = c(0.45,2.0)
  ) + 
  my_theme 
# Display it
distribution

# Patchwork 
map / distribution + 
  plot_layout(nrow = 2, heights = c(5,3))

# save the plot
ggsave(here::here("2021_w20","tidytuesday_2021_w20.png"), width = 15, height = 12, dpi = 320)
