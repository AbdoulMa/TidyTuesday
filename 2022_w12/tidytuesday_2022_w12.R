
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(babynames)
library(ggtext)
library(glue)
library(gggrid)

# Data Wrangling ----------------------------------------------------------
popular_unisex_names <- c("Jessie","Marion","Jackie","Alva","Ollie",
                          "Jody","Cleo","Kerry","Frankie","Guadalupe",
                          "Carey","Tommie","Angel","Hollis","Sammie",
                          "Jamie","Kris","Robbie","Tracy","Merrill",
                          "Noel","Rene","Johnnie","Ariel","Jan",
                          "Devon","Cruz","Michel","Gale","Robin",
                          "Dorian","Casey","Dana","Kim","Shannon"
)


babynames_df <- babynames %>% 
  select(-prop) %>%
  filter(name %in% popular_unisex_names) %>% 
  filter(year > 1925)

unisex_babynames <- babynames_df %>% 
  pivot_wider(names_from = sex, values_from = n) %>% 
  drop_na(`F`,`M`) %>% 
  mutate(
    prop_f = `F`/(`F`+`M`), # Proportion of girls
    prop_m = `M`/(`F`+`M`) # Proportion of boys 
  )


# Compute Proportions distance Means for order names 
(most_unisex_props_means <-  unisex_babynames %>% 
    mutate(prop_dist = abs(prop_f-prop_m)) %>% 
    group_by(name) %>% 
    summarise(prop_dist_mean = mean(prop_dist)) %>% 
    mutate(rank = rank(prop_dist_mean)) %>% 
    arrange(rank))

# Order names by proortion distance means 
unisex_babynames <- unisex_babynames %>% 
  left_join(most_unisex_props_means, by = "name") %>% 
  mutate(
    name_ranked = glue("{rank}. {name}"),
    name_ranked = fct_reorder(name_ranked,rank)
  )

# Determine most unisex year for each name 
most_unisex_year <- unisex_babynames  %>% 
  mutate(prop_dist = abs(prop_f-prop_m)) %>% 
  group_by(name_ranked) %>% 
  slice_min(prop_dist,n=1, with_ties = F) %>% 
  ungroup() 

# Function for normalize between range ( I use that inside grobs for panels annotations )
range_between <-  function(x,range){ (x-min(range)) / (max(range)-min(range)) }

# Jackie Robinson introduction to MLB 
j_rob_in_mlb <- unisex_babynames %>% 
  filter(year == 1947, name == 'Jackie')
  
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w12", "tidytuesday_2022_w12")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

