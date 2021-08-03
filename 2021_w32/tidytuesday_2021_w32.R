# Load libraries ----------------------------------------------------------
library(tidyverse)

# Data Reading and Wrangling ----------------------------------------------
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')
 athletes <- athletes %>% 
  mutate(abb = case_when(abb == "URS" ~ "RUS",
                         abb == "FRG" ~ "GER",
                         TRUE ~ abb))
 
 top_federations <- athletes %>% 
   filter(medal == "Gold") %>% 
   count(abb, sort = T) %>% 
   head(20)

 top_federations_list <- top_federations %>% 
   pull(abb)
 
 top_federations_list
  medals_records <- athletes %>% 
  filter(medal == "Gold", abb %in% top_federations_list) %>% 
  distinct(abb, year,event , .keep_all = T) %>% 
  count(abb, year,  sort = T)  %>% 
    complete(abb, year, fill = list(n = 0)) %>%
    group_by(year) %>%
  mutate(rk = rank(-n, ties.method = "first")) %>%
  ungroup(year) %>%
  arrange(year)
  
medals_records

# top_federations <- medals_records %>% 
#   count(abb, wt = n, sort = T) %>% 
#   head(6) %>% 
#   select(-n) %>% 
#   mutate(
#     primary_clr = c("#3c3b6e", "#DE2910","#001f7e", "#000000","#ff0000","#0055A4"),
#     secondary_clr = c("#b22234","#FFDE00","#FFFFFF","#ffce00", "#FFFFFF","#EF4135")
#   )



# Graphic -----------------------------------------------------------------
medals_records %>% 
  # left_join(top_federations) %>% 
  # mutate(across(.cols = contains("clr"),.fns = ~ ifelse(is.na(.),"#000000", .))) %>% 
  ggplot(aes(year, rk)) + 
  geom_line(aes(group = abb, alpha = ifelse(abb %in% c("USA","CHN"), 1, .4),
                size = ifelse(abb %in% c("USA","CHN"), 2.5, 1))) +
  scale_y_reverse() +
  scale_alpha_identity() + 
  # scale_color_identity() + 
  scale_size_identity()
  


# Saving ------------------------------------------------------------------
