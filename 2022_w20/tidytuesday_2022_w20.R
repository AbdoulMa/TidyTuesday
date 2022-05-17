
# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------
eurovision_votes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv")

eurovision_2022 <- eurovision_votes %>% 
  filter(year == 2022, semi_final == 'f') %>% 
  group_by(from_country) %>% 
  mutate(rk_grp = case_when(points == 12 ~ "1st", 
                          points == 10 ~ "2nd",
                          points == 8 ~ "3rd", 
                          between(points, 4, 7) ~ "4th-7th", 
                          between(points, 1, 3) ~ "8th-10th", 
                          TRUE ~ "Not classed"
  ), 
  rk_grp = fct_relevel(rk_grp, levels = c("1st","2nd","3rd", "4th-7th", "8th-10th", "Not classed")))

eurovision_2022_standings <- eurovision_2022 %>% 
  group_by(to_country) %>% 
  summarize(total_points = sum(points)) %>% 
  ungroup() %>% 
  mutate(event_rk = rank(desc(total_points), ties.method = "first"), 
         fancy_rk = glue::glue("#{event_rk}, {to_country}")) 

eurovision_2022_pts_prop <- eurovision_2022 %>% 
  group_by(to_country, jury_or_televoting) %>% 
  summarise(nb_points = sum(points)) %>% 
  ungroup() 


final_eurovision_2022_pts_prop <- eurovision_2022_pts_prop %>% 
  group_by(to_country) %>% 
  mutate(points_prop = prop.table(nb_points), 
         prop_start = lag(points_prop)) %>% 
  replace_na(list(prop_start = 0))  %>% 
  left_join(eurovision_2022_standings)

eurovision_2022 %>% 
  group_by(to_country, rk_grp) %>% 
  count() %>% 
  ungroup() %>% 
  # group_by() %>% 
  arrange(to_country, desc(rk_grp)) %>% 
  group_by(to_country) %>% 
  mutate(indice_start = lag(cumsum(n))) %>% 
  replace_na(list(indice_start = 0)) %>% 
  rowwise() %>% 
  mutate(indice = list(indice_start:(indice_start + n -1))) %>% 
  unnest_longer(indice) %>% 
  select(-c(n, indice_start)) %>% 
  mutate(
    xind = indice %% 13,
  )


# Graphic -----------------------------------------------------------------
final_eurovision_2022_pts_prop %>% 
  ggplot() + 
  geom_rect(aes(xmin = prop_start, xmax = prop_start + points_prop, ymin = 0, ymax = 1, fill = jury_or_televoting)) + 
  facet_wrap(vars(fancy_rk))


# Saving ------------------------------------------------------------------
path <- here::here("2022_w20", "tidytuesday_2022_w20")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

