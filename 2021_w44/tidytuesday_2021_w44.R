# Load libraries ----------------------------------------------------------
library(tidyverse)
library(countrycode)
library(ggtext)
library(patchwork)
# Data Reading and Wrangling ----------------------------------------------
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

ultra_rankings
# Graphic -----------------------------------------------------------------

ultra_rankings %>% 
  count(nationality, sort = T)

ultra_representations <- ultra_rankings %>% 
  distinct(runner,.keep_all = T) %>% 
  filter(!is.na(gender)) %>% 
  count(nationality, gender) %>% 
  pivot_wider(names_from = gender, values_from = n ) %>% 
  drop_na() %>% 
  mutate(sum = M + W) %>% 
  arrange(-sum) %>% 
  slice_max(order_by = sum, n= 15) %>% 
  mutate(across(c(M, W), .fns = list(prop = ~ .x /sum), .names = "{.fn}_{.col}"))


ultra_representations <- ultra_representations %>% 
  mutate(country_name = countrycode(nationality, origin = 'iso3c', destination = 'country.name'), 
         country_name = case_when(nationality == "GER" ~ "Germany", 
                                  str_detect(country_name, "Hong\\sKong") ~ "Hong Kong", 
                                  TRUE ~ country_name)
  )

summary <- "USA, France and United Kingdom have the highest proportions of world's ultra runners. But, when we take the number of runners from the country compared to its population France is largely at top."
summary <- str_wrap(summary, 50) %>% str_replace_all("\n","<br>")
(countries_rep_plot  <- ultra_representations %>% 
    ggplot() + 
    geom_col(aes(x = sum, y = fct_reorder(nationality, sum))) + 
    geom_text(aes(x = 500, y = fct_reorder(nationality, sum), label = country_name),
              hjust = 0) + 
    labs( 
      x = NULL,
      y = NULL,
      subtitle = "TOP 15 Countries with the most ultra Trail Runners") + 
    annotate(geom = "richtext", x = 13500, y = 5, label = summary) + 
    coord_cartesian() + 
    scale_x_continuous(
      expand = expansion(add = 0),
      position = "top", 
      breaks = 5000*1:4
    ) + 
    theme_minimal() + 
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "#111111"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(size = .75)
    )
)

(countries_gender_plot <- ultra_representations %>% 
    mutate(nationality = fct_reorder(nationality, prop_W)) %>% 
    pivot_longer(cols = c(prop_M, prop_W), names_to = "prop", values_to = "gender_proportion") %>% 
    mutate(prop = fct_relevel(prop, c("prop_W", "prop_M"))) %>% 
    ggplot(aes(x = gender_proportion, fill = prop, y = nationality)) + 
    geom_col() + 
    geom_text(aes(x = ifelse(prop == "prop_M", 0.01, 0.99),label = glue::glue("{round(gender_proportion, 2)*100}%"), group =prop, hjust = ifelse(prop == "prop_M", 0, 1)), color = "white") + 
    labs(subtitle = "Gender Distribution by country") + 
    coord_cartesian(expand = F) + 
    theme_minimal() + 
    theme()
) 

glimpse(race)
glimpse(ultra_rankings)
ultra_rankings_speed <- ultra_rankings %>% 
  left_join(race) %>% 
  filter(!is.na(gender),!is.na(time_in_seconds), !is.na(distance), distance != 0) %>% 
  mutate(mile_sp = time_in_seconds / distance )

ultra_rankings_speed %>% 
  group_by(gender) %>% 
  summarise(
    n = n(),
    mean = mean(mile_sp)
  )

men_summary  <- "<span>Men</span><br> 96,502 finishers<br> 12,63mins/Km on average"
women_summary  <- "<span>Men</span><br> 16993 finishers<br> 12,16mins/Km on average"

(finisher_plot <- ultra_rankings_speed %>% 
  ggplot() +
  geom_histogram(aes(x = mile_sp,fill = gender), binwidth = 60,color = "white", alpha = .5) + 
  scale_x_time(
    name = "Finish time (each bar is one minute)",
    labels = scales::time_format("%M mins"), 
               breaks = seq(5,30, by = 5)*60
               ) + 
  annotate(geom = "richtext", x = 1350, y = 13500, label = men_summary) + 
  annotate(geom = "richtext", x = 1350, y = 10000, label = women_summary) + 
  coord_cartesian(expand = F) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(size =.5),
    axis.ticks = element_line(size =.5), 
    axis.ticks.length = unit(.25, "cm")
  )
) 
  
(countries_rep_plot + countries_gender_plot ) / finisher_plot

# Saving ------------------------------------------------------------------
