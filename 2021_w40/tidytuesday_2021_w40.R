# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggforce)
# Data Reading and Wrangling ----------------------------------------------
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

glimpse(paper_programs)
# Graphic -----------------------------------------------------------------

papers_19_20 <- papers %>% 
  filter(between(year,2019,2020)) %>% 
  left_join(paper_programs, by = "paper") %>%
  count(year, program, sort = T) %>% 
  mutate(n = n/4)
  
repeat_programs <- function(program, n){
  tibble(
    program = rep(program, n)
  ) 
}

papers_19_circles <- papers_19_20 %>% 
  filter(year == 2019) %>% 
  mutate(program = fct_reorder(program, n)) %>% 
  arrange(desc(program)) %>% 
  select(program, n) %>%
  pmap_df(~repeat_programs(.x,.y)) %>% 
  mutate(row_num = row_number(),
         row_num = row_num - 1) %>% 
  mutate(x = row_num %% 10,
         y = row_num %/% 10) %>% 
  relocate(row_num) 

papers_19_circles %>% 
  ggplot() + 
  geom_circle(aes(x0= x, y0= y, r = .495, fill = program), size = .25, color = "#111111") + 
  theme_void() +
  coord_fixed(ylim = c(-.5, 105)) +
  scale_fill_manual(
    values = c("LS" = "#8931EF",
               "PE" = "#F2CA19",
               "EFG" = "#FF00BD",
               "DEV" = "#0057E9",
               "ME" = "#87E911" , 
               "PR"  = "#E11845",
               "HE" = "#19A3FE",
               "AP" = "#FF8A12",
               "CF" = "#129114",
               "IFM" = "#39CCCC",
               "CH" = "#001F3F",
               "IO" = "#85144B", 
               "ED" = "#7FDBFF",
               "ITI"= "#01FF70",
               "POL" = "#B10DC9",
               "HC" = "#FF4136"),
    na.value = "white",
    guide = "none"
  )


program_levels <- levels(sort(papers_19_circles$program))

program_levels
papers_20_circles <- papers_19_20 %>% 
  filter(year == 2020) %>% 
  mutate(program = factor(program, levels = program_levels)) %>% 
  arrange(desc(program)) %>% 
  select(program, n) %>%
  pmap_df(~repeat_programs(.x,.y)) %>% 
  mutate(row_num = row_number(),
         row_num = row_num - 1) %>% 
  mutate(x = row_num %% 10,
         y = row_num %/% 10) %>% 
  relocate(row_num) 

papers_20_circles %>% 
  ggplot() + 
  geom_circle(aes(x0= x, y0= y, r = .495, fill = program), size = .25, color = "#111111") +
  theme_void() +
  coord_fixed(ylim = c(-.5, 105)) + 
  scale_fill_manual(
    values = c("LS" = "#8931EF",
               "PE" = "#F2CA19",
               "EFG" = "#FF00BD",
               "DEV" = "#0057E9",
               "ME" = "#87E911" , 
               "PR"  = "#E11845",
               "HE" = "#19A3FE",
               "AP" = "#FF8A12",
               "CF" = "#129114",
               "IFM" = "#39CCCC",
               "CH" = "#001F3F",
               "IO" = "#85144B", 
               "ED" = "#7FDBFF",
               "ITI"= "#01FF70",
              "POL" = "#B10DC9",
              "HC" = "#FF4136"),
    na.value = "white",
    guide = "none"
  )


# Saving ------------------------------------------------------------------
