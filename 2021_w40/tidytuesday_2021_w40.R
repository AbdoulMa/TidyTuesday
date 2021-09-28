# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)
library(patchwork)
# Data Reading and Wrangling ----------------------------------------------
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

glimpse(paper_programs)
# Graphic -----------------------------------------------------------------

papers %>% 
  filter(between(year,2019,2020)) %>% 
  count(year)

papers_19_20 <- papers %>% 
  filter(between(year,2019,2020)) %>% 
  left_join(paper_programs, by = "paper") %>%
  count(year, program, sort = T) %>% 
  mutate(n = n/8)
  
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

text_2019 <- "In 2019, 1185 working papers
were published to the NATIONAL 
BUREAU OF ECONOMIC 
REASEARCH(NBER) which 
corresponds to the average.
But, apparently, something
happened in 2020 ?
and boosted the publications."
(papers_19_plot <- papers_19_circles %>% 
  ggplot() + 
  geom_circle(aes(x0= x, y0= y, r = .495, fill = program), size = .25, color = "#111111") + 
    scale_y_continuous(breaks = seq(0,50,by = 5)) + 
  annotate(geom = "text", x = 5, y = 54, label = "2019", color = "white") + 
  annotate(geom = "text", x = 5, y = 51, label = text_2019, color = "white", vjust = 1) + 
  coord_fixed(ylim = c(-.5, 55), xlim = c(-5,15)) +
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
  ) + 
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA)
  )
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

(papers_20_plot <- papers_20_circles %>% 
  ggplot() + 
  geom_circle(aes(x0= x, y0= y, r = .495, fill = program), size = .25, color = "#111111") +
  theme_void() +
    annotate(geom = "text", x = 5, y = 2,  
             label = "Labor Studies", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) + 
    annotate(geom = "text", x = 5, y = 7.5, 
             label = "Public Economics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 13,
             label = "Economic Fluctuations \nand Growth", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +
    annotate(geom = "text", x = 5, y = 16.5, 
             label = "Development Economics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 18.5,
             label = "Monetary Economics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 21.5,
             label = "Productivity, Innovation,\n and Entrepreneurship", color = "#DDDDDD",  fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 25,
             label = "Health Economics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 28,
             label = "Asset Pricing", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 31,
             label = "Corporate Finance", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 33.5,
             label = "International Finance and \nMacroeconomics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 35.85,
             label = "Children", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 37.5,
             label = "Industrial Organization", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 39,
             label = "Economics of Education", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 4.65, y = 41, 
             label = "International Trade and Investment", color = "grey15", fontface = "bold.italic", size = 2.75) +     
    annotate(geom = "text", x = 4.5, y = 42.75, 
             label = "Political Economics", color = "#DDDDDD", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 45,
             label = "Health Care", color = "#DDDDDD",fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 48, color = "#111111", 
             label = "Others", fontface = "bold.italic", size = 3.25)  + 
    annotate(geom = "text", x = 5, y = 54, label = "2020", color = "#DDDDDD") + 
  coord_fixed(ylim = c(-.5, 55),xlim = c(-5,15)) + 
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
  ) + 
    theme(
      plot.background = element_rect(fill = "#111111", color = NA)
    )
)

(labels <- ggplot() + 
 annotate(geom = "text", x = 5, y = 5, color = "#8931EF", 
          label = "Labor Studies", fontface = "bold.italic", size = 3.25) + 
    annotate(geom = "text", x = 5, y = 15, color = "#F2CA19", 
             label = "Public Economics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 26, color = "#FF00BD", 
             label = "Economic Fluctuations and Growth", fontface = "bold.italic", size = 3.25) +
    annotate(geom = "text", x = 5, y = 34, color = "#0057E9", 
             label = "Development Economics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 39, color = "#87E911", 
             label = "Monetary Economics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 45, color = "#E11845", 
             label = "Productivity, Innovation,\n and Entrepreneurship", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 52, color = "#19A3FE", 
             label = "Health Economics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 57, color = "#FF8A12", 
             label = "Asset Pricing", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 64, color = "#129114", 
             label = "Corporate Finance", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 69, color = "#39CCCC", 
             label = "International Finance and \nMacroeconomics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 73, color = "#001F3F", 
             label = "Children", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 76, color = "#85144B", 
             label = "Industrial Organization", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 80, color = "#7FDBFF", 
             label = "Economics of Education", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 83, color = "#01FF70", 
             label = "International Trade and Investment", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 87.25, color = "#B10DC9", 
             label = "Political Economics", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 92, color = "#FF4136", 
             label = "Health Care", fontface = "bold.italic", size = 3.25) +     
    annotate(geom = "text", x = 5, y = 100, color = "#111111", 
             label = "Others", fontface = "bold.italic", size = 3.25)  + 
    coord_fixed(xlim = c(-20,25), ylim = c(-.5,105)) + 
    theme_void()
)

papers_19_plot +  papers_20_plot &
  theme(plot.background = element_rect(fill = "#111111", color = NA))
# Saving ------------------------------------------------------------------
