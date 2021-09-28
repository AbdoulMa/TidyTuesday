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

# Graphic -----------------------------------------------------------------
programs_colors <- c("LS" = "#8931EF",
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
                     "HC" = "#FF4136")

text_2019 <- "In 2019, 1185 working papers
were distribued by the NATIONAL 
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
  annotate(geom = "text", x = 5, y = 54, label = "2019", color = "white",size = 10, fontface = "bold", family = "Mercury") + 
  annotate(geom = "text", x = 5, y = 51, label = text_2019, color = "white", vjust = 1,size = 4, fontface = "bold", family = "Mercury") + 
   annotate(geom = "text", x = -1, hjust = 1, y = 2,  
            label = "Labor Studies", color = "#8931EF", family = "Mercury", fontface = "bold.italic", size = 3.85) + 
   annotate(geom = "text", x = -1, hjust = 1, y = 5, 
            label = "Public Economics", color = "#F2CA19", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 9.25,
            label = "Economic Fluctuations \nand Growth", color = "#FF00BD", family = "Mercury", lineheight = .85, fontface = "bold.italic", size = 3.85) +
   annotate(geom = "text", x = -1, hjust = 1, y = 11, 
            label = "Development Economics", color = "#0057E9", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 13,
            label = "Monetary Economics", color = "#87E911", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 14.5,
            label = "Productivity, Innovation,\n and Entrepreneurship", color = "#E11845",  family = "Mercury", lineheight = .85, fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 16.5,
            label = "Health Economics", color = "#19A3FE", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 18,
            label = "Asset Pricing", color = "#FF8A12", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 19.5,
            label = "Corporate Finance", color = "#129114", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 21,
            label = "International Finance and \nMacroeconomics", color = "#39CCCC", family = "Mercury", lineheight = .85, fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 22.75,
            label = "Children", color = "#001F3F", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 24,
            label = "Industrial Organization", color = "#85144B", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 25,
            label = "Economics of Education", color = "#7FDBFF", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 26.5, 
            label = "International Trade and Investment", color = "#01FF70", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 28, 
            label = "Political Economics", color = "#B10DC9", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 29,
            label = "Health Care", color = "#FF4136",family = "Mercury", fontface = "bold.italic", size = 3.85) +     
   annotate(geom = "text", x = -1, hjust = 1, y = 31.5, 
            label = "Others", family = "Mercury", fontface = "bold.italic", size = 3.85, color = "white")  + 
   coord_fixed(ylim = c(-.5, 55),xlim = c(-15,15)) + 
  scale_fill_manual(
    values = programs_colors,
    na.value = "white",
    guide = "none"
  ) + 
  theme_void()
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
    annotate(geom = "text", x = 11, hjust = 0, y = 2,  
             label = "Labor Studies", color = "#8931EF", family = "Mercury", fontface = "bold.italic", size = 3.85) + 
    annotate(geom = "text", x = 11, hjust = 0, y = 7.5, 
             label = "Public Economics", color = "#F2CA19", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 13,
             label = "Economic Fluctuations \nand Growth", color = "#FF00BD", family = "Mercury", fontface = "bold.italic",lineheight = .85, size = 3.85) +
    annotate(geom = "text", x = 11, hjust = 0, y = 16.5, 
             label = "Development Economics", color = "#0057E9", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 18.5,
             label = "Monetary Economics", color = "#87E911", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 21.5,
             label = "Productivity, Innovation,\n and Entrepreneurship", color = "#E11845",  family = "Mercury", fontface = "bold.italic",lineheight = .85, size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 25,
             label = "Health Economics", color = "#19A3FE", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 28,
             label = "Asset Pricing", color = "#FF8A12", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 31,
             label = "Corporate Finance", color = "#129114", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 33.5,
             label = "International Finance and \nMacroeconomics", color = "#39CCCC", family = "Mercury", fontface = "bold.italic", lineheight = .85, size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 35.85,
             label = "Children", color = "#001F3F", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 37.5,
             label = "Industrial Organization", color = "#85144B", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 39,
             label = "Economics of Education", color = "#7FDBFF", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 41, 
             label = "International Trade and Investment", color = "#01FF70", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 42.75, 
             label = "Political Economics", color = "#B10DC9", family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 45,
             label = "Health Care", color = "#FF4136",family = "Mercury", fontface = "bold.italic", size = 3.85) +     
    annotate(geom = "text", x = 11, hjust = 0, y = 48, 
             label = "Others", family = "Mercury", fontface = "bold.italic", size = 3.85, color = "white")  + 
    annotate(geom = "text", x = 5, y = 54, label = "2020", color = "white", size = 10, fontface = "bold", family = "Mercury") + 
  coord_fixed(ylim = c(-.5, 55),xlim = c(-5,25)) + 
  scale_fill_manual(
    values = programs_colors,
    na.value = "white",
    guide = "none"
  )
)

# Combine plots
papers_19_plot +  papers_20_plot + 
  plot_annotation(title = "Researches papers and programs in pandemic time", 
                  subtitle = "The Covid 19 pandemic has significantly boosted working papers distribution 
by the National Bureau of Economic Research. From an average of 1,200, the bureau 
registered a record of 1,713 papers, 477 more than the previous record of 2018.") + 
labs(
  caption = "Data from ***NBER*** by way of Ben Davies.<br>
        * Each circle represents 8 papers submitted.<br>
       Tidytuesday Week-40 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
) &
  theme(
        plot.background = element_rect(fill = "#111111", color = NA),
        plot.title = element_text(size = rel(2.15), family = "Gotham Black",color = "white", hjust = 0),
        plot.subtitle = element_text(size = rel(1.25),family = "Mercury", face ="bold", color = "white", hjust = 0, margin = margin(t = 10, b = 0)),
        plot.caption = element_markdown(color = "white", size = rel(.95), family = "Gotham Bold", margin = margin(t = 5,b = 10), hjust = 1),
        plot.margin = margin(t = 20, r = 10, l = 10))


# Saving ------------------------------------------------------------------
path <-  here::here("2021_w40/tidytuesday_2021_w40")
ggsave(glue::glue("{path}.pdf"), width = 11, height = 11, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)
