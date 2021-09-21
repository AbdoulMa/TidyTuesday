# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggrepel)
library(patchwork)
# Data Reading and Wrangling ----------------------------------------------
nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

winners_2021 <-  nominees %>% 
  distinct(category, type, title, distributor, producer, year) %>% 
  filter(year == 2021, type == "Winner") %>% 
  mutate(
    distributor = fct_lump_n(distributor,5), 
    category = str_remove_all(category, "Outstanding |- 2021"), 
    fancy_desc = glue::glue('<span style = "color :white;"><span style = "font-size: 18px;">{str_trunc(title, 20)}</span> &bull; <span style = "color :grey90;font-size: 12px;">{str_trunc(category, 30)}</span></span>')
  ) %>% 
  filter(distributor != 'Other') %>% 
  mutate(distributor= fct_infreq(distributor),
         title = fct_infreq(title)) %>%
  arrange(distributor, title) 
  
winners_2021 <- winners_2021 %>% 
  group_by(distributor) %>% 
  mutate(
    group_id = cur_group_id(),
    index = row_number()) %>% 
  ungroup() %>% 
  relocate(group_id,index) 

evolution_2020_2021 <- nominees %>% 
  distinct(category, type, title, distributor, producer,year) %>% 
  filter(year %in% c(2020,2021), type == "Winner") %>% 
  count(year, distributor) %>% 
  pivot_wider(names_from = "year", values_from = "n") %>% 
  drop_na() %>% 
  pivot_longer(cols = c(`2020`,`2021`), names_to = 'year') %>% 
  mutate(
    label_first = ifelse(year == 2020, glue::glue("{distributor}: {value}  {emo::ji(\"trophy\")}"), NA),
    label_last = ifelse(year == 2021, glue::glue("{distributor}: {value} {emo::ji(\"trophy\")}"), NA)
  )


# Graphic -----------------------------------------------------------------
# 2021 Winners Plot 
(winners_plot <- winners_2021 %>% 
   ggplot(aes(x = 0, y = index)) +
   geom_richtext(aes(label = fancy_desc), hjust = 0,
                 fill = NA, label.color = NA, family = "Mercury Display", fontface = "bold") + 
   xlim(c(0,4)) + 
   annotate(geom = "segment", x = 0, xend = 4, y = 0, yend = 0, size = 1, color = "white") + 
   coord_cartesian(clip = "off") + 
   facet_wrap(vars(distributor), nrow = 1) + 
   scale_y_reverse(expand = expansion(mult = c(.1,0))) + 
   labs(subtitle = "2021 - 73rd Emmy Awards",
        caption = "Data from ***emmys.com***.<br>
       TidyTuesday Week-39 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**.") + 
   theme_minimal() + 
   theme( 
     plot.background = element_rect(fill = "#111111"),
     panel.grid = element_blank(),
     strip.text = element_text(size = rel(1.35), color = "white", family = "Lato Black", margin = margin(t=10, b=10)),
     axis.text = element_blank(),
     axis.title = element_blank(),
     plot.subtitle = element_text(size = rel(2.5), hjust = .5, color = "white", family = "Lato Black", margin = margin(t = 25, b = 15)),
     plot.caption = element_markdown(family = "Lato",color = "#DDDDDD", size = rel(1.15), margin = margin(t = 5,b = 10), hjust = 0)
   )
)

# Slopegraph
netflix_progress <- "Netflix has not only surpassed the 200\n million subscribers mark, it has also been \nthe distribution platform for many \naward-winning productions."
others_labels <- "For HBO and the others,\n these are the crumbs, even if\n the Apple + platform is doing\n well thanks in particular to the coach\n Ted Lasso."
(winners_change_plot <- evolution_2020_2021 %>% 
    ggplot(aes(year, value, group = distributor,color = distributor== "Netflix" )) + 
    geom_line(size = 1.5) + 
    
    geom_text_repel(aes(label = label_first), family = "Mercury Display", size = 4,fontface = "bold", direction = "y", nudge_x = -1, seed = 1234) + 
    geom_text_repel(aes(label = label_last), family = "Mercury Display", size = 4,fontface = "bold",direction = "y", nudge_x = 1, seed = 1234) + 
    annotate(geom = "segment", x = "2020", xend = "2020", y = 0, yend = 42, color = "white") + 
    annotate(geom = "segment", x = "2021", xend = "2021", y = 0, yend = 42,color = "white") + 
    annotate(geom= "text", x = "2020", y = 44, label = "2020", size = 7.5, color = "white",family = "Mercury Display", fontface = "bold")+ 
    annotate(geom= "text", x = "2021", y = 44, label = "2021", size = 7.5, color = "white",family = "Mercury Display", fontface = "bold")+ 
    annotate(geom= "text", x = "2021", y = 36.5, label = netflix_progress, size = 3, color = "white",
             family = "Mercury Display", fontface = "bold.italic", hjust = 0)+ 
    annotate(geom= "text", x = "2021", y = 25, label = others_labels, size = 3, color = "white",
             family = "Mercury Display", fontface = "bold.italic", hjust = 0)+ 
    geom_point(size = 4) + 
    scale_color_manual(
      values = c("white","#E50914"),
      guide = "none"
    ) + 
    labs(
      subtitle = "Change of EMMYS Winners between 2020 and 2021"
      
    )+ 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill =  "#111111", color = "white"), 
      plot.subtitle = element_text(size = rel(1.75), hjust = .5, color = "white", family = "Lato Black", margin = margin(t = 25, b = 15)), 
      axis.text = element_blank(),
      axis.title = element_blank()
    )
)


winners_plot + 
  inset_element(winners_change_plot, 0.4,0,.98,0.65)

# Saving ------------------------------------------------------------------
path <-  here::here("2021_w39/tidytuesday_2021_w39")

ggsave(glue::glue("{path}.pdf"), width = 21, height = 15.5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}_twitter.png"),
                      format = "png", dpi = 300)

emo::ji("trophy")
