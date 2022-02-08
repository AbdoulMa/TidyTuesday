
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(waffle)
library(cowplot)



# Data Wrangling ----------------------------------------------------------
airmen <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv")
states_abbreviation <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")

airmen <- airmen %>%
  left_join(states_abbreviation, by = c("state" = "Abbreviation")) %>% 
  filter(!rank_at_graduation %in% c("N/A", "Unk"), state != "Unk") %>% 
  drop_na(rank_at_graduation, State) %>% 
  mutate( 
    State = fct_infreq(State),
    pilot_type = fct_infreq(pilot_type),
    rank_at_graduation = fct_recode(rank_at_graduation, 
                                    "Captain" = "Capt"
    ), 
    rank_label = case_when(
      rank_at_graduation == '2nd Lt' ~ 'a',
      rank_at_graduation == 'Flight Officer' ~ 'b',
      rank_at_graduation == '1st Lt' ~ 'c',
      TRUE ~ 'd'
    ),
    rank_label = factor(rank_label, levels = c("a","b","c","d"))
  ) 

main_states <- levels(airmen$State)[1:25]


# Graphic -----------------------------------------------------------------
(plot <- airmen %>% 
   mutate(State = fct_other(State, keep = main_states, other_level = "Other states")) %>%
   group_by(State) %>% 
   mutate(nb_pilots = n()) %>%  
   ungroup() %>% 
   mutate( 
     state_label = glue::glue('{State} <br> {nb_pilots}'),
     state_label = fct_infreq(state_label),
     state_label = fct_relevel(state_label, "Other states <br> 74", after = Inf)
   ) %>% 
   arrange(pilot_type, rank_label) %>% 
   ggplot() + 
   stat_waffle(
     aes(fill = rank_label, 
         label = rank_label,
         values = 1,
         color = pilot_type),
     n_rows = 2,
     geom = "text",
     size = 6,
     fontface = "bold"
   ) + 
   facet_wrap(vars(state_label), ncol = 1, strip.position = "left") + 
   coord_equal()+
   theme_minimal() + 
   theme(
     panel.grid = element_blank(),
     axis.text = element_blank(),
     axis.title = element_blank(),
     strip.text.y.left =  element_markdown(angle = 0)
   )
)


cowplot::ggdraw(plot)


# Saving ------------------------------------------------------------------
path <- here::here("2022_w6", "tidytuesday_2022_w6")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

