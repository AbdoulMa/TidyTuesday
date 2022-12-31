
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(waffle)
library(cowplot)

# Fix Trouble with ggdraw and element_markdown
# https://github.com/wilkelab/cowplot/issues/167
set_null_device("pdf")
set_null_device("png")
set_null_device("cairo")
set_null_device("agg")


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
         rank_at_graduation == '2nd Lt' ~ "i",
         rank_at_graduation == 'Flight Officer' ~ "p",
         rank_at_graduation == '1st Lt' ~ 'y',
         TRUE ~ 'Y'
      ),
      rank_label = factor(rank_label, levels = c("i","p","y","Y"))
   ) 

main_states <- levels(airmen$State)[1:25]


# Graphic -----------------------------------------------------------------
f1 <- "Gotham Bold"
f2 <- "MercurySSm-Bold"

graduation_labels <- "<span style='font-family:SoldierWW2;font-size: 45px;'>i</span>2nd Lt <span style='font-family:SoldierWW2;font-size: 45px;'>p</span>Flight Officer <span style='font-family:SoldierWW2;font-size: 45px;'>y</span>1st Lt <span style='font-family:SoldierWW2;font-size: 45px;'>Y</span>Captain<br>
Data from Tuskegee Airmen Challenge.<br>
Tidytuesday Week-6 2022 &bull;Abdoul ISSA BIDA &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
(plot <- airmen %>% 
    mutate(State = fct_other(State, keep = main_states, other_level = "Other states")) %>%
    group_by(State) %>% 
    mutate(nb_pilots = n()) %>%  
    ungroup() %>% 
    mutate( 
       state_label = glue::glue("**<span style='font-size:20px;'>{State}</span><br><span style='color:#353535;font-size:12.5px;'>{nb_pilots}</span>**"),
       state_label = fct_infreq(state_label),
       # TODO find a better way
       state_label = fct_relevel(state_label, "**<span style='font-size:20px;'>Other states</span><br><span style='color:#353535;font-size:12.5px;'>74</span>**", after = Inf), 
       pilot_type = fct_recode(pilot_type, 
                               "Liaison pilot" = "Liason pilot")
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
       size = 10,
       family = "SoldierWW2",
       alpha = .85
    ) + 
    facet_wrap(vars(state_label), ncol = 1, strip.position = "left") + 
    scale_color_manual(
       values = c(
          "Single engine" = "#c30038", 
          "Twin engine" =  "#90cb00",
          "Liaison pilot" = "#5c58b2",
          "Service pilot" = "#221f20"
       ),
       guide = guide_legend(
          title = "Pilot Type", 
          title.hjust = .5,
          title.theme = element_text(family = f1, colour = "#111111"),
          label.theme = element_text(family = f1, colour = "#111111"),
          nrow = 1
       )
    ) + 
    labs(
       caption = graduation_labels
    ) + 
    coord_cartesian(expand = F,clip = 'off')+
    theme_minimal() + 
   theme(
     panel.grid = element_blank(),
     panel.spacing.y = unit(.5, "cm"),
     axis.text = element_blank(),
     axis.title = element_blank(),
     plot.caption = element_markdown(size = rel(1.15), family = f1, colour = "#111111"),
     strip.text.y.left =  element_markdown(angle = 0, hjust = 1, family = f1),
     legend.position = c(.675, .1),
     plot.margin = margin(t= 1.5, r = 1, b= 1.5 , unit = "cm")
   )
)

# Insert Image and label
ggdraw(plot) +
   draw_image(image = here::here("2022_w6", "tuskgee_airmen.png"), x = .25, y = .15, scale = .4, hjust = 0) +
   draw_label(x = .7, y = .42, label =  "Tuskegee Airmen", size = 50, fontfamily = f1, color = "#111111") +
   draw_label(x = .7, y = .35, label =  "Mobilization of African-American military pilots\n during World War II.", size = 20, fontfamily = f2, color = "#111111") + 
   theme(
      plot.background = element_rect(fill = "#f8f7f5", color = NA)
   )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w6", "tidytuesday_2022_w6")
ggsave(filename = glue::glue("{path}.pdf"), width = 13.5, height = 15, device = cairo_pdf)

pdftools::pdf_convert(
   pdf = glue::glue("{path}.pdf"),
   filenames = glue::glue("{path}.png"),
   dpi = 300
)

