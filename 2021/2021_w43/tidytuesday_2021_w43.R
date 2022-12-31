# Load libraries ----------------------------------------------------------
library(tidyverse)
library(packcircles)
library(ggtext)
library(patchwork)

# Data Reading and Wrangling ----------------------------------------------
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
pumpkins <- pumpkins %>% 
  separate(id, into = c("year", "type"), sep = "-") %>% 
  mutate(weight_lbs = parse_number(weight_lbs)) %>%  
  filter(!is.na(weight_lbs))

# Types DF
types_df  <- tribble(
  ~abbr, ~name,
  "F", "Field Pumpkin", 
  "P" , "Giant Pumpkin", 
  "S" , "Giant Squash", 
  "W", "Giant Watermelon", 
  "L",  "Long Gourd",
  "T", "Tomato"
)

# Define Types Filling Colors Palette 
type_fillings <- c("F" = "#ffbe0b",
                   "P" = "#ff5400",
                   "L" = "#ff0054",
                   "S" = "#390099",
                   "T" = "#3a86ff",
                   "W" = "#2ec4b6")

# Only keep 2021 Weigh-off  
pumpkins_2021 <- pumpkins %>% filter(year == 2021) %>% 
  # Important to arrange and generate the id 
  # with row number (it will be used for the join 
  # after the generation of the circles) 
  arrange(weight_lbs) %>% 
  mutate(id = row_number()) %>% 
  relocate(id)


# Summarise some informations 
pumpkins_2021_summary <- pumpkins_2021 %>% 
  group_by(type) %>% 
  summarise(
    n = n(),
    max = max(weight_lbs, na.rm = T),
    nb_c = n_distinct(country),
    mean = mean(weight_lbs, na.rm =T)
  )  %>% 
  left_join(types_df, by =c("type" = "abbr"))


# Circles Generation
set.seed(1234)
packing <- circleProgressiveLayout(pumpkins_2021$weight_lbs)

# Graphic -----------------------------------------------------------------
(summary_plot <-  pumpkins_2021_summary  %>% 
   mutate(type = fct_reorder(type, -mean)) %>% 
   ggplot() + 
   geom_rect(
     aes(fill = type), 
     xmin = -1, ymin = -1, xmax = 1, ymax = 1, color = "white"
   ) + 
   geom_richtext(aes(label = glue::glue("<span style='font-size: 17.5px;font-family: \"Gotham Black\";'>{str_wrap(name,10) %>% str_replace_all('\n', '<br>')}</span><br>
                                        <span style='font-size: 15px; font-family:\"Mercury Display\";'> **{n}** specimens<br> from {nb_c} countries<br> Best :<b>{round(max,2)}</b> {ifelse(type == \"L\", \"inches\", \"lbs\")} <br> Avg : {round(mean,2)} {ifelse(type == \"L\", \"inches\", \"lbs\")}</span>")),
                 hjust = 0.5,
                 size = 3,
                 color = "white",
                 x = 0, y = 0.05,
                 lineheight = 1.5,
                 fontface = "bold",
                 fill = NA, label.colour = NA, show.legend = F) + 
   coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
   scale_fill_manual(values = type_fillings, 
                     guide = "none") + 
   theme_minimal() + 
   theme(panel.spacing = unit(-5,"points"),
         strip.text = element_blank()) +
   facet_wrap(vars(type), nrow = 1))

(circles_plot <- packing %>% 
    arrange(radius) %>% 
    mutate(id = row_number())  %>% 
    left_join(pumpkins_2021) %>% 
    ggplot() + 
    ggforce::geom_circle(aes(x0= x, y0=y, r = radius, fill = type)) + 
    annotate(geom = "text", x = -800, y = 700, label = "The lightest are in the center \nand the heaviest at the edges.",
             family = "Mercury Display", color = "white", fontface = "bold.italic", size = 4.5, hjust = 0) + 
    scale_fill_manual(
      values = type_fillings,
      guide = "none"
    ) + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(), 
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(t = 20)
    ) + 
    coord_fixed(expand = F, clip = "off") 
)

# Combine the plots
summary_plot / circles_plot + 
  plot_annotation(title = "Great Pumpkin Commonwealth's \n Weigh-off Results · 2021", 
                  caption = "Data from BigPumpkins.com.\n *Long gourds lengths used\n as weights for circles areas.\n Tidytuesday Week-43 2021 · Abdoul ISSA BIDA.") + 
  plot_layout(nrow = 2, heights   = c(1,6)) & 
  theme(plot.background = element_rect(fill = "#41414f", color = NA), 
    plot.title = element_text(size = rel(2.5),hjust = .5, margin = margin(t = 10, b = 15), family = "Gotham Black", color = "white",  face = "bold"),
    plot.subtitle = element_text(size = rel(1.45),hjust = .5, margin = margin(b = 25), family = "Mercury Display"),
    plot.caption = element_text(color = "white", size = rel(.95), family = "Gotham Medium", margin = margin(t = 15, b = 10, r = 25))
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w43", "tidytuesday_2021_w43")
ggsave(filename = glue::glue("{path}.pdf"), width = 10.5, height = 12, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"), format = "png", dpi = 320) 

