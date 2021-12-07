# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggalluvial)
library(ggtext)
# Data Reading and Wrangling ----------------------------------------------
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# General Informations
range(spiders$year) 
spiders$family %>% unique() %>% length()
spiders$species %>% unique() %>% length()
spiders$author %>% unique() %>% length()

# Wrangling
spiders_authors <- spiders %>% 
  mutate(
    author = fct_lump_n(author, n = 10),
    family = fct_lump_n(family, n= 10)
  ) %>% 
  filter(author != "Other", family != "Other") %>% 
  count(author, family) %>% 
  mutate(
    author = fct_reorder(author, desc(n), .fun = sum),
    family = fct_reorder(family, desc(n), .fun = sum)
  ) 

# Graphic -----------------------------------------------------------------
simon_label <- paste0("<span style='color: #8931EF;'>Eugène Louis Simon</span><br>", "(Apr 30, 1848 – Nov 17, 1924, France) is by far the most prolific spider taxonomist in history, describing over 4,000 species." %>% str_wrap(35) %>% str_replace_all("\n","<br>"))
levi_label <- paste0("<span style='color: #F2CA19;'>Herbert Walter Levi</span><br>", "(Jan 2, 1921 – Nov 3, 2014, Germany) was professor emeritus of zoology and curator of arachnology at the Museum of Comparative Zoology, Harvard University." %>% str_wrap(35) %>% str_replace_all("\n","<br>"))
huber_label <- paste0("<span style='color: #FF00BD;'>Bernhard A. Huber</span><br>", "(1967 – , Austria) is an Austrian arachnologist who named more than 480 taxons." %>% str_wrap(35) %>% str_replace_all("\n","<br>"))
thorell_label <- paste0("<span style='color: #E11845;'>Tord Tamerlan Teodor Thorell</span><br>","(May 3, 1830 – Dec 22, 1901, Sweden) described more than 1,000 spider species during his time from the 1850 to 1900" %>% str_wrap(35) %>% str_replace_all("\n","<br>"))

title <- 'Pioneers of <span style="color:#B40672;">Arachnology</span>'
subtitle <- "In nearly 3 centuries, 27,453 species classified into 129 families\n have been discovered by more than 3,070 arachnologists."
spiders_authors %>% 
  ggplot(aes(axis1 = author, axis2 = family, y = n,
             fill = fct_other(author, keep = c("Simon","Levi", "Huber","Thorell")),
             
             label = author
  )) + 
  geom_alluvium(curve_type = "quintic", 
                aes(alpha = fct_collapse(author, 
                                         "main" = c("Simon","Levi","Huber", "Thorell"), 
                                         other_level = "Other"))) + 
  geom_stratum() + 
  ggfittext::geom_fit_text(aes(label = after_stat(stratum), color = after_scale(prismatic::best_contrast(fill))),
                           family = "Mercury",                           
                           stat = "stratum", fontface = "bold",width = 1/4, min.size = 3, grow = T, 
                           lineheight = .95) +
  annotate(geom = "text", x = 1, y = 8000, size = 8, label = "Author", family = "Gotham Bold") + 
  annotate(geom = "text", x = 2, y = 8000, size = 8, label = "Family", family = "Gotham Bold") + 
  annotate(geom = "richtext", x = .8, y = 6500, label = simon_label,
           size = 4.5, family = "Mercury", fontface = "bold",
           hjust = 1, lineheight = .95, label.color = NA, fill = NA) + 
  annotate(geom = "richtext", x = .8, y = 4800, label = levi_label,
           size = 3.75, family = "Mercury", fontface = "bold",
           hjust = 1, lineheight = .95, label.color = NA, fill = NA) + 
  annotate(geom = "richtext", x = .8, y = 3850, label = huber_label,
           size =3.75, family = "Mercury", fontface = "bold",
           hjust = 1, lineheight = .95, label.color = NA, fill = NA) + 
  annotate(geom = "richtext", x = .8, y = 3100, label = thorell_label,
           size = 3.75, family = "Mercury", fontface = "bold",
           hjust = 1, lineheight = .95, label.color = NA, fill = NA) + 
  labs(
    title = title, 
    subtitle = subtitle,
    caption = "Data from  World Spider Database.\n Tidytuesday Week-50 2021 · Abdoul ISSA BIDA."
  ) + 
  coord_cartesian(clip = "off") + 
  scale_x_continuous(expand = expansion(mult = c(.2,.05))) + 
  scale_fill_manual(
    values = c(
      "Simon"  = "#8931EF",
      "Levi" =  "#F2CA19",
      "Huber" = "#FF00BD",
      "Thorell" = "#E11845",
      "Other" = "#87E911"
    ),
    na.value = "#FFFFFF"
  ) + 
  scale_alpha_manual(
    values = c(
      "main" = .9, 
      "Other" = .4
    )
  ) + 
  theme_minimal() + 
  theme(
    text = element_text(color = "#111111", family = "Gotham Bold"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_markdown(size = rel(7.5), family = "Mercury", face ="bold"),
    plot.subtitle = element_text(size = rel(2.5), margin = margin(t = 5, b = 5)),
    plot.caption = element_text(hjust = .5, size = rel(1.5), margin = margin(t = 10, b =10)),
    plot.margin = margin(t =25, r=15,b=15, l=20),
    plot.background = element_rect(fill = "#F3F6F7", color = NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w50", "tidytuesday_2021_w50")
ggsave(glue::glue("{path}.png"), device = ragg::agg_png, width = 17.5, height = 12, dpi = 320)
