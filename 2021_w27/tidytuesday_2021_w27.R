library(tidyverse) 
library(waffle)
library(MESS) # round_percent  https://www.rdocumentation.org/packages/MESS/versions/0.5.7/topics/round_percent
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

animal_rescues %>% 
  view()

animal_rescues %>% 
  count(cal_year, animal_group_parent, sort = T) %>% 
  group_by(cal_year) %>% 
  mutate(prop = prop.table(n)) %>% 
   arrange(cal_year)  %>% 
  view()
  
unique(animal_rescues$property_category)

(top_property_categories <- animal_rescues %>% 
  count(property_category, sort = T))
top_animals <- animal_rescues %>% 
  count(animal_group_parent, sort = T) %>% 
  head(8) %>% 
  pull(animal_group_parent)

top_animals

(rescued_animals_properties <- animal_rescues %>% 
  mutate(animal_group_parent = fct_other(animal_group_parent, keep = top_animals, other_level = "Others")) %>% 
  count(animal_group_parent, property_category, sort = T) %>% 
  group_by(animal_group_parent) %>% 
  mutate(prop = round_percent(prop.table(n))) %>% 
  arrange(animal_group_parent))
  
# Plot 
(plot <- rescued_animals_properties %>% 
  mutate(animal_group_parent = str_wrap(str_to_upper(animal_group_parent), width = 20)) %>% 
  # filter(animal_group_parent == "Horse") %>% 
  ggplot(aes(fill = property_category, values = prop )) + 
  geom_waffle(color = "white", size=.15, n_rows = 10, flip = T) + 
  coord_equal(expand = F) +
  facet_wrap(vars(animal_group_parent)) + 
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = .5
    )
  ) + 
  theme_minimal(base_family = "Lato Semibold")+ 
  theme(
    panel.border = element_rect(colour = "black",size = 1.25, fill = "transparent"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(margin = margin(b = 10)),
    legend.spacing.x = unit(10,"pt"),
    legend.spacing.y = unit(5,"pt")
  )
)

png(here::here("2021_w27/tidytuesday_2021_w27.png"),width = 7, height = 7.5,res = 320, units = "in",type = "cairo")
plot
dev.off()
