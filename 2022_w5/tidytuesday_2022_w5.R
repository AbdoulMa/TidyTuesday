# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggimage)
library(ggrepel)
library(magick)

# Data Wrangling ----------------------------------------------------------
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
# Solve join trouble
breed_traits %>% 
  mutate(Breed_wo_space = str_remove_all(Breed, "\\s")) -> breed_traits
breed_rank_all %>% 
  mutate(Breed_wo_space = str_remove_all(Breed, "\\s")) -> breed_rank_all

dogs_dir_path <- here::here("2022_w5","Dogs")

# Compute a data score for each dog trait ---- 
(breed_traits_all <- breed_traits %>% 
   mutate(trait_mean = rowMeans(select_if(., is.numeric), na.rm = TRUE), 
          trait_normalized = (trait_mean - min(trait_mean))/(max(trait_mean)-min(trait_mean))) %>% 
   select(Breed, Breed_wo_space, trait_normalized, `Coat Length`)
)

# Compute a popularity score for each dog trait ---- 
(breed_rank_popularity_total <- breed_rank_all %>% 
   # keep dogs with a ranking every year 
   filter(if_all(everything(), ~!is.na(.))) %>% 
   mutate(popularity_avg = rowMeans(select_if(., is.numeric), na.rm = TRUE),
          popularity_normalized = (popularity_avg -min(popularity_avg))/ (max(popularity_avg)-min(popularity_avg))
   ) %>% 
   # Only keep the 100 more popular 
   slice_min(order_by = popularity_normalized, n = 100))

breed_overall_popularity <- breed_rank_popularity_total %>% 
  left_join(breed_traits_all, by = 'Breed_wo_space', suffix = c("", "_trait")) %>% 
  select(Breed, Image, coat_length = `Coat Length`, popularity_normalized, trait_normalized) %>% 
  drop_na(trait_normalized) 

# Means popularity and data score --- 
mean_popularity <- mean(breed_overall_popularity$popularity_normalized)
mean_trait <- mean(breed_overall_popularity$trait_normalized)

# Remove background from dog image ---- 
clean_logo_transparent <- function(breed_name,img_url) {
  # https://themockup.blog/posts/2021-01-28-removing-image-backgrounds-with-magick/
  raw_img <-img_url %>%
    image_read() %>% 
    image_convert("PNG")
  
  img_mask <- raw_img  %>% 
    image_fill("transparent", "+1+1", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+1+99", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+140+1", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+140+99", fuzz = 2, refcolor = "white") %>% 
    image_channel("Opacity") %>%
    image_convert(matte=FALSE) %>%
    image_negate() %>%
    image_blur()
  
  # Create Dogs Image Directory if not exists
  if (!fs::dir_exists(dogs_dir_path)) fs::dir_create(dogs_dir_path)
  image_composite(raw_img, img_mask, operator = "CopyOpacity") %>%
    image_write(here::here(dogs_dir_path, paste0(breed_name, ".png")))
}

# Remove background and save ----
breed_overall_popularity %>% 
  pwalk(~clean_logo_transparent(..1, ..2))

# Graphic -----------------------------------------------------------------
coats_labels <- "<span>Coats</span><br>
<span style='color:#008ecf;';><span style ='font-family:\"Dog Font\"; font-size:35pt;'>O</span>Long</span><br>
<span style='color:#40a535;'><span style ='font-family:\"Dog Font\"; font-size:35pt;'>D</span>Medium</span><br>
<span style='color:#fec800;'><span style ='font-family:\"Dog Font\"; font-size:35pt;'>E</span>Short</span>"

data_score_labels <- "Data score combines dog intelligence, adaptability, Openness, Playfulness, Trainability, Energy and others." %>% str_wrap(width = 30)
caption <- "Data from American Kennel Club courtesy of Kristen Akey.<br>
Inspired from a graphic in **Information is Beautiful** by **David McCandless**.<br>
Tidytuesday Week-5 2022 · Abdoul ISSA BIDA."
(plot <- breed_overall_popularity  %>% 
   ggplot(aes(trait_normalized, popularity_normalized)) + 
   annotate(geom = "segment", y = .7, yend = -0.12, x = mean_trait, xend = mean_trait, size = 1.5, arrow = arrow(length = unit(0.1, "inches")), lineend = "round") +
   annotate(geom = "segment", x = 0.55, xend = 1.05, y = mean_popularity, yend = mean_popularity, size = 1.5, arrow = arrow(length = unit(0.1, "inches")), lineend = "round") +
   annotate(geom = "label", x = mean_trait, y = -0.15, label = "Popularity", family = "Go Bold", fill = "#111111", size = 6.5, color = "white", label.size = unit(0,"cm"), label.r = unit(0,"cm"))+ 
   annotate(geom = "label", x = .55, y = mean_popularity, label = "Data Score", family = "Go Bold", fill = "#111111", size = 6.5, color = "white", label.size = unit(0,"cm"), label.r = unit(0,"cm")) + 
   annotate(geom = "text", x = 1.05, y = -.05, label = "Hot Dogs!", size=7.5, family = "Go Bold", hjust = 1) + 
   annotate(geom = "text", x = 1.05, y = .65, label = "Overlooked treasures", size=7.5, family = "Go Bold", hjust = 1) + 
   annotate(geom = "text", x = 0.55, y = -.05, label = "Inexpicably overrated", size=7.5, family = "Go Bold", hjust = 0) + 
   annotate(geom = "text", x = 0.55, y = .65, label = "The Rightly Ignored", size=7.5, family = "Go Bold", hjust = 0) + 
   annotate(geom ="richtext", x = 1.05, y = -0.15, label = coats_labels, fill = "white", label.size = unit(0,"cm"), family = "Go Bold", size = 5, hjust = 1) +
   annotate(geom = "segment", x = .55, xend = .55, y = mean_popularity - .025,  yend =  mean_popularity - .06) +
   annotate(geom = "segment", x = .55, xend = .567, y = mean_popularity - .06,  yend =  mean_popularity - .06) +
   annotate(geom = "text", x = .57, y = mean_popularity - .06, label = data_score_labels, size= 2.5, family = "M G2 Semibold", hjust = 0, lineheight = .9) + 
   geom_image(aes(image = here::here(dogs_dir_path, paste0(Breed, ".png"))), size = 0.085) +
   geom_label_repel(aes(label = Breed, fill = coat_length), alpha = .65, color = "#111111", 
                    family = "M G2 Bold",
                    size = 2.25,
                    point.padding = 0.9, 
                    label.r = unit(0, "cm"),
                    label.size = unit(0,"cm"),
                    segment.alpha = 0) +
   labs(title = "Dog breeds",
        caption =  caption) + 
   scale_y_reverse() + 
   scale_fill_manual(
     values =c(
       "Long" = "#008ecf",
       "Short" = "#fec800",
       "Medium" = "#40a535"
     ),
     guide = "none"
   ) +
   coord_cartesian(clip = "off") + 
   theme_minimal() + 
   theme(
     text = element_text(color = "#111111"),
     plot.title = element_text(family = "NY Bold", size = rel(4.5)),
     plot.caption = element_markdown(family = "Gotham Medium", size = rel(1.05)),
     axis.text = element_blank(),
     axis.title = element_blank(),
     panel.grid = element_blank(),
     plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
     plot.background = element_rect(fill = "white", color = NA),
   )
)

# Saving ------------------------------------------------------------------
path <- here::here("2022_w5", "tidytuesday_2022_w5")
ggsave(filename = glue::glue("{path}.png"), width = 11.5, height = 11.5, device = ragg::agg_png, dpi = 300)
