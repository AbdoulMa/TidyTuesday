# Code copied from https://github.com/coulmont/varia/blob/master/web-du-bois-spirale-code.R

# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------
df <- tribble(~cat,~nombre,~y_zero,~couleur,~label,
              "1875",21186,25,"#FFCDCB",    "1875_____ $ 21,186",
              "1880",498532,24,"#989EB4",   "1880____ $ 498,532",
              "1885",736170,23,"#b08c71",   "1885____ $ 736,170",
              "1890",1172624,22,"#FFC942",  "1890__ $ 1,173,624",
              "1895",1322694,21,"#EFDECC",  "1895__ $ 1,322,694",
              "1899",1434975,20,"#F02C49",  "1899__ $ 1,434,975")


# Graphic -----------------------------------------------------------------
# Slope
pente <- (20-6)/1434975

# 
df <- df %>% mutate(y_final = y_zero - nombre*pente)

# Polygons compositions
positions <- df %>% mutate(x_1=0, x_2=nombre, x_3=nombre, x_4=0,
                           y_1=y_zero, y_2=y_final, y_3=y_final+1, y_4=y_zero+1) %>%
  select(cat,x_1:y_4) %>%
  pivot_longer(cols=-cat,
               names_sep="_",
               names_to=c("coord","rang")) %>%
  pivot_wider(names_from = "coord", values_from=value)

# Join with initial data
positions <- positions %>% left_join(df,by="cat")

p <- ggplot(positions, aes(x = x, y = y, group=cat)) +
  geom_polygon(aes(group=cat, fill=I(couleur)),color="black")

final_plot <- p + 
  scale_y_continuous(expand=expansion(add=c(11,-5))) +
  scale_x_continuous(expand=expansion(add=c(0,-650000))) +
  coord_polar() +
  geom_text(data = . %>% filter(rang==1),
            aes(label = paste(label,"   ",sep="")),
            adj=1, nudge_y=.5, nudge_x = -0000, color="black",size=3,family="DecimaMonoPro") +
  theme_void() +
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES.") + 
  theme(legend.position="none",
        text = element_text(family="I Sans Semibold"),
        plot.title = element_text(face = "bold", size = rel(1.75), hjust = .5),
        plot.margin = margin(c(.25,0,0,0), unit = "cm"))

cowplot::ggdraw(final_plot) + 
  theme(
        plot.background = element_rect(fill="#e9d9c9", color=NA)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w7", "tidytuesday_2022_w7")
ggsave(filename = glue::glue("{path}.png"), width = 9, height = 9, dpi = 300, device = ragg::agg_png)
