# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gganimate)
library(lubridate)
library(ggtext)

# Data Reading and Wrangling ----------------------------------------------
# Didn't use it (toxonomic_codes)
toxonomic_codes <- tribble(
  ~Taxon,	~latin_name 	,~ common_name,
  "CMEAD", 	"Cheirogaleus medius", 	"Fat-tailed dwarf lemur",
  "DMAD" ,	"Daubentonia madagascariensis", "Aye-aye",
  "EALB" ,	"Eulemur albifrons",	"White-fronted brown lemur",
  "ECOL",	"Eulemur collaris",	"Collared brown lemur",
  "ECOR" , "Eulemur coronatus", "Crowned lemur",
  "EFLA",	"Eulemur flavifrons", "Blue-eyed black lemur",
  "EFUL" ,	"Eulemur fulvus", "Common brown lemur",
  "EMAC",	"Eulemur macaco", "Black lemur",
  "EMON" ,"Eulemur mongoz", "Mongoose lemur",
  "ERUB",	"Eulemur rubriventer", "Red-bellied lemur",
  "ERUF" ,	"Eulemur rufus", 	"Red-fronted brown lemur",
  "ESAN",	"Eulemur sanfordi", "Sanford’s brown lemur",
  "EUL" ,	"Eulemur Eulemur", "hybrid",
  "GMOH" ,	"Galago moholi", 	"Mohol bushbaby",
  "HGG" ,	"Hapalemur griseus griseus",	"Eastern lesser bamboo lemur",
  "LCAT" ,	"Lemur catta", "Ring-tailed lemur",
  "LTAR" ,	"Loris tardigradus", "Slender loris",
  "MMUR" ,	"Mircocebus murinus", "Gray mouse lemur",
  "MZAZ" ,	"Mirza coquereli", 	"Northern giant mouse lemur",
  "NCOU" ,	"Nycticebus coucang", "Slow loris",
  "NPYG" ,	"Nycticebus pygmaeus", "Pygmy slow loris",
  "OGG" ,	"Otolemur garnettii garnettii", "Northern greater galago",
  "PCOQ" ,	"Propithecus coquereli", "Coquerel’s sifaka",
  "PPOT" ,	"Perodicticus potto", "Potto",
  "VAR" ,	"Varecia Varecia", "hybrid",
  "VRUB" ,"Varecia rubra", "Red ruffed lemur",
  "VVV", 	"Varecia variegata variegata", "Black-and-white ruffed lemur"
)


lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
distinct_lemurs <- lemurs %>% 
  distinct(taxon, dlc_id, hybrid, sex, name, dob,.keep_all = T) %>% 
  filter(!is.na(dob)) %>% 
  mutate(dob = ymd(dob), 
         dob_year = year(dob) # Retrieve year of birth
         ) %>% 
  filter(dob_year >= 1975) %>% # Only keep data for year >= 1975
  select(taxon:birth_institution, dob_year) 

years_range <- range(distinct_lemurs$dob_year)

lemurs_1975 <- distinct_lemurs %>% 
  filter(dob_year == 1975)
nb_rows <- nrow(lemurs_1975)

# Rep 1975 data for each year (important for draw  1975 density curve in each year facet) 
lemurs_1975_rep <- lemurs_1975  %>% 
  select(-dob_year) %>% 
  slice(rep(row_number(), diff(years_range) + 1)) %>% 
  mutate(dob_year = rep(years_range[1]:years_range[2], each = nb_rows))

# Compute annual births
(annual_births <- distinct_lemurs %>% 
  group_by(dob_year) %>% 
  summarise(
    n = n(), # number of births 
    n_m  =sum(sex == 'M'),  # number of males
    n_f =sum(sex == 'F') # number of females
  ) %>% 
  mutate(
    variation_1975 = round(100*((n / nb_rows) - 1), 2), # Compute variation comparatively to 1975 
    # Fancy texts for the the year and the variation printing
    fancy_text = glue::glue('<b style = "font-family: Inconsolata ; font-weight: bold; font-size: 75px; color: grey25;">{dob_year}</b><br><span style="font-family: Lato ; font-size: 35px;" >{n} lemurs<br>({n_m} M, {n_f} F)</span>'), 
  fancy_variation = case_when( dob_year == 1975 ~ "",
    variation_1975 >=  0 ~ paste0("+",variation_1975,"%"),
                              TRUE ~ paste0("",variation_1975,"%"))
)
)
  

# Animation ---------------------------------------------------------------
(animation <- distinct_lemurs %>%
  ggplot(aes(birth_month, ..count..)) + 
  geom_density( size = 1.5, color = "#909dac",fill = "#909dac", alpha = .1) + 
  geom_density(data = lemurs_1975_rep, color = "#2196F3", fill = "#2196F3", size = 1.5, alpha = .3) + 
  geom_richtext(data = annual_births, inherit.aes = F,
                aes(x = 10, y = 20, label = fancy_text),
                fill = NA,
                label.color = NA) +
  geom_richtext(data = annual_births, inherit.aes = F,
                aes(x = 10, y = 15, label = fancy_variation, color = variation_1975),
                vjust = 1,
                size = 20,
                family = "Inconsolata",
                fontface = "bold",
                fill = NA,
                label.color = NA) +
  labs(
    title = 'Evolution of lemur births compared to <span style="color: #2196F3;">1975</span>',
    caption = "Data from ***Duke Lemur Center*** and cleaned by Jesse Mostipak.<br>
       Tidytuesday Week-35 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) + 
  scale_x_continuous(
    breaks = seq(1, 12, by = 3),
    labels = c("JANUARY","APRIL","JULY","OCTOBER"),
    expand =expansion(mult = c(0.01,0))
  ) +
  scale_y_continuous(
    limits = c(0,25),
    breaks = seq(5,25, 5),
    expand = expansion(mult = c(0,0.1))
  )  +
  scale_color_gradient(
    low = "#96281B",
    high = "#019875",
    guide = "none"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_markdown(family = "Lato Black",size = rel(3.5), hjust = .5, margin = margin(t = 15,b = 10)),
    axis.ticks = element_line(size = 0.35),
    axis.ticks.length = unit(0.20,"cm"),
    axis.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(family = "Inconsolata", size =rel(1.5)),
    strip.text = element_blank(),
    plot.margin = margin(t = 10,r = 10,b = 10, l=15),
    plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10))
  )  +
transition_states(dob_year,
                  transition_length = 1,
                  state_length = 4
                  )
)

# Saving ------------------------------------------------------------------
animate(
  animation,
  nframes = (diff(years_range) + 1) * 10,
  height = 7.5,
  width = 12.5,
  units = "in",
  res = 90,
  end_pause = 10,
  renderer = gifski_renderer(here::here
                             ("2021_w35/tidytuesday_2021_w35.gif"))
)

