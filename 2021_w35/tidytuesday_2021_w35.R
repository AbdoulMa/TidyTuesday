# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gganimate)
# Data Reading and Wrangling ----------------------------------------------

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

toxonomic_codes
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
distinct_lemurs <- lemurs %>% 
  distinct(taxon, dlc_id, hybrid, sex, name, dob,.keep_all = T) %>% 
  filter(!is.na(dob)) %>% 
  mutate(dob = ymd(dob), 
         dob_year = year(dob)) %>% 
  mutate(taxon = fct_lump_n(taxon, 10)) %>% 
  filter(dob_year >= 1975) %>% 
  select(taxon:birth_institution, dob_year) 

years_range <- range(distinct_lemurs$dob_year)

lemurs_1975 <- distinct_lemurs %>% 
  filter(dob_year == 1975)
nb_rows <- nrow(lemurs_1975)


lemurs_1975_rep <- lemurs_1975  %>% 
  select(-dob_year) %>% 
  slice(rep(row_number(), diff(years_range) + 1)) %>% 
  mutate(dob_year = rep(years_range[1]:years_range[2], each = nb_rows))

distinct_lemurs %>%  
  ggplot(aes(birth_month, ..count..)) + 
  geom_density() + 
  geom_density(data = lemurs_1975_rep, color = "steelblue") + 
  # facet_wrap(vars(dob_year), scales = "free_y") + 
  labs(title = "{closest_state}") + 
  scale_y_continuous(
    limits = c(0,30)
  ) + 
  transition_states(dob_year
  )


# Attempt with columns ----------------------------------------------------
lemurs_80 <- distinct_lemurs %>% 
  filter(dob_year == 1980) %>% 
  group_by(taxon) %>% 
  count(name = "nb_1980")


annual_births <- distinct_lemurs %>% 
  filter(dob_year > 1980) %>% 
  count(taxon, dob_year) %>% 
  arrange(dob_year) %>% 
  left_join(lemurs_80) %>% 
  mutate(nb_1980 = ifelse(is.na(nb_1980), 0, nb_1980))

(annual_growth <- annual_births %>% 
  mutate(grow =  nb_1980 / n) %>% 
  mutate(grow_variation = grow - 1)
)
annual_growth %>% 
  filter(dob_year == 2010) %>% 
  ggplot(aes(grow, fct_rev(taxon))) +
  geom_col() + 
  geom_col(inherit.aes = F, aes(1,fct_rev(taxon)), fill = "steelblue", alpha = .2)
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
