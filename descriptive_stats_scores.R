library(tidyverse)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# read production and scores data-------
std_scores <- read_csv('outputs/std_scores.csv')
prod_all_status_ecoreg <- read_csv('outputs/prod_all_status_ecoreg.csv')

# 01 Description of production data by status
# summary total production
summary(std_scores$prod_total)

sum(std_scores$prod_total)# total production
sum(std_scores$prod_native) # total production native
sum(std_scores$prod_introduced)# total production non-native


# percentage production native 
round(sum(std_scores$prod_native) / sum(std_scores$prod_total) *100, 2) 

# percentage production non-native 
round(sum(std_scores$prod_introduced) / sum(std_scores$prod_total)* 100,2)


# 03 Number of species per MEOW------------
# total summary
summary(std_scores$n_total)

# mean and se species per MEOW
gmodels::ci(prod_all_status_scores$n_total)

std_scores %>% arrange(desc(prod_native))

# 04 Genetic score-------
summary(std_scores$Genetic)

std_scores %>%
  group_by(REALM) %>%
  select(ECOREGION, Genetic) %>% 
  top_n(wt = Genetic, n = 3) %>% 
  arrange(desc(Genetic))


prod_all_status_ecoreg %>%
  filter(ECOREGION == "Yellow Sea" |
           ECOREGION == "Southern China" | ECOREGION == "East China Sea") %>%
  filter(status == "Native") %>%
  group_by(Species) %>%
  summarise(p = sum(mean_prod_ecoreg)) %>%
  arrange(desc(p))

prod_all_status_ecoreg %>%
  filter(
    REALM == "Temperate Northern Atlantic" & status == "Native" &
      ECOREGION == "Celtic Seas" |
      ECOREGION == "Aegean Sea" |
      ECOREGION == "South European Atlantic Shelf"
  ) %>%
  group_by(ECOREGION, Species) %>%
  summarise(p = sum(mean_prod_ecoreg)) %>%
  arrange(desc(p))

# 05 Disease data -------
# overall
disease_data <- read_csv('outputs/diseases_data_all.csv')

disease_data %>% 
  distinct(AGENT)

disease_data %>% 
  mutate(total_pathogens =  n_distinct(AGENT)) %>% 
  group_by(type) %>% 
  summarise(prop_types = round(n_distinct(AGENT)/first(total_pathogens)*100))

disease_data %>%
  group_by(Species) %>%
  summarise(n = n_distinct(AGENT))

gmodels::ci(.Last.value$n)

# disease data ecoregion

diseases_data_ecoreg <- read_csv('outputs/diseases_data_ecoreg.csv')
summary(diseases_data_ecoreg)
gmodels::ci(diseases_data_ecoreg$cumm_diseases)

summary(std_scores$Diseases)

std_scores %>%
  group_by(REALM) %>%
  select(ECOREGION, Diseases) %>% 
  top_n(wt = Diseases, n = 3) %>% 
  arrange(desc(Diseases))

# 06 Ecological score-------
summary(std_scores$Invasive)

# Top MEOWs by realm
std_scores %>%
  group_by(REALM) %>%
  select(ECOREGION, Invasive) %>% 
  top_n(wt = Invasive, n = 2) %>% 
  arrange(desc(Invasive))

prod_all_status %>% 
  filter(ECOREGION == "Yellow Sea") %>% 
  filter(status =="Introduced") %>% 
  group_by(Species) %>% 
  summarise(p = sum(mean_prod_ecoreg)) %>% 
  arrange(desc(p))


prod_all_status %>% 
  filter(str_detect(ECOREGION, "Black Sea")) %>% 
  filter(status =="Introduced") %>% 
  group_by(Species) %>% 
  summarise(p = sum(mean_prod_ecoreg)) %>% 
  arrange(desc(p))

prod_all_status %>% 
  filter(ECOREGION == "Yellow Sea") %>% 
  filter(status =="Introduced") %>% 
  group_by(Species) %>% 
  summarise(p = sum(mean_prod_ecoreg)) %>% 
  arrange(desc(p))

# 07 Overall score-------
summary(std_scores$Overall)

std_scores %>%
  group_by(PROVINCE) %>%
  select(ECOREGION, Overall) %>% 
  top_n(wt = Overall, n = 1) %>% 
  arrange(desc(Overall))


std_scores %>%
  group_by(PROVINCE) %>%
  summarise(Overall = mean(Overall)) %>% 
  # top_n(wt = Overall, n = 1) %>% 
  arrange(desc(Overall))
