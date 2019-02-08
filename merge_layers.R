library(tidyverse)

# merge all data layers by ecoregion----
disease_data <-
  read_csv('outputs/diseases_data.csv') %>%
  dplyr::select(Species, AGENT)

disease_data_species_ecoreg <-    read_csv('outputs/diseases_data_species_ecoreg.csv')

pathogenic_score <-
  disease_data_species_ecoreg %>% 
  group_by(Species, ECOREGION) %>%
  mutate(pathogenic = n_distinct(AGENT) * sqrt(mean_prod)) %>%
  summarise(pathogenic_total = sum(pathogenic)/1e+4)

# ecological parameters--------



# production data---------------
mean_prod_ecoreg <-
  read_csv('outputs/production_country_data_ecoreg_manual.csv') %>% 
  mutate(status = "native")

# distribution data---------
all_native_data <- read_csv('outputs/all_native_data.csv')

suitable_introductions_ecoreg_data <-
  read_csv("outputs/suitable_introductions_ecoreg_data.csv") %>%
  group_by(ECOREGION, Species) %>%
  transmute(Exotic = if_else(Introductions > 0, 1, 0))


production_status <- 
  full_join(mean_prod_ecoreg,
          all_native_data,
          by = c("ECOREGION", "Species")) %>%
  mutate(status = if_else(status == "native", status, "exotic"))




