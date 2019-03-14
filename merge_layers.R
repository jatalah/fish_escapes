library(tidyverse)
library(sf)
library(rworldmap)
theme_set(theme_minimal())


# add ecoregion polygons----------
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

# 01 production data---------------
prod_all_status_scores <-
  read_csv('outputs/prod_all_status.csv') %>%
  dplyr::select(Species,
                ECOREGION,
                mean_prod_ecoreg,
                status) %>%
  group_by(ECOREGION) %>%
  summarise(
    n_total = n_distinct(Species),
    n_introduced = n_distinct(Species[status == "Introduced"]),
    n_native = n_distinct(Species[status == "Native"]),
    prod_total = sum(mean_prod_ecoreg),
    prod_native = sum(mean_prod_ecoreg[status == "Native"]),
    prod_introduced = sum(mean_prod_ecoreg[status == "Introduced"]),
    genetic_score = n_native * log10(prod_native + 1)
  ) %>%
  write_csv('outputs/prod_all_status_scores.csv')

  

production_dat_long <-
  prod_all_status_scores %>%
  gather(key, value, c("prod_native", "prod_introduced")) %>%
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>%
  st_sf(sf_column_name = 'geometry') %>%
  filter(value > 0) %>%
  mutate(key = fct_recode(key, Native = "prod_native",
                          "Non-native" = "prod_introduced"),
         key = fct_rev(key)) %>% 
  write_csv('outputs/production_dat_long.csv')



# 03 disease data -------
disease_data_ecoreg <-
  read_csv('outputs/diseases_data_species_ecoreg.csv') %>%
  group_by(ECOREGION) %>%
  summarise(n_pathogen = n_distinct(AGENT)) %>% 
  write_csv('outputs/disease_data_ecoreg.csv')

disease_score <-
  disease_data_ecoreg %>%
  left_join(prod_all_status_scores, by = 'ECOREGION') %>%
  mutate(pathogenic_score = n_pathogen * log10(prod_total + 1)) %>%
  select(ECOREGION, pathogenic_score)

# 04 Ecological data --------
prod_all_status <-
  read_csv('outputs/prod_all_status.csv')

ecoreg_with_data <- 
  prod_all_status_scores %>% 
  select(ECOREGION)

invasive_ecoreg <-
  read_csv('outputs/estimates_fishbase.csv') %>%
  full_join(prod_all_status, by = "Species") %>%
  filter(status == "Introduced") %>%
  select(ECOREGION, Species, eco_ind, mean_prod_ecoreg) %>%
  group_by(ECOREGION, Species) %>%
  summarise(mean_prod_ecoreg = sum(mean_prod_ecoreg),
            eco_ind = first(eco_ind)) %>%
  group_by(ECOREGION) %>%
  summarise(invasive_score = sum(eco_ind * log10(mean_prod_ecoreg + 1))) %>%
  full_join(ecoreg_with_data,  by = "ECOREGION") %>% 
  mutate(invasive_score = ifelse(is.na(invasive_score), 0, invasive_score))


# merge all data ------------
score_data <-
  full_join(prod_all_status_scores,invasive_ecoreg, by = "ECOREGION") %>% 
  full_join(disease_score, by = "ECOREGION") 

range_0_100 <-
  function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * 100
  }

std_scores <-
  score_data %>%
  mutate(
    Genetic = range_0_100(genetic_score),
    Invasive = range_0_100(invasive_score),
    Diseases = range_0_100(pathogenic_score),
    final_score = ((Genetic * 1) + (Invasive * 1) + (Diseases * 1)) / 3,# add expert weights
    sq_final_score = sqrt(final_score),
    Overall = range_0_100(sq_final_score)
  ) %>%
  left_join(select(as.data.frame(eco_reg), ECOREGION, PROVINCE, REALM), by = 'ECOREGION') %>%
  select(
    ECOREGION,
    PROVINCE,
    REALM,
    everything(),-genetic_score,-invasive_score,-pathogenic_score,-sq_final_score,-final_score
  ) %>%
  write_csv('outputs/std_scores.csv')


std_scores %>% 
  gather(key, value, Genetic:Overall) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free')

# add ecoregion polygons----------
# eco_reg <- st_read('data/MEOW/meow_ecos.shp')

score_data_sf <- 
  std_scores %>% 
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry') %>%
  drop_na(prod_total) %>% 
  st_write("outputs/score_data_sf.geojson",  delete_dsn=TRUE )

std_scores %>% 
  gather(key, value, genetic_score:Overall) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free')

