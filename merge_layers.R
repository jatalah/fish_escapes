library(tidyverse)
library(sf)
library(rworldmap)
theme_set(theme_minimal())

# 01 Read ecoregion polygons----------
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

# 02 Production data---------------
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

# convert into the long format for plotting  
production_dat_long <-
  prod_all_status_scores %>%
  gather(key, value, c("prod_native", "prod_introduced")) %>%
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>%
  st_sf(sf_column_name = 'geometry') %>%
  filter(value > 0) %>%
  mutate(
    key = fct_recode(key, "A. native" = "prod_native",
                     "B. non-native" = "prod_introduced"),
    key = fct_rev(key),
    value = value/1000
  ) %>%
  write_csv('outputs/production_dat_long.csv')


# 03 Disease data -------
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


# 05 Merge all data ------------
score_data <-
  full_join(prod_all_status_scores,invasive_ecoreg, by = "ECOREGION") %>% 
  full_join(disease_score, by = "ECOREGION") 

range_0_100 <-
  function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * 100
  }


weights <- read_csv('outputs/score_weight.csv')
sum(weights$mean_weight)

std_scores <-
  score_data %>%
  mutate(
    Genetic = range_0_100(sqrt(genetic_score)),
    Invasive = range_0_100(sqrt(invasive_score)),
    Diseases = range_0_100(sqrt(pathogenic_score)),
    # final_score = ((Genetic * 1.63) + (Invasive * 2) + (Diseases * 1.59)) / sum(weights$mean_weight),# add expert weights
    Overall = (Genetic + Invasive + Diseases) / 3
  ) %>%
  left_join(select(as.data.frame(eco_reg), ECOREGION, PROVINCE, REALM), by = 'ECOREGION') %>%
  select(
    PROVINCE,
    ECOREGION,
    REALM,
    everything(),
    -genetic_score,
    -invasive_score,
    -pathogenic_score
  ) %>%
  write_csv('outputs/std_scores.csv')

# add ecoregion polygons
score_data_sf <-
  std_scores %>%
  select(-PROVINCE,-REALM) %>%
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>%
  st_sf(sf_column_name = 'geometry') %>%
  drop_na(prod_total) %>%
  st_write("outputs/score_data_sf.geojson",  delete_dsn = TRUE)

std_scores %>%
  gather(key, value, Genetic:Overall) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free')

std_scores %>%
  select(Genetic, Invasive, Diseases, Overall) %>% 
  cor(.)
