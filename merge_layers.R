library(tidyverse)
library(sf)
theme_set(theme_minimal())

# production data---------------
prod_all_status <-
  read_csv('outputs/prod_all_status.csv') %>%
  dplyr::select(Species,
                ECOREGION,
                mean_prod_ecoreg,
                status,
                Introduced,
                Native)

# disease data -------
disease_data_species_ecoreg <-
  read_csv('outputs/diseases_data_species_ecoreg.csv') %>%
  dplyr::select(Species, ECOREGION, AGENT)

# ecological data --------
ecology_data_species_ecoreg <-
  read_csv('outputs/ecology_data_species_ecoreg.csv') %>%
  dplyr::select(Species, ECOREGION, MaxLengthTL, Troph, K, eco_pca)


# merge all data ------------
data_all <-
  full_join(prod_all_status,
            disease_data_species_ecoreg,
            by = c("Species", "ECOREGION")) %>%
  full_join(ecology_data_species_ecoreg,
            by = c("Species", "ECOREGION")) %>%
  group_by(ECOREGION) %>%
  mutate(
    n_fish = n_distinct(Species),
    n_introduced = n_distinct(Species[status == "Introduced"]),
    n_native = n_distinct(Species[status == "Native"]),
    n_pathogen = n_distinct(AGENT),
    sqrt_prod = sqrt(mean_prod_ecoreg),
    log_prod = log10(mean_prod_ecoreg))


score_data %>% 
  group_by(ECOREGION) %>% 
  summarise(mean(n_native)) %>% 
  print(n = 200)

hist(data_all$n_native)
hist(score_data$n_native)


prod_all_status %>% 
  group_by(ECOREGION) %>% 
  summarise()


# calculate scores-------------
score_data <-
  data_all %>%
  select(ECOREGION,
         sqrt_prod,
         n_native,
         n_introduced,
         n_pathogen,
         eco_pca) %>%
  group_by(ECOREGION) %>%
  summarise_all(mean) %>%
  mutate(
    genetic_score = n_native * log_prod,
    invasive_score = n_introduced * log_prod * eco_pca,
    pathogenic_score = n_pathogen * log_prod * n_native,
    final_score = genetic_score + invasive_score + pathogenic_score,
    log_score = log(final_score)
  )

# add ecoregion polygons----------
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

score_data_sf <- 
  score_data %>% 
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry')

# create maps-----------
ggplot(data = world_low) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = score_data_sf, aes(fill = log_score), na.rm = T) +
  scale_fill_viridis_c(name = 'Risk score', option = 'plasma')


ggplot(data = world_low) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = score_data_sf, aes(fill = eco_pca), na.rm = T) +
  scale_fill_viridis_c(name = 'Risk score', option = 'D')


ggplot(data = world_low) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = score_data_sf, aes(fill = pathogenic_score), na.rm = T) +
  scale_fill_viridis_c(name = 'Risk score', option = 'D')

ggplot(data = world_low) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = score_data_sf, aes(fill = pathogenic_score), na.rm = T) +
  scale_fill_viridis_c(name = 'Risk score', option = 'D') + 
  coord_sf(ylim = c(-60, 90), expand = F)
