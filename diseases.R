library(tidyverse)
library(readxl)
library(sf)

# read disease data fixed manually---------
disease_data <-
  read_excel('outputs/diseases_data_manual.xlsx') %>%
  mutate(type = fct_collapse(
    TYPE,
    Parasites =  c(
      "Cestoda",
      "Copepoda",
      "Digenea",
      "Digenea",
      "Hirudinea",
      "Isopoda",
      "Monogenea",
      "Nematoda",
      "Trematoda"
    ),
    Others = c(
      "Amoeba",
      'Ciliophora',
      'Dinoflagellate',
      "Fungi",
      "Microsporidia",
      "Myxozoa"
    )
  )) %>% 
  write_csv('outputs/diseases_data_all.csv')

disease_data %>% 
  distinct(AGENT)

disease_data %>% distinct(AGENT)

disease_data %>% 
  mutate(total_pathogens =  n_distinct(AGENT)) %>% 
  group_by(type) %>% 
  summarise(porp_types = n_distinct(AGENT)/first(total_pathogens)*100)


# join with farming ecoregions by species-----------
mean_prod_ecoreg <-
  read_csv('outputs/production_country_data_ecoreg_manual.csv')

disease_data_species_ecoreg <-
  disease_data %>%
  dplyr::select(Species, AGENT, TYPE) %>%
  right_join(mean_prod_ecoreg, by = 'Species') %>%
  write_csv('outputs/diseases_data_species_ecoreg.csv')


diseases_data_ecoreg <-
  disease_data_species_ecoreg %>%
  group_by(ECOREGION) %>%
  summarise(cumm_diseases = n_distinct(AGENT)) %>%
  write_csv('outputs/diseases_data_ecoreg.csv')

diseases_data_ecoreg_sf <-
  diseases_data_ecoreg %>%
  right_join(eco_reg %>% as.data.frame(),
             mean_prod_ecoreg,
             by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry') %>%
  write_sf('outputs/diseases_ecoreg.geojson', delete_dsn = TRUE)

# create a map of diseases------------------
diseases_data_sf <- st_read('outputs/diseases_ecoreg.geojson')

world_map_low <- 
  ggplot(data = world_low) +
  geom_sf(fill = 'gray95')

map_diseases <-
  world_map_low +
  geom_sf(data = diseases_data_sf, aes(fill = cumm_diseases), alpha = .7) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    na.value = 'gray70',
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Number of diseases'
  ) +
  theme_bw()

print(map_diseases)

ggsave(
  map_diseases,
  filename = 'figures/map_diseases.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 15,
  height = 10
)


# Calculate pathogenic score-------------
pathogenic_score <-
  disease_data_species_ecoreg %>% 
  group_by(Species, ECOREGION) %>%
  mutate(pathogenic = n_distinct(AGENT) * sqrt(mean_prod)) %>%
  summarise(pathogenic_total = sum(pathogenic)/1e+4)

eco_reg <- st_read('data/MEOW/meow_ecos.shp')

pathogenic_score_sf <-
  pathogenic_score %>%
  right_join(eco_reg %>% as.data.frame(),
             by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry') 
  # write_sf('outputs/diseases_ecoreg.geojson', delete_dsn = TRUE)

world_map_low +
  geom_sf(data = pathogenic_score_sf, aes(fill = sqrt(pathogenic_total)), alpha = .7) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    na.value = 'gray90',
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Pathogenic score'
  ) +
  theme_bw()
