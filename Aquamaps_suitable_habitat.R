# load libraries----------
rm(list=ls())
library(sf)
library(tidyverse)
library(rworldmap)
theme_set(theme_bw())

# get the world map and convert into sf object----
world <- 
  getMap(resolution = "less islands") %>% 
  st_as_sf()

# get ecoregions of the world shapefile----
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

# read function
read_suita <- 
  function(data){
    read_csv(paste0('C:/Users/javiera/Documents/Fish Escapes/data/FishBase/', data), skip = 13) %>%
      rename(lat = `Center Lat`, lon = `Center Long`, prob = `Overall Probability`) %>%
      mutate(Species = paste(Genus, Species, sep = " ")) %>%
      dplyr::select(Species, lat, lon, prob)
  }


# read_files and merge files ---------
all_suita <- 
  bind_rows(
    read_suita('seabass_suit.csv'),
    read_suita('seabream_suit.csv'),
    read_suita('salmon_suit.csv'),
    read_suita('cobia_suit.csv'),
    read_suita('rabbitfish_suit.csv'),
    read_suita('trout_suit.csv'),
    read_suita('tuna_suit.csv'),
    read_suita('meagre_suit.csv'),
    read_suita('turbot_suit.csv'),
    read_suita('red_drum_suit.csv')
  ) %>% 
  write_csv('outputs/all_hab_suitability_raw.csv')

# convert into sf object ----------
all_suita_sf <- 
  all_suita %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4326) %>% 
  st_join(eco_reg, .)

all_suitability <- 
  all_suita_sf %>% 
  # dplyr::select(ECOREGION, Species) %>% 
  group_by(ECOREGION, Species) %>% 
  summarise(prob = mean(prob, na.rm = T)) %>% 
  mutate(Status = "Habitat Suitability") %>% 
  drop_na(Species)

# Write files --------
st_write(all_suitability, "outputs/suitability_data.geojson", delete_dsn=TRUE )

all_suitability_data_ecoreg <- 
  all_suitability %>% 
  as.data.frame() %>% 
  dplyr::select(-Status, -geometry) %>% 
  write_csv('outputs/all_suitability_data_ecoreg.csv')
