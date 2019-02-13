# load libraries----------
rm(list = ls())
library(sf)
library(tidyverse)
library(rworldmap)
theme_set(theme_bw())
library(readxl)

# read function------------
read_native <-
  function(path) {
    read_csv(paste0(
      'C:/Users/javiera/Documents/fish_escapes/data/AquaMaps/',
      path
    ),
    skip = 7) %>%
      rename(lat = `Center Lat`, lon = `Center Long`) %>%
      mutate(Species = paste(Genus, Species, sep = " ")) %>%
      dplyr::select(Species, lat, lon)
  }

# merge all species files-----------
all_occu <-
  bind_rows(
    read_native('seabass.csv'),
    read_native('seabream.csv'),
    read_native('salmon.csv'),
    read_native('cobia.csv'),
    read_native('rabbitfish.csv'),
    read_native('trout.csv'),
    read_native('tuna.csv'),
    read_native('meagre.csv'),
    read_native('turbot.csv'),
    read_native('red_drum.csv'),
    read_native('jap_mackerel.csv'),
    read_native('barramundi.csv'),
    read_native('atlantic_halibut.csv'),
    read_native('blackhead_seabream.csv'),
    read_native('mullet.csv'),
    read_native('red_porgy.csv'),
    read_native('cod.csv'),
    read_native('white_trevally.csv'),
    read_native('chinook.csv'),
    read_native('southern_bluefin_tuna.csv'),
    read_native('korean_rockfish.csv'),
    read_native('halibut.csv'),
    read_native('jap_seabass.csv'),
    read_native('coho.csv'),
    read_native('amberjack.csv'),
    read_native('pacific_tuna.csv'),
    read_native('milkfish.csv'),
    read_native('pompano.csv'),
    read_native('pufferfish.csv'),
    read_native('yellow_croaker.csv')
  ) %>% 
  write_csv('outputs/native_dist_data.csv')

## join native distribution data with ecoregions of the world-----
all_occu_sf <-
  all_occu %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_join(st_read('data/MEOW/meow_ecos.shp'), .)

all_native <-
  all_occu_sf %>%
  dplyr::select(ECOREGION, Species) %>%
  drop_na(Species) %>%
  group_by(ECOREGION, Species) %>%
  summarise() %>%
  mutate(Status = "Native",
         Introductions = NA)

st_write(all_native, "outputs/native_data.geojson",  delete_dsn=TRUE )
all_native <- st_read("outputs/native_data.geojson")


# save data by ecoregions------------
all_native_data <- 
  all_native %>%
  as.data.frame() %>%
  dplyr::select(ECOREGION, Species) %>% 
  mutate(status = 'Native') %>% 
  write_csv('outputs/all_native_data.csv')

map_native_dist <- 
  world_map_low +
  geom_sf(data = all_native, aes(fill = Species), alpha = .7) +
  facet_wrap( ~ Species) +
  scale_fill_discrete(guide = F)

ggsave(
  map_native_dist,
  filename = 'map_native_dist.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 300,
  units = 'cm',
  width = 50,
  heigh = 30
)

