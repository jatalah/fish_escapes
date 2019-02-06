rm(list=ls())

library(sf)
library(tidyverse)
library(rworldmap)
library(mapview)
theme_set(theme_bw())


# get the world map and convert into sf object----
world_highres <- 
  getMap(resolution = "high") %>% 
  st_as_sf()

world_map_highres <- 
  ggplot(data = world_highres) +
  geom_sf(fill = 'gray95')


world_low <- 
  getMap(resolution = "low") %>% 
  st_as_sf()

world_map_low <- 
  ggplot(data = world_low) +
  geom_sf(fill = 'gray95')

# save maps as RDS 
saveRDS(world_map_highres,'outputs/world_map_highres.RDS')
saveRDS(world_map_low,'outputs/world_map_low.RDS')

# get ecoregions of the world shapefile----
eco_reg <- st_read('data/MEOW/meow_ecos.shp')
mapview(eco_reg["ECOREGION"], col.regions = sf.colors(999),legend =F)

# plot world with marine eco-regions-----
world_map_low +
  geom_sf(data = eco_reg, aes(fill = ECOREGION), alpha = .5) +
  scale_fill_discrete(guide = F)

# write ecoregions and world as csv for checking------------
eco_reg %>% 
  as.tibble() %>% 
  select( ECO_CODE_X, ECOREGION) %>% 
  write_csv('eco_reg.csv')

world_highres %>% 
  as.tibble () %>% 
  dplyr::select( SOVEREIGNT, NAME_FORMA, NAME, SUBUNIT, ADMIN) %>% 
  write_csv('world.csv')

# read geojson data wieth introductions and native ranges----------
all_introduced <- st_read("outputs/introduction_data.geojson")
all_native <- st_read("outputs/native_data.geojson")

# bind native and introduced datasets----
both_dist <- rbind(all_native,all_introduced)

# ggsave map function
save_map <-
  function(map, filename) {
    ggsave(
      map,
      filename = paste0('figures/',
                        filename,
                        '.tiff'),
      device = 'tiff',
      compression = 'lzw',
      dpi = 600,
      units = 'cm',
      width = 30,
      heigh = 15
    )
  }

# map distributions by species------------
map_both_dist <- 
  world_map_low +
  geom_sf(data = both_dist, aes(fill = Status), alpha = .7) +
  facet_wrap( ~ Species)

save_map(map = map_both_dist, filename = 'both_dist')

# Habitat suitability ----------
all_suitability <- st_read("outputs/suitability_data.geojson")

# map by taxa and ecoregion------------
suitability_plot <- 
  world_map_low +
  geom_sf(data = all_suitability, aes(fill = prob), alpha = .7) +
  facet_wrap( ~ Species) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Probability of occurrence'
  )

save_map(map = suitability_plot, filename = 'suitability_plot_eocregions')

# habitat suitability point data maps-------------
habitat_suit_raw <- 
  world_map_low +
  geom_point(data = read_csv('outputs/all_hab_suitability_raw.csv'), aes(lon,lat, color = prob), alpha = .1, size = .3) +
  facet_wrap( ~ Species) +
  scale_color_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Probability of occurrence'
  )

save_map(map = habitat_suit_maps_raw, filename = 'suitability_plot_raw')

# production by ecoregion--------
mean_prod_ecoreg_sf <- st_read('outputs/production_ecoreg_sf.geojson', delete_dsn=TRUE)

production_ecoreg_map <- 
  world_map_low +
  geom_sf(data = mean_prod_ecoreg_sf, aes(fill = mean_prod)) +
  facet_wrap( ~ Species) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Mean production',
    trans = 'log10'
  )

save_map(map = production_ecoreg_map, filename = 'production_by_ecoreg_map')

# catches by ecoregion---------
catch_ecoreg <- st_read('outputs/catch_by_ecoreg.geojson')

catch_ecoreg_map <- 
  world_map_low +
  geom_sf(data = catch_ecoreg, aes(fill = mean_catch)) +
  facet_wrap( ~ Scientific_Name) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Total catch',
    trans = 'log10'
  )

save_map(map = catch_ecoreg_map, filename = 'catch_ecoreg_map.tiff')

# biodiversity maps---------
obis_summ <- st_read('data/OBIS/summaries.shp')

es50_map <- 
  world_map_low +
  geom_sf(data = obis_summ, aes(fill = es)) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'ES50'
  )

save_map(map = es50_map, filename = 'es50_map.tiff')

# diseases map--------------
diseases_ecoreg <- st_read('outputs/diseases_ecoreg.geojson')

map_diseases <- 
  world_map_low +
  geom_sf(data = diseases_data_sf, aes(fill = cumm_diseases), alpha = .7) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    na.value = 'gray70',
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Number of diseases'
  )


ggsave(map_diseases,
       filename = 'figures/map_diseases.tiff',
device = 'tiff',
compression = 'lzw',
dpi = 600,
units = 'cm',
width = 8,
heigh = 6
)
