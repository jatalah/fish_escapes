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
    read_native('rabbitfish.csv'),# not in the top 30 species
    read_native('trout.csv'),
    read_native('tuna.csv'),
    read_native('meagre.csv'),
    read_native('turbot.csv'),
    read_native('red_drum.csv'),
    read_native('pagrus_major.csv'),
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
  mutate(Species = fct_recode(Species, 
                              "Scophthalmus maximus" = "Scophthalmus rhombus",
                              "Pagrus auratus" = "Pagrus major")) %>% 
  write_csv('outputs/native_dist_data.csv')

# join native distribution data with ecoregions of the world-----
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


# save data by ecoregions------------
missing_sp <-
  cbind(ECOREGION = "Yellow Sea",
        Species = c("Larimichthys crocea",
                    "Pagrus auratus",
                    "Acanthopagrus schlegelii",
                    "Mugil cephalus",
                    "Paralichthys olivaceus",
                    "Rachycentron canadum", 
                    "Sebastes schlegelii",
                    "Seriola quinqueradiata",
                    "Trachurus japonicus"
                    )) %>%
  as.data.frame()

all_native_data <- 
  all_native %>%
  as.data.frame() %>%
  dplyr::select(ECOREGION, Species) %>% 
  bind_rows(missing_sp) %>% 
  mutate(status = 'Native') %>% 
  filter(Species!="Siganus rivulatus") %>% 
  write_csv('outputs/all_native_data.csv')


# save distribution data as sf-----------
all_native_data_sf <- 
  all_native_data %>% 
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>%
  st_sf(sf_column_name = 'geometry') %>% 
  st_write("outputs/all_native_data_sf.geojson",  delete_dsn=TRUE )



# map native distribution data-----------
all_native_data_sf <- st_read("outputs/all_native_data_sf.geojson")

world_less_is <- 
  getMap(resolution = "less islands") %>% 
  st_as_sf()

p <-
  ggplot(data = world_less_is) +
  geom_sf(fill = 'gray95') +
  theme( axis.text.x=element_blank(), axis.text.y=element_blank())

map_native_dist <- 
  p +
  geom_sf(data = all_native_data_sf, aes(fill = Species), alpha = .7) +
  facet_wrap( ~ Species) +
  scale_fill_discrete(guide = F) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())


ggsave(
  map_native_dist,
  filename = 'figures/map_native_dist.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 300,
  units = 'cm',
  width = 50,
  heigh = 30
)
