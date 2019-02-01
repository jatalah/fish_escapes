library(tidyverse)
library(readxl)
library(rfishbase)
library(sf)


# study species names --------
study_spp <-
  read_excel('data/fish_escapes.xlsx', sheet = 'Names') 

# capture data ---------------       
catch <- 
  read_csv('data/Capture_2018.1.2/TS_FI_CAPTURE.csv') %>% 
  mutate(COUNTRY = as.numeric(COUNTRY))

# species codes------
spp <-
  read_csv('data/Capture_2018.1.2/CL_FI_SPECIES_GROUPS.csv') %>%
  rename(SPECIES = "3Alpha_Code",
         spp_name = "Name_En") %>%
  dplyr::select(SPECIES, spp_name, Scientific_Name, Major_Group) 


# country codes-------
country <- 
  read_csv('data/Capture_2018.1.2/CL_FI_COUNTRY_GROUPS.csv') %>% 
  rename(COUNTRY = "UN_Code",
         country_name = "Name_En") %>% 
  mutate(COUNTRY = as.numeric(COUNTRY)) %>% ## check match with country
  dplyr::select(COUNTRY, ISO3_Code, country_name)


# join catch all data------
catch_data_all <- 
  left_join(catch, country, by = 'COUNTRY') %>% 
  left_join(spp, by = "SPECIES") %>% 
  mutate(Scientific_Name = fct_recode(Scientific_Name,  "Scophthalmus maxima" = "Psetta maxima")) 

saveRDS(catch_data_all,'outputs/catch_data_all.RDS')
catch_data_all <- readRDS('outputs/catch_data_all.RDS')

catch_data <-
  catch_data_all %>% # unaccepeted name
  filter(Scientific_Name %in% study_spp$Species) %>%
  dplyr::select(COUNTRY,
                FISHING_AREA,
                YEAR,
                spp_name ,
                country_name,
                Scientific_Name,
                QUANTITY) %>% 
  write_csv('outputs/catch_data.csv')

catch_data <- read_csv('outputs/catch_data.csv')

# plot times series function --------
plot_trend <-
  function(data) {
    data %>%
      arrange(YEAR) %>%
      ggplot() +
      geom_path(aes(YEAR, QUANTITY, color = factor(FISHING_AREA))) +
      facet_wrap( ~ country_name, scales = 'free_y') +
      ggtitle(first(data$Scientific_Name)) +
      scale_color_viridis_d(name= 'Fishing Area') +
      theme_bw()
  }

nest_catch <- 
  catch_data %>% 
  group_by(spp_name) %>% 
  nest() %>% 
  mutate(plots = map(.x = data, .f = plot_trend),
         filename = paste0(spp_name, ".tiff"))


# get plots one by one 
nest_catch[[3]][[1]]


# save all plots separatedly-----
walk2(
  .x = nest_catch$filename,
  .y = nest_catch$plots,
  ~ ggsave(
    filename = paste0('figures/captures times series/', .x),
    plot = .y,
    height = 8,
    width = 12
  )
)

# summarise data by species and country---------
mean_catch_data <-
  catch_data %>%
  filter(YEAR > 2006) %>% # latest 10 years of records
  group_by(country_name, Scientific_Name, FISHING_AREA) %>%
  summarise(mean_catch = mean(QUANTITY, na.rm = T)) %>%
  filter(mean_catch > 0) %>%
  rename(SUBUNIT = 'country_name')


# join by country---
world <- 
  rworldmap::getMap(resolution = "high") %>% 
  st_as_sf() %>% 
  dplyr::select(ADMIN, SUBUNIT)

mean_catch_data_world <- full_join(world %>% as.data.frame(), mean_catch_data, by = "SUBUNIT")

# check missing countries-----
filter(mean_catch_data, is.na(SUBUNIT))

# get ecoregions of the world shapefile----
eco_reg <- 
  st_read('data/MEOW/meow_ecos.shp') %>% 
  dplyr::select(ECOREGION)

catch_ecoreg_sf <-
  mean_catch_data_world %>%
  st_sf(sf_column_name = 'geometry') %>% # convert to sf  object
  st_join(eco_reg, .) %>% # join subunit with ecoregion polygons
  drop_na(Scientific_Name) %>%
  group_by(ECOREGION, Scientific_Name) %>%
  summarise(mean_catch = mean(mean_catch, na.rm = T))

st_write(catch_ecoreg_sf, 'outputs/catch_by_ecoreg.geojson', delete_dsn=TRUE)
catch_ecoreg_sf <- st_read('outputs/catch_by_ecoreg.geojson')

# save dat -------------------
catch_ecoreg_data <- 
  catch_ecoreg_sf %>% 
  dplyr::select(-geometry) %>% 
  as.data.frame() %>% 
  rename(Species = 'Scientific_Name') %>% 
  write_csv('outputs/catch_ecoreg_data.csv')


# merge with habitat suitability-------------
all_suitability_data_ecoreg <- read_csv('outputs/all_suitability_data_ecoreg.csv')

suitable_catch_ecoreg_data <-
  inner_join(
    catch_ecoreg_data,
    all_suitability_data_ecoreg,
    by = c("Species", "ECOREGION")
  ) %>% 
  dplyr::filter(prob>0.3) %>%
  dplyr::select(-geometry) %>% 
  write_csv("outputs/suitable_catch_ecoreg_data.csv")

suitable_catch_ecoreg_data_sf <- 
  right_join(eco_reg %>% as.data.frame(),
             suitable_catch_ecoreg_data,
             by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry')

st_write(suitable_catch_ecoreg_data_sf, 'outputs/suitable_catch_ecoreg_data_sf.geojson', delete_dsn=TRUE)


suitable_catch_ecoreg_map <- 
  world_map_low +
  geom_sf(data = suitable_catch_ecoreg_data_sf, aes(fill = mean_catch)) +
  facet_wrap( ~ Species) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Total catch'
  ) +
  theme_bw()

ggsave(
  suitable_catch_ecoreg_map,
  filename = 'figures/suitable_catch_ecoreg_map.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 30,
  heigh = 15
)


# FAO production areas------------
fao_areas <-
  st_read('data/FAO areas/FAO_AREAS.shp') %>%
  rename(FISHING_AREA = "F_AREA") %>%
  mutate(FISHING_AREA = as.numeric(FISHING_AREA))

sum_catch <- 
  sum_catch %>%
  mutate(FISHING_AREA = as.factor(FISHING_AREA))

sum_catch_fao_region <-
  full_join(fao_areas %>% as.data.frame(), sum_catch, by = "FISHING_AREA")
saveRDS(sum_catch_fao_region, 'outputs/sum_catch_fao_region.RDS')

catch_fao_areas <-
  sum_catch_fao_region %>%
  st_sf(sf_column_name = 'geometry') %>% # convert to sf  object
  drop_na(Scientific_Name) %>%
  group_by(FISHING_AREA, Scientific_Name) %>%
  summarise(total_catch = sum(mean_catch))

catch_fao_areas_map <-
  ggplot(data = world) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = catch_fao_areas, aes(fill = total_catch)) +
  facet_wrap(~ Scientific_Name) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Total catch'
  )

ggsave(
  catch_fao_areas_map,
  filename = 'figures/catch_fao_regions_map.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 30,
  heigh = 15
)