library(readxl)
library(tidyverse)
library(stringr)
library(sf)
source('theme_javier.R')

# species names-----------
study_spp <-
  read_excel('data/fish_escapes.xlsx', sheet = 'Names') 

# production data ---------------       
prod <- 
  read_csv('data/FAO aqua production/TS_FI_AQUACULTURE.csv') %>% 
  dplyr::select(COUNTRY:YEAR,QUANTITY, VALUE)

# country codes-------
country <- 
  read_csv('data/FAO aqua production/CL_FI_COUNTRY_GROUPS.csv') %>% 
  rename(COUNTRY = "UN_Code",
         country_name = "Name_En") %>% 
  dplyr::select(COUNTRY, ISO3_Code, country_name)

# species codes------5
spp <-
  read_csv('data/FAO aqua production/CL_FI_SPECIES_GROUPS.csv') %>%
  rename(SPECIES = "3Alpha_Code",
         spp_name = "Name_En") %>%
  dplyr::select(SPECIES, spp_name, Scientific_Name, Major_Group) 


# join all data------
prod_data_all <-
  left_join(prod, country, by = 'COUNTRY') %>%
  left_join(spp, by = "SPECIES") %>%
  mutate(Scientific_Name = fct_recode(Scientific_Name,  "Scophthalmus maximus" = "Psetta maxima"),
         Scientific_Name = as.character(Scientific_Name))

write_rds(prod_data_all, 'outputs/production_data_all.RDS')


prod_data <-
  prod_data_all %>%
  filter(Scientific_Name %in% study_spp$Species) %>%
  group_by(COUNTRY,
           PRODUCTION_AREA,
           SPECIES ,
           YEAR,
           spp_name ,
           country_name,
           Scientific_Name,
           ENVIRONMENT,
           ISO3_Code)

write_rds(prod_data, 'outputs/production_data_all_study_spp.RDS')

# summary marine production by species and country---
marine_prod <-
  prod_data %>%
  dplyr::filter(ENVIRONMENT == 3)

# get top produced fish species in more than 8 countries------
prod_data_all %>%
  filter(ENVIRONMENT == 3,
           Major_Group == 'PISCES' & 
           YEAR > 2015) %>%
  group_by(Scientific_Name,  spp_name) %>%
  summarise(tot_quant = sum(QUANTITY),
            country = n_distinct(ISO3_Code)) %>%
  filter(country > 6 &
           str_detect(Scientific_Name, " ") &
           !str_detect(Scientific_Name, "spp")) %>%
  arrange(desc(tot_quant)) %>%
  print(n = 50)

# plot times series function --------
plot_trend <-
  function(data) {
    data %>%
      arrange(YEAR) %>%
      ggplot() +
      geom_path(aes(YEAR, QUANTITY, color = factor(PRODUCTION_AREA))) +
      facet_wrap( ~ country_name, scales = 'free_y') +
      ggtitle(first(data$Scientific_Name)) +
      scale_color_viridis_d(name= 'Production Area') +
      theme_bw()
  }

# times series plot by species and country--------- 
nest_production <- 
  all_prod %>% 
  group_by(spp_name) %>% 
  nest() %>% 
  mutate(plots = map(.x = data, .f = plot_trend),
         filename = paste0(spp_name, ".tiff"))

# get plots one by one 
nest_production[[3]][[1]]


# save all plots separatedly-----
walk2(.x = nest_production$filename,
      .y = nest_production$plots,
  ~ ggsave(
    filename = paste0('figures/',.x),
    plot = .y,
    height = 8,
    width = 12
  )
)

# summarise data by species and country---------
mean_prod <-
  marine_prod %>%
  filter(YEAR > 2006) %>% # latest 5 years of records
  group_by(country_name, Scientific_Name, PRODUCTION_AREA, ENVIRONMENT,ISO3_Code, COUNTRY) %>%
  summarise(mean_prod = mean(QUANTITY, na.rm = T)) %>%
  filter(mean_prod > 0) %>% 
  mutate(ISO_N3 = as.numeric(COUNTRY)) %>% 
  write_csv("outputs/production_country_data.csv")


# join with world by contry--------
mean_prod_world <- 
  full_join(
  getMap(resolution = "high") %>%
    st_as_sf() %>%
    dplyr::select(ADMIN, SUBUNIT, ISO_N3) %>% 
    as.data.frame(),
  mean_prod,
  by = "ISO_N3"
)

# get ecoregions of the world shapefile----
eco_reg <-
  st_read('data/MEOW/meow_ecos.shp') %>%
  dplyr::select(ECOREGION)

production_ecoreg_sf <-
  mean_prod_world %>%
  st_sf(sf_column_name = 'geometry') %>% # convert to sf  object
  st_join(eco_reg, .) %>% # join subunit with ecoregion polygons
  drop_na(Scientific_Name) %>% 
  group_by(ECOREGION, Scientific_Name) %>% 
  summarise(mean_prod = mean(mean_prod, na.rm = T)) %>% 
  rename(Species = "Scientific_Name")

st_write(production_ecoreg_sf, 'outputs/production_ecoreg_sf.geojson', delete_dsn=TRUE)

production_ecoreg_data <- 
  production_ecoreg_sf %>% 
  dplyr::select(-geometry) %>% 
  as.data.frame() %>% 
  write_csv('outputs/production_ecoreg_data.csv')

# merge with habitat suitability-------------
all_suitability_data_ecoreg <- read_csv('outputs/all_suitability_data_ecoreg.csv')

suitable_production_ecoreg_data <-
  inner_join(
    production_ecoreg_data,
    all_suitability_data_ecoreg,
    by = c("Species", "ECOREGION")
  ) %>% 
  dplyr::filter(prob>0.3) %>%
  dplyr::select(-geometry) %>% 
  write_csv("outputs/suitable_production_ecoreg_data.csv")

suitable_production_ecoreg_data_sf <- 
  right_join(eco_reg %>% as.data.frame(),
             suitable_production_ecoreg_data,
             by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry')

st_write(suitable_production_ecoreg_data_sf, 'outputs/suitable_production_ecoreg_data_sf.geojson', delete_dsn=TRUE)

map_suitable_production <-
  world_map_low +
  geom_sf(data = suitable_production_ecoreg_data_sf, aes(fill = mean_prod)) +
  facet_wrap( ~ Species) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values =  c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Mean production',
    trans = 'log',
    labels = scales::scientific
  ) +
  theme_bw()

map_suitable_production

ggsave(
  map_suitable_production,
  filename = 'figures/production_maps.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 30,
  heigh = 15
)

# FAO production areas------------
fao_areas <- st_read('data/FAO areas/FAO_AREAS.shp')

world_map_low +
  geom_sf(data = fao_areas, aes(fill = F_AREA)) +
  scale_fill_viridis_d()
