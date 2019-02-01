# load libraries-----------------
rm(list=ls())

library(sf)
library(rvest)
library(tidyverse)
library(rworldmap)
library(rfishbase)
library(readxl)
theme_set(theme_bw())

# species names-----------
study_spp <-
  read_excel('data/fish_escapes.xlsx', sheet = 'Names') 

spp_names <- study_spp$Species

world_data <- 
  getMap(resolution = "high") %>% 
  as.data.frame() %>% 
  write_csv('data/world_data.csv')

# get introductions data from fishbase--------
fisbase_introductions <-
  study_spp %>%
  mutate(introductions = map(Species, introductions))

# bind all introductions data----
all_int <- 
  fisbase_introductions %>%  
  unnest() %>% 
  as.data.frame() %>% 
  write_csv('outputs/all_introductions_data_fishbase.csv')


introductions_references <- 
  all_int %>% 
  mutate(refs = map(.$IntrCaseRefNo, references)) %>% 
  unnest() %>% 
  write_csv('outputs/references_introductions.csv')


# # read FISHbase country codes =-------------
# fishbase_c_code <-
#   read_html('http://www.fishbase.org/country/ListOfCountryCodes.php') %>%
#   html_nodes(xpath = '/html/body/table') %>%
#   html_table(header = TRUE) %>%
#   .[[1]]


# join data -------------
all_intro <-
  all_int %>%
  mutate(C_Code_To = str_replace(C_Code_To, "[:alpha:]", ""),
         ISO_N3 = as.numeric(C_Code_To)) %>%
  as.data.frame()

# join introductions data with world and ecoregions-------
world <-
  getMap(resolution = "high") %>%
  st_as_sf(world) %>%
  dplyr::select(ADMIN, SUBUNIT, ISO_N3)


all_introd <- right_join(world %>% as.data.frame(), all_intro, by = "ISO_N3")


# get ecoregions of the world shapefile----
eco_reg <- 
  st_read('data/MEOW/meow_ecos.shp') %>% 
  dplyr::select(ECOREGION)
  
# join data introduction data with ecoregions------
all_introduced <-
  all_introd %>%
  dplyr::select(Species,geometry) %>% 
  st_sf(sf_column_name = 'geometry') %>% # convert to sf object
  st_join(eco_reg, .) %>% # join subunit with ecoregion polygons
  drop_na(Species) %>% 
  group_by(ECOREGION, Species) %>% 
  summarise(Introductions = n()) %>% 
  mutate(Status = "Introduced") %>% 
  ungroup()

all_introduced_data_ecoreg <- 
  all_introduced %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry, -Status) %>% 
  write_csv("outputs/introduction_data_ecoreg.csv")

st_write(all_introduced, "outputs/introduction_data.geojson", delete_dsn=TRUE )


## show introdction in areas with habitat suitability only------------
all_suitability_data_ecoreg <- read_csv('outputs/all_suitability_data_ecoreg.csv')

suitable_introductions_ecoreg_data <-
  inner_join(
    all_introduced_data_ecoreg,
    all_suitability_data_ecoreg,
    by = c("Species", "ECOREGION")
  ) %>% 
  dplyr::filter(prob>0.3) %>%
  write_csv("outputs/suitable_introductions_ecoreg_data.csv")



suitable_introductions_ecoreg_sf <- 
  right_join(eco_reg %>% as.data.frame(),
           suitable_introductions_ecoreg_data,
           by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry')

world_map_low +
  geom_sf(data = suitable_introductions_ecoreg_sf, aes(fill = Introductions)) +
  facet_wrap( ~ Species) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Number of introductions'
  )


