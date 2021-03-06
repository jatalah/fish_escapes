library(readxl)
library(tidyverse)
library(stringr)
library(sf)
library(rworldmap)
source('theme_javier.R')

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
  mutate(
    Scientific_Name = fct_recode(Scientific_Name, "Scophthalmus maximus" = "Psetta maxima"),
    Scientific_Name = as.character(Scientific_Name)
  )

# write_rds(prod_data_all, 'outputs/production_data_all.RDS')
# prod_data_all <- read_rds('outputs/production_data_all.RDS')


# get top produced fish species in more than 8 countries------
top_aqua_sp <-
  prod_data_all %>%
  filter(ENVIRONMENT == 3,
         Major_Group == 'PISCES' &
           YEAR > 2010) %>%
  group_by(Scientific_Name,  spp_name) %>%
  summarise(mean_quant = mean(QUANTITY, na.rm = T),
            country = n_distinct(ISO3_Code)) %>%
  filter(
    country > 0 &
      str_detect(Scientific_Name, " ") &
      !str_detect(Scientific_Name, "spp") &
      mean_quant > 500
  ) %>%
  arrange(desc(mean_quant)) %>%
  rename(Species = "Scientific_Name") %>%
  ungroup() %>% 
  mutate(
    Species = fct_recode(Species,
      "Larimichthys crocea" = "Larimichthys croceus",
      "Sebastes schlegelii" = "Sebastes schlegeli",
      "Acanthopagrus schlegelii" = "Acanthopagrus schlegeli")
  ) %>%
  print(n = 50) %>%
  write_csv('outputs/top_aqua_sp.csv')

sp_names <- top_aqua_sp$spp_name
sp_sci_names <- top_aqua_sp$Species


# select study species and marine production only-----
prod_data <-
  prod_data_all %>%
  filter(Scientific_Name %in% sp_sci_names & ENVIRONMENT == 3) %>%
  rename(Species = Scientific_Name)

write_rds(prod_data, 'outputs/production_data_all_study_spp.RDS')

# prod_data <- read_rds('outputs/production_data_all_study_spp.RDS')

# summarise data by species and country---------
mean_prod <-
  prod_data %>%
  filter(YEAR > 2006) %>% # latest 5 years of records
  group_by(country_name, Species, PRODUCTION_AREA, ISO3_Code, COUNTRY) %>%
  summarise(mean_prod = mean(QUANTITY, na.rm = T)) %>%
  filter(mean_prod > 0) %>% 
  mutate(ISO_N3 = as.numeric(COUNTRY)) %>% 
  ungroup() %>% 
  arrange(Species, PRODUCTION_AREA) %>% 
  write_csv("outputs/production_country_data.csv")

# add ecoregions based on www.seaaroundus.org mariculture dataset-------------- 
mean_prod_ecoreg <-
  read_csv('outputs/production_country_data_ecoreg_manual.csv') %>%
  group_by(Species, country_name, PRODUCTION_AREA) %>%
  mutate(n_eco = n_distinct(ECOREGION),
         mean_prod_ecoreg = mean_prod / n_eco) %>% 
  write_csv('outputs/mean_prod_ecoreg.csv')

# add ecoregions based on www.seaaroundus.org mariculture dataset-------------- 
mean_prod_ecoreg <-
mean_prod_ecoreg <- read_csv('outputs/mean_prod_ecoreg_manual.csv')
mean_prod_ecoreg <- distinct(mean_prod_ecoreg)

mean_prod_ecoreg %>% 
  distinct(.)

# get ecoregions of the world shapefile----
eco_reg <-
  st_read('data/MEOW/meow_ecos.shp') %>%
  dplyr::select(ECOREGION)

mean_prod_ecoreg_sf <- 
  full_join(mean_prod_ecoreg,as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry') %>% 
  rename(Species = "Species") %>% 
  drop_na(Species)

st_write(mean_prod_ecoreg_sf, 'outputs/production_ecoreg_sf.geojson', delete_dsn=TRUE)