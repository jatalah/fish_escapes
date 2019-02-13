library(readxl)
library(tidyverse)
library(stringr)
library(sf)
library(rworldmap)
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
  mutate(
    Scientific_Name = fct_recode(Scientific_Name,
                                 "Scophthalmus maximus" = "Psetta maxima"),
    Scientific_Name = as.character(Scientific_Name)
  )
write_rds(prod_data_all, 'outputs/production_data_all.RDS')

# 1	IN	Freshwater
# 2	BW	Brackishwater
# 3	MA	Marine
# 101	AL	All environments

# get top produced fish species in more than 8 countries------
top_aqua_sp <- 
  prod_data_all %>%
  filter(ENVIRONMENT == 3,
         Major_Group == 'PISCES' & 
           YEAR > 2010) %>%
  group_by(Scientific_Name,  spp_name) %>%
  summarise(mean_quant = mean(QUANTITY,na.rm = T),
            country = n_distinct(ISO3_Code)) %>%
  filter(country > 0 &
           str_detect(Scientific_Name, " ") &
           !str_detect(Scientific_Name, "spp") &
           mean_quant>500) %>%
  arrange(desc(mean_quant)) %>%
  print(n = 50)

# write_csv('outputs/top_aqua_spp_2005.csv')


sp_names <- top_aqua_sp$Scientific_Name
# select study species and marine production only-----
prod_data <-
  prod_data_all %>%
  filter(Scientific_Name %in% sp_names &
           ENVIRONMENT == 3) %>%
  # group_by(
  #   COUNTRY,
  #   PRODUCTION_AREA,
  #   SPECIES ,
  #   YEAR,
  #   spp_name ,
  #   country_name,
  #   Scientific_Name,
  #   ENVIRONMENT,
  #   ISO3_Code
  # ) %>% 
  rename(Species = Scientific_Name) %>% 
  # ungroup() %>% 
  mutate(Species = fct_recode(Species, "Larimichthys crocea" = "Larimichthys croceus"))


write_rds(prod_data, 'outputs/production_data_all_study_spp.RDS')

# # view selected spp
# prod_data_all %>% 
#   filter(Scientific_Name == 'Lates calcarifer') %>% 
#   group_by(country_name, ENVIRONMENT, PRODUCTION_AREA) %>%
#   summarise(mean_quant = mean(QUANTITY,na.rm = T)) %>% 
#   print(n = 100)
  
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
  prod_data %>%
  filter(YEAR > 2006) %>% # latest 5 years of records
  group_by(country_name, Species, PRODUCTION_AREA, ISO3_Code, COUNTRY) %>%
  summarise(mean_prod = mean(QUANTITY, na.rm = T)) %>%
  filter(mean_prod > 0) %>% 
  mutate(ISO_N3 = as.numeric(COUNTRY)) %>% 
  ungroup() %>% 
  arrange(Species, PRODUCTION_AREA) %>% 
  write_csv("outputs/production_country_data.csv")

# add ecoregions manually based on www.seaaroundus.org mariculture dataset-------------- 
mean_prod_ecoreg <-
  read_csv('outputs/production_country_data_ecoreg_manual.csv') %>%
  group_by(Species, country_name, PRODUCTION_AREA, mean_prod) %>%
  mutate(n_eco = n_distinct(ECOREGION),
         mean_prod_ecoreg = mean_prod / n_eco)

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

mean_prod_total_ecoreg_sf <-
  mean_prod_ecoreg %>% 
  group_by(ECOREGION) %>% 
  mutate(total_prod = sum(mean_prod)) %>% 
  full_join(as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry')%>% 
  drop_na(Species)

production_total_ecoreg_map <- 
  world_map_low +
  geom_sf(data = mean_prod_total_ecoreg_sf, aes(fill = total_prod)) +
  scale_fill_gradientn(
    colours = rev(heat.colors(100)),
    name = 'Total production',
    trans = 'sqrt'
  )
  # scale_fill_gradientn(
  #   colours = c("red", "yellow", "green", "lightblue", "darkblue"),
  #   values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
  #   name = 'Total production',
  #   trans = 'log10'
  # )

production_total_ecoreg_map
