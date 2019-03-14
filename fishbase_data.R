library(tidyverse)
library(rfishbase)
library(readxl)

# read species data---------------
study_spp <-
  read_excel('data/fish_escapes.xlsx', sheet = 'Names')

sp_names <- top_aqua_sp$Scientific_Name

# add fishbase information----------------
fisbase_data <-
  study_spp %>%
  mutate(
    ecology = map(Species, rfishbase::ecology),
    countries = map(Species, rfishbase::country),
    countrysub =  map(Species, rfishbase::countrysub),
    introductions = map(Species, rfishbase::introductions),
    reproduction = map(Species, rfishbase::reproduction),
    diet = map(Species, diet),
    stocks = map(Species, stocks),
    ecosystem = map(Species, ecosystem),
    FoodTroph = map_dbl(ecology, ~ .x$FoodTroph)
  )

write_rds(fisbase_data, 'outputs/fishbase_data.RDS')
fisbase_data <- read_rds('outputs/fishbase_data.RDS')

glimpse(fisbase_data[['diet']][[1]])
glimpse(fisbase_data[['stocks']][[3]])
glimpse(fisbase_data[['ecology']][[1]])

fisbase_data[['stocks']][[2]]$ResilienceRemark
fisbase_data[['stocks']][[3]]$Maturity


# get fish traits-------
fisbase_data %>% transmute(map(stocks, ~ .x$ResilienceRemark))
    
# Describe the popgrowth  (or other) table variables----------
dplyr::filter(docs(), table == "stocks")$description
docs("stocks")

    
    
    stocks = map(Species, rfishbase::stocks),
    FoodTroph = map_dbl(ecology, ~ .x$FoodTroph),
    Resilience = map(stocks, ~ .x$Resilience),
    ResilienceRemark = map(stocks, ~ .x$ResilienceRemark)
  )


fecundity('Salmo salar')

fecund_data <- 
  top_aqua_sp %>%
  mutate(fecundity = map(Species, rfishbase::fecundity)) %>%
  select(fecundity) %>%
  flatten_df()

fecund_data %>% 
  group_by(Species) %>% 
  summarise(RelFecundityMean = mean(RelFecundityMean, na.rm = T))

# get growth estimates MaxLength, K and trophic level---------------
top_aqua_sp <- 
  read_csv('outputs/top_aqua_sp.csv')

estimates <-
  top_aqua_sp %>%
  mutate(estimates = map(Species, rfishbase::estimate)) %>%
  select(estimates) %>%
  flatten_df() %>%
  select(Species, MaxLengthTL, Troph, a , K) %>% 
  mutate(K = if_else(is.na(K),mean(K,na.rm=T)))


library(ggord)
estimates <- 
  read_csv('outputs/estimates_fishbase.csv') %>%  
  mutate(K_inv = 1/K) %>% 
  as.data.frame()


ord <- prcomp(estimates[,c(2,3,5)], scale. = T, center = T)
summary(ord)
ggord(ord,obslab = T)

range_0_1 <- function(x){(x-min(x))/(max(x)-min(x))}



estimates <- 
  estimates %>% 
  mutate(eco_ind = (range_0_1(K) + range_0_1(Troph) + range_0_1(MaxLengthTL))/3) %>% 
  write_csv('outputs/estimates_fishbase.csv')
  
range_0_100 <- function(x){(x-min(x))/(max(x)-min(x))*100}

 



estimates <-
  read_csv('outputs/estimates_fishbase.csv')
  
  # dplyr::select(Species, eco_pca) %>% 


ecology_data_species_ecoreg <-
  estimates %>%
  # dplyr::select(Species, eco_pca) %>%
  right_join(mean_prod_ecoreg, by = 'Species') %>% 
  write_csv('outputs/ecology_data_species_ecoreg.csv')

ecology_score <-
  ecology_data_species_ecoreg %>% 
  group_by(ECOREGION) %>%
  # mutate(ecol = eco_pca * sqrt(mean_prod)/1e+3) %>%
  summarise(ecol_total = mean(eco_pca)) %>% 
  right_join(eco_reg %>% as.data.frame(), by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry')



world_map_low +
  geom_sf(data = ecology_score, aes(fill = ecol_total), alpha = .7) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    na.value = 'gray70',
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Invasiveness score'
  ) +
  theme_bw()


# fecundity information---------
top_aqua_sp %>%
  mutate(fecund = map(Species, rfishbase::fecundity)) %>%
  select(fecund) %>% 
  flatten_df() %>% 
  group_by(Species) %>% 
  summarise(Fecundity = mean(FecundityMin, na.rm = T))

# population growth parameter--------------------
pop_growth <-
  top_aqua_sp %>%
  mutate(popgrow = map(Species, rfishbase::popgrowth)) %>%
  select(popgrow) %>%
  flatten_df() %>%
  select(Species, Loo, K) %>%
  group_by(Species) %>%
  summarise(Loo = mean(Loo, na.rm = T),
            k = mean(K, na.rm = T))


# 01 get diseases data from fishbase--------
fisbase_fooditems <-
  top_aqua_sp %>%
  mutate(fooditems = map(Species, fooditems)) %>%
  unnest() %>%
  as.data.frame() %>% 
  filter(!str_detect(Foodname, "unident")) %>% 
  group_by(Species, Foodname) %>% 
  summarise(first(Foodgroup))


fisbase_diseases <-
  top_aqua_sp %>%
  mutate(stocks = map(Species, ~ stocks(fields = 'Diseases'))) %>%
  unnest() %>%
  as.data.frame()



fisbase_diseases <-
  all_int %>%
  mutate(refs = map(.$IntrCaseRefNo, references)) %>%
  unnest() %>%
  write_csv('outputs/references_introductions.csv')

list_fields("Diseases")
stocks("Salmo salar", fields = c("Species", "Diseases"))


# WORMS ---------
library('worrms')

wm_attr_data(id =  126975)
