library(tidyverse)
library(rfishbase)
library(readxl)

# read species data---------------
study_spp <-
  read_excel('data/fish_escapes.xlsx', sheet = 'Names')

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


glimpse(fisbase_traits[['stocks']][[3]])
fisbase_traits[['ResilienceRemark']][[1]]

# get growth estimates MaxLength, K and trophic level---------------
estimates <-
  study_spp %>%
  mutate(estimates = map(Species, rfishbase::estimate)) %>%
  select(estimates) %>%
  flatten_df() %>%
  select(Species, MaxLengthTL, Troph, a , K) 

ord <- prcomp(estimates[,c(2,3,5)], scale. = T, center = T)
estimates$eco_pca <- ord$x[,1]
write_csv(estimates, 'outputs/estimates_fishbase.csv')


biplot(ord)

estimates <-
  read_csv('outputs/estimates_fishbase.csv') %>%
  dplyr::select(Species, eco_pca) %>% 
  mutate(eco_pca = (max(eco_pca)- eco_pca) + 1)


ecology_data_species_ecoreg <-
  estimates %>%
  dplyr::select(Species, eco_pca) %>%
  right_join(mean_prod_ecoreg, by = 'Species') 

ecology_score <-
  ecology_data_species_ecoreg %>% 
  group_by(ECOREGION) %>%
  mutate(ecol = eco_pca * sqrt(mean_prod)/1e+3) %>%
  summarise(ecol_total = sum(ecol)) %>% 
  right_join(eco_reg %>% as.data.frame(), by = "ECOREGION") %>%
  st_sf(sf_column_name = 'geometry')



world_map_low +
  geom_sf(data = ecology_score, aes(fill = ecol_total), alpha = .7) +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "lightblue", "darkblue"),
    na.value = 'gray70',
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    name = 'Ecological score'
  ) +
  theme_bw()


# fecundity information---------
study_spp %>%
  mutate(fecund = map(Species, rfishbase::fecundity)) %>%
  select(fecund) %>% 
  flatten_df()

# population growth parameter--------------------
pop_growth <-
  study_spp %>%
  mutate(popgrow = map(Species, rfishbase::popgrowth)) %>%
  select(popgrow) %>%
  flatten_df() %>%
  select(Species, Loo, K) %>%
  group_by(Species) %>%
  summarise(Loo = mean(Loo, na.rm = T),
            k = mean(K, na.rm = T))


# 01 get diseases data from fishbase--------
fisbase_diseases <-
  study_spp %>%
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
