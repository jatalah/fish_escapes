library(tidyverse)
library(rfishbase)
rm(list=ls())

# funtion to scle between 0 and 1------
range_0_1 <- function(x){(x-min(x))/(max(x)-min(x))}

# get growth estimates MaxLength, K and trophic level---------------
top_aqua_sp <-
  read_csv('outputs/top_aqua_sp.csv')

estimates <-
  top_aqua_sp %>%
  mutate(estimates = map(Species, rfishbase::estimate)) %>%
  select(estimates) %>%
  unnest() %>%
  select(Species, MaxLengthTL, Troph, K) %>%
  mutate(K = if_else(is.na(K), mean(K, na.rm = T), K)) %>%
  mutate(eco_ind = (range_0_1(K) + range_0_1(Troph) + range_0_1(MaxLengthTL))/3) %>%
  write_csv('outputs/estimates_fishbase.csv')

# join ecological estimates with production data------------
mean_prod_ecoreg <- read_csv('outputs/mean_prod_ecoreg.csv')

ecology_data_species_ecoreg <-
  estimates %>%
  right_join(mean_prod_ecoreg, by = 'Species') %>%
  write_csv('outputs/ecology_data_species_ecoreg.csv')
