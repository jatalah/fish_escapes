library(tidyverse)
library(sf)

# load data --------
mean_prod_ecoreg <- read_csv('outputs/mean_prod_ecoreg_manual.csv')
all_native_data <- read_csv('outputs/all_native_data.csv')

prod_all_status <-
  full_join(mean_prod_ecoreg,
            all_native_data,
            by = c('Species', 'ECOREGION')) %>%
  filter(!is.na(mean_prod)) %>%
  mutate(
    status = ifelse(is.na(status), "Introduced", "Native"),
    Introduced = ifelse(status == "Introduced", 1, 0),
    Native = ifelse(status == 'Native', 1, 0)
  ) %>%
  ungroup() %>%
  write_csv('outputs/prod_all_status.csv')


# join with MEOW data ------------
prod_all_status %>%
  left_join(select(as.data.frame(eco_reg), REALM, PROVINCE, ECOREGION), by = 'ECOREGION') %>%
  select(
    REALM,
    PROVINCE,
    ECOREGION,
    everything(),
    -ISO_N3,
    -ISO3_Code,
    -PRODUCTION_AREA,
    -Introduced,
    -Native,
    -COUNTRY
  ) %>% 
write_csv('outputs/prod_all_status_ecoreg.csv')

