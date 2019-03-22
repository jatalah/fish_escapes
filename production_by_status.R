library(tidyverse)
library(mapview)
library(sf)
library(rworldmap)
theme_set(theme_minimal())
source('theme_javier.R')


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

## production maps---------------
prod_all_status_sf <- 
  full_join(prod_all_status, as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry') %>% 
  drop_na(Species)


sum_prod_all <- 
  prod_all_status_sf %>% 
  group_by(ECOREGION,status) %>% 
  summarise(sum_prod = sum(mean_prod_ecoreg, na.rm = T)) 

# Mapview-----
mapview(prod_all_status_sf["mean_prod_ecoreg"],
        col.regions = sf.colors(5),
        legend = T)


native_pro_map <- 
  sum_prod_all %>%
  dplyr::filter(status == 'Native') %>%
  mutate(log_prod = log10(sum_prod)) %>% 
  mapview(zcol = "log_prod",col.regions = rev(heat.colors(10)))


intro_pro_map <- 
  sum_prod_all %>%
  dplyr::filter(status == 'Introduced') %>%
  mutate(log_prod = log10(sum_prod)) %>% 
  mapview(zcol = "log_prod",col.regions = rev(heat.colors(10)))


latticeview(native_pro_map,intro_pro_map)

all_native %>%
  dplyr::filter(Species=='Sparus aurata') %>%
  mapview(zcol = "mean_prod_ecoreg",col.regions = sf.colors(5))
  


# world production by status overall-----------
prod_all_status %>% 
  group_by(status) %>% 
  summarise(sum(mean_prod_ecoreg))

plot_prod_status <-
  ggplot(prod_all_status, aes(status, mean_prod_ecoreg, fill = status)) +
  geom_boxplot() +
  scale_y_log10()

plot_prod_status

# production by status and species-----------
plot_production_status_species <-
  ggplot(prod_all_status, aes(fct_reorder(Species, mean_prod_ecoreg), mean_prod_ecoreg, fill = status)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    color = 1,
    position = position_dodge(width = .9),
    na.rm = T
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2,
    na.rm = T
  ) +
  theme_bw() +
  coord_flip() +
  labs(y = 'Mean annual production (tonnes)', x = 'Species') +
  scale_fill_discrete() +
  facet_wrap( ~ fct_rev(status), scales= 'free') +
  theme_javier() +
  theme(legend.position = 'none',
        axis.text.y = element_text(face = "italic"))

# save plot -
ggsave(
  plot_production_status_species,
  filename = "figures/plot_production_status_species.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 15,
  height = 15
)


# production by status and ECOREGION-----------
plot_production_status_ecoreg <-
  ggplot(
    prod_all_status,
    aes(
      x = fct_reorder(ECOREGION, mean_prod_ecoreg),
      mean_prod_ecoreg,
      fill = status
    )
  ) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    color = 1,
    position = position_dodge(width = .9),
    na.rm = T
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2,
    na.rm = T
  ) +
  theme_bw() +
  coord_flip() +
  labs(y = 'Mean annual production (tonnes)', x = 'Ecoregion') +
  scale_fill_discrete() +
  facet_wrap(~ fct_rev(status), scales = 'free') +
  theme_javier() +
  theme(legend.position = 'none')


# save plot -
ggsave(
  plot_production_status_ecoreg,
  filename = "figures/plot_production_status_ecoreg.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 25,
  height = 15
)

