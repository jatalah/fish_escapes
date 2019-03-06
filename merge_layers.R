library(tidyverse)
library(sf)
library(rworldmap)
theme_set(theme_minimal())


# add ecoregion polygons----------
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

# production data---------------
prod_all_status_scores <-
  read_csv('outputs/prod_all_status.csv') %>%
  dplyr::select(Species,
                ECOREGION,
                mean_prod_ecoreg,
                status) %>%
  group_by(ECOREGION) %>%
  summarise(
    n_total = n_distinct(Species),
    n_introduced = n_distinct(Species[status == "Introduced"]),
    n_native = n_distinct(Species[status == "Native"]),
    prod_total = sum(mean_prod_ecoreg),
    prod_native = sum(mean_prod_ecoreg[status == "Native"]),
    prod_introduced = sum(mean_prod_ecoreg[status == "Introduced"]),
    genetic_score = n_native * log10(prod_native + 1)
  ) %>%
  write_csv('outputs/prod_all_status_scores.csv')


production_dat_long <-
  prod_all_status_scores %>%
  gather(key, value, c("prod_native", "prod_introduced")) %>%
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>%
  st_sf(sf_column_name = 'geometry') %>%
  filter(value > 0) %>%
  mutate(key = fct_recode(key, Native = "prod_native",
                          Introduced = "prod_introduced"))

ggplot(data = world_less_is) +
  geom_sf(fill = 'gray95') +
  geom_sf(data = production_dat_long, aes(fill = value), 
          na.rm = T,
          alpha = 0.8)  +
  scale_fill_gradientn(colours = rev(heat.colors(10)),
                       name = 'Tonnes',
                       trans = 'log10') +
  facet_wrap( ~ key, ncol = 1) +
  theme_minimal(base_size = 10) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key.size =  unit(0.4, "cm"),
    # legend.title.align = 0.5,
    legend.position = c(.1,.6)
  )


# number of fish data ----------------------
fish_dat_long <-
  prod_all_status_scores %>%
  gather(key, value, c("n_introduced", "n_native")) %>%
  left_join(as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry') %>% 
  filter(value>0) %>% 
  mutate(key = fct_recode(key, Native = "n_native",
                                         Introduced = "n_introduced"))

ggplot(data = world_less_is) +
  geom_sf(fill = 'gray90') +
  geom_sf(data = fish_dat_long, aes(fill = value), na.rm = T)  +
  scale_fill_gradientn(colours = rev(heat.colors(10)),
                       name = 'Number \nof species',
                       trans = 'log10') +
  facet_wrap( ~ key, ncol = 1) +
  theme_minimal(base_size = 10) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title.align = 0,
    legend.key.size =  unit(0.5, "cm"),
    legend.position = c(.1,.6)
  ) 


# disease data -------
disease_data_species_ecoreg <-
  read_csv('outputs/diseases_data_species_ecoreg.csv') %>%
  dplyr::select(Species, ECOREGION, AGENT) %>% 
  left_join(select(prod_all_status, Species, ECOREGION, status),  by = c("Species", "ECOREGION")) %>% 
  group_by(ECOREGION) %>% 
  mutate(n_pathogen = n_distinct(AGENT),
         n_pathogen_native = n_distinct(AGENT[status=='Native']),
         n_pathogen_introduced = n_distinct(AGENT[status=='Introduced']))


disease_data_ecoreg <- 
  disease_data_species_ecoreg %>% 
  select(ECOREGION, n_pathogen:n_pathogen_introduced) %>% 
  group_by(ECOREGION) %>% 
  summarise_all(first)

disease_score <- 
  disease_data_ecoreg %>% 
  left_join(prod_all_status_ecoregion, by = 'ECOREGION') %>% 
  mutate(pathogenic_score = n_pathogen * log_mean_prod_total) %>% 
  select(ECOREGION, pathogenic_score)

# ecological data --------
invasive_ecoreg <-
  read_csv('outputs/estimates_fishbase.csv') %>% 
  full_join(prod_all_status_scores, by = "Species") %>% 
  filter(status == "Introduced") %>% 
  select(ECOREGION, Species, eco_pca, log_prod) %>% 
  group_by(ECOREGION, Species) %>%
  summarise_all(first) %>% 
  group_by(ECOREGION) %>% 
  summarise(invasive_score = sum(eco_pca*log_prod)) %>% 
  full_join(ecoreg_with_data,  by = "ECOREGION") %>% 
  mutate(invasive_score = ifelse(is.na(invasive_score),0,invasive_score))



# merge all data ------------
score_data <-
  full_join(prod_all_status_ecoregion,invasive_ecoreg, by = "ECOREGION") %>% 
  full_join(disease_score, by = "ECOREGION") 

std_scores <-
  score_data %>%
  mutate(
    Genetic = (genetic_score - min(genetic_score, na.rm = T)) / (max(genetic_score) - min(genetic_score)) *
      100,
    Invasive = (invasive_score - min(invasive_score, na.rm = T)) / (max(invasive_score, na.rm = T) - min(invasive_score, na.rm = T)) *
      100,
    Diseases = (pathogenic_score - min(pathogenic_score)) / (max(pathogenic_score) - min(pathogenic_score)) *
      100,
    final_score = ((Genetic*1) + (Invasive*1) + (Diseases*1))/3, # can add expert weight shere
    sq_final_score = sqrt(final_score),
    Overall = (sq_final_score - min(sq_final_score)) / (max(sq_final_score) - min(sq_final_score)) * 100
  )


std_scores %>% 
  gather(key, value, genetic_score:Overall) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free')

# add ecoregion polygons----------
eco_reg <- st_read('data/MEOW/meow_ecos.shp')

score_data_sf <- 
  std_scores %>% 
  full_join(as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry') %>%
  drop_na(mean_prod_total) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), km^2)))


std_scores %>% 
  gather(key, value, genetic_score:Overall) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free')


ggplot(score_data_sf, aes(area, Overall)) +
  geom_point()


# create maps-----------
world_less_is <- 
  getMap(resolution = "less islands") %>% 
  st_as_sf()

p <-
  ggplot(data = world_less_is) +
  geom_sf(fill = 'gray95') +
  theme( axis.text.x=element_blank(), axis.text.y=element_blank())

map_overall <- 
  p +
  geom_sf(data = score_data_sf, aes(fill = Overall), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Overall risk')+
  coord_sf(ylim = c(-55, 90),
           xlim = c(-160, 170))

p +
  geom_sf(data = score_data_sf, aes(fill = Overall), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Overall risk')+
  coord_sf(ylim = c(-55, 90),
           xlim = c(-160, 170))

map_genetic <- 
  p +
  geom_sf(data = score_data_sf, aes(fill = Genetic), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Genetic risk') +
  coord_sf(ylim = c(-55, 90),
           xlim = c(-160, 170))


map_invasive <-
  p + 
  geom_sf(data = score_data_sf, aes(fill = Invasive), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Invasive risk') +
  coord_sf(ylim = c(-55, 90),
           xlim = c(-160, 170))

map_diseases<-
  p + 
  geom_sf(data = score_data_sf, aes(fill = Diseases), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Disease risk') +
  coord_sf(ylim = c(-55, 90),
           xlim = c(-160, 170))

# library(ggpubr)
# ggarrange(
#   map_genetic,
#   map_invasive,
#   map_diseases,
#   map_overall,
#   labels = 'auto',
#   common.legend = T,
#   ncol = 2
# )

# faceted plots-----------
long_score_data_sf <- 
  score_data_sf %>% 
  gather(key, value, c("Genetic", "Invasive", "Diseases", "Overall" ))

production_dat_long <-
  score_data_sf %>%
  gather(key, value, c("mean_prod_native", "mean_prod_introduced" )) %>%
  filter(value>0)


ggplot(data = world_less_is) +
  geom_sf(fill = 'gray90') +
  geom_sf(data = production_dat_long, aes(fill = value), na.rm = T)  +
  scale_fill_gradientn(colours = rev(heat.colors(20)),
                       name = 'Mean annual \nproduction (tonnes)',
                       trans = 'log10') +
  facet_wrap( ~ status, ncol = 1) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title.align = 0.5
  ) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170))


score_plots <- 
  ggplot(data = world_less_is) +
  geom_sf(fill = 'gray90') +
  geom_sf(data = long_score_data_sf, aes(fill = value), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = '') +
  facet_wrap(~key) +
  theme( axis.text.x=element_blank(), axis.text.y=element_blank()) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160,170))

ggsave(
  score_plots,
  filename = "figures/scores.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 300,
  units = 'cm',
  width = 25,
  height = 12
)

# bar plots--------
score_data_ecoreg <- 
  score_data_sf %>% 
  as.data.frame()


score_data_ecoreg %>% 
gather(key, value,  c("Genetic", "Invasive", "Diseases" )) %>% 
  ggplot(aes(fct_rev(REALM), value, fill = key)) +
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
  labs(y = 'Relative impact risk', x = 'Realm') +
  facet_wrap( ~ key, scales= 'fixed') +
  theme_javier() +
  theme(legend.position = 'none') 


score_data_ecoreg %>% 
  gather(key, value,  c("Genetic", "Invasive", "Diseases" )) %>% 
  ggplot(aes(fct_rev(PROVINCE), value, fill = PROVINCE)) +
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
  labs(y = 'Relative impact risk', x = 'PROVINCE') +
  facet_wrap( ~ key, scales= 'fixed') +
  theme_javier() +
  scale_fill_viridis_d() +
  theme(legend.position = 'none') 



score_data_ecoreg %>% 
  gather(key, value,  c("Genetic", "Invasive", "Diseases" )) %>% 
  ggplot(aes(fct_rev(REALM), value, fill = key)) +
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
  labs(y = 'Relative impact risk', x = 'Realm') +
  facet_wrap( ~ key, scales= 'fixed') +
  theme_javier() +
  theme(legend.position = 'none') 


score_data_ecoreg %>% 
  ggplot(aes(fct_reorder(PROVINCE, as.numeric(REALM)), Overall, fill = REALM)) +
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
  coord_flip() +
  labs(y = 'Relative impact risk', x = 'PROVINCE') +
  theme_javier() +
  scale_fill_viridis_d()

score_data_ecoreg %>%
  ggplot(aes(PROVINCE, Overall)) +
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
  coord_flip() +
  facet_wrap( ~ REALM, scales = 'free_y') +
  labs(y = 'Relative impact risk', x = 'PROVINCE') +
  theme_javier() +
  scale_fill_viridis_d()
