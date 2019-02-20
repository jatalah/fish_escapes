prod_all_status <- 
  full_join(mean_prod_ecoreg,
          all_native_data,
          by = c('Species', 'ECOREGION')) %>%
  filter(!is.na(mean_prod)) %>% 
  mutate(status = if_else(is.na(status), "Introduced", "Native")) %>% 
  ungroup() %>% 
  write_csv('outputs/prod_all_status.csv')


prod_all_status_sf <- 
  full_join(prod_all_status, as.data.frame(eco_reg), by = 'ECOREGION') %>% 
  st_sf(sf_column_name = 'geometry')%>% 
  drop_na(Species)

# world productionby status overall-----------
plot_prod_status <-
  ggplot(prod_all_status, aes(status, mean_prod_ecoreg, fill = status)) +
  geom_boxplot() +
  scale_y_log10()

# maps of world production by status-------------
production_all_status_map <-
  world_map_low +
  geom_sf(data = prod_all_status_sf,
          aes(fill = mean_prod_ecoreg),
          alpha = 0.8) +
 scale_fill_gradientn(colours = rev(heat.colors(20)),
                       name = 'Mean annual \nproduction (tonnes)',
                       trans = 'log10') +
  facet_wrap(~ status, ncol = 1) +
  theme(legend.title.align = 0.5)

production_all_status_map

# save map---
ggsave(
  production_all_status_map,
  filename = "figures/production_all_status_map.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 20,
  height = 15
)

# production by status and species-----------
plot_production_status_species <-
  ggplot(prod_all_status, aes(fct_reorder(Species, mean_prod_ecoreg), mean_prod_ecoreg, fill = status)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    color = 1,
    position = position_dodge(width = .9)
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2
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
    filter(prod_all_status, mean_prod_ecoreg > 1e+4),
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
    position = position_dodge(width = .9)
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2
  ) +
  theme_bw() +
  coord_flip() +
  labs(y = 'Mean annual production (tonnes)', x = 'Ecoregion') +
  scale_fill_discrete() +
  facet_wrap(~ fct_rev(status), scales = 'free_y') +
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