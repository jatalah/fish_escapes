# Supplementary Figure 1. Mean annual production (tonnes) for the thirty most extensively farmed fish species in net-pen systems in Marine Ecoregions of the World by introduction status: native (top panel) and non-native (bottom panel).

# load libraries-----------
library(tidyverse)
library(sf)
library(rworldmap)
source('theme_javier.R')

# read data-------------
production_dat_long <- st_read("outputs/production_dat_long.geojson")
score_data_sf <-  st_read("outputs/score_data_sf.geojson")
std_scores <- read_csv('outputs/std_scores.csv')

# Base world maps ---------
world_less_is <- 
  getMap(resolution = "less islands") %>% 
  st_as_sf()

# Figure 1 Global production by status-----------
p <-
  ggplot(data = world_less_is) +
  geom_sf(fill = 'gray95') +
  theme( axis.text.x=element_blank(), axis.text.y=element_blank())


production_all_status_map <-
  p +
  geom_sf(data = production_dat_long,
          aes(fill = value),
          na.rm = T,
          alpha = 0.8)  +
  facet_wrap(~ key, ncol = 1) +
  scale_fill_gradientn(colours = rev(heat.colors(20)),
                       name = expression("Tonnes x"~10^3),
                       trans = 'log10',
                       labels = scales::comma) +
  theme_minimal(base_size = 12) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key.size =  unit(0.5, "cm"),
    legend.position = c(.1, .1),
    strip.text.x = element_text(size = 12)
  )

print(production_all_status_map)

ggsave(
  production_all_status_map,
  filename = "figures/production_all_status_map.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 300,
  units = 'cm',
  width = 15,
  height = 15
)