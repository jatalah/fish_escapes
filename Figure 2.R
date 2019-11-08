# Figure 1 Invasive, genetic and pathogenic risk scores associated with the impacts of escapes of the thirsty most extensively farmed marine fish species in net-pen systems

# load libraries-----------
library(tidyverse)
library(sf)
library(rworldmap)
source('theme_javier.R')
setwd("C:/Users/javiera/OneDrive - Cawthron/fish_escapes/outputs/")

# read data-------------
score_data_sf <-  st_read("score_data_sf.geojson")

# Get base world maps ---------
world_less_is <- 
  getMap(resolution = "less islands") %>% 
  st_as_sf()

# get data in long format for facet plot----------------------
long_score_data_sf <-
  score_data_sf %>%
  gather(key, value, c("Genetic", "Invasive", "Diseases")) %>%
  mutate(key = fct_rev(
    fct_recode(
      key,
      "B. Genetic" = "Genetic",
      "A. Invasive" = "Invasive",
      "C. Pathogenic" = "Diseases"
    )
  ))

score_plots <-
  ggplot(data = world_less_is) +
  geom_sf(fill = 'gray90') +
  geom_sf(data = long_score_data_sf, aes(fill = value), na.rm = T) +
  scale_fill_gradientn(colours = rev(heat.colors(100)), name = 'Risk score') +
  facet_wrap(~ key, ncol = 1) +
  coord_sf(ylim = c(-55, 90),  xlim = c(-160, 170)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.key.size =  unit(0.5, "cm"),
    legend.title=element_text(size=12),
    # legend.title.align = 0.5,
    legend.position = c(.05, .08)
  )

ggsave(
  score_plots,
  filename = "figures/scores_maps.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 300,
  units = 'cm',
  width = 15,
  height = 21
)