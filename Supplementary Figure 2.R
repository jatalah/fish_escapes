# load libraries-----------
library(tidyverse)
source('theme_javier.R')

# read data-------------
std_scores <- read_csv('outputs/std_scores.csv')

# Supplementary Figure 2 lollipop plot by province--------------------
province_lollipop <- 
  std_scores %>%
  group_by(PROVINCE, REALM) %>% 
  summarise_at(vars(Overall),mean) %>% 
  arrange(Overall) %>% 
  ungroup() %>% 
  mutate(PROVINCE = factor(PROVINCE, levels = unique(PROVINCE))) %>% 
  ggplot(aes(
    x = PROVINCE,
    y = Overall,
    label = round(Overall, 0),
    color = REALM
  )) +
  geom_segment(aes(
    x = PROVINCE,
    xend = PROVINCE,
    y = 0,
    yend = Overall
  )) +
  geom_point(size = 8) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  scale_color_brewer(palette = 'Spectral') +
  coord_flip() +
  labs(x = '', y = 'Overall risk score') +
  theme(legend.position = c(.7,.25)) +
  geom_text(color = 1, size = 3)

# Save plot -----
ggsave(
  province_lollipop,
  filename = "figures/province_lollipop.tiff",
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  units = 'cm',
  width = 20,
  height = 20
)