prod_data_all %>%
  filter(Major_Group == "PISCES") %>%
  filter(YEAR > 2015) %>% 
  group_by(Scientific_Name) %>%
  summarise(Q = sum(QUANTITY)) %>%
  arrange(desc(Q)) %>%
  full_join(spp, by = "Scientific_Name")
