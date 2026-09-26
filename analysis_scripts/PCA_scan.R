library(dplyr)
library(tidyverse)

pca_scan_data = lemur_scan %>%
  select(activity, focal_id, troop)

pca_scan = pca_scan_data %>%
  group_by(focal_id, troop, activity) %>%
  summarise(
    n_events = n(),
    .groups = "drop"
  )

pca_scan = pca_scan %>%
  group_by(focal_id) %>%
  mutate(
    total_events = sum(n_events)
  )

pca_scan = pca_scan %>%
  group_by(focal_id, activity) %>%
  mutate(
    event_proportion = n_events/total_events
  )

pca_scan_wide = pca_scan %>%
  select(focal_id, troop, activity, event_proportion) %>%
  pivot_wider(
    names_from = activity,
    values_from = event_proportion,
    values_fill = 0
  )

saveRDS(pca_scan_wide, "clean_data/pca_scan_data.rds")


##### combining scan and continuous

pca_all_data  = pca_scan_wide %>%
  left_join(
    pca_data %>%
      select_all(),
    by = "focal_id"
  )

pca_all_data = pca_all_data %>%
  select(-troop.y) %>%
  rename(troop = troop.x)

pca_scan_model <- prcomp(
  pca_all_data %>%
    select(Forage, Locomotion, Rest, Social, Vigilance, `Self-groom`, `Self-scratch`),
  scale. = TRUE
)
