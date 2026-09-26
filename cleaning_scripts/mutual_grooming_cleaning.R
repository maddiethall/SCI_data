library(dplyr)
library(tidyverse)

# isolate mutual grooming
mutual_grooming <- lemur_behavior %>%
  filter(
    behavior == "Mutual grooming",
    !is.na(partner_id),
    focal_id != partner_id,
    !grepl("Adjacent group", partner_id)
  )

# denominators for grooming rates
focal_observation_time <- lemur_scan %>%
  distinct(troop, focal_id, `Date-Time`) %>%
  group_by(troop, focal_id) %>%
  summarise(
    focal_samples = n(),
    observation_minutes = focal_samples * 15,
    .groups = "drop"
  )

# calculate duration and number of events
mutual_grooming_summary <- mutual_grooming %>%
  group_by(troop, focal_id, partner_id) %>%
  summarise(
    events = n(),
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    focal_observation_time %>%
      select(troop, focal_id, observation_minutes),
    by = c("troop", "focal_id")
  ) %>%
  mutate(
    duration_per_hour =
      total_duration / observation_minutes * 60,
    event_rate_per_hour =
      events / observation_minutes * 60
  )

# dyadic mutual-grooming measure
### (total mutual groom sec across both focals / total obs time across both focals) × 60

mutual_grooming_dyads <- mutual_grooming_summary %>%
  mutate(
    individual_1 = pmin(focal_id, partner_id),
    individual_2 = pmax(focal_id, partner_id)
  ) %>%
  group_by(troop, individual_1, individual_2) %>%
  summarise(
    total_mutual_duration = sum(total_duration),
    total_mutual_events = sum(events),
    total_observation_minutes = sum(observation_minutes),
    duration_per_hour =
      total_mutual_duration / total_observation_minutes * 60,
    event_rate_per_hour =
      total_mutual_events / total_observation_minutes * 60,
    .groups = "drop"
  )

saveRDS(mutual_grooming_dyads, "clean_data/mutual_grooming.rds")
