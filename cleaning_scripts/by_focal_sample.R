library(dplyr)
library(tidyverse)

by_focal = lemur_behavior %>%
  select(troop, focal_id, datetime, Weather, temperature, behavior, duration_seconds)

by_focal_summary = by_focal %>%
  group_by(datetime, focal_id, troop, behavior) %>%
  summarise(
    duration = sum(duration_seconds),
    .groups = "drop")

by_focal_summary = by_focal_summary %>%
  mutate(
    proportion_time = duration/900
  )

focal_observation_time <- lemur_scan %>%
  distinct(troop, focal_id, `Date-Time`) %>%
  group_by(troop, focal_id) %>%
  summarise(
    focal_samples = n(),
    observation_minutes = focal_samples * 15,
    .groups = "drop"
  )

behavior_by_focal <- lemur_behavior %>%
  group_by(datetime, focal_id, troop, behavior, Weather, temperature) %>%
  summarise(
    n_events = n(),
    duration_per_focal = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proportion_per_focal = duration_per_focal/900,
    rate_per_minute = n_events / 15
  )


behavior_by_focal = behavior_by_focal %>%
  left_join(
    focal_observation_time,
    by = c("troop", "focal_id")
  )


behavior_by_focal_summary = behavior_by_focal %>%
  group_by(focal_id, behavior, troop) %>%
  summarise(
    avg_proportion = mean(proportion_per_focal),
    avg_rate = mean(rate_per_minute),
    .groups = "drop"
  )
  





################################## SCAN DATA

scan_by_focal = lemur_scan %>%
  group_by(`Date-Time`, activity, focal_id, troop) %>%
  summarise(
    n_events = n(),
    .groups = "drop"
  )

scan_by_focal = scan_by_focal%>%
  mutate(
    corrected_n = case_when(
      troop == "AAS" ~ n_events/2,
      troop == "TLACJ" ~ n_events/2,
      troop == "Yankee" ~ n_events/2,
      troop == "Windmill" ~ n_events/3,
      troop == "East Road" ~ n_events/4
    )
  )

scan_by_focal = scan_by_focal %>%
  mutate(
    proportion_of_focal = corrected_n/8
  )


scan_by_focal = scan_by_focal %>%
  group_by(focal_id) %>%
  mutate(
    n_focals = n_distinct(`Date-Time`)) %>%
  ungroup()

scan_by_focal_summary = scan_by_focal %>%
  group_by(focal_id, activity, troop, n_focals) %>%
  summarise(
    total_proportion = sum(proportion_of_focal),
    .groups = "drop"
  )

scan_by_focal_summary = scan_by_focal_summary %>%
  group_by(focal_id, activity) %>%
  mutate(
    avg_proportion = total_proportion/n_focals
  )

  