library(dplyr)
library(tidyverse)
library(psych)
library(lme4)
library(rptR)
library(glmmTMB)

# Initial focal-level aggregation
# NOTE: only includes focal x behavior combinations where behavior occurred
# Revised below to add true zeros for focal samples where behavior did not occur

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
  
# Revised aggregation
## partner-weighted contact duration per focal, rather than literal proportion of focal time. 
## A value >1 means simultaneous contact with multiple partners contributed overlapping partner-time

focal_samples <- lemur_scan %>%
  distinct(
    troop,
    focal_id,
    datetime = `Date-Time`
  )

behavior_observed_new <- lemur_behavior %>%
  group_by(
    troop,
    focal_id,
    datetime,
    behavior
  ) %>%
  summarise(
    n_events = n(),
    duration_per_focal = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  )
behavior_list <- lemur_behavior %>%
  distinct(behavior)
behavior_by_focal_new <- focal_samples %>%
  tidyr::crossing(behavior_list)

behavior_by_focal_new <- behavior_by_focal_new %>%
  left_join(
    behavior_observed_new,
    by = c("troop", "focal_id", "datetime", "behavior")
  )

focal_samples <- focal_samples %>%
  mutate(
    datetime = lubridate::force_tz(
      datetime,
      tzone = "America/New_York"
    )
  )
focal_samples <- focal_samples %>%
  mutate(
    datetime_key = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

lemur_behavior <- lemur_behavior %>%
  mutate(
    datetime_key = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

behavior_observed_new <- lemur_behavior %>%
  group_by(
    troop,
    focal_id,
    datetime_key,
    behavior
  ) %>%
  summarise(
    n_events = n(),
    duration_per_focal = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  )
behavior_by_focal_new <- focal_samples %>%
  select(troop, focal_id, datetime, datetime_key) %>%
  tidyr::crossing(behavior_list)

behavior_by_focal_new <- behavior_by_focal_new %>%
  left_join(
    behavior_observed_new,
    by = c("troop", "focal_id", "datetime_key", "behavior")
  )
behavior_by_focal_new <- behavior_by_focal_new %>%
  mutate(
    n_events = tidyr::replace_na(n_events, 0L),
    duration_per_focal = tidyr::replace_na(duration_per_focal, 0),
    proportion_per_focal = duration_per_focal / 900,
    rate_per_minute = n_events / 15
  )

behavior_by_focal_summary <-behavior_by_focal_new %>%
  group_by(focal_id, behavior, troop) %>%
  summarise(
    avg_proportion = mean(proportion_per_focal),
    avg_rate = mean(rate_per_minute),
    .groups = "drop"
  )
saveRDS(behavior_by_focal_summary, "clean_data/behavior_by_focal_summary.rds")
saveRDS(behavior_by_focal_new, "clean_data/behavior_by_focal_complete.rds")


################################## SCAN DATA


# Initial attempt... revised below
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

# Revised aggregation
scan_activity <- lemur_scan %>%
  distinct(
    troop,
    focal_id,
    `Date-Time`,
    scan,
    activity
  )

scan_activity_by_focal <- scan_activity %>%
  filter(!is.na(activity)) %>%
  group_by(troop, focal_id, `Date-Time`, activity) %>%
  summarise(
    n_scans = n(),
    .groups = "drop"
  )

all_activities <- scan_activity %>%
  filter(!is.na(activity)) %>%
  distinct(activity)

scan_activity_complete <- focal_samples %>%
  tidyr::crossing(all_activities)

scan_activity_by_focal <- scan_activity_by_focal %>%
  rename(datetime = `Date-Time`) %>%
  mutate(
    datetime = as.POSIXct(
      datetime,
      format = "%m/%d/%Y %I:%M %p"
    )
  )

scan_activity_complete <- scan_activity_complete %>%
  mutate(
    datetime = as.POSIXct(
      datetime,
      format = "%m/%d/%Y %I:%M %p"
    )
  )

scan_activity_complete <- scan_activity_complete %>%
  left_join(
    scan_activity_by_focal,
    by = c("troop", "focal_id", "datetime", "activity")
  ) %>%
  mutate(
    n_scans = tidyr::replace_na(n_scans, 0L)
  )

valid_scans_per_focal <- scan_activity %>%
  group_by(troop, focal_id, `Date-Time`) %>%
  summarise(
    n_valid_scans = sum(!is.na(activity)),
    .groups = "drop"
  )

valid_scans_per_focal <- valid_scans_per_focal %>%
  rename(datetime = `Date-Time`) %>%
  mutate(
    datetime = as.POSIXct(
      datetime,
      format = "%m/%d/%Y %I:%M %p"
    )
  )

scan_activity_complete <- scan_activity_complete %>%
  left_join(
    valid_scans_per_focal,
    by = c("troop", "focal_id", "datetime")
  )

scan_activity_complete <- scan_activity_complete %>%
  mutate(
    proportion_per_focal = n_scans / n_valid_scans
  )

# average the focal-level proportions within each individual
scan_activity_summary <- scan_activity_complete %>%
  group_by(focal_id, troop, activity) %>%
  summarise(
    avg_proportion = mean(proportion_per_focal),
    .groups = "drop"
  )
saveRDS(scan_activity_summary, "clean_data/scan_activity_summary.rds")



