library(dplyr)


# inspecting grooming data structure
lemur_behavior %>%
  filter(behavior %in% c("Give grooming", "Receive grooming", "Mutual grooming")) %>%
  count(behavior, focal_id, partner_id, sort = TRUE)

# inspecting proximity data structure
lemur_scan %>%
  count(troop, focal_id, partner_id, proximity, sort = TRUE)
lemur_scan %>%
  count(troop, focal_id, partner_id) %>%
  arrange(troop, focal_id, desc(n))
lemur_scan %>%
  count(focal_id, proximity) %>%
  arrange(focal_id, proximity)


############# CREATE GROOMING DATASET

lemur_behavior %>%
  filter(
    behavior %in% c(
      "Give grooming",
      "Receive grooming",
      "Mutual grooming"
    )
  ) %>%
  group_by(troop, focal_id, partner_id, behavior) %>%
  summarise(
    events = n(),
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(troop, focal_id, partner_id, behavior)

grooming_data <- lemur_behavior %>%
  filter(
    behavior %in% c(
      "Give grooming",
      "Receive grooming",
      "Mutual grooming"
    ),
    !grepl("Adjacent group", partner_id)
  )

grooming_focal <- grooming_data %>%
  filter(
    !is.na(partner_id),
    focal_id != partner_id
  ) #removing NA

grooming_summary <- grooming_focal %>%
  group_by(troop, focal_id, partner_id, behavior) %>%
  summarise(
    events = n(),
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
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

# calculating directed grooming rates
grooming_summary <- grooming_summary %>%
  left_join(
    focal_observation_time %>%
      select(troop, focal_id, observation_minutes),
    by = c("troop", "focal_id")
  ) %>%
  mutate(
    event_rate_per_hour = events / observation_minutes * 60,
    duration_per_hour = total_duration / observation_minutes * 60
  )

# identify dyads with missing grooming info
grooming_summary %>%
  filter(behavior == "Mutual grooming") %>%
  select(troop, focal_id, partner_id) %>%
  anti_join(
    grooming_summary %>%
      filter(behavior == "Give grooming") %>%
      select(troop, focal_id, partner_id),
    by = c("troop", "focal_id", "partner_id")
  ) 
grooming_summary %>%
  filter(behavior == "Mutual grooming") %>%
  select(troop, focal_id, partner_id) %>%
  anti_join(
    grooming_summary %>%
      filter(behavior == "Receive grooming") %>%
      select(troop, focal_id, partner_id),
    by = c("troop", "focal_id", "partner_id")
  )

dyads <- lemur_scan %>%
  filter(
    !is.na(partner_id),
    focal_id != partner_id
  ) %>%
  distinct(troop, focal_id, partner_id)

grooming_complete <- dyads %>%
  tidyr::crossing(
    behavior = c(
      "Give grooming",
      "Receive grooming",
      "Mutual grooming"
    )
  ) %>%
  left_join(
    grooming_summary,
    by = c("troop", "focal_id", "partner_id", "behavior")
  ) %>%
  mutate(
    events = tidyr::replace_na(events, 0),
    total_duration = tidyr::replace_na(total_duration, 0),
    event_rate_per_hour = tidyr::replace_na(event_rate_per_hour, 0),
    duration_per_hour = tidyr::replace_na(duration_per_hour, 0)
  )
grooming_complete <- grooming_complete %>%
  left_join(
    focal_observation_time %>%
      select(troop, focal_id, observation_minutes),
    by = c("troop", "focal_id"),
    suffix = c("", "_new")
  ) %>%
  mutate(
    observation_minutes = coalesce(
      observation_minutes,
      observation_minutes_new
    )
  ) %>%
  select(-observation_minutes_new)
