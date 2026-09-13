library(dplyr)
library(tidyverse)


# inspecting grooming data structure
lemur_behavior %>%
  filter(behavior %in% c("Give grooming", "Receive grooming", "Mutual grooming")) %>%
  count(behavior, focal_id, partner_id, sort = TRUE)


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

# relationship between event rate and duration rate
grooming_complete %>%
  select(
    behavior,
    event_rate_per_hour,
    duration_per_hour
  ) %>%
  group_by(behavior) %>%
  summarise(
    correlation = cor(
      event_rate_per_hour,
      duration_per_hour
    )
  )
# Give: r = .861; Mutual: r = .909; Receive: r = .788
# use duration/hour as the main relationship-strength measure


################### CREATE DIRECTIONAL GROOMING DATA

directional_grooming <- grooming_complete %>%
  group_by(troop, focal_id, partner_id) %>%
  summarise(
    observation_minutes = first(observation_minutes),
    
    give_duration = sum(
      total_duration[behavior == "Give grooming"],
      na.rm = TRUE
    ),
    
    receive_duration = sum(
      total_duration[behavior == "Receive grooming"],
      na.rm = TRUE
    ),
    
    give_events = sum(
      events[behavior == "Give grooming"],
      na.rm = TRUE
    ),
    
    receive_events = sum(
      events[behavior == "Receive grooming"],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# combine focal perspectives
# create a copy of the data from the recipient's perspective, then join the two perspectives together
directional_grooming <- directional_grooming %>%
  left_join(
    directional_grooming %>%
      select(
        troop,
        focal_id,
        partner_id,
        observation_minutes,
        give_duration,
        receive_duration,
        give_events,
        receive_events
      ) %>%
      rename(
        recipient = focal_id,
        groomer = partner_id,
        recipient_observation_minutes = observation_minutes,
        recipient_receive_duration = receive_duration,
        recipient_receive_events = receive_events
      ),
    by = c(
      "troop",
      "focal_id" = "groomer",
      "partner_id" = "recipient"
    )
  )
# clean dataset
directional_grooming_clean <- directional_grooming %>%
  transmute(
    troop,
    groomer = focal_id,
    recipient = partner_id,
    observation_minutes_groomer = observation_minutes,
    observation_minutes_recipient = recipient_observation_minutes,
    groomer_give_duration = give_duration.x,
    recipient_receive_duration = recipient_receive_duration,
    groomer_give_events = give_events.x,
    recipient_receive_events = recipient_receive_events
  )
# fixing CBR NAs 
directional_grooming_clean <- directional_grooming_clean %>%
  mutate(
    observation_minutes_groomer = replace_na(
      observation_minutes_groomer, 0
    ),
    observation_minutes_recipient = replace_na(
      observation_minutes_recipient, 0
    )
  )
directional_grooming_clean <- directional_grooming_clean %>%
  mutate(
    groomer_give_duration = replace_na(
      groomer_give_duration, 0
    ),
    recipient_receive_duration = replace_na(
      recipient_receive_duration, 0
    ),
    groomer_give_events = replace_na(
      groomer_give_events, 0
    ),
    recipient_receive_events = replace_na(
      recipient_receive_events, 0
    )
  )

# Calculate the pooled directional grooming rate
#### grooming directed from A to B:
## (focal_A GIVE + focal_B RECEIVE)/(focal_A obs time + focal_B obs time) x 60
#### grooming directed from B to A:
## (focal_B GIVE + focal_A RECEIVE)/(focal_B obs time + focal_A obs time) x 60
#### mutual grooming is a separate measure

directional_grooming_clean <- directional_grooming_clean %>%
  mutate(
    total_directional_duration =
      groomer_give_duration + recipient_receive_duration,
    
    total_observation_minutes =
      observation_minutes_groomer + observation_minutes_recipient,
    
    duration_per_hour =
      total_directional_duration /
      total_observation_minutes * 60,
    
    total_directional_events =
      groomer_give_events + recipient_receive_events,
    
    event_rate_per_hour =
      total_directional_events /
      total_observation_minutes * 60
  )


## correcting CBR rows
cbr_reverse <- directional_grooming %>%
  filter(partner_id == "CBR") %>%
  transmute(
    troop,
    groomer = "CBR",
    recipient = focal_id,
    observation_minutes = observation_minutes,
    total_directional_duration = receive_duration,
    total_directional_events = receive_events,
    duration_per_hour = receive_duration / observation_minutes * 60,
    event_rate_per_hour = receive_events / observation_minutes * 60
  )
female_to_cbr <- directional_grooming_clean %>%
  filter(recipient == "CBR") %>%
  transmute(
    troop,
    groomer,
    recipient,
    observation_minutes = observation_minutes_groomer,
    total_directional_duration,
    total_directional_events,
    duration_per_hour,
    event_rate_per_hour
  )
cbr_directional <- bind_rows(
  female_to_cbr,
  cbr_reverse
)

# add corrected rows to dataset
directional_grooming_complete <- directional_grooming_clean %>%
  filter(recipient != "CBR") %>%
  select(
    troop,
    groomer,
    recipient,
    observation_minutes = observation_minutes_groomer,
    total_directional_duration,
    total_directional_events,
    duration_per_hour,
    event_rate_per_hour
  ) %>%
  bind_rows(cbr_directional)

saveRDS(directional_grooming_complete, "clean_data/directional_grooming.rds")
