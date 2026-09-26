library(dplyr)
library(tidyverse)

# Combine directional grooming + reciprocity + mutual grooming

## combine reciprocity and mutual grooming
social_grooming <- reciprocity %>%
  left_join(
    mutual_grooming_dyads %>%
      select(
        troop,
        individual_1,
        individual_2,
        total_mutual_duration,
        total_mutual_events,
        total_observation_minutes,
        duration_per_hour,
        event_rate_per_hour
      ) %>%
      rename(
        mutual_grooming_duration = total_mutual_duration,
        mutual_grooming_events = total_mutual_events,
        mutual_grooming_duration_per_hour = duration_per_hour,
        mutual_grooming_event_rate_per_hour = event_rate_per_hour
      ),
    by = c("troop", "individual_1", "individual_2")
  )

## add proximity
dyadic_social_relationships <- social_grooming %>%
  left_join(
    proximity_dyads_wide,
    by = c("troop", "individual_1", "individual_2")
  )

### renaming variables
dyadic_social_relationships <- dyadic_social_relationships %>%
  rename(
    grooming_duration_1_to_2_per_hour = grooming_1_to_2,
    grooming_duration_2_to_1_per_hour = grooming_2_to_1,
    grooming_reciprocity = reciprocity,
    social_observation_minutes = total_observation_minutes
  ) %>%
  select(
    troop,
    individual_1,
    individual_2,
    grooming_duration_1_to_2_per_hour,
    grooming_duration_2_to_1_per_hour,
    grooming_reciprocity,
    grooming_asymmetry,
    mutual_grooming_duration,
    mutual_grooming_events,
    mutual_grooming_duration_per_hour,
    social_observation_minutes,
    proximity_2_5m,
    proximity_body_contact,
    proximity_less_2m,
    proximity_over_5m
  )

#### add dyad metadata
dyadic_social_relationships <- dyadic_social_relationships %>%
  mutate(
    Dyad = paste(
      pmin(individual_1, individual_2),
      pmax(individual_1, individual_2),
      sep = "_"
    )
  )

dyad_metadata <- dyad_metadata %>%
  separate(
    Dyad,
    into = c("individual_1", "individual_2"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    Dyad = paste(
      pmin(individual_1, individual_2),
      pmax(individual_1, individual_2),
      sep = "_"
    )
  )
dyadic_social_relationships <- dyadic_social_relationships %>%
  left_join(
    dyad_metadata %>%
      select(Dyad, Relationship, Degree, Related_coeff),
    by = "Dyad"
  )

saveRDS(dyadic_social_relationships, "clean_data/dyadic_data_complete.rds")
