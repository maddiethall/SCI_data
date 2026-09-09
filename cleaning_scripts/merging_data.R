library(dplyr)
library(tidyverse)

AAS = AAS_behavior_events
east_road = EastRoad_behavior_events
TLACJ = TLACJ_behavior_events
yankee = yankee_behavior_events
windmill = windmill_behavior_events


names(AAS)
names(TLACJ)
names(east_road)
names(yankee)
names(windmill)


AAS = AAS %>%
  filter(!(focal_row >= 80 & focal_row <= 91))



AAS_merge <- AAS %>%
  mutate(
    troop = "AAS",
    datetime = as.POSIXct(
      `Date-Time`,
      format = "%m/%d/%Y %I:%M %p"
    ),
    temperature = `Temperature (F)`
  ) %>%
  select(
    troop,
    focal_id,
    partner_id,
    datetime,
    Weather,
    temperature,
    behavior_category,
    behavior,
    duration_seconds
  )
east_road_merge <- east_road %>%
  mutate(
    troop = "East Road",
    datetime = as.POSIXct(
      `Date-Time`,
      format = "%m/%d/%Y %I:%M %p"
    ),
    temperature = `Temperature (F)`
  ) %>%
  select(
    troop,
    focal_id,
    partner_id,
    datetime,
    Weather,
    temperature,
    behavior_category,
    behavior,
    duration_seconds
  )
windmill_merge <- windmill %>%
  mutate(
    troop = "Windmill",
    datetime = as.POSIXct(
      `Date-Time`,
      format = "%m/%d/%Y %I:%M %p"
    ),
    temperature = `Temperature (F)`
  ) %>%
  select(
    troop,
    focal_id,
    partner_id,
    datetime,
    Weather,
    temperature,
    behavior_category,
    behavior,
    duration_seconds
  )
TLACJ_merge <- TLACJ %>%
  mutate(
    troop = "TLACJ",
    datetime = as.POSIXct(
      Date.Time,
      format = "%m/%d/%Y %I:%M %p"
    ),
    temperature = `Temperature..F.`,
    focal_id = animal_id
  ) %>%
  select(
    troop,
    focal_id,
    partner_id,
    datetime,
    Weather,
    temperature,
    behavior_category,
    behavior,
    duration_seconds
  )
yankee_merge <- yankee %>%
  mutate(
    troop = "Yankee",
    datetime = as.POSIXct(
      Date.Time,
      format = "%m/%d/%Y %I:%M %p"
    ),
    temperature = `Temperature..F.`,
    focal_id = animal_id
  ) %>%
  select(
    troop,
    focal_id,
    partner_id,
    datetime,
    Weather,
    temperature,
    behavior_category,
    behavior,
    duration_seconds
  )



lemur_behavior <- bind_rows(
  AAS_merge,
  east_road_merge,
  windmill_merge,
  TLACJ_merge,
  yankee_merge
)
