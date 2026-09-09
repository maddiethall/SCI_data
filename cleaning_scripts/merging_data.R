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


lemur_behavior <- lemur_behavior %>%
  filter(
    !(focal_id == "POL" & datetime == "2026-06-06 09:55:00"),
    !(focal_id == "CHA" & datetime == "2026-07-10 11:00:00")
  )


lemur_behavior <- lemur_behavior %>%
  mutate(
    troop = case_when(
      focal_id %in% c("ASH", "AST", "SEL") ~ "AAS",
      focal_id %in% c("DEL", "JJO", "LAU", "PHA") ~ "East Road",
      focal_id %in% c("ART", "CJA", "TLY") ~ "TLACJ",
      focal_id %in% c("AUT", "CHA", "POL", "TCH") ~ "Windmill",
      focal_id %in% c("DDA", "MAR", "MAY") ~ "Yankee"
    )
  )

#combining agonistic behaviors into one variable
lemur_behavior <- lemur_behavior %>%
  mutate(
    agonism = if_else(
      behavior %in% c(
        "Charge",
        "Chase",
        "Displace",
        "Gape",
        "Lunge",
        "Other"
      ),
      1,
      0
    )
  )


saveRDS(lemur_behavior, "clean_data/lemur_behavior.rds")

