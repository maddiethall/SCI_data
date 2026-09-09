library(dplyr)
library(tidyverse)

yankee_raw = read.csv('/Users/maddiethall/R Repos/SCI_data/raw_data/YankeeBridge_raw.csv')


names(yankee_raw)
glimpse(yankee_raw)
nrow(yankee_raw)

#creating behavioral events dataset

behavior_cols <- yankee_raw %>%
  select(starts_with("CONTINUOUS.Behaviors"))

colSums(!is.na(behavior_cols) & behavior_cols != "")
sum(!is.na(unlist(behavior_cols)) & unlist(behavior_cols) != "")

behavior_events <- yankee_raw %>%
  mutate(focal_row = row_number()) %>%
  pivot_longer(
    cols = matches("^(START|END|CONTINUOUS\\.Behaviors|Social\\.Partner\\.s\\.)(\\.[0-9]+)?$"),
    names_to = c(".value", "event"),
    names_pattern = "^(START|END|CONTINUOUS\\.Behaviors|Social\\.Partner\\.s\\.)(?:\\.(\\d+))?$"
  )

#checking
dim(behavior_events)
head(behavior_events, 10)
sum(!is.na(behavior_events$CONTINUOUS.Behaviors) &
      behavior_events$CONTINUOUS.Behaviors != "")

#keep only behavioral events
behavior_events_clean <- behavior_events %>%
  filter(!is.na(CONTINUOUS.Behaviors),
         CONTINUOUS.Behaviors != "")
behavior_events_clean %>%
  select(
    focal_row,
    Date.Time,
    Focal,
    Weather,
    Temperature..F.,
    event,
    START,
    END,
    CONTINUOUS.Behaviors,
    Social.Partner.s.
  ) %>%
  head(15)


#cleaning event durations
behavior_events_clean <- behavior_events_clean %>%
  mutate(
    start_seconds = (START %/% 100) * 60 + (START %% 100),
    end_seconds = (END %/% 100) * 60 + (END %% 100),
    duration_seconds = end_seconds - start_seconds
  )
behavior_events_clean %>%
  select(
    Focal,
    event,
    START,
    END,
    start_seconds,
    end_seconds,
    duration_seconds,
    CONTINUOUS.Behaviors,
    Social.Partner.s.
  ) %>%
  head(15)
summary(behavior_events_clean$duration_seconds)



behavior_events_clean <- behavior_events_clean %>%
  separate(
    CONTINUOUS.Behaviors,
    into = c("behavior_category", "behavior"),
    sep = " - ",
    fill = "right",
    remove = FALSE
  )


behavior_events_clean <- behavior_events_clean %>%
  mutate(
    animal_id = case_when(
      Focal == "Marla" ~ "MAR",
      Focal == "May Lillie" ~ "MAY",
      Focal == "Delta Dawn" ~ "DDA",
      TRUE ~ NA_character_
    )
  )

behavior_events_clean <- behavior_events_clean %>%
  mutate(
    partner_id = case_when(
      Social.Partner.s. == "Marla" ~ "MAR",
      Social.Partner.s. == "May Lillie" ~ "MAY",
      Social.Partner.s. == "Delta Dawn" ~ "DDA",
      Social.Partner.s. == "" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

behavior_events_clean <- behavior_events_clean %>%
  select(-Task.Owner, -Referrer.Name, -Added.Time, -Notes)



yankee_behavior_events = yankee_clean %>%
  select(Date.Time,
         Focal,
         Weather,
         Temperature..F.,
         behavior_category,
         behavior,
         duration_seconds,
         animal_id,
         partner_id)

saveRDS(yankee_behavior_events, "clean_data/yankee_behavior_events.rds")
