library(dplyr)
library(tidyverse)

#creating behavioral events dataset

east_road <- EastRoad_raw

event_cols <- names(east_road)[5:132]

new_names <- unlist(lapply(1:32, function(i) {
  c(
    paste0("START_", i),
    paste0("END_", i),
    paste0("BEHAVIOR_", i),
    paste0("PARTNER_", i)
  )
}))


names(east_road)[5:132] <- new_names

behavior_events <- east_road %>%
  mutate(focal_row = row_number()) %>%
  pivot_longer(
    cols = matches("^(START|END|BEHAVIOR|PARTNER)_\\d+$"),
    names_to = c(".value", "event"),
    names_pattern = "^(START|END|BEHAVIOR|PARTNER)_(\\d+)$"
  )

behavior_events <- behavior_events %>%
  select(
    focal_row,
    `Date-Time`,
    Focal,
    Weather,
    `Temperature (F)`,
    event,
    START,
    END,
    BEHAVIOR,
    PARTNER
  )

behavior_events <- behavior_events %>%
  filter(!is.na(BEHAVIOR), BEHAVIOR != "")


behavior_events <- behavior_events %>%
  mutate(
    start_seconds = floor(START / 100) * 60 + (START %% 100),
    end_seconds = floor(END / 100) * 60 + (END %% 100),
    duration_seconds = end_seconds - start_seconds
  )


behavior_events <- behavior_events %>%
  mutate(
    START_corrected = START,
    END_corrected = END,
    correction_note = NA_character_
  ) %>%
  mutate(
    START_corrected = case_when(
      focal_row == 109 & event == "2" ~ 248,
      TRUE ~ START_corrected
    ),
    END_corrected = case_when(
      focal_row == 109 & event == "2" ~ 408,
      TRUE ~ END_corrected
    ),
    correction_note = case_when(
      focal_row == 109 & event == "2" ~
        "START/END reversed; corrected 408-248 to 248-408",
      focal_row == 112 & event == "12" ~
        "START and END both 1001; duration set to NA because intended duration cannot be determined",
      TRUE ~ correction_note
    )
  ) %>%
  mutate(
    start_seconds = floor(START_corrected / 100) * 60 +
      (START_corrected %% 100),
    end_seconds = floor(END_corrected / 100) * 60 +
      (END_corrected %% 100),
    duration_seconds = end_seconds - start_seconds
  ) %>%
  mutate(
    duration_seconds = if_else(
      focal_row == 112 & event == "12",
      NA_real_,
      duration_seconds
    )
  )

behavior_events <- behavior_events %>%
  separate(
    BEHAVIOR,
    into = c("behavior_category", "behavior"),
    sep = " - ",
    fill = "right",
    remove = FALSE
  )

behavior_events <- behavior_events %>%
  mutate(
    focal_id = recode(
      Focal,
      "Janis Joplin" = "JJO",
      "Pearl Hart" = "PHA",
      "Delilah" = "DEL",
      "Laurel" = "LAU"
    )
  )

behavior_events <- behavior_events %>%
  mutate(
    partner_id = recode(
      PARTNER,
      "Janis Joplin" = "JJO",
      "Pearl Hart" = "PHA",
      "Delilah" = "DEL",
      "Laurel" = "LAU",
      "Charlie Brown" = "CBR"
    )
  )

saveRDS(behavior_events, "clean_data/EastRoad_behavior_events.rds")
