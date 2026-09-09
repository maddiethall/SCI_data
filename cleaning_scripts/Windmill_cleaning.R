library(dplyr)
library(tidyverse)

windmill_clean <- Windmill_raw

event_cols <- names(windmill_clean)[5:132]

new_names <- unlist(lapply(1:32, function(i) {
  c(
    paste0("START_", i),
    paste0("END_", i),
    paste0("BEHAVIOR_", i),
    paste0("PARTNER_", i)
  )
}))

names(windmill_clean)[5:132] <- new_names

behavior_events <- windmill_clean %>%
  mutate(focal_row = row_number()) %>%
  pivot_longer(
    cols = matches("^(START|END|BEHAVIOR|PARTNER)_\\d+$"),
    names_to = c(".value", "event"),
    names_pattern = "^(START|END|BEHAVIOR|PARTNER)_(\\d+)$"
  ) %>%
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
      focal_row == 10 & event == "13" ~ 1148,
      focal_row == 47 & event == "4" ~ 230,
      focal_row == 144 & event == "1" ~ 31,
      TRUE ~ START_corrected
    ),
    END_corrected = case_when(
      focal_row == 10 & event == "13" ~ 1230,
      focal_row == 47 & event == "4" ~ 234,
      focal_row == 144 & event == "1" ~ 301,
      TRUE ~ END_corrected
    ),
    correction_note = case_when(
      focal_row == 10 & event == "13" ~ "START/END reversed; corrected 1230-1148 to 1148-1230",
      focal_row == 47 & event == "4" ~ "START/END reversed; corrected 234-230 to 230-234",
      focal_row == 144 & event == "1" ~ "START/END reversed; corrected 301-31 to 31-301",
      TRUE ~ correction_note
    )
  )
behavior_events <- behavior_events %>%
  mutate(
    start_seconds = floor(START_corrected / 100) * 60 +
      (START_corrected %% 100),
    end_seconds = floor(END_corrected / 100) * 60 +
      (END_corrected %% 100),
    duration_seconds = end_seconds - start_seconds
  )

behavior_events <- behavior_events %>%
  mutate(
    duration_seconds = if_else(
      focal_row == 124 & event == "10",
      NA_real_,
      duration_seconds
    ),
    correction_note = if_else(
      focal_row == 124 & event == "10",
      "START 1387 is invalid MMSS; duration set to NA because intended time cannot be determined",
      correction_note
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
      "Tracy Champan" = "TCH",
      "Chagall" = "CHA",
      "Autumn" = "AUT",
      "Polli" = "POL"
    )
  )

behavior_events <- behavior_events %>%
  mutate(
    partner_id = recode(
      PARTNER,
      "Tracy Chapman" = "TCH",
      "Chagall" = "CHA",
      "Autumn" = "AUT",
      "Polli" = "POL"
    )
  )

saveRDS(behavior_events, "clean_data/windmill_behavior_events.rds")
