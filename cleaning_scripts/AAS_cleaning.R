library(dplyr)
library(tidyverse)

#creating behavioral events dataset

YokeAAS <- YokeAAS_raw

event_cols <- names(YokeAAS)[5:132]

new_names <- unlist(lapply(1:32, function(i) {
  c(
    paste0("START_", i),
    paste0("END_", i),
    paste0("BEHAVIOR_", i),
    paste0("PARTNER_", i)
  )
}))

names(YokeAAS)[5:132] <- new_names

behavior_events <- YokeAAS %>%
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

#calculating duration
behavior_events <- behavior_events %>%
  mutate(
    start_seconds = floor(START / 100) * 60 + (START %% 100),
    end_seconds = floor(END / 100) * 60 + (END %% 100),
    duration_seconds = end_seconds - start_seconds
  )


behavior_events <- behavior_events %>%
  mutate(
    END_corrected = END,
    correction_note = NA_character_
  ) %>%
  mutate(
    # Correct likely missing zero in Selene's vigilance event
    END_corrected = if_else(
      focal_row == 27 & event == "4",
      200,
      END_corrected
    ),
    correction_note = if_else(
      focal_row == 27 & event == "4",
      "END corrected from 20 to 200; likely missing zero",
      correction_note
    )
  ) %>%
  mutate(
    # Recalculate end time and duration using corrected END
    end_seconds = if_else(
      !is.na(END_corrected),
      floor(END_corrected / 100) * 60 + (END_corrected %% 100),
      NA_real_
    ),
    duration_seconds = end_seconds - start_seconds,
    # Head throw-back recorded with identical START/END;
    # duration cannot be reliably determined
    duration_seconds = if_else(
      focal_row == 6 & event == "7",
      NA_real_,
      duration_seconds
    ),
    correction_note = if_else(
      focal_row == 6 & event == "7",
      "START and END both 415; duration set to NA rather than assuming duration",
      correction_note
    )
  )


#cleaning behavior events
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
      "Asteria" = "AST",
      "Ash" = "ASH",
      "Selene" = "SEL"
    )
  )

behavior_events <- behavior_events %>%
  mutate(
    partner_id = recode(
      PARTNER,
      "Asteria" = "AST",
      "Ash" = "ASH",
      "Selene" = "SEL"
    )
  )

saveRDS(behavior_events, "clean_data/AAS_behavior_events.rds")