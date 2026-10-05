library(tidyverse)


fecal_behavior <- fecal_clean %>%
  left_join(
    focal_metadata %>%
      select(Animal, Animal_id),
    by = c("animal_id" = "Animal")
  )
fecal_behavior <- fecal_behavior %>%
  mutate(
    fecal_datetime = date + lubridate::seconds(time * 3600)
  )

fecal_windows <- fecal_behavior %>%
  mutate(
    fecal_sample_id = row_number(),
    window_start = fecal_datetime - lubridate::hours(48)
  ) %>%
  select(
    fecal_sample_id,
    Animal_id,
    fecal_datetime,
    window_start
  )

focal_samples <- lemur_scan %>%
  distinct(
    troop,
    focal_id,
    datetime = `Date-Time`
  )
focal_samples <- focal_samples %>%
  mutate(
    datetime = as.POSIXct(
      datetime,
      format = "%m/%d/%Y %I:%M %p",
      tz = "UTC"
    )
  )

fecal_focal_matches <- fecal_windows %>%
  inner_join(
    focal_samples,
    by = join_by(
      Animal_id == focal_id,
      window_start <= datetime,
      fecal_datetime >= datetime
    )
  )

fecal_windows_3d <- fecal_behavior %>%
  mutate(
    fecal_sample_id = row_number(),
    window_start = fecal_datetime - lubridate::days(3)
  ) %>%
  select(
    fecal_sample_id,
    Animal_id,
    fecal_datetime,
    window_start
  )
fecal_focal_matches_3d <- fecal_windows_3d %>%
  inner_join(
    focal_samples,
    by = join_by(
      Animal_id == focal_id,
      window_start <= datetime,
      fecal_datetime >= datetime
    )
  )
focal_coverage_3d <- fecal_windows_3d %>%
  select(fecal_sample_id) %>%
  left_join(
    fecal_focal_matches_3d %>%
      count(fecal_sample_id, name = "n_focals"),
    by = "fecal_sample_id"
  ) %>%
  mutate(
    n_focals = tidyr::replace_na(n_focals, 0L)
  )

##

fecal_windows_5d <- fecal_behavior %>%
  mutate(
    fecal_sample_id = row_number(),
    window_start = fecal_datetime - lubridate::days(5)
  ) %>%
  select(
    fecal_sample_id,
    Animal_id,
    fecal_datetime,
    window_start
  )
fecal_focal_matches_5d <- fecal_windows_5d %>%
  inner_join(
    focal_samples,
    by = join_by(
      Animal_id == focal_id,
      window_start <= datetime,
      fecal_datetime >= datetime
    )
  )
focal_coverage_5d <- fecal_windows_5d %>%
  select(fecal_sample_id) %>%
  left_join(
    fecal_focal_matches_5d %>%
      count(fecal_sample_id, name = "n_focals"),
    by = "fecal_sample_id"
  ) %>%
  mutate(
    n_focals = tidyr::replace_na(n_focals, 0L)
  )
focal_coverage_5d %>%
  count(n_focals)

##

fecal_windows_7d <- fecal_behavior %>%
  mutate(
    fecal_sample_id = row_number(),
    window_start = fecal_datetime - lubridate::days(7)
  ) %>%
  select(
    fecal_sample_id,
    Animal_id,
    fecal_datetime,
    window_start
  )
fecal_focal_matches_7d <- fecal_windows_7d %>%
  inner_join(
    focal_samples,
    by = join_by(
      Animal_id == focal_id,
      window_start <= datetime,
      fecal_datetime >= datetime
    )
  )
focal_coverage_7d <- fecal_windows_7d %>%
  select(fecal_sample_id) %>%
  left_join(
    fecal_focal_matches_7d %>%
      count(fecal_sample_id, name = "n_focals"),
    by = "fecal_sample_id"
  ) %>%
  mutate(
    n_focals = tidyr::replace_na(n_focals, 0L)
  )
focal_coverage_7d %>%
  count(n_focals)

############ 7-day behavioral stress dataset

# just self-scratch
scratch_by_focal <- behavior_by_focal_complete %>%
  filter(behavior == "Self-scratch") %>%
  select(
    focal_id,
    datetime,
    scratch_count = n_events
  )
