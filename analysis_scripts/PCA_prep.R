library(dplyr)
library(ggplot2)

lemur_behavior %>%
  count(focal_id, behavior) %>%
  pivot_wider(
    names_from = behavior,
    values_from = n,
    values_fill = 0
  ) %>%
  print(n=Inf)

lemur_behavior %>%
  filter(behavior_category == "Agonistic") %>%
  count(focal_id) %>%
  arrange(desc(n))
#potentially useful as a composite "agonism" category

lemur_behavior %>%
  filter(behavior_category == "Submission") %>%
  count(focal_id) %>%
  arrange(desc(n))
#not enough submissive behavior to include in PCA



behavior_counts <- lemur_behavior %>%
  count(troop, focal_id, behavior) %>%
  pivot_wider(
    names_from = behavior,
    values_from = n,
    values_fill = 0
  )
behavior_counts %>%
  select(-troop, -focal_id) %>%
  summarise(
    across(
      everything(),
      ~ sum(.x > 0)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "behavior",
    values_to = "n_individuals"
  ) %>%
  arrange(desc(n_individuals)) %>%
  print(n=Inf)

#present in all lemurs: contact sit, all grooming, self-groom, scratch, vig, yawn
#possible candidates: greet (14/17), displace (10/17), be displaced (8/17), head throw-back (6/17)

behavior_counts %>%
  select(-troop, -focal_id) %>%
  summarise(
    across(
      everything(),
      list(
        mean = mean,
        sd = sd,
        min = min,
        max = max
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("behavior", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  print(n=Inf)


#comparing duration and frequency
lemur_behavior %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Self-scratch",
      "Vigilance",
      "Yawn"
    )
  ) %>%
  group_by(focal_id, behavior) %>%
  summarise(
    n_events = n(),
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(behavior) %>%
  summarise(
    correlation = cor(n_events, total_duration),
    .groups = "drop"
  ) %>%
  print(n=Inf)


focal_observation_time <- lemur_scan %>%
  distinct(troop, focal_id, `Date-Time`) %>%
  group_by(troop, focal_id) %>%
  summarise(
    focal_samples = n(),
    observation_minutes = focal_samples * 15,
    .groups = "drop"
  )


behavior_proportions <- lemur_behavior %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Self-scratch",
      "Vigilance",
      "Yawn"
    )
  ) %>%
  group_by(troop, focal_id, behavior) %>%
  summarise(
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    focal_observation_time,
    by = c("troop", "focal_id")
  ) %>%
  mutate(
    proportion_time = total_duration / (observation_minutes * 60)
  ) %>%
  select(troop, focal_id, behavior, proportion_time) %>%
  pivot_wider(
    names_from = behavior,
    values_from = proportion_time,
    values_fill = 0
  )

behavior_summary <- lemur_behavior %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Self-scratch",
      "Vigilance",
      "Yawn"
    )
  ) %>%
  group_by(troop, focal_id, behavior) %>%
  summarise(
    n_events = n(),
    total_duration = sum(duration_seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    focal_observation_time,
    by = c("troop", "focal_id")
  ) %>%
  mutate(
    event_rate = n_events / observation_minutes,
    proportion_time = total_duration / (observation_minutes * 60)
  )
behavior_summary %>%
  group_by(behavior) %>%
  summarise(
    sd_event_rate = sd(event_rate),
    sd_proportion_time = sd(proportion_time),
    .groups = "drop"
  )

#retain duration due to more variation: give/receive/mutual groom, contact sit, vig.
#retain rate due to more variation: self-groom, self-scratch, rate, agonism (candidate)

agonism_rate <- lemur_behavior %>%
  group_by(troop, focal_id) %>%
  summarise(
    agonism_events = sum(agonism),
    .groups = "drop"
  ) %>%
  left_join(
    focal_observation_time,
    by = c("troop", "focal_id")
  ) %>%
  mutate(
    agonism_rate = agonism_events / observation_minutes
  )

#examine distribution of candidate variables
pca_candidates <- behavior_summary %>%
  select(
    troop,
    focal_id,
    behavior,
    event_rate,
    proportion_time
  ) %>%
  mutate(
    value = case_when(
      behavior %in% c(
        "Contact sit",
        "Give grooming",
        "Mutual grooming",
        "Receive grooming",
        "Vigilance"
      ) ~ proportion_time,
      behavior %in% c(
        "Self-groom",
        "Self-scratch",
        "Yawn"
      ) ~ event_rate
    )
  ) %>%
  select(troop, focal_id, behavior, value) %>%
  pivot_wider(
    names_from = behavior,
    values_from = value
  ) %>%
  left_join(
    agonism_rate %>%
      select(troop, focal_id, agonism_rate),
    by = c("troop", "focal_id")
  )

#summarize distributions
pca_candidates %>%
  pivot_longer(
    cols = -c(troop, focal_id),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    sd = sd(value),
    min = min(value),
    max = max(value),
    zeros = sum(value == 0),
    .groups = "drop"
  )


pca_candidates %>%
  pivot_longer(
    cols = -c(troop, focal_id),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, height = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#correlations between behaviors
pca_candidates %>%
  select(-troop, -focal_id) %>%
  cor()
#no correlations above +/- 0.61


pca_data <- pca_candidates
saveRDS(pca_data, "clean_data/pca_data.rds")
