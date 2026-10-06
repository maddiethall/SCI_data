library(tidyverse)
library(lme4)
library(dplyr)
library(ggplot2)
library(glmmTMB)


# scratching

scratch_by_individual <- behavior_by_focal_complete %>%
  filter(behavior == "Self-scratch") %>%
  group_by(focal_id) %>%
  summarise(
    n_focals = n(),
    total_scratches = sum(n_events),
    observation_hours = n_focals * 15 / 60,
    scratches_per_hour = total_scratches / observation_hours,
    .groups = "drop"
  )


##### physiological stress

fgc_model <- lmer(
  log(fecal_gc) ~ time + (1 | animal_id),
  data = fecal_clean
)
# time effect is still negative on the log scale: estimate = −0.0428 per hour, t = −2.34
# substantial between-individual variation remaining after accounting for collection time: 
# animal variance = 0.0873, compared with residual variance = 0.1091

# time-adjusted fGC estimates
fgc_adjusted <- data.frame(
  animal_id = unique(fecal_clean$animal_id),
  time = 12
)
fgc_adjusted$predicted_log_fgc <- predict(
  fgc_model,
  newdata = fgc_adjusted,
  re.form = NULL
)
fgc_adjusted <- fgc_adjusted %>%
  mutate(
    adjusted_fgc = exp(predicted_log_fgc)
  )

####### fGC and scratching

id_lookup <- fecal_clean %>%
  distinct(animal_id, focal_id)

stress_individual <- scratch_by_individual %>%
  left_join(
    id_lookup,
    by = "focal_id"
  ) %>%
  left_join(
    fgc_adjusted %>%
      select(animal_id, predicted_log_fgc, adjusted_fgc),
    by = "animal_id"
  )
# weak positive association, but with substantial uncertainty and no evidence of 
# a statistically reliable relationship in these 17 females

###### all stress behaviors

stress_behaviors_by_individual <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Self-scratch",
      "Self-groom",
      "Yawn",
      "Vigilance"
    )
  ) %>%
  group_by(focal_id, behavior) %>%
  summarise(
    n_focals = n(),
    total_events = sum(n_events),
    observation_hours = n_focals * 15 / 60,
    events_per_hour = total_events / observation_hours,
    .groups = "drop"
  )
# Self-scratch → events/hour
# Yawn → events/hour
# Self-groom → minutes/hour
# Vigilance → minutes/hour

stress_metrics <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Self-scratch",
      "Self-groom",
      "Yawn",
      "Vigilance"
    )
  ) %>%
  group_by(focal_id, behavior) %>%
  summarise(
    n_focals = n(),
    observation_hours = n_focals * 15 / 60,
    total_events = sum(n_events),
    total_duration_seconds = sum(duration_per_focal),
    events_per_hour = total_events / observation_hours,
    minutes_per_hour = (total_duration_seconds / 60) / observation_hours,
    .groups = "drop"
  )
stress_metrics %>%
  group_by(behavior) %>%
  summarise(
    min_events_hr = min(events_per_hour),
    max_events_hr = max(events_per_hour),
    min_minutes_hr = min(minutes_per_hour),
    max_minutes_hr = max(minutes_per_hour)
  )

stress_metrics <- stress_metrics %>%
  mutate(
    stress_value = case_when(
      behavior %in% c("Self-scratch", "Yawn") ~ events_per_hour,
      behavior %in% c("Self-groom", "Vigilance") ~ minutes_per_hour
    )
  )

stress_analysis <- stress_metrics %>%
  left_join(
    id_lookup,
    by = "focal_id"
  ) %>%
  left_join(
    fgc_adjusted %>%
      select(animal_id, predicted_log_fgc, adjusted_fgc),
    by = "animal_id"
  )

stress_correlations <- stress_analysis %>%
  group_by(behavior) %>%
  summarise(
    r = cor(
      stress_value,
      predicted_log_fgc,
      method = "pearson"
    ),
    p = cor.test(
      stress_value,
      predicted_log_fgc,
      method = "pearson"
    )$p.value,
    .groups = "drop"
  )
# behavior           r        p
# 1 Self-groom    0.0920 0.725   
# 2 Self-scratch  0.198  0.445   
# 3 Vigilance    -0.760  0.000394
# 4 Yawn         -0.517  0.0335  

stress_analysis %>%
  filter(behavior == "Vigilance") %>%
  ggplot(
    aes(
      x = stress_value,
      y = predicted_log_fgc,
      label = focal_id
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(
    nudge_y = 0.03,
    size = 3,
    check_overlap = TRUE
  ) +
  labs(
    x = "Vigilance (minutes/hour)",
    y = "Time-adjusted log fGC",
    title = "Vigilance and fecal glucocorticoids"
  ) +
  theme_classic()


####### captive vs free-ranging

troop_lookup <- behavior_by_focal_complete %>%
  distinct(focal_id, troop)

stress_analysis <- stress_analysis %>%
  left_join(troop_lookup, by = "focal_id") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

stress_analysis %>%
  filter(behavior == "Vigilance") %>%
  ggplot(
    aes(
      x = stress_value,
      y = predicted_log_fgc,
      shape = context,
      linetype = context,
      label = focal_id
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    aes(group = context),
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    nudge_y = 0.025,
    size = 3,
    check_overlap = TRUE
  ) +
  labs(
    x = "Vigilance (minutes/hour)",
    y = "Time-adjusted log fGC",
    shape = "Context",
    linetype = "Context"
  ) +
  theme_classic()

########## using repeated fecal samples with vigilance

vigilance_individual <- stress_analysis %>%
  filter(behavior == "Vigilance") %>%
  select(
    focal_id,
    vigilance = stress_value,
    context
  )

fecal_vigilance <- fecal_clean %>%
  left_join(
    vigilance_individual,
    by = "focal_id"
  )

vig_model <- lmer(
  log(fecal_gc) ~ time + vigilance + context + (1 | animal_id),
  data = fecal_vigilance
)
# Once we account for captive vs. free-ranging context, the apparent vigilance–fGC association largely disappears:
# Vigilance: estimate = −0.0076, SE = 0.0086, t = −0.88
# Free-ranging vs. captive: estimate = +0.581 on the log-fGC scale, SE = 0.124, t = 4.71
# strong negative pooled association → very small negative association after context adjustment



######## yawning and fgc

yawn_individual <- stress_analysis %>%
  filter(behavior == "Yawn") %>%
  select(
    focal_id,
    yawn = stress_value,
    context
  )

fecal_yawn <- fecal_clean %>%
  left_join(yawn_individual, by = "focal_id")

yawn_model <- lmer(
  log(fecal_gc) ~ time + yawn + context + (1 | animal_id),
  data = fecal_yawn
)
# Yawning: moderate negative pooled association → essentially zero/slightly positive after context adjustment





############ distribution of stress behaviors
behavior_context_summary <- stress_analysis %>%
  group_by(behavior, context) %>%
  summarise(
    n = n(),
    mean = mean(stress_value),
    sd = sd(stress_value),
    median = median(stress_value),
    min = min(stress_value),
    max = max(stress_value),
    .groups = "drop"
  )

############ CAPTIVE VS FREE-RANGING
behavior_context_effects <- stress_analysis %>%
  group_by(behavior) %>%
  summarise(
    captive_mean = mean(stress_value[context == "Captive"]),
    free_mean = mean(stress_value[context == "Free-ranging"]),
    
    captive_sd = sd(stress_value[context == "Captive"]),
    free_sd = sd(stress_value[context == "Free-ranging"]),
    
    n_captive = sum(context == "Captive"),
    n_free = sum(context == "Free-ranging"),
    
    pooled_sd = sqrt(
      ((n_captive - 1) * captive_sd^2 +
         (n_free - 1) * free_sd^2) /
        (n_captive + n_free - 2)
    ),
    
    cohens_d = (free_mean - captive_mean) / pooled_sd,
    .groups = "drop"
  )

behavior_context_effects <- behavior_context_effects %>%
  mutate(
    df = n_captive + n_free - 2,
    correction = 1 - (3 / (4 * df - 1)),
    hedges_g = cohens_d * correction
  )

behavior_context_effects %>%
  select(
    behavior,
    captive_mean,
    free_mean,
    cohens_d,
    hedges_g
  )

# vigilance: FR lower, z = −4.17, p < .001
vig_context <- behavior_by_focal_complete %>%
  filter(behavior == "Vigilance") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )
vig_context %>%
  summarise(
    n_focals = n(),
    n_zero = sum(duration_per_focal == 0),
    pct_zero = mean(duration_per_focal == 0) * 100,
    mean_seconds = mean(duration_per_focal),
    sd_seconds = sd(duration_per_focal),
    median_seconds = median(duration_per_focal),
    max_seconds = max(duration_per_focal)
  )
vig_context %>%
  group_by(context) %>%
  summarise(
    n_focals = n(),
    n_individuals = n_distinct(focal_id),
    pct_zero = mean(duration_per_focal == 0) * 100,
    mean_seconds = mean(duration_per_focal),
    median_seconds = median(duration_per_focal),
    .groups = "drop"
  )

vig_context_model <- glmmTMB(
  cbind(
    duration_per_focal,
    900 - duration_per_focal
  ) ~ context + (1 | focal_id),
  family = betabinomial(link = "logit"),
  data = vig_context
)
# free-ranging females spent a significantly smaller proportion of 
# focal observation time vigilant than captive females, while accounting for 
# repeated focal observations within individuals


# yawning: FR lower, z = −2.55, p = .011
yawn_context <- behavior_by_focal_complete %>%
  filter(behavior == "Yawn") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )
yawn_context %>%
  group_by(context) %>%
  summarise(
    n_focals = n(),
    n_individuals = n_distinct(focal_id),
    total_yawns = sum(n_events),
    pct_zero = mean(n_events == 0) * 100,
    mean_count = mean(n_events),
    variance_count = var(n_events),
    max_count = max(n_events),
    .groups = "drop"
  )

yawn_context_model <- glmmTMB(
  n_events ~ context + (1 | focal_id),
  family = nbinom2,
  data = yawn_context
)

# self-scratching: FR somewhat higher, z = 1.19, p = .234
scratch_context <- behavior_by_focal_complete %>%
  filter(behavior == "Self-scratch") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

scratch_context %>%
  group_by(context) %>%
  summarise(
    n_focals = n(),
    n_individuals = n_distinct(focal_id),
    total_scratches = sum(n_events),
    pct_zero = mean(n_events == 0) * 100,
    mean_count = mean(n_events),
    variance_count = var(n_events),
    max_count = max(n_events),
    .groups = "drop"
  )

scratch_context_model <- glmmTMB(
  n_events ~ context + (1 | focal_id),
  family = nbinom2,
  data = scratch_context
)


scratch_individual <- stress_analysis %>%
  filter(behavior == "Self-scratch") %>%
  select(
    focal_id,
    scratch = stress_value,
    context
  )

fecal_scratch <- fecal_clean %>%
  left_join(
    scratch_individual,
    by = "focal_id"
  )

scratch_model <- lmer(
  log(fecal_gc) ~ time + scratch + context + (1 | animal_id),
  data = fecal_scratch
)




# self-grooming: No difference, z = −0.07, p = .944
groom_context <- behavior_by_focal_complete %>%
  filter(behavior == "Self-groom") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

groom_context %>%
  group_by(context) %>%
  summarise(
    n_focals = n(),
    n_individuals = n_distinct(focal_id),
    pct_zero = mean(duration_per_focal == 0) * 100,
    mean_seconds = mean(duration_per_focal),
    sd_seconds = sd(duration_per_focal),
    median_seconds = median(duration_per_focal),
    max_seconds = max(duration_per_focal),
    .groups = "drop"
  )
groom_context_model <- glmmTMB(
  cbind(
    duration_per_focal,
    900 - duration_per_focal
  ) ~ context + (1 | focal_id),
  family = betabinomial(link = "logit"),
  data = groom_context
)

groom_individual <- stress_analysis %>%
  filter(behavior == "Self-groom") %>%
  select(
    focal_id,
    self_groom = stress_value,
    context
  )

fecal_groom <- fecal_clean %>%
  left_join(
    groom_individual,
    by = "focal_id"
  )
groom_model <- lmer(
  log(fecal_gc) ~ time + self_groom + context + (1 | animal_id),
  data = fecal_groom
)



################ physiological context
fgc_context <- fecal_clean %>%
  left_join(
    troop_lookup,
    by = "focal_id"
  ) %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )
fgc_context_model <- lmer(
  log(fecal_gc) ~ time + context + (1 | animal_id),
  data = fgc_context
)






