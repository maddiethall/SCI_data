library(tidyverse)
library(lme4)
library(dplyr)
library(ggplot2)
library(glmmTMB)

social_summary <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Receive grooming",
      "Mutual grooming",
      "Play"
    )
  ) %>%
  group_by(behavior) %>%
  summarise(
    total_events = sum(n_events),
    total_duration_min = sum(duration_per_focal) / 60,
    n_individuals_with_behavior = n_distinct(focal_id[n_events > 0]),
    pct_focals_zero = mean(n_events == 0) * 100,
    .groups = "drop"
  )


### testing context
social_context_summary <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Receive grooming",
      "Mutual grooming"
    )
  ) %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  ) %>%
  group_by(behavior, context) %>%
  summarise(
    n_focals = n(),
    n_individuals = n_distinct(focal_id),
    pct_zero = mean(duration_per_focal == 0) * 100,
    mean_seconds = mean(duration_per_focal),
    median_seconds = median(duration_per_focal),
    sd_seconds = sd(duration_per_focal),
    .groups = "drop"
  )

# contact sitting: FR ~26% lower; p=.115
contact_context <- behavior_by_focal_complete %>%
  filter(behavior == "Contact sit") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

contact_context %>%
  filter(duration_per_focal > 0) %>%
  summarise(
    n_positive = n(),
    mean = mean(duration_per_focal),
    median = median(duration_per_focal),
    sd = sd(duration_per_focal),
    q75 = quantile(duration_per_focal, .75),
    q90 = quantile(duration_per_focal, .90),
    q95 = quantile(duration_per_focal, .95),
    max = max(duration_per_focal)
  )


contact_context_model <- glmmTMB(
  duration_per_focal ~ context + (1 | focal_id),
  family = tweedie(link = "log"),
  data = contact_context
)

# give and receive grooming:FR ~2.44× higher; p=.0015

directional_grooming <- behavior_by_focal_complete %>%
  filter(behavior %in% c("Give grooming", "Receive grooming")) %>%
  group_by(troop, focal_id, datetime_key) %>%
  summarise(
    duration_per_focal = sum(duration_per_focal),
    .groups = "drop"
  ) %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

directional_grooming_model <- glmmTMB(
  duration_per_focal ~ context + (1 | focal_id),
  family = tweedie(link = "log"),
  data = directional_grooming
)

# mutual grooming: No clear difference; p=.581

mutual_context <- behavior_by_focal_complete %>%
  filter(behavior == "Mutual grooming") %>%
  mutate(
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  )

mutual_context_model <- glmmTMB(
  duration_per_focal ~ context + (1 | focal_id),
  family = tweedie(link = "log"),
  data = mutual_context
)

########## fgc x directional grooming
# once context is accounted for, directional grooming has essentially no association with fGC

directional_grooming_individual <- directional_grooming %>%
  group_by(focal_id, context) %>%
  summarise(
    directional_grooming_sec_per_focal = mean(duration_per_focal),
    .groups = "drop"
  )

fecal_directional <- fecal_clean %>%
  left_join(
    directional_grooming_individual,
    by = "focal_id"
  )

directional_fgc_model <- lmer(
  log(fecal_gc) ~
    time +
    directional_grooming_sec_per_focal +
    context +
    (1 | animal_id),
  data = fecal_directional
)

# contact sitting x fgc
contact_individual <- contact_context %>%
  group_by(focal_id, context) %>%
  summarise(
    contact_sec_per_focal = mean(duration_per_focal),
    .groups = "drop"
  )

fecal_contact <- fecal_clean %>%
  left_join(
    contact_individual,
    by = "focal_id"
  )

contact_fgc_model <- lmer(
  log(fecal_gc) ~
    time +
    contact_sec_per_focal +
    context +
    (1 | animal_id),
  data = fecal_contact
)


# mutual grooming x fgc
mutual_individual <- mutual_context %>%
  group_by(focal_id, context) %>%
  summarise(
    mutual_groom_sec_per_focal = mean(duration_per_focal),
    .groups = "drop"
  )

fecal_mutual <- fecal_clean %>%
  left_join(
    mutual_individual,
    by = "focal_id"
  )

mutual_fgc_model <- lmer(
  log(fecal_gc) ~
    time +
    mutual_groom_sec_per_focal +
    context +
    (1 | animal_id),
  data = fecal_mutual
)

#################### aggregated fgc samples

individual_analysis <- fgc_adjusted %>%
  left_join(id_lookup, by = "animal_id") %>%
  mutate(
    troop = case_when(
      focal_id %in% c("ASH", "AST", "SEL") ~ "AAS",
      focal_id %in% c("ART", "CJA", "TLY") ~ "TLACJ",
      focal_id %in% c("DEL", "JJO", "LAU", "PHA") ~ "East Road",
      focal_id %in% c("AUT", "CHA", "POL", "TCH") ~ "Windmill",
      focal_id %in% c("DDA", "MAR", "MAY") ~ "Yankee"
    ),
    context = if_else(
      troop %in% c("AAS", "TLACJ"),
      "Captive",
      "Free-ranging"
    )
  ) %>%
  select(
    focal_id,
    animal_id,
    troop,
    context,
    predicted_log_fgc,
    adjusted_fgc
  )

stress_individual_wide <- stress_metrics %>%
  select(focal_id, behavior, stress_value) %>%
  pivot_wider(
    names_from = behavior,
    values_from = stress_value
  ) %>%
  rename(
    self_groom_min_hr = `Self-groom`,
    self_scratch_events_hr = `Self-scratch`,
    vigilance_min_hr = Vigilance,
    yawn_events_hr = Yawn
  )
individual_analysis <- individual_analysis %>%
  left_join(
    stress_individual_wide,
    by = "focal_id"
  )

individual_analysis <- individual_analysis %>%
  left_join(
    contact_individual %>%
      select(focal_id, contact_sec_per_focal),
    by = "focal_id"
  ) %>%
  left_join(
    directional_grooming_individual %>%
      select(focal_id, directional_grooming_sec_per_focal),
    by = "focal_id"
  ) %>%
  left_join(
    mutual_individual %>%
      select(focal_id, mutual_groom_sec_per_focal),
    by = "focal_id"
  )

troop_summary <- individual_analysis %>%
  group_by(context, troop) %>%
  summarise(
    n = n(),
    mean_adjusted_fgc = mean(adjusted_fgc),
    mean_self_groom = mean(self_groom_min_hr),
    mean_self_scratch = mean(self_scratch_events_hr),
    mean_vigilance = mean(vigilance_min_hr),
    mean_yawn = mean(yawn_events_hr),
    mean_contact = mean(contact_sec_per_focal),
    mean_directional_groom = mean(directional_grooming_sec_per_focal),
    mean_mutual_groom = mean(mutual_groom_sec_per_focal),
    .groups = "drop"
  )

individual_analysis %>%
  group_by(context) %>%
  summarise(
    n = n(),
    r_self_groom = cor(predicted_log_fgc, self_groom_min_hr),
    r_self_scratch = cor(predicted_log_fgc, self_scratch_events_hr),
    r_vigilance = cor(predicted_log_fgc, vigilance_min_hr),
    r_yawn = cor(predicted_log_fgc, yawn_events_hr),
    r_contact = cor(predicted_log_fgc, contact_sec_per_focal),
    r_directional_groom = cor(
      predicted_log_fgc,
      directional_grooming_sec_per_focal
    ),
    r_mutual_groom = cor(
      predicted_log_fgc,
      mutual_groom_sec_per_focal
    )
  )

fgc_individual_check <- fecal_clean %>%
  group_by(focal_id) %>%
  summarise(
    n_fecal = n(),
    mean_log_fgc = mean(log(fecal_gc)),
    mean_raw_fgc = mean(fecal_gc),
    mean_collection_time = mean(time),
    .groups = "drop"
  ) %>%
  left_join(
    individual_analysis %>%
      select(focal_id, predicted_log_fgc, adjusted_fgc),
    by = "focal_id"
  )

stress_models_individual <- list(
  self_groom = lm(
    predicted_log_fgc ~ self_groom_min_hr + context,
    data = individual_analysis
  ),
  
  self_scratch = lm(
    predicted_log_fgc ~ self_scratch_events_hr + context,
    data = individual_analysis
  ),
  
  vigilance = lm(
    predicted_log_fgc ~ vigilance_min_hr + context,
    data = individual_analysis
  ),
  
  yawn = lm(
    predicted_log_fgc ~ yawn_events_hr + context,
    data = individual_analysis
  )
)

lapply(stress_models_individual, summary)




social_models_individual <- list(
  contact = lm(
    predicted_log_fgc ~ contact_sec_per_focal + context,
    data = individual_analysis
  ),
  
  directional_grooming = lm(
    predicted_log_fgc ~ directional_grooming_sec_per_focal + context,
    data = individual_analysis
  ),
  
  mutual_grooming = lm(
    predicted_log_fgc ~ mutual_groom_sec_per_focal + context,
    data = individual_analysis
  )
)

lapply(social_models_individual, summary)



############ diagnostics

#contact siting
par(mfrow = c(2, 2))
plot(social_models_individual$contact)
par(mfrow = c(1, 1))

cooks.distance(social_models_individual$contact)

which(
  cooks.distance(social_models_individual$contact) >
    4 / nrow(individual_analysis)
)

# outside of Cook's D threshold:
## JJO: D = .251
## PHA: D = .265
## CJA: D = .586

# leave-one-out sensitivity analysis
contact_loo <- lapply(seq_len(nrow(individual_analysis)), function(i) {
  
  model <- lm(
    predicted_log_fgc ~ contact_sec_per_focal + context,
    data = individual_analysis[-i, ]
  )
  
  coef_table <- summary(model)$coefficients
  
  data.frame(
    omitted = individual_analysis$focal_id[i],
    estimate = coef_table["contact_sec_per_focal", "Estimate"],
    se = coef_table["contact_sec_per_focal", "Std. Error"],
    t = coef_table["contact_sec_per_focal", "t value"],
    p = coef_table["contact_sec_per_focal", "Pr(>|t|)"]
  )
})

contact_loo <- bind_rows(contact_loo)

contact_loo

contact_loo %>%
  summarise(
    min_estimate = min(estimate),
    max_estimate = max(estimate),
    min_p = min(p),
    max_p = max(p)
  )


#mutual grooming
par(mfrow = c(2, 2))
plot(social_models_individual$mutual_grooming)
par(mfrow = c(1, 1))

cooks.distance(social_models_individual$mutual_grooming)

which(
  cooks.distance(social_models_individual$mutual_grooming) >
    4 / nrow(individual_analysis)
)

# outside of Cook's D threshold:
## PHA: D = .288
## CJA: D = .499

# leave-one-out sensitivity analysis
mutual_loo <- lapply(seq_len(nrow(individual_analysis)), function(i) {
  
  model <- lm(
    predicted_log_fgc ~ mutual_groom_sec_per_focal + context,
    data = individual_analysis[-i, ]
  )
  
  coef_table <- summary(model)$coefficients
  
  data.frame(
    omitted = individual_analysis$focal_id[i],
    estimate = coef_table["mutual_groom_sec_per_focal", "Estimate"],
    se = coef_table["mutual_groom_sec_per_focal", "Std. Error"],
    t = coef_table["mutual_groom_sec_per_focal", "t value"],
    p = coef_table["mutual_groom_sec_per_focal", "Pr(>|t|)"]
  )
})

mutual_loo <- bind_rows(mutual_loo)

mutual_loo

mutual_loo %>%
  summarise(
    min_estimate = min(estimate),
    max_estimate = max(estimate),
    min_p = min(p),
    max_p = max(p)
  )

#####################
# contact sitting: no clear context differences
## fGC association after accounting for context is positive (b=.000703, p=.056)
## positive in all LOO models
# directional grooming: higher in FR; essentially no association with fGC after context (b=.000583, p=.811)
## mutual grooming: no clear context differences
## fGC association after accounting for context is positive (b=.00393, p=.086)
## positive in all LOO models
#####################

confint(social_models_individual$contact)
confint(social_models_individual$directional_grooming)
confint(social_models_individual$mutual_grooming)
lapply(stress_models_individual, confint)


# sensitivity analysis with raw vs predicted fGC values
individual_analysis <- individual_analysis %>%
  left_join(
    fgc_individual_check %>%
      select(focal_id, mean_log_fgc),
    by = "focal_id"
  )

stress_models_rawmean <- list(
  self_groom = lm(
    mean_log_fgc ~ self_groom_min_hr + context,
    data = individual_analysis
  ),
  self_scratch = lm(
    mean_log_fgc ~ self_scratch_events_hr + context,
    data = individual_analysis
  ),
  vigilance = lm(
    mean_log_fgc ~ vigilance_min_hr + context,
    data = individual_analysis
  ),
  yawn = lm(
    mean_log_fgc ~ yawn_events_hr + context,
    data = individual_analysis
  )
)

social_models_rawmean <- list(
  contact = lm(
    mean_log_fgc ~ contact_sec_per_focal + context,
    data = individual_analysis
  ),
  directional_grooming = lm(
    mean_log_fgc ~ directional_grooming_sec_per_focal + context,
    data = individual_analysis
  ),
  mutual_grooming = lm(
    mean_log_fgc ~ mutual_groom_sec_per_focal + context,
    data = individual_analysis
  )
)

extract_behavior_result <- function(model) {
  x <- summary(model)$coefficients
  
  data.frame(
    estimate = x[2, "Estimate"],
    se = x[2, "Std. Error"],
    t = x[2, "t value"],
    p = x[2, "Pr(>|t|)"]
  )
}

lapply(stress_models_rawmean, extract_behavior_result)
lapply(social_models_rawmean, extract_behavior_result)


