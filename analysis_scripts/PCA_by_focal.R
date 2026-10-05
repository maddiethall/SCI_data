#################### VARIABLE SELECTION FOR PCA

# behavior prevalence
behavior_prevalence_corrected <- behavior_by_focal_summary_corrected %>%
  group_by(behavior) %>%
  summarise(
    n_individuals_observed = sum(avg_rate > 0),
    n_individuals_zero = sum(avg_rate == 0),
    .groups = "drop"
  ) %>%
  arrange(n_individuals_observed)

# rate/duraction correlations
rate_duration_correlations <- behavior_by_focal_summary_corrected %>%
  group_by(behavior) %>%
  summarise(
    correlation = cor(
      avg_rate,
      avg_proportion,
      use = "complete.obs"
    ),
    .groups = "drop"
  ) %>%
  arrange(correlation)

# discrete events as rates: self-scratch, yawn, greet, displace, be displaced, head throw-back
# duration more biologically meaningful for: contact sit, all grooming, self-grooming, vigilance

# investigating intermediately prevalent behaviors: displace, be displaced, head throw-back, greet
behavior_by_focal_summary_corrected %>%
  filter(
    behavior %in% c(
      "Head throw-back",
      "Be displaced",
      "Displace"
    )
  ) %>%
  select(focal_id, behavior, avg_rate) %>%
  tidyr::pivot_wider(
    names_from = behavior,
    values_from = avg_rate
  ) %>%
  arrange(focal_id)

behavior_by_focal_summary_corrected %>%
  filter(behavior == "Greet") %>%
  select(focal_id, avg_rate) %>%
  arrange(avg_rate)

# retain greet, drop others

# looking at scan data
scan_activity_summary %>%
  group_by(activity) %>%
  summarise(
    n_individuals_observed = sum(avg_proportion > 0),
    n_individuals_zero = sum(avg_proportion == 0),
    min = min(avg_proportion),
    median = median(avg_proportion),
    max = max(avg_proportion),
    .groups = "drop"
  )

# comparing vigilance vs "other" in scan
vigilance_compare <- behavior_by_focal_summary_corrected %>%
  filter(behavior == "Vigilance") %>%
  select(
    focal_id,
    vigilance_continuous = avg_proportion
  ) %>%
  left_join(
    scan_activity_summary %>%
      filter(activity == "Other") %>%
      select(
        focal_id,
        other_scan = avg_proportion
      ),
    by = "focal_id"
  )

cor.test(
  vigilance_compare$vigilance_continuous,
  vigilance_compare$other_scan
)
# r = 0.974


# comparing social behavior vs "social" in scan
social_compare <- behavior_by_focal_summary_corrected %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming"
    )
  ) %>%
  select(focal_id, behavior, avg_proportion) %>%
  tidyr::pivot_wider(
    names_from = behavior,
    values_from = avg_proportion
  ) %>%
  left_join(
    scan_activity_summary %>%
      filter(activity == "Social") %>%
      select(
        focal_id,
        scan_social = avg_proportion
      ),
    by = "focal_id"
  )
# strong correlation with mutual grooming, receive, and give, but not contact sit
# "social" measures active social engagement

# scan variables to retain: social, rest, locomotion
# correlations between scan variables: none

scan_activity_summary %>%
  filter(activity %in% c("Rest", "Social", "Locomotion")) %>%
  select(focal_id, activity, avg_proportion) %>%
  tidyr::pivot_wider(
    names_from = activity,
    values_from = avg_proportion
  ) %>%
  select(-focal_id) %>%
  cor()

################### GENERATING PCA DATASET
pca_continuous <- behavior_by_focal_summary_corrected %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Self-scratch",
      "Vigilance",
      "Yawn",
      "Greet"
    )
  ) %>%
  mutate(
    pca_value = if_else(
      behavior %in% c("Self-scratch", "Yawn", "Greet"),
      avg_rate,
      avg_proportion
    )
  ) %>%
  select(focal_id, behavior, pca_value) %>%
  tidyr::pivot_wider(
    names_from = behavior,
    values_from = pca_value
  )

pca_scan <- scan_activity_summary %>%
  filter(
    activity %in% c(
      "Rest",
      "Social",
      "Locomotion"
    )
  ) %>%
  select(focal_id, activity, avg_proportion) %>%
  tidyr::pivot_wider(
    names_from = activity,
    values_from = avg_proportion
  )
pca_candidates <- pca_continuous %>%
  left_join(
    pca_scan,
    by = "focal_id"
  )

# inspect correlation structure
candidate_correlations <- pca_candidates %>%
  select(-focal_id) %>%
  cor()

round(candidate_correlations, 2)
# Vigilance vs. Rest: r = −.93
# keep vigilance, drop rest


# KMO measure of sampling adequacy
pca_candidates_reduced <- pca_candidates %>%
  select(-Rest)

KMO(
  cor(
    pca_candidates_reduced %>%
      select(-focal_id, -Social, -Locomotion, -Yawn, -`Self-scratch`)
  )
)

pca_by_focal <- prcomp(
  pca_candidates_reduced %>%
    select(-focal_id, -Social, -Locomotion, -Yawn),
  scale. = TRUE
)

summary(pca_by_focal)
pca_by_focal$rotation[,1:4]




fecal_median = fecal_clean %>%
  group_by(focal_id) %>%
  summarise(
    median_fgc = median(fecal_gc)
  )

####### repeatability of candidate behaviors

vigilance_repeatability <- behavior_by_focal_complete %>%
  filter(behavior == "Vigilance") %>%
  select(
    focal_id,
    troop,
    datetime,
    proportion_per_focal
  )
vigilance_repeatability %>%
  group_by(focal_id) %>%
  summarise(
    n_focals = n(),
    mean_vigilance = mean(proportion_per_focal),
    sd_vigilance = sd(proportion_per_focal)
  )
vigilance_rpt <- rpt(
  proportion_per_focal ~ (1 | focal_id),
  grname = "focal_id",
  data = vigilance_repeatability,
  datatype = "Gaussian",
  nboot = 1000,
  npermut = 1000
)
# R = 0.229, 95% CI [0.098, 0.359], permutation p = .001; repeatability in vigilance across focal observations



repeatability_continuous <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Vigilance",
      "Self-scratch",
      "Yawn",
      "Greet"
    )
  ) %>%
  mutate(
    repeatability_value = if_else(
      behavior %in% c("Self-scratch", "Yawn", "Greet"),
      rate_per_minute,
      proportion_per_focal
    )
  ) %>%
  select(
    troop,
    focal_id,
    datetime,
    behavior,
    repeatability_value
  )

repeatability_scan <- scan_activity_complete %>%
  filter(
    activity %in% c(
      "Rest",
      "Social",
      "Locomotion"
    )
  ) %>%
  select(
    troop,
    focal_id,
    datetime,
    behavior = activity,
    repeatability_value = proportion_per_focal
  )

repeatability_all <- bind_rows(
  repeatability_continuous,
  repeatability_scan
)

scratch_repeatability <- repeatability_all %>%
  filter(behavior == "Self-scratch")
scratch_repeatability %>%
  summarise(
    n_focals = n(),
    n_zero = sum(repeatability_value == 0),
    percent_zero = mean(repeatability_value == 0) * 100,
    min = min(repeatability_value),
    max = max(repeatability_value),
    mean = mean(repeatability_value)
  )
scratch_counts <- behavior_by_focal_complete %>% # Poisson repeatability model
  filter(behavior == "Self-scratch") %>%
  select(
    focal_id,
    troop,
    datetime,
    n_events
  )
scratch_counts %>%
  summarise(
    mean_count = mean(n_events),
    variance_count = var(n_events),
    variance_mean_ratio = var(n_events) / mean(n_events)
  )
scratch_poisson <- glmer(
  n_events ~ 1 + (1 | focal_id),
  data = scratch_counts,
  family = poisson
)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  
  c(
    chisq = Pearson.chisq,
    ratio = prat,
    rdf = rdf
  )
}
overdisp_fun(scratch_poisson) # overdispersed

scratch_nb <- glmmTMB( #negative binomial mixed model
  n_events ~ 1 + (1 | focal_id),
  data = scratch_counts,
  family = nbinom2
)
scratch_nb_null <- glmmTMB(
  n_events ~ 1,
  data = scratch_counts,
  family = nbinom2
)
# self-scratching is not showing detectable stable individual differences
## individual identity does not improve likelihood ratio

repeatability_summary <- repeatability_all %>%
  group_by(behavior) %>%
  summarise(
    n = n(),
    n_zero = sum(repeatability_value == 0),
    percent_zero = mean(repeatability_value == 0) * 100,
    mean = mean(repeatability_value),
    sd = sd(repeatability_value),
    min = min(repeatability_value),
    max = max(repeatability_value),
    .groups = "drop"
  )

duration_repeatability <- repeatability_all %>%
  filter(
    behavior %in% c(
      "Contact sit",
      "Give grooming",
      "Mutual grooming",
      "Receive grooming",
      "Self-groom",
      "Vigilance"
    )
  )

# Gaussian random-intercept model
duration_icc <- duration_repeatability %>%
  group_by(behavior) %>%
  group_modify(~ {
    
    model <- lmer(
      repeatability_value ~ 1 + (1 | focal_id),
      data = .x
    )
    
    variances <- as.data.frame(VarCorr(model))
    
    individual_variance <- variances$vcov[
      variances$grp == "focal_id"
    ]
    
    residual_variance <- variances$vcov[
      variances$grp == "Residual"
    ]
    
    tibble(
      individual_variance = individual_variance,
      residual_variance = residual_variance,
      ICC = individual_variance /
        (individual_variance + residual_variance)
    )
  })

scan_icc <- repeatability_all %>%
  filter(
    behavior %in% c(
      "Rest",
      "Social",
      "Locomotion"
    )
  ) %>%
  group_by(behavior) %>%
  group_modify(~ {
    
    model <- lmer(
      repeatability_value ~ 1 + (1 | focal_id),
      data = .x
    )
    
    variances <- as.data.frame(VarCorr(model))
    
    individual_variance <- variances$vcov[
      variances$grp == "focal_id"
    ]
    
    residual_variance <- variances$vcov[
      variances$grp == "Residual"
    ]
    
    tibble(
      individual_variance = individual_variance,
      residual_variance = residual_variance,
      ICC = individual_variance /
        (individual_variance + residual_variance)
    )
  })

event_counts <- behavior_by_focal_complete %>%
  filter(
    behavior %in% c(
      "Yawn",
      "Greet"
    )
  ) %>%
  select(
    focal_id,
    troop,
    datetime,
    behavior,
    n_events
  )

greet_counts <- event_counts %>%
  filter(behavior == "Greet")

yawn_counts <- event_counts %>%
  filter(behavior == "Yawn")

greet_poisson <- glmer(
  n_events ~ 1 + (1 | focal_id),
  data = greet_counts,
  family = poisson
)

yawn_poisson <- glmer(
  n_events ~ 1 + (1 | focal_id),
  data = yawn_counts,
  family = poisson
)

overdisp_fun(yawn_poisson)

yawn_nb <- glmmTMB(
  n_events ~ 1 + (1 | focal_id),
  data = yawn_counts,
  family = nbinom2
)

yawn_nb_null <- glmmTMB(
  n_events ~ 1,
  data = yawn_counts,
  family = nbinom2
)

anova(yawn_nb_null, yawn_nb)
