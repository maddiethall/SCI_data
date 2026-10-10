library(dplyr)
library(tidyr)
library(ggplot2)

stress_wide <- stress_analysis %>%
  select(focal_id, behavior, stress_value) %>%
  pivot_wider(
    names_from = behavior,
    values_from = stress_value,
    names_prefix = "stress_"
  )

social_stress <- relationship_analysis %>%
  left_join(stress_wide, by = "focal_id")


ggplot(
  social_stress,
  aes(
    x = mean_reciprocity,
    y = `stress_Self-scratch`,
    color = context
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black"
  ) +
  theme_classic() +
  labs(
    x = "Mean grooming reciprocity",
    y = "Self-scratching (events/hour)",
    color = "Context",
    title = "Grooming reciprocity and self-scratching"
  )


#self-scratch
cor.test(
  social_stress$mean_reciprocity,
  social_stress$`stress_Self-scratch`,
  method = "spearman",
  exact = FALSE
) # r = 0.31

cor.test(
  social_stress$mean_asymmetry,
  social_stress$`stress_Self-scratch`,
  method = "spearman",
  exact = FALSE
) # r = 0.16

cor.test(
  social_stress$total_proximity,
  social_stress$`stress_Self-scratch`,
  method = "spearman",
  exact = FALSE
) # r = 0.33


#vigilance
cor.test(
  social_stress$mean_reciprocity,
  social_stress$stress_Vigilance,
  method = "spearman",
  exact = FALSE
) # r = 0.066

cor.test(
  social_stress$mean_asymmetry,
  social_stress$stress_Vigilance,
  method = "spearman",
  exact = FALSE
) # r = 0.23

cor.test(
  social_stress$total_proximity,
  social_stress$stress_Vigilance,
  method = "spearman",
  exact = FALSE
) # r = -0.61, p = 0.01

cor.test(
  social_stress$mean_close_proximity,
  social_stress$stress_Vigilance,
  method = "spearman",
  exact = FALSE
) # r = 0.4


cor.test(
  social_stress$n_partners,
  social_stress$stress_Vigilance,
  method = "spearman",
  exact = FALSE
) # r = -0.75, p < 0.001


ggplot(
  social_stress,
  aes(
    x = total_proximity,
    y = stress_Vigilance,
    color = context
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_classic() +
  labs(
    x = "Total close proximity",
    y = "Vigilance (minutes/hour)",
    color = "Context",
    title = "Social proximity and vigilance"
  )

social_stress %>%
  group_by(context) %>%
  summarise(
    n = sum(complete.cases(total_proximity, stress_Vigilance)),
    rho = cor(
      total_proximity,
      stress_Vigilance,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  )

social_stress %>%
  group_by(context) %>%
  summarise(
    n = n(),
    mean_vigilance = mean(stress_Vigilance, na.rm = TRUE),
    sd_vigilance = sd(stress_Vigilance, na.rm = TRUE),
    median_vigilance = median(stress_Vigilance, na.rm = TRUE),
    mean_partners = mean(n_partners, na.rm = TRUE),
    mean_total_proximity = mean(total_proximity, na.rm = TRUE),
    .groups = "drop"
  )

model_vigilance <- lm(
  stress_Vigilance ~ total_proximity + n_partners + context,
  data = social_stress
)

summary(model_vigilance)

# vig by group
social_stress %>%
  group_by(context, troop, n_partners) %>%
  summarise(
    n = n(),
    mean_vigilance = mean(stress_Vigilance, na.rm = TRUE),
    sd_vigilance = sd(stress_Vigilance, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  social_stress,
  aes(
    x = troop,
    y = stress_Vigilance,
    color = context
  )
) +
  geom_jitter(width = 0.1, height = 0, size = 3) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.4,
    color = "black"
  ) +
  theme_classic() +
  labs(
    x = "Troop",
    y = "Vigilance (minutes/hour)",
    color = "Context",
    title = "Individual vigilance by troop"
  )


stress_analysis %>%
  filter(behavior == "Self-scratch") %>%
  left_join(
    relationship_analysis %>%
      select(focal_id, context),
    by = "focal_id"
  ) %>%
  group_by(context) %>%
  summarise(
    n = n(),
    mean_scratch = mean(events_per_hour, na.rm = TRUE),
    sd_scratch = sd(events_per_hour, na.rm = TRUE),
    median_scratch = median(events_per_hour, na.rm = TRUE),
    .groups = "drop"
  )
scratch_data <- stress_analysis %>%
  filter(behavior == "Self-scratch") %>%
  left_join(
    relationship_analysis %>%
      select(focal_id, context, troop),
    by = "focal_id"
  )

ggplot(scratch_data,
       aes(x = context, y = events_per_hour, color = context)) +
  geom_jitter(width = 0.1, size = 3) +
  theme_classic() +
  labs(
    x = "Context",
    y = "Self-scratching (events/hour)",
    title = "Self-scratching by context"
  )
wilcox.test(
  events_per_hour ~ context,
  data = scratch_data,
  exact = FALSE
)
