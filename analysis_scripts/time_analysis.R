library(lme4)
library(dplyr)
library(ggplot2)
library(lmerTest)

fecal = readRDS("fecal_clean.rds")

ggplot(fecal, aes(x = time, y = fecal_gc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Collection time (hour)",
    y = "Fecal glucocorticoid concentration",
    title = "Fecal glucocorticoid concentration by collection time"
  ) +
  theme_classic()

ggplot(fecal, aes(x = time)) +
  geom_histogram(bins = 15) +
  labs(
    x = "Collection time (hour)",
    y = "Number of samples",
    title = "Distribution of fecal sample collection times"
  ) +
  theme_classic()


fecal %>%
  group_by(animal_id) %>%
  summarise(
    n = n(),
    mean_time = mean(time, na.rm = TRUE),
    min_time = min(time, na.rm = TRUE),
    max_time = max(time, na.rm = TRUE)
  )


morning_data = fecal %>%
  filter(time <= 12)

morning_model = lmer(
  fecal_gc ~ time + (1 | animal_id),
  data = morning_data
)
summary(morning_model)



ggplot(fecal, aes(x = animal_id, y = time)) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    x = "Animal",
    y = "Collection time",
    title = "Collection time by individual"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(fecal, aes(x = time, y = fecal_gc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  facet_wrap(~ animal_id) +
  labs(
    x = "Collection time (hour)",
    y = "Fecal glucocorticoid concentration",
    title = "Fecal glucocorticoid concentration by collection time and individual"
  ) +
  theme_classic()

ggplot(fecal, aes(x = animal_id, y = fecal_gc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(size = time), width = 0.15, alpha = 0.7) +
  labs(
    x = "Individual",
    y = "Fecal glucocorticoid concentration",
    size = "Collection time",
    title = "fGC by individual with collection time indicated"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

######################################

time_model_full = lmer(
  fecal_gc ~ time + (1 | animal_id),
  data = fecal
)
summary(time_model_full)

time_model_morning = lmer(
  fecal_gc ~ time + (1 | animal_id),
  data = morning_data
)
summary(time_model_morning)


model_no_time = lmer(
  fecal_gc ~ (1 | animal_id),
  data = fecal,
  REML = FALSE
)
model_with_time = lmer(
  fecal_gc ~ time + (1 | animal_id),
  data = fecal,
  REML = FALSE
)

anova(model_no_time, model_with_time)


model_time_quad = lmer(
  fecal_gc ~ time + I(time^2) + (1 | animal_id),
  data = fecal,
  REML = FALSE
)

anova(model_with_time, model_time_quad)




summary(lm(time ~ animal_id, data = fecal))
anova(lm(time ~ animal_id, data = fecal))


fgc_time_model = lm(
  fecal_gc ~ animal_id + time,
  data = fecal
)


summary(fgc_time_model)
anova(fgc_time_model)

fgc_model_no_time = lm(
  fecal_gc ~ animal_id,
  data = fecal
)

anova(fgc_model_no_time)


