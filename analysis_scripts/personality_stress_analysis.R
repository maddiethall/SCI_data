library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)

#investing distribution of fgc
fecal_clean %>%
  count(animal_id)
summary(fecal_clean$fecal_gc)

hist(fecal_clean$fecal_gc) #heavily right-skewed
hist(log(fecal_clean$fecal_gc)) #normal distribution

#looking at time
summary(fecal_clean$time)
fecal_clean %>%
  group_by(animal_id) %>%
  summarise(
    mean_time = mean(time),
    min_time = min(time),
    max_time = max(time),
    n = n()
  ) %>%
  arrange(mean_time)

fecal_mean_time <- fecal_clean %>%
  group_by(focal_id) %>%
  summarise(
    mean_time = mean(time),
    .groups = "drop"
  )

pca_scores_reduced %>%
  left_join(fecal_mean_time, by = "focal_id") %>%
  select(troop, focal_id, PC1, PC2, mean_time) %>%
  arrange(mean_time)

#correlations between PCs and sampling time
analysis2_individual <- pca_scores_reduced %>%
  left_join(fecal_mean_time, by = "focal_id")

cor.test(analysis2_individual$PC1,
         analysis2_individual$mean_time)

cor.test(analysis2_individual$PC2,
         analysis2_individual$mean_time)
ggplot(analysis2_individual, aes(x = mean_time, y = PC2, label = focal_id)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.15) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Mean fGC sampling time",
    y = "PC2 score"
  ) +
  theme_minimal()

#### Sampling time varies substantially among individuals
#### PC1 isn't strongly associated with typical sampling time
#### PC2 has some association with sampling time, but appears heavily influenced by AST and ASH.

#####################################
#### BEGIN ANALYSIS 2

analysis2_data <- fecal_clean %>%
  left_join(
    pca_scores_reduced %>%
      select(focal_id, PC1, PC2),
    by = "focal_id"
  ) %>%
  mutate(
    log_fgc = log(fecal_gc)
  )

analysis2_data %>%
  select(animal_id, focal_id, date, fecal_gc, log_fgc, time, PC1, PC2) %>%
  arrange(focal_id, date)

