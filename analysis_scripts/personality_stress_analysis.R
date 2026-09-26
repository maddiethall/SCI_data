library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(lme4)
library(glmmTMB)

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

pca_scores %>%
  left_join(fecal_mean_time, by = "focal_id") %>%
  select(troop, focal_id, PC1, PC2, mean_time) %>%
  arrange(mean_time)

#correlations between PCs and sampling time
analysis2_individual <- pca_scores %>%
  left_join(fecal_mean_time, by = "focal_id")

cor.test(analysis2_individual$PC1,
         analysis2_individual$mean_time)

cor.test(analysis2_individual$P2,
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


analysis2_data_repeated <- fecal_clean %>%
  left_join(
    pca_scores %>%
      select(focal_id, PC1, PC2, PC3, PC4),
    by = "focal_id"
  ) %>%
  mutate(
    log_fgc = log(fecal_gc)
  )

analysis2_data_repeated <- analysis2_data_repeated %>%
  left_join(
    focal_metadata %>%
      select(focal_id, Age, Troop_id),
    by = "focal_id"
  )

analysis2_data_repeated <- analysis2_data_repeated %>%
  mutate(
    environment = case_when(
      Troop_id == "AAS" ~ "captive",
      Troop_id == "TLACJ" ~ "captive",
      Troop_id == "ER" ~ "free_range",
      Troop_id == "WM" ~ "free_range",
      Troop_id == "YB" ~ "free_range")
)


analysis2_data = analysis2_data %>%
  group_by(focal_id) %>%
  summarise(
      median_fgc = median(fecal_gc),
      median_logfgc = median(log_fgc),
      mean_fgc = mean(fecal_gc),
      mean_logfgc = mean(log_fgc),
      PC1 = mean(PC1),
      PC2 = mean(PC2),
      PC3 = mean(PC3),
      PC4 = mean(PC4),
      .groups = "drop"
    )
    
analysis2_data_summary = analysis2_data %>%
  left_join(
  focal_metadata %>%
    select(focal_id, Age, Troop_id),
  by = "focal_id"
)


#####################################
#### BEGIN ANALYSIS 2

fecal_model_lmer <- lmer(
  log_fgc ~ PC1 + PC2 + (1 | environment),
  data = analysis2_data_repeated
)
summary(fecal_model_lmer)





