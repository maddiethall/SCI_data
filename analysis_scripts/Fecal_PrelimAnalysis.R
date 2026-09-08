library(dplyr)
library(ggplot2)
library(emmeans)

fecal = readRDS('/Users/maddiethall/R Repos/SCI_data/clean_data/fecal_clean.rds')

##############################################

ggplot(fecal, aes(x = fecal_gc)) +
  geom_histogram(bins = 20) +
  labs(
    x = "Fecal glucocorticoid concentration",
    y = "Number of samples",
    title = "Distribution of fecal glucocorticoid concentrations"
  ) +
  theme_classic()

ggplot(fecal,aes(
    x = reorder(animal_id, fecal_gc, FUN = median),
    y = fecal_gc
  )
) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    x = "Animal",
    y = "fGC concentration"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###################################

summary(aov(fecal_gc ~ animal_id, data = fecal))
