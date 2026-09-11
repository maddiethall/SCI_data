library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(psych)

## removing yawning from PCA (in addition to agonism)
#### rational: 
####### included primarily as a behavioral indicator of stress
####### relatively rare: ~5.7 events per individual

pca_data_reduced = pca_data %>%
  select(-agonism_rate, -Yawn)


pca_model_reduced <- prcomp(
  pca_data_reduced %>%
    select(-troop, -focal_id),
  scale. = TRUE
)
summary(pca_model_reduced)
pca_model_reduced$rotation[,1:4]

# removing yawning didn't fundamentally change the overall dimensional structure
## but, cleaner PCs?

## PC1:
### Give grooming: −0.536
### Mutual grooming: −0.518
### Receive grooming: −0.437
### Vigilance: +0.471

# PC2:
### Contact sit: +0.634
### Self-scratch: −0.520
### Self-groom: −0.476

## PC3 and PC4 are messy

# looking at individuals
pca_scores_reduced <- as.data.frame(pca_model_reduced$x) %>%
  bind_cols(
    pca_data_reduced %>%
      select(troop, focal_id))

## PC1
pca_scores_reduced %>%
  select(troop, focal_id, PC1) %>%
  arrange(PC1)
## PC2: potentially, socially-oriented coping vs self-directed coping
pca_scores_reduced %>%
  select(troop, focal_id, PC2) %>%
  arrange(PC2)
## PC3:
pca_scores_reduced %>%
  select(troop, focal_id, PC3) %>%
  arrange(PC3)
## PC4:
pca_scores_reduced %>%
  select(troop, focal_id, PC4) %>%
  arrange(PC4)

############## RETAIN PC1 & 2?
