library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(psych)

normalized_data = pca_data %>%
  select(-troop, -focal_id)
normalized_data = scale(normalized_data)
corr_matrix = cor(normalized_data)
ggcorrplot(corr_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
#no problematic correlations


pca_model <- prcomp(
  pca_data %>%
    select(-troop, -focal_id), #unselect troop and focal_id
  scale. = TRUE #standardize variables
)
summary(pca_model)


pca_model$rotation


plot(pca_model, type = "l")
#5 PCs potentially suggested by the scree plot, but PC5 has an eigenvalue <1

scree_values <- pca_model$sdev^2

scree_values
diff(scree_values)



#investigating PC1-PC4
pca_loadings <- as.data.frame(pca_model$rotation) %>%
  select(PC1, PC2, PC3, PC4) %>%
  mutate(variable = rownames(.)) %>% #save behavior names as column: variable
  pivot_longer(
    cols = starts_with("PC"),
    names_to = "PC",
    values_to = "loading" #turn PC columns into rows
  ) %>%
  mutate(
    abs_loading = abs(loading) #calculate absolute value of each loading
  ) %>%
  arrange(PC, desc(abs_loading)) #sort each PC from strongest to weakest loading

pca_loadings %>%
  print(n=Inf)


#loading plot for PC1-PC4
ggplot(
  pca_loadings,
  aes(x = PC, y = loading, label = variable)
) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text(vjust = -0.5)

#PC1: social affiliation vs. vigilance/agonism (but self-scratch?)
#PC2: self-directed vs social interaction (but agonism?)
#PC3: mainly driven by yawning but could relate to coping style??
#PC4: clear self-directed/vigilance tendency


################################################
########## RETAINING NO AGONISM MODEL ##########
################################################

#PCA without agonism
pca_model_no_agonism <- prcomp(
  pca_data %>%
    select(-troop, -focal_id, -agonism_rate),
  scale. = TRUE
)
summary(pca_model_no_agonism)
#does not meaningfully chance variance structure

pca_model_no_agonism$rotation[,1:4]
#PC1 is essentially the same; mutual grooming is stronger
####### give groom (-0.46), mutual groom (-0.42), receive groom (-0.45)
####### vigilance (0.47), yawn (0.34)

#PC2: similar but scratch is much stronger, yawn is added
####### contact sit (0.53), mutual groom (0.42), yawn (0.45)
####### self scratch (-0.54)

#PC3: self-groom becomes dominant (+.728); cleaner than PCA with agonism
####### give groom (0.42), self-groom (0.73), yawn (0.43)

#PC4: biggest difference; contact sit replaces self groom
####### contact sit (0.53), self-scratch (0.42), vig (0.52), self groom (0.35)


pca_loadings_no_ag <- as.data.frame(pca_model_no_agonism$rotation) %>%
  select(PC1, PC2, PC3, PC4) %>%
  mutate(variable = rownames(.)) %>% #save behavior names as column: variable
  pivot_longer(
    cols = starts_with("PC"),
    names_to = "PC",
    values_to = "loading" #turn PC columns into rows
  ) %>%
  mutate(
    abs_loading = abs(loading) #calculate absolute value of each loading
  ) %>%
  arrange(PC, desc(abs_loading)) #sort each PC from strongest to weakest loading

ggplot(
  pca_loadings_no_ag,
  aes(x = PC, y = loading, label = variable)
) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text(vjust = -0.5)

pca_scores_no_ag <- as.data.frame(pca_model_no_agonism$x) %>%
  bind_cols(
    pca_data %>%
      select(troop, focal_id))
pca_scores_no_ag %>%
  select(troop, focal_id, PC1) %>%
  arrange(PC1)

pca_scores_no_ag %>%
  select(focal_id, PC1, PC2, PC3, PC4) %>%
  arrange(PC1)

#################################
# evidence for not including agonism:
# does not chance variance or overall PC structure (first 4 explain 81-82%)
# doesn't chance number of components with eigenvalues > 1
# similar individual-level scores across all 4 PCs
# makes PCs harder to interpret
# agonism is a sparce variable: only 40 events out of whole dataset and 12/17 individuals

#correlations between PC components agonism vs no agonism
cor(
  pca_scores$PC1,
  pca_scores_no_ag$PC1
) 
#PC1: -0.927
#PC2: -0.82
#PC3: 0.846
#PC4: -0.863

pca_data = pca_data %>%
  select(-agonism_rate)


# looking at individuals for each PC

### PC1: grooming/affiliation vs vigilance/yawning
pca_scores_no_ag %>%
  select(troop, focal_id, PC1) %>%
  arrange(PC1)
pca_data %>%
  filter(focal_id %in% c("TCH", "PHA", "ART", "TLY")) %>%
  print(n=Inf)

### PC2: contact sit/mutual groom/yawn vs self-scratch
pca_scores_no_ag %>%
  select(troop, focal_id, PC2) %>%
  arrange(PC2)
pca_data %>%
  filter(focal_id %in% c("POL", "CJA", "CHA", "AST"))

### PC3: Primarily self-grooming, with contributions from yawning/grooming
pca_scores_no_ag %>%
  select(troop, focal_id, PC3) %>%
  arrange(PC3)
pca_data %>%
  filter(focal_id %in% c("ASH", "AST", "LAU", "CJA"))

### PC4:
pca_scores_no_ag %>%
  select(troop, focal_id, PC4) %>%
  arrange(PC4)
pca_data %>%
  filter(focal_id %in% c("LAU", "DEL", "CJA", "PHA"))


###############################################
## Parallel analysis: ultimately not useful due to small sample size
fa.parallel(
  pca_data %>%
    select(-troop, -focal_id),
  fa = "pc",
  n.iter = 1000
)
eigen(
  cor(
    pca_data %>%
      select(-troop, -focal_id)
  )
)$values
