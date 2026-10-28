library(tidyverse)
library(lme4)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)




##################################
## Vigilance - beta binomial
##################################

vig_res <- simulateResiduals(
  vig_context_model_troop,
  n = 1000
)

plot(vig_res)

par(mfrow = c(1,3))
testUniformity(vig_res)
testDispersion(vig_res)
testZeroInflation(vig_res)

# Additional plots
plotResiduals(vig_res, form = vig_context$context)



par(mfrow = c(1,1))
# Model summary
summary(vig_context_model_troop)

# Random-effect variance
VarCorr(vig_context_model_troop)

# Distribution of vigilance proportions
summary(vig_context$duration_per_focal / 900)

# Number of zero-vigilance focals
sum(vig_context$duration_per_focal == 0)

# Residuals by troop
plotResiduals(
  vig_res,
  form = factor(vig_context$troop)
)

# beta-binomial meeting

vig_context_model_disp <- glmmTMB(
  cbind(duration_per_focal, 900 - duration_per_focal) ~
    context + (1 | troop) + (1 | focal_id),
  dispformula = ~ context,
  family = betabinomial(link = "logit"),
  data = vig_context
)

summary(vig_context_model_disp)


AIC(
  vig_context_model_troop,
  vig_context_model_disp
)

anova(
  vig_context_model_troop,
  vig_context_model_disp
)


vig_disp_res <- simulateResiduals(
  vig_context_model_disp,
  n = 1000
)

plot(vig_disp_res)

par(mfrow=c(1,3))
testUniformity(vig_disp_res)
testDispersion(vig_disp_res)
testZeroInflation(vig_disp_res)
par(mfrow=c(1,1))

plotResiduals(
  vig_disp_res,
  form = factor(vig_context$troop)
)


# Examine residuals by individual
plotResiduals(
  vig_disp_res,
  form = factor(vig_context$focal_id)
)


vig_context %>%
  filter(troop %in% c("AAS", "TLACJ")) %>%
  group_by(troop, focal_id) %>%
  summarise(
    n_focals = n(),
    mean_vig_min = mean(duration_per_focal) / 60,
    median_vig_min = median(duration_per_focal) / 60,
    pct_zero = mean(duration_per_focal == 0) * 100,
    max_vig_min = max(duration_per_focal) / 60,
    .groups = "drop"
  )


# Simulate 1000 datasets from the fitted model
set.seed(123)

sim_vig <- simulate(
  vig_context_model_disp,
  nsim = 1000
)

# Identify ASH's observations
ash_rows <- vig_context$focal_id == "ASH"

# Observed number of zero-vigilance focals
observed_zeros <- sum(
  vig_context$duration_per_focal[ash_rows] == 0
)

# Simulated zero-vigilance counts for ASH
simulated_zeros <- sapply(
  sim_vig,
  function(x) {
    sum(x[ash_rows, 1] == 0)
  }
)

# Compare observed and simulated
observed_zeros
mean(simulated_zeros)
quantile(simulated_zeros, c(0.025, 0.975))

# Visualize
hist(
  simulated_zeros,
  breaks = seq(
    min(simulated_zeros) - 0.5,
    max(simulated_zeros) + 0.5,
    by = 1
  ),
  main = "Zero-vigilance focals: ASH",
  xlab = "Number of zero-vigilance focals"
)

abline(
  v = observed_zeros,
  col = "red",
  lwd = 2
)



##################################
## Yawning — negative binomial GLMM: LOOKS GOOD!
##################################

set.seed(123)

yawn_res <- simulateResiduals(
  yawn_context_model_troop,
  n = 1000
)

plot(yawn_res)

par(mfrow=c(1,3))
testUniformity(yawn_res)
testDispersion(yawn_res)
testZeroInflation(yawn_res)
par(mfrow=c(1,1))

# Residuals by troop
plotResiduals(
  yawn_res,
  form = factor(yawn_context$troop)
)

##################################
## Directional grooming — tweedie GLMM: LOOKS GOOD!
##################################


set.seed(123)

groom_res <- simulateResiduals(
  directional_grooming_model_troop,
  n = 1000
)

plot(groom_res)

par(mfrow=c(1,3))
testUniformity(groom_res)
testDispersion(groom_res)
testZeroInflation(groom_res)
par(mfrow=c(1,1))

# Residuals by troop
plotResiduals(
  groom_res,
  form = factor(directional_grooming$troop)
)


##################################
## fGC — linear mixed-effects model
##################################


# Residuals vs fitted
plot(
  fgc_context_model_troop,
  main = "fGC: Residuals vs Fitted"
)

# Normal Q-Q plot
qqnorm(
  residuals(fgc_context_model_troop),
  main = "fGC: Normal Q-Q"
)
qqline(
  residuals(fgc_context_model_troop),
  col = "red"
)

# Check for singularity
lme4::isSingular(fgc_context_model_troop)

# Random-effect variances
VarCorr(fgc_context_model_troop)


##### SIMPLE MODEL (NO TROOP)
summary(fgc_context_model_simple)

lme4::isSingular(fgc_context_model_simple)

VarCorr(fgc_context_model_simple)


# Residuals vs fitted
plot(fgc_context_model_simple)

# Normal Q-Q plot
qqnorm(residuals(fgc_context_model_simple))
qqline(residuals(fgc_context_model_simple), col = "red")

# Compare fixed-effect estimates
fixef(fgc_context_model_troop)
fixef(fgc_context_model_simple)

