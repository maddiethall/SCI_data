library(dplyr)
library(tidyr)
library(ggplot2)



#turn every dyad into two individual-partner records
dyadic_relationships <- dyadic_data_complete %>%
  mutate(
    close_proximity = proximity_body_contact + proximity_less_2m #proportion of scans
  )

relationship_long <- bind_rows(
  dyadic_relationships %>%
    transmute(
      focal_id = individual_1,
      partner_id = individual_2,
      troop,
      grooming_reciprocity,
      close_proximity,
      grooming_asymmetry
    ),
  
  dyadic_relationships %>%
    transmute(
      focal_id = individual_2,
      partner_id = individual_1,
      troop,
      grooming_reciprocity,
      close_proximity,
      grooming_asymmetry
    )
)

#summarize relationship profiles by female
relationship_individual <- relationship_long %>%
  filter(focal_id %in% individual_analysis$focal_id) %>%
  group_by(focal_id) %>%
  summarise(
    n_partners = n(),
    mean_reciprocity = mean(grooming_reciprocity, na.rm = TRUE),
    mean_close_proximity = mean(close_proximity, na.rm = TRUE),
    mean_asymmetry = mean(grooming_asymmetry, na.rm = TRUE),
    total_proximity = sum(close_proximity, na.rm = TRUE),
    .groups = "drop"
  )

relationship_analysis <- individual_analysis %>%
  left_join(relationship_individual, by = "focal_id") %>%
  select(
    focal_id, troop, context,
    predicted_log_fgc, adjusted_fgc,
    n_partners, mean_reciprocity, mean_close_proximity,
    mean_asymmetry, total_proximity
  )

###

reciprocity_fgc_model <- lm(
  predicted_log_fgc ~ mean_reciprocity + context,
  data = relationship_analysis
)

proximity_fgc_model <- lm(
  predicted_log_fgc ~ mean_close_proximity + context,
  data = relationship_analysis
)

summary(reciprocity_fgc_model)
summary(proximity_fgc_model)

# no associations between reciprocity or proximity and fgc
