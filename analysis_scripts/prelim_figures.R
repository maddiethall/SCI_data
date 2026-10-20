library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)


######################################################
# vigilance, yawning, and fgc x context/troop

figure1_data <- individual_analysis %>%
  select(
    focal_id,
    troop,
    context,
    vigilance_min_hr,
    yawn_events_hr,
    median_fgc
  ) %>%
  pivot_longer(
    cols = c(vigilance_min_hr, yawn_events_hr, median_fgc),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    context = factor(
      context,
      levels = c("Captive", "Free-ranging")
    ),
    measure = factor(
      measure,
      levels = c(
        "vigilance_min_hr",
        "yawn_events_hr",
        "median_fgc"
      ),
      labels = c(
        "Vigilance (min/hr)",
        "Yawning (events/hr)",
        "Median fGC (ng/g)"
      )
    )
  )


figure1_plot <- ggplot(
  figure1_data,
  aes(x = context, y = value)
) +
  geom_boxplot(
    width = 0.45,
    fill = "gray95",
    color = "gray35",
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = troop),
    width = 0.07,
    height = 0,
    size = 2.8,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "AAS" = "#D45A91",
      "TLACJ" = "#8E5AB5",
      "East Road" = "#257D9D",
      "Windmill" = "#39A5A0",
      "Yankee" = "#344E91"
    ),
    breaks = c(
      "AAS", "TLACJ",
      "East Road", "Windmill", "Yankee"
    )
  ) +
  facet_wrap(
    ~ measure,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = "Context",
    shape = "Troop"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = grid::unit(1.5, "lines"),
    axis.text.x = element_text(size = 9)
  )

significance_labels_1 <- figure1_data %>%
  group_by(measure) %>%
  summarise(
    y.position = max(value, na.rm = TRUE) * 1.12,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "Captive",
    group2 = "Free-ranging",
    label = case_when(
      measure == "Vigilance (min/hr)" ~ "**",
      measure == "Yawning (events/hr)" ~ "*",
      measure == "Median fGC (ng/g)" ~ "***"
    )
  )

figure1_plot_sig <- figure1_plot +
  ggpubr::stat_pvalue_manual(
    significance_labels_1,
    label = "label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.02,
    bracket.size = 0.5,
    size = 5,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20)))

figure1_plot_sig

######################################################
# affiliative behaviors x context/troop

figure2_data <- individual_analysis %>%
  select(
    focal_id,
    troop,
    context,
    directional_grooming_sec_per_focal,
    mutual_groom_sec_per_focal,
    contact_sec_per_focal
  ) %>%
  pivot_longer(
    cols = c(directional_grooming_sec_per_focal, mutual_groom_sec_per_focal, contact_sec_per_focal),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    context = factor(
      context,
      levels = c("Captive", "Free-ranging")
    ),
    measure = factor(
      measure,
      levels = c(
        "directional_grooming_sec_per_focal",
        "mutual_groom_sec_per_focal",
        "contact_sec_per_focal"
      ),
      labels = c(
        "Directional Grooming",
        "Mutual Grooming",
        "Contact Sitting"
      )
    )
  )

figure2_plot <- ggplot(
  figure2_data,
  aes(x = context, y = value)
) +
  geom_boxplot(
    width = 0.45,
    fill = "gray95",
    color = "gray35",
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = troop),
    width = 0.10,
    height = 0,
    size = 2.8,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "AAS" = "#D45A91",
      "TLACJ" = "#8E5AB5",
      "East Road" = "#257D9D",
      "Windmill" = "#39A5A0",
      "Yankee" = "#344E91"
    ),
    breaks = c(
      "AAS", "TLACJ",
      "East Road", "Windmill", "Yankee"
    )
  ) +
  facet_wrap(
    ~ measure,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = "Context",
    shape = "Troop"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = grid::unit(1.5, "lines"),
    axis.text.x = element_text(size = 9)
  )

figure2_sig <- figure2_data %>%
  filter(measure == "Directional Grooming") %>%
  summarise(
    y.position = max(value, na.rm = TRUE) * 1.12
  ) %>%
  mutate(
    measure = "Directional Grooming",
    group1 = "Captive",
    group2 = "Free-ranging",
    label = "**"
  )

figure2_plot_sig <- figure2_plot +
  ggpubr::stat_pvalue_manual(
    figure2_sig,
    label = "label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.02,
    bracket.size = 0.5,
    size = 5,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.20))
  )

figure2_plot_final <- ggdraw() +
  draw_plot(
    figure2_plot_sig,
    x = 0.06, y = 0.15,
    width = 0.90, height = 0.80
  ) +
  draw_label(
    "Seconds/hour",
    x = 0.05, y = 0.55,
    angle = 90,
    size = 12)
  
figure2_plot_final

######################################################
# individual variation in vigilance

vigilance_plot_data <- behavior_by_focal_complete %>%
  filter(behavior == "Vigilance") %>%
  mutate(
    vigilance_pct = proportion_per_focal * 100
  )

vigilance_plot_data <- vigilance_plot_data %>%
  mutate(
    troop = factor(
      troop,
      levels = c(
        "AAS",
        "TLACJ",
        "East Road",
        "Windmill",
        "Yankee"
      )
    )
  )

vigilance_plot <- ggplot(
  vigilance_plot_data,
  aes(x = focal_id, y = vigilance_pct)
) +
  geom_boxplot(
    width = 0.55,
    fill = "gray95",
    color = "gray40",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    height = 0,
    size = 0.8,
    alpha = 0.18
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymin = after_stat(y), ymax = after_stat(y)),
    width = 0.5,
    color = "#344E91",
    linewidth = 1,
    linetype = "21"
  ) +
  facet_grid(
    ~ troop,
    scales = "free_x",
    space = "free_x"
  ) +
  labs(
    x = element_blank(),
    y = "Time spent vigilant (%)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing = grid::unit(1, "lines"),
    axis.text.x = element_text(size = 9)
  )

vigilance_plot

######################################################
# social relationships
library(igraph)
library(ggraph)
library(patchwork)
library(grid)
library(cowplot)


all_grooming_edges <- dyadic_data_complete %>%
  pivot_longer(
    cols = starts_with("grooming_duration_"),
    names_to = "direction",
    values_to = "grooming_seconds_per_hour"
  ) %>%
  mutate(
    from = if_else(
      direction == "grooming_duration_1_to_2_per_hour",
      individual_1, individual_2
    ),
    to = if_else(
      direction == "grooming_duration_1_to_2_per_hour",
      individual_2, individual_1
    ),
    troop = factor(
      troop,
      levels = c("East Road", "Windmill", "Yankee", "AAS", "TLACJ")
    )
  ) %>%
  select(troop, from, to, grooming_seconds_per_hour)
all_diagonal_cells <- all_grooming_edges %>%
  select(troop, individual = from) %>%
  distinct() %>%
  mutate(
    from = individual,
    to = individual
  )


make_grooming_heatmap <- function(troop_name) {
  plot_data <- all_grooming_edges %>%
    filter(troop == troop_name)
  individuals <- sort(unique(c(plot_data$from, plot_data$to)))
  plot_data <- plot_data %>%
    mutate(
      from = factor(from, levels = rev(individuals)),
      to = factor(to, levels = individuals)
    )
  diagonal_data <- tibble(
    from = factor(individuals, levels = rev(individuals)),
    to = factor(individuals, levels = individuals)
  )
  ggplot(plot_data, aes(x = to, y = from)) +
    geom_tile(
      data = diagonal_data,
      aes(x = to, y = from),
      inherit.aes = FALSE,
      fill = "gray90",
      color = "white"
    ) +
    geom_tile(
      aes(fill = grooming_seconds_per_hour),
      color = "white"
    ) +
    geom_text(
      aes(label = round(grooming_seconds_per_hour, 1)),
      size = 3
    ) +
    scale_fill_gradient(
      low = "#F8EAF3",
      high = "#9B4D96",
      limits = c(0, 75),
      name = "Seconds/hour"
    ) +
    coord_equal() +
    labs(
      title = troop_name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}

grooming_heatmaps <- lapply(
  c("East Road", "Windmill", "Yankee", "AAS", "TLACJ"),
  make_grooming_heatmap
)

# Extract the shared legend from one heatmap
legend_plot <- grooming_heatmaps[[1]] +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(4, "cm"),
      barheight = grid::unit(0.3, "cm")
    )
  )

shared_legend <- get_legend(legend_plot)

# Remove legends and axis titles from the five heatmaps
clean_heatmaps <- lapply(grooming_heatmaps, function(p) {
  p +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none")
})

# Arrange the five heatmaps in one row
heatmap_row <- plot_grid(
  plotlist = c(clean_heatmaps, list(NULL)),
  ncol = 3,
  align = "hv"
)

# Assemble everything using manual positions
final_grooming_figure <- ggdraw() +
  draw_plot(
    heatmap_row,
    x = 0.06, y = 0.15,
    width = 0.90, height = 0.80
  ) +
  draw_label(
    "Groomer",
    x = 0.05, y = 0.55,
    angle = 90,
    size = 12
  ) +
  draw_label(
    "Recipient",
    x = 0.50, y = 0.1,
    size = 12
  ) +
  draw_grob(
    shared_legend,
    x = 0.68, y = 0.25,
    width = 0.30, height = 0.10
  )


final_grooming_figure


