source("data/data_collection.R")

#### Interactive Effects Analysis ####

## QB Predictive Interaction Analysis
# Create QB prediction dataset with interactions (Year N → Year N+1)
qb_prediction_interaction_data <- combined_passing_analysis %>%
  filter(attempts >= 120,
         !is.na(expected_completion_percentage),
         !is.na(completion_percentage_above_expectation),
         !is.na(max_completed_air_distance),
         !is.na(aggressiveness)) %>%
  select(player_gsis_id, player_display_name, season, age, years_exp,
         expected_completion_percentage, completion_percentage_above_expectation,
         max_completed_air_distance, aggressiveness, avg_air_yards_differential,
         passing_epa_per_play, attempts, avg_intended_air_yards) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  # Create next year's EPA as target variable
  mutate(
    next_year_epa_per_play = lead(passing_epa_per_play, 1),
    next_year_attempts = lead(attempts, 1)
  ) %>%
  ungroup() %>%
  # Only keep rows where we have next year data
  filter(!is.na(next_year_epa_per_play), next_year_attempts >= 100) %>%
  rename(current_year = season)

cat("QB Predictive Interaction dataset:", nrow(qb_prediction_interaction_data), "player-season pairs\n")

# Test predictive combinations (Year N NGS → Year N+1 EPA)
qb_predictive_combinations <- list(
  "Accuracy Foundation + Arm Talent" = c("expected_completion_percentage", "max_completed_air_distance"),
  "Dual Accuracy Metrics" = c("expected_completion_percentage", "completion_percentage_above_expectation"),  
  "Accuracy + Controlled Aggression" = c("expected_completion_percentage", "aggressiveness"),
  "Arm Talent + Controlled Aggression" = c("max_completed_air_distance", "aggressiveness"),
  "Foundation + Efficiency" = c("expected_completion_percentage", "avg_air_yards_differential")
)

qb_predictive_combo_results <- map_dfr(names(qb_predictive_combinations), ~{
  combo_name <- .x
  metrics <- qb_predictive_combinations[[.x]]
  
  # Create composite score (standardized sum)
  combo_data <- qb_prediction_interaction_data %>%
    mutate(
      # For aggressiveness, flip the z-score since negative correlation means lower is better for future prediction
      metric1_z = if(metrics[1] == "aggressiveness") -scale(!!sym(metrics[1]))[,1] else scale(!!sym(metrics[1]))[,1],
      metric2_z = if(metrics[2] == "aggressiveness") -scale(!!sym(metrics[2]))[,1] else scale(!!sym(metrics[2]))[,1],
      combo_score = metric1_z + metric2_z
    )
  
  # Calculate predictive correlations (Current NGS → Next Year EPA)
  individual_cor1 <- cor(combo_data[[metrics[1]]], combo_data$next_year_epa_per_play, use = "complete.obs")
  individual_cor2 <- cor(combo_data[[metrics[2]]], combo_data$next_year_epa_per_play, use = "complete.obs")
  combo_cor <- cor(combo_data$combo_score, combo_data$next_year_epa_per_play, use = "complete.obs")
  
  # Calculate synergy (combo correlation vs sum of individual correlations)
  expected_combo <- (abs(individual_cor1) + abs(individual_cor2)) / 2
  synergy_effect <- abs(combo_cor) - expected_combo
  
  tibble(
    combination = combo_name,
    metric1 = metrics[1],
    metric2 = metrics[2], 
    cor1_future = individual_cor1,
    cor2_future = individual_cor2,
    combo_correlation_future = combo_cor,
    expected_correlation = expected_combo,
    synergy_effect = synergy_effect
  )
}) %>%
  arrange(desc(abs(combo_correlation_future)))

cat("QB PREDICTIVE METRIC COMBINATION ANALYSIS (Year N → Year N+1):\n")
print(qb_predictive_combo_results)

# Best QB predictive combination visualization
best_qb_predictive_combo <- qb_predictive_combo_results$combination[1]
best_qb_predictive_metrics <- qb_predictive_combinations[[best_qb_predictive_combo]]

qb_best_predictive_combo_data <- qb_prediction_interaction_data %>%
  mutate(
    metric1_z = if(best_qb_predictive_metrics[1] == "aggressiveness") -scale(!!sym(best_qb_predictive_metrics[1]))[,1] else scale(!!sym(best_qb_predictive_metrics[1]))[,1],
    metric2_z = if(best_qb_predictive_metrics[2] == "aggressiveness") -scale(!!sym(best_qb_predictive_metrics[2]))[,1] else scale(!!sym(best_qb_predictive_metrics[2]))[,1],
    combo_score = metric1_z + metric2_z,
    combo_tier = case_when(
      combo_score >= quantile(combo_score, 0.8, na.rm = TRUE) ~ "Elite Combo",
      combo_score >= quantile(combo_score, 0.6, na.rm = TRUE) ~ "Good Combo", 
      combo_score >= quantile(combo_score, 0.4, na.rm = TRUE) ~ "Average Combo",
      combo_score >= quantile(combo_score, 0.2, na.rm = TRUE) ~ "Below Avg Combo",
      TRUE ~ "Poor Combo"
    ),
    combo_tier = factor(combo_tier, levels = c("Elite Combo", "Good Combo", "Average Combo", "Below Avg Combo", "Poor Combo"))
  )

qb_predictive_interaction_plot <- ggplot(qb_best_predictive_combo_data, aes(x = !!sym(best_qb_predictive_metrics[1]), y = !!sym(best_qb_predictive_metrics[2]))) +
  geom_point(aes(color = next_year_epa_per_play, size = combo_score), alpha = 0.7) +
  scale_color_viridis_c(name = "Next Year\nEPA/Play") +
  scale_size_continuous(name = "Combo Score") +
  labs(title = paste("QB Best Predictive Combination:", best_qb_predictive_combo),
       subtitle = paste("Predicts Next Year EPA (r =", round(qb_predictive_combo_results$combo_correlation_future[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_qb_predictive_metrics[1])),
       y = str_to_title(gsub("_", " ", best_qb_predictive_metrics[2]))) +
  theme_minimal()

print(qb_predictive_interaction_plot)

# QB Predictive combination performance by tier
qb_predictive_combo_tier_analysis <- qb_best_predictive_combo_data %>%
  group_by(combo_tier) %>%
  summarise(
    count = n(),
    avg_next_year_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    avg_current_epa = mean(passing_epa_per_play, na.rm = TRUE),
    consistency = cor(passing_epa_per_play, next_year_epa_per_play, use = "complete.obs"),
    avg_metric1 = mean(!!sym(best_qb_predictive_metrics[1]), na.rm = TRUE),
    avg_metric2 = mean(!!sym(best_qb_predictive_metrics[2]), na.rm = TRUE),
    .groups = "drop"
  )

cat("\nQB Best Predictive Combination Performance by Tier:\n")
print(qb_predictive_combo_tier_analysis)

## RB Predictive Interaction Analysis
rb_prediction_interaction_data <- combined_rushing_analysis %>%
  filter(rush_attempts >= 85,
         !is.na(efficiency),
         !is.na(rush_yards_over_expected_per_att)) %>%
  select(player_gsis_id, player_display_name, season, age, years_exp,
         efficiency, rush_yards_over_expected_per_att, rush_pct_over_expected,
         expected_rush_yards, avg_time_to_los, percent_attempts_gte_eight_defenders,
         rushing_epa_per_play, rush_attempts) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  mutate(
    next_year_epa_per_play = lead(rushing_epa_per_play, 1),
    next_year_attempts = lead(rush_attempts, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(next_year_epa_per_play), next_year_attempts >= 50) %>%
  rename(current_year = season)

cat("RB Predictive Interaction dataset:", nrow(rb_prediction_interaction_data), "player-season pairs\n")

# Test RB predictive combinations
rb_predictive_combinations <- list(
  "Performance + Directness" = c("rush_yards_over_expected_per_att", "efficiency"),
  "Performance + Consistency" = c("rush_yards_over_expected_per_att", "rush_pct_over_expected"),
  "Directness + Patience" = c("efficiency", "avg_time_to_los"),
  "Performance + Difficulty" = c("rush_yards_over_expected_per_att", "percent_attempts_gte_eight_defenders"),
  "Directness + Difficulty" = c("efficiency", "percent_attempts_gte_eight_defenders")
)

rb_predictive_combo_results <- map_dfr(names(rb_predictive_combinations), ~{
  combo_name <- .x
  metrics <- rb_predictive_combinations[[.x]]
  
  combo_data <- rb_prediction_interaction_data %>%
    mutate(
      # For efficiency, flip the z-score since lower is better
      metric1_z = if(metrics[1] == "efficiency") -scale(!!sym(metrics[1]))[,1] else scale(!!sym(metrics[1]))[,1],
      metric2_z = if(metrics[2] == "efficiency") -scale(!!sym(metrics[2]))[,1] else scale(!!sym(metrics[2]))[,1],
      combo_score = metric1_z + metric2_z
    )
  
  individual_cor1 <- cor(combo_data[[metrics[1]]], combo_data$next_year_epa_per_play, use = "complete.obs")
  individual_cor2 <- cor(combo_data[[metrics[2]]], combo_data$next_year_epa_per_play, use = "complete.obs") 
  combo_cor <- cor(combo_data$combo_score, combo_data$next_year_epa_per_play, use = "complete.obs")
  
  expected_combo <- (abs(individual_cor1) + abs(individual_cor2)) / 2
  synergy_effect <- abs(combo_cor) - expected_combo
  
  tibble(
    combination = combo_name,
    metric1 = metrics[1],
    metric2 = metrics[2],
    cor1_future = individual_cor1,
    cor2_future = individual_cor2, 
    combo_correlation_future = combo_cor,
    expected_correlation = expected_combo,
    synergy_effect = synergy_effect
  )
}) %>%
  arrange(desc(abs(combo_correlation_future)))

cat("RB PREDICTIVE METRIC COMBINATION ANALYSIS (Year N → Year N+1):\n")
print(rb_predictive_combo_results)

# Best RB predictive combination visualization
best_rb_predictive_combo <- rb_predictive_combo_results$combination[1]
best_rb_predictive_metrics <- rb_predictive_combinations[[best_rb_predictive_combo]]

rb_best_predictive_combo_data <- rb_prediction_interaction_data %>%
  mutate(
    metric1_z = if(best_rb_predictive_metrics[1] == "efficiency") -scale(!!sym(best_rb_predictive_metrics[1]))[,1] else scale(!!sym(best_rb_predictive_metrics[1]))[,1],
    metric2_z = if(best_rb_predictive_metrics[2] == "efficiency") -scale(!!sym(best_rb_predictive_metrics[2]))[,1] else scale(!!sym(best_rb_predictive_metrics[2]))[,1],
    combo_score = metric1_z + metric2_z
  )

rb_predictive_interaction_plot <- ggplot(rb_best_predictive_combo_data, aes(x = !!sym(best_rb_predictive_metrics[1]), y = !!sym(best_rb_predictive_metrics[2]))) +
  geom_point(aes(color = next_year_epa_per_play, size = combo_score), alpha = 0.7) +
  scale_color_viridis_c(name = "Next Year\nEPA/Play") +
  scale_size_continuous(name = "Combo Score") +
  labs(title = paste("RB Best Predictive Combination:", best_rb_predictive_combo),
       subtitle = paste("Predicts Next Year EPA (r =", round(rb_predictive_combo_results$combo_correlation_future[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_rb_predictive_metrics[1])),
       y = str_to_title(gsub("_", " ", best_rb_predictive_metrics[2]))) +
  theme_minimal()

print(rb_predictive_interaction_plot)

# RB Predictive combination performance by tier
rb_predictive_combo_tier_analysis <- rb_best_predictive_combo_data %>%
  mutate(
    combo_tier = case_when(
      combo_score >= quantile(combo_score, 0.8, na.rm = TRUE) ~ "Elite Combo",
      combo_score >= quantile(combo_score, 0.6, na.rm = TRUE) ~ "Good Combo", 
      combo_score >= quantile(combo_score, 0.4, na.rm = TRUE) ~ "Average Combo",
      combo_score >= quantile(combo_score, 0.2, na.rm = TRUE) ~ "Below Avg Combo",
      TRUE ~ "Poor Combo"
    ),
    combo_tier = factor(combo_tier, levels = c("Elite Combo", "Good Combo", "Average Combo", "Below Avg Combo", "Poor Combo"))
  ) %>%
  group_by(combo_tier) %>%
  summarise(
    count = n(),
    avg_next_year_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    avg_current_epa = mean(rushing_epa_per_play, na.rm = TRUE),
    consistency = cor(rushing_epa_per_play, next_year_epa_per_play, use = "complete.obs"),
    avg_metric1 = mean(!!sym(best_rb_predictive_metrics[1]), na.rm = TRUE),
    avg_metric2 = mean(!!sym(best_rb_predictive_metrics[2]), na.rm = TRUE),
    .groups = "drop"
  )

cat("\nRB Best Predictive Combination Performance by Tier:\n")
print(rb_predictive_combo_tier_analysis)

## WR Predictive Interaction Analysis
wr_prediction_interaction_data <- combined_receiving_analysis %>%
  filter(targets >= 40,
         !is.na(avg_yac_above_expectation),
         !is.na(percent_share_of_intended_air_yards)) %>%
  select(player_gsis_id, player_display_name, season, age, years_exp,
         avg_yac_above_expectation, percent_share_of_intended_air_yards, avg_intended_air_yards,
         avg_yac, catch_percentage, avg_expected_yac,
         avg_separation, avg_cushion,
         receiving_epa_per_target, targets) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  mutate(
    next_year_epa_per_target = lead(receiving_epa_per_target, 1),
    next_year_targets = lead(targets, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(next_year_epa_per_target), next_year_targets >= 30) %>%
  rename(current_year = season)

cat("WR Predictive Interaction dataset:", nrow(wr_prediction_interaction_data), "player-season pairs\n")

# Test WR predictive combinations
wr_predictive_combinations <- list(
  "Playmaking + Role" = c("avg_yac_above_expectation", "percent_share_of_intended_air_yards"),
  "YAC Ability + Target Depth" = c("avg_yac_above_expectation", "avg_intended_air_yards"),
  "Role + Target Depth" = c("percent_share_of_intended_air_yards", "avg_intended_air_yards"),
  "Playmaking + Raw YAC" = c("avg_yac_above_expectation", "avg_yac"),
  "YAC + Reliability" = c("avg_yac_above_expectation", "catch_percentage")
)

wr_predictive_combo_results <- map_dfr(names(wr_predictive_combinations), ~{
  combo_name <- .x
  metrics <- wr_predictive_combinations[[.x]]
  
  combo_data <- wr_prediction_interaction_data %>%
    mutate(
      metric1_z = scale(!!sym(metrics[1]))[,1],
      metric2_z = scale(!!sym(metrics[2]))[,1], 
      combo_score = metric1_z + metric2_z
    )
  
  individual_cor1 <- cor(combo_data[[metrics[1]]], combo_data$next_year_epa_per_target, use = "complete.obs")
  individual_cor2 <- cor(combo_data[[metrics[2]]], combo_data$next_year_epa_per_target, use = "complete.obs")
  combo_cor <- cor(combo_data$combo_score, combo_data$next_year_epa_per_target, use = "complete.obs")
  
  expected_combo <- (abs(individual_cor1) + abs(individual_cor2)) / 2
  synergy_effect <- abs(combo_cor) - expected_combo
  
  tibble(
    combination = combo_name,
    metric1 = metrics[1],
    metric2 = metrics[2],
    cor1_future = individual_cor1,
    cor2_future = individual_cor2,
    combo_correlation_future = combo_cor, 
    expected_correlation = expected_combo,
    synergy_effect = synergy_effect
  )
}) %>%
  arrange(desc(abs(combo_correlation_future)))

cat("WR PREDICTIVE METRIC COMBINATION ANALYSIS (Year N → Year N+1):\n")
print(wr_predictive_combo_results)

# Best WR predictive combination visualization
best_wr_predictive_combo <- wr_predictive_combo_results$combination[1] 
best_wr_predictive_metrics <- wr_predictive_combinations[[best_wr_predictive_combo]]

wr_best_predictive_combo_data <- wr_prediction_interaction_data %>%
  mutate(
    metric1_z = scale(!!sym(best_wr_predictive_metrics[1]))[,1],
    metric2_z = scale(!!sym(best_wr_predictive_metrics[2]))[,1],
    combo_score = metric1_z + metric2_z,
    combo_tier = case_when(
      combo_score >= quantile(combo_score, 0.8, na.rm = TRUE) ~ "Elite Combo",
      combo_score >= quantile(combo_score, 0.6, na.rm = TRUE) ~ "Good Combo", 
      combo_score >= quantile(combo_score, 0.4, na.rm = TRUE) ~ "Average Combo",
      combo_score >= quantile(combo_score, 0.2, na.rm = TRUE) ~ "Below Avg Combo",
      TRUE ~ "Poor Combo"
    ),
    combo_tier = factor(combo_tier, levels = c("Elite Combo", "Good Combo", "Average Combo", "Below Avg Combo", "Poor Combo"))
  )

wr_predictive_interaction_plot <- ggplot(wr_best_predictive_combo_data, aes(x = !!sym(best_wr_predictive_metrics[1]), y = !!sym(best_wr_predictive_metrics[2]))) +
  geom_point(aes(color = next_year_epa_per_target, size = combo_score), alpha = 0.7) +
  scale_color_viridis_c(name = "Next Year\nEPA/Target") +
  scale_size_continuous(name = "Combo Score") +
  labs(title = paste("WR Best Predictive Combination:", best_wr_predictive_combo),
       subtitle = paste("Predicts Next Year EPA (r =", round(wr_predictive_combo_results$combo_correlation_future[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_wr_predictive_metrics[1])),
       y = str_to_title(gsub("_", " ", best_wr_predictive_metrics[2]))) +
  theme_minimal()

print(wr_predictive_interaction_plot)

wr_combo_tier_analysis <- wr_best_predictive_combo_data %>%
  group_by(combo_tier) %>%
  summarise(
    count = n(),
    avg_next_year_epa = mean(next_year_epa_per_target, na.rm = TRUE),
    avg_current_epa = mean(receiving_epa_per_target, na.rm = TRUE),
    consistency = cor(receiving_epa_per_target, next_year_epa_per_target, use = "complete.obs"),
    avg_metric1 = mean(!!sym(best_wr_predictive_metrics[1]), na.rm = TRUE),
    avg_metric2 = mean(!!sym(best_wr_predictive_metrics[2]), na.rm = TRUE),
    .groups = "drop"
  )
cat("\nWR Best Combination Performance by Tier:\n")
print(wr_combo_tier_analysis)

## 3 metric combos
qb_3metric_data <- qb_prediction_interaction_data %>%
  filter(!is.na(expected_completion_percentage), !is.na(completion_percentage_above_expectation), !is.na(max_completed_air_distance)) %>%
  mutate(
    # Standardize all metrics (flip efficiency since lower = better)
    exp_comp_z = scale(expected_completion_percentage)[,1],  # Flip for directness
    cpoe_z = scale(completion_percentage_above_expectation)[,1],
    mcad_z = scale(max_completed_air_distance)[,1],
    
    # Create 3-metric composite
    qb_3metric_combo = exp_comp_z + cpoe_z + mcad_z,
    
    # Also create 2-metric comparison (best 2-metric combo from earlier)
    qb_2metric_combo = exp_comp_z + cpoe_z
  )

# Calculate correlations for comparison
qb_individual_cors <- tibble(
  metric = c("expected_completion_percentage", "completion_percentage_above_expectation", "max_completed_air_distance"),
  correlation = c(
    cor(qb_3metric_data$expected_completion_percentage, qb_3metric_data$next_year_epa_per_play, use = "complete.obs"),
    cor(qb_3metric_data$completion_percentage_above_expectation, qb_3metric_data$next_year_epa_per_play, use = "complete.obs"),
    cor(qb_3metric_data$max_completed_air_distance, qb_3metric_data$next_year_epa_per_play, use = "complete.obs")
  )
) %>%
  arrange(desc(abs(correlation)))

qb_2metric_cor <- cor(qb_3metric_data$qb_2metric_combo, qb_3metric_data$next_year_epa_per_play, use = "complete.obs")
qb_3metric_cor <- cor(qb_3metric_data$qb_3metric_combo, qb_3metric_data$next_year_epa_per_play, use = "complete.obs")

cat("Individual Metric Correlations with Next Year EPA:\n")
print(qb_individual_cors)
cat("\n2-Metric Combo Correlation:", round(qb_2metric_cor, 3), "\n")
cat("3-Metric Combo Correlation:", round(qb_3metric_cor, 3), "\n")
cat("3-Metric Improvement:", round(abs(qb_3metric_cor) - abs(qb_2metric_cor), 3), "\n")

# qb 3-metric visualization
qb_3metric_plot <- qb_3metric_data %>%
  mutate(combo_tier = case_when(
    qb_3metric_combo >= quantile(qb_3metric_combo, 0.8, na.rm = TRUE) ~ "Elite",
    qb_3metric_combo >= quantile(qb_3metric_combo, 0.6, na.rm = TRUE) ~ "Good",
    qb_3metric_combo >= quantile(qb_3metric_combo, 0.4, na.rm = TRUE) ~ "Average",
    qb_3metric_combo >= quantile(qb_3metric_combo, 0.2, na.rm = TRUE) ~ "Below Avg",
    TRUE ~ "Poor"
  )) %>%
  ggplot(aes(x = qb_3metric_combo, y = next_year_epa_per_play)) +
  geom_point(aes(color = combo_tier), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_color_viridis_d() +
  labs(title = "QB 3-Metric Combo: xCOMP% + CPOE + LCAD",
       subtitle = paste("Predicts Next Year EPA (r =", round(qb_3metric_cor, 3), ")"),
       x = "3-Metric Composite Score",
       y = "Next Year EPA per Play",
       color = "Combo Tier") +
  theme_minimal()

print(qb_3metric_plot)

# RB
rb_3metric_data <- rb_prediction_interaction_data %>%
  filter(!is.na(efficiency), !is.na(avg_time_to_los), !is.na(rush_yards_over_expected_per_att)) %>%
  mutate(
    # Standardize all metrics (flip efficiency since lower = better)
    efficiency_z = -scale(efficiency)[,1],  # Flip for directness
    time_to_los_z = scale(avg_time_to_los)[,1],
    ryoe_att_z = scale(rush_yards_over_expected_per_att)[,1],
    
    # Create 3-metric composite
    rb_3metric_combo = efficiency_z + time_to_los_z + ryoe_att_z,
    
    # Also create 2-metric comparison (best 2-metric combo from earlier)
    rb_2metric_combo = efficiency_z + time_to_los_z
  )

# Calculate correlations for comparison
rb_individual_cors <- tibble(
  metric = c("efficiency", "avg_time_to_los", "rush_yards_over_expected_per_att"),
  correlation = c(
    cor(rb_3metric_data$efficiency, rb_3metric_data$next_year_epa_per_play, use = "complete.obs"),
    cor(rb_3metric_data$avg_time_to_los, rb_3metric_data$next_year_epa_per_play, use = "complete.obs"),
    cor(rb_3metric_data$rush_yards_over_expected_per_att, rb_3metric_data$next_year_epa_per_play, use = "complete.obs")
  )
) %>%
  arrange(desc(abs(correlation)))

rb_2metric_cor <- cor(rb_3metric_data$rb_2metric_combo, rb_3metric_data$next_year_epa_per_play, use = "complete.obs")
rb_3metric_cor <- cor(rb_3metric_data$rb_3metric_combo, rb_3metric_data$next_year_epa_per_play, use = "complete.obs")

cat("Individual Metric Correlations with Next Year EPA:\n")
print(rb_individual_cors)
cat("\n2-Metric Combo Correlation:", round(rb_2metric_cor, 3), "\n")
cat("3-Metric Combo Correlation:", round(rb_3metric_cor, 3), "\n")
cat("3-Metric Improvement:", round(abs(rb_3metric_cor) - abs(rb_2metric_cor), 3), "\n")

# RB 3-metric visualization
rb_3metric_plot <- rb_3metric_data %>%
  mutate(combo_tier = case_when(
    rb_3metric_combo >= quantile(rb_3metric_combo, 0.8, na.rm = TRUE) ~ "Elite",
    rb_3metric_combo >= quantile(rb_3metric_combo, 0.6, na.rm = TRUE) ~ "Good",
    rb_3metric_combo >= quantile(rb_3metric_combo, 0.4, na.rm = TRUE) ~ "Average",
    rb_3metric_combo >= quantile(rb_3metric_combo, 0.2, na.rm = TRUE) ~ "Below Avg",
    TRUE ~ "Poor"
  )) %>%
  ggplot(aes(x = rb_3metric_combo, y = next_year_epa_per_play)) +
  geom_point(aes(color = combo_tier), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_color_viridis_d() +
  labs(title = "RB 3-Metric Combo: Efficiency + Time to LOS + RYOE/ATT",
       subtitle = paste("Predicts Next Year EPA (r =", round(rb_3metric_cor, 3), ")"),
       x = "3-Metric Composite Score",
       y = "Next Year EPA per Play",
       color = "Combo Tier") +
  theme_minimal()

print(rb_3metric_plot)

# WR 4-Metric Combination: YAC Above Expected + Air Yards Share + Intended Air Yards  
cat("\nWR 4-METRIC COMBINATION: YAC Above Expected + Air Yards Share + Intended Air Yards + Catch Percentage\n")

wr_4metric_data <- wr_prediction_interaction_data %>%
  filter(!is.na(avg_yac_above_expectation), !is.na(percent_share_of_intended_air_yards), !is.na(avg_intended_air_yards)) %>%
  mutate(
    # Standardize all metrics
    yac_above_exp_z = scale(avg_yac_above_expectation)[,1],
    air_yards_share_z = scale(percent_share_of_intended_air_yards)[,1],
    intended_air_yards_z = scale(avg_intended_air_yards)[,1],
    catch_pct_z = scale(catch_percentage)[,1],
    
    # Create 4-metric composite
    wr_4metric_combo = yac_above_exp_z + air_yards_share_z + intended_air_yards_z + catch_pct_z,
    
    # Also create 2-metric comparison (best 2-metric combo from earlier)
    wr_2metric_combo = yac_above_exp_z + air_yards_share_z
  )

# Calculate correlations for comparison
wr_individual_cors <- tibble(
  metric = c("avg_yac_above_expectation", "percent_share_of_intended_air_yards", "avg_intended_air_yards", "catch_percentage"),
  correlation = c(
    cor(wr_4metric_data$avg_yac_above_expectation, wr_4metric_data$next_year_epa_per_target, use = "complete.obs"),
    cor(wr_4metric_data$percent_share_of_intended_air_yards, wr_4metric_data$next_year_epa_per_target, use = "complete.obs"),
    cor(wr_4metric_data$avg_intended_air_yards, wr_4metric_data$next_year_epa_per_target, use = "complete.obs"),
    cor(wr_4metric_data$catch_percentage, wr_4metric_data$next_year_epa_per_target, use = "complete.obs")
  )
)

wr_2metric_cor <- cor(wr_4metric_data$wr_2metric_combo, wr_4metric_data$next_year_epa_per_target, use = "complete.obs")
wr_4metric_cor <- cor(wr_4metric_data$wr_4metric_combo, wr_4metric_data$next_year_epa_per_target, use = "complete.obs")

cat("Individual Metric Correlations with Next Year EPA:\n")
print(wr_individual_cors)
cat("\n2-Metric Combo Correlation:", round(wr_2metric_cor, 3), "\n")
cat("4-Metric Combo Correlation:", round(wr_4metric_cor, 3), "\n")
cat("4-Metric Improvement:", round(abs(wr_4metric_cor) - abs(wr_2metric_cor), 3), "\n")

# WR 4-metric visualization
wr_4metric_plot <- wr_4metric_data %>%
  mutate(combo_tier = case_when(
    wr_4metric_combo >= quantile(wr_4metric_combo, 0.8, na.rm = TRUE) ~ "Elite",
    wr_4metric_combo >= quantile(wr_4metric_combo, 0.6, na.rm = TRUE) ~ "Good",
    wr_4metric_combo >= quantile(wr_4metric_combo, 0.4, na.rm = TRUE) ~ "Average",
    wr_4metric_combo >= quantile(wr_4metric_combo, 0.2, na.rm = TRUE) ~ "Below Avg",
    TRUE ~ "Poor"
  )) %>%
  ggplot(aes(x = wr_4metric_combo, y = next_year_epa_per_target)) +
  geom_point(aes(color = combo_tier), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_color_viridis_d() +
  labs(title = "WR 4-Metric Combo: YAC Above Expected + Air Yards Share + Target Depth + Catch %",
       subtitle = paste("Predicts Next Year EPA (r =", round(wr_4metric_cor, 3), ")"),
       x = "4-Metric Composite Score", 
       y = "Next Year EPA per Target",
       color = "Combo Tier") +
  theme_minimal()

print(wr_4metric_plot)

# Comparison Summary
cat("\n=== MULTI-METRIC vs 2-METRIC COMPARISON SUMMARY ===\n")
cat("QB Analysis:\n")
cat("  2-Metric:", round(qb_2metric_cor, 3), "\n")
cat("  3-Metric:", round(qb_3metric_cor, 3), "\n")
cat("  Improvement:", round(abs(qb_3metric_cor) - abs(qb_2metric_cor), 3), "\n")
cat("  Worth adding 3rd metric:", ifelse(abs(qb_3metric_cor) > abs(qb_2metric_cor) + abs(qb_2metric_cor)*0.05, "YES", "NO"), "\n\n")

cat("RB Analysis:\n")
cat("  2-Metric (Efficiency + RYOE/ATT):", round(rb_2metric_cor, 3), "\n")
cat("  3-Metric (+ Time to LOS):", round(rb_3metric_cor, 3), "\n")
cat("  Improvement:", round(abs(rb_3metric_cor) - abs(rb_2metric_cor), 3), "\n")
cat("  Worth adding 3rd metric:", ifelse(abs(rb_3metric_cor) > abs(rb_2metric_cor) + abs(rb_2metric_cor)*0.05, "YES", "NO"), "\n\n")

cat("WR Analysis:\n")
cat("  2-Metric (YAC Above Expected + Air Yards Share):", round(wr_2metric_cor, 3), "\n")
cat("  4-Metric (+ Target Depth + Catch Pct):", round(wr_4metric_cor, 3), "\n")
cat("  Improvement:", round(abs(wr_4metric_cor) - abs(wr_2metric_cor), 3), "\n")
cat("  Worth adding 3rd + 4th metric:", ifelse(abs(wr_4metric_cor) > abs(wr_2metric_cor) + abs(wr_2metric_cor)*0.05, "YES", "NO"), "\n")