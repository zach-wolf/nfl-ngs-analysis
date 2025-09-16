source("data/data_collection.R")

## Stability analysis
# QB Metric Stability
combined_passing_analysis %>%
  filter(attempts >= 100) %>%
  select(player_gsis_id, season, all_of(qb_ngs_vars)) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  # Only keep players with at least 2 consecutive seasons
  filter(n() >= 2) %>%
  # Create next year values for correlation
  mutate(across(all_of(qb_ngs_vars), ~lead(.x, 1), .names = "next_{.col}")) %>%
  # Remove rows without next year data
  filter(!is.na(next_avg_time_to_throw)) %>%
  # Calculate correlations only if we have data
  summarise(
    seasons = n(),
    stability_avg_time_to_throw = 
      ifelse(seasons >= 2 & sum(!is.na(avg_time_to_throw) & !is.na(next_avg_time_to_throw)) >= 2,
             cor(avg_time_to_throw, next_avg_time_to_throw, use = "complete.obs"), NA),
    stability_avg_completed_air_yards = 
      ifelse(seasons >= 2 & sum(!is.na(avg_completed_air_yards) & !is.na(next_avg_completed_air_yards)) >= 2,
             cor(avg_completed_air_yards, next_avg_completed_air_yards, use = "complete.obs"), NA),
    stability_avg_intended_air_yards = 
      ifelse(seasons >= 2 & sum(!is.na(avg_intended_air_yards) & !is.na(next_avg_intended_air_yards)) >= 2,
             cor(avg_intended_air_yards, next_avg_intended_air_yards, use = "complete.obs"), NA),
    stability_avg_air_yards_differential = 
      ifelse(seasons >= 2 & sum(!is.na(avg_air_yards_differential) & !is.na(next_avg_air_yards_differential)) >= 2,
             cor(avg_air_yards_differential, next_avg_air_yards_differential, use = "complete.obs"), NA),
    stability_aggressiveness = 
      ifelse(seasons >= 2 & sum(!is.na(aggressiveness) & !is.na(next_aggressiveness)) >= 2,
             cor(aggressiveness, next_aggressiveness, use = "complete.obs"), NA),
    stability_max_completed_air_distance = 
      ifelse(seasons >= 2 & sum(!is.na(max_completed_air_distance) & !is.na(next_max_completed_air_distance)) >= 2,
             cor(max_completed_air_distance, next_max_completed_air_distance, use = "complete.obs"), NA),
    stability_avg_air_yards_to_sticks = 
      ifelse(seasons >= 2 & sum(!is.na(avg_air_yards_to_sticks) & !is.na(next_avg_air_yards_to_sticks)) >= 2,
             cor(avg_air_yards_to_sticks, next_avg_air_yards_to_sticks, use = "complete.obs"), NA),
    stability_expected_completion_percentage = 
      ifelse(seasons >= 2 & sum(!is.na(expected_completion_percentage) & !is.na(next_expected_completion_percentage)) >= 2,
             cor(expected_completion_percentage, next_expected_completion_percentage, use = "complete.obs"), NA),
    stability_completion_percentage_above_expectation = 
      ifelse(seasons >= 2 & sum(!is.na(completion_percentage_above_expectation) & !is.na(next_completion_percentage_above_expectation)) >= 2,
             cor(completion_percentage_above_expectation, next_completion_percentage_above_expectation, use = "complete.obs"), NA),
    .groups = "drop"
  ) %>%
  # Calculate average stability across all players
  summarise(
    stability_avg_time_to_throw = mean(stability_avg_time_to_throw, na.rm = TRUE),
    stability_avg_completed_air_yards = mean(stability_avg_completed_air_yards, na.rm = TRUE),
    stability_avg_intended_air_yards = mean(stability_avg_intended_air_yards, na.rm = TRUE),
    stability_avg_air_yards_differential = mean(stability_avg_air_yards_differential, na.rm = TRUE),
    stability_aggressiveness = mean(stability_aggressiveness, na.rm = TRUE),
    stability_max_completed_air_distance = mean(stability_max_completed_air_distance, na.rm = TRUE),
    stability_avg_air_yards_to_sticks = mean(stability_avg_air_yards_to_sticks, na.rm = TRUE),
    stability_expected_completion_percentage = mean(stability_expected_completion_percentage, na.rm = TRUE),
    stability_completion_percentage_above_expectation = mean(stability_completion_percentage_above_expectation, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "stability") %>%
  mutate(metric = str_remove(metric, "stability_")) %>%
  arrange(desc(stability))

# RB Metric Stability  
cat("\nRB METRIC STABILITY (Year N → Year N+1):\n")
combined_rushing_analysis %>%
  filter(rush_attempts >= 50) %>%
  select(player_gsis_id, season, all_of(rb_ngs_vars)) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  filter(n() >= 2) %>%
  mutate(across(all_of(rb_ngs_vars), ~lead(.x, 1), .names = "next_{.col}")) %>%
  filter(!is.na(next_efficiency)) %>%
  summarise(
    seasons = n(),
    stability_efficiency = 
      ifelse(seasons >= 2 & sum(!is.na(efficiency) & !is.na(next_efficiency)) >= 2,
             cor(efficiency, next_efficiency, use = "complete.obs"), NA),
    stability_percent_attempts_gte_eight_defenders = 
      ifelse(seasons >= 2 & sum(!is.na(percent_attempts_gte_eight_defenders) & !is.na(next_percent_attempts_gte_eight_defenders)) >= 2,
             cor(percent_attempts_gte_eight_defenders, next_percent_attempts_gte_eight_defenders, use = "complete.obs"), NA),
    stability_avg_time_to_los = 
      ifelse(seasons >= 2 & sum(!is.na(avg_time_to_los) & !is.na(next_avg_time_to_los)) >= 2,
             cor(avg_time_to_los, next_avg_time_to_los, use = "complete.obs"), NA),
    stability_rush_yards_over_expected_per_att = 
      ifelse(seasons >= 2 & sum(!is.na(rush_yards_over_expected_per_att) & !is.na(next_rush_yards_over_expected_per_att)) >= 2,
             cor(rush_yards_over_expected_per_att, next_rush_yards_over_expected_per_att, use = "complete.obs"), NA),
    stability_rush_pct_over_expected = 
      ifelse(seasons >= 2 & sum(!is.na(rush_pct_over_expected) & !is.na(next_rush_pct_over_expected)) >= 2,
             cor(rush_pct_over_expected, next_rush_pct_over_expected, use = "complete.obs"), NA),
    .groups = "drop"
  ) %>%
  summarise(
    stability_efficiency = mean(stability_efficiency, na.rm = TRUE),
    stability_percent_attempts_gte_eight_defenders = mean(stability_percent_attempts_gte_eight_defenders, na.rm = TRUE),
    stability_avg_time_to_los = mean(stability_avg_time_to_los, na.rm = TRUE),
    stability_rush_yards_over_expected_per_att = mean(stability_rush_yards_over_expected_per_att, na.rm = TRUE),
    stability_rush_pct_over_expected = mean(stability_rush_pct_over_expected, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "stability") %>%
  mutate(metric = str_remove(metric, "stability_")) %>%
  arrange(desc(stability))

# WR Metric Stability (using ALL WR NGS variables)
cat("\nWR METRIC STABILITY (Year N → Year N+1):\n")

combined_receiving_analysis %>%
  filter(targets >= 30) %>%
  select(player_gsis_id, season, all_of(wr_ngs_vars)) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  filter(n() >= 2) %>%
  mutate(across(all_of(wr_ngs_vars), ~lead(.x, 1), .names = "next_{.col}")) %>%
  filter(!is.na(next_avg_yac_above_expectation)) %>%
  summarise(
    seasons = n(),
    stability_avg_intended_air_yards = 
      ifelse(seasons >= 2 & sum(!is.na(avg_intended_air_yards) & !is.na(next_avg_intended_air_yards)) >= 2,
             cor(avg_intended_air_yards, next_avg_intended_air_yards, use = "complete.obs"), NA),
    stability_catch_percentage = 
      ifelse(seasons >= 2 & sum(!is.na(catch_percentage) & !is.na(next_catch_percentage)) >= 2,
             cor(catch_percentage, next_catch_percentage, use = "complete.obs"), NA),
    stability_avg_cushion = 
      ifelse(seasons >= 2 & sum(!is.na(avg_cushion) & !is.na(next_avg_cushion)) >= 2,
             cor(avg_cushion, next_avg_cushion, use = "complete.obs"), NA),
    stability_avg_separation = 
      ifelse(seasons >= 2 & sum(!is.na(avg_separation) & !is.na(next_avg_separation)) >= 2,
             cor(avg_separation, next_avg_separation, use = "complete.obs"), NA),
    stability_percent_share_of_intended_air_yards = 
      ifelse(seasons >= 2 & sum(!is.na(percent_share_of_intended_air_yards) & !is.na(next_percent_share_of_intended_air_yards)) >= 2,
             cor(percent_share_of_intended_air_yards, next_percent_share_of_intended_air_yards, use = "complete.obs"), NA),
    stability_avg_yac = 
      ifelse(seasons >= 2 & sum(!is.na(avg_yac) & !is.na(next_avg_yac)) >= 2,
             cor(avg_yac, next_avg_yac, use = "complete.obs"), NA),
    stability_avg_expected_yac = 
      ifelse(seasons >= 2 & sum(!is.na(avg_expected_yac) & !is.na(next_avg_expected_yac)) >= 2,
             cor(avg_expected_yac, next_avg_expected_yac, use = "complete.obs"), NA),
    stability_avg_yac_above_expectation = 
      ifelse(seasons >= 2 & sum(!is.na(avg_yac_above_expectation) & !is.na(next_avg_yac_above_expectation)) >= 2,
             cor(avg_yac_above_expectation, next_avg_yac_above_expectation, use = "complete.obs"), NA),
    .groups = "drop"
  ) %>%
  summarise(
    stability_avg_intended_air_yards = mean(stability_avg_intended_air_yards, na.rm = TRUE),
    stability_catch_percentage = mean(stability_catch_percentage, na.rm = TRUE),
    stability_avg_cushion = mean(stability_avg_cushion, na.rm = TRUE),
    stability_avg_separation = mean(stability_avg_separation, na.rm = TRUE),
    stability_percent_share_of_intended_air_yards = mean(stability_percent_share_of_intended_air_yards, na.rm = TRUE),
    stability_avg_yac = mean(stability_avg_yac, na.rm = TRUE),
    stability_avg_expected_yac = mean(stability_avg_expected_yac, na.rm = TRUE),
    stability_avg_yac_above_expectation = mean(stability_avg_yac_above_expectation, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "stability") %>%
  mutate(metric = str_remove(metric, "stability_")) %>%
  arrange(desc(stability))