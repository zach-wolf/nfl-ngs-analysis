source("data/data_collection.R")

#### Future performance prediction - QB ####
# Create QB prediction dataset: Year N NGS → Year N+1 EPA
qb_prediction_data <- combined_passing_analysis %>%
  # Minimum volume threshold for reliability
  filter(attempts >= 100) %>%
  select(player_gsis_id, player_display_name, season, team = team_abbr,
         # Current year NGS metrics (predictors)
         all_of(qb_ngs_vars),
         # Current year EPA (for comparison)
         passing_epa_per_play, total_passing_epa, attempts) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  # Create next year's EPA as target variable
  mutate(
    next_season = season + 1,
    next_year_epa_per_play = lead(passing_epa_per_play, 1),
    next_year_total_epa = lead(total_passing_epa, 1),
    next_year_attempts = lead(attempts, 1)
  ) %>%
  ungroup() %>%
  # Only keep rows where we have next year data
  filter(!is.na(next_year_epa_per_play), next_year_attempts >= 100) %>%
  # Rename for clarity
  rename(
    current_year = season,
    current_epa_per_play = passing_epa_per_play,
    current_total_epa = total_passing_epa,
    current_attempts = attempts
  )

# QB: Current Year EPA vs Next Year EPA (baseline correlation)
qb_epa_persistence <- cor(qb_prediction_data$current_epa_per_play, 
                          qb_prediction_data$next_year_epa_per_play, 
                          use = "complete.obs")

cat("QB EPA Persistence (Year N → Year N+1):", round(qb_epa_persistence, 3), "\n")

# QB: NGS metrics predicting next year EPA
qb_prediction_cors <- qb_prediction_data %>%
  select(all_of(qb_ngs_vars), next_year_epa_per_play) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  select(next_year_epa_per_play) %>%
  rownames_to_column("ngs_metric") %>%
  filter(ngs_metric != "next_year_epa_per_play") %>%
  mutate(abs_correlation = abs(next_year_epa_per_play)) %>%
  arrange(desc(abs_correlation))

cat("\nQB NGS Metrics Predicting Next Year EPA:\n")
print(qb_prediction_cors)

# QB: Best predictor visualization
best_qb_predictor <- qb_prediction_cors$ngs_metric[1]

qb_prediction_data %>%
  ggplot(aes(x = !!sym(best_qb_predictor), y = next_year_epa_per_play)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = paste("QB:", str_to_title(gsub("_", " ", best_qb_predictor)), "Predicting Next Year EPA"),
       subtitle = paste0("Strongest QB predictor (r = ", round(qb_prediction_cors$next_year_epa_per_play[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_qb_predictor)),
       y = "Next Year EPA per Play") +
  theme_minimal()

# QB: Current vs Next Year EPA comparison
qb_prediction_data %>%
  ggplot(aes(x = current_epa_per_play, y = next_year_epa_per_play)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "QB: Current Year EPA vs Next Year EPA",
       subtitle = paste0("EPA Persistence (r = ", round(qb_epa_persistence, 3), ")"),
       x = "Current Year EPA per Play",
       y = "Next Year EPA per Play") +
  theme_minimal()

#### Future performance prediction - RB ####
# Create RB prediction dataset
rb_prediction_data <- combined_rushing_analysis %>%
  filter(rush_attempts >= 50) %>%
  select(player_gsis_id, player_display_name, season, team = team_abbr,
         # Current year NGS metrics (predictors) - using top correlating ones
         all_of(rb_ngs_vars),
         # Current year EPA (for comparison)
         rushing_epa_per_play, total_rushing_epa, rush_attempts) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  mutate(
    next_season = season + 1,
    next_year_epa_per_play = lead(rushing_epa_per_play, 1),
    next_year_total_epa = lead(total_rushing_epa, 1),
    next_year_attempts = lead(rush_attempts, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(next_year_epa_per_play), next_year_attempts >= 50) %>%
  rename(
    current_year = season,
    current_epa_per_play = rushing_epa_per_play,
    current_total_epa = total_rushing_epa,
    current_attempts = rush_attempts
  )

# RB: EPA Persistence
rb_epa_persistence <- cor(rb_prediction_data$current_epa_per_play, 
                          rb_prediction_data$next_year_epa_per_play, 
                          use = "complete.obs")

cat("RB EPA Persistence (Year N → Year N+1):", round(rb_epa_persistence, 3), "\n")

# RB: NGS metrics predicting next year EPA
rb_prediction_cors <- rb_prediction_data %>%
  select(all_of(rb_ngs_vars), next_year_epa_per_play) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  select(next_year_epa_per_play) %>%
  rownames_to_column("ngs_metric") %>%
  filter(ngs_metric != "next_year_epa_per_play") %>%
  mutate(abs_correlation = abs(next_year_epa_per_play)) %>%
  arrange(desc(abs_correlation))

cat("\nRB NGS Metrics Predicting Next Year EPA:\n")
print(rb_prediction_cors)

# RB: Best predictor visualization
best_rb_predictor <- rb_prediction_cors$ngs_metric[1]

rb_prediction_data %>%
  ggplot(aes(x = !!sym(best_rb_predictor), y = next_year_epa_per_play)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = paste("RB:", str_to_title(gsub("_", " ", best_rb_predictor)), "Predicting Next Year EPA"),
       subtitle = paste0("Strongest RB predictor (r = ", round(rb_prediction_cors$next_year_epa_per_play[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_rb_predictor)),
       y = "Next Year EPA per Play") +
  theme_minimal()

# RB: Current vs Next Year EPA
rb_prediction_data %>%
  ggplot(aes(x = current_epa_per_play, y = next_year_epa_per_play)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "RB: Current Year EPA vs Next Year EPA", 
       subtitle = paste("EPA Persistence (r =", round(rb_epa_persistence, 3), ")"),
       x = "Current Year EPA per Play",
       y = "Next Year EPA per Play") +
  theme_minimal()

#### Future performance prediction - WR ####
# Create WR prediction dataset
wr_prediction_data <- combined_receiving_analysis %>%
  filter(targets >= 30) %>%
  select(player_gsis_id, player_display_name, season, team = team_abbr,
         # Current year NGS metrics (predictors) - using top correlating ones
         all_of(wr_ngs_vars),
         # Current year EPA (for comparison)
         receiving_epa_per_target, total_receiving_epa, targets) %>%
  arrange(player_gsis_id, season) %>%
  group_by(player_gsis_id) %>%
  mutate(
    next_season = season + 1,
    next_year_epa_per_target = lead(receiving_epa_per_target, 1),
    next_year_total_epa = lead(total_receiving_epa, 1),
    next_year_targets = lead(targets, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(next_year_epa_per_target), next_year_targets >= 30) %>%
  rename(
    current_year = season,
    current_epa_per_target = receiving_epa_per_target,
    current_total_epa = total_receiving_epa,
    current_targets = targets
  )

# WR: EPA Persistence
wr_epa_persistence <- cor(wr_prediction_data$current_epa_per_target, 
                          wr_prediction_data$next_year_epa_per_target, 
                          use = "complete.obs")

cat("WR EPA Persistence (Year N → Year N+1):", round(wr_epa_persistence, 3), "\n")

# WR: NGS metrics predicting next year EPA
wr_prediction_cors <- wr_prediction_data %>%
  select(all_of(wr_ngs_vars), next_year_epa_per_target) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  select(next_year_epa_per_target) %>%
  rownames_to_column("ngs_metric") %>%
  filter(ngs_metric != "next_year_epa_per_target") %>%
  mutate(abs_correlation = abs(next_year_epa_per_target)) %>%
  arrange(desc(abs_correlation))

cat("\nWR NGS Metrics Predicting Next Year EPA:\n")
print(wr_prediction_cors)

# WR: Best predictor visualization
best_wr_predictor <- wr_prediction_cors$ngs_metric[1]

wr_prediction_data %>%
  ggplot(aes(x = !!sym(best_wr_predictor), y = next_year_epa_per_target)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = paste("WR:", str_to_title(gsub("_", " ", best_wr_predictor)), "Predicting Next Year EPA"),
       subtitle = paste("Strongest WR predictor (r =", round(wr_prediction_cors$next_year_epa_per_target[1], 3), ")"),
       x = str_to_title(gsub("_", " ", best_wr_predictor)),
       y = "Next Year EPA per Target") +
  theme_minimal()

# WR: Current vs Next Year EPA
wr_prediction_data %>%
  ggplot(aes(x = current_epa_per_target, y = next_year_epa_per_target)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "WR: Current Year EPA vs Next Year EPA",
       subtitle = paste("EPA Persistence (r =", round(wr_epa_persistence, 3), ")"),
       x = "Current Year EPA per Target", 
       y = "Next Year EPA per Target") +
  theme_minimal()

## Predictive power comparison
# Combine all prediction correlations
all_predictions <- bind_rows(
  qb_prediction_cors %>% mutate(position = "QB", epa_persistence = qb_epa_persistence),
  rb_prediction_cors %>% mutate(position = "RB", epa_persistence = rb_epa_persistence),
  wr_prediction_cors %>% mutate(position = "WR", epa_persistence = wr_epa_persistence)
) %>%
  rename(future_epa_correlation = next_year_epa_per_play,
         future_epa_correlation_wr = next_year_epa_per_target) %>%
  mutate(future_epa_correlation = coalesce(future_epa_correlation, future_epa_correlation_wr)) %>%
  select(-future_epa_correlation_wr)

# Top predictors by position
top_predictors <- all_predictions %>%
  group_by(position) %>%
  slice(1) %>%
  ungroup()

for(i in 1:nrow(top_predictors)) {
  cat(top_predictors$position[i], "Best Predictor:", top_predictors$ngs_metric[i], 
      "(r =", round(top_predictors$future_epa_correlation[i], 3), ")\n")
  cat("  EPA Persistence:", round(top_predictors$epa_persistence[i], 3), "\n")
  if(abs(top_predictors$future_epa_correlation[i]) > abs(top_predictors$epa_persistence[i])) {
    cat("  → NGS metric is MORE predictive than past EPA!\n")
  } else {
    cat("  → Past EPA is more predictive than NGS metrics\n")
  }
  cat("\n")
}

# Predictive power visualization
all_predictions %>%
  group_by(position) %>%
  slice(1:3) %>%  # Top 3 per position
  ungroup() %>%
  ggplot(aes(x = reorder(ngs_metric, abs_correlation), y = abs_correlation, fill = position)) +
  geom_col(alpha = 0.7) +
  geom_hline(data = top_predictors, aes(yintercept = abs(epa_persistence), color = position), 
             linetype = "dashed", size = 1) +
  facet_wrap(~position, scales = "free_y") +
  coord_flip() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(title = "Top 3 NGS Predictors by Position vs EPA Persistence",
       subtitle = "Dashed lines = EPA persistence\nBars above line = NGS beats past performance!",
       x = "NGS Metric", y = "Absolute Correlation with Next Year EPA") +
  theme_minimal() +
  theme(legend.position = "none")
