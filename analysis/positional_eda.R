source("data/data_collection.R")

## QB correlation analysis
qb_epa_vars <- c("total_passing_epa", "passing_epa_per_play", "pass_success_rate")

# Select QB correlation data
qb_cor_data <- combined_passing_analysis %>%
  select(all_of(c(qb_ngs_vars, qb_epa_vars))) %>%
  select_if(is.numeric)

# Calculate correlation matrix
qb_cor_matrix <- cor(qb_cor_data, use = "complete.obs")

# Extract NGS vs EPA correlations
qb_ngs_epa_cors <- qb_cor_matrix[qb_ngs_vars, qb_epa_vars, drop = FALSE]

# Convert to long format for plotting
qb_cor_long <- qb_ngs_epa_cors %>%
  as.data.frame() %>%
  rownames_to_column("ngs_metric") %>%
  pivot_longer(-ngs_metric, names_to = "epa_metric", values_to = "correlation")

# Create QB correlation heatmap
ggplot(qb_cor_long, aes(x = epa_metric, y = ngs_metric, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limit = c(-1, 1)) +
  labs(title = "QB NGS vs EPA Correlations",
       subtitle = "Which NGS metrics correlate with QB performance?",
       x = "EPA Metrics", y = "NGS Metrics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Top QB correlations
qb_cor_long %>%
  mutate(abs_correlation = abs(correlation)) %>%
  filter(epa_metric == "passing_epa_per_play") %>%
  arrange(desc(abs_correlation)) %>%
  head(10)

## RB correlation analysis
rb_epa_vars <- c("total_rushing_epa", "rushing_epa_per_play", "rush_success_rate")

# Select RB correlation data
rb_cor_data <- combined_rushing_analysis %>%
  select(all_of(c(rb_ngs_vars, rb_epa_vars))) %>%
  select_if(is.numeric)

# Calculate correlation matrix
rb_cor_matrix <- cor(rb_cor_data, use = "complete.obs")

# Extract NGS vs EPA correlations
rb_ngs_epa_cors <- rb_cor_matrix[rb_ngs_vars, rb_epa_vars, drop = FALSE]

# Convert to long format
rb_cor_long <- rb_ngs_epa_cors %>%
  as.data.frame() %>%
  rownames_to_column("ngs_metric") %>%
  pivot_longer(-ngs_metric, names_to = "epa_metric", values_to = "correlation")

# Create RB correlation heatmap
ggplot(rb_cor_long, aes(x = epa_metric, y = ngs_metric, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limit = c(-1, 1)) +
  labs(title = "RB NGS vs EPA Correlations",
       subtitle = "Which NGS metrics correlate with RB performance?",
       x = "EPA Metrics", y = "NGS Metrics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Top RB correlations
rb_cor_long %>%
  mutate(abs_correlation = abs(correlation)) %>%
  filter(epa_metric == "rushing_epa_per_play") %>%
  arrange(desc(abs_correlation)) %>%
  head(10)

## WR correlation analysis
wr_epa_vars <- c("total_receiving_epa", "receiving_epa_per_target", "target_success_rate")

# Select WR correlation data
wr_cor_data <- combined_receiving_analysis %>%
  select(all_of(c(wr_ngs_vars, wr_epa_vars))) %>%
  select_if(is.numeric)

# Calculate correlation matrix
wr_cor_matrix <- cor(wr_cor_data, use = "complete.obs")

# Extract NGS vs EPA correlations
wr_ngs_epa_cors <- wr_cor_matrix[wr_ngs_vars, wr_epa_vars, drop = FALSE]

# Convert to long format
wr_cor_long <- wr_ngs_epa_cors %>%
  as.data.frame() %>%
  rownames_to_column("ngs_metric") %>%
  pivot_longer(-ngs_metric, names_to = "epa_metric", values_to = "correlation")

# Create WR correlation heatmap
ggplot(wr_cor_long, aes(x = epa_metric, y = ngs_metric, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limit = c(-1, 1)) +
  labs(title = "WR NGS vs EPA Correlations",
       subtitle = "Which NGS metrics correlate with WR performance?",
       x = "EPA Metrics", y = "NGS Metrics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Top WR correlations
wr_cor_long %>%
  mutate(abs_correlation = abs(correlation)) %>%
  filter(epa_metric == "receiving_epa_per_target") %>%
  arrange(desc(abs_correlation)) %>%
  head(10)

#### position-specific profiling ####
## QB deep dive

# QB Scatter Plot: Completion % Above Expected vs EPA (TOP CORRELATION!)
combined_passing_analysis %>%
  filter(!is.na(completion_percentage_above_expectation), !is.na(passing_epa_per_play)) %>%
  ggplot(aes(x = completion_percentage_above_expectation, y = passing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "QB: Completion % Above Expected vs EPA per Play",
       subtitle = "Strongest QB correlation - Completing passes beyond expectation drives value",
       x = "Completion % Above Expected",
       y = "EPA per Play") +
  theme_minimal()

# QB Air Yards Differential Analysis  
combined_passing_analysis %>%
  filter(!is.na(avg_air_yards_differential), !is.na(passing_epa_per_play)) %>%
  ggplot(aes(x = avg_air_yards_differential, y = passing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "QB: Air Yards Differential vs EPA per Play",
       subtitle = "Does completing passes deeper than intended create more value?",
       x = "Air Yards Differential (Completed - Intended)",
       y = "EPA per Play") +
  theme_minimal()

# QB Completed Air Yards Analysis
combined_passing_analysis %>%
  filter(!is.na(avg_completed_air_yards), !is.na(passing_epa_per_play)) %>%
  ggplot(aes(x = avg_completed_air_yards, y = passing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "QB: Average Completed Air Yards vs EPA per Play",
       subtitle = "Does actually completing deeper passes create more value?",
       x = "Average Completed Air Yards",
       y = "EPA per Play") +
  theme_minimal()

# QB Performance Tiers Analysis
qb_tiers <- combined_passing_analysis %>%
  filter(!is.na(passing_epa_per_play), attempts >= 100) %>%
  mutate(
    performance_tier = case_when(
      passing_epa_per_play >= quantile(passing_epa_per_play, 0.8, na.rm = TRUE) ~ "Elite",
      passing_epa_per_play >= quantile(passing_epa_per_play, 0.6, na.rm = TRUE) ~ "Good",
      passing_epa_per_play >= quantile(passing_epa_per_play, 0.4, na.rm = TRUE) ~ "Average",
      passing_epa_per_play >= quantile(passing_epa_per_play, 0.2, na.rm = TRUE) ~ "Below Average",
      TRUE ~ "Poor"
    ),
    performance_tier = factor(performance_tier, levels = c("Elite", "Good", "Average", "Below Average", "Poor"))
  )

# QB NGS by Performance Tier (using the metrics that actually correlate!)
qb_tier_summary <- qb_tiers %>%
  group_by(performance_tier) %>%
  summarise(
    count = n(),
    avg_cpoe = mean(completion_percentage_above_expectation, na.rm = TRUE),
    avg_air_yards_diff = mean(avg_air_yards_differential, na.rm = TRUE),
    avg_completed_air_yards = mean(avg_completed_air_yards, na.rm = TRUE),
    avg_exp_comp_pct = mean(expected_completion_percentage, na.rm = TRUE),
    avg_air_yards_to_sticks = mean(avg_air_yards_to_sticks, na.rm = TRUE),
    .groups = "drop"
  )

# QB Tier Visualization (updated with top correlating metrics)
qb_tier_metrics <- qb_tiers %>%
  select(performance_tier, completion_percentage_above_expectation, avg_air_yards_differential, 
         avg_completed_air_yards, avg_air_yards_to_sticks) %>%
  pivot_longer(-performance_tier, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = performance_tier, y = value, fill = performance_tier)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d() +
  labs(title = "QB NGS Metrics by Performance Tier (Top Correlations)",
       subtitle = "How do the most predictive NGS metrics differ across QB performance?",
       x = "Performance Tier", y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(qb_tier_metrics,
       filename = "visualizations/qb_metrics_by_tier.png",
       width = 9,
       height = 7)

## RB deep dive
# RB Efficiency vs EPA (NEGATIVE correlation - lower efficiency = better!)
combined_rushing_analysis %>%
  filter(!is.na(efficiency), !is.na(rushing_epa_per_play)) %>%
  ggplot(aes(x = efficiency, y = rushing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "RB: Efficiency vs EPA per Play (Negative Correlation!)",
       subtitle = "Lower efficiency (more direct running) creates MORE value",
       x = "Efficiency (lower = more direct paths)",
       y = "EPA per Play") +
  theme_minimal()

# RB Rush Yards Over Expected per Attempt vs EPA (TOP CORRELATION!)
combined_rushing_analysis %>%
  filter(!is.na(rush_yards_over_expected_per_att), !is.na(rushing_epa_per_play)) %>%
  ggplot(aes(x = rush_yards_over_expected_per_att, y = rushing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "RB: Rush Yards Over Expected per Attempt vs EPA per Play",
       subtitle = "Strongest RB correlation - Beating expected yards drives value",
       x = "Rush Yards Over Expected per Attempt",
       y = "EPA per Play") +
  theme_minimal()

# RB Rush Percentage Over Expected vs EPA  
combined_rushing_analysis %>%
  filter(!is.na(rush_pct_over_expected), !is.na(rushing_epa_per_play)) %>%
  ggplot(aes(x = rush_pct_over_expected, y = rushing_epa_per_play)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "RB: Rush Percentage Over Expected vs EPA per Play",
       subtitle = "Consistently beating expectations creates value",
       x = "Rush Percentage Over Expected",
       y = "EPA per Play") +
  theme_minimal()

# RB Performance Tiers
rb_tiers <- combined_rushing_analysis %>%
  filter(!is.na(rushing_epa_per_play), rush_attempts >= 50) %>%
  mutate(
    performance_tier = case_when(
      rushing_epa_per_play >= quantile(rushing_epa_per_play, 0.8, na.rm = TRUE) ~ "Elite",
      rushing_epa_per_play >= quantile(rushing_epa_per_play, 0.6, na.rm = TRUE) ~ "Good", 
      rushing_epa_per_play >= quantile(rushing_epa_per_play, 0.4, na.rm = TRUE) ~ "Average",
      rushing_epa_per_play >= quantile(rushing_epa_per_play, 0.2, na.rm = TRUE) ~ "Below Average",
      TRUE ~ "Poor"
    ),
    performance_tier = factor(performance_tier, levels = c("Elite", "Good", "Average", "Below Average", "Poor"))
  )

# RB NGS by Performance Tier (using the metrics that actually correlate!)
rb_tier_summary <- rb_tiers %>%
  group_by(performance_tier) %>%
  summarise(
    count = n(),
    avg_efficiency = mean(efficiency, na.rm = TRUE),
    avg_ryoe_per_att = mean(rush_yards_over_expected_per_att, na.rm = TRUE),
    avg_ryoe_total = mean(rush_yards_over_expected, na.rm = TRUE),
    avg_rush_pct_over_exp = mean(rush_pct_over_expected, na.rm = TRUE),
    avg_expected_rush_yards = mean(expected_rush_yards, na.rm = TRUE),
    .groups = "drop"
  )

# RB Tier Visualization (updated with top correlating metrics)  
rb_tier_metrics <- rb_tiers %>%
  select(performance_tier, efficiency, rush_yards_over_expected_per_att, 
         rush_pct_over_expected, avg_time_to_los) %>%
  pivot_longer(-performance_tier, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = performance_tier, y = value, fill = performance_tier)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d() +
  labs(title = "RB NGS Metrics by Performance Tier (Top Correlations)",
       subtitle = "How do the most predictive NGS metrics differ across RB performance?",
       x = "Performance Tier", y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(rb_tier_metrics,
       filename = "visualizations/rb_metrics_by_tier.png",
       width = 9,
       height = 7)

## WR deep dive
# WR Catch Percentage vs EPA (TOP CORRELATION!)
combined_receiving_analysis %>%
  filter(!is.na(catch_percentage), !is.na(receiving_epa_per_target)) %>%
  ggplot(aes(x = catch_percentage, y = receiving_epa_per_target)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "WR: Catch Percentage vs EPA per Target",
       subtitle = "Strongest WR correlation - Reliability drives value creation",
       x = "Catch Percentage",
       y = "EPA per Target") +
  theme_minimal()

# WR YAC Above Expected vs EPA
combined_receiving_analysis %>%
  filter(!is.na(avg_yac_above_expectation), !is.na(receiving_epa_per_target)) %>%
  ggplot(aes(x = avg_yac_above_expectation, y = receiving_epa_per_target)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "WR: YAC Above Expected vs EPA per Target",
       subtitle = "Creating extra yards after catch drives value",
       x = "Average YAC Above Expected",
       y = "EPA per Target") +
  theme_minimal()

# WR Target Share vs EPA
combined_receiving_analysis %>%
  filter(!is.na(percent_share_of_intended_air_yards), !is.na(receiving_epa_per_target)) %>%
  ggplot(aes(x = percent_share_of_intended_air_yards, y = receiving_epa_per_target)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "WR: Share of Intended Air Yards vs EPA per Target",
       subtitle = "Does being the primary deep target create more value?",
       x = "% Share of Team's Intended Air Yards",
       y = "EPA per Target") +
  theme_minimal()

# WR Performance Tiers
wr_tiers <- combined_receiving_analysis %>%
  filter(!is.na(receiving_epa_per_target), targets >= 30) %>%
  mutate(
    performance_tier = case_when(
      receiving_epa_per_target >= quantile(receiving_epa_per_target, 0.8, na.rm = TRUE) ~ "Elite",
      receiving_epa_per_target >= quantile(receiving_epa_per_target, 0.6, na.rm = TRUE) ~ "Good",
      receiving_epa_per_target >= quantile(receiving_epa_per_target, 0.4, na.rm = TRUE) ~ "Average", 
      receiving_epa_per_target >= quantile(receiving_epa_per_target, 0.2, na.rm = TRUE) ~ "Below Average",
      TRUE ~ "Poor"
    ),
    performance_tier = factor(performance_tier, levels = c("Elite", "Good", "Average", "Below Average", "Poor"))
  )

# WR NGS by Performance Tier (using the metrics that actually correlate!)
wr_tier_summary <- wr_tiers %>%
  group_by(performance_tier) %>%
  summarise(
    count = n(),
    avg_catch_percentage = mean(catch_percentage, na.rm = TRUE),
    avg_yac_above_exp = mean(avg_yac_above_expectation, na.rm = TRUE),
    avg_yac = mean(avg_yac, na.rm = TRUE),
    avg_air_share = mean(percent_share_of_intended_air_yards, na.rm = TRUE),
    avg_expected_yac = mean(avg_expected_yac, na.rm = TRUE),
    .groups = "drop"
  )

# WR Tier Visualization (updated with top correlating metrics)
wr_tier_metrics <- wr_tiers %>%
  select(performance_tier, catch_percentage, avg_yac_above_expectation, 
         avg_yac, percent_share_of_intended_air_yards) %>%
  pivot_longer(-performance_tier, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = performance_tier, y = value, fill = performance_tier)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d() +
  labs(title = "WR NGS Metrics by Performance Tier (Top Correlations)",
       subtitle = "How do the most predictive NGS metrics differ across WR performance?",
       x = "Performance Tier", y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(wr_tier_metrics,
       filename = "visualizations/wr_metrics_by_tier.png",
       width = 9,
       height = 7)

## Position comparison analysis
# Create position comparison dataset
position_comparison <- bind_rows(
  qb_tiers %>% 
    select(performance_tier, passing_epa_per_play) %>%
    rename(epa_per_play = passing_epa_per_play) %>%
    mutate(position = "QB"),
  rb_tiers %>%
    select(performance_tier, rushing_epa_per_play) %>%
    rename(epa_per_play = rushing_epa_per_play) %>%
    mutate(position = "RB"),
  wr_tiers %>%
    select(performance_tier, receiving_epa_per_target) %>%
    rename(epa_per_play = receiving_epa_per_target) %>%
    mutate(position = "WR")
)

# EPA Distribution by Position and Tier
ggplot(position_comparison, aes(x = performance_tier, y = epa_per_play, fill = position)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~position, scales = "free_y") +
  scale_fill_viridis_d() +
  labs(title = "EPA Performance by Position and Tier",
       subtitle = "How does value creation vary across positions?",
       x = "Performance Tier", y = "EPA per Play/Target") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
