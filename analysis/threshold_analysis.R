source("data/data_collection.R")

#### Threshold Analysis ####

## QB threshold analysis
qb_threshold_data <- combined_passing_analysis %>%
  filter(attempts >= 100, 
         !is.na(completion_percentage_above_expectation),
         !is.na(passing_epa_per_play)) %>%
  select(player_gsis_id, player_display_name, season, completion_percentage_above_expectation, 
         passing_epa_per_play, attempts) %>%
  arrange(completion_percentage_above_expectation)

# Create percentile-based performance bins
qb_threshold_analysis <- qb_threshold_data %>%
  mutate(
    cpoe_percentile = ntile(completion_percentage_above_expectation, 10), # 20 bins (5% each)
    cpoe_bin = case_when(
      cpoe_percentile <= 1 ~ "Bottom 10%",
      cpoe_percentile <= 2 ~ "10th-20th %ile", 
      cpoe_percentile <= 4 ~ "20th-40th %ile",
      cpoe_percentile <= 6 ~ "40th-60th %ile",
      cpoe_percentile <= 8 ~ "60th-80th %ile",
      cpoe_percentile <= 9 ~ "80th-90th %ile",
      TRUE ~ "Top 10%"
    ),
    cpoe_bin = factor(cpoe_bin, levels = c("Bottom 10%", "10th-20th %ile", "20th-40th %ile", 
                                           "40th-60th %ile", "60th-80th %ile", "80th-90th %ile", "Top 10%"))
  )

# Calculate bin statistics
qb_bin_stats <- qb_threshold_analysis %>%
  group_by(cpoe_bin, cpoe_percentile) %>%
  summarise(
    count = n(),
    min_cpoe = min(completion_percentage_above_expectation),
    max_cpoe = max(completion_percentage_above_expectation),
    avg_cpoe = mean(completion_percentage_above_expectation),
    avg_epa = mean(passing_epa_per_play),
    median_epa = median(passing_epa_per_play),
    sd_epa = sd(passing_epa_per_play),
    .groups = "drop"
  ) %>%
  arrange(cpoe_percentile)

cat("QB CPOE Performance by Percentile Bins:\n")
print(qb_bin_stats)

# Identify dramatic breakpoints
qb_epa_changes <- qb_bin_stats %>%
  arrange(cpoe_percentile) %>%
  mutate(
    epa_change = avg_epa - lag(avg_epa),
    epa_change_pct = (avg_epa - lag(avg_epa)) / lag(avg_epa) * 100
  ) %>%
  filter(!is.na(epa_change)) %>%
  arrange(desc(abs(epa_change)))

cat("\nLargest EPA Changes Between Adjacent CPOE Bins:\n")
print(qb_epa_changes %>% select(cpoe_bin, avg_cpoe, avg_epa, epa_change, epa_change_pct) %>% head(5))

# QB Threshold Visualization
ggplot(qb_threshold_analysis, aes(x = completion_percentage_above_expectation, y = passing_epa_per_play)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", color = "red", size = 1.5) +
  # Add vertical lines at key thresholds
  geom_vline(xintercept = quantile(qb_threshold_data$completion_percentage_above_expectation, c(0.1, 0.25, 0.5, 0.75, 0.9)), 
             linetype = "dashed", alpha = 0.5, color = "blue") +
  # Add bin averages
  geom_point(data = qb_bin_stats, aes(x = avg_cpoe, y = avg_epa), 
             color = "red", size = 4, shape = 18) +
  labs(title = "QB: Completion % Above Expected Thresholds",
       subtitle = "Red diamonds = bin averages. Blue lines = key percentiles. Look for steep changes!",
       x = "Completion % Above Expected",
       y = "EPA per Play") +
  theme_minimal()

# Elite vs Average vs Poor thresholds
qb_thresholds <- qb_threshold_data %>%
  summarise(
    bottom_10_threshold = quantile(completion_percentage_above_expectation, 0.1),
    bottom_25_threshold = quantile(completion_percentage_above_expectation, 0.25), 
    median_threshold = quantile(completion_percentage_above_expectation, 0.5),
    top_20_threshold = quantile(completion_percentage_above_expectation, 0.8),
    top_10_threshold = quantile(completion_percentage_above_expectation, 0.9),
    # EPA at these thresholds
    bottom_10_epa = mean(passing_epa_per_play[completion_percentage_above_expectation <= bottom_10_threshold]),
    median_epa = mean(passing_epa_per_play[abs(completion_percentage_above_expectation - median_threshold) <= 0.5]),
    top_20_epa = mean(passing_epa_per_play[completion_percentage_above_expectation >= top_20_threshold]),
    top_10_epa = mean(passing_epa_per_play[completion_percentage_above_expectation >= top_10_threshold])
  )

cat("\nQB CPOE Key Thresholds:\n")
cat("Bottom 10%:", round(qb_thresholds$bottom_10_threshold, 2), "CPOE (", round(qb_thresholds$bottom_10_epa, 3), "EPA/play)\n")
cat("Median    :", round(qb_thresholds$median_threshold, 2), "CPOE (", round(qb_thresholds$median_epa, 3), "EPA/play)\n")  
cat("Top 20%   :", round(qb_thresholds$top_20_threshold, 2), "CPOE (", round(qb_thresholds$top_20_epa, 3), "EPA/play)\n")
cat("Good Premium: +", round(qb_thresholds$top_20_epa - qb_thresholds$median_epa, 3), "EPA/play vs median\n")
cat("Top 10%   :", round(qb_thresholds$top_10_threshold, 2), "CPOE (", round(qb_thresholds$top_10_epa, 3), "EPA/play)\n")
cat("Elite Premium: +", round(qb_thresholds$top_10_epa - qb_thresholds$median_epa, 3), "EPA/play vs median\n")

## RB threshold analysis
rb_threshold_data <- combined_rushing_analysis %>%
  filter(rush_attempts >= 50,
         !is.na(rush_yards_over_expected_per_att),
         !is.na(rushing_epa_per_play)) %>%
  select(player_gsis_id, player_display_name, season, rush_yards_over_expected_per_att, rushing_epa_per_play, rush_attempts) %>%
  arrange(desc(rush_yards_over_expected_per_att))

# Create rush_yards_over_expected_per_att performance bins (remember: lower rush_yards_over_expected_per_att = better performance)
rb_threshold_analysis <- rb_threshold_data %>%
  mutate(
    ryoeatt_percentile = ntile(rush_yards_over_expected_per_att, 20), # Lower percentile = lower rush_yards_over_expected_per_att = BETTER
    ryoeatt_bin = case_when(
      ryoeatt_percentile <= 2 ~ "Top 10%",      # Best performers
      ryoeatt_percentile <= 4 ~ "80th-90th %ile",
      ryoeatt_percentile <= 8 ~ "60th-80th %ile",
      ryoeatt_percentile <= 12 ~ "40th-60th %ile",
      ryoeatt_percentile <= 16 ~ "20th-40th %ile",
      ryoeatt_percentile <= 18 ~ "10th-20th %ile",
      TRUE ~ "Bottom 10%"                  # Worst performers  
    ),
    ryoeatt_bin = factor(ryoeatt_bin, levels = c("Most Direct (Top 10%)", "Very Direct (80th-90th %ile)", 
                                                 "Above Avg Direct (60th-80th %ile)", "Average (40th-60th %ile)",
                                                 "Below Avg (20th-40th %ile)", "Poor (10th-20th %ile)", "Most Indirect (Bottom 10%)"))
  )

# Calculate bin statistics
rb_bin_stats <- rb_threshold_analysis %>%
  group_by(ryoeatt_bin, ryoeatt_percentile) %>%
  summarise(
    count = n(),
    min_rush_yards_over_expected_per_att = min(rush_yards_over_expected_per_att),
    max_rush_yards_over_expected_per_att = max(rush_yards_over_expected_per_att), 
    avg_rush_yards_over_expected_per_att = mean(rush_yards_over_expected_per_att),
    avg_epa = mean(rushing_epa_per_play),
    median_epa = median(rushing_epa_per_play),
    .groups = "drop"
  ) %>%
  arrange(ryoeatt_percentile)

cat("RB rush_yards_over_expected_per_att Performance by Percentile Bins:\n")
print(rb_bin_stats)

# RB Threshold Visualization
ggplot(rb_threshold_analysis, aes(x = rush_yards_over_expected_per_att, y = rushing_epa_per_play)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", color = "red", size = 1.5) +
  geom_vline(xintercept = quantile(rb_threshold_data$rush_yards_over_expected_per_att, c(0.1, 0.25, 0.5, 0.75, 0.9)), 
             linetype = "dashed", alpha = 0.5, color = "blue") +
  geom_point(data = rb_bin_stats, aes(x = avg_rush_yards_over_expected_per_att, y = avg_epa), 
             color = "red", size = 4, shape = 18) +
  labs(title = "RB: RYOE/ATT Thresholds",
       subtitle = "Red diamonds = bin averages",
       x = "RYOE/ATT",
       y = "EPA per Play") +
  theme_minimal()

# RB key thresholds
rb_thresholds <- rb_threshold_data %>%
  summarise(
    bottom_10_threshold = quantile(rush_yards_over_expected_per_att, 0.1),
    bottom_25_threshold = quantile(rush_yards_over_expected_per_att, 0.25), 
    median_threshold = quantile(rush_yards_over_expected_per_att, 0.5),
    top_25_threshold = quantile(rush_yards_over_expected_per_att, 0.75),
    top_10_threshold = quantile(rush_yards_over_expected_per_att, 0.9),
    # EPA at these thresholds
    bottom_10_epa = mean(rushing_epa_per_play[rush_yards_over_expected_per_att <= bottom_10_threshold]),
    median_epa = mean(rushing_epa_per_play[abs(rush_yards_over_expected_per_att - median_threshold) <= 0.1]),
    top_25_epa = mean(rushing_epa_per_play[rush_yards_over_expected_per_att >= top_25_threshold]),
    top_10_epa = mean(rushing_epa_per_play[rush_yards_over_expected_per_att >= top_10_threshold])
  )

cat("\nRB RYOE/ATT Key Thresholds:\n")
cat("Bottom 10%:", round(rb_thresholds$bottom_10_threshold, 2), "RYOE/ATT (", round(rb_thresholds$bottom_10_epa, 3), "EPA/play)\n")
cat("Median    :", round(rb_thresholds$median_threshold, 2), "RYOE/ATT (", round(rb_thresholds$median_epa, 3), "EPA/play)\n")  
cat("Top 25%   :", round(rb_thresholds$top_25_threshold, 2), "RYOE/ATT (", round(rb_thresholds$top_25_epa, 3), "EPA/play)\n")
cat("Good Premium: +", round(rb_thresholds$top_25_epa - rb_thresholds$median_epa, 3), "EPA/play vs median\n")
cat("Top 10%   :", round(rb_thresholds$top_10_threshold, 2), "RYOE/ATT (", round(rb_thresholds$top_10_epa, 3), "EPA/play)\n")
cat("Elite Premium: +", round(rb_thresholds$top_10_epa - rb_thresholds$median_epa, 3), "EPA/play vs median\n")

## WR thresholds
wr_threshold_data <- combined_receiving_analysis %>%
  filter(targets >= 30,
         !is.na(avg_yac_above_expectation),
         !is.na(receiving_epa_per_target)) %>%
  select(player_gsis_id, player_display_name, season, avg_yac_above_expectation, receiving_epa_per_target, targets) %>%
  arrange(avg_yac_above_expectation)

# Create catch percentage performance bins
wr_threshold_analysis <- wr_threshold_data %>%
  mutate(
    yac_percentile = ntile(avg_yac_above_expectation, 20),
    yac_bin = case_when(
      yac_percentile <= 2 ~ "Bottom 10%",
      yac_percentile <= 4 ~ "10th-20th %ile",
      yac_percentile <= 8 ~ "20th-40th %ile", 
      yac_percentile <= 12 ~ "40th-60th %ile",
      yac_percentile <= 16 ~ "60th-80th %ile",
      yac_percentile <= 18 ~ "80th-90th %ile",
      TRUE ~ "Top 10%"
    ),
    yac_bin = factor(yac_bin, levels = c("Bottom 10%", "10th-20th %ile", "20th-40th %ile",
                                         "40th-60th %ile", "60th-80th %ile", "80th-90th %ile", "Top 10%"))
  )

# Calculate bin statistics
wr_bin_stats <- wr_threshold_analysis %>%
  group_by(yac_bin, yac_percentile) %>%
  summarise(
    count = n(),
    min_yac_pct = min(avg_yac_above_expectation),
    max_yac_pct = max(avg_yac_above_expectation),
    avg_yac_pct = mean(avg_yac_above_expectation),
    avg_epa = mean(receiving_epa_per_target),
    median_epa = median(receiving_epa_per_target),
    .groups = "drop"
  ) %>%
  arrange(yac_percentile)

cat("WR yac % Performance by Percentile Bins:\n")
print(wr_bin_stats)

# WR Threshold Visualization
ggplot(wr_threshold_analysis, aes(x = avg_yac_above_expectation, y = receiving_epa_per_target)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", color = "red", size = 1.5) +
  geom_vline(xintercept = quantile(wr_threshold_data$avg_yac_above_expectation, c(0.1, 0.25, 0.5, 0.75, 0.9)), 
             linetype = "dashed", alpha = 0.5, color = "blue") +
  geom_point(data = wr_bin_stats, aes(x = avg_yac_pct, y = avg_epa), 
             color = "red", size = 4, shape = 18) +
  labs(title = "WR: YAC +/- Thresholds",
       subtitle = "Red diamonds = bin averages. Look for steep reliability thresholds!",
       x = "YAC +/-",
       y = "EPA per Target") +
  theme_minimal()

# WR key thresholds
wr_thresholds <- wr_threshold_data %>%
  summarise(
    bottom_10_threshold = quantile(avg_yac_above_expectation, 0.1),
    bottom_25_threshold = quantile(avg_yac_above_expectation, 0.25),
    median_threshold = quantile(avg_yac_above_expectation, 0.5),
    top_25_threshold = quantile(avg_yac_above_expectation, 0.75),
    top_10_threshold = quantile(avg_yac_above_expectation, 0.9),
    # EPA at these thresholds
    bottom_10_epa = mean(receiving_epa_per_target[avg_yac_above_expectation <= bottom_10_threshold]),
    median_epa = mean(receiving_epa_per_target[abs(avg_yac_above_expectation - median_threshold) <= 0.2]),
    top_25_epa = mean(receiving_epa_per_target[avg_yac_above_expectation >= top_25_threshold]),
    top_10_epa = mean(receiving_epa_per_target[avg_yac_above_expectation >= top_10_threshold])
  )

cat("\nWR Catch % Key Thresholds:\n")
cat("Bottom 10%:", round(wr_thresholds$bottom_10_threshold, 1), " YAC +/- (", round(wr_thresholds$bottom_10_epa, 3), "EPA/target)\n")
cat("Median    :", round(wr_thresholds$median_threshold, 1), " YAC +/- (", round(wr_thresholds$median_epa, 3), "EPA/target)\n")
cat("Top 25%   :", round(wr_thresholds$top_25_threshold, 1), " YAC +/- (", round(wr_thresholds$top_25_epa, 3), "EPA/target)\n") 
cat("Good Premium: +", round(wr_thresholds$top_25_epa - wr_thresholds$median_epa, 3), "EPA/target vs median\n")
cat("Top 10%   :", round(wr_thresholds$top_10_threshold, 1), " YAC +/- (", round(wr_thresholds$top_10_epa, 3), "EPA/target)\n") 
cat("Elite Premium: +", round(wr_thresholds$top_10_epa - wr_thresholds$median_epa, 3), "EPA/target vs median\n")