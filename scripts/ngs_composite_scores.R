source("analysis/interaction_analysis.R")

## QB Predictive Composite Score
qb_predictive_composite <- qb_best_predictive_combo_data %>%
  mutate(
    # Weight metrics by their individual predictive correlation strength
    metric1_weight = abs(qb_predictive_combo_results$cor1_future[1]),
    metric2_weight = abs(qb_predictive_combo_results$cor2_future[1]),
    metric3_weight = abs(qb_predictive_combo_results$cor2_future[2]),
    total_weight = metric1_weight + metric2_weight + metric3_weight,
    
    qb_predictive_composite_score = (scale(!!sym(best_qb_predictive_metrics[1]))[,1] * metric1_weight + 
                                       scale(!!sym(best_qb_predictive_metrics[2]))[,1] * metric2_weight +
                                       scale(!!sym("max_completed_air_distance"))[,1] * metric3_weight) / total_weight,
    
    composite_percentile = ntile(qb_predictive_composite_score, 100)
  ) %>%
  select(player_display_name, current_year, age, years_exp, qb_predictive_composite_score, composite_percentile, 
         passing_epa_per_play, next_year_epa_per_play, metric1_weight, metric2_weight, metric3_weight, total_weight)
# Test predictive composite score correlation
qb_predictive_composite_cor <- cor(qb_predictive_composite$qb_predictive_composite_score, 
                                   qb_predictive_composite$next_year_epa_per_play, use = "complete.obs")

cat("QB PREDICTIVE COMPOSITE SCORE ANALYSIS:\n")
cat("Composite Score → Next Year EPA Correlation:", round(qb_predictive_composite_cor, 3), "\n")
cat("vs Individual Best Predictor Correlation:", round(qb_predictive_combo_results$cor1_future[1], 3), "\n")
cat("Predictive Improvement:", round(qb_predictive_composite_cor - abs(qb_predictive_combo_results$cor1_future[1]), 3), "\n\n")

# Top predictive composite scores
cat("Top 10 QB Predictive Composite Scores (for future performance):\n")
print(qb_predictive_composite %>%
        arrange(desc(qb_predictive_composite_score)) %>%
        head(10))

quantile(qb_predictive_composite$qb_predictive_composite_score, 0.90)
quantile(qb_predictive_composite$qb_predictive_composite_score, 0.95)
quantile(qb_predictive_composite$qb_predictive_composite_score, 0.99)

qb_2024_composite <- combined_passing_analysis %>%
  filter(season == 2024) %>%
  mutate(composite_score = (scale(expected_completion_percentage)[,1] * qb_predictive_composite$metric1_weight[1] +
                              scale(completion_percentage_above_expectation)[,1] * qb_predictive_composite$metric2_weight[1] +
                              scale(max_completed_air_distance)[,1] * qb_predictive_composite$metric3_weight[1]) / qb_predictive_composite$total_weight[1],
         composite_90perc = ifelse(composite_score >= quantile(qb_predictive_composite$qb_predictive_composite_score, 0.90), 1, 0)) %>%
  select(season:team_abbr, composite_score, composite_90perc) %>%
  arrange(desc(composite_score))


## RB Predictive Composite Score
rb_predictive_composite <- rb_best_predictive_combo_data %>%
  mutate(
    # Weight metrics by their individual predictive correlation strength
    metric1_weight = abs(rb_predictive_combo_results$cor1_future[1]),
    metric2_weight = abs(rb_predictive_combo_results$cor2_future[1]),
    metric3_weight = abs(rb_predictive_combo_results$cor1_future[2]),
    total_weight = metric1_weight + metric2_weight + metric3_weight,
    
    rb_predictive_composite_score = (-scale(!!sym(best_rb_predictive_metrics[1]))[,1] * metric1_weight + 
                                       scale(!!sym(best_rb_predictive_metrics[2]))[,1] * metric2_weight +
                                       scale(!!sym("rush_yards_over_expected_per_att"))[,1] * metric3_weight) / total_weight,
    
    composite_percentile = ntile(rb_predictive_composite_score, 100)
  ) %>%
  select(player_display_name, current_year, age, years_exp, rb_predictive_composite_score, composite_percentile, 
         rushing_epa_per_play, next_year_epa_per_play, metric1_weight, metric2_weight, metric3_weight, total_weight)

# Test predictive composite score correlation
rb_predictive_composite_cor <- cor(rb_predictive_composite$rb_predictive_composite_score, 
                                   rb_predictive_composite$next_year_epa_per_play, use = "complete.obs")

cat("rb PREDICTIVE COMPOSITE SCORE ANALYSIS:\n")
cat("Composite Score → Next Year EPA Correlation:", round(rb_predictive_composite_cor, 3), "\n")
cat("vs Individual Best Predictor Correlation:", round(max(rb_predictive_combo_results$cor1_future), 3), "\n")
cat("Predictive Improvement:", round(rb_predictive_composite_cor - abs(rb_predictive_combo_results$cor1_future[2]), 3), "\n\n")

# Top predictive composite scores
cat("Top 10 rb Predictive Composite Scores (for future performance):\n")
print(rb_predictive_composite %>%
        arrange(desc(rb_predictive_composite_score)) %>%
        head(10))

quantile(rb_predictive_composite$rb_predictive_composite_score, 0.90)
quantile(rb_predictive_composite$rb_predictive_composite_score, 0.95)
quantile(rb_predictive_composite$rb_predictive_composite_score, 0.99)

rb_2024_composite <- combined_rushing_analysis %>%
  filter(season == 2024) %>%
  mutate(composite_score = (scale(efficiency)[,1] * rb_predictive_composite$metric1_weight[1] +
                              scale(avg_time_to_los)[,1] * rb_predictive_composite$metric2_weight[1] +
                              scale(rush_yards_over_expected_per_att)[,1] * rb_predictive_composite$metric3_weight[1]) / rb_predictive_composite$total_weight[1],
         composite_90perc = ifelse(composite_score >= quantile(rb_predictive_composite$rb_predictive_composite_score, 0.90), 1, 0)) %>%
  select(season:team_abbr, composite_score, composite_90perc) %>%
  arrange(desc(composite_score))

## WR Predictive Composite Score
wr_predictive_composite <- wr_best_predictive_combo_data %>%
  mutate(
    # Weight metrics by their individual predictive correlation strength
    metric1_weight = abs(wr_predictive_combo_results$cor1_future[1]),
    metric2_weight = abs(wr_predictive_combo_results$cor2_future[1]),
    metric3_weight = abs(wr_predictive_combo_results$cor2_future[2]),
    metric4_weight = abs(wr_predictive_combo_results$cor2_future[3]),
    total_weight = metric1_weight + metric2_weight + metric3_weight + metric4_weight,
    
    wr_predictive_composite_score = (scale(!!sym(best_wr_predictive_metrics[1]))[,1] * metric1_weight + 
                                       scale(!!sym(best_wr_predictive_metrics[2]))[,1] * metric2_weight +
                                       scale(!!sym("avg_intended_air_yards"))[,1] * metric3_weight +
                                       scale(!!sym("catch_percentage"))[,1] * metric4_weight) / total_weight,
    
    composite_percentile = ntile(wr_predictive_composite_score, 100)
  ) %>%
  select(player_display_name, current_year, age, years_exp, wr_predictive_composite_score, composite_percentile, 
         receiving_epa_per_target, next_year_epa_per_target, metric1_weight, metric2_weight, metric3_weight, metric4_weight, total_weight)

# Test predictive composite score correlation
wr_predictive_composite_cor <- cor(wr_predictive_composite$wr_predictive_composite_score, 
                                   wr_predictive_composite$next_year_epa_per_target, use = "complete.obs")

cat("wr PREDICTIVE COMPOSITE SCORE ANALYSIS:\n")
cat("Composite Score → Next Year EPA Correlation:", round(wr_predictive_composite_cor, 3), "\n")
cat("vs Individual Best Predictor Correlation:", round(wr_predictive_combo_results$cor1_future[1], 3), "\n")
cat("Predictive Improvement:", round(wr_predictive_composite_cor - abs(wr_predictive_combo_results$cor1_future[1]), 3), "\n\n")

# Top predictive composite scores
cat("Top 10 WR Predictive Composite Scores (for future performance):\n")
print(wr_predictive_composite %>%
        arrange(desc(wr_predictive_composite_score)) %>%
        head(10))

quantile(wr_predictive_composite$wr_predictive_composite_score, 0.90)
quantile(wr_predictive_composite$wr_predictive_composite_score, 0.95)
quantile(wr_predictive_composite$wr_predictive_composite_score, 0.99)

wr_2024_composite <- combined_receiving_analysis %>%
  filter(season == 2024) %>%
  mutate(composite_score = (scale(avg_yac_above_expectation)[,1] * wr_predictive_composite$metric1_weight[1] +
                              scale(percent_share_of_intended_air_yards)[,1] * wr_predictive_composite$metric2_weight[1] +
                              scale(avg_intended_air_yards)[,1] * wr_predictive_composite$metric3_weight[1] +
                              scale(catch_percentage)[,1] * wr_predictive_composite$metric4_weight[1]) / wr_predictive_composite$total_weight[1],
         composite_90perc = ifelse(composite_score >= quantile(wr_predictive_composite$wr_predictive_composite_score, 0.90), 1, 0)) %>%
  select(season:team_abbr, composite_score, composite_90perc) %>%
  arrange(desc(composite_score))
