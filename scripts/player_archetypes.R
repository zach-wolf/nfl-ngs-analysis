source("analysis/predictive_analysis.R")

#### Player Archetype Analysis ####
# Create QB archetype dataset using top predictive metrics
qb_archetype_data <- combined_passing_analysis %>%
  filter(season <= 2023) %>%
  filter(attempts >= 150,  # Higher threshold for more established players
         !is.na(expected_completion_percentage),
         !is.na(completion_percentage_above_expectation),
         !is.na(max_completed_air_distance),
         !is.na(aggressiveness),
         !is.na(avg_time_to_throw),
         !is.na(avg_air_yards_differential)) %>%
  select(player_gsis_id, player_display_name, season, 
         all_of(qb_ngs_vars),
         passing_epa_per_play, attempts) %>%
  # Take most recent season for each player to avoid duplicates
  group_by(player_gsis_id) %>%
  slice_max(season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(select(qb_prediction_data, current_year, player_gsis_id, next_year_epa_per_play),
            by = c("season" = "current_year", "player_gsis_id"))

cat("QB Archetype analysis using", nrow(qb_archetype_data), "quarterbacks\n")

# Prepare data for clustering (standardize metrics)
qb_cluster_data <- qb_archetype_data %>%
  select(all_of(qb_ngs_vars)) %>%
  scale() %>%
  as.data.frame()

# Add player names for reference
rownames(qb_cluster_data) <- qb_archetype_data$player_display_name

# Determine optimal number of clusters using elbow method
set.seed(123)
wss <- map_dbl(1:10, ~{
  kmeans(qb_cluster_data, centers = .x, nstart = 25)$tot.withinss
})

elbow_plot <- tibble(k = 1:10, wss = wss) %>%
  ggplot(aes(x = k, y = wss)) +
  geom_line() + 
  geom_point() +
  labs(title = "QB Clustering: Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares") +
  theme_minimal()

print(elbow_plot)

# Perform k-means clustering (using k=4 as a starting point)
k_optimal <- 5  # Adjust based on elbow plot
set.seed(123)
qb_kmeans <- kmeans(qb_cluster_data, centers = k_optimal, nstart = 25)

# Add cluster assignments back to original data
qb_archetype_results <- qb_archetype_data %>%
  mutate(archetype = paste0("QB_Type_", qb_kmeans$cluster))

# Analyze archetype characteristics
qb_archetype_summary <- qb_archetype_results %>%
  group_by(archetype) %>%
  summarise(
    count = n(),
    # Future metrics
    avg_next_season_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    # Performance metrics
    avg_epa = mean(passing_epa_per_play, na.rm = TRUE),
    median_epa = median(passing_epa_per_play, na.rm = TRUE),
    # Accuracy metrics  
    avg_expected_comp_pct = mean(expected_completion_percentage, na.rm = TRUE),
    avg_cpoe = mean(completion_percentage_above_expectation, na.rm = TRUE),
    # Arm talent
    avg_max_air_distance = mean(max_completed_air_distance, na.rm = TRUE),
    avg_completed_air_yards = mean(avg_completed_air_yards, na.rm = TRUE),
    # Style metrics
    avg_aggressiveness = mean(aggressiveness, na.rm = TRUE),
    avg_time_to_throw = mean(avg_time_to_throw, na.rm = TRUE),
    avg_air_yards_to_sticks = mean(avg_air_yards_to_sticks, na.rm = TRUE),
    avg_intended_air_yards = mean(avg_intended_air_yards, na.rm = TRUE),
    avg_air_yards_diff = mean(avg_air_yards_differential, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_epa))

cat("\nQB ARCHETYPE CHARACTERISTICS (Ranked by EPA):\n")
print(qb_archetype_summary)

qb_archetype_labels <- qb_archetype_summary %>%
  mutate(
    epa_rank = rank(-avg_epa),
    archetype_label = case_when(
      epa_rank == 1 ~ "System Quarterbacks",
      epa_rank == 2 ~ "Pocket Presences",
      epa_rank == 3 ~ "Struggling Gunslingers",
      epa_rank == 4 ~ "Replacement Level",
      epa_rank == 5 ~ "Bounce-Back Candidates", 
      TRUE ~ archetype
    )
  )

# Update the main results with labels
qb_archetype_final <- qb_archetype_results %>%
  left_join(qb_archetype_labels %>% select(archetype, archetype_label), by = "archetype") %>%
  mutate(archetype_name = archetype_label)

# Display the updated archetype summary with labels
cat("\nQB ARCHETYPE LABELS (k=5):\n")
qb_summary_with_labels <- qb_archetype_summary %>%
  left_join(qb_archetype_labels %>% select(archetype, archetype_label), by = "archetype") %>%
  select(archetype_label, count, avg_epa, avg_cpoe, avg_max_air_distance, avg_aggressiveness) %>%
  arrange(desc(avg_epa))

print(qb_summary_with_labels)

# Updated visualization with new labels
qb_archetype_plot <- qb_archetype_final %>%
  ggplot(aes(x = expected_completion_percentage, y = max_completed_air_distance, color = archetype_name)) +
  geom_point(aes(size = passing_epa_per_play), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size_continuous(name = "EPA/Play", range = c(1, 4)) +
  labs(title = "QB Archetypes (k=5): Accuracy vs Arm Talent",
       subtitle = "Size = EPA per Play, Color = Archetype",
       x = "Expected Completion %",
       y = "Max Completed Air Distance",
       color = "QB Archetype") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

print(qb_archetype_plot)

# Success rates with new labels
qb_success_rates <- qb_archetype_final %>%
  group_by(archetype_name) %>%
  summarise(
    total_players = n(),
    successful_players = sum(passing_epa_per_play > 0, na.rm = TRUE),
    success_rate = successful_players / total_players * 100,
    avg_epa = mean(passing_epa_per_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

cat("\nQB ARCHETYPE SUCCESS RATES:\n")
print(qb_success_rates)

# Normalize to 0-10 scale for radar chart
normalize_to_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 10
}

qb_radar_data <- qb_archetype_summary %>%
  inner_join(select(qb_archetype_labels, archetype, archetype_label),
             by = "archetype") %>%
  mutate(across(where(is.numeric), normalize_to_scale)) %>%
  column_to_rownames("archetype_label") %>%
  select(-archetype)

# Add max and min rows (required by fmsb)
qb_radar_data <- rbind(rep(10, ncol(qb_radar_data)),  # max
                       rep(0, ncol(qb_radar_data)),   # min
                       qb_radar_data) %>%
  select(-c(count, median_epa)) %>%
  rename(`Next Season EPA` = avg_next_season_epa,
         EPA = avg_epa,
         AYD = avg_air_yards_diff,
         IAY = avg_intended_air_yards,
         AYTS = avg_air_yards_to_sticks,
         TT = avg_time_to_throw,
         `AGG%` = avg_aggressiveness,
         CAY = avg_completed_air_yards,
         LCAD = avg_max_air_distance,
         CPOE = avg_cpoe,
         `xCOMP%` = avg_expected_comp_pct)

# Create radar chart
colors <- c("#D62728", "#2CA02C", "#1F77B4", "#FF7F0E", "#9467BD")

library(fmsb)
par(mar = c(2,2,2,10), oma = c(0, 0, 0, 0))

png("visualizations/qb_archetype_radar.png", width = 3000, height = 2000, res = 300)
radarchart(qb_radar_data,
           axistype = 1,
           pcol = colors,
           pfcol = scales::alpha(colors, 0.3),
           plwd = 2,
           plty = 1,
           cglcol = "darkgrey",
           cglty = 1,
           axislabcol = "darkgrey",
           title = "QB Archetype Profiles")

legend(x=1.5, y=1, legend = rownames(qb_radar_data[-c(1,2),]), bty = "n", pch=20, col=colors , text.col = "black", cex=0.8, pt.cex=3, xpd = TRUE)
dev.off()

# Top players by archetype with new labels
cat("\nTOP PLAYERS BY QB ARCHETYPE:\n")
top_qbs_by_type <- qb_archetype_final %>%
  group_by(archetype_name) %>%
  slice_max(passing_epa_per_play, n = 3) %>%
  select(archetype_name, player_display_name, season, passing_epa_per_play, 
         expected_completion_percentage, max_completed_air_distance, aggressiveness) %>%
  arrange(archetype_name, desc(passing_epa_per_play))

print(top_qbs_by_type)

## RB archetype analysis
rb_archetype_data <- combined_rushing_analysis %>%
  filter(season <= 2023) %>%
  filter(rush_attempts >= 100,
         !is.na(efficiency),
         !is.na(rush_yards_over_expected_per_att),
         !is.na(avg_time_to_los),
         !is.na(percent_attempts_gte_eight_defenders)) %>%
  select(player_gsis_id, player_display_name, season,
         all_of(rb_ngs_vars),
         rushing_epa_per_play, rush_attempts) %>%
  group_by(player_gsis_id) %>%
  slice_max(season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(select(rb_prediction_data, current_year, player_gsis_id, next_year_epa_per_play),
            by = c("season" = "current_year", "player_gsis_id"))

cat("RB Archetype analysis using", nrow(rb_archetype_data), "running backs\n")

# Prepare RB data for clustering
rb_cluster_data <- rb_archetype_data %>%
  select(all_of(rb_ngs_vars)) %>%
  scale() %>%
  as.data.frame()

rownames(rb_cluster_data) <- rb_archetype_data$player_display_name

# RB clustering
k_rb <- 5
set.seed(123)
rb_kmeans <- kmeans(rb_cluster_data, centers = k_rb, nstart = 25)

rb_archetype_results <- rb_archetype_data %>%
  mutate(archetype = paste0("RB_Type_", rb_kmeans$cluster))

# RB archetype characteristics
rb_archetype_summary <- rb_archetype_results %>%
  group_by(archetype) %>%
  summarise(
    count = n(),
    # Future performance
    avg_next_season_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    # Performance
    avg_epa = mean(rushing_epa_per_play, na.rm = TRUE),
    # Style metrics
    avg_efficiency = mean(efficiency, na.rm = TRUE),
    avg_time_to_los = mean(avg_time_to_los, na.rm = TRUE), 
    avg_dib_pct = mean(percent_attempts_gte_eight_defenders, na.rm = TRUE),
    # Performance metrics
    avg_ryoe_per_att = mean(rush_yards_over_expected_per_att, na.rm = TRUE),
    avg_ryoe_pct = mean(rush_pct_over_expected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_epa))

cat("\nRB ARCHETYPE CHARACTERISTICS (Ranked by EPA):\n")
print(rb_archetype_summary)

# Create RB archetype labels
rb_archetype_labels <- rb_archetype_summary %>%
  mutate(
    epa_rank = rank(-avg_epa),
    archetype_label = case_when(
      epa_rank == 1 ~ "Unsustainable Elite",
      epa_rank == 2 ~ "Steady System Runners",
      epa_rank == 3 ~ "Limited Grinders", 
      epa_rank == 4 ~ "Ineffective Volume Runners",
      epa_rank == 5 ~ "Bounce-Back Candidates",
      TRUE ~ archetype
    )
  )
rb_archetype_final <- rb_archetype_results %>%
  left_join(rb_archetype_labels %>% select(archetype, archetype_label), by = "archetype") %>%
  mutate(archetype_name = archetype_label)

cat("\nRB ARCHETYPE LABELS:\n")
print(rb_archetype_labels %>% select(archetype, archetype_label, count, avg_epa))

# RB Archetype visualization
rb_archetype_plot <- rb_archetype_final %>%
  ggplot(aes(x = efficiency, y = rush_yards_over_expected_per_att, color = archetype_name)) +
  geom_point(aes(size = rushing_epa_per_play), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size_continuous(name = "EPA/Play", range = c(1, 4)) +
  labs(title = "RB Archetypes: Running Style vs Performance", 
       subtitle = "Size = EPA per Play, Lower efficiency = more direct running",
       x = "Efficiency (Lower = More Direct)",
       y = "Rush Yards Over Expected per Attempt",
       color = "RB Archetype") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(rb_archetype_plot)

# Top RBs by archetype
cat("\nTOP PLAYERS BY RB ARCHETYPE:\n")
top_rbs_by_type <- rb_archetype_final %>%
  group_by(archetype_name) %>%
  slice_max(rushing_epa_per_play, n = 3) %>%
  select(archetype_name, player_display_name, season, rushing_epa_per_play, efficiency, rush_yards_over_expected_per_att) %>%
  arrange(archetype_name, desc(rushing_epa_per_play))

print(top_rbs_by_type)

rb_radar_data <- rb_archetype_summary %>%
  inner_join(select(rb_archetype_labels, archetype, archetype_label),
             by = "archetype") %>%
  mutate(across(where(is.numeric), normalize_to_scale)) %>%
  column_to_rownames("archetype_label") %>%
  select(-archetype)

# Add max and min rows (required by fmsb)
rb_radar_data <- rbind(rep(10, ncol(rb_radar_data)),  # max
                       rep(0, ncol(rb_radar_data)),   # min
                       rb_radar_data) %>%
  select(-c(count)) %>%
  rename(`Next Season EPA` = avg_next_season_epa,
         EPA = avg_epa,
         `RYOE%` = avg_ryoe_pct,
         `RYOE/ATT` = avg_ryoe_per_att,
         # xYDS = avg_expected_rush_yards,
         `8+D%` = avg_dib_pct,
         TLOS = avg_time_to_los,
         EFF = avg_efficiency)

# Create radar chart
colors <- c("#D62728", "#2CA02C", "#1F77B4", "#FF7F0E", "#9467BD")

par(mar = c(2,2,2,10), oma = c(0, 0, 0, 0))

png("visualizations/rb_archetype_radar.png", width = 3000, height = 2000, res = 300)
radarchart(rb_radar_data,
           axistype = 1,
           pcol = colors,
           pfcol = scales::alpha(colors, 0.3),
           plwd = 2,
           plty = 1,
           cglcol = "darkgrey",
           cglty = 1,
           axislabcol = "darkgrey",
           title = "RB Archetype Profiles")
legend(x=1.5, y=1, legend = rownames(rb_radar_data[-c(1,2),]), bty = "n", pch=20, col=colors , text.col = "black", cex=0.8, pt.cex=3, xpd = TRUE)
dev.off()

## WR Archetype Analysis
wr_archetype_data <- combined_receiving_analysis %>%
  filter(season <= 2023) %>%
  filter(targets >= 60,
         !is.na(avg_yac_above_expectation),
         !is.na(percent_share_of_intended_air_yards),
         !is.na(catch_percentage),
         !is.na(avg_separation)) %>%
  select(player_gsis_id, player_display_name, player_position, season,
         all_of(wr_ngs_vars),
         receiving_epa_per_target, targets) %>%
  group_by(player_gsis_id) %>%
  slice_max(season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(select(wr_prediction_data, current_year, player_gsis_id, next_year_epa_per_target),
            by = c("season" = "current_year", "player_gsis_id"))

cat("WR Archetype analysis using", nrow(wr_archetype_data), "wide receivers\n")

# Prepare WR data for clustering
wr_cluster_data <- wr_archetype_data %>%
  filter(player_position == "WR") %>%
  select(all_of(wr_ngs_vars)) %>%
  scale() %>%
  as.data.frame()

rownames(wr_cluster_data) <- wr_archetype_data$player_display_name[wr_archetype_data$player_position == "WR"]

# WR clustering
k_wr <- 5  # More archetypes for WRs given role diversity
set.seed(123) 
wr_kmeans <- kmeans(wr_cluster_data, centers = k_wr, nstart = 25)

wr_archetype_results <- wr_archetype_data %>%
  filter(player_position == "WR") %>%
  mutate(archetype = paste0("WR_Type_", wr_kmeans$cluster))

# WR archetype characteristics
wr_archetype_summary <- wr_archetype_results %>%
  group_by(archetype) %>%
  summarise(
    count = n(),
    # Future performance
    avg_next_season_epa = mean(next_year_epa_per_target, na.rm = TRUE),
    # Performance
    avg_epa = mean(receiving_epa_per_target, na.rm = TRUE),
    # Role metrics
    avg_air_share = mean(percent_share_of_intended_air_yards, na.rm = TRUE),
    avg_target_depth = mean(avg_intended_air_yards, na.rm = TRUE),
    # Skill metrics
    avg_yac = mean(avg_yac, na.rm = TRUE),
    avg_expected_yac = mean(avg_expected_yac, na.rm = TRUE),
    avg_yac_above_exp = mean(avg_yac_above_expectation, na.rm = TRUE),
    avg_catch_pct = mean(catch_percentage, na.rm = TRUE),
    avg_separation = mean(avg_separation, na.rm = TRUE),
    avg_cushion = mean(avg_cushion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_epa))

cat("\nWR ARCHETYPE CHARACTERISTICS (Ranked by EPA):\n")
print(wr_archetype_summary)

wr_archetype_labels <- wr_archetype_summary %>%
  mutate(
    epa_rank = rank(-avg_epa),
    archetype_label = case_when(
      epa_rank == 1 ~ "Elite Deep Threats",
      epa_rank == 2 ~ "Precision Underneath Specialists",
      epa_rank == 3 ~ "Reliable Role Players", 
      epa_rank == 4 ~ "Inconsistent Volume Receivers",
      epa_rank == 5 ~ "Projectable Deep Options",
      TRUE ~ archetype
    )
  )

wr_archetype_final <- wr_archetype_results %>%
  left_join(wr_archetype_labels %>% select(archetype, archetype_label), by = "archetype") %>%
  mutate(archetype_name = archetype_label)

cat("\nWR ARCHETYPE LABELS:\n")
print(wr_archetype_labels %>% select(archetype, archetype_label, count, avg_epa))

# WR Archetype visualization
wr_archetype_plot <- wr_archetype_final %>%
  ggplot(aes(x = percent_share_of_intended_air_yards, y = avg_yac_above_expectation, color = archetype_name)) +
  geom_point(aes(size = receiving_epa_per_target), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size_continuous(name = "EPA/Target", range = c(1, 4)) +
  labs(title = "WR Archetypes: Role vs Playmaking Ability",
       subtitle = "Size = EPA per Target, Color = Archetype", 
       x = "% Share of Team's Intended Air Yards",
       y = "YAC Above Expected",
       color = "WR Archetype") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(wr_archetype_plot)

# Top WRs by archetype
cat("\nTOP PLAYERS BY WR ARCHETYPE:\n")
top_wrs_by_type <- wr_archetype_final %>%
  group_by(archetype_name) %>%
  slice_max(receiving_epa_per_target, n = 3) %>%
  select(archetype_name, player_display_name, season, receiving_epa_per_target, 
         percent_share_of_intended_air_yards, avg_yac_above_expectation, catch_percentage) %>%
  arrange(archetype_name, desc(receiving_epa_per_target))

print(top_wrs_by_type)

wr_radar_data <- wr_archetype_summary %>%
  inner_join(select(wr_archetype_labels, archetype, archetype_label),
             by = "archetype") %>%
  mutate(across(where(is.numeric), normalize_to_scale)) %>%
  column_to_rownames("archetype_label") %>%
  select(-archetype)

# Add max and min rows (required by fmsb)
wr_radar_data <- rbind(rep(10, ncol(wr_radar_data)),  # max
                       rep(0, ncol(wr_radar_data)),   # min
                       wr_radar_data) %>%
  select(-c(count)) %>%
  rename(`Next Season EPA` = avg_next_season_epa,
         EPA = avg_epa,
         CUSH = avg_cushion,
         SEP = avg_separation,
         `CATCH%` = avg_catch_pct,
         `+/-` = avg_yac_above_exp,
         `xYAC/R` = avg_expected_yac,
         `YAC/R` = avg_yac,
         TAY = avg_target_depth,
         `TAY%` = avg_air_share)

# Create radar chart
colors <- c("#D62728", "#2CA02C", "#1F77B4", "#FF7F0E", "#9467BD")

par(mar = c(2,2,2,10), oma = c(0, 0, 0, 0))

png("visualizations/wr_archetype_radar.png", width = 3200, height = 2000, res = 300)
radarchart(wr_radar_data,
           axistype = 1,
           pcol = colors,
           pfcol = scales::alpha(colors, 0.3),
           plwd = 2,
           plty = 1,
           cglcol = "darkgrey",
           cglty = 1,
           axislabcol = "darkgrey",
           title = "WR Archetype Profiles")
legend(x=1.5, y=1, legend = rownames(wr_radar_data[-c(1,2),]), bty = "n", pch=20, col=colors , text.col = "black", cex=0.8, pt.cex=3, xpd = TRUE)
dev.off()

## Archetype success rate
qb_success_rates <- qb_archetype_final %>%
  group_by(archetype_name) %>%
  summarise(
    total_players = n(),
    successful_players = sum(passing_epa_per_play > 0, na.rm = TRUE),
    success_rate = successful_players / total_players * 100,
    avg_epa = mean(passing_epa_per_play, na.rm = TRUE),
    next_season_avg_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

rb_success_rates <- rb_archetype_final %>%
  group_by(archetype_name) %>%
  summarise(
    total_players = n(),
    successful_players = sum(rushing_epa_per_play > 0, na.rm = TRUE),
    success_rate = successful_players / total_players * 100,
    avg_epa = mean(rushing_epa_per_play, na.rm = TRUE),
    next_season_avg_epa = mean(next_year_epa_per_play, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

wr_success_rates <- wr_archetype_final %>%
  group_by(archetype_name) %>%
  summarise(
    total_players = n(),
    successful_players = sum(receiving_epa_per_target > 0, na.rm = TRUE), 
    success_rate = successful_players / total_players * 100,
    avg_epa = mean(receiving_epa_per_target, na.rm = TRUE),
    next_season_avg_epa = mean(next_year_epa_per_target, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

cat("QB ARCHETYPE SUCCESS RATES:\n")
print(qb_success_rates)

cat("\nRB ARCHETYPE SUCCESS RATES:\n")
print(rb_success_rates)

cat("\nWR ARCHETYPE SUCCESS RATES:\n") 
print(wr_success_rates)