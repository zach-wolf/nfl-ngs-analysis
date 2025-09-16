source("scripts/ngs_composite_scores.R")
source("scripts/player_archetypes.R")

## QB ##
qb_model <- lm(data = qb_predictive_composite %>% rename(composite_score = qb_predictive_composite_score), 
               formula = next_year_epa_per_play ~ age + years_exp + composite_score + passing_epa_per_play)
summary(qb_model)

qb_pred_set <- combined_passing_analysis %>%
  inner_join(qb_2024_composite, by = c("player_gsis_id", "season", "player_display_name", "player_position", "team_abbr"))

qb_2024_pred <- predict(qb_model, qb_pred_set) %>%
  cbind(qb_pred_set) %>%
  arrange(desc(.))

## 0.23 R^2 for QBs, not terrible for 4 variables!

# Save the cluster centers and scaling parameters
qb_cluster_centers <- qb_kmeans$centers
qb_cluster_vars <- colnames(qb_cluster_data)

# First, let's properly save the scaling parameters from your original process
qb_scale_params <- qb_archetype_data %>%
  select(all_of(qb_ngs_vars)) %>%
  scale()

qb_scale_center <- attr(qb_scale_params, "scaled:center")
qb_scale_scale <- attr(qb_scale_params, "scaled:scale")

# Corrected assignment function
assign_qb_archetype <- function(new_qb_data) {
  
  # Select the same variables and apply the SAME scaling as original
  new_qb_scaled <- new_qb_data %>%
    select(all_of(qb_ngs_vars)) %>%
    scale(center = qb_scale_center, scale = qb_scale_scale) %>%
    as.numeric()  # Convert to numeric vector
  
  # Calculate distances to each cluster center
  distances <- apply(qb_cluster_centers, 1, function(center) {
    sqrt(sum((new_qb_scaled - center)^2))
  })
  
  # Assign to nearest cluster
  assigned_cluster <- which.min(distances)
  
  return(list(
    archetype = paste0("QB_Type_", assigned_cluster),
    cluster_number = assigned_cluster,
    distances = distances,
    confidence = min(distances) / mean(distances)
  ))
}

assign_multiple_qbs <- function(qb_dataframe) {
  qb_dataframe %>%
    rowwise() %>%
    mutate(
      assignment = list(assign_qb_archetype(cur_data())),
      archetype = assignment$archetype,
      confidence = assignment$confidence
    ) %>%
    select(-assignment)
}

qb_2024_pred <- assign_multiple_qbs(qb_2024_pred) 
qb_2024_pred <- qb_2024_pred %>%
  left_join(select(qb_archetype_labels, archetype, archetype_label), 
            by = "archetype") %>%
  rename(pred_epa = 1) %>%
  select(season, player_display_name, pred_epa, composite_score, passing_epa_per_play, age, years_exp, archetype_label, confidence)


## RB ##
rb_model <- lm(data = rb_predictive_composite %>% rename(composite_score = rb_predictive_composite_score), 
               formula = next_year_epa_per_play ~ age + years_exp + composite_score + rushing_epa_per_play)
summary(rb_model)

rb_pred_set <- combined_rushing_analysis %>%
  inner_join(rb_2024_composite, by = c("player_gsis_id", "season", "player_display_name", "player_position", "team_abbr"))

rb_2024_pred <- predict(rb_model, rb_pred_set) %>%
  cbind(rb_pred_set) %>%
  arrange(desc(.))

# RB is hard to predict!

# Save the cluster centers and scaling parameters
rb_cluster_centers <- rb_kmeans$centers
rb_cluster_vars <- colnames(rb_cluster_data)

# First, let's properly save the scaling parameters from your original process
rb_scale_params <- rb_archetype_data %>%
  select(all_of(rb_ngs_vars)) %>%
  scale()

rb_scale_center <- attr(rb_scale_params, "scaled:center")
rb_scale_scale <- attr(rb_scale_params, "scaled:scale")

# Corrected assignment function
assign_rb_archetype <- function(new_rb_data) {
  
  # Select the same variables and apply the SAME scaling as original
  new_rb_scaled <- new_rb_data %>%
    select(all_of(rb_ngs_vars)) %>%
    scale(center = rb_scale_center, scale = rb_scale_scale) %>%
    as.numeric()  # Convert to numeric vector
  
  # Calculate distances to each cluster center
  distances <- apply(rb_cluster_centers, 1, function(center) {
    sqrt(sum((new_rb_scaled - center)^2))
  })
  
  # Assign to nearest cluster
  assigned_cluster <- which.min(distances)
  
  return(list(
    archetype = paste0("RB_Type_", assigned_cluster),
    cluster_number = assigned_cluster,
    distances = distances,
    confidence = min(distances) / mean(distances)
  ))
}

assign_multiple_rbs <- function(rb_dataframe) {
  rb_dataframe %>%
    rowwise() %>%
    mutate(
      assignment = list(assign_rb_archetype(cur_data())),
      archetype = assignment$archetype,
      confidence = assignment$confidence
    ) %>%
    select(-assignment)
}

rb_2024_pred <- assign_multiple_rbs(rb_2024_pred) 
rb_2024_pred <- rb_2024_pred %>%
  left_join(select(rb_archetype_labels, archetype, archetype_label), 
            by = "archetype") %>%
  rename(pred_epa = 1) %>%
  select(season, player_display_name, pred_epa, composite_score, rushing_epa_per_play, age, years_exp, archetype_label, confidence)

## WR ##
wr_model <- lm(data = wr_predictive_composite %>% rename(composite_score = wr_predictive_composite_score), 
               formula = next_year_epa_per_target ~ years_exp + composite_score + receiving_epa_per_target)
summary(wr_model)

wr_pred_set <- combined_receiving_analysis %>%
  inner_join(wr_2024_composite, by = c("player_gsis_id", "season", "player_display_name", "player_position", "team_abbr"))

wr_2024_pred <- predict(wr_model, wr_pred_set) %>%
  cbind(wr_pred_set) %>%
  arrange(desc(.))

# WR is better, but not by much

# Save the cluster centers and scaling parameters
wr_cluster_centers <- wr_kmeans$centers
wr_cluster_vars <- colnames(wr_cluster_data)

# First, let's properly save the scaling parameters from your original process
wr_scale_params <- wr_archetype_data %>%
  select(all_of(wr_ngs_vars)) %>%
  scale()

wr_scale_center <- attr(wr_scale_params, "scaled:center")
wr_scale_scale <- attr(wr_scale_params, "scaled:scale")

# Corrected assignment function
assign_wr_archetype <- function(new_wr_data) {
  
  # Select the same variables and apply the SAME scaling as original
  new_wr_scaled <- new_wr_data %>%
    select(all_of(wr_ngs_vars)) %>%
    scale(center = wr_scale_center, scale = wr_scale_scale) %>%
    as.numeric()  # Convert to numeric vector
  
  # Calculate distances to each cluster center
  distances <- apply(wr_cluster_centers, 1, function(center) {
    sqrt(sum((new_wr_scaled - center)^2))
  })
  
  # Assign to nearest cluster
  assigned_cluster <- which.min(distances)
  
  return(list(
    archetype = paste0("WR_Type_", assigned_cluster),
    cluster_number = assigned_cluster,
    distances = distances,
    confidence = min(distances) / mean(distances)
  ))
}

assign_multiple_wrs <- function(wr_dataframe) {
  wr_dataframe %>%
    rowwise() %>%
    mutate(
      assignment = list(assign_wr_archetype(cur_data())),
      archetype = assignment$archetype,
      confidence = assignment$confidence
    ) %>%
    select(-assignment)
}

wr_2024_pred <- assign_multiple_wrs(wr_2024_pred) 
wr_2024_pred <- wr_2024_pred %>%
  left_join(select(wr_archetype_labels, archetype, archetype_label), 
            by = "archetype") %>%
  rename(pred_epa = 1) %>%
  select(season, player_display_name, pred_epa, composite_score, receiving_epa_per_target, age, years_exp, archetype_label, confidence)
