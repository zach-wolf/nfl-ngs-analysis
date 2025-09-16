source("data/data_collection.R")

#### Data exploration ####
qb_ngs_data <- combined_passing_analysis %>%
  select(all_of(qb_ngs_vars)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

qb_ngs_dist <- ggplot(qb_ngs_data, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "black", linewidth = 0.5, alpha = 0.8) +
  facet_wrap(~metric, scales = "free", ncol = 3) +
  labs(title = "Quarterback NGS Metric Distributions",
       subtitle = "Understanding the range and shape of each metric") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(qb_ngs_dist,
       filename = "visualizations/qb_ngs_dist.png",
       width = 9,
       height = 7)

rb_ngs_data <- combined_rushing_analysis %>%
  select(all_of(rb_ngs_vars)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

rb_ngs_dist <- ggplot(rb_ngs_data, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "black", linewidth = 0.5, alpha = 0.8) +
  facet_wrap(~metric, scales = "free", ncol = 3) +
  labs(title = "Running Back NGS Metric Distributions",
       subtitle = "Understanding the range and shape of each metric") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(rb_ngs_dist,
       filename = "visualizations/rb_ngs_dist.png",
       width = 9,
       height = 7)

wr_ngs_data <- combined_receiving_analysis %>%
  select(all_of(wr_ngs_vars)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

wr_ngs_dist <- ggplot(wr_ngs_data, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "black", linewidth = 0.5, alpha = 0.8) +
  facet_wrap(~metric, scales = "free", ncol = 3) +
  labs(title = "Wide Receiver NGS Metric Distributions",
       subtitle = "Understanding the range and shape of each metric") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(wr_ngs_dist,
       filename = "visualizations/wr_ngs_dist.png",
       width = 9,
       height = 7)

qb_epa_data <- combined_passing_analysis %>%
  select(season, passing_epa_per_play) %>%
  mutate(position = "QB") %>%
  rename(epa_per_play = passing_epa_per_play) %>%
  filter(!is.na(epa_per_play))

rb_epa_data <- combined_rushing_analysis %>%
  select(season, rushing_epa_per_play) %>%
  mutate(position = "RB") %>%
  rename(epa_per_play = rushing_epa_per_play) %>%
  filter(!is.na(epa_per_play))

wr_epa_data <- combined_receiving_analysis %>%
  select(season, receiving_epa_per_target) %>%
  mutate(position = "WR") %>%
  rename(epa_per_play = receiving_epa_per_target) %>%
  filter(!is.na(epa_per_play))

# Combine EPA data
all_epa_data <- bind_rows(qb_epa_data, rb_epa_data, wr_epa_data)

# EPA distribution plot
epa_by_pos <- ggplot(all_epa_data, aes(x = epa_per_play, fill = position)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~position, scales = "free_y", ncol = 1) +
  scale_fill_viridis_d() +
  labs(title = "EPA Performance Distributions by Position",
       subtitle = "Comparing EPA efficiency across QB, RB, and WR",
       x = "EPA per Play/Target",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(epa_by_pos,
       filename = "visualizations/epa_pos_dist.png",
       width = 9,
       height = 7)
