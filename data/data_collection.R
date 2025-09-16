#### Load data sources and organize ####
library(tidyverse)
library(nflreadr)
library(nflfastR)

# Load passing NGS data
passing_ngs <- load_nextgen_stats(stat_type = "passing")
# Regular season summary
passing_ngs <- passing_ngs %>%
  filter(week == 0)
# Select wanted NGS
passing_ngs <- passing_ngs %>%
  select(season, player_display_name, player_position, player_gsis_id, team_abbr,
         avg_time_to_throw:max_air_distance)

# Load receiving NGS data  
receiving_ngs <- load_nextgen_stats(stat_type = "receiving")
# Regular season summary
receiving_ngs <- receiving_ngs %>%
  filter(week == 0)
# Select wanted NGS
receiving_ngs <- receiving_ngs %>%
  select(season, player_display_name, player_position, player_gsis_id, team_abbr,
         avg_cushion:avg_yac_above_expectation)

# Load rushing NGS data
rushing_ngs <- load_nextgen_stats(stat_type = "rushing")
# Regular season summary
rushing_ngs <- rushing_ngs %>%
  filter(week == 0)
# Select wanted NGS
rushing_ngs <- rushing_ngs %>%
  select(season, player_display_name, player_position, player_gsis_id, team_abbr,
         efficiency:rush_touchdowns,
         expected_rush_yards:rush_pct_over_expected)

# Load play-by-play data
pbp <- load_pbp(seasons = 2016:2024)

# Update EPA aggregations to include GSIS IDs for joining
passing_epa <- pbp %>%
  filter(
    !is.na(passer_player_id), 
    !is.na(epa),
    play_type == "pass"
  ) %>%
  group_by(season, passer_player_id) %>%
  summarize(
    total_passing_epa = sum(epa, na.rm = TRUE),
    attempts = n(),
    passing_epa_per_play = total_passing_epa / attempts,
    total_air_epa = sum(air_epa, na.rm = TRUE),
    total_yac_epa = sum(yac_epa, na.rm = TRUE),
    successful_passes = sum(epa > 0, na.rm = TRUE),
    pass_success_rate = successful_passes / attempts,
    .groups = "drop"
  ) %>%
  select(-attempts) %>%
  rename(player_gsis_id = passer_player_id) %>%
  arrange(desc(total_passing_epa))

rushing_epa <- pbp %>%
  filter(
    !is.na(rusher_player_id), 
    !is.na(epa),
    play_type == "run"
  ) %>%
  group_by(season, rusher_player_id) %>%
  summarize(
    total_rushing_epa = sum(epa, na.rm = TRUE),
    rush_attempts = n(),
    rushing_epa_per_play = total_rushing_epa / rush_attempts,
    successful_rushes = sum(epa > 0, na.rm = TRUE),
    rush_success_rate = successful_rushes / rush_attempts,
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    avg_rush_yards = mean(rushing_yards, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-c(rush_attempts, avg_rush_yards, total_rush_yards)) %>%
  rename(player_gsis_id = rusher_player_id) %>%
  arrange(desc(total_rushing_epa))

receiving_epa <- pbp %>%
  filter(
    !is.na(receiver_player_id), 
    !is.na(epa),
    play_type == "pass"
  ) %>%
  group_by(season, receiver_player_id) %>%
  summarize(
    total_receiving_epa = sum(epa, na.rm = TRUE),
    targets = n(),
    receiving_epa_per_target = total_receiving_epa / targets,
    receptions = sum(!is.na(receiving_yards), na.rm = TRUE),
    receiving_epa_completions = sum(epa[!is.na(receiving_yards)], na.rm = TRUE),
    receiving_epa_per_reception = receiving_epa_completions / receptions,
    total_air_epa = sum(air_epa, na.rm = TRUE),
    total_yac_epa = sum(yac_epa, na.rm = TRUE),
    successful_targets = sum(epa > 0, na.rm = TRUE),
    target_success_rate = successful_targets / targets,
    total_receiving_yards = sum(receiving_yards, na.rm = TRUE),
    catch_rate = receptions / targets,
    .groups = "drop"
  ) %>%
  select(-c(targets, receptions, total_receiving_yards, catch_rate)) %>%
  rename(player_gsis_id = receiver_player_id) %>%
  arrange(desc(total_receiving_epa))

combined_epa <- pbp %>%
  filter(!is.na(epa), play_type %in% c("pass", "run")) %>%
  mutate(
    # Create unified player ID and name fields
    player_gsis_id = case_when(
      !is.na(passer_player_id) ~ passer_player_id,
      !is.na(rusher_player_id) ~ rusher_player_id,
      !is.na(receiver_player_id) ~ receiver_player_id,
      TRUE ~ NA_character_
    ),
    # Create play involvement flag
    involvement_type = case_when(
      !is.na(passer_player_id) ~ "passing",
      !is.na(rusher_player_id) ~ "rushing",
      !is.na(receiver_player_id) ~ "receiving",
      TRUE ~ "other"
    )
  ) %>%
  filter(!is.na(player_gsis_id)) %>%
  group_by(season, player_gsis_id) %>%
  summarize(
    # Total offensive EPA across all play types
    total_offensive_epa = sum(epa, na.rm = TRUE),
    total_offensive_plays = n(),
    offensive_epa_per_play = total_offensive_epa / total_offensive_plays,
    
    # EPA breakdown by play type
    passing_epa = sum(epa[involvement_type == "passing"], na.rm = TRUE),
    rushing_epa = sum(epa[involvement_type == "rushing"], na.rm = TRUE),
    receiving_epa = sum(epa[involvement_type == "receiving"], na.rm = TRUE),
    
    # Play counts by type
    passing_plays = sum(involvement_type == "passing"),
    rushing_plays = sum(involvement_type == "rushing"),
    receiving_plays = sum(involvement_type == "receiving"),
    
    # EPA per play by type (avoid division by zero)
    passing_epa_per_play = ifelse(passing_plays > 0, passing_epa / passing_plays, 0),
    rushing_epa_per_play = ifelse(rushing_plays > 0, rushing_epa / rushing_plays, 0),
    receiving_epa_per_play = ifelse(receiving_plays > 0, receiving_epa / receiving_plays, 0),
    
    # Success rates by play type
    passing_success_rate = ifelse(passing_plays > 0, 
                                  sum(epa[involvement_type == "passing"] > 0, na.rm = TRUE) / passing_plays, 0),
    rushing_success_rate = ifelse(rushing_plays > 0,
                                  sum(epa[involvement_type == "rushing"] > 0, na.rm = TRUE) / rushing_plays, 0),
    receiving_success_rate = ifelse(receiving_plays > 0,
                                    sum(epa[involvement_type == "receiving"] > 0, na.rm = TRUE) / receiving_plays, 0),
    
    # Determine primary role (most plays)
    primary_role = case_when(
      passing_plays >= rushing_plays & passing_plays >= receiving_plays ~ "QB",
      rushing_plays >= receiving_plays ~ "RB", 
      TRUE ~ "WR/TE"
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(total_offensive_epa))

# rosters <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv") %>%
#   group_by(season, playerid, full_name) %>%
#   summarise(games = sum(games, na.rm = T),
#             starts = sum(starts, na.rm = T),
#             years = sum(years, na.rm = T),
#             av = sum(av, na.rm = T)) 
# 
player_mappings <- load_players() %>%
  mutate(birth_year = year(as.Date(birth_date))) %>%
  rename(player_gsis_id = gsis_id) %>%
  select(player_gsis_id, birth_year, rookie_season)

combined_passing_analysis <- passing_epa %>%
  left_join(player_mappings, by = c("player_gsis_id")) %>%
  inner_join(passing_ngs,
             by = c("season", "player_gsis_id")) %>%
  # left_join(select(rosters, season, playerid, av),
  #            by = c("season", "pfr_id" = "playerid"))
  mutate(age = as.numeric(season) - as.numeric(birth_year),
         years_exp = as.numeric(season) - as.numeric(rookie_season)) %>%
  select(season, player_gsis_id, player_display_name, player_position, team_abbr, age, years_exp,
         total_passing_epa:pass_success_rate,
         avg_time_to_throw:max_air_distance)

combined_rushing_analysis <- rushing_epa %>%
  left_join(player_mappings, by = c("player_gsis_id")) %>%
  inner_join(rushing_ngs,
             by = c("season", "player_gsis_id")) %>%
  mutate(age = as.numeric(season) - as.numeric(birth_year),
         years_exp = as.numeric(season) - as.numeric(rookie_season)) %>%
  select(season, player_gsis_id, player_display_name, player_position, team_abbr, age, years_exp,
         total_rushing_epa:rush_success_rate,
         efficiency:rush_pct_over_expected) %>%
  filter(season >= 2018)

combined_receiving_analysis <- receiving_epa %>%
  left_join(player_mappings, by = c("player_gsis_id")) %>%
  inner_join(receiving_ngs,
             by = c("season", "player_gsis_id")) %>%
  mutate(age = as.numeric(season) - as.numeric(birth_year),
         years_exp = as.numeric(season) - as.numeric(rookie_season)) %>%
  select(season, player_gsis_id, player_display_name, player_position, team_abbr, age, years_exp,
         total_receiving_epa:target_success_rate,
         avg_cushion:avg_yac_above_expectation)

qb_ngs_vars <- c("avg_time_to_throw", "avg_completed_air_yards", "avg_intended_air_yards", "avg_air_yards_differential", "aggressiveness", "max_completed_air_distance", "avg_air_yards_to_sticks", "expected_completion_percentage", "completion_percentage_above_expectation")

rb_ngs_vars <- c("efficiency", "percent_attempts_gte_eight_defenders", "avg_time_to_los", "rush_yards_over_expected_per_att", "rush_pct_over_expected")

wr_ngs_vars <- c("avg_cushion", "avg_separation", "avg_intended_air_yards", "percent_share_of_intended_air_yards", "catch_percentage", "avg_yac", "avg_expected_yac", "avg_yac_above_expectation")