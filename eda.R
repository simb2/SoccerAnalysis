# Data exploration
library(data.table)
library(tidyverse)
library(fst)
df <- as.data.table(read.fst('train.fst'))


predvars <- setdiff(names(df), c('match_id', 'event_name', '.row_id', 
  'cutoff', 'is_home', 'sub_event_id', 'goal_scored', 'dateutc')) # I'll use dateutc for impuation later on. 
predvars

factor_vars <- c('league', 'match_period')
# non predictors:
# match_id, event_name, .row_id, cutoff, is_home

## Relationship with response (home goals remaining)

count_vars_home <- grep("^count_home", names(df), value = TRUE)

count_vars_away <- grep("^count_away", names(df), value = TRUE)

str_vars <- grep('_str$', names(df), value = TRUE)

df <- df |> slice_sample(n = min(100000, nrow(df)))

df |>
  select(league, home_goals_remaining, current_home_score, all_of(count_vars_home)) |>
  mutate(total_goals = home_goals_remaining + current_home_score) |>
  pivot_longer(all_of(count_vars_home), names_to = "predictor",
               values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_home_")) |>
  group_by(predictor, league, value) |>
  summarise(total_goals = mean(total_goals), .groups = "drop") |>
  ggplot(aes(x = value, y = total_goals, colour = league)) +
  geom_smooth() +
  facet_wrap(~predictor, scales = "free_x")

df |>
  select(league, away_goals_remaining, current_away_score, all_of(count_vars_away)) |>
  mutate(total_goals = away_goals_remaining + current_away_score) |>
  pivot_longer(all_of(count_vars_away), names_to = "predictor",
               values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_away_")) |>
  group_by(predictor, league, value) |>
  summarise(total_goals = mean(total_goals), .groups = "drop") |>
  ggplot(aes(x = value, y = total_goals, colour = league)) +
  geom_smooth() +
  facet_wrap(~predictor, scales = "free_x")

df |>
  select(league, away_goals_remaining, current_away_score, all_of(str_vars)) |>
  mutate(total_goals = away_goals_remaining + current_away_score) |>
  pivot_longer(all_of(str_vars), names_to = "predictor",
               values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_away_")) |>
  group_by(predictor, league, value) |>
  summarise(total_goals = mean(total_goals), .groups = "drop") |>
  ggplot(aes(x = value, y = total_goals, colour = league)) +
  geom_smooth() +
  facet_wrap(~predictor, scales = "free_x")


df |>
  select(league, all_of(count_vars_home)) |>
  GGally::ggpairs(aes(colour = league, alpha = 0.4))
df |>
  select(league, all_of(count_vars_away)) |>
  GGally::ggpairs(aes(colour = league, alpha = 0.4))


df |>
  select(league, all_of(count_vars_home)) |>
  GGally::ggpairs(aes(colour = duration, alpha = 0.4))


