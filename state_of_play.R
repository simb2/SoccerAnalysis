library(fst)
library(tidyverse)
library(data.table)
library(mice)
library(VIM)
library(rsample)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
set.seed(1)

# I wanna do a better job of this first. And then I will do something else.

# --- Load Data ----------------------------------------------------------------

df <- data.table::as.data.table(fst::read.fst('events_processed.fst'))
df <- df |> janitor::clean_names()
df[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
# Goals are likely well-recorded; treat missing values as no goal scored.
colSums(is.na(df))
df[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

# Some of the event types will not be included
# `non-debut` missingness by event type
df |> group_by(event_name) |> 
  filter(is_debut == 0 | is.na(is_debut)) |> 
  summarise(count = sum(is.na(ewma_rank))/n()) |> 
  ggplot(aes(x = event_name, y = count)) + 
  geom_col()

df <- df |> dplyr::filter(
  !(event_name %in% c('Interruption', 'Goalkeeper leaving line', 'Save attempt'))
)
# ---- Creating features --------------------------------------------------

df_wide <- df |>
  mutate(
    event_side = paste0(ifelse(is_home, "home", "away"), "_", event_name),
    n1 = 1
  ) |>
  pivot_wider(
    names_from  = event_side,
    values_from = n1,
    values_fn   = length,
    values_fill = 0
  ) |>
  clean_names()

event_cols <- df_wide |>
  select(matches("^home_|^away_")) |>
  select(-c('home_mean_str', 'home_max_str', 'home_id', 
            'home_min_str', 'home_balance', 'away_id', 
            'away_mean_str', 'away_min_str', 'away_max_str', 'away_balance')) |> 
  names()

df <- df_wide |>
  arrange(match_id, event_sec) |>
  group_by(match_id) |>
  mutate(
    across(
      all_of(event_cols),
      cumsum,
      .names = "count_{.col}"
    )
  ) |>
  ungroup()

df_wide <- NULL

df <- df |>
  select(-all_of(event_cols))

df <- data.table::as.data.table(df)
setorder(df, match_id, event_sec)
df[, `:=`(
  current_home_score = cumsum(fifelse(is_home, goal*1, 0L)) + cumsum(fifelse(!is_home, own_goal*1, 0L)),
  current_away_score = cumsum(fifelse(!is_home, goal*1, 0L)) + cumsum(fifelse(is_home, own_goal*1, 0L))
), by = c('match_id','dateutc')]
df[, `:=`(
  home_goals_remaining = sum(fifelse(is_home, goal*1, 0L)) + sum(fifelse(!is_home, own_goal*1, 0L))  - current_home_score,
  away_goals_remaining = sum(fifelse(!is_home, goal*1, 0L)) + sum(fifelse(is_home, own_goal*1, 0L)) - current_away_score
), by = match_id]

df |>
  group_by(match_id) |>
  summarise(max = max(count_home_free_kick)) 
df |>
  group_by(match_id) |>
  summarise(max = max(count_home_corner_kick)) 
df |>
  group_by(match_id) |>
  summarise(max = max(current_home_score)) 
gc()
df |>
  group_by(match_id) |>
  summarise(max = max(current_home_score)) 
gc()
df <- data.table::as.data.table(df)
df[, c('sub_event_name') := NULL]


df <- df[, c('home_id', 'away_id') := NULL]



df[!df$is_home, pos_orig_x := -pos_orig_x + 100]
df[!df$is_home, pos_orig_y := -pos_orig_y + 100]
df[!df$is_home, pos_dest_x := -pos_dest_x + 100]
df[!df$is_home, pos_dest_y := -pos_dest_y + 100]

df[, goal := NULL]
fst::write.fst(df, 'clnd_df.fst')

