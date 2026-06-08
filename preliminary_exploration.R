library(tidyverse)
library(data.table)
library(fst)



# --- Matches By date, league. As well as missing value exploration.
df <- as.data.table(fst::read.fst('clnd_df.fst'))
df |> group_by(event_name) |> 
  filter(is_debut == 0 | is.na(is_debut)) |> 
  summarise(count = sum(is.na(ewma_rank))/n()) |> 
  ggplot(aes(x = event_name, y = count)) + 
  geom_col()
# Time series of missing values by date
df |>
  mutate(date = as.Date(dateutc)) |>
  group_by(date) |>
  summarise(debut = sum(is_debut, na.rm = TRUE)/n()) |>
  filter(debut > 0 ) |>
  ggplot(aes(x = date, y = debut)) +
  geom_col() +
  labs(title = 'Missing Values Over Time', x = 'Date (UTC)',
       y = 'Proportion Missing') +
  theme_minimal()

# Most 'debut' matches happen at the begining of each eason (which makes sense)
df |>
  mutate(date = as.Date(dateutc)) |>
  group_by(date, league) |>
  count() |>
  ggplot(aes(x = date, y = n, colour = league, fill = league)) +
  geom_col() + 
  labs(title = 'Matches by Date', x = 'Date (UTC)',
       y = 'Number of Matches') +
  theme_minimal()

# For ewma_rank
df[, ewma_missing := (is.na(ewma_rank))]
df[, league := as.factor(league)]




ggplot(df, aes(x = event_sec, fill = ewma_missing)) +
  geom_density(alpha = 0.5) +
  labs(title = 'Missingness vs Time', x = 'Second', 
       fill = 'Missing Player Ranking') +
  theme_minimal()

df |>
  group_by(league) |>
  summarize(count = sum(ewma_missing)/n()) |>
  ggplot(aes(x = league, y = count)) +
  geom_col() +
  labs(title = 'Missingness vs League') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Time series of missing values by date
df |>
  mutate(date = as.Date(dateutc)) |>
  group_by(date) |>
  filter(is_debut == 0 | is.na(is_debut)) |> 
  summarise(across(everything(), function(x) mean(is.na(x)))) |>
  select(date, where(function(x) any(x > 0))) |>
  pivot_longer(-date, names_to = "variable", values_to = "prop_missing") |>
  ggplot(aes(x = date, y = prop_missing, color = variable)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5,position = 'jitter') + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Missing Values Over Time', x = 'Date (UTC)',
       y = 'Proportion Missing', color = 'Variable') +
  theme_minimal() +
  theme(legend.position = 'bottom')

df |>
  mutate(date = as.Date(dateutc)) |>
  group_by(date) |>
  filter(is_debut == 0 | is.na(is_debut)) |> 
  summarise(across(everything(), function(x) mean(is.na(x)))) |>
  select(date, ewma_rank, home_balance, away_balance) |>
  pivot_longer(-date, names_to = "variable", values_to = "prop_missing") |>
  ggplot(aes(x = date, y = prop_missing, color = variable)) +
  geom_point(alpha = 0.5,position = 'dodge') + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Missing Values Over Time', x = 'Date (UTC)',
       y = 'Proportion Missing', color = 'Variable') +
  theme_minimal() +
  theme(legend.position = 'bottom')


df |>
  filter(is_debut == 0 | is.na(is_debut)) |>
  ggplot(aes(x = pos_orig_x, y = pos_orig_y)) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c() +
  facet_wrap(~ewma_missing + event_name) +
  theme_minimal()


# positions of players by event (non missing)

df |> 
  filter(is_debut == 0) |> 
  ggplot(aes(x = pos_orig_x, y = pos_orig_y)) + 
  geom_hex(bins = 30) + 
  scale_fill_viridis_c() + 
  facet_wrap(~event_name) + 
  theme_minimal()

df |> 
  ggplot(aes(x = pos_orig_x, y = pos_orig_y)) + 
  geom_hex(bins = 30) + 
  scale_fill_viridis_c() + 
  facet_wrap(~event_name) + 
  theme_minimal()



# Boxplots of count predictors by missingness
df |>
  filter(is_debut == 0 | is.na(is_debut)) |>
  select(ewma_missing, starts_with("count_")) |>
  pivot_longer(-ewma_missing, names_to = "predictor", values_to = "value") |>
  mutate(ewma_missing = factor(ewma_missing, labels = c("Observed", "Missing"))) |>
  ggplot(aes(x = ewma_missing, y = value, fill = ewma_missing)) +
  geom_boxplot(outlier.alpha = 0.05) +
  facet_wrap(~predictor, scales = "free_y") +
  labs(title = 'Count Predictors by Missingness of Player Ranking',
       x = NULL, y = 'Cumulative Count') +
  theme_minimal() +
  theme(legend.position = 'none')

# I won't have to consider corner kicks to be mar.


