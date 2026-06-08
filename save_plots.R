library(fst)
library(data.table)
library(tidyverse)
library(janitor)
library(GGally)
library(here)

dir.create(here("plots"), showWarnings = FALSE)

# ---- 1. EWMA decay by alpha (no data needed) ---------------------------------
png(here("plots", "ewma_decay.png"), width = 800, height = 500)
alphas <- c(0.1, 0.3, 0.5, 0.7)
decay <- sapply(alphas, function(a) a * (1 - a)^(0:15))
matplot(decay,
  type = "l", lty = 1, lwd = 2,
  col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
  xlab = "Games ago", ylab = "Weight",
  main = "EWMA decay by alpha"
)
legend("topright", legend = paste("alpha =", alphas),
       col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"), lty = 1, lwd = 2)
dev.off()

# ---- 2. Non-debut EWMA missingness by event type ----------------------------
events_proc <- data.table::as.data.table(fst::read.fst(here("events_processed.fst")))
events_proc <- janitor::clean_names(events_proc)

p_missing <- events_proc |>
  group_by(event_name) |>
  filter(is_debut == 0 | is.na(is_debut)) |>
  summarise(missing_rate = sum(is.na(ewma_rank)) / n(), .groups = "drop") |>
  ggplot(aes(x = reorder(event_name, missing_rate), y = missing_rate)) +
  geom_col(fill = "#4e79a7") +
  coord_flip() +
  labs(
    title = "EWMA rank missingness rate by event type (non-debut players)",
    x = NULL, y = "Proportion missing"
  ) +
  theme_minimal(base_size = 12)

ggsave(here("plots", "ewma_missing_by_event.png"), p_missing,
       width = 9, height = 5, dpi = 150)

events_proc <- NULL; gc()

# ---- 3-4. Cumulative event counts vs total goals (train set) ----------------
set.seed(1)
train <- data.table::as.data.table(fst::read.fst(here("train.fst")))
train <- train |> slice_sample(n = min(100000, nrow(train)))

count_vars_home <- grep("^count_home", names(train), value = TRUE)
count_vars_away <- grep("^count_away", names(train), value = TRUE)

p_home_counts <- train |>
  select(league, home_goals_remaining, current_home_score, all_of(count_vars_home)) |>
  mutate(total_goals = home_goals_remaining + current_home_score) |>
  pivot_longer(all_of(count_vars_home), names_to = "predictor", values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_home_")) |>
  group_by(predictor, league, value) |>
  summarise(total_goals = mean(total_goals), .groups = "drop") |>
  ggplot(aes(x = value, y = total_goals, colour = league)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~predictor, scales = "free_x") +
  labs(
    title = "Mean total home goals vs cumulative home event counts",
    x = "Cumulative event count", y = "Mean total home goals"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("plots", "home_counts_vs_goals.png"), p_home_counts,
       width = 14, height = 9, dpi = 150)

p_away_counts <- train |>
  select(league, away_goals_remaining, current_away_score, all_of(count_vars_away)) |>
  mutate(total_goals = away_goals_remaining + current_away_score) |>
  pivot_longer(all_of(count_vars_away), names_to = "predictor", values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_away_")) |>
  group_by(predictor, league, value) |>
  summarise(total_goals = mean(total_goals), .groups = "drop") |>
  ggplot(aes(x = value, y = total_goals, colour = league)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~predictor, scales = "free_x") +
  labs(
    title = "Mean total away goals vs cumulative away event counts",
    x = "Cumulative event count", y = "Mean total away goals"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("plots", "away_counts_vs_goals.png"), p_away_counts,
       width = 14, height = 9, dpi = 150)

# ---- 5. Response variable distributions ------------------------------------
p_response <- train |>
  select(league, home_goals_remaining, away_goals_remaining) |>
  pivot_longer(c(home_goals_remaining, away_goals_remaining),
               names_to = "side", values_to = "goals_remaining") |>
  mutate(side = ifelse(side == "home_goals_remaining", "Home", "Away")) |>
  ggplot(aes(x = goals_remaining, fill = side)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  facet_wrap(~league, scales = "free_y") +
  scale_fill_manual(values = c("Home" = "#4e79a7", "Away" = "#f28e2b")) +
  labs(
    title = "Distribution of goals remaining at each event",
    x = "Goals remaining", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("plots", "goals_remaining_dist.png"), p_response,
       width = 12, height = 7, dpi = 150)

# ---- 6. GGally pair plot for home count vars (subset) ----------------------
pair_cols <- c("league", intersect(count_vars_home, names(train))[1:5])
pair_sample <- train |>
  select(all_of(pair_cols)) |>
  slice_sample(n = 5000)

p_pairs_home <- GGally::ggpairs(pair_sample,
  aes(colour = league, alpha = 0.4),
  upper = list(continuous = "cor"),
  lower = list(continuous = GGally::wrap("points", size = 0.3)),
  title = "Pairwise relationships: home cumulative counts (sample n=5 000)"
)

ggsave(here("plots", "pairs_home_counts.png"), p_pairs_home,
       width = 12, height = 10, dpi = 120)

# ---- 7-9. Missing data plots (full train, not sampled) ----------------------
train_full <- data.table::as.data.table(fst::read.fst(here("train.fst")))
train_full[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

count_vars_home_full <- grep("^count_home", names(train_full), value = TRUE)

# 7. Temporal pattern of debut/missing values
p_missing_temporal <- train_full |>
  mutate(date = as.Date(dateutc)) |>
  group_by(date) |>
  summarise(debut_rate = sum(is_debut, na.rm = TRUE) / n(), .groups = "drop") |>
  filter(debut_rate > 0) |>
  ggplot(aes(x = date, y = debut_rate)) +
  geom_col(fill = "#4e79a7", width = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of debut events over time",
    x = "Date", y = "Proportion of events with missing EWMA rank"
  ) +
  theme_minimal(base_size = 11)

ggsave(here("plots", "missing_temporal.png"), p_missing_temporal,
       width = 10, height = 4, dpi = 150)

# 8. Missingness rate by league (non-debut players only)
p_missing_league <- train_full |>
  filter(is_debut == 0 | is.na(is_debut)) |>
  group_by(league) |>
  summarise(missing_rate = sum(is.na(ewma_rank)) / n(), .groups = "drop") |>
  ggplot(aes(x = reorder(league, missing_rate), y = missing_rate)) +
  geom_col(fill = "#4e79a7") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "EWMA rank missingness rate by league (non-debut players)",
    x = NULL, y = "Proportion missing"
  ) +
  theme_minimal(base_size = 11)

ggsave(here("plots", "missing_by_league.png"), p_missing_league,
       width = 8, height = 4, dpi = 150)

# 9. MAR assessment: count predictors by missingness (non-debut, sampled for speed)
set.seed(1)
mar_sample <- train_full |>
  filter(is_debut == 0 | is.na(is_debut)) |>
  mutate(ewma_missing = factor(is.na(ewma_rank), labels = c("Observed", "Missing"))) |>
  select(ewma_missing, all_of(count_vars_home_full)) |>
  slice_sample(n = min(50000, nrow(train_full)))

p_missing_mar <- mar_sample |>
  pivot_longer(-ewma_missing, names_to = "predictor", values_to = "value") |>
  mutate(predictor = str_remove(predictor, "^count_home_")) |>
  ggplot(aes(x = ewma_missing, y = value, fill = ewma_missing)) +
  geom_boxplot(outlier.alpha = 0.02) +
  facet_wrap(~predictor, scales = "free_y") +
  scale_fill_manual(values = c("Observed" = "#4e79a7", "Missing" = "#f28e2b")) +
  labs(
    title = "Cumulative home event counts by EWMA missingness (non-debut players)",
    x = NULL, y = "Cumulative count"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave(here("plots", "missing_mar.png"), p_missing_mar,
       width = 14, height = 9, dpi = 150)

train_full <- NULL; gc()

cat("All plots saved to plots/\n")
