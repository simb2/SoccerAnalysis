# missing data
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


df <- data.table::as.data.table(fst::read.fst("clnd_df.fst"))
df[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

cutoff <- df |>
  group_by(league) |>
  summarise(cutoff = quantile(dateutc, 0.8), .groups = "drop")

df <- df |>
  left_join(cutoff, by = "league") |>
  mutate(.row_id = row_number())

train <- df |> filter(dateutc <= cutoff)
test <- df |> filter(dateutc > cutoff)

# Let's make sure that we are not using imputed data to test the model.
test <- test

fst::write.fst(train, 'train.fst')
fst::write.fst(test, 'test.fst')
# Let's first make some folds that we will cross validate different models on.


# Walk-forward CV on train only, per league
cv_folds <- train |>
  group_by(league) |>
  group_split() |>
  map(function(league_df) {
    folds <- sliding_period(
      arrange(league_df, dateutc),
      index = dateutc,
      period = "month",
      lookback = 3,
      assess_stop = 1
    )
    mutate(folds, fold_id = row_number())
  }) |>
  set_names(map_chr(
    group_split(group_by(train, league)),
    ~ unique(.x$league)
  ))

train <- imap_dfr(cv_folds, function(rset, league_name) {
  map_dfr(seq_len(nrow(rset)), function(i) {
    bind_rows(
      analysis(rset$splits[[i]]) |> mutate(fold_id = rset$fold_id[i], cv_role = "train"),
      assessment(rset$splits[[i]]) |> mutate(fold_id = rset$fold_id[i], cv_role = "test")
    )
  })
}) |> select(-.row_id)
train <- as.data.table(train)
###############################################################################

fst::write.fst(train[fold_id == 1], 'fold1.fst')
fst::write.fst(train[fold_id == 2], 'fold2.fst')
fst::write.fst(train[fold_id == 3], 'fold3.fst')
fst::write.fst(train[fold_id == 4], 'fold4.fst')
fst::write.fst(train[fold_id == 5], 'fold5.fst')
fst::write.fst(test, 'test.fst')

