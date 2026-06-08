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

predvars <- setdiff(names(df), c('match_id', 'event_name', '.row_id',
                                 'cutoff', 'is_home', 'sub_event_id', 'goal_scored', 'dateutc')) # I'll use dateutc for impuation later on.

for (k in 3:5) {
  fold <- data.table::as.data.table(fst::read.fst(paste0('fold', k, '.fst')))
  
  # form <- home_goals_remaining ~ . - current_away_score - goal_scored
  # Let's add in some code to make things factors

  fold[, league:= as.factor(league)]
  fold[, match_period:= as.factor(match_period)]
  fold[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
  fold[, c('is_debut', 'is_home', 'fold_id', 'dateutc', 'sub_event_id', 'own_goal', 'cutoff', 
    'match_id', 'event_name') := NULL]

  fold_T <- fold[cv_role == 'train']
  fold_V <- fold[cv_role == 'test']
  fold_T[, c('cv_role') := NULL]
  fold_V[, c('cv_role') := NULL]

  cols <- setdiff(names(fold_T)[sapply(fold_T, is.numeric)], c('home_goals_remaining', 'away_goals_remaining'))

  # Scale train; capture params to apply to test (avoids leakage)
  scaled_mat <- scale(as.matrix(fold_T[, .SD, .SDcols = cols]))
  fold_T[, (cols) := as.data.table(scaled_mat)]
  fold_V[, (cols) := as.data.table(scale(as.matrix(fold_V[, .SD, .SDcols = cols]),
                                          center = attr(scaled_mat, "scaled:center"),
                                          scale = attr(scaled_mat, "scaled:scale")))]

  # inspect column variances (numeric columns only)
  col_vars <- sort(sapply(
    fold_T[, .SD, .SDcols = sapply(fold_T, is.numeric)],
    function(x) var(x, na.rm = TRUE)
  ))
  print(data.frame(variance = col_vars))

  imp_T <- mice(fold_T, m = 1)
  saveRDS(imp_T, paste0('fold', k, 'T_imp.rds'))

  imp_V <- mice(fold_V, m = 1)
  saveRDS(imp_V, paste0('fold', k, 'V_imp.rds'))
}

train <- data.table::as.data.table(fst::read.fst('train.fst'))

train[, league := as.factor(league)]
train[, match_period := as.factor(match_period)]
train[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
train[, c('is_debut', 'is_home', 'fold_id', 'dateutc', 'sub_event_id', 'own_goal', 'cutoff',
          'match_id', 'event_name', 'cv_role') := NULL]

cols_train <- setdiff(names(train)[sapply(train, is.numeric)], c('home_goals_remaining', 'away_goals_remaining'))

scaled_train <- scale(as.matrix(train[, .SD, .SDcols = cols_train]))
train[, (cols_train) := data.table::as.data.table(scaled_train)]

imp_train <- mice::mice(train, m = 1)
saveRDS(imp_train, 'train_imp.rds')

test <- data.table::as.data.table(fst::read.fst('test.fst'))

test[, league := as.factor(league)]
test[, match_period := as.factor(match_period)]
test[, dateutc := as.POSIXct(dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
test[, c('is_debut', 'is_home', 'fold_id', 'dateutc', 'sub_event_id', 'own_goal', 'cutoff',
         'match_id', 'event_name') := NULL]
if ('cv_role' %in% names(test)) test[, cv_role := NULL]

# Scale test using training set parameters (no leakage)
test[, (cols_train) := data.table::as.data.table(
  scale(as.matrix(test[, .SD, .SDcols = cols_train]),
        center = attr(scaled_train, "scaled:center"),
        scale  = attr(scaled_train, "scaled:scale"))
)]

imp_test <- mice::mice(test, m = 1)
saveRDS(imp_test, 'test_imp.rds')
