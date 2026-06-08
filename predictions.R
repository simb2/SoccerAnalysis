library(fst); library(data.table); library(mgcv); library(mice)
set.seed(1)

RESPONSES <- c("home_goals_remaining", "away_goals_remaining")
BAM_ARGS <- list(family = poisson(), discrete = TRUE, select = TRUE, na.action = na.omit)

train_i <- as.data.table(mice::complete(readRDS('train_imp.rds'), 1))
test_i  <- as.data.table(mice::complete(readRDS('test_imp.rds'),  1))
vars <- setdiff(names(train_i), RESPONSES)
count_vars <- grep("^count_", vars, value = TRUE)
strength_vars <- union(grep("_str$", vars, value = TRUE), grep('_balance$', vars, value = TRUE))

# ---- Define formulas -------------------------------------------------------
s_terms <- paste0("s(", union(count_vars, strength_vars), ", k = 7)", collapse = " + ")
base_terms <- "+ league + match_period + s(pos_orig_x, pos_orig_y, bs = 'gp') + s(pos_dest_x, pos_dest_y, bs = 'gp') + s(event_sec, k = 7) +
s(current_away_score, k = 7) + s(current_home_score, k = 7)"

formulas <- list(
  "BAM home" = list(resp = "home_goals_remaining", f = as.formula(paste("home_goals_remaining ~", s_terms, base_terms))),
  "BAM away" = list(resp = "away_goals_remaining", f = as.formula(paste("away_goals_remaining ~", s_terms, base_terms)))
)

poisson_dev <- function(y, mu) mean(2 * (ifelse(y == 0, 0, y * log(y / mu)) - (y - mu)))

results <- list()
for (model_name in names(formulas)) {
  print(model_name)
  resp <- formulas[[model_name]]$resp
  f <- formulas[[model_name]]$f
  
  fit <- do.call(bam, c(list(f, data = train_i), BAM_ARGS))
  yhat <- predict(fit, test_i, type = "response", na.action = na.pass)
  y <- test_i[[resp]]
  
  results[[length(results) + 1]] <- data.frame(
    model = model_name,
    outcome = resp,
    AIC = AIC(fit),
    BIC = BIC(fit),
    test_pois_dev = poisson_dev(y, yhat)
  )
  saveRDS(fit, paste0("bam_", gsub(" ", "_", model_name), ".rds"))
}

# ---- Summarise -------------------------------------------------------------
all_results <- do.call(rbind, results)
all_results <- all_results[order(all_results$outcome, all_results$BIC), ]
rownames(all_results) <- NULL
print(all_results, digits = 6)
saveRDS(all_results, "results.rds")
