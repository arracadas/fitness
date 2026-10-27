#####################################################
## Market Intelligence Planet Fitness              ##
## Statistical Model                               ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################

library(tidyverse)    # ggplot2, dplyr, purrr, tidyr, etc.
library(tidymodels)   # parsnip, rsample, yardstick, recipes, workflows
library(corrplot)     # correlation matrix visualization
library(vip)          # variable importance plots
library(broom)        # tidy model output
library(probably)     # threshold performance analysis
library(logistf)      # Firth logistic regression + FLIC (intercept correction)

# =============================================
# CLUB PROFITABILITY DRIVERS ANALYSIS
# Goal: Identify which club characteristics predict high
#       profitability (EBITDAPerSqFt in the top 30%).
# Data: 62 clubs at Year 4 of operation, pre-built by fit_clean.R.
# =============================================

# =============================================
# STEP 1: LOAD DATA
# =============================================
club_perf_stat <- readRDS("./data/club_perf_year4.rds")

# =============================================
# STEP 2: FEATURE SELECTION
# =============================================
# Drop columns that are identifiers, dates, or otherwise not usable
# as predictors. Also drop columns derived from the outcome variables
# (RevenuePerSqFt, EBITDAPerSqFt and their ranks/categories) to avoid
# target leakage. ClosestClub, ShadowAnchor, and ADA are removed because
# they have too many levels for n=62 (would create sparse, unstable dummies)
# or are redundant proxies for State.
club_stat <- club_perf_stat %>%
  select(
    -ClubNumber, -ClubName, -City, -Address, -ZipCode, -Latitude, -Longitude,
    -MarketClass, -TimeZone, -OpenDate, -MaturityDate, -MaturityFlag,
    -AnnualRent, -ClosestClub, -ShadowAnchor, -ADA, -OpenYear, -ClubNameAlt,
    -Year, -YearNumber, -ActiveMembers,
    -Revenue, -EBITDA, -CAGRYears04PercRank, -CAGRYears04Cat,
    -CAGRYears47, -CAGRYears47PercRank, -CAGRYears47Cat,
    -CAGRYears36, -CAGRYears36PercRank, -CAGRYears36Cat
  ) %>%
  # Drop raw counts that are near-duplicates of retained percentage columns
  select(
    -TAMedHHNetWorth, -TAFamilyHH, -PopAdult44, -PopAdult64, -TAPopMale
  ) %>%
  # Drop outcome-leaking columns (derived from EBITDAPerSqFt / RevenuePerSqFt)
  select(
    -RevenuePerSqFt, -RevenuePerSqFtPercRank, -RevenuePerSqFtCat,
    -EBITDAPerSqFt, -EBITDAPerSqFtPercRank
  )

# Diagnostic: confirm class distribution before binarizing
club_stat %>% group_by(EBITDAPerSqFtCat) %>%
  summarise(n = n())

# =============================================
# STEP 3: OUTCOME VARIABLE
# =============================================
# Binary outcome: 1 = High profitability (top 30% by EBITDAPerSqFt),
#                 0 = Low or Medium profitability.
# Factor levels are set as c("0", "1") so that "1" (the event of
# interest) is the LAST level. yardstick treats the last level as
# the positive/event class for precision, recall, specificity, and
# ROC AUC, and all metrics below use event_level = "second".
club_stat <- club_stat %>%
  mutate(outcome = factor(ifelse(EBITDAPerSqFtCat == "High", 1, 0), levels = c(0, 1))) %>%
  select(-EBITDAPerSqFtCat)

# =============================================
# STEP 4: CORRELATION ANALYSIS
# =============================================
# Examine pairwise correlations among numeric predictors and their
# correlation with the binary outcome. This is exploratory — the
# correlation-based removal happens in Step 4b.
numeric_club <- club_stat %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_club, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", order = "hclust")

cor_values <- cor(numeric_club, as.numeric(club_stat$outcome), use = "complete.obs")

top_cor <- tibble(
  Variable = names(numeric_club),
  Correlation = cor_values
) %>%
  arrange(desc(abs(Correlation))) %>%
  head(12)

print("Top 12 variables correlated with outcome:")
print(top_cor)


# =============================================
# STEP 4b: REMOVE HIGHLY CORRELATED PREDICTORS
# =============================================
# Iteratively remove numeric predictors with |r| > 0.8 to reduce
# multicollinearity. For each correlated pair, the variable with the
# weaker absolute correlation to the outcome is removed.
# Pairwise complete observations are used for the outcome correlation
# so each variable is evaluated on its own available cases (not just
# rows that are complete across all predictors). The loop handles
# clusters (e.g., the five trade-area population counts that are all
# highly inter-correlated) one pair at a time.
cor_threshold <- 0.8

repeat {
  numeric_club <- club_stat %>% select(where(is.numeric))
  cor_matrix   <- cor(numeric_club, use = "complete.obs")
  cor_outcome  <- cor(numeric_club, as.numeric(club_stat$outcome),
                      use = "pairwise.complete.obs")[, 1]

  high_cor_idx <- which(abs(cor_matrix) > cor_threshold & upper.tri(cor_matrix),
                        arr.ind = TRUE)
  if (nrow(high_cor_idx) == 0) break

  pair_r    <- abs(cor_matrix[high_cor_idx])
  max_pair  <- which.max(pair_r)
  v1 <- rownames(cor_matrix)[high_cor_idx[max_pair, 1]]
  v2 <- rownames(cor_matrix)[high_cor_idx[max_pair, 2]]

  if (abs(cor_outcome[v1]) >= abs(cor_outcome[v2])) {
    club_stat <- club_stat %>% select(-all_of(v2))
    cat(sprintf("Removed %s (r = %.3f with %s; outcome |r|: %.3f vs %.3f)\n",
                v2, cor_matrix[v1, v2], v1, abs(cor_outcome[v2]), abs(cor_outcome[v1])))
  } else {
    club_stat <- club_stat %>% select(-all_of(v1))
    cat(sprintf("Removed %s (r = %.3f with %s; outcome |r|: %.3f vs %.3f)\n",
                v1, cor_matrix[v1, v2], v2, abs(cor_outcome[v1]), abs(cor_outcome[v2])))
  }
}


# =============================================
# STEP 5: VERIFY OUTCOME FACTOR LEVELS
# =============================================
# Assertion: "1" (High profitability) must be the last level so that
# yardstick metrics below correctly treat it as the event class.
levels(club_stat$outcome)  # expected: [1] "0" "1"

# =============================================
# STEP 6: FEATURE ENGINEERING RECIPE
# =============================================
# Preprocessing for the exploratory random forest in Step 7.
# Dummy-encodes categorical predictors, removes zero-variance columns,
# imputes missing numeric values with the median, and normalizes.
club_rec <- recipe(outcome ~ ., data = club_stat) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())


# =============================================
# STEP 7: EXPLORATORY RANDOM FOREST — FEATURE IMPORTANCE
# =============================================
# A random forest is fit on the full dataset to rank predictors by
# importance. This is purely exploratory: the top 3 features guide
# the predictor selection in Step 8. No performance metrics from
# this model are reported because it saw all observations (in-sample
# fit would be optimistic).
set.seed(123)

rf_spec <- rand_forest(
  mtry  = round(sqrt(ncol(club_stat %>% select(-outcome)))),
  trees = 500,
  mode  = "classification"
) %>%
  set_engine("ranger", importance = "impurity")

rf_wf <- workflow() %>%
  add_recipe(club_rec) %>%
  add_model(rf_spec)

rf_fit <- fit(rf_wf, data = club_stat)

rf_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 20, plot_args = list(size = 3, color = "blue"))

rf_importance <- rf_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  mutate(
    Importance = as.numeric(Importance),
    Variable = reorder(Variable, Importance)
  ) %>%
  head(20)

saveRDS(rf_importance, file = "./data/rf_importance.rds")

ggplot(rf_importance, aes(x = Importance, y = Variable, fill = Importance)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_gradient(low = "#0077bb", high = "#ee7733", name = "Importance Score") +
  scale_x_continuous(labels = comma, name = "Importance Score") +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Top 20 Features by Mean Decrease in Gini",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


# =============================================
# STEP 8: TOP-3 MODELING DATASET & BOOTSTRAP RESAMPLES
# =============================================
# Reduce to the top 3 predictors identified by the exploratory RF:
# Size, OverallScore, RentPSF. With n = 62, a parsimonious
# 3-predictor model avoids overfitting.
#
# Bootstrap resampling (1000 resamples, stratified by outcome) is used
# for validation instead of k-fold CV because:
#   - CV folds would be tiny (~8 obs each), making fold-level metrics
#     extremely noisy at this sample size.
#   - Firth's bias correction is most effective on samples close to the
#     full data size; bootstrap resamples contain ~63% unique observations.
#   - Out-of-bag (OOB) predictions approximate performance on new clubs.
top3_vars <- c("Size", "OverallScore", "RentPSF")

club_top3 <- club_stat %>%
  select(outcome, all_of(top3_vars))

set.seed(456)
boot_resamples <- bootstraps(club_top3, times = 1000, strata = outcome)


# =============================================
# STEP 9: FIRTH LOGISTIC REGRESSION (FLIC) + BOOTSTRAP VALIDATION
# =============================================
# Standard maximum-likelihood logistic regression is biased with small
# samples (n = 62, 19 events). Firth's penalized likelihood (Jeffreys
# prior) eliminates this first-order bias and prevents infinite
# estimates under separation.
#
# FLIC (Firth's Logistic regression with Intercept Correction) goes
# one step further: it corrects the intercept so predicted probabilities
# are calibrated for the population prevalence, not the sample prevalence.
# This matters because bootstrap resamples have varying class balance,
# and the deployment target is new clubs whose distribution matches the
# population.

# --- 9a: Fit FLIC on the full dataset (this is the deployment model) ---
flic_full <- flic(outcome ~ Size + OverallScore + RentPSF,
                  data = club_top3)

print("FLIC Model -- Firth Logistic Regression with Intercept Correction")
print(summary(flic_full))

# Express coefficients as odds ratios for interpretability
flic_coef <- tibble(
  term       = names(coef(flic_full)),
  estimate   = coef(flic_full),
  odds_ratio = exp(coef(flic_full))
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    pct_odds_change = (odds_ratio - 1) * 100
  ) %>%
  arrange(desc(abs(estimate)))

print("FLIC coefficients (odds ratios):")
print(flic_coef)

# --- 9b: Bootstrap validation ---
# For each bootstrap resample: fit FLIC on the in-bag sample, predict
# on out-of-bag (OOB) observations, and compute precision and recall.
# This estimates how the model will perform on genuinely new clubs.
# A for-loop is used because flic()'s formula evaluation does not
# resolve local variables correctly inside purrr::map()'s anonymous
# function environment.
set.seed(789)
n_boot <- nrow(boot_resamples)
flic_fits  <- vector("list", n_boot)
oob_preds  <- vector("list", n_boot)

for (i in 1:n_boot) {
  split      <- boot_resamples$splits[[i]]
  train_data <- analysis(split)

  flic_fits[[i]] <- tryCatch(
    flic(outcome ~ Size + OverallScore + RentPSF, data = train_data),
    error = function(e) NULL
  )

  if (!is.null(flic_fits[[i]])) {
    oob_data <- assessment(split)
    if (nrow(oob_data) > 0) {
      pred_prob <- predict(flic_fits[[i]], newdata = oob_data, type = "response")
      oob_preds[[i]] <- tibble(
        id          = boot_resamples$id[i],
        outcome     = oob_data$outcome,
        .pred_1     = as.numeric(pred_prob),
        .pred_class = factor(ifelse(.pred_1 > 0.5, "1", "0"), levels = c("0", "1"))
      )
    }
  }
}

n_success <- sum(!map_lgl(flic_fits, is.null))
cat(sprintf("\nFLIC converged on %d / %d bootstrap resamples\n", n_success, n_boot))

oob_predictions <- bind_rows(oob_preds)
cat(sprintf("Total OOB predictions: %d\n", nrow(oob_predictions)))

# --- Pooled metrics across all OOB predictions ---
# event_level = "second" ensures "1" (High profitability) is the
# positive class for all metrics.
boot_overall <- oob_predictions %>%
  summarise(
    n           = n(),
    accuracy    = accuracy_vec(truth = outcome, estimate = .pred_class),
    precision   = precision_vec(truth = outcome, estimate = .pred_class, event_level = "second"),
    recall      = sens_vec(truth = outcome, estimate = .pred_class, event_level = "second"),
    specificity = spec_vec(truth = outcome, estimate = .pred_class, event_level = "second"),
    roc_auc     = roc_auc_vec(truth = outcome, .pred_1, event_level = "second")
  )

print("Bootstrap OOB Validation -- Pooled Metrics:")
print(boot_overall)

# --- Per-resample metrics to quantify variability ---
boot_per_sample <- map_dfr(oob_preds, function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tibble(
    precision = precision_vec(truth = df$outcome, estimate = df$.pred_class, event_level = "second"),
    recall    = sens_vec(truth = df$outcome, estimate = df$.pred_class, event_level = "second")
  )
})

boot_summary <- boot_per_sample %>%
  summarise(
    n_bootstraps   = n(),
    precision_mean = mean(precision, na.rm = TRUE),
    precision_sd   = sd(precision, na.rm = TRUE),
    precision_q025 = quantile(precision, 0.025, na.rm = TRUE),
    precision_q975 = quantile(precision, 0.975, na.rm = TRUE),
    recall_mean    = mean(recall, na.rm = TRUE),
    recall_sd      = sd(recall, na.rm = TRUE),
    recall_q025    = quantile(recall, 0.025, na.rm = TRUE),
    recall_q975    = quantile(recall, 0.975, na.rm = TRUE)
  )

print("Bootstrap OOB Validation -- Per-Resample Precision & Recall (mean [95% CI]):")
print(boot_summary)

# --- Confusion matrix on pooled OOB predictions ---
oob_conf_mat <- oob_predictions %>%
  conf_mat(truth = outcome, estimate = .pred_class)
print("Confusion Matrix -- Bootstrap OOB (pooled):")
print(oob_conf_mat)

# --- Distribution of OOB predicted probabilities by actual class ---
oob_predictions %>%
  ggplot(aes(x = .pred_1, fill = outcome)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Predicted Probability Distribution by Actual Class",
    subtitle = "FLIC model, bootstrap out-of-bag predictions",
    x = "Predicted P(High Profitability)",
    fill = "Actual Class"
  )
