#####################################################
## Market Intelligence Planet Fitness              ##
## Statistical Model                               ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################
# Install vip package
# install.packages("vip", repos = c("https://bgreenwell.r-universe.dev", "https://cloud.r-project.org"))

# Load libraries
# NOTE: caret has been REMOVED. It masked yardstick metrics (precision, etc.)
# causing namespace collisions. All modeling is handled by tidymodels.
library(tidyverse)    # ggplot2, dplyr, purrr, tidyr, etc.
library(tidymodels)   # parsnip, rsample, yardstick, recipes, tune, workflows
library(corrplot)
library(vip)          # For variable importance plots
library(broom)        # For tidy()
library(probably)     # For threshold_perf()

# =============================================
# CLUB PERFORMANCE DRIVERS ANALYSIS WITH TIDYMODELS
# Objective: Identify success drivers for EBITDAPerSqFt == "High"
# =============================================

# =============================================
# STEP 1: PREPARE DATASET
# =============================================
# Read data
club_perf_stat <- readRDS("./data/club_perf_year4.rds")

# Subset data to eliminate Nulls and 309 - North Miami Beach (low revenue)
# Removed CAGRYears04 from list of independent vars
# club_stat <- club_perf_year4 %>%
#   filter(!is.na(CAGRYears04) & RevenuePerSqFt > 40)


# =============================================
# STEP 2: FEATURE SELECTION
# =============================================
# Remove irrelevant fields
club_stat <- club_perf_stat %>%
  select(
    -ClubNumber, -ClubName, -City, -Address, -ZipCode, -Latitude, -Longitude,
    -MarketClass, -TimeZone, -OpenDate, -MaturityDate, -MaturityFlag,
    -AnnualRent, -ClosestClub, -OpenYear, -ClubNameAlt, -Year, -YearNumber, -ActiveMembers,
    -Revenue, -EBITDA, -CAGRYears04, -CAGRYears04PercRank, -CAGRYears04Cat, 
    -CAGRYears47, -CAGRYears47PercRank, -CAGRYears47Cat,
    -CAGRYears36, -CAGRYears36PercRank, -CAGRYears36Cat
  ) %>%
  # Remove highly correlated features
  select(
    -TAMedHHNetWorth, -TAFamilyHH, -PopAdult44, -PopAdult64, -TAPopMale 
  ) %>%
  # Remove features that are out of time
  select(
    -RevenuePerSqFt, -RevenuePerSqFtPercRank, -RevenuePerSqFtCat,
    -EBITDAPerSqFt, -EBITDAPerSqFtPercRank
  )

club_stat %>% group_by(EBITDAPerSqFtCat) %>%
  summarise(n = n())

# =============================================
# STEP 3: OUTCOME VARIABLE
# =============================================
# FIXED: "1" (highly profitable) must be the SECOND/last factor level,
# because yardstick treats the LAST level as the event class for
# sens/spec/precision/roc_auc. The previous relevel(ref = "1") made "1"
# the FIRST level, which silently inverted the event class to "0".
club_stat <- club_stat %>%
  mutate(outcome = factor(ifelse(EBITDAPerSqFtCat == "High", 1, 0), levels = c(0, 1))) %>%
  select(-EBITDAPerSqFtCat)

# =============================================
# STEP 4: CORRELATION ANALYSIS
# =============================================
# Select numeric variables for correlation
numeric_club <- club_stat %>%
  select(where(is.numeric))

# Correlation matrix
cor_matrix <- cor(numeric_club, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", order = "hclust")

# Top variables correlated with outcome
cor_values <- cor(numeric_club, as.numeric(club_stat$outcome), use = "complete.obs")

# Create a data frame with variable names and correlation values
top_cor <- tibble(
  Variable = names(numeric_club),
  Correlation = cor_values
) %>%
  arrange(desc(abs(Correlation))) %>%
  head(12)

print("Top 12 variables correlated with outcome:")
print(top_cor)


# =============================================
# STEP 5: VERIFY OUTCOME FACTOR LEVEL ORDER
# =============================================
# FIXED: Confirming "1" is LAST, since sens/spec/precision/roc_auc below all
# depend on this for correctly identifying the positive/event class.
levels(club_stat$outcome)  # should print: [1] "0" "1"

# =============================================
# STEP 6: FEATURE ENGINEERING RECIPE
# =============================================
club_rec <- recipe(outcome ~ ., data = club_stat) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  # Remove zero-variance predictors (e.g., dummy variables)
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())


# =============================================
# STEP 7: FEATURE IMPORTANCE VIA RANDOM FOREST (EXPLORATORY)
# =============================================
# NOTE: this RF is fit on the FULL dataset (club_stat) purely as an
# EXPLORATORY diagnostic to see which features look influential. It is
# NOT used for feature selection anymore -- the elastic net in Step 9
# performs feature selection inside each CV fold (via its tuned L1
# component), so the leakage caveat no longer applies to the models below.
# Any performance numbers from this RF would still be optimistic (it saw
# all observations) -- treat it as insight, not evaluation.

set.seed(123)

rf_spec <- rand_forest(
  mtry  = round(sqrt(ncol(club_stat %>% select(-outcome)))),  # Features per split
  trees = 500,
  mode  = "classification"
) %>%
  set_engine("ranger", importance = "impurity")

rf_wf <- workflow() %>%
  add_recipe(club_rec) %>%
  add_model(rf_spec)

rf_fit <- fit(rf_wf, data = club_stat)


# Visualize feature importance
rf_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 20, plot_args = list(size = 3, color = "blue"))


# Extract feature importance from the RF model
rf_importance <- rf_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  mutate(
    Importance = as.numeric(Importance),
    Variable = reorder(Variable, Importance)  # Order by importance
  ) %>%
  head(20)  # Show top 20 features for clarity

saveRDS(rf_importance, file = "./data/rf_importance.rds")

# Create the plot
ggplot(rf_importance, aes(x = Importance, y = Variable, fill = Importance)) +
  geom_col(width = 0.7, alpha = 0.9) +  # Horizontal bars with gradient fill
  scale_fill_gradient(
    low = "#0077bb",   # Blue for lower importance
    high = "#ee7733",  # Orange for higher importance
    name = "Importance Score"
  ) +
  scale_x_continuous(
    labels = comma,   # Format axis labels
    name = "Importance Score"
  ) +
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
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank()
  )


# =============================================
# STEP 8: BUILD SHARED CV FOLDS ON ALL FEATURES
# =============================================
# FIXED (leakage): Previously the top-10 RF features were selected on the FULL
# dataset before CV folds were created, which leaked information into CV
# performance estimates. We now keep ALL features and let the elastic net's
# L1 component (tuned below) perform feature selection INSIDE each fold --
# honest, data-driven selection. Both the baseline and final models below
# use these SAME folds so their CV performance is directly comparable.
prepped_data <- bake(prep(club_rec, training = club_stat), new_data = club_stat)

set.seed(456)
folds <- vfold_cv(prepped_data, v = 8, repeats = 3, strata = outcome)  # stratified given 46/16 imbalance




# =============================================
# STEP 9: BASELINE MODEL — SIMPLE LOGISTIC REGRESSION (SMALL PENALTY)
# =============================================
# Back to basics: a single logistic regression with a small fixed penalty.
# No tuning grid, no hyperparameter selection. The small penalty just
# stabilizes coefficient estimates at n ~ 60 (guards against perfect
# separation) without meaningfully shrinking the signal.
#
# EXPECTATION SETTING: the original simple model's ROC AUC of 0.743 was
# inflated by RF feature-selection leakage. An honest number for this
# data is likely in the 0.5-0.6 range. This model's value is simplicity,
# interpretability, and truthful evaluation -- not peak performance.

logit_spec <- logistic_reg(penalty = 0.001, mixture = 1) %>%  # tiny lasso penalty
  set_engine("glmnet")

# No further preprocessing needed -- prepped_data is already dummy-encoded,
# imputed, and normalized from club_rec above.
logit_rec <- recipe(outcome ~ ., data = prepped_data)

logit_wf <- workflow() %>%
  add_recipe(logit_rec) %>%
  add_model(logit_spec)

logit_cv_results <- fit_resamples(
  logit_wf,
  resamples = folds,              # same folds, comparable to previous runs
  metrics   = metric_set(accuracy, sens, spec, precision, roc_auc),
  control   = control_resamples(save_pred = TRUE)
)

print("Simple Logistic Regression (penalty = 0.001) CV Metrics:")
collect_metrics(logit_cv_results)

# --- Out-of-fold predictions, kept per repeat (NO probability averaging) ---
logit_preds_rep <- collect_predictions(logit_cv_results, summarize = FALSE)

# Pooled AUC per repeat -- the deployment-relevant number
roc_auc_by_rep <- logit_preds_rep %>%
  group_by(id) %>%
  roc_auc(truth = outcome, .pred_1)

print("Pooled ROC AUC per repeat:")
print(roc_auc_by_rep)


# 1. The level order in THIS run
levels(club_stat$outcome)# if this prints "1" "0" -- the old bug is back

# 2. The proof -- look at the confusion matrix
collect_predictions(logit_cv_results) %>%
  conf_mat(truth = outcome, estimate = .pred_class)

# 3. The class counts
table(club_stat$outcome)


# Per-repeat class metrics, then averaged
logit_metrics_rep <- logit_preds_rep %>%
  group_by(id) %>%
  metric_set(accuracy, sens, spec, precision)(truth = outcome, estimate = .pred_class) %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate), sd = sd(.estimate), .groups = "drop")

print("Class metrics (mean across repeats):")
print(logit_metrics_rep)

# --- Score distributions (the blind-spot check) ---
logit_preds_rep %>%
  ggplot(aes(x = .pred_1, fill = outcome)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  labs(title = "Predicted Probability Distribution by Actual Class",
       subtitle = "Simple logistic regression, penalty = 0.001",
       x = "Predicted P(High Profitability)",
       fill = "Actual Class")



# =============================================
# STEP 10: FINAL MODEL — RANDOM FOREST (NON-LINEAR)
# =============================================
# Uses the SAME feature set as the baseline (all features, no leakage)
# and the SAME CV folds, so its performance is directly comparable.
rf_final_spec <- rand_forest(
  mtry  = round(sqrt(ncol(prepped_data %>% select(-outcome)))),
  trees = 500,
  mode  = "classification"
) %>%
  set_engine("ranger")

rf_final_wf <- workflow() %>%
  add_recipe(logit_rec) %>%       # same full feature set, already preprocessed
  add_model(rf_final_spec)

rf_cv_results <- fit_resamples(
  rf_final_wf,
  resamples = folds,              # SAME folds as baseline for fair comparison
  metrics   = metric_set(accuracy, sens, spec, precision, roc_auc),
  control   = control_resamples(save_pred = TRUE)
)

print("Final (Random Forest) CV Metrics:")
collect_metrics(rf_cv_results)


# Check thresholds
rf_preds <- collect_predictions(rf_cv_results)

rf_threshold_data <- rf_preds %>%
  threshold_perf(
    truth      = outcome,
    estimate   = .pred_1,
    thresholds = seq(0.1, 0.6, by = 0.05),
    metrics    = metric_set(precision, sens, spec, j_index)
  )

# Plot with all four metrics
rf_threshold_data %>%
  ggplot(aes(x = .threshold, y = .estimate, color = .metric)) +
  geom_line(linewidth = 1) +
  labs(title = "Threshold vs. Sens/Spec/Precision/J-Index",
       x = "Classification Threshold", y = "Metric Value")


# Full picture across all thresholds
rf_threshold_data %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(.threshold) %>%
  print(n = Inf)


rf_threshold_data %>%
  filter(.metric == "precision") %>%
  arrange(desc(.estimate)) %>%
  print(n = Inf)


# =============================================
# STEP 11: MODEL EVALUATION
# =============================================

## ---- Baseline: Logistic Regression ----
# FIXED: uses the out-of-fold predictions from the tuning run (Step 9b)
# instead of the removed logit_cv_results refit.
logit_roc_data <- logit_preds %>%
  roc_curve(truth = outcome, .pred_1)  # probability of class "1" (event)

autoplot(logit_roc_data) +
  labs(title = "ROC Curve — Baseline (Tuned Logistic Regression)",
       subtitle = "Out-of-fold predictions, best tuned configuration")

logit_conf_mat <- conf_mat(logit_preds, truth = outcome, estimate = .pred_class)
print("Confusion Matrix — Baseline (Tuned Logistic Regression):")
print(logit_conf_mat)
autoplot(logit_conf_mat, type = "heatmap") +
  labs(title = "Confusion Matrix — Baseline (Tuned Logistic Regression)")

## ---- Final: Random Forest ----
rf_roc_data <- rf_preds %>%
  roc_curve(truth = outcome, .pred_1)

autoplot(rf_roc_data) +
  labs(title = "ROC Curve — Final (Random Forest)",
       subtitle = "Cross-validated, averaged across folds")

rf_conf_mat <- conf_mat(rf_preds, truth = outcome, estimate = .pred_class)
print("Confusion Matrix — Final (Random Forest):")
print(rf_conf_mat)
autoplot(rf_conf_mat, type = "heatmap") +
  labs(title = "Confusion Matrix — Final (Random Forest)")

## ---- Side-by-side metric comparison ----
# FIXED: logistic metrics come from logit_metrics (Step 9b); RF metrics from
# fit_resamples. Mean/std_err are reported for RF; point estimates for the
# logistic model (out-of-fold, summarized across repeats).
model_comparison <- bind_rows(
  logit_metrics %>%
    mutate(model = "Logistic Regression (Baseline)") %>%
    select(model, .metric, mean = .estimate),
  collect_metrics(rf_cv_results) %>%
    mutate(model = "Random Forest (Final)") %>%
    select(model, .metric, mean)
) %>%
  arrange(.metric, model)

print("Model Comparison — Baseline vs. Final:")
print(model_comparison)


# =============================================
# STEP 12: BASELINE MODEL INTERPRETATION
# =============================================
# FIXED: previously fit logit_wf (which no longer exists after Option 2).
# Finalize the tuning workflow with the best hyperparameters and fit once
# on the FULL prepped_data to obtain a single model object for coefficient
# interpretation. This is a legitimate full-data fit for INTERPRETATION
# (not evaluation) -- CV metrics above are the honest performance numbers.
final_logit_fit <- fit(finalize_workflow(logit_tune_wf, best_params),
                       data = prepped_data)

# Extract coefficients from Logistic Regression
logit_coef <- final_logit_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Coefficient        = estimate,
    Odds_Ratio          = exp(estimate),        # multiplicative change in odds per 1-unit increase
    Pct_Odds_Change     = (exp(estimate) - 1) * 100,  # % change in odds, easier to communicate
    Significant_at_05   = p.value < 0.05
  ) %>%
  arrange(desc(abs(Coefficient)))

print("Logistic Regression coefficients (full model, ranked by magnitude):")
print(logit_coef)