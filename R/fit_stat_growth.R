#####################################################
## Market Intelligence Planet Fitness              ##
## Statistical Model                               ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################
# Install vip package
# install.packages("vip", repos = c("https://bgreenwell.r-universe.dev", "https://cloud.r-project.org"))

# Load libraries
library(tidyverse)
library(tidymodels)  # Includes parsnip, rsample, yardstick, recipes, etc.
library(corrplot)
library(vip)         # For variable importance plots
library(caret)
library(broom)  # For tidy()
library(yardstick)

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
  filter(!is.na(CAGRYears04)) %>%
  select(
    -ClubNumber, -ClubName, -City, -Address, -ZipCode, -Latitude, -Longitude,
    -MarketClass, -TimeZone, -OpenDate, -MaturityDate, -MaturityFlag,
    -AnnualRent, -ClosestClub, -OpenYear, -ClubNameAlt, -Year, -YearNumber, -ActiveMembers,
    -Revenue, -EBITDA, -CAGRYears47, -CAGRYears47PercRank, -CAGRYears47Cat,
    -CAGRYears04, -CAGRYears04PercRank, -EBITDAPerSqFtCat
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

club_stat %>% group_by(CAGRYears04Cat) %>%
  summarise(n = n())

# =============================================
# STEP 3: OUTCOME VARIABLE
# =============================================
# Create binary outcome as a factor
club_stat <- club_stat %>%
  mutate(outcome = factor(ifelse(CAGRYears04Cat == "High", 1, 0), levels = c(0, 1))) %>%
  select(-CAGRYears04Cat)

# Set variable of interest i.e. positive class as a reference level
club_stat <- club_stat %>%
  mutate(outcome = relevel(outcome, ref = "1"))

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
# Confirming "1" is first, since sens/spec/precision/roc_auc below all
# depend on this for correctly identifying the positive/event class.
levels(club_stat$outcome)  # should print: [1] "1" "0"

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
# STEP 7: FEATURE IMPORTANCE VIA RANDOM FOREST
# =============================================
# NOTE (data leakage caveat): this RF is fit on the FULL dataset (club_stat)
# to select important features, and those same features are then used to
# build/evaluate the baseline and final models below via cross-validation.
# Because feature selection "saw" all 60 observations before CV folds were
# created, reported CV performance will likely be optimistic. This is a
# known trade-off with small-n, high-p data. For a fully rigorous estimate,
# feature selection would need to happen INSIDE each CV fold

set.seed(136)

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

saveRDS(rf_importance, file = "./data/rf_growth_importance.rds")

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



# Top features by importance
rf_var_imp <- rf_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  arrange(desc(Importance)) %>%
  head(10)

important_features <- rf_var_imp$Variable
print("Top 10 features from Random Forest:")
print(important_features)


# =============================================
# STEP 8: BUILD SHARED CV FOLDS ON SELECTED FEATURES
# =============================================
# Preprocess the FULL dataset, then subset to the top-10 features + outcome.
# Both the baseline and final models below use these SAME folds so their
# CV performance is directly comparable.
prepped_data <- bake(prep(club_rec, training = club_stat), new_data = club_stat) %>%
  select(all_of(important_features), outcome)

set.seed(456)
folds <- vfold_cv(prepped_data, v = 8, repeats = 3, strata = outcome)  # stratified given 46/16 imbalance


# =============================================
# STEP 9: BASELINE MODEL — LOGISTIC REGRESSION
# =============================================
logit_spec <- logistic_reg(penalty = 0.01, mixture = 0.5) %>%  # elastic net
  set_engine("glmnet")

# No further preprocessing needed -- prepped_data is already dummy-encoded,
# imputed, and normalized from club_rec above.
logit_rec <- recipe(outcome ~ ., data = prepped_data)

logit_wf <- workflow() %>%
  add_recipe(logit_rec) %>%
  add_model(logit_spec)

logit_cv_results <- fit_resamples(
  logit_wf,
  resamples = folds,
  metrics   = metric_set(accuracy, sens, spec, yardstick::precision, roc_auc),
  control   = control_resamples(save_pred = TRUE)
)

print("Baseline (Logistic Regression) CV Metrics:")
collect_metrics(logit_cv_results)


# Adjust threshold of 0.5 because recall (sensitivity) = 0.33
library(probably)

logit_preds <- collect_predictions(logit_cv_results)

# Try a range of thresholds and see the sens/spec/precision trade-off at each
threshold_data <- logit_preds %>%
  threshold_perf(
    truth      = outcome,
    estimate   = .pred_1,
    thresholds = seq(0.1, 0.9, by = 0.05),
    metrics    = metric_set(sens, spec, yardstick::precision, j_index)
  )

# Plot with all four metrics
threshold_data %>%
  ggplot(aes(x = .threshold, y = .estimate, color = .metric)) +
  geom_line(linewidth = 1) +
  labs(title = "Threshold vs. Sens/Spec/Precision/J-Index",
       x = "Classification Threshold", y = "Metric Value")

# Find threshold that maximizes Youden's J (sens + spec - 1) as one reasonable choice
best_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  arrange(desc(.estimate)) %>%
  slice(1)

print(best_threshold)

# Full picture across all thresholds
threshold_data %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(.threshold) %>%
  print(n = Inf)

# Keep threshold at 0.5 because Precision == 0.552
# We care more about precision


# =============================================
# STEP 10: FINAL MODEL — RANDOM FOREST (NON-LINEAR)
# =============================================
rf_final_spec <- rand_forest(
  mtry  = round(sqrt(ncol(prepped_data %>% select(-outcome)))),
  trees = 500,
  mode  = "classification"
) %>%
  set_engine("ranger")

rf_final_wf <- workflow() %>%
  add_recipe(logit_rec) %>%       # same reduced feature set, no extra preprocessing needed
  add_model(rf_final_spec)

rf_cv_results <- fit_resamples(
  rf_final_wf,
  resamples = folds,              # SAME folds as baseline for fair comparison
  metrics   = metric_set(accuracy, sens, spec, yardstick::precision, roc_auc),
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
    metrics    = metric_set(yardstick::precision, sens, spec, j_index)
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
logit_preds <- collect_predictions(logit_cv_results)

logit_roc_data <- logit_preds %>%
  roc_curve(truth = outcome, .pred_1)  # probability of class "1" (event)

autoplot(logit_roc_data) +
  labs(title = "ROC Curve — Baseline (Logistic Regression)",
       subtitle = "Cross-validated, averaged across folds")

logit_conf_mat <- conf_mat(logit_preds, truth = outcome, estimate = .pred_class)
print("Confusion Matrix — Baseline (Logistic Regression):")
print(logit_conf_mat)
autoplot(logit_conf_mat, type = "heatmap") +
  labs(title = "Confusion Matrix — Baseline (Logistic Regression)")

## ---- Final: Random Forest ----
rf_preds <- collect_predictions(rf_cv_results)

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
model_comparison <- bind_rows(
  collect_metrics(logit_cv_results) %>% mutate(model = "Logistic Regression (Baseline)"),
  collect_metrics(rf_cv_results)    %>% mutate(model = "Random Forest (Final)")
) %>%
  select(model, .metric, mean, std_err) %>%
  arrange(.metric, model)

print("Model Comparison — Baseline vs. Final:")
print(model_comparison)


# =============================================
# STEP 12: BASELINE MODEL INTERPRETATION
# =============================================
# fit_resamples() only fits temporary models inside each CV fold for
# evaluation -- it does not retain a single final model object. To
# interpret coefficients, fit the logistic regression workflow one more
# time on the FULL prepped_data (same data used to build the CV folds).
final_logit_fit <- fit(logit_wf, data = prepped_data)

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

