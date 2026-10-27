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
library(logistf)      # Firth logistic regression + FLIC (intercept correction)

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
    -AnnualRent, -ClosestClub, -ShadowAnchor, -ADA, -OpenYear, -ClubNameAlt,
    -Year, -YearNumber, -ActiveMembers,
    -Revenue, -EBITDA, -CAGRYears04PercRank, -CAGRYears04Cat,
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
# STEP 4b: REMOVE HIGHLY CORRELATED PREDICTORS
# =============================================
# Iteratively remove numeric predictors that are highly correlated with each
# other (|r| > 0.8). For each correlated pair, keep the variable with the
# stronger absolute correlation to the outcome (pairwise complete obs, so
# each variable is evaluated on its own available cases). This handles
# clusters (e.g., the five trade-area population counts) one pair at a time
# until no pair exceeds the threshold.
cor_threshold <- 0.8

repeat {
  numeric_club <- club_stat %>% select(where(is.numeric))
  cor_matrix   <- cor(numeric_club, use = "complete.obs")
  cor_outcome  <- cor(numeric_club, as.numeric(club_stat$outcome),
                      use = "pairwise.complete.obs")[, 1]

  high_cor_idx <- which(abs(cor_matrix) > cor_threshold & upper.tri(cor_matrix),
                        arr.ind = TRUE)
  if (nrow(high_cor_idx) == 0) break

  # Most correlated remaining pair
  pair_r    <- abs(cor_matrix[high_cor_idx])
  max_pair  <- which.max(pair_r)
  v1 <- rownames(cor_matrix)[high_cor_idx[max_pair, 1]]
  v2 <- rownames(cor_matrix)[high_cor_idx[max_pair, 2]]

  # Remove the one less correlated with the outcome
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
# STEP 8: PREPARE TOP-3 MODELING DATASET & BOOTSTRAP RESAMPLES
# =============================================
# Use only the top 3 predictors from the exploratory RF (Step 7):
# Size, RentPSF, PopYoungAdult24. With n = 62 and a 16/46 class
# imbalance, a parsimonious 3-predictor model is preferred over
# the full feature set.
top3_vars <- c("Size", "RentPSF", "PopYoungAdult24")

club_top3 <- club_stat %>%
  select(outcome, all_of(top3_vars))

# Bootstrap resamples for honest performance estimation.
# Bootstrap (resampling with replacement, evaluating on out-of-bag
# observations) is preferred over k-fold CV here because:
#   - With n = 62, CV folds are tiny (7-8 obs), making fold-level
#     estimates extremely noisy.
#   - Firth's bias correction works best on samples close to the
#     full data size, which bootstrap resamples provide (~63% unique obs).
#   - OOB predictions approximate performance on genuinely new clubs.
set.seed(456)
boot_resamples <- bootstraps(club_top3, times = 1000, strata = outcome)


# =============================================
# STEP 9: FIRTH LOGISTIC REGRESSION WITH FLIC + BOOTSTRAP VALIDATION
# =============================================
# Firth's penalized likelihood (Jeffreys prior) eliminates the first-order
# bias in maximum likelihood estimates -- critical with n = 62 and only
# 16 events, where standard ML can produce infinite or severely biased
# coefficients under separation.
#
# FLIC (Firth's Logistic regression with Intercept Correction) additionally
# corrects the intercept so that predicted probabilities are calibrated
# for the population prevalence rather than the sample prevalence. This
# matters when predicting new clubs, whose class distribution matches
# the population, not the training sample.

# --- 9a: Fit FLIC on the FULL dataset (deployment model) ---
flic_full <- flic(outcome ~ Size + RentPSF + PopYoungAdult24,
                  data = club_top3)

print("FLIC Model -- Firth Logistic Regression with Intercept Correction")
print(summary(flic_full))

# Coefficient interpretation
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
# For each bootstrap resample: fit FLIC on the resample (in-bag),
# predict on out-of-bag (OOB) observations, compute precision & recall.
# This estimates how the model performs on new clubs.
# A for-loop is used instead of purrr::map because flic()'s formula
# evaluation does not resolve variables correctly inside map()'s
# anonymous function environment.
set.seed(789)
n_boot <- nrow(boot_resamples)
flic_fits  <- vector("list", n_boot)
oob_preds  <- vector("list", n_boot)

for (i in 1:n_boot) {
  split     <- boot_resamples$splits[[i]]
  train_data <- analysis(split)

  flic_fits[[i]] <- tryCatch(
    flic(outcome ~ Size + RentPSF + PopYoungAdult24, data = train_data),
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

# Pooled OOB predictions across all bootstraps
oob_predictions <- bind_rows(oob_preds)
cat(sprintf("Total OOB predictions: %d\n", nrow(oob_predictions)))

# --- Overall metrics on pooled OOB predictions ---
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

# --- Per-bootstrap metrics for variability ---
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

# --- Distribution of OOB predicted probabilities ---
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
