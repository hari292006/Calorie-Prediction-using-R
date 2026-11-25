# ==============================================================================
# tests/run_full_evaluation.R - Rigorous 5-Fold Cross-Validation and Final Test Evaluation
# ==============================================================================

source("requirements.R")
load_environment()

source("R/data.R")
source("R/preprocessing.R")
source("R/feature_engineering.R")
source("R/models.R")
source("R/evaluation.R")
source("R/prediction.R")
source("R/visualizations.R")

cat("\n=================================================================\n")
cat("      CALORIE BURN PREDICTOR - RIGOROUS EVALUATION RUN          \n")
cat("=================================================================\n\n")

# Step 1: Load and preprocess data
df <- engineer_features(clean_calorie_data(load_calorie_data("calories.csv")))
split <- split_train_test(df, train_prop = 0.8, seed = 123)

cat("Training partition observations:", split$n_train, "\n")
cat("Testing partition observations :", split$n_test, "\n\n")

# Step 2: 5-Fold Cross-Validation on Training Set (Zero leakage)
cat("--- Executing 5-Fold Cross-Validation on Training Partition (12,000 obs) ---\n")
cv_results <- cross_validate_models(split$train, k = 5, ntree = 60, seed = 123)

cat("\n5-Fold Cross-Validation Summary Table:\n")
print(cv_results$summary)

cat("\nSelected Best Model via CV RMSE:", cv_results$best_model, "\n\n")

# Step 3: Train final models on entire training partition
cat("--- Training Final Models on Complete Training Partition ---\n")
final_models <- train_all_models(split$train, ntree = 80, seed = 123)
save_models(final_models, output_dir = "models", filename = "trained_models.rds")

# Step 4: Final Test Set Evaluation (Untouched 3,000 observations)
cat("--- Evaluating on Untouched Test Partition (3,000 obs) ---\n")
test_eval <- evaluate_all_models(final_models$models, split$test, target_col = "Calories")

cat("\nFinal Test-Set Performance Table:\n")
print(test_eval$comparison)

# Step 5: Residual Diagnostics on Test Set
cat("\n--- Residual Diagnostics & Outlier Analysis (Test Set) ---\n")
res_diag <- analyze_residuals(test_eval$residuals)
print(res_diag)

# Step 6: Target Relationship Analysis
cat("\n--- Target Correlation & Feature Power Analysis ---\n")
target_inv <- investigate_target_relationship(df)
print(target_inv$correlations)
cat("\nInterpretation:\n", target_inv$interpretation, "\n\n")

# Step 7: Save Evaluation Artifacts
eval_artifact <- list(
  cv_results = cv_results,
  test_eval = test_eval,
  residual_diagnostics = res_diag,
  target_investigation = target_inv,
  timestamp = Sys.time()
)
saveRDS(eval_artifact, file = "models/evaluation_summary.rds")
cat("Saved complete evaluation summary to: models/evaluation_summary.rds\n")
cat("=================================================================\n\n")

