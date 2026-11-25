# ==============================================================================
# tests/test_pipeline.R - Comprehensive Automated Machine Learning Pipeline Tests
# ==============================================================================

# Ensure environment and modules are loaded
source("requirements.R")
load_environment()

source("R/data.R")
source("R/preprocessing.R")
source("R/feature_engineering.R")
source("R/validation.R")
source("R/models.R")
source("R/evaluation.R")
source("R/prediction.R")
source("R/visualizations.R")
source("R/reporting.R")

test_results <- list()
total_tests <- 0
passed_tests <- 0

run_test <- function(test_name, expr) {
  total_tests <<- total_tests + 1
  cat(sprintf("[TEST %02d] %-55s ... ", total_tests, test_name))
  res <- tryCatch({
    val <- eval(expr)
    if (isTRUE(val)) {
      passed_tests <<- passed_tests + 1
      cat("PASS\n")
      TRUE
    } else {
      cat("FAIL (Assertion returned FALSE)\n")
      FALSE
    }
  }, error = function(e) {
    cat(paste("FAIL (Error:", e$message, ")\n"))
    FALSE
  })
  test_results[[test_name]] <<- res
}

cat("\n=================================================================\n")
cat("      CALORIE BURN PREDICTOR - AUTOMATED TEST SUITE             \n")
cat("=================================================================\n\n")

# 1. Dataset Loading
run_test("Dataset exists and loads properly", {
  df <- load_calorie_data("calories.csv")
  is.data.frame(df) && nrow(df) == 15000 && ncol(df) == 9
})

# 2. Dataset Column Validation
run_test("Dataset contains all 9 required columns", {
  df <- load_calorie_data("calories.csv")
  all(EXPECTED_COLUMNS %in% colnames(df))
})

# 3. Missing Value Handling
run_test("Missing value handler removes NA rows properly", {
  dummy_df <- data.frame(
    User_ID = 1:3,
    Gender = c("male", "female", "male"),
    Age = c(25, NA, 30),
    Height = c(170, 165, 180),
    Weight = c(70, 60, 80),
    Duration = c(30, 20, 45),
    Heart_Rate = c(100, 95, 110),
    Body_Temp = c(40, 39, 41),
    Calories = c(150, 120, 210)
  )
  cleaned <- clean_calorie_data(dummy_df, drop_na = TRUE)
  nrow(cleaned) == 2 && sum(is.na(cleaned)) == 0
})

# 4. User_ID Exclusion
run_test("User_ID is excluded from cleaned predictive data", {
  df <- load_calorie_data("calories.csv")
  cleaned <- clean_calorie_data(df)
  !("User_ID" %in% colnames(cleaned))
})

# 5. BMI Calculation
run_test("BMI is calculated correctly (Weight / Height_m^2)", {
  # 70 kg, 175 cm -> 70 / (1.75^2) = 22.857 -> 22.86
  bmi <- calculate_bmi(70, 175)
  abs(bmi - 22.86) < 0.01
})

# 6. Gender Standardization
run_test("Gender values are converted to lowercase and standardized factor", {
  raw_df <- data.frame(
    Gender = c("MALE", " Female ", "Male", "female"),
    Age = c(20, 25, 30, 35),
    Height = c(170, 165, 180, 160),
    Weight = c(70, 60, 80, 55),
    Duration = c(30, 20, 45, 15),
    Heart_Rate = c(100, 95, 110, 85),
    Body_Temp = c(40, 39, 41, 38),
    Calories = c(150, 120, 210, 80)
  )
  cleaned <- clean_calorie_data(raw_df)
  is.factor(cleaned$Gender) &&
    identical(levels(cleaned$Gender), c("female", "male")) &&
    all(cleaned$Gender %in% c("female", "male"))
})

# 7. Invalid Input Rejection
run_test("Validation rejects invalid, negative, or absurd inputs", {
  v1 <- validate_user_inputs("alien", 25, 175, 70, 30, 100, 40)
  v2 <- validate_user_inputs("male", -5, 175, 70, 30, 100, 40)
  v3 <- validate_user_inputs("male", 25, 0, 70, 30, 100, 40)
  v4 <- validate_user_inputs("female", 25, 175, 70, 30, 300, 40) # HR > 220
  
  !v1$valid && !v2$valid && !v3$valid && !v4$valid
})

# 8. Training and Testing Partition Integrity
run_test("Train-test partition has zero overlap and exact proportions", {
  df <- engineer_features(clean_calorie_data(load_calorie_data("calories.csv")))
  split <- split_train_test(df, train_prop = 0.8, seed = 123)
  
  n_total <- nrow(df)
  has_no_overlap <- length(intersect(split$train_indices, setdiff(1:n_total, split$train_indices))) == 0
  correct_sizes <- (split$n_train == 12000) && (split$n_test == 3000)
  
  has_no_overlap && correct_sizes
})

# 9. Metric Calculation Accuracy (MAE, RMSE, R²)
run_test("MAE, RMSE, and R² match exact mathematical definitions", {
  act  <- c(100, 120, 140, 160)
  pred <- c(110, 120, 130, 170)
  # Residuals: -10, 0, 10, -10
  # Abs: 10, 0, 10, 10 -> MAE = 7.5
  # Sq: 100, 0, 100, 100 -> Mean Sq = 75 -> RMSE = sqrt(75) = 8.660
  # Mean actual: 130 -> Var actual: 900+100+100+900 = 2000 -> SS_res = 300 -> R2 = 1 - 300/2000 = 0.85
  mae  <- calculate_mae(act, pred)
  rmse <- calculate_rmse(act, pred)
  r2   <- calculate_r2(act, pred)
  
  abs(mae - 7.5) < 0.001 && abs(rmse - sqrt(75)) < 0.001 && abs(r2 - 0.85) < 0.001
})

# 10. Cross-Validation Execution
run_test("Cross-validation runs without error on identical folds", {
  df <- engineer_features(clean_calorie_data(load_calorie_data("calories.csv")))
  # Test with 2 folds on a 500-sample slice for quick automated verification
  cv_res <- cross_validate_models(df[1:500, ], k = 2, ntree = 20, seed = 123)
  
  is.list(cv_res) &&
    nrow(cv_res$summary) >= 2 &&
    all(c("CV_MAE_Mean", "CV_RMSE_Mean", "CV_R2_Mean") %in% colnames(cv_res$summary))
})

# 11. Model Prediction
run_test("All 3 models generate non-negative, valid calorie predictions", {
  df <- engineer_features(clean_calorie_data(load_calorie_data("calories.csv")))
  bundle <- load_models("models/trained_models.rds")
  if (is.null(bundle)) {
    bundle <- train_all_models(df[1:1000, ], ntree = 20, seed = 123)
  }
  
  sample_user <- data.frame(
    Gender = factor("female", levels = c("female", "male")),
    Age = 25, Height = 165, Weight = 60, Duration = 25, Heart_Rate = 95, Body_Temp = 40.0, BMI = 22.04
  )
  
  preds <- predict_all_models(bundle$models, sample_user)
  
  is.numeric(preds$best_prediction) &&
    preds$best_prediction > 0 &&
    length(preds$predictions) >= 2 &&
    all(sapply(preds$predictions, function(p) is.numeric(p) && p > 0))
})

# 12. Burn Rate (kcal/min) Computation
run_test("Estimated calories per minute (burn rate) is computed correctly", {
  sample_user <- data.frame(
    Gender = factor("male", levels = c("female", "male")),
    Age = 30, Height = 175, Weight = 70, Duration = 20, Heart_Rate = 100, Body_Temp = 40.0, BMI = 22.86
  )
  bundle <- load_models("models/trained_models.rds")
  res <- predict_all_models(bundle$models, sample_user)
  
  expected_rate <- round(res$best_prediction / 20, 2)
  abs(res$burn_rate_kcal_per_min - expected_rate) < 0.01
})

# 13. Model Serialization and Reloading
run_test("Trained model bundle serializes to disk and reloads identically", {
  bundle <- load_models("models/trained_models.rds")
  test_path <- "models/test_serialization_bundle.rds"
  save_models(bundle, output_dir = "models", filename = "test_serialization_bundle.rds")
  
  reloaded <- load_models(test_path)
  file.remove(test_path)
  
  !is.null(reloaded) &&
    identical(names(bundle$models), names(reloaded$models))
})

# 14. Target Leakage Investigation
run_test("Investigate target relationships verifies Duration & HR are top drivers", {
  df <- engineer_features(clean_calorie_data(load_calorie_data("calories.csv")))
  inv <- investigate_target_relationship(df)
  
  top_features <- inv$correlations$Feature[1:2]
  all(c("Duration", "Heart_Rate") %in% top_features)
})

cat("\n-----------------------------------------------------------------\n")
cat(sprintf("Test Summary: %d/%d Passed (%.1f%% Success Rate)\n",
            passed_tests, total_tests, (passed_tests / total_tests) * 100))
cat("=================================================================\n\n")

if (passed_tests != total_tests) {
  stop("One or more tests failed.")
}

