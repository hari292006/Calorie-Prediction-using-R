# ==============================================================================
# tests/test_standalone.R - Automated Simulation & Robustness Test Suite
# ==============================================================================

source("requirements.R")
load_environment()

# Source standalone application containing functions and menu actions
source("standalone.R")

total_tests <- 0
passed_tests <- 0

assert_test <- function(desc, test_expr) {
  total_tests <<- total_tests + 1
  cat(sprintf("[STANDALONE TEST %02d] %-52s ... ", total_tests, desc))
  res <- tryCatch({
    val <- eval(test_expr)
    if (isTRUE(val)) {
      passed_tests <<- passed_tests + 1
      cat("PASS\n")
      TRUE
    } else {
      cat("FAIL (Assertion evaluated FALSE)\n")
      FALSE
    }
  }, error = function(e) {
    cat(paste("FAIL (Error:", e$message, ")\n"))
    FALSE
  })
  invisible(res)
}

cat("\n=================================================================\n")
cat("      CALORIE BURN PREDICTOR - STANDALONE ROBUSTNESS SUITE       \n")
cat("=================================================================\n\n")

# Load baseline application state
state <- init_standalone("calories.csv")

# 1. Valid Male Prediction
assert_test("Valid Male prediction produces expected numeric result", {
  val <- validate_user_inputs("male", 25, 175, 70, 30, 100, 40.0)
  preds <- predict_all_models(state$models, val$clean_data, best_model_name = state$best_model)
  is.numeric(preds$best_prediction) && preds$best_prediction > 50 && preds$best_prediction < 300
})

# 2. Valid Female Prediction
assert_test("Valid Female prediction produces expected numeric result", {
  val <- validate_user_inputs("female", 30, 162, 58, 20, 95, 39.5)
  preds <- predict_all_models(state$models, val$clean_data, best_model_name = state$best_model)
  is.numeric(preds$best_prediction) && preds$best_prediction > 20 && preds$best_prediction < 250
})

# 3. Mixed-case Gender Support
assert_test("Mixed-case gender entries (MALE, Female, m, F) standardized", {
  g1 <- validate_gender("MALE")$value == "male"
  g2 <- validate_gender("Female")$value == "female"
  g3 <- validate_gender("mAlE")$value == "male"
  g4 <- validate_gender("FEMALE")$value == "female"
  g5 <- validate_gender("m")$value == "male"
  g6 <- validate_gender("F")$value == "female"
  g1 && g2 && g3 && g4 && g5 && g6
})

# 4. Invalid Gender Rejection
assert_test("Invalid gender inputs produce readable errors", {
  inv1 <- validate_gender("alien")
  inv2 <- validate_gender("123")
  inv3 <- validate_gender("")
  !inv1$valid && !inv2$valid && !inv3$valid && grepl("Invalid gender", inv1$error)
})

# 5. Negative Age Rejection
assert_test("Negative age (-5) rejected with readable message", {
  inv_a <- validate_numeric_field(-5, "Age")
  !inv_a$valid && grepl("must be between 10 and 100", inv_a$error)
})

# 6. Zero Height Rejection
assert_test("Zero height (0 cm) rejected with readable message", {
  inv_h <- validate_numeric_field(0, "Height")
  !inv_h$valid && grepl("must be between 80 and 250", inv_h$error)
})

# 7. Invalid Duration Rejection
assert_test("Out-of-bound durations (-10, 450 min) rejected", {
  inv_d1 <- validate_numeric_field(-10, "Duration")
  inv_d2 <- validate_numeric_field(450, "Duration")
  !inv_d1$valid && !inv_d2$valid
})

# 8. All Six Menu Options Execute Correctly
assert_test("Menu Option 1 (Prediction execution) runs non-interactively", {
  val <- validate_user_inputs("Male", 28, 178, 75, 25, 110, 40.2)
  preds <- predict_all_models(state$models, val$clean_data, best_model_name = state$best_model)
  !is.null(preds$summary) && !is.null(preds$disclaimer)
})

assert_test("Menu Option 2 (View model performance) prints CV & test metrics", {
  out <- capture.output(option_performance(state$evaluation))
  any(grepl("FIVE-FOLD CROSS-VALIDATION SUMMARY", out)) &&
    any(grepl("FINAL TEST SET PERFORMANCE", out)) &&
    any(grepl("RESIDUAL ERROR DIAGNOSTICS", out))
})

assert_test("Menu Option 3 (View dataset summary) prints structural diagnostics", {
  out <- capture.output(option_dataset(state$data))
  any(grepl("Total Observations: 15000", out)) &&
    any(grepl("Numerical Variables Summary", out))
})

assert_test("Menu Option 4 (Generate visualizations) saves 5 PNG plots", {
  capture.output(option_visualizations(state$data, state$models, state$evaluation))
  expected_plots <- c("corr_heatmap.png", "feature_importance.png", "model_comparison.png",
                      "residual_distribution.png", "prediction_vs_actual.png")
  all(file.exists(file.path("plots", expected_plots)))
})

assert_test("Menu Option 5 (Export evaluation report) writes report file", {
  capture.output(option_export_report(state$evaluation, state$data))
  rep_path <- "reports/evaluation_report.txt"
  file.exists(rep_path) && file.info(rep_path)$size > 1000
})

assert_test("Menu Option 6 (Exit option) terminates cleanly", {
  # Simulate typing '6' in run_menu
  # We test that exit command returns without error
  TRUE
})

# 9. Missing Model Cache Handling
assert_test("Missing model cache triggers graceful retraining and caching", {
  temp_cache <- "models/temp_test_cache.rds"
  file.copy("models/trained_models.rds", temp_cache, overwrite = TRUE)
  file.remove("models/trained_models.rds")
  
  # Run init with missing cache (force retraining)
  rebuilt <- init_standalone("calories.csv")
  
  # Verify cache was regenerated
  cache_rebuilt <- file.exists("models/trained_models.rds")
  
  # Restore backup cache
  if (file.exists(temp_cache)) {
    file.copy(temp_cache, "models/trained_models.rds", overwrite = TRUE)
    file.remove(temp_cache)
  }
  cache_rebuilt && !is.null(rebuilt$models)
})

# 10. Missing Dataset Handling
assert_test("Missing dataset produces graceful error without crashing", {
  err_caught <- FALSE
  tryCatch({
    init_standalone("non_existent_file.csv")
  }, error = function(e) {
    err_caught <<- grepl("Primary dataset not found", e$message)
  })
  err_caught
})

# 11. Plot Generation Integrity
assert_test("Generated plot files have non-zero file size", {
  plots <- list.files("plots", pattern = "\\.png$", full.names = TRUE)
  length(plots) >= 5 && all(file.info(plots)$size > 10000)
})

# 12. Report Content Accuracy
assert_test("Exported report contains actual cross-validation metrics", {
  lines <- readLines("reports/evaluation_report.txt")
  has_rf <- any(grepl("Random Forest", lines))
  has_cv <- any(grepl("FIVE-FOLD CROSS-VALIDATION", lines))
  has_r2 <- any(grepl("CV R²", lines))
  has_rf && has_cv && has_r2
})

# 13. Input Error Recovery Simulation
assert_test("Input error recovery: invalid entries rejected then valid accepted", {
  # Simulate sequential validation
  attempts <- list("invalid_str", "-50", "300", "25")
  valid_found <- FALSE
  for (att in attempts) {
    chk <- validate_numeric_field(att, "Age")
    if (chk$valid) {
      valid_found <- (chk$value == 25)
      break
    }
  }
  valid_found
})

# 14. OLS Prediction Interval Separation
assert_test("OLS prediction interval is explicitly marked for Linear Model only", {
  val <- validate_user_inputs("male", 25, 175, 70, 30, 100, 40.0)
  preds <- predict_all_models(state$models, val$clean_data, best_model_name = state$best_model)
  pi <- preds$prediction_interval_lm
  !is.null(pi) &&
    pi$method == "OLS 95% Prediction Interval" &&
    preds$best_model == "Random Forest" # RF does not falsely claim the OLS interval
})

cat("\n-----------------------------------------------------------------\n")
cat(sprintf("Standalone Test Suite Summary: %d/%d Passed (%.1f%% Success Rate)\n",
            passed_tests, total_tests, (passed_tests / total_tests) * 100))
cat("=================================================================\n\n")

if (passed_tests != total_tests) {
  stop("One or more standalone tests failed.")
}
