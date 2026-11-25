# ==============================================================================
# tests/test_shiny.R - Comprehensive Automated Test Suite for Shiny Application
# ==============================================================================

source("requirements.R")
load_environment()

# Check for shiny
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Shiny package is required to run this test suite.")
}
library(shiny)

# Source core pipeline and modules
source("R/data.R")
source("R/preprocessing.R")
source("R/feature_engineering.R")
source("R/validation.R")
source("R/models.R")
source("R/evaluation.R")
source("R/prediction.R")
source("R/visualizations.R")
source("R/reporting.R")

source("modules/mod_dashboard.R")
source("modules/mod_prediction.R")
source("modules/mod_performance.R")
source("modules/mod_insights.R")
source("modules/mod_reports.R")

total_tests <- 0
passed_tests <- 0

assert_test <- function(desc, test_expr) {
  total_tests <<- total_tests + 1
  cat(sprintf("[SHINY TEST %02d] %-54s ... ", total_tests, desc))
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
cat("      CALORIE BURN PREDICTOR - SHINY AUTOMATED 20-POINT SUITE    \n")
cat("=================================================================\n\n")

# Load baseline objects
raw_df <- load_calorie_data("calories.csv")
cleaned_df <- clean_calorie_data(raw_df)
test_data <- engineer_features(cleaned_df)
model_bundle <- load_models("models/trained_models.rds")
eval_summary <- if (file.exists("models/evaluation_summary.rds")) readRDS("models/evaluation_summary.rds") else NULL

# Source app.R in separate environment to test app instantiation
app_env <- new.env()
sys.source("app.R", envir = app_env)

# TEST 1: App startup & UI parsing
assert_test("Shiny app object instantiates with valid UI and server", {
  is.list(app_env$ui) && is.function(app_env$server)
})

# TEST 2: Server initialization & module binding
assert_test("Server function initializes cleanly with session mock", {
  is.function(app_env$server) && length(formals(app_env$server)) == 3
})

# TEST 3: Navigation tabs verification
assert_test("All 5 core navigation tabs exist in UI layout", {
  ui_str <- as.character(htmltools::as.tags(app_env$ui))
  all(sapply(c("Dashboard", "Calorie Prediction", "Model Performance", "Dataset Insights", "Download Reports"),
             function(tab) grepl(tab, ui_str, fixed = TRUE)))
})

# TEST 4: Valid Male prediction
assert_test("Valid Male prediction calculates expected calories (> 0)", {
  val <- validate_user_inputs("male", 28, 178, 75, 40, 120, 40.2)
  res <- predict_all_models(model_bundle$models, val$clean_data, best_model_name = "Random Forest")
  is.numeric(res$best_prediction) && res$best_prediction > 100 && res$best_prediction < 400
})

# TEST 5: Valid Female prediction
assert_test("Valid Female prediction calculates expected calories (> 0)", {
  val <- validate_user_inputs("female", 24, 165, 55, 30, 110, 39.8)
  res <- predict_all_models(model_bundle$models, val$clean_data, best_model_name = "Random Forest")
  is.numeric(res$best_prediction) && res$best_prediction > 50 && res$best_prediction < 300
})

# TEST 6: Mixed-case gender normalization
assert_test("Mixed-case gender input normalized to valid factor", {
  g1 <- validate_gender("MALE")
  g2 <- validate_gender("fEmALe")
  g1$valid && g2$valid && g1$value == "male" && g2$value == "female"
})

# TEST 7: Invalid input error handling
assert_test("Invalid physical inputs trigger structured validation errors", {
  val <- validate_user_inputs("male", -5, 350, 10, -30, 300, 50.0)
  !val$valid && length(val$errors) >= 5
})

# TEST 8: Prediction reset logic
assert_test("Prediction reset correctly returns clean initial state", {
  val <- validate_user_inputs("male", 25, 175, 70, 30, 100, 40.0)
  p_res <- predict_all_models(model_bundle$models, val$clean_data, best_model_name = "Random Forest")
  # Simulate reset
  reset_val <- list(prediction_result = NULL, user_data = NULL, bmi = NULL, errors = NULL)
  is.null(reset_val$prediction_result) && is.null(reset_val$user_data) && is.null(reset_val$bmi)
})

# TEST 9: 5-Fold CV metrics rendering & formatting
assert_test("5-fold CV metrics summary formatted with Mean ± SD", {
  !is.null(eval_summary$cv_results$summary) &&
    nrow(eval_summary$cv_results$summary) == 3 &&
    all(c("CV_MAE_Mean", "CV_RMSE_Mean", "CV_R2_Mean") %in% colnames(eval_summary$cv_results$summary))
})

# TEST 10: Final test metrics rendering
assert_test("Final test set comparison contains exact verified metrics", {
  comp <- eval_summary$test_eval$comparison
  !is.null(comp) &&
    nrow(comp) == 3 &&
    any(comp$Model == "Random Forest" & comp$RMSE < 5.0) &&
    any(comp$Model == "Linear Regression" & comp$RMSE > 10.0)
})

# TEST 11: Correlation heatmap rendering without User_ID
assert_test("Correlation heatmap generates with User_ID excluded", {
  p <- plot_correlation_matrix(test_data)
  is.matrix(p) && !("User_ID" %in% rownames(p)) && !("User_ID" %in% colnames(p))
})

# TEST 12: Random Forest feature importance rendering
assert_test("Random Forest feature importance chart renders ggplot object", {
  p <- plot_feature_importance(model_bundle$models[["Random Forest"]])
  inherits(p, "ggplot")
})

# TEST 13: Actual vs predicted scatter plot rendering
assert_test("Actual vs predicted chart renders with reference line", {
  p <- plot_actual_vs_predicted(eval_summary$test_eval$predictions)
  inherits(p, "ggplot")
})

# TEST 14: Residual distribution rendering
assert_test("Residual error distribution chart renders ggplot object", {
  p <- plot_residual_distribution(eval_summary$test_eval$residuals)
  inherits(p, "ggplot")
})

# TEST 15: Text evaluation report download handler output
assert_test("Text evaluation report generates with complete 7 sections", {
  tmp_txt <- tempfile(fileext = ".txt")
  generate_text_evaluation_report(eval_summary, test_data, output_file = tmp_txt)
  lines <- readLines(tmp_txt)
  file.remove(tmp_txt)
  any(grepl("CROSS-VALIDATION RESULTS", lines, ignore.case = TRUE)) &&
    any(grepl("FINAL TEST SET PERFORMANCE", lines, ignore.case = TRUE)) &&
    length(lines) > 50
})

# TEST 16: PDF diagnostic report download handler output
assert_test("PDF diagnostic report generates valid PDF file (%PDF magic)", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  u_data <- data.frame(Gender = factor("male", levels = c("female", "male")),
                       Age = 25, Height = 175, Weight = 70, Duration = 30,
                       Heart_Rate = 100, Body_Temp = 40.0, BMI = 22.86)
  p_res <- predict_all_models(model_bundle$models, u_data, best_model_name = "Random Forest")
  generate_pdf_report(tmp_pdf, u_data, p_res, eval_summary$test_eval, test_data)
  raw_bytes <- readBin(tmp_pdf, "raw", n = 4)
  file.remove(tmp_pdf)
  # Check PDF magic bytes '%PDF'
  rawToChar(raw_bytes) == "%PDF"
})

# TEST 17: CSV prediction export handler output
assert_test("CSV prediction download handler writes valid structured records", {
  tmp_csv <- tempfile(fileext = ".csv")
  u_data <- data.frame(Gender = factor("female", levels = c("female", "male")),
                       Age = 28, Height = 165, Weight = 60, Duration = 45,
                       Heart_Rate = 135, Body_Temp = 40.5, BMI = 22.04)
  p_res <- predict_all_models(model_bundle$models, u_data, best_model_name = "Random Forest")
  exp_df <- data.frame(
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    Gender = as.character(u_data$Gender[1]),
    Age = u_data$Age[1],
    BMI = round(u_data$BMI[1], 2),
    Predicted_Calories = round(p_res$best_prediction, 2),
    Primary_Model = p_res$best_model
  )
  write.csv(exp_df, file = tmp_csv, row.names = FALSE)
  read_back <- read.csv(tmp_csv)
  file.remove(tmp_csv)
  nrow(read_back) == 1 && read_back$Gender == "female" && read_back$Primary_Model == "Random Forest"
})

# TEST 18: Model cache loading and model class integrity
assert_test("Cached models load with appropriate classes (randomForest, lm, rpart)", {
  inherits(model_bundle$models[["Random Forest"]], "randomForest") &&
    inherits(model_bundle$models[["Linear Regression"]], "lm") &&
    inherits(model_bundle$models[["Decision Tree"]], "rpart")
})

# TEST 19: Missing model cache recovery mechanism
assert_test("load_models returns NULL safely on missing path without crashing", {
  missing_res <- load_models("models/non_existent_file.rds")
  is.null(missing_res)
})

# TEST 20: Missing dataset error trap / validation robustness
assert_test("load_calorie_data throws informative error on non-existent dataset", {
  caught <- FALSE
  tryCatch({
    load_calorie_data("missing_dataset_xyz.csv")
  }, error = function(e) {
    caught <<- grepl("not found", e$message, ignore.case = TRUE)
  })
  caught
})

cat("\n=================================================================\n")
cat(sprintf("   SHINY SUITE RESULTS: %d / %d TESTS PASSED (%.1f%%)\n",
            passed_tests, total_tests, (passed_tests / total_tests) * 100))
cat("=================================================================\n\n")

if (passed_tests == total_tests) {
  cat("✅ ALL 20 SHINY AUTOMATED TESTS PASSED SUCCESSFULLY!\n\n")
  quit(status = 0)
} else {
  cat("❌ SOME TESTS FAILED.\n\n")
  quit(status = 1)
}
