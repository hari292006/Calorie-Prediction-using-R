# ==============================================================================
# standalone.R - Complete Independent Calorie Burn Predictor Application
# ==============================================================================

# Ensure environment and local libraries are loaded
source("requirements.R")
load_environment()

# Source all shared R pipeline modules
source("R/data.R")
source("R/preprocessing.R")
source("R/feature_engineering.R")
source("R/validation.R")
source("R/models.R")
source("R/evaluation.R")
source("R/prediction.R")
source("R/visualizations.R")
source("R/reporting.R")

# ------------------------------------------------------------------------------
# Input Reading & Defensive Reprompting Helpers
# ------------------------------------------------------------------------------

#' Safe prompt reader supporting interactive console, stdin, and simulated input
#' @param prompt_msg Message to display
#' @return Trimmed character string entered by user
read_line_safe <- function(prompt_msg) {
  cat(prompt_msg)
  flush.console()
  
  # Check if running in standard interactive session or piped stdin
  if (interactive()) {
    line <- readline()
  } else {
    con <- file("stdin", "r")
    line <- tryCatch(readLines(con, n = 1, warn = FALSE), error = function(e) "")
    # Note: do not close stdin
  }
  
  if (length(line) == 0) return("")
  return(trimws(line))
}

#' Prompt for a numeric parameter with validation and automatic retry
#' @param param_name Name of parameter (e.g. 'Age')
#' @param prompt_label Display prompt
#' @param min_val Minimum physiological bound
#' @param max_val Maximum physiological bound
#' @return Validated numeric value
prompt_numeric <- function(param_name, prompt_label, min_val, max_val) {
  repeat {
    input_str <- read_line_safe(paste0("  Enter ", prompt_label, " [", min_val, " - ", max_val, "]: "))
    if (input_str == "q" || input_str == "quit" || input_str == "exit") {
      stop("USER_CANCEL")
    }
    
    val_res <- validate_numeric_field(input_str, param_name)
    if (val_res$valid) {
      return(val_res$value)
    } else {
      cat("  ❌ Error:", val_res$error, "Please try again (or enter 'q' to cancel).\n")
    }
  }
}

#' Prompt for Gender with case-insensitive validation and retry
#' @return Validated character string ("female" or "male")
prompt_gender <- function() {
  repeat {
    input_str <- read_line_safe("  Enter Gender (Male/Female/M/F): ")
    if (input_str == "q" || input_str == "quit" || input_str == "exit") {
      stop("USER_CANCEL")
    }
    
    val_res <- validate_gender(input_str)
    if (val_res$valid) {
      return(val_res$value)
    } else {
      cat("  ❌ Error:", val_res$error, "Please try again (or enter 'q' to cancel).\n")
    }
  }
}

# ------------------------------------------------------------------------------
# Menu Action Implementations
# ------------------------------------------------------------------------------

#' Option 1: Predict Calories Burned
option_predict <- function(model_bundle, best_model, eval_summary) {
  cat("\n=================================================================\n")
  cat("             CALORIE BURN PREDICTION INTERFACE                   \n")
  cat("=================================================================\n")
  cat("Please enter your physical parameters and workout session details:\n\n")
  
  user_vals <- tryCatch({
    g  <- prompt_gender()
    a  <- prompt_numeric("Age", "Age (years)", 10, 100)
    h  <- prompt_numeric("Height", "Height (cm)", 80, 250)
    w  <- prompt_numeric("Weight", "Weight (kg)", 25, 250)
    d  <- prompt_numeric("Duration", "Workout Duration (minutes)", 1, 300)
    hr <- prompt_numeric("Heart_Rate", "Exercise Heart Rate (bpm)", 40, 220)
    bt <- prompt_numeric("Body_Temp", "Body Temperature (°C)", 35.0, 43.0)
    
    list(gender = g, age = a, height = h, weight = w, duration = d, heart_rate = hr, body_temp = bt)
  }, error = function(e) {
    if (grepl("USER_CANCEL", e$message)) {
      cat("\nPrediction canceled by user. Returning to main menu.\n")
      return(NULL)
    }
    stop(e)
  })
  
  if (is.null(user_vals)) return(invisible(NULL))
  
  # Format input data
  val_res <- validate_user_inputs(
    gender = user_vals$gender,
    age = user_vals$age,
    height = user_vals$height,
    weight = user_vals$weight,
    duration = user_vals$duration,
    heart_rate = user_vals$heart_rate,
    body_temp = user_vals$body_temp
  )
  
  if (!val_res$valid) {
    cat("❌ Validation failed unexpectedly:\n")
    for (err in val_res$errors) cat("  -", err, "\n")
    return(invisible(NULL))
  }
  
  # Generate Prediction across models
  preds <- predict_all_models(model_bundle$models, val_res$clean_data, best_model_name = best_model)
  
  # Retrieve validation RMSE for selected model
  val_rmse <- NA_real_
  if (!is.null(eval_summary$test_eval$comparison)) {
    comp <- eval_summary$test_eval$comparison
    row_idx <- which(comp$Model == best_model)
    if (length(row_idx) > 0) val_rmse <- comp$RMSE[row_idx]
  }
  
  # BMI status description (non-medical)
  bmi_category <- as.character(classify_bmi(preds$bmi))
  
  cat("\n=================================================================\n")
  cat("                     PREDICTION SUMMARY                          \n")
  cat("=================================================================\n")
  cat(sprintf("  • Predicted Energy Expenditure: %s kcal\n", format(round(preds$best_prediction, 1), nsmall = 1)))
  cat(sprintf("  • Selected Prediction Model   : %s\n", preds$best_model))
  if (!is.na(val_rmse)) {
    cat(sprintf("  • Model Validation RMSE       : %.3f kcal\n", val_rmse))
  }
  cat(sprintf("  • Estimated Burn Rate Intensity: %.2f kcal / minute\n", preds$burn_rate_kcal_per_min))
  cat(sprintf("  • Calculated Body Mass Index  : %.2f kg/m² (%s)\n", preds$bmi, bmi_category))
  
  cat("\n  Predictions Across Alternative Architectures:\n")
  for (m_name in names(preds$predictions)) {
    marker <- ifelse(m_name == preds$best_model, " (Optimal)", "")
    cat(sprintf("    - %-20s: %6.1f kcal%s\n", m_name, preds$predictions[[m_name]], marker))
  }
  
  # Prediction Interval (Linear Model)
  if (!is.null(preds$prediction_interval_lm)) {
    pi <- preds$prediction_interval_lm
    cat(sprintf("\n  • Linear Model 95%% Prediction Interval: [%.1f kcal, %.1f kcal]\n", pi$lwr, pi$upr))
    cat("    (Note: OLS interval assumes Gaussian errors; applies to Linear Model only)\n")
  }
  
  cat("\n  Explanation of Prediction:\n")
  cat(sprintf("  Caloric burn is driven primarily by workout duration (%d min) and cardiovascular\n", as.integer(user_vals$duration)))
  cat(sprintf("  intensity (Heart Rate %d bpm). With a body temperature of %.1f °C, metabolic heat\n", as.integer(user_vals$heart_rate), user_vals$body_temp))
  cat(sprintf("  production indicates sustained aerobic exertion for a %s participant.\n", user_vals$gender))
  
  cat("\n  ⚠️  ETHICAL / NON-MEDICAL NOTICE:\n")
  cat("  Predicted calorie values and BMI calculations are statistical machine-learning\n")
  cat("  estimates derived from empirical data. They are not intended for clinical\n")
  cat("  diagnosis, medical prescriptions, or metabolic evaluation.\n")
  cat("=================================================================\n\n")
  
  return(invisible(preds))
}

#' Option 2: View Model Performance & Diagnostics
option_performance <- function(eval_summary) {
  cat("\n=================================================================\n")
  cat("              MODEL PERFORMANCE & VALIDATION METRICS             \n")
  cat("=================================================================\n\n")
  
  cat("1. FIVE-FOLD CROSS-VALIDATION SUMMARY (12,000 Training Observations):\n")
  cat("-----------------------------------------------------------------\n")
  if (!is.null(eval_summary$cv_results$summary)) {
    print(eval_summary$cv_results$summary)
  }
  cat("-----------------------------------------------------------------\n")
  cat(">> Optimal Model Selected via CV RMSE:", eval_summary$cv_results$best_model, "\n\n")
  
  cat("2. FINAL TEST SET PERFORMANCE (Untouched 3,000 Observations):\n")
  cat("-----------------------------------------------------------------\n")
  if (!is.null(eval_summary$test_eval$comparison)) {
    print(eval_summary$test_eval$comparison)
  }
  cat("-----------------------------------------------------------------\n\n")
  
  cat("3. RESIDUAL ERROR DIAGNOSTICS (Test Set Outliers):\n")
  cat("-----------------------------------------------------------------\n")
  if (!is.null(eval_summary$residual_diagnostics)) {
    rd <- eval_summary$residual_diagnostics[, c("Model", "Mean_Residual", "SD_Residual", "Skewness", "Outliers_3SD", "Outlier_Pct")]
    print(rd)
  }
  cat("-----------------------------------------------------------------\n\n")
}

#' Option 3: View Dataset Summary & Provenance
option_dataset <- function(dataset) {
  cat("\n=================================================================\n")
  cat("                 DATASET SPECIFICATIONS & SUMMARY                \n")
  cat("=================================================================\n\n")
  
  diag <- inspect_calorie_data(dataset)
  cat(sprintf("Total Observations: %d | Total Features: %d\n\n", diag$rows, diag$cols))
  
  cat("Numerical Variables Summary:\n")
  cat("-----------------------------------------------------------------\n")
  print(diag$numerical_summary[, c("Variable", "Min", "Median", "Mean", "Max", "SD")])
  cat("-----------------------------------------------------------------\n\n")
  
  cat("Categorical Breakdown (Gender):\n")
  print(diag$categorical_summary$Gender)
  cat("\nMissing Values Count per Column:\n")
  print(diag$missing_counts)
  cat("=================================================================\n\n")
}

#' Option 4: Generate and Save Visualizations
option_visualizations <- function(dataset, model_bundle, eval_summary) {
  cat("\n=================================================================\n")
  cat("                GENERATING DIAGNOSTIC VISUALIZATIONS             \n")
  cat("=================================================================\n\n")
  
  dir.create("plots", showWarnings = FALSE)
  
  cat("Generating [1/5] Feature Correlation Matrix ('plots/corr_heatmap.png')...\n")
  plot_correlation_matrix(dataset, file_path = "plots/corr_heatmap.png")
  
  cat("Generating [2/5] Random Forest Feature Importance ('plots/feature_importance.png')...\n")
  if (!is.null(model_bundle$models[["Random Forest"]])) {
    plot_feature_importance(model_bundle$models[["Random Forest"]], file_path = "plots/feature_importance.png")
  }
  
  cat("Generating [3/5] Model RMSE Comparison Chart ('plots/model_comparison.png')...\n")
  plot_model_comparison(eval_summary$test_eval$comparison, metric = "RMSE", file_path = "plots/model_comparison.png")
  
  cat("Generating [4/5] Residual Error Distribution ('plots/residual_distribution.png')...\n")
  plot_residual_distribution(eval_summary$test_eval$residuals, file_path = "plots/residual_distribution.png")
  
  cat("Generating [5/5] Actual vs. Predicted Calories ('plots/prediction_vs_actual.png')...\n")
  plot_actual_vs_predicted(eval_summary$test_eval$predictions, file_path = "plots/prediction_vs_actual.png")
  
  cat("\n✅ All 5 diagnostic figures successfully generated and saved under 'plots/':\n")
  for (f in list.files("plots", pattern = "\\.png$")) {
    cat("  • plots/", f, "\n", sep = "")
  }
  cat("=================================================================\n\n")
}

#' Option 5: Export Evaluation Report
option_export_report <- function(eval_summary, dataset) {
  cat("\n=================================================================\n")
  cat("                   EXPORTING EVALUATION REPORT                   \n")
  cat("=================================================================\n\n")
  
  report_txt <- generate_text_evaluation_report(eval_summary, dataset, output_file = "reports/evaluation_report.txt")
  cat("✅ Machine learning evaluation report written to:\n")
  cat("   • ", report_txt, "\n")
  cat("=================================================================\n\n")
}

# ------------------------------------------------------------------------------
# Console Menu Loop
# ------------------------------------------------------------------------------

#' Interactive Console Menu Loop
#' @param state Application environment containing data, models, and evaluation
run_menu <- function(state) {
  repeat {
    cat("=================================================================\n")
    cat("           CALORIE BURN PREDICTOR - CONSOLE MENU                 \n")
    cat("=================================================================\n")
    cat("  1. Predict calories burned\n")
    cat("  2. View model performance & cross-validation metrics\n")
    cat("  3. View dataset summary & diagnostics\n")
    cat("  4. Generate and save diagnostic visualizations\n")
    cat("  5. Export machine-learning evaluation report\n")
    cat("  6. Exit application\n")
    cat("=================================================================\n")
    
    choice <- read_line_safe("  Please enter option (1-6): ")
    
    if (choice == "1") {
      option_predict(state$models, state$best_model, state$evaluation)
    } else if (choice == "2") {
      option_performance(state$evaluation)
    } else if (choice == "3") {
      option_dataset(state$data)
    } else if (choice == "4") {
      option_visualizations(state$data, state$models, state$evaluation)
    } else if (choice == "5") {
      option_export_report(state$evaluation, state$data)
    } else if (choice == "6" || choice == "q" || choice == "quit" || choice == "exit") {
      cat("\nThank you for using Calorie Burn Predictor. Goodbye!\n\n")
      break
    } else {
      cat("❌ Invalid selection '", choice, "'. Please enter a number between 1 and 6.\n\n", sep = "")
    }
  }
}

# ------------------------------------------------------------------------------
# Main Application Initialization
# ------------------------------------------------------------------------------

#' Initialize and run the standalone application
#' @param data_path Path to calories.csv
#' @param force_retrain Logical, whether to ignore model cache
#' @return State list containing dataset, models, and evaluation artifacts
init_standalone <- function(data_path = "calories.csv", force_retrain = FALSE) {
  cat("\nInitializing Calorie Burn Predictor Environment...\n")
  
  # Verify dataset exists
  if (!file.exists(data_path)) {
    stop("Primary dataset not found at '", data_path, "'. Please check the file path.")
  }
  
  raw_df <- load_calorie_data(data_path)
  cleaned_df <- clean_calorie_data(raw_df)
  engineered_df <- engineer_features(cleaned_df)
  
  split_res <- split_train_test(engineered_df, train_prop = 0.8, seed = 123)
  
  model_rds <- "models/trained_models.rds"
  eval_rds  <- "models/evaluation_summary.rds"
  
  # Load or train models
  model_bundle <- if (!force_retrain) load_models(model_rds) else NULL
  if (is.null(model_bundle)) {
    cat("Training models on training partition (12,000 observations)...\n")
    model_bundle <- train_all_models(split_res$train, ntree = 80, seed = 123)
    save_models(model_bundle, output_dir = "models")
  }
  
  # Load or compute evaluation
  eval_summary <- if (!force_retrain && file.exists(eval_rds)) readRDS(eval_rds) else NULL
  if (is.null(eval_summary)) {
    cat("Computing 5-fold cross-validation and evaluating test partition...\n")
    cv_res <- cross_validate_models(split_res$train, k = 5, ntree = 80, seed = 123)
    test_eval <- evaluate_all_models(model_bundle$models, split_res$test, target_col = "Calories")
    res_diag <- analyze_residuals(test_eval$residuals)
    target_inv <- investigate_target_relationship(engineered_df)
    
    eval_summary <- list(
      cv_results = cv_res,
      test_eval = test_eval,
      residual_diagnostics = res_diag,
      target_investigation = target_inv,
      timestamp = Sys.time()
    )
    saveRDS(eval_summary, file = eval_rds)
  }
  
  best_model <- select_best_model(eval_summary$cv_results$summary, criterion = "RMSE")
  
  cat("Initialization complete. Optimal model: ", best_model, "\n\n")
  
  list(
    data = engineered_df,
    split = split_res,
    models = model_bundle,
    evaluation = eval_summary,
    best_model = best_model
  )
}

# ------------------------------------------------------------------------------
# Auto-Execution Entry Point
# ------------------------------------------------------------------------------

# Initialize state
app_state <- init_standalone("calories.csv")

# If interactive, launch console menu
if (interactive()) {
  run_menu(app_state)
}
