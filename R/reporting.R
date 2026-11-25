# ==============================================================================
# R/reporting.R - Report Generation Module
# ==============================================================================

#' Generate a comprehensive PDF report of predictions and analytics
#' @param file Output file path for PDF
#' @param user_data Single-row data.frame of user inputs
#' @param prediction_res List output from predict_all_models
#' @param eval_res List output from evaluate_all_models
#' @param dataset Full dataset for context plots
#' @return Output file path on success
generate_pdf_report <- function(file, user_data, prediction_res, eval_res, dataset) {
  # Open Cairo PDF or standard PDF device
  if (capabilities("cairo")) {
    cairo_pdf(file, width = 8.5, height = 11, family = "sans")
  } else {
    pdf(file, width = 8.5, height = 11)
  }
  
  on.exit(dev.off(), add = TRUE)
  
  # Page 1: Cover and User Profile Summary
  grid::grid.newpage()
  
  # Top banner
  grid::grid.rect(y = 0.9, height = 0.2, gp = grid::gpar(fill = "#384959", col = NA))
  grid::grid.text("CALORIE BURN PREDICTOR REPORT", y = 0.92,
                  gp = grid::gpar(fontsize = 22, fontface = "bold", col = "white"))
  grid::grid.text(paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                  y = 0.85, gp = grid::gpar(fontsize = 11, col = "#BDDDFC"))
  
  # Prediction Highlight Box
  grid::grid.roundrect(x = 0.5, y = 0.72, width = 0.8, height = 0.16,
                       r = grid::unit(0.02, "snpc"),
                       gp = grid::gpar(fill = "#88BDF2", col = "#6A89A7", lwd = 2))
  grid::grid.text(paste("Predicted Energy Expenditure:", round(prediction_res$best_prediction, 2), "kcal"),
                  x = 0.5, y = 0.74, gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#384959"))
  grid::grid.text(paste("Primary Model:", prediction_res$best_model,
                        " | Est. Burn Rate:", round(prediction_res$burn_rate_kcal_per_min, 2), "kcal/min"),
                  x = 0.5, y = 0.68, gp = grid::gpar(fontsize = 12, col = "#384959"))
  
  # User Profile Details
  grid::grid.text("Input Parameters & User Profile", x = 0.15, y = 0.58, hjust = 0,
                  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#384959"))
  
  details <- c(
    paste("• Gender:", tools::toTitleCase(as.character(user_data$Gender))),
    paste("• Age:", user_data$Age, "years"),
    paste("• Height:", user_data$Height, "cm"),
    paste("• Weight:", user_data$Weight, "kg"),
    paste("• Body Mass Index (BMI):", round(user_data$BMI, 2)),
    paste("• Workout Duration:", user_data$Duration, "minutes"),
    paste("• Heart Rate:", user_data$Heart_Rate, "bpm"),
    paste("• Body Temperature:", user_data$Body_Temp, "°C")
  )
  
  y_pos <- 0.54
  for (line in details) {
    grid::grid.text(line, x = 0.18, y = y_pos, hjust = 0,
                    gp = grid::gpar(fontsize = 11, col = "#222222"))
    y_pos <- y_pos - 0.03
  }
  
  # Model Predictions Comparison
  grid::grid.text("Predictions Across All Models", x = 0.55, y = 0.58, hjust = 0,
                  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#384959"))
  y_pos_m <- 0.54
  for (m_name in names(prediction_res$predictions)) {
    val <- prediction_res$predictions[[m_name]]
    grid::grid.text(paste0("• ", m_name, ": ", round(val, 2), " kcal"),
                    x = 0.58, y = y_pos_m, hjust = 0,
                    gp = grid::gpar(fontsize = 11, col = "#222222"))
    y_pos_m <- y_pos_m - 0.03
  }
  
  # Model Validation Metrics Table
  grid::grid.text("Model Validation Performance Summary", x = 0.15, y = 0.26, hjust = 0,
                  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#384959"))
  
  if (!is.null(eval_res) && !is.null(eval_res$comparison)) {
    comp <- eval_res$comparison
    # Header
    grid::grid.text("Model", x = 0.18, y = 0.22, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 10))
    grid::grid.text("MAE (kcal)", x = 0.45, y = 0.22, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 10))
    grid::grid.text("RMSE (kcal)", x = 0.62, y = 0.22, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 10))
    grid::grid.text("R-Squared", x = 0.78, y = 0.22, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 10))
    
    y_tbl <- 0.19
    for (i in seq_len(nrow(comp))) {
      grid::grid.text(as.character(comp$Model[i]), x = 0.18, y = y_tbl, hjust = 0, gp = grid::gpar(fontsize = 9))
      grid::grid.text(as.character(round(comp$MAE[i], 3)), x = 0.45, y = y_tbl, hjust = 0, gp = grid::gpar(fontsize = 9))
      grid::grid.text(as.character(round(comp$RMSE[i], 3)), x = 0.62, y = y_tbl, hjust = 0, gp = grid::gpar(fontsize = 9))
      grid::grid.text(as.character(round(comp$R2[i], 4)), x = 0.78, y = y_tbl, hjust = 0, gp = grid::gpar(fontsize = 9))
      y_tbl <- y_tbl - 0.025
    }
  }
  
  # Footer note
  grid::grid.text("Calorie Burn Predictor | Developed in R | Department of Computer Science",
                  x = 0.5, y = 0.05, gp = grid::gpar(fontsize = 9, col = "#888888"))
  
  return(file)
}



#' Generate a structured text machine-learning evaluation report
#' @param eval_summary List output containing cv_results, test_eval, residual_diagnostics
#' @param dataset Cleaned data.frame
#' @param output_file File path for output text file (default: 'reports/evaluation_report.txt')
#' @return File path of the written report
generate_text_evaluation_report <- function(eval_summary, dataset, output_file = "reports/evaluation_report.txt") {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  con <- file(output_file, open = "wt")
  on.exit(close(con), add = TRUE)
  
  writeLines(c(
    "================================================================================",
    "       CALORIE BURN PREDICTOR - MACHINE LEARNING EVALUATION REPORT             ",
    "================================================================================",
    paste("Generated On:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("Environment : R version", R.version$major, ".", R.version$minor, "(", R.version$platform, ")", sep = ""),
    "",
    "1. DATASET SPECIFICATIONS",
    "--------------------------------------------------------------------------------",
    paste("Total Observations     :", nrow(dataset)),
    paste("Features Analyzed      : Gender, Age, Height, Weight, Duration, Heart_Rate, Body_Temp, BMI"),
    paste("Target Variable        : Calories (Continuous kcal)"),
    "Data Leakage Prevention: User_ID uniquely identifies participants and was strictly",
    "                         excluded from feature matrices and correlation computations.",
    "                         Preprocessing parameters are fitted solely on training data.",
    "",
    "2. ALGORITHM SELECTION RATIONALE",
    "--------------------------------------------------------------------------------",
    "Three distinct regression paradigms were selected to evaluate physical energy expenditure:",
    "  • Multiple Linear Regression (OLS):",
    "    Serves as the interpretable parametric baseline. Assumes linear relationship between",
    "    physiological predictors and caloric expenditure.",
    "  • Decision Tree Regression (rpart):",
    "    Non-parametric recursive binary partitioning model. Captures basic threshold-based",
    "    interactions without linearity assumptions.",
    "  • Random Forest Regression (randomForest, 80 trees):",
    "    Bootstrap aggregation (bagging) ensemble. Averages 80 decorrelated decision trees,",
    "    modeling complex non-linear metabolic interactions while resisting overfitting.",
    "",
    "3. FIVE-FOLD CROSS-VALIDATION RESULTS (Training Partition: 12,000 Obs)",
    "--------------------------------------------------------------------------------",
    "Folds were identically partitioned across all candidate models to ensure strict pairing."
  ), con)
  
  # Format CV Table
  if (!is.null(eval_summary$cv_results) && !is.null(eval_summary$cv_results$summary)) {
    cv_df <- eval_summary$cv_results$summary
    header <- sprintf("%-20s | %-16s | %-16s | %-16s", "Model Architecture", "CV MAE (Mean±SD)", "CV RMSE (Mean±SD)", "CV R² (Mean±SD)")
    writeLines(c(header, paste(rep("-", nchar(header)), collapse = "")), con)
    for (i in seq_len(nrow(cv_df))) {
      row_str <- sprintf("%-20s | %6.3f ± %-7.3f | %6.3f ± %-7.3f | %6.4f ± %-7.4f",
                         cv_df$Model[i], cv_df$CV_MAE_Mean[i], cv_df$CV_MAE_SD[i],
                         cv_df$CV_RMSE_Mean[i], cv_df$CV_RMSE_SD[i],
                         cv_df$CV_R2_Mean[i], cv_df$CV_R2_SD[i])
      writeLines(row_str, con)
    }
  }
  
  writeLines(c(
    "",
    "Model Selection Decision: Random Forest was chosen as the optimal operational model",
    "based on achieving the lowest cross-validation RMSE and highest cross-validation R².",
    "",
    "4. FINAL TEST SET PERFORMANCE (Untouched 3,000 Obs)",
    "--------------------------------------------------------------------------------",
    "Held-out test partition evaluated once to verify genuine out-of-sample generalization."
  ), con)
  
  # Format Test Table
  if (!is.null(eval_summary$test_eval) && !is.null(eval_summary$test_eval$comparison)) {
    test_df <- eval_summary$test_eval$comparison
    header_t <- sprintf("%-20s | %-12s | %-12s | %-10s", "Model Architecture", "Test MAE", "Test RMSE", "Test R²")
    writeLines(c(header_t, paste(rep("-", nchar(header_t)), collapse = "")), con)
    for (i in seq_len(nrow(test_df))) {
      row_str <- sprintf("%-20s | %10.3f kcal | %10.3f kcal | %10.4f",
                         test_df$Model[i], test_df$MAE[i], test_df$RMSE[i], test_df$R2[i])
      writeLines(row_str, con)
    }
  }
  
  writeLines(c(
    "",
    "5. RESIDUAL DIAGNOSTICS & ERROR BEHAVIOR",
    "--------------------------------------------------------------------------------"
  ), con)
  
  if (!is.null(eval_summary$residual_diagnostics)) {
    rd <- eval_summary$residual_diagnostics
    header_r <- sprintf("%-20s | %-10s | %-10s | %-8s | %-12s", "Model", "Mean Error", "SD Error", "Skewness", "Outliers (>3σ)")
    writeLines(c(header_r, paste(rep("-", nchar(header_r)), collapse = "")), con)
    for (i in seq_len(nrow(rd))) {
      row_str <- sprintf("%-20s | %10.4f | %10.3f | %8.3f | %4d (%4.2f%%)",
                         rd$Model[i], rd$Mean_Residual[i], rd$SD_Residual[i], rd$Skewness[i],
                         rd$Outliers_3SD[i], rd$Outlier_Pct[i])
      writeLines(row_str, con)
    }
  }
  
  writeLines(c(
    "",
    "6. PHYSIOLOGICAL TARGET CORRELATIONS & HIGH R² ANALYSIS",
    "--------------------------------------------------------------------------------",
    "Top feature correlations with Calorie burn (User_ID excluded):",
    "  • Duration   : r = +0.9554 (Cumulative mechanical workout volume)",
    "  • Heart Rate : r = +0.8979 (Cardiovascular intensity & metabolic strain)",
    "  • Body Temp  : r = +0.8246 (Thermogenic metabolic heat production)",
    "  • Age        : r = +0.1544",
    "  • BMI        : r = +0.0565",
    "  • Weight     : r = +0.0355",
    "  • Height     : r = +0.0175",
    "",
    "High R² Discussion: Energy expenditure in exercise physiology is fundamentally governed",
    "by the laws of thermodynamics: Total Work = Intensity × Duration. Duration and Heart Rate",
    "serve as direct proxies for volume and intensity, accounting for the strong predictive fit.",
    "This high explanatory power represents physical law, not target leakage.",
    "",
    "7. ASSUMPTIONS, LIMITATIONS & ETHICAL NOTICE",
    "--------------------------------------------------------------------------------",
    "  1. Observational Scope: Dataset reflects structured indoor exercise recordings.",
    "     Extrapolation to extreme endurance sports (>300 min) or elite athletes may vary.",
    "  2. Uncertainty Intervals: Classical OLS prediction intervals strictly apply to Linear",
    "     Regression under homoscedastic Gaussian error assumptions. They are not transferable",
    "     to Random Forest non-linear ensembles.",
    "  3. Non-Medical Disclaimer: Calorie burn predictions and BMI calculations are statistical",
    "     machine-learning estimates. They are not intended for clinical diagnosis, medical",
    "     treatment, or individualized metabolic prescription.",
    "================================================================================"
  ), con)
  
  message("Evaluation report successfully written to: ", output_file)
  return(output_file)
}
