# ==============================================================================
# R/prediction.R - Unified Calorie Prediction Engine
# ==============================================================================

# Ensure randomForest is attached if installed so S3 predict method is registered
if (requireNamespace("randomForest", quietly = TRUE)) {
  suppressPackageStartupMessages(require(randomForest, quietly = TRUE))
}

# Standard factor levels for Gender to guarantee alignment with training data
GENDER_LEVELS <- c("female", "male")

# Required model features
EXPECTED_FEATURES <- c("Gender", "Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp", "BMI")

#' Predict calories burned for new input data using a specific model
#' @param model Trained model object (lm, randomForest, or rpart)
#' @param new_data data.frame of user input features
#' @return Numeric predicted calorie value
predict_calories_single <- function(model, new_data) {
  if (is.null(model)) stop("Model object is NULL.")
  if (is.null(new_data) || nrow(new_data) == 0) stop("Input data is empty or NULL.")
  
  # Ensure Gender is properly factored
  clean_df <- prepare_prediction_input(new_data)
  
  pred <- tryCatch({
    as.numeric(predict(model, newdata = clean_df))
  }, error = function(e) {
    stop("Prediction failed: ", e$message)
  })
  
  # Calorie burn cannot physically be negative
  pred <- pmax(0, pred)
  return(round(pred, 2))
}

#' Ensure input dataframe has exact column names, types, and factor levels as training data
#' @param df data.frame with raw or pre-processed inputs
#' @return Formatted data.frame ready for model prediction
prepare_prediction_input <- function(df) {
  clean_df <- df
  
  # Ensure Gender is factor with standard levels
  if ("Gender" %in% colnames(clean_df)) {
    g_str <- trimws(tolower(as.character(clean_df$Gender)))
    clean_df$Gender <- factor(g_str, levels = GENDER_LEVELS)
  }
  
  # Ensure BMI is calculated if missing
  if (!("BMI" %in% colnames(clean_df)) && ("Height" %in% colnames(clean_df)) && ("Weight" %in% colnames(clean_df))) {
    clean_df$BMI <- round(clean_df$Weight / ((clean_df$Height / 100)^2), 2)
  }
  
  # Check for all required features
  missing_f <- setdiff(EXPECTED_FEATURES, colnames(clean_df))
  if (length(missing_f) > 0) {
    stop("Input is missing required model features: ", paste(missing_f, collapse = ", "))
  }
  
  # Ensure numeric types
  num_cols <- c("Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp", "BMI")
  for (col in num_cols) {
    clean_df[[col]] <- as.numeric(clean_df[[col]])
  }
  
  return(clean_df)
}

#' Compute statistical prediction interval using Linear Regression
#' Method: Classical ordinary least squares prediction interval
#' Assumptions: Normally distributed residual errors, homoscedasticity, independent observations
#' Limitations: Assumes linear response surface; may be wider than non-linear ensemble error bands
#'
#' @param lm_model Trained lm model
#' @param new_data data.frame of input features
#' @param level Confidence level (default: 0.95)
#' @return Named list with fit, lwr, upr, and documentation
compute_lm_prediction_interval <- function(lm_model, new_data, level = 0.95) {
  clean_df <- prepare_prediction_input(new_data)
  
  pi <- predict(lm_model, newdata = clean_df, interval = "prediction", level = level)
  
  list(
    fit = round(max(0, pi[1, "fit"]), 2),
    lwr = round(max(0, pi[1, "lwr"]), 2),
    upr = round(max(0, pi[1, "upr"]), 2),
    level = level,
    method = "OLS 95% Prediction Interval",
    assumptions = "Gaussian error distribution with constant variance across input space."
  )
}

#' Generate comprehensive predictions from all available models in the bundle
#' @param models Named list of trained models or model bundle containing $models
#' @param new_data data.frame of validated user inputs
#' @param best_model_name Optional name of the selected best model
#' @return List with individual predictions, best prediction, burn rate, and summary
predict_all_models <- function(models, new_data, best_model_name = NULL) {
  if (is.null(models) || length(models) == 0) {
    stop("No models available in model bundle.")
  }
  
  # Automatically unwrap if a model bundle containing $models was passed
  if (is.list(models) && !is.null(models$models) && is.list(models$models)) {
    models <- models$models
  }
  
  clean_df <- prepare_prediction_input(new_data)
  
  preds <- list()
  for (name in names(models)) {
    preds[[name]] <- predict_calories_single(models[[name]], clean_df)
  }
  
  if (is.null(best_model_name) || !(best_model_name %in% names(models))) {
    best_model_name <- if ("Random Forest" %in% names(models)) "Random Forest" else names(models)[1]
  }
  
  best_prediction <- preds[[best_model_name]]
  
  # Calculate exercise burn rate: kcal / minute
  duration <- clean_df$Duration[1]
  burn_rate <- if (!is.na(duration) && duration > 0) {
    round(best_prediction / duration, 2)
  } else {
    NA_real_
  }
  
  # Prediction interval from linear model if available
  pi_info <- NULL
  if ("Linear Regression" %in% names(models) && inherits(models[["Linear Regression"]], "lm")) {
    pi_info <- tryCatch({
      compute_lm_prediction_interval(models[["Linear Regression"]], clean_df, level = 0.95)
    }, error = function(e) NULL)
  }
  
  # Human-readable summary
  summary_text <- sprintf(
    "Predicted Energy Expenditure: %.1f kcal (%s). Workout Burn Rate: %.2f kcal/min over %d minutes. BMI: %.2f kg/m².",
    best_prediction, best_model_name, burn_rate, as.integer(duration), clean_df$BMI[1]
  )
  
  disclaimer <- paste(
    "DISCLAIMER: Predicted calorie burn values are statistical machine-learning estimates",
    "derived from empirical observational data. They are not intended for medical diagnosis,",
    "clinical prescriptions, or metabolic evaluation."
  )
  
  list(
    predictions = preds,
    best_model = best_model_name,
    best_prediction = best_prediction,
    burn_rate_kcal_per_min = burn_rate,
    bmi = clean_df$BMI[1],
    prediction_interval_lm = pi_info,
    summary = summary_text,
    disclaimer = disclaimer,
    input_data = clean_df
  )
}
