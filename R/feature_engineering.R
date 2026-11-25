# ==============================================================================
# R/feature_engineering.R - Feature Engineering and Derivation
# ==============================================================================

#' Predictor variables used in modeling
MODEL_FEATURES <- c(
  "Gender", "Age", "Height", "Weight", "Duration",
  "Heart_Rate", "Body_Temp", "BMI"
)

#' Calculate Body Mass Index (BMI)
#' Formula: BMI = Weight (kg) / (Height (m))^2 = Weight / (Height / 100)^2
#' @param weight Numeric vector of weight in kilograms
#' @param height Numeric vector of height in centimeters
#' @return Numeric vector of BMI values rounded to 2 decimal places
calculate_bmi <- function(weight, height) {
  if (any(is.na(weight)) || any(is.na(height))) {
    warning("NA values detected in height or weight inputs.")
  }
  
  # Protect against non-positive heights
  invalid_idx <- which(height <= 0)
  if (length(invalid_idx) > 0) {
    warning("Height must be positive. Non-positive heights will result in NA.")
    height[invalid_idx] <- NA_real_
  }
  
  height_m <- height / 100
  bmi <- weight / (height_m^2)
  return(round(bmi, 2))
}

#' Classify BMI into WHO Standard Health Categories
#' @param bmi Numeric vector of BMI values
#' @return Factor vector of BMI classifications
classify_bmi <- function(bmi) {
  categories <- cut(
    bmi,
    breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
    labels = c("Underweight", "Normal weight", "Overweight", "Obese"),
    right = TRUE
  )
  return(categories)
}

#' Apply full feature engineering pipeline to a dataframe
#' @param df data.frame containing Height and Weight
#' @param include_category Logical, whether to include categorical BMI column
#' @return data.frame with engineered feature columns added
engineer_features <- function(df, include_category = FALSE) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Input dataframe is empty or NULL.")
  }
  
  data <- df
  
  # Ensure Height and Weight exist
  if (!("Height" %in% colnames(data)) || !("Weight" %in% colnames(data))) {
    stop("Dataframe must contain 'Height' and 'Weight' columns for BMI calculation.")
  }
  
  # Calculate BMI
  data$BMI <- calculate_bmi(data$Weight, data$Height)
  
  if (include_category) {
    data$BMI_Category <- classify_bmi(data$BMI)
  }
  
  return(data)
}

#' Extract clean feature matrix and target vector for modeling
#' @param df data.frame with engineered features
#' @param target_col Name of the target variable (default: 'Calories')
#' @return List with 'x' (features data.frame) and 'y' (target vector, if present)
prepare_model_data <- function(df, target_col = "Calories") {
  available_features <- intersect(MODEL_FEATURES, colnames(df))
  
  # Check if all required features are present
  missing_features <- setdiff(MODEL_FEATURES, available_features)
  if (length(missing_features) > 0) {
    stop("Missing required model features: ", paste(missing_features, collapse = ", "))
  }
  
  x <- df[, MODEL_FEATURES, drop = FALSE]
  
  y <- NULL
  if (!is.null(target_col) && target_col %in% colnames(df)) {
    y <- as.numeric(df[[target_col]])
  }
  
  list(x = x, y = y, formula = as.formula(paste(target_col, "~", paste(MODEL_FEATURES, collapse = " + "))))
}

