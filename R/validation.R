# ==============================================================================
# R/validation.R - User Input Validation & Defensive Checks
# ==============================================================================

#' Physiological and operational boundaries for inputs
INPUT_BOUNDS <- list(
  Age = list(min = 10, max = 100, label = "Age (years)"),
  Height = list(min = 80, max = 250, label = "Height (cm)"),
  Weight = list(min = 25, max = 250, label = "Weight (kg)"),
  Duration = list(min = 1, max = 300, label = "Exercise Duration (minutes)"),
  Heart_Rate = list(min = 40, max = 220, label = "Heart Rate (bpm)"),
  Body_Temp = list(min = 35.0, max = 43.0, label = "Body Temperature (°C)")
)

#' Validate a single numeric input against boundary criteria
#' @param value Numeric or coercible value
#' @param field_name Character string name of the parameter
#' @return List with 'valid' (logical), 'value' (numeric), and 'error' (character or NULL)
validate_numeric_field <- function(value, field_name) {
  bounds <- INPUT_BOUNDS[[field_name]]
  if (is.null(bounds)) {
    stop("Unknown field name for boundary validation: ", field_name)
  }
  
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(list(valid = FALSE, value = NA_real_, error = paste0(bounds$label, " cannot be empty.")))
  }
  
  num_val <- suppressWarnings(as.numeric(value))
  if (is.na(num_val) || !is.finite(num_val)) {
    return(list(valid = FALSE, value = NA_real_, error = paste0(bounds$label, " must be a valid finite number.")))
  }
  
  if (num_val < bounds$min || num_val > bounds$max) {
    err_msg <- sprintf("%s must be between %s and %s (entered: %s).",
                       bounds$label, bounds$min, bounds$max, num_val)
    return(list(valid = FALSE, value = num_val, error = err_msg))
  }
  
  return(list(valid = TRUE, value = num_val, error = NULL))
}

#' Validate gender input
#' @param gender Character string or factor
#' @return List with 'valid', 'value' ("female" or "male"), and 'error'
validate_gender <- function(gender) {
  if (is.null(gender) || length(gender) == 0 || is.na(gender)) {
    return(list(valid = FALSE, value = NA_character_, error = "Gender must be selected/entered."))
  }
  
  clean_g <- trimws(tolower(as.character(gender)))
  if (clean_g %in% c("f", "female")) {
    return(list(valid = TRUE, value = "female", error = NULL))
  } else if (clean_g %in% c("m", "male")) {
    return(list(valid = TRUE, value = "male", error = NULL))
  } else {
    return(list(valid = FALSE, value = NA_character_,
                error = paste0("Invalid gender: '", gender, "'. Please enter 'female' or 'male'.")))
  }
}

#' Comprehensive validation of all user inputs for prediction
#' @param gender User gender
#' @param age User age
#' @param height User height in cm
#' @param weight User weight in kg
#' @param duration Session duration in minutes
#' @param heart_rate Exercise heart rate in bpm
#' @param body_temp Body temperature in °C
#' @return List containing:
#'   - valid (logical)
#'   - errors (character vector of all detected errors)
#'   - clean_data (data.frame formatted for prediction, NULL if invalid)
#'   - bmi (calculated BMI)
validate_user_inputs <- function(gender, age, height, weight, duration, heart_rate, body_temp) {
  errors <- character()
  
  # Validate Gender
  g_res <- validate_gender(gender)
  if (!g_res$valid) errors <- c(errors, g_res$error)
  
  # Validate Numeric Fields
  a_res <- validate_numeric_field(age, "Age")
  if (!a_res$valid) errors <- c(errors, a_res$error)
  
  h_res <- validate_numeric_field(height, "Height")
  if (!h_res$valid) errors <- c(errors, h_res$error)
  
  w_res <- validate_numeric_field(weight, "Weight")
  if (!w_res$valid) errors <- c(errors, w_res$error)
  
  d_res <- validate_numeric_field(duration, "Duration")
  if (!d_res$valid) errors <- c(errors, d_res$error)
  
  hr_res <- validate_numeric_field(heart_rate, "Heart_Rate")
  if (!hr_res$valid) errors <- c(errors, hr_res$error)
  
  bt_res <- validate_numeric_field(body_temp, "Body_Temp")
  if (!bt_res$valid) errors <- c(errors, bt_res$error)
  
  is_valid <- (length(errors) == 0)
  
  clean_df <- NULL
  bmi_val <- NULL
  
  if (is_valid) {
    bmi_val <- round(w_res$value / ((h_res$value / 100)^2), 2)
    clean_df <- data.frame(
      Gender = factor(g_res$value, levels = c("female", "male")),
      Age = a_res$value,
      Height = h_res$value,
      Weight = w_res$value,
      Duration = d_res$value,
      Heart_Rate = hr_res$value,
      Body_Temp = bt_res$value,
      BMI = bmi_val,
      stringsAsFactors = FALSE
    )
  }
  
  list(
    valid = is_valid,
    errors = errors,
    clean_data = clean_df,
    bmi = bmi_val
  )
}

