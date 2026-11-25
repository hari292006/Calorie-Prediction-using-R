# ==============================================================================
# R/data.R - Data Loading and Inspection Module
# ==============================================================================

#' Expected columns and their expected types in the raw dataset
EXPECTED_COLUMNS <- c(
  "User_ID", "Gender", "Age", "Height", "Weight",
  "Duration", "Heart_Rate", "Body_Temp", "Calories"
)

#' Load the calorie prediction dataset
#' @param file_path Character string pointing to the CSV file
#' @return data.frame containing the raw dataset
load_calorie_data <- function(file_path = "calories.csv") {
  if (!file.exists(file_path)) {
    stop("Dataset file not found at path: ", file_path)
  }
  
  df <- tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    stop("Failed to read CSV dataset: ", e$message)
  })
  
  # Validate that expected columns exist
  missing_cols <- setdiff(EXPECTED_COLUMNS, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(df) == 0) {
    stop("The loaded dataset has 0 rows.")
  }
  
  return(df)
}

#' Inspect and summarize the dataset structure
#' @param df data.frame loaded from calories.csv
#' @return A list with data summary, dimensions, missing value counts, and distributions
inspect_calorie_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Cannot inspect an empty or null data frame.")
  }
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  char_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  
  # Missing values count per column
  na_counts <- colSums(is.na(df))
  
  # Numerical summaries
  num_summary <- do.call(rbind, lapply(num_cols, function(col) {
    vals <- df[[col]]
    data.frame(
      Variable = col,
      Min = round(min(vals, na.rm = TRUE), 2),
      Q1 = round(quantile(vals, 0.25, na.rm = TRUE), 2),
      Median = round(median(vals, na.rm = TRUE), 2),
      Mean = round(mean(vals, na.rm = TRUE), 2),
      Q3 = round(quantile(vals, 0.75, na.rm = TRUE), 2),
      Max = round(max(vals, na.rm = TRUE), 2),
      SD = round(sd(vals, na.rm = TRUE), 2),
      Missing = sum(is.na(vals)),
      stringsAsFactors = FALSE
    )
  }))
  
  # Categorical summary
  cat_summary <- lapply(char_cols, function(col) {
    table(df[[col]], useNA = "ifany")
  })
  names(cat_summary) <- char_cols
  
  list(
    rows = nrow(df),
    cols = ncol(df),
    column_names = colnames(df),
    missing_counts = na_counts,
    numerical_summary = num_summary,
    categorical_summary = cat_summary
  )
}

