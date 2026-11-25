# ==============================================================================
# R/preprocessing.R - Data Cleaning, Transformation and Partitioning
# ==============================================================================

#' Standard factor levels for Gender
GENDER_LEVELS <- c("female", "male")

#' Clean and preprocess the raw calorie dataset
#' @param df data.frame of raw calorie data
#' @param drop_na Logical, whether to drop rows with missing values
#' @return Preprocessed data.frame with standardized columns and types
clean_calorie_data <- function(df, drop_na = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Input dataframe is empty or NULL.")
  }
  
  data <- df
  
  # Standardize Gender: trim whitespace, lowercase, factor with explicit levels
  if ("Gender" %in% colnames(data)) {
    data$Gender <- trimws(tolower(as.character(data$Gender)))
    # Validate gender entries
    invalid_genders <- setdiff(unique(data$Gender), GENDER_LEVELS)
    if (length(invalid_genders) > 0) {
      warning("Found unexpected Gender values: ", paste(invalid_genders, collapse = ", "))
    }
    data$Gender <- factor(data$Gender, levels = GENDER_LEVELS)
  }
  
  # Ensure numerical columns are numeric
  num_fields <- c("Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp")
  if ("Calories" %in% colnames(data)) {
    num_fields <- c(num_fields, "Calories")
  }
  
  for (col in num_fields) {
    if (col %in% colnames(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  # Drop User_ID if present to prevent data leakage and noise
  if ("User_ID" %in% colnames(data)) {
    data$User_ID <- NULL
  }
  
  # Missing value handling
  if (drop_na) {
    initial_rows <- nrow(data)
    data <- na.omit(data)
    dropped <- initial_rows - nrow(data)
    if (dropped > 0) {
      message("Dropped ", dropped, " rows containing missing (NA) values.")
    }
  }
  
  return(data)
}

#' Split dataset into reproducible train and test partitions
#' @param data data.frame to split
#' @param train_prop Proportion for training set (default: 0.8)
#' @param seed Random seed for reproducibility (default: 123)
#' @return List containing train and test data.frames
split_train_test <- function(data, train_prop = 0.8, seed = 123) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is empty or NULL.")
  }
  if (train_prop <= 0 || train_prop >= 1) {
    stop("train_prop must be strictly between 0 and 1.")
  }
  
  set.seed(seed)
  n <- nrow(data)
  
  # Check if caret createDataPartition is available; fallback to base R sample
  idx <- NULL
  if (requireNamespace("caret", quietly = TRUE) && "Calories" %in% colnames(data)) {
    idx <- caret::createDataPartition(data$Calories, p = train_prop, list = FALSE)
    idx <- as.vector(idx)
  } else {
    train_size <- floor(train_prop * n)
    idx <- sample.int(n, size = train_size, replace = FALSE)
  }
  
  train_data <- data[idx, , drop = FALSE]
  test_data  <- data[-idx, , drop = FALSE]
  
  # Reset row names
  rownames(train_data) <- NULL
  rownames(test_data) <- NULL
  
  list(
    train = train_data,
    test = test_data,
    train_indices = idx,
    n_train = nrow(train_data),
    n_test = nrow(test_data)
  )
}

