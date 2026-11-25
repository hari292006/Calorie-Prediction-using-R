# ==============================================================================
# R/models.R - Model Training, Persistence and Model Management
# ==============================================================================

#' Train regression models on prepared calorie training data
#' Models trained:
#' 1. Linear Regression (Base R `lm`)
#' 2. Random Forest Regression (`randomForest::randomForest`)
#' 3. Decision Tree Regression (`rpart::rpart`)
#'
#' @param train_data data.frame of training data with engineered features
#' @param formula_str Formula string or formula object (default: Calories ~ .)
#' @param ntree Number of trees for Random Forest (default: 80)
#' @param seed Random seed for reproducibility (default: 123)
#' @return List containing trained model objects, feature names, and metadata
train_all_models <- function(train_data, ntree = 80, seed = 123) {
  if (is.null(train_data) || nrow(train_data) == 0) {
    stop("Training data is empty or NULL.")
  }
  
  # Ensure target column exists
  if (!("Calories" %in% colnames(train_data))) {
    stop("Training data must contain 'Calories' target column.")
  }
  
  # Explicit modeling formula excluding User_ID
  model_formula <- Calories ~ Gender + Age + Height + Weight + Duration + Heart_Rate + Body_Temp + BMI
  
  models <- list()
  metadata <- list(
    trained_at = Sys.time(),
    seed = seed,
    n_train = nrow(train_data),
    features = c("Gender", "Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp", "BMI")
  )
  
  set.seed(seed)
  
  # 1. Linear Regression
  message("Training Linear Regression model...")
  lm_fit <- lm(model_formula, data = train_data)
  models[["Linear Regression"]] <- lm_fit
  
  # 2. Decision Tree (rpart) - Interpretable single tree baseline
  message("Training Decision Tree Regression (rpart)...")
  dt_fit <- rpart::rpart(model_formula, data = train_data, method = "anova",
                         control = rpart::rpart.control(cp = 0.001, maxdepth = 10))
  models[["Decision Tree"]] <- dt_fit
  
  # 3. Random Forest Regression
  if (requireNamespace("randomForest", quietly = TRUE)) {
    message("Training Random Forest model (ntree = ", ntree, ")...")
    rf_fit <- randomForest::randomForest(
      model_formula,
      data = train_data,
      ntree = ntree,
      importance = TRUE
    )
    models[["Random Forest"]] <- rf_fit
  } else {
    warning("randomForest package not available; skipping Random Forest training.")
  }
  
  return(list(
    models = models,
    formula = model_formula,
    metadata = metadata
  ))
}

#' Save trained models bundle to disk
#' @param model_bundle Output from train_all_models
#' @param output_dir Directory path to save model files (default: 'models')
#' @param filename Filename for RDS bundle (default: 'trained_models.rds')
#' @return Logical indicating success
save_models <- function(model_bundle, output_dir = "models", filename = "trained_models.rds") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  filepath <- file.path(output_dir, filename)
  saveRDS(model_bundle, file = filepath)
  message("Models successfully saved to: ", filepath)
  return(file.exists(filepath))
}

#' Load trained models bundle from disk
#' @param file_path Full path to RDS file or directory containing trained_models.rds
#' @return Loaded model_bundle or NULL if file does not exist
load_models <- function(file_path = "models/trained_models.rds") {
  if (dir.exists(file_path)) {
    file_path <- file.path(file_path, "trained_models.rds")
  }
  if (!file.exists(file_path)) {
    return(NULL)
  }
  bundle <- readRDS(file_path)
  return(bundle)
}

