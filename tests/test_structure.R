# ==============================================================================
# tests/test_structure.R - Verification of Project Structure and Module Loading
# ==============================================================================

cat("\n======================================================\n")
cat("      STAGE 2 PROJECT STRUCTURE & MODULE TEST        \n")
cat("======================================================\n\n")

source("requirements.R")
load_environment()

modules_to_test <- c(
  "R/data.R",
  "R/preprocessing.R",
  "R/feature_engineering.R",
  "R/validation.R",
  "R/models.R",
  "R/evaluation.R",
  "R/prediction.R",
  "R/visualizations.R",
  "R/reporting.R"
)

all_ok <- TRUE

for (mod in modules_to_test) {
  cat("Testing module sourcing: ", mod, " ... ")
  res <- tryCatch({
    source(mod)
    "PASS"
  }, error = function(e) {
    all_ok <<- FALSE
    paste("FAIL -", e$message)
  })
  cat(res, "\n")
}

cat("\n------------------------------------------------------\n")
cat("Testing Legacy Preservation:\n")
legacy_files <- c("legacy/calorie_prediction.R", "legacy/app.R", "legacy/README.md")
for (lf in legacy_files) {
  exists <- file.exists(lf)
  cat(lf, ":", ifelse(exists, "PRESERVED", "MISSING"), "\n")
  if (!exists) all_ok <- FALSE
}

cat("\n------------------------------------------------------\n")
cat("Testing Functional Workflow using Dummy Sample:\n")

# 1. Validation test
v_pass <- validate_user_inputs("male", 25, 175, 70, 30, 100, 39.5)
cat("Validation on valid inputs: ", ifelse(v_pass$valid, "PASS", "FAIL"), "\n")

v_fail <- validate_user_inputs("alien", -5, 0, 70, 30, 100, 39.5)
cat("Validation on invalid inputs: ", ifelse(!v_fail$valid && length(v_fail$errors) >= 3, "PASS", "FAIL"), "\n")

# 2. BMI Calculation test
bmi_test <- calculate_bmi(70, 175)
cat("BMI calculation (70kg, 175cm = 22.86): ", ifelse(abs(bmi_test - 22.86) < 0.01, "PASS", "FAIL"), "\n")

# 3. Model evaluation metric test
actuals <- c(100, 150, 200)
preds <- c(105, 145, 210)
mae_val <- calculate_mae(actuals, preds)
cat("MAE metric test (expected 6.67): ", ifelse(abs(mae_val - 6.667) < 0.01, "PASS", "FAIL"), "\n")

cat("\n------------------------------------------------------\n")
cat("Overall Stage 2 Architecture Status: ", ifelse(all_ok, "SUCCESSFUL", "FAILED"), "\n")
cat("======================================================\n\n")

