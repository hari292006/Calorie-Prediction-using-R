# ==============================================================================
# app.R - Interactive Shiny Calorie Burn Predictor Dashboard
# ==============================================================================

# 1. Environment & Package Loading
source("requirements.R")
load_environment()

# Check for Shiny framework
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Shiny package is required to run the web application. Please run: Rscript -e 'source(\"requirements.R\"); install_missing_packages()'")
}

library(shiny)

# 2. Source Shared Core ML & Visualization Pipeline
source("R/data.R")
source("R/preprocessing.R")
source("R/feature_engineering.R")
source("R/validation.R")
source("R/models.R")
source("R/evaluation.R")
source("R/prediction.R")
source("R/visualizations.R")
source("R/reporting.R")

# 3. Source Shiny Modules
source("modules/mod_dashboard.R")
source("modules/mod_prediction.R")
source("modules/mod_performance.R")
source("modules/mod_insights.R")
source("modules/mod_reports.R")

# 4. Global Data & Model Initialization (Cached for Instant App Loading)
DATA_PATH <- "calories.csv"
MODEL_RDS <- "models/trained_models.rds"
EVAL_RDS  <- "models/evaluation_summary.rds"

# Ingest and clean dataset
raw_df <- load_calorie_data(DATA_PATH)
cleaned_df <- clean_calorie_data(raw_df)
app_data <- engineer_features(cleaned_df)

# Train/test partition
split_obj <- split_train_test(app_data, train_prop = 0.8, seed = 123)

# Load cached model bundle or train once if not found
app_models <- load_models(MODEL_RDS)
if (is.null(app_models)) {
  message("No cached models found. Training initial models...")
  app_models <- train_all_models(split_obj$train, ntree = 80, seed = 123)
  save_models(app_models, output_dir = "models")
}

# Load cached evaluation summary (5-fold CV + test metrics) or compute once
app_eval_summary <- if (file.exists(EVAL_RDS)) readRDS(EVAL_RDS) else NULL
if (is.null(app_eval_summary)) {
  cv_res <- cross_validate_models(split_obj$train, k = 5, ntree = 80, seed = 123)
  test_eval <- evaluate_all_models(app_models$models, split_obj$test, target_col = "Calories")
  res_diag <- analyze_residuals(test_eval$residuals)
  target_inv <- investigate_target_relationship(app_data)
  app_eval_summary <- list(
    cv_results = cv_res,
    test_eval = test_eval,
    residual_diagnostics = res_diag,
    target_investigation = target_inv,
    timestamp = Sys.time()
  )
  saveRDS(app_eval_summary, file = EVAL_RDS)
}

app_best_model <- select_best_model(app_eval_summary$cv_results$summary, criterion = "RMSE")

# 5. UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Premium Light Fitness Analytics Styling */
      body {
        background-color: #F8FBFB;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        color: #2D3748;
      }
      .navbar {
        background-color: #264653;
        border: none;
        border-radius: 0;
        margin-bottom: 25px;
        box-shadow: 0 2px 8px rgba(38, 70, 83, 0.15);
      }
      .navbar-default .navbar-brand {
        color: #FFFFFF !important;
        font-weight: 700;
        font-size: 19px;
        letter-spacing: 0.5px;
      }
      .navbar-default .navbar-nav > li > a {
        color: #D8F3DC !important;
        font-weight: 500;
        padding-top: 16px;
        padding-bottom: 16px;
        transition: all 0.2s ease;
      }
      .navbar-default .navbar-nav > li > a:hover {
        color: #FFFFFF !important;
        background-color: rgba(42, 157, 143, 0.3) !important;
      }
      .navbar-default .navbar-nav > .active > a, 
      .navbar-default .navbar-nav > .active > a:hover, 
      .navbar-default .navbar-nav > .active > a:focus {
        color: #FFFFFF !important;
        background-color: #2A9D8F !important;
        font-weight: 600;
      }
      .metric-card {
        background: white;
        border: 1px solid #E5E9EC;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.03);
      }
      .footer {
        margin-top: 45px;
        padding: 24px 0;
        border-top: 1px solid #E2E8F0;
        text-align: center;
        color: #718096;
        font-size: 13px;
        background-color: #FFFFFF;
      }
    "))
  ),
  
  navbarPage(
    title = div(icon("fire", lib = "font-awesome"), " Calorie Burn Predictor"),
    id = "main_nav",
    
    # Tab 1: Dashboard
    tabPanel("Dashboard",
      div(class = "container-fluid",
        mod_dashboard_ui("dashboard")
      )
    ),
    
    # Tab 2: Prediction
    tabPanel("Calorie Prediction",
      div(class = "container-fluid",
        mod_prediction_ui("prediction")
      )
    ),
    
    # Tab 3: Model Performance
    tabPanel("Model Performance",
      div(class = "container-fluid",
        mod_performance_ui("performance")
      )
    ),
    
    # Tab 4: Dataset Insights
    tabPanel("Dataset Insights",
      div(class = "container-fluid",
        mod_insights_ui("insights")
      )
    ),
    
    # Tab 5: Download Reports
    tabPanel("Download Reports",
      div(class = "container-fluid",
        mod_reports_ui("reports")
      )
    )
  ),
  
  tags$footer(
    class = "footer",
    div(
      p(style = "font-weight: 600; margin-bottom: 4px; color: #2D3748;",
        "Calorie Burn Predictor | Academic Machine Learning Revamp"),
      p(style = "margin: 0; font-size: 12px; color: #A0AEC0;",
        "Powered purely by R 4.6.1 & Shiny | Random Forest, Multiple Linear Regression & Decision Trees | Strict Zero-Leakage Architecture")
    )
  )
)

# 6. Server Definition
server <- function(input, output, session) {
  # 1. Dashboard Module
  mod_dashboard_server("dashboard", eval_results = app_eval_summary$test_eval, dataset = app_data)
  
  # 2. Prediction Module (Returns reactive state for downstream modules)
  pred_state <- mod_prediction_server("prediction", model_bundle = app_models, eval_summary = app_eval_summary, best_model_name = app_best_model)
  
  # 3. Model Performance Module (Displays 5-fold CV and test evaluations)
  mod_performance_server("performance", eval_summary = app_eval_summary)
  
  # 4. Insights Module
  mod_insights_server("insights", dataset = app_data, model_bundle = app_models, pred_state = pred_state)
  
  # 5. Reports Module
  mod_reports_server("reports", pred_state = pred_state, eval_summary = app_eval_summary, eval_results = app_eval_summary$test_eval, dataset = app_data)
}

# 7. Run Application
shinyApp(ui, server)
