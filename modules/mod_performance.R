# ==============================================================================
# modules/mod_performance.R - Model Performance & Diagnostics Shiny Module
# ==============================================================================

mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6,
        shiny::div(
          style = "background: white; padding: 22px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("5-Fold Cross-Validation Performance (Training Set)", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::p("Models evaluated across 5 non-overlapping folds to ensure generalization and guard against overfitting.", style = "color: #666; font-size: 13px;"),
          shiny::tableOutput(ns("tbl_cv_metrics"))
        )
      ),
      shiny::column(6,
        shiny::div(
          style = "background: white; padding: 22px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Final Test-Set Evaluation (Untouched 3,000 Obs)", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::p("Independent evaluation on held-out test data for unbiased final model verification.", style = "color: #666; font-size: 13px;"),
          shiny::tableOutput(ns("tbl_test_metrics"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::div(
          style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Model RMSE Comparison", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::plotOutput(ns("plot_model_comp"), height = "360px")
        )
      ),
      shiny::column(6,
        shiny::div(
          style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Residual Error Distribution", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::plotOutput(ns("plot_residuals"), height = "360px")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::div(
          style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
          shiny::h4("Observed vs. Predicted Calorie Expenditure", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::plotOutput(ns("plot_actual_pred"), height = "420px")
        )
      )
    )
  )
}

mod_performance_server <- function(id, eval_summary) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Render CV Metrics Table
    output$tbl_cv_metrics <- shiny::renderTable({
      if (is.null(eval_summary) || is.null(eval_summary$cv_results) || is.null(eval_summary$cv_results$summary)) {
        return(NULL)
      }
      cv_df <- eval_summary$cv_results$summary
      out <- data.frame(
        Model = cv_df$Model,
        `MAE (Mean ± SD)` = sprintf("%.3f ± %.3f", cv_df$CV_MAE_Mean, cv_df$CV_MAE_SD),
        `RMSE (Mean ± SD)` = sprintf("%.3f ± %.3f", cv_df$CV_RMSE_Mean, cv_df$CV_RMSE_SD),
        `R² (Mean ± SD)` = sprintf("%.4f ± %.4f", cv_df$CV_R2_Mean, cv_df$CV_R2_SD),
        check.names = FALSE
      )
      out
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    # Render Test Metrics Table
    output$tbl_test_metrics <- shiny::renderTable({
      if (is.null(eval_summary) || is.null(eval_summary$test_eval) || is.null(eval_summary$test_eval$comparison)) {
        return(NULL)
      }
      test_df <- eval_summary$test_eval$comparison
      out <- data.frame(
        Model = test_df$Model,
        `MAE (kcal)` = sprintf("%.3f", test_df$MAE),
        `RMSE (kcal)` = sprintf("%.3f", test_df$RMSE),
        `R²` = sprintf("%.4f", test_df$R2),
        check.names = FALSE
      )
      out
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    output$plot_model_comp <- shiny::renderPlot({
      if (is.null(eval_summary) || is.null(eval_summary$test_eval)) return(NULL)
      plot_model_comparison(eval_summary$test_eval$comparison, metric = "RMSE")
    })
    
    output$plot_residuals <- shiny::renderPlot({
      if (is.null(eval_summary) || is.null(eval_summary$test_eval)) return(NULL)
      plot_residual_distribution(eval_summary$test_eval$residuals)
    })
    
    output$plot_actual_pred <- shiny::renderPlot({
      if (is.null(eval_summary) || is.null(eval_summary$test_eval)) return(NULL)
      plot_actual_vs_predicted(eval_summary$test_eval$predictions)
    })
  })
}
