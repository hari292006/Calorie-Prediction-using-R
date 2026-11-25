# ==============================================================================
# modules/mod_reports.R - Downloadable Reports Shiny Module
# ==============================================================================

mod_reports_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10, offset = 1,
        shiny::div(
          style = "background: white; padding: 28px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 15px; margin-bottom: 25px;",
          shiny::h3("Export Reports & Prediction Summaries", style = "color: #264653; font-weight: bold; margin-top: 0;"),
          shiny::p("Generate and export presentation-ready diagnostic reports, full machine-learning evaluation summaries, or raw tabular prediction records.",
                  style = "font-size: 14px; color: #555; margin-bottom: 20px;"),
          shiny::uiOutput(ns("report_status"))
        )
      )
    ),
    
    shiny::fluidRow(
      # Card 1: Full PDF Report
      shiny::column(4, offset = 1,
        shiny::div(
          style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center; border-top: 4px solid #2A9D8F; min-height: 280px; display: flex; flex-direction: column; justify-content: space-between;",
          shiny::div(
            shiny::h4("Personalized PDF Report", style = "color: #264653; font-weight: bold; margin-top: 0;"),
            shiny::p("Multi-page PDF featuring your complete workout profile, model predictions across all architectures, validation diagnostics, and exercise guidance.",
                    style = "font-size: 13px; color: #666;")
          ),
          shiny::div(
            style = "margin-top: 15px;",
            shiny::downloadButton(ns("btn_download_pdf"), "📥 Download PDF Report",
                                  class = "btn btn-primary",
                                  style = "background-color: #2A9D8F; border-color: #2A9D8F; width: 100%; font-weight: 600; padding: 10px;")
          )
        )
      ),
      
      # Card 2: Plain Text Evaluation Report
      shiny::column(3,
        shiny::div(
          style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center; border-top: 4px solid #384959; min-height: 280px; display: flex; flex-direction: column; justify-content: space-between;",
          shiny::div(
            shiny::h4("ML Evaluation Text Report", style = "color: #264653; font-weight: bold; margin-top: 0;"),
            shiny::p("Comprehensive 7-section statistical report including 5-fold cross-validation tables, held-out test metrics, residual analysis, and correlation insights.",
                    style = "font-size: 13px; color: #666;")
          ),
          shiny::div(
            style = "margin-top: 15px;",
            shiny::downloadButton(ns("btn_download_txt"), "📄 Download Text Report",
                                  class = "btn btn-default",
                                  style = "background-color: #384959; border-color: #384959; color: white; width: 100%; font-weight: 600; padding: 10px;")
          )
        )
      ),
      
      # Card 3: CSV Prediction Export
      shiny::column(3,
        shiny::div(
          style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center; border-top: 4px solid #E76F51; min-height: 280px; display: flex; flex-direction: column; justify-content: space-between;",
          shiny::div(
            shiny::h4("Prediction Data CSV", style = "color: #264653; font-weight: bold; margin-top: 0;"),
            shiny::p("Download structured CSV containing your input metrics, computed BMI, predicted burn rates across all regression algorithms, and validation metadata.",
                    style = "font-size: 13px; color: #666;")
          ),
          shiny::div(
            style = "margin-top: 15px;",
            shiny::downloadButton(ns("btn_download_csv"), "📊 Export Prediction CSV",
                                  class = "btn btn-default",
                                  style = "background-color: #E76F51; border-color: #E76F51; color: white; width: 100%; font-weight: 600; padding: 10px;")
          )
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(10, offset = 1,
        shiny::div(
          style = "background: #F8F9FA; padding: 14px 20px; border-radius: 6px; border: 1px solid #E5E9EC; font-size: 12px; color: #666;",
          shiny::strong("Academic & Compliance Note: "),
          "All exported reports reflect reproducible R models trained without data leakage. Linear regression prediction intervals follow parametric OLS assumptions and are distinct from Random Forest ensemble variance."
        )
      )
    )
  )
}

mod_reports_server <- function(id, pred_state, eval_summary = NULL, eval_results = NULL, dataset = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Status indicator
    output$report_status <- shiny::renderUI({
      if (is.null(pred_state) || is.null(pred_state$prediction_result)) {
        shiny::div(
          style = "background-color: #EBF3FB; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #2A9D8F; color: #264653;",
          shiny::tags$b("ℹ️ Notice: "),
          "Standard baseline parameters are currently loaded. You can navigate to ",
          shiny::tags$strong("Calorie Prediction"),
          " to run a prediction with custom workout metrics before exporting."
        )
      } else {
        shiny::div(
          style = "background-color: #E8F5E9; padding: 12px 16px; border-radius: 6px; border-left: 4px solid #28A745; color: #1B5E20;",
          shiny::tags$b("✅ Custom Prediction Active: "),
          sprintf("Predicted %s burned using %s. All exports will contain your exact parameters.",
                  paste0(round(pred_state$prediction_result$best_prediction, 1), " kcal"),
                  pred_state$prediction_result$best_model)
        )
      }
    })
    
    # Helper to resolve active user data
    get_export_user_data <- function() {
      if (!is.null(pred_state) && !is.null(pred_state$user_data)) {
        pred_state$user_data
      } else {
        data.frame(
          Gender = factor("male", levels = c("female", "male")),
          Age = 25, Height = 175, Weight = 70, Duration = 30,
          Heart_Rate = 100, Body_Temp = 40.0, BMI = 22.86
        )
      }
    }
    
    # Helper to resolve active prediction result
    get_export_pred_res <- function() {
      if (!is.null(pred_state) && !is.null(pred_state$prediction_result)) {
        pred_state$prediction_result
      } else {
        list(
          best_model = "Random Forest",
          best_prediction = 115.4,
          burn_rate_kcal_per_min = 3.85,
          predictions = list("Linear Regression" = 112.1, "Decision Tree" = 110.5, "Random Forest" = 115.4)
        )
      }
    }
    
    # Handler 1: PDF Download
    output$btn_download_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("Calorie_Burn_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        u_data <- get_export_user_data()
        p_res <- get_export_pred_res()
        
        # Use eval_results or eval_summary$test_eval
        e_res <- if (!is.null(eval_results)) {
          eval_results
        } else if (!is.null(eval_summary) && !is.null(eval_summary$test_eval)) {
          eval_summary$test_eval
        } else {
          list()
        }
        
        generate_pdf_report(
          file = file,
          user_data = u_data,
          prediction_res = p_res,
          eval_res = e_res,
          dataset = dataset
        )
      }
    )
    
    # Handler 2: Text Report Download
    output$btn_download_txt <- shiny::downloadHandler(
      filename = function() {
        paste0("Model_Evaluation_Report_", format(Sys.Date(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        if (!is.null(eval_summary) && !is.null(dataset)) {
          generate_text_evaluation_report(eval_summary = eval_summary, dataset = dataset, output_file = file)
        } else if (file.exists("reports/evaluation_report.txt")) {
          file.copy("reports/evaluation_report.txt", file, overwrite = TRUE)
        } else {
          writeLines("Evaluation report cache not found. Please train models to generate.", file)
        }
      }
    )
    
    # Handler 3: CSV Download
    output$btn_download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("calorie_prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        u_data <- get_export_user_data()
        p_res  <- get_export_pred_res()
        
        rf_pred <- if (!is.null(p_res$predictions[["Random Forest"]])) p_res$predictions[["Random Forest"]] else NA
        lm_pred <- if (!is.null(p_res$predictions[["Linear Regression"]])) p_res$predictions[["Linear Regression"]] else NA
        dt_pred <- if (!is.null(p_res$predictions[["Decision Tree"]])) p_res$predictions[["Decision Tree"]] else NA
        
        bmi_cat <- as.character(classify_bmi(u_data$BMI[1]))
        
        export_df <- data.frame(
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          Gender = as.character(u_data$Gender[1]),
          Age = u_data$Age[1],
          Height_cm = u_data$Height[1],
          Weight_kg = u_data$Weight[1],
          Duration_min = u_data$Duration[1],
          Heart_Rate_bpm = u_data$Heart_Rate[1],
          Body_Temp_C = u_data$Body_Temp[1],
          BMI = round(u_data$BMI[1], 2),
          BMI_Category = bmi_cat,
          Primary_Model = p_res$best_model,
          Predicted_Calories_kcal = round(p_res$best_prediction, 2),
          Est_Burn_Rate_kcal_min = round(p_res$burn_rate_kcal_per_min, 2),
          Pred_Random_Forest_kcal = round(rf_pred, 2),
          Pred_Linear_Regression_kcal = round(lm_pred, 2),
          Pred_Decision_Tree_kcal = round(dt_pred, 2),
          Validation_RMSE_Ref_kcal = 4.479,
          Disclaimer = "Statistical estimate. Not intended for clinical or medical diagnosis.",
          stringsAsFactors = FALSE
        )
        
        write.csv(export_df, file = file, row.names = FALSE)
      }
    )
    
  })
}
