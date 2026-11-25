# ==============================================================================
# modules/mod_prediction.R - Calorie Prediction Interface Shiny Module
# ==============================================================================

mod_prediction_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      # Inputs Column (Left)
      shiny::column(4,
        shiny::div(
          style = "background: white; padding: 22px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Workout & Physiological Inputs", style = "color: #384959; font-weight: bold; margin-top: 0; border-bottom: 2px solid #88BDF2; padding-bottom: 8px;"),
          
          shiny::selectInput(ns("gender"), "Gender", choices = c("Male" = "male", "Female" = "female"), selected = "male"),
          shiny::numericInput(ns("age"), "Age (years)", value = 25, min = 10, max = 100, step = 1),
          shiny::numericInput(ns("height"), "Height (cm)", value = 175, min = 80, max = 250, step = 0.5),
          shiny::numericInput(ns("weight"), "Weight (kg)", value = 70, min = 25, max = 250, step = 0.5),
          shiny::numericInput(ns("duration"), "Workout Duration (minutes)", value = 30, min = 1, max = 300, step = 1),
          shiny::numericInput(ns("heart_rate"), "Exercise Heart Rate (bpm)", value = 100, min = 40, max = 220, step = 1),
          shiny::numericInput(ns("body_temp"), "Body Temperature (°C)", value = 40.0, min = 35.0, max = 43.0, step = 0.1),
          
          shiny::selectInput(ns("model_choice"), "Primary Prediction Architecture",
                             choices = c("Random Forest (Optimal CV)" = "Random Forest",
                                         "Linear Regression (Baseline)" = "Linear Regression",
                                         "Decision Tree (rpart)" = "Decision Tree"),
                             selected = "Random Forest"),
          
          shiny::actionButton(ns("btn_predict"), "🔮 Predict Calories Burned",
                              class = "btn btn-primary",
                              style = "width: 100%; font-weight: bold; background-color: #2A9D8F; border-color: #2A9D8F; padding: 12px; margin-top: 10px; font-size: 15px; color: white;"),
          
          shiny::actionButton(ns("btn_reset"), "↺ Reset Inputs to Defaults",
                              class = "btn btn-default",
                              style = "width: 100%; margin-top: 8px; color: #555; background-color: #F8F9FA; border: 1px solid #D1D5DB; font-weight: 500;"),
          
          shiny::uiOutput(ns("validation_errors"))
        )
      ),
      
      # Results Display Column (Right)
      shiny::column(8,
        # Big Prediction Banner
        shiny::div(
          style = "background: linear-gradient(135deg, #6A89A7 0%, #384959 100%); color: white; padding: 28px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
          shiny::h4("Estimated Energy Expenditure", style = "margin-top: 0; font-size: 15px; text-transform: uppercase; letter-spacing: 1.5px; color: #BDDDFC;"),
          shiny::div(
            style = "font-size: 52px; font-weight: 800; margin: 10px 0;",
            shiny::textOutput(ns("predicted_calories"), inline = TRUE)
          ),
          shiny::div(
            style = "font-size: 15px; color: #E0E0E0;",
            shiny::textOutput(ns("model_info_text"))
          )
        ),
        
        # Diagnostic Metric Cards
        shiny::fluidRow(
          shiny::column(4,
            shiny::div(
              style = "background: white; padding: 18px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center;",
              shiny::h5("Body Mass Index", style = "color: #777; margin-top: 0; font-size: 13px;"),
              shiny::h3(shiny::textOutput(ns("bmi_display")), style = "color: #384959; font-weight: bold; margin: 5px 0;"),
              shiny::p(shiny::textOutput(ns("bmi_category_display")), style = "margin: 0; font-weight: 500; font-size: 12px; color: #6A89A7;")
            )
          ),
          shiny::column(4,
            shiny::div(
              style = "background: white; padding: 18px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center;",
              shiny::h5("Burn Rate Intensity", style = "color: #777; margin-top: 0; font-size: 13px;"),
              shiny::h3(shiny::textOutput(ns("burn_rate_display")), style = "color: #384959; font-weight: bold; margin: 5px 0;"),
              shiny::p("Average kcal / minute", style = "margin: 0; color: #777; font-size: 12px;")
            )
          ),
          shiny::column(4,
            shiny::div(
              style = "background: white; padding: 18px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px; text-align: center;",
              shiny::h5("Model Validation RMSE", style = "color: #777; margin-top: 0; font-size: 13px;"),
              shiny::h3(shiny::textOutput(ns("model_rmse_display")), style = "color: #384959; font-weight: bold; margin: 5px 0;"),
              shiny::p("Evaluated on 3,000 test records", style = "margin: 0; color: #777; font-size: 12px;")
            )
          )
        ),
        
        # Multi-Model Comparison Table
        shiny::div(
          style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Prediction Comparison Across Architectures", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::tableOutput(ns("tbl_model_comparison"))
        ),
        
        # Statistical Prediction Interval (OLS) Box
        shiny::uiOutput(ns("prediction_interval_box")),
        
        # Clinical Guidance Box
        shiny::div(
          style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
          shiny::h4("Exercise & Wellness Context", style = "color: #384959; font-weight: bold; margin-top: 0;"),
          shiny::htmlOutput(ns("recommendation_html"))
        ),
        
        # Ethical Non-Medical Disclaimer
        shiny::div(
          style = "background: #F8F9FA; padding: 12px 18px; border-radius: 6px; border: 1px solid #E0E0E0; font-size: 12px; color: #777;",
          shiny::p(style = "margin: 0;",
            shiny::strong("Ethical / Non-Medical Notice: "),
            "Caloric burn estimates and BMI values are generated via statistical machine-learning algorithms on observational data. They are not intended for clinical diagnosis, medical evaluation, or dietary prescriptions."
          )
        )
      )
    )
  )
}

mod_prediction_server <- function(id, model_bundle, eval_summary = NULL, best_model_name = "Random Forest") {
  shiny::moduleServer(id, function(input, output, session) {
    
    state <- shiny::reactiveValues(
      prediction_result = NULL,
      user_data = NULL,
      bmi = NULL,
      errors = NULL,
      selected_model = best_model_name
    )
    
    # Observe Reset Button Click
    shiny::observeEvent(input$btn_reset, {
      shiny::updateSelectInput(session, "gender", selected = "male")
      shiny::updateNumericInput(session, "age", value = 25)
      shiny::updateNumericInput(session, "height", value = 175)
      shiny::updateNumericInput(session, "weight", value = 70)
      shiny::updateNumericInput(session, "duration", value = 30)
      shiny::updateNumericInput(session, "heart_rate", value = 100)
      shiny::updateNumericInput(session, "body_temp", value = 40.0)
      shiny::updateSelectInput(session, "model_choice", selected = best_model_name)
      
      state$prediction_result <- NULL
      state$user_data <- NULL
      state$bmi <- NULL
      state$errors <- NULL
      state$selected_model <- best_model_name
    })
    
    # Observe Predict Button Click
    shiny::observeEvent(input$btn_predict, {
      val_res <- validate_user_inputs(
        gender = input$gender,
        age = input$age,
        height = input$height,
        weight = input$weight,
        duration = input$duration,
        heart_rate = input$heart_rate,
        body_temp = input$body_temp
      )
      
      if (!val_res$valid) {
        state$errors <- val_res$errors
        state$prediction_result <- NULL
        state$user_data <- NULL
        return()
      }
      
      state$errors <- NULL
      state$user_data <- val_res$clean_data
      state$bmi <- val_res$bmi
      state$selected_model <- input$model_choice
      
      # Run prediction across all models
      pred_res <- predict_all_models(
        model_bundle$models,
        val_res$clean_data,
        best_model_name = input$model_choice
      )
      state$prediction_result <- pred_res
    }, ignoreNULL = FALSE)
    
    output$validation_errors <- shiny::renderUI({
      if (!is.null(state$errors) && length(state$errors) > 0) {
        shiny::div(
          style = "margin-top: 15px; padding: 12px; background-color: #F8D7DA; color: #721C24; border-radius: 5px; border-left: 4px solid #F5C6CB;",
          shiny::tags$b("Please correct the following errors:"),
          shiny::tags$ul(
            lapply(state$errors, function(err) shiny::tags$li(err))
          )
        )
      }
    })
    
    output$predicted_calories <- shiny::renderText({
      if (is.null(state$prediction_result)) {
        "-- kcal"
      } else {
        paste0(format(round(state$prediction_result$best_prediction, 1), nsmall = 1), " kcal")
      }
    })
    
    output$model_info_text <- shiny::renderText({
      if (is.null(state$prediction_result)) {
        "Click 'Predict Calories Burned' to generate estimate"
      } else {
        paste("Predicted via", state$prediction_result$best_model)
      }
    })
    
    output$bmi_display <- shiny::renderText({
      if (is.null(state$bmi)) {
        "-- kg/m²"
      } else {
        paste(round(state$bmi, 2), "kg/m²")
      }
    })
    
    output$bmi_category_display <- shiny::renderText({
      if (is.null(state$bmi)) return("Awaiting parameters")
      paste("Status:", as.character(classify_bmi(state$bmi)))
    })
    
    output$burn_rate_display <- shiny::renderText({
      if (is.null(state$prediction_result)) {
        "-- kcal/min"
      } else {
        paste(state$prediction_result$burn_rate_kcal_per_min, "kcal/min")
      }
    })
    
    output$model_rmse_display <- shiny::renderText({
      if (is.null(eval_summary) || is.null(eval_summary$test_eval$comparison)) return("± 4.48 kcal")
      comp <- eval_summary$test_eval$comparison
      m_name <- state$selected_model
      row_idx <- which(comp$Model == m_name)
      if (length(row_idx) > 0) {
        paste0("± ", sprintf("%.2f", comp$RMSE[row_idx]), " kcal")
      } else {
        "± 4.48 kcal"
      }
    })
    
    # Render Multi-Model Comparison Table
    output$tbl_model_comparison <- shiny::renderTable({
      if (is.null(state$prediction_result)) {
        return(data.frame(
          Model = c("Random Forest", "Linear Regression", "Decision Tree"),
          `Predicted Burn` = c("Awaiting input", "Awaiting input", "Awaiting input"),
          `Test RMSE` = c("4.479 kcal", "10.816 kcal", "12.849 kcal"),
          check.names = FALSE
        ))
      }
      
      preds <- state$prediction_result$predictions
      comp_metrics <- if (!is.null(eval_summary$test_eval$comparison)) eval_summary$test_eval$comparison else NULL
      
      rows <- lapply(names(preds), function(m) {
        val_rmse_str <- if (!is.null(comp_metrics) && m %in% comp_metrics$Model) {
          paste(round(comp_metrics$RMSE[comp_metrics$Model == m], 3), "kcal")
        } else {
          "N/A"
        }
        marker <- ifelse(m == state$selected_model, " (Selected)", "")
        data.frame(
          `Model Architecture` = paste0(m, marker),
          `Predicted Burn` = paste0(format(round(preds[[m]], 1), nsmall = 1), " kcal"),
          `Test RMSE` = val_rmse_str,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    # Render OLS Prediction Interval
    output$prediction_interval_box <- shiny::renderUI({
      if (is.null(state$prediction_result) || is.null(state$prediction_result$prediction_interval_lm)) return(NULL)
      pi <- state$prediction_result$prediction_interval_lm
      
      shiny::div(
        style = "background: #F0F7FD; padding: 15px 20px; border-radius: 8px; border-left: 4px solid #6A89A7; margin-bottom: 20px;",
        shiny::h5(style = "color: #384959; font-weight: bold; margin-top: 0;", "Linear Regression 95% Prediction Interval:"),
        shiny::p(
          style = "font-size: 15px; margin-bottom: 5px; color: #384959;",
          shiny::strong(sprintf("[%.1f kcal  —  %.1f kcal]", pi$lwr, pi$upr)),
          sprintf(" (Point Estimate: %.1f kcal)", pi$fit)
        ),
        shiny::p(
          style = "font-size: 12px; color: #666; margin: 0; font-style: italic;",
          "Method: Ordinary Least Squares (OLS) prediction interval assuming normally distributed homoscedastic residual errors. This statistical interval strictly applies to Linear Regression and is not transferable to Random Forest."
        )
      )
    })
    
    # Render Recommendations
    output$recommendation_html <- shiny::renderUI({
      if (is.null(state$bmi)) {
        return(shiny::HTML("<p style='color: #888;'>Enter your physical workout parameters and click 'Predict Calories Burned' to view personalized activity guidance.</p>"))
      }
      
      bmi_val <- state$bmi
      if (bmi_val < 18.5) {
        shiny::HTML("<div style='line-height: 1.6;'>
          <b style='color: #E63946;'>Underweight Category (BMI &lt; 18.5):</b>
          <p>Prioritize nutrient-dense caloric surplus alongside progressive resistance training 3x/week to stimulate lean muscle hypertrophy. Avoid prolonged extreme cardiovascular sessions without adequate caloric compensation.</p>
        </div>")
      } else if (bmi_val < 25.0) {
        shiny::HTML("<div style='line-height: 1.6;'>
          <b style='color: #2A9D8F;'>Optimal Healthy Weight (BMI 18.5 – 24.9):</b>
          <p>Maintain consistent aerobic conditioning (150 minutes weekly) paired with 2 weekly strength training sessions. Target workout intensity in aerobic zone (65–75% HR max) for cardiovascular longevity.</p>
        </div>")
      } else if (bmi_val < 30.0) {
        shiny::HTML("<div style='line-height: 1.6;'>
          <b style='color: #F4A261;'>Overweight Category (BMI 25.0 – 29.9):</b>
          <p>Combine moderate sustained cardiovascular exercise (30–45 min, 4–5x weekly) with a steady, sustainable caloric deficit of 300–500 kcal/day. Emphasize low-impact cross-training (elliptical, cycling, swimming) to protect joints.</p>
        </div>")
      } else {
        shiny::HTML("<div style='line-height: 1.6;'>
          <b style='color: #E76F51;'>Obese Classification (BMI &ge; 30.0):</b>
          <p>Engage in low-impact endurance training under appropriate physiological monitoring. Prioritize walking, water aerobics, and stationary cycling while consulting medical professionals for nutritional planning.</p>
        </div>")
      }
    })
    
    return(state)
  })
}
