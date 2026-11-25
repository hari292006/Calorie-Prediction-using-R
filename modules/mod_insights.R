# ==============================================================================
# modules/mod_insights.R - Dataset Insights & User Analytics Shiny Module
# ==============================================================================

mod_insights_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("insights_tabs"),
      
      # Sub-Tab 1: Population Analytics & Feature Importance
      shiny::tabPanel("Population & Correlation Analysis",
        shiny::fluidRow(
          shiny::column(6,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 15px; margin-bottom: 20px;",
              shiny::h4("Feature Correlation Heatmap", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::p("Correlation matrix across all numeric features (User_ID strictly excluded to prevent data leakage).", style = "color: #666; font-size: 13px;"),
              shiny::plotOutput(ns("plot_corr"), height = "420px")
            )
          ),
          shiny::column(6,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 15px; margin-bottom: 20px;",
              shiny::h4("Random Forest Feature Importance", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::p("Relative feature importance measured via total reduction in node impurity (IncNodePurity).", style = "color: #666; font-size: 13px;"),
              shiny::plotOutput(ns("plot_imp"), height = "420px")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
              shiny::h4("Physiological Feature Relationship & High R² Investigation", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::p(
                "In exercise physiology, energy expenditure (calories) is a direct thermodynamic function of work performed: ",
                shiny::strong("Work = Metabolic Intensity × Duration. "),
                "Workout Duration (r = +0.955) and Heart Rate (r = +0.898) serve as direct physiological proxies for volume and cardiovascular exertion, explaining why Random Forest achieves R² > 0.99. This reflects genuine physical law rather than artificial data leakage."
              ),
              shiny::tableOutput(ns("tbl_correlations"))
            )
          )
        )
      ),
      
      # Sub-Tab 2: User Context Analytics
      shiny::tabPanel("User Context & Distribution Analytics",
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 15px; margin-bottom: 20px;",
              shiny::h4("Your Workout in Population Context", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::p("Visualizes your workout duration and predicted calories relative to the 15,000 observations in the dataset.", style = "color: #666; font-size: 13px;"),
              shiny::plotOutput(ns("plot_user_context"), height = "420px")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
              shiny::h4("User vs. Dataset Distribution Snapshots", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::p("Four-panel overview comparing your Age, BMI, Workout Duration, and Heart Rate to population distributions.", style = "color: #666; font-size: 13px;"),
              shiny::plotOutput(ns("plot_user_snapshots"), height = "480px")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 20px;",
              shiny::h4("User Profile Summary Card", style = "color: #384959; font-weight: bold; margin-top: 0;"),
              shiny::plotOutput(ns("plot_user_card"), height = "380px")
            )
          )
        )
      )
    )
  )
}

mod_insights_server <- function(id, dataset, model_bundle, pred_state) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # 1. Correlation Matrix
    output$plot_corr <- shiny::renderPlot({
      if (is.null(dataset)) return(NULL)
      plot_correlation_matrix(dataset)
    })
    
    # 2. Random Forest Importance
    output$plot_imp <- shiny::renderPlot({
      if (is.null(model_bundle) || is.null(model_bundle$models[["Random Forest"]])) return(NULL)
      plot_feature_importance(model_bundle$models[["Random Forest"]])
    })
    
    # 3. Target Correlation Ranking Table
    output$tbl_correlations <- shiny::renderTable({
      if (is.null(dataset)) return(NULL)
      num_df <- dataset[, sapply(dataset, is.numeric), drop = FALSE]
      if ("User_ID" %in% colnames(num_df)) num_df$User_ID <- NULL
      
      cors <- sort(cor(num_df)["Calories", ], decreasing = TRUE)
      df_cors <- data.frame(
        Feature = names(cors),
        `Correlation with Calories (r)` = sprintf("%.4f", cors),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df_cors <- df_cors[df_cors$Feature != "Calories", ]
      df_cors
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    # 4. User Context Scatter
    output$plot_user_context <- shiny::renderPlot({
      dur <- if (!is.null(pred_state$user_data)) pred_state$user_data$Duration[1] else 30
      cal <- if (!is.null(pred_state$prediction_result)) pred_state$prediction_result$best_prediction else 115.4
      plot_user_scatter(dataset, user_duration = dur, user_calories = cal)
    })
    
    # 5. User Snapshots
    output$plot_user_snapshots <- shiny::renderPlot({
      u_data <- if (!is.null(pred_state$user_data)) {
        pred_state$user_data
      } else {
        data.frame(Gender = factor("male", levels = c("female", "male")),
                   Age = 25, Height = 175, Weight = 70, Duration = 30,
                   Heart_Rate = 100, Body_Temp = 40.0, BMI = 22.86)
      }
      plot_user_snapshots(dataset, u_data)
    })
    
    # 6. User Profile Card
    output$plot_user_card <- shiny::renderPlot({
      u_data <- if (!is.null(pred_state$user_data)) {
        pred_state$user_data
      } else {
        data.frame(Gender = factor("male", levels = c("female", "male")),
                   Age = 25, Height = 175, Weight = 70, Duration = 30,
                   Heart_Rate = 100, Body_Temp = 40.0, BMI = 22.86)
      }
      cal <- if (!is.null(pred_state$prediction_result)) pred_state$prediction_result$best_prediction else 115.4
      m_name <- if (!is.null(pred_state$prediction_result)) pred_state$prediction_result$best_model else "Random Forest"
      plot_user_summary_card(u_data, cal, model_name = m_name)
    })
  })
}
