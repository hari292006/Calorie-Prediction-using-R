# ==============================================================================
# R/visualizations.R - Reusable Data Visualizations & Diagnostic Plots
# ==============================================================================

# Primary color palette for consistency across dashboard and standalone exports
PALETTE <- list(
  light_blue = "#BDDDFC",
  soft_blue  = "#88BDF2",
  steel_blue = "#6A89A7",
  deep_slate = "#384959",
  accent     = "#E63946",
  gradient   = c("#BDDDFC", "#88BDF2", "#6A89A7", "#384959")
)

#' Set standard ggplot2 theme with local font fallbacks
get_project_theme <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = PALETTE$deep_slate, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#555555", hjust = 0.5),
        axis.title = ggplot2::element_text(face = "bold", size = 11, color = PALETTE$deep_slate),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.4)
      )
  } else {
    NULL
  }
}

#' Generate correlation matrix plot
#' @param df data.frame containing numeric variables
#' @param file_path Optional file path to save plot (PNG)
#' @return The generated plot or matrix
plot_correlation_matrix <- function(df, file_path = NULL) {
  # Exclude User_ID if present
  num_cols <- df[, sapply(df, is.numeric), drop = FALSE]
  if ("User_ID" %in% colnames(num_cols)) {
    num_cols$User_ID <- NULL
  }
  
  corr_mat <- cor(num_cols, use = "complete.obs")
  
  if (!is.null(file_path)) {
    png(file_path, width = 800, height = 700, res = 120)
  }
  
  if (requireNamespace("corrplot", quietly = TRUE)) {
    corrplot::corrplot(
      corr_mat,
      method = "color",
      type = "upper",
      tl.col = PALETTE$deep_slate,
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.8,
      title = "Feature Correlation Matrix",
      mar = c(0, 0, 2, 0)
    )
  } else {
    # Base R fallback
    image(corr_mat, main = "Correlation Matrix", col = colorRampPalette(PALETTE$gradient)(20))
  }
  
  if (!is.null(file_path)) {
    dev.off()
    message("Saved correlation matrix to: ", file_path)
  }
  
  invisible(corr_mat)
}

#' Plot Feature Importance for Random Forest or Decision Tree
#' @param model Trained model
#' @param file_path Optional path to save PNG
#' @return ggplot object or data.frame
plot_feature_importance <- function(model, file_path = NULL) {
  imp_df <- NULL
  
  if (inherits(model, "randomForest") && requireNamespace("randomForest", quietly = TRUE)) {
    imp <- randomForest::importance(model)
    metric_col <- if ("IncNodePurity" %in% colnames(imp)) "IncNodePurity" else colnames(imp)[1]
    imp_df <- data.frame(
      Feature = rownames(imp),
      Importance = as.numeric(imp[, metric_col]),
      stringsAsFactors = FALSE
    )
  } else if (inherits(model, "rpart")) {
    vi <- model$variable.importance
    imp_df <- data.frame(
      Feature = names(vi),
      Importance = as.numeric(vi),
      stringsAsFactors = FALSE
    )
  } else if (inherits(model, "lm")) {
    # Use absolute t-statistic from summary
    s <- summary(model)$coefficients
    # Exclude intercept
    s <- s[rownames(s) != "(Intercept)", , drop = FALSE]
    imp_df <- data.frame(
      Feature = rownames(s),
      Importance = abs(s[, "t value"]),
      stringsAsFactors = FALSE
    )
  }
  
  if (is.null(imp_df) || nrow(imp_df) == 0) return(NULL)
  
  imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    imp_df$Feature <- factor(imp_df$Feature, levels = imp_df$Feature)
    p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = Feature, y = Importance, fill = Importance)) +
      ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = PALETTE$light_blue, high = PALETTE$deep_slate) +
      ggplot2::labs(title = "Feature Importance Analysis", x = "Feature", y = "Relative Importance") +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 8, height = 6, dpi = 150)
    }
    return(p)
  } else {
    # Base R fallback
    barplot(imp_df$Importance, names.arg = imp_df$Feature, horiz = TRUE, las = 1,
            col = PALETTE$steel_blue, main = "Feature Importance")
    return(invisible(imp_df))
  }
}

#' Plot Model Performance Comparison (RMSE / MAE)
#' @param comparison_table data.frame of evaluation metrics
#' @param metric Metric to plot ("RMSE" or "MAE")
#' @param file_path Optional path to save PNG
plot_model_comparison <- function(comparison_table, metric = "RMSE", file_path = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(comparison_table, ggplot2::aes(x = reorder(Model, .data[[metric]]), y = .data[[metric]], fill = Model)) +
      ggplot2::geom_col(width = 0.5, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = round(.data[[metric]], 2)), vjust = -0.4, fontface = "bold", color = PALETTE$deep_slate) +
      ggplot2::scale_fill_manual(values = c("Linear Regression" = PALETTE$steel_blue,
                                            "Decision Tree" = PALETTE$soft_blue,
                                            "Random Forest" = PALETTE$deep_slate)) +
      ggplot2::labs(title = paste("Model Comparison (", metric, " - Lower is Better)"),
                    x = "Model Architecture", y = paste("Validation", metric)) +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 8, height = 6, dpi = 150)
    }
    return(p)
  }
}

#' Plot Residual Distributions
#' @param residuals_df data.frame of residuals (Actual - Predicted)
#' @param file_path Optional path to save PNG
plot_residual_distribution <- function(residuals_df, file_path = NULL) {
  # Pivot to long format for ggplot2
  models <- setdiff(colnames(residuals_df), "Actual")
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    long_list <- list()
    for (m in models) {
      long_list[[m]] <- data.frame(Model = m, Residual = residuals_df[[m]])
    }
    long_df <- do.call(rbind, long_list)
    
    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = Residual, fill = Model)) +
      ggplot2::geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
      ggplot2::scale_fill_manual(values = c("Linear Regression" = PALETTE$steel_blue,
                                            "Decision Tree" = PALETTE$soft_blue,
                                            "Random Forest" = PALETTE$deep_slate)) +
      ggplot2::labs(title = "Model Residual Distribution", subtitle = "Centered around 0 indicates unbiased predictions",
                    x = "Residual Error (Actual - Predicted kcal)", y = "Frequency Count") +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 8, height = 6, dpi = 150)
    }
    return(p)
  }
}

#' Plot Predicted vs Actual Values
#' @param predictions_df data.frame of predictions
#' @param file_path Optional path to save PNG
plot_actual_vs_predicted <- function(predictions_df, file_path = NULL) {
  models <- setdiff(colnames(predictions_df), "Actual")
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    long_list <- list()
    for (m in models) {
      long_list[[m]] <- data.frame(Model = m, Actual = predictions_df$Actual, Predicted = predictions_df[[m]])
    }
    long_df <- do.call(rbind, long_list)
    
    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = Actual, y = Predicted, color = Model)) +
      ggplot2::geom_point(alpha = 0.4, size = 1.2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
      ggplot2::scale_color_manual(values = c("Linear Regression" = PALETTE$steel_blue,
                                             "Decision Tree" = PALETTE$soft_blue,
                                             "Random Forest" = PALETTE$deep_slate)) +
      ggplot2::labs(title = "Actual vs. Predicted Calories",
                    subtitle = "Dashed line indicates perfect 1:1 agreement",
                    x = "Observed Calories (kcal)", y = "Predicted Calories (kcal)") +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 8, height = 6, dpi = 150)
    }
    return(p)
  }
}

#' Plot User Context Scatter: Highlights user prediction within dataset distribution
#' @param dataset Full dataset
#' @param user_duration Numeric duration in minutes
#' @param user_calories Predicted calories
#' @param file_path Optional path to save PNG
plot_user_scatter <- function(dataset, user_duration, user_calories, file_path = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(dataset, ggplot2::aes(x = Duration, y = Calories)) +
      ggplot2::geom_point(ggplot2::aes(color = Calories), size = 1.8, alpha = 0.6) +
      ggplot2::scale_color_gradientn(colors = PALETTE$gradient) +
      ggplot2::annotate("point", x = user_duration, y = user_calories, size = 6, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = user_duration, y = user_calories,
                        label = paste0(" You: ", round(user_calories, 1), " kcal"),
                        vjust = -1.2, color = PALETTE$deep_slate, fontface = "bold", size = 4.5) +
      ggplot2::labs(title = "Your Workout in Context of Overall Dataset",
                    subtitle = "Exercise Duration vs. Calories Burned",
                    x = "Workout Duration (minutes)", y = "Calories Burnt (kcal)") +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 9, height = 6, dpi = 150)
    }
    return(p)
  }
}


#' Plot Feature Density with User Indicator Line
#' @param dataset Full dataset
#' @param feature Character name of feature column
#' @param label Display title for feature
#' @param user_value User numeric value
#' @param file_path Optional path to save PNG
plot_feature_density <- function(dataset, feature, label, user_value, file_path = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    vals <- dataset[[feature]]
    dens <- density(vals, na.rm = TRUE)
    df_dens <- data.frame(x = dens$x, y = dens$y)
    
    p <- ggplot2::ggplot(df_dens, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_area(fill = PALETTE$light_blue, alpha = 0.5) +
      ggplot2::geom_line(color = PALETTE$steel_blue, linewidth = 1.2) +
      ggplot2::geom_vline(xintercept = user_value, color = PALETTE$deep_slate, linewidth = 1.3, linetype = "solid") +
      ggplot2::annotate("text", x = user_value, y = max(df_dens$y) * 0.9,
                        label = paste("You:", round(user_value, 1)),
                        color = PALETTE$deep_slate, fontface = "bold", vjust = -0.5, hjust = -0.1) +
      ggplot2::labs(title = paste(label, "Distribution in Dataset"),
                    x = label, y = "Density") +
      get_project_theme()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 8, height = 5, dpi = 150)
    }
    return(p)
  }
}

#' Plot 4-Panel User Snapshot Overview (Age, BMI, Duration, Heart Rate)
#' @param dataset Full dataset
#' @param user_data Single-row user data.frame
#' @param file_path Optional path to save PNG
plot_user_snapshots <- function(dataset, user_data, file_path = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Prepare data for faceting
    features <- c("Age", "BMI", "Duration", "Heart_Rate")
    labels <- c("Age (years)", "BMI (kg/m²)", "Duration (min)", "Heart Rate (bpm)")
    names(labels) <- features
    
    long_list <- lapply(features, function(f) {
      data.frame(
        Feature = labels[[f]],
        Value = dataset[[f]],
        UserVal = user_data[[f]][1]
      )
    })
    long_df <- do.call(rbind, long_list)
    
    # Calculate user values per facet for geom_vline
    user_lines <- data.frame(
      Feature = labels,
      UserVal = sapply(features, function(f) user_data[[f]][1])
    )
    
    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = Value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              bins = 25, fill = PALETTE$steel_blue, alpha = 0.75, color = "white") +
      ggplot2::geom_vline(data = user_lines, ggplot2::aes(xintercept = UserVal),
                          color = PALETTE$deep_slate, linewidth = 1.2, linetype = "solid") +
      ggplot2::facet_wrap(~ Feature, scales = "free", ncol = 2) +
      ggplot2::labs(title = "User vs. Population Snapshot Overview",
                    subtitle = "Dark vertical line indicates your specific value",
                    x = "", y = "Density") +
      get_project_theme() +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", color = PALETTE$deep_slate))
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 10, height = 7, dpi = 150)
    }
    return(p)
  }
}

#' Plot Clean User Profile Summary Card
#' @param user_data Single-row user data.frame
#' @param pred_calories Predicted calorie expenditure
#' @param model_name Name of selected model
#' @param file_path Optional path to save PNG
plot_user_summary_card <- function(user_data, pred_calories, model_name = "Random Forest", file_path = NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      # Background container
      ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                        fill = PALETTE$soft_blue, alpha = 0.4, color = NA) +
      ggplot2::annotate("rect", xmin = 0.03, xmax = 0.97, ymin = 0.03, ymax = 0.97,
                        fill = "#FFFFFF", alpha = 0.95, color = PALETTE$steel_blue, linewidth = 1) +
      
      # Header
      ggplot2::annotate("text", x = 0.5, y = 0.90, label = "USER PROFILE & PREDICTION SUMMARY",
                        size = 6, fontface = "bold", color = PALETTE$deep_slate, hjust = 0.5) +
      
      # Left Column: User Inputs
      ggplot2::annotate("text", x = 0.10, y = 0.78, label = paste("• Gender:", tools::toTitleCase(as.character(user_data$Gender))),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.70, label = paste("• Age:", user_data$Age, "years"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.62, label = paste("• Height:", user_data$Height, "cm"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.54, label = paste("• Weight:", user_data$Weight, "kg"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.46, label = paste("• BMI:", round(user_data$BMI, 2), "kg/m²"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.38, label = paste("• Duration:", user_data$Duration, "min"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.30, label = paste("• Heart Rate:", user_data$Heart_Rate, "bpm"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      ggplot2::annotate("text", x = 0.10, y = 0.22, label = paste("• Body Temp:", user_data$Body_Temp, "°C"),
                        hjust = 0, size = 4.5, color = PALETTE$deep_slate) +
      
      # Right Column: Big Predicted Calories Box
      ggplot2::annotate("rect", xmin = 0.55, xmax = 0.92, ymin = 0.35, ymax = 0.75,
                        fill = PALETTE$light_blue, alpha = 0.5, color = PALETTE$steel_blue, linewidth = 0.8) +
      ggplot2::annotate("text", x = 0.735, y = 0.67, label = "PREDICTED BURN",
                        size = 4.5, fontface = "bold", color = PALETTE$deep_slate, hjust = 0.5) +
      ggplot2::annotate("text", x = 0.735, y = 0.53, label = paste(round(pred_calories, 1), "kcal"),
                        size = 8.5, fontface = "bold", color = PALETTE$deep_slate, hjust = 0.5) +
      ggplot2::annotate("text", x = 0.735, y = 0.42, label = paste("via", model_name),
                        size = 3.8, fontface = "italic", color = "#555555", hjust = 0.5) +
      
      # Footer
      ggplot2::annotate("text", x = 0.5, y = 0.10, label = "Calorie Burn Predictor | Department of Computer Science",
                        size = 3.5, color = "#888888", hjust = 0.5) +
      
      ggplot2::theme_void()
    
    if (!is.null(file_path)) {
      ggplot2::ggsave(file_path, p, width = 9, height = 6, dpi = 150)
    }
    return(p)
  }
}
