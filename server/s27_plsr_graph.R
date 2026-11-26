# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s27_plsr_graph  (sourced inside server(), local = TRUE)

  # PLSR score plot: same options as the PCA score plot (LV pair, ellipse, hull, spokes, labels, cos2)
  plsr_score_gg <- function(res, repel = TRUE) {
    idx <- as.integer(strsplit(input$pca_pc_pair %||% "1_2", "_")[[1]])
    T <- unclass(pls::scores(res$fit)); T <- matrix(T, nrow = nrow(T))
    validate(need(ncol(T) >= max(idx), sprintf("LV%d is not available; the model has %d LVs.", max(idx), ncol(T))))
    esc_plot_scores(T, idx[1], idx[2], Class = res$cal_class, ID = res$cal_id, expl = esc_pls_explvar(res$fit),
                    overlays = input$pca_scores_overlays %||% character(0), show_id = isTRUE(input$pca_show_id),
                    cos2 = isTRUE(input$pca_color_cos2), class_title = class_legend_title(), repel = repel,
                    colour_by = if (is.null(res$cal_class)) res$y_cal, colour_title = paste("Measured", input$plsr_y_col %||% "Y"))
  }

  output$plsr_graph <- renderPlotly({
    if (isTRUE(input$plsr_graph_type %in% ESC_EXTRA_GRAPHS)) return(esc_ggplotly(extra_plot_obj()))
    res <- plsr_result()
    
    # Error handling
    if (is.null(res) || !is.null(res$error)) {
      p <- ggplot() +
        annotate(
          "text",
          x = 0.5, y = 0.5,
          label = paste("PLSR Error:", ifelse(!is.null(res$error), res$error, "Unknown error")),
          size = 6, color = "red"
        ) +
        theme_void()
      return(esc_ggplotly(p))
    }
    
    req(input$plsr_graph_type)
    
    # ----------------- LV vs RMSEC / RMSECV -----------------
    if (input$plsr_graph_type == "lv_rmse") {
      perf <- res$perf
      req(nrow(perf) > 0)
      
      lv_num <- as.numeric(sub("LV-", "", perf$LV))
      df <- data.frame(
        LV     = lv_num,
        RMSEC  = perf$RMSEC,
        RMSECV = perf$RMSECV
      )
      
      df_long <- tidyr::pivot_longer(
        df,
        cols      = c("RMSEC", "RMSECV"),
        names_to  = "Metric",
        values_to = "Value"
      )
      
      p <- ggplot(df_long, aes(x = LV, y = Value, color = Metric)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        scale_x_continuous(breaks = lv_num) +
        labs(
          x = "Number of latent variables (LV)",
          y = "RMSE"
        )
      return(esc_ggplotly(p))
    }
    
    # ----------------- Scores: LV1 vs LV2 -----------------
    if (input$plsr_graph_type == "lv_scores") {
      req(res$fit)
      return(esc_ggplotly(plsr_score_gg(res, repel = FALSE)))
    }
    
    
    # ----------------- Measured vs CV\u2011predicted Y -----------------
    if (input$plsr_graph_type == "y_meas_vs_cv") {
      req(length(res$y_cal) > 0, length(res$y_cv_hat) > 0)
      
      valid_idx <- !is.na(res$y_cal) & !is.na(res$y_cv_hat)
      if (sum(valid_idx) < 2) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "No cross\u2011validation predictions available.\nCheck CV type and data.",
            size = 5, color = "red"
          ) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
        return(esc_ggplotly(p))
      }
      
      y_obs <- res$y_cal[valid_idx]
      y_pred <- res$y_cv_hat[valid_idx]
      resid <- y_obs - y_pred
      
      # IDs for optional labels
      if (!is.null(res$cal_id) && length(res$cal_id) == length(res$y_cal)) {
        ID <- res$cal_id[valid_idx]
      } else {
        ID <- seq_len(length(y_obs))
      }
      
      df <- data.frame(
        Observed  = y_obs,
        Predicted = y_pred,
        Residual  = resid,
        ID        = ID
      )
      
      # axis limits similar to Python example
      lLim <- min(df$Predicted, df$Observed, na.rm = TRUE)
      uLim <- max(df$Predicted, df$Observed, na.rm = TRUE)
      rLim <- (uLim - lLim) * 0.15
      
      zeroLine <- seq(lLim - rLim, uLim + rLim, length.out = 200)
      
      # regression of Observed ~ Predicted
      fit <- lm(Observed ~ Predicted, data = df)
      predLine <- predict(fit, newdata = data.frame(Predicted = zeroLine))
      
      p <- ggplot(df, aes(x = Predicted, y = Observed, color = Residual)) +
        geom_point(alpha = 0.75, size = 2.2) +
        {
          if (isTRUE(input$pca_show_id)) {
            geom_text(
              aes(label = ID),
              size = 3,
              vjust = -0.7,
              show.legend = FALSE
            )
          } else {
            NULL
          }
        } +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = "dashed", color = "red", linewidth = 1
        ) +
        geom_line(
          data = data.frame(Predicted = zeroLine, Observed = predLine),
          aes(x = Predicted, y = Observed),
          color = "black", linetype = "dashed", linewidth = 0.8,
          inherit.aes = FALSE
        ) +
        coord_equal(xlim = c(lLim - rLim, uLim + rLim),
                    ylim = c(lLim - rLim, uLim + rLim)) +
        scale_color_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(
          x     = "Predicted Y (CV)",
          y     = "Observed Y (Calibration)",
          color = "Residual\n(Obs \u2212 Pred)"
        )
      
      return(esc_ggplotly(p))
    }
    
    
    
    
    
    # ----------------- Measured vs Predicted Y (external) -----------------
    if (input$plsr_graph_type == "y_meas_vs_pred") {
      req(length(res$y_pred) > 0, length(res$y_pred_hat) > 0)
      
      valid_idx <- !is.na(res$y_pred) & !is.na(res$y_pred_hat)
      if (sum(valid_idx) < 2) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "No external predictions available.\nCheck calibration/prediction split and data.",
            size = 5, color = "red"
          ) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
        return(esc_ggplotly(p))
      }
      
      y_obs  <- res$y_pred[valid_idx]
      y_pred <- res$y_pred_hat[valid_idx]
      resid  <- y_obs - y_pred
      
      if (!is.null(res$pred_id) && length(res$pred_id) == length(res$y_pred)) {
        ID <- res$pred_id[valid_idx]
      } else {
        ID <- seq_len(length(y_obs))
      }
      
      df <- data.frame(
        Observed  = y_obs,
        Predicted = y_pred,
        Residual  = resid,
        ID        = ID
      )
      
      lLim <- min(df$Predicted, df$Observed, na.rm = TRUE)
      uLim <- max(df$Predicted, df$Observed, na.rm = TRUE)
      rLim <- (uLim - lLim) * 0.15
      
      zeroLine <- seq(lLim - rLim, uLim + rLim, length.out = 200)
      
      fit <- lm(Observed ~ Predicted, data = df)
      predLine <- predict(fit, newdata = data.frame(Predicted = zeroLine))
      
      p <- ggplot(df, aes(x = Predicted, y = Observed, color = Residual)) +
        geom_point(alpha = 0.75, size = 2.2) +
        {
          if (isTRUE(input$pca_show_id)) {
            geom_text(
              aes(label = ID),
              size = 3,
              vjust = -0.7,
              show.legend = FALSE
            )
          } else {
            NULL
          }
        } +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = "dashed", color = "red", linewidth = 1
        ) +
        geom_line(
          data = data.frame(Predicted = zeroLine, Observed = predLine),
          aes(x = Predicted, y = Observed),
          color = "black", linetype = "dashed", linewidth = 0.8,
          inherit.aes = FALSE
        ) +
        coord_equal(xlim = c(lLim - rLim, uLim + rLim),
                    ylim = c(lLim - rLim, uLim + rLim)) +
        scale_color_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(
          x     = "Predicted Y (External)",
          y     = "Observed Y (External)",
          color = "Residual\n(Obs \u2212 Pred)"
        )
      
      return(esc_ggplotly(p))
    }
    
    
    
    
    # ----------------- Q vs T\u00b2 -----------------
    if (input$plsr_graph_type == "q_vs_t2") {
      df <- data.frame(
        Q  = res$Q,
        T2 = res$T2
      )
      # Class (for color) if available
      if (!is.null(res$cal_class) && length(res$cal_class) == nrow(df)) {
        df$Class <- res$cal_class
      } else {
        df$Class <- factor("All")
      }
      # IDs
      if (!is.null(res$cal_id) && length(res$cal_id) == nrow(df)) {
        df$ID <- res$cal_id
      } else {
        df$ID <- seq_len(nrow(df))
      }
      
      # legend only if a class column is selected
      show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
      
      p <- ggplot(
        df,
        aes(x = T2, y = Q, color = Class, key = ID)
      ) +
        geom_point(alpha = 0.7, size = 2) +
        theme_minimal() +
        labs(
          x     = "Hotelling T\u00b2",
          y     = "Q residuals",
          color = class_legend_title()
        )
      
      if (isTRUE(input$pca_show_id)) {
        p <- p +
          geom_text(
            aes(label = ID),
            color = "black",
            size = 3,
            vjust = -0.5,
            show.legend = FALSE
          )
      }
      
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      }
      
      return(
        esc_ggplotly(p, source = "plsr_qt") %>%
          plotly::event_register("plotly_click") %>%
          plotly::event_register("plotly_selected") %>%
          plotly::layout(
            dragmode   = "lasso",
            selectdirection = "any",
            showlegend = show_legend  # reinforce at plotly level
          )
      )
    }
    
    
    
    # ----------------- Regression vector -----------------
    if (input$plsr_graph_type == "reg_vector") {
      vars <- names(res$coef)
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$coef[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Regression coefficient"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Regression coefficient"
          )
      }
      return(esc_ggplotly(p))
    }
    
    # ----------------- VIP -----------------
    if (input$plsr_graph_type == "vip") {
      vars <- names(res$vip)
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$vip[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "VIP score"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "VIP score"
          )
      }
      return(esc_ggplotly(p))
    }
    
    # ----------------- Selectivity ratio -----------------
    if (input$plsr_graph_type == "sr") {
      vars <- names(res$sr)
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$sr[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Selectivity ratio"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Selectivity ratio"
          )
      }
      return(esc_ggplotly(p))
    }
  })
  
  
  #################### Draw Current Graph ###########################
  
