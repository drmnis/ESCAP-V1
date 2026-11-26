# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s18_correlation_heatmaps  (sourced inside server(), local = TRUE)

  corr_result <- eventReactive(input$run_corr, {
    df <- dat_multi()
    rng <- input$multi_var_range
    req(rng)
    
    # Same variable range as PCA
    vars_idx <- seq(rng[1], rng[2])
    df_vars <- df[, vars_idx, drop = FALSE]
    
    # Coerce all to numeric
    X_num <- as.data.frame(
      lapply(df_vars, function(col) suppressWarnings(as.numeric(col)))
    )
    X_mat <- as.matrix(X_num)
    
    # Keep only columns that are fully numeric and have no NA
    good_cols <- which(colSums(is.na(X_mat)) == 0)
    validate(need(
      length(good_cols) > 1,
      "Need at least two complete numeric variables for correlation analysis."
    ))
    
    X_good <- X_mat[, good_cols, drop = FALSE]
    var_names <- colnames(df_vars)[good_cols]
    colnames(X_good) <- var_names
    
    # Spearman and Pearson correlations
    rc_spear <- Hmisc::rcorr(X_good, type = "spearman")
    rc_pear <- Hmisc::rcorr(X_good, type = "pearson")
    
    list(
      r_spearman = rc_spear$r,
      P_spearman = rc_spear$P,
      n_spearman = rc_spear$n,
      r_pearson  = rc_pear$r,
      P_pearson  = rc_pear$P,
      n_pearson  = rc_pear$n,
      vars       = var_names
    )
  })
  
  
  observeEvent(input$corr_spear_make_heat, {
    res <- corr_result()
    req(res$r_spearman)
    
    heatmap_state$corr_mat <- res$r_spearman
    heatmap_state$source <- "corr"
    
    # Send user to Graphs -> Univariate -> Heatmaps
    updateTabsetPanel(session, "graphs_subtab", selected = "General")
    updateSelectInput(session, "graph_type_uni", selected = "heatmaps")
  })
  
  observeEvent(input$corr_pearson_make_heat, {
    res <- corr_result()
    req(res$r_pearson)
    
    heatmap_state$corr_mat <- res$r_pearson
    heatmap_state$source <- "corr"
    
    updateTabsetPanel(session, "graphs_subtab", selected = "General")
    updateSelectInput(session, "graph_type_uni", selected = "heatmaps")
  })
  
  
  heatmap_plots <- reactive({
    tryCatch(
      {
        m_norm <- heatmap_matrix()
        m_raw <- heatmap_matrix_raw()
        
        # Return empty list if no data
        if (is.null(m_norm) || is.null(m_raw) ||
            nrow(m_norm) == 0 || ncol(m_norm) == 0) {
          return(list())
        }
        
        plots <- list()
        
        # 1) Basic seriate="mean"
        tryCatch(
          {
            p1 <- heatmaply(
              m_norm,
              seriate       = "mean",
              row_dend_left = TRUE,
              plot_method   = "plotly"
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[1]] <- p1
          },
          error = function(e) {
            cat("Heatmap 1 error:", e$message, "\n")
          }
        )
        
        # 2) k_row, k_col clusters
        tryCatch(
          {
            k_row_val <- min(2, nrow(m_norm))
            k_col_val <- min(2, ncol(m_norm))
            
            p2 <- heatmaply(
              m_norm,
              k_row = k_row_val,
              k_col = k_col_val
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[2]] <- p2
          },
          error = function(e) {
            cat("Heatmap 2 error:", e$message, "\n")
          }
        )
        
        # 3) RdBu diverging palette
        tryCatch(
          {
            k_row_val <- min(2, nrow(m_norm))
            k_col_val <- min(2, ncol(m_norm))
            
            p3 <- heatmaply(
              m_norm,
              colors = colorRampPalette(RColorBrewer::brewer.pal(3, "RdBu"))(256),
              k_row  = k_row_val,
              k_col  = k_col_val
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[3]] <- p3
          },
          error = function(e) {
            cat("Heatmap 3 error:", e$message, "\n")
          }
        )
        
        # 4) Custom gradient (blue\u2013red, midpoint 0.5)
        tryCatch(
          {
            gradient_col <- ggplot2::scale_fill_gradient2(
              low      = "blue",
              high     = "red",
              midpoint = 0.5,
              limits   = c(0, 1)
            )
            p4 <- heatmaply(
              m_norm,
              scale_fill_gradient_fun = gradient_col
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[4]] <- p4
          },
          error = function(e) {
            cat("Heatmap 4 error:", e$message, "\n")
          }
        )
        
        # 5) Side colors - SAFE VERSION
        tryCatch(
          {
            if (ncol(m_norm) >= 3) {
              main_cols <- max(1, ncol(m_norm) - 2)
              main_mat <- m_norm[, 1:main_cols, drop = FALSE]
              row_side <- m_norm[, (main_cols + 1):ncol(m_norm), drop = FALSE]
              col_side <- 0:(ncol(main_mat) - 1)
            } else {
              main_mat <- m_norm
              row_side <- NULL
              col_side <- NULL
            }
            
            p5 <- heatmaply(
              main_mat,
              col_side_colors = col_side,
              row_side_colors = row_side
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[5]] <- p5
          },
          error = function(e) {
            cat("Heatmap 5 error:", e$message, "\n")
          }
        )
        
        # 6) Cell notes = original values
        tryCatch(
          {
            # Ensure dimensions match
            if (nrow(m_raw) == nrow(m_norm) && ncol(m_raw) == ncol(m_norm)) {
              cn <- round(m_raw, 2)
            } else {
              cn <- round(m_norm, 2)
            }
            
            p6 <- heatmaply(
              m_norm,
              cellnote = cn
            ) %>% layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            )
            plots[[6]] <- p6
          },
          error = function(e) {
            cat("Heatmap 6 error:", e$message, "\n")
          }
        )
        
        # Remove NULL plots
        plots <- plots[!sapply(plots, is.null)]
        
        # Ensure all plots have valid data
        if (length(plots) == 0) {
          return(list())
        }
        
        return(plots)
      },
      error = function(e) {
        cat("Overall heatmap_plots error:", e$message, "\n")
        return(list())
      }
    )
  })
  
  output$heatmaps_all <- renderPlotly({
    req(input$graph_type_uni == "heatmaps")
    
    tryCatch(
      {
        plots <- heatmap_plots()
        
        if (length(plots) == 0) {
          # Create a simple error message plot
          p <- plot_ly() %>%
            add_annotations(
              text      = "No valid heatmap data available or heatmap generation failed.",
              x         = 0.5,
              y         = 0.5,
              xref      = "paper",
              yref      = "paper",
              showarrow = FALSE,
              font      = list(size = 14, color = "red")
            ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              xaxis         = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis         = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
          return(p)
        }
        
        # Create a simpler arrangement - just show the first plot
        # This avoids subplot issues
        idx <- suppressWarnings(as.integer(input$simple_bar_which))
        if (is.na(idx) || idx < 1 || idx > length(plots)) idx <- 1
        
        return(plots[[idx]])
      },
      error = function(e) {
        cat("heatmaps_all render error:", e$message, "\n")
        
        # Return a simple error plot
        plot_ly() %>%
          add_annotations(
            text      = paste("Error creating heatmap:", e$message),
            x         = 0.5,
            y         = 0.5,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            font      = list(size = 12, color = "red")
          ) %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis         = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis         = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      }
    )
  })
  
  
  output$graph_plot_corr <- esc_renderPlot({
    # Only for correlation tab; use same code as for downloads
    req(input$graph_type_corr %in% c("corr_spearman", "corr_pearson"))
    draw_current_graph()
  })
  
  
  output$dendro_plot <- esc_renderPlot({
    req(input$graphs_subtab == "General",
        input$graph_type_uni == "pca_dendrogram")
    draw_current_graph()
  })
  
  
  
  ### ADD HERE (PLSR) \u2013 VIP & SR helpers + main PLSR reactive
  
  # Helper: VIP
