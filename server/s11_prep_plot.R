# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s11_prep_plot  (sourced inside server(), local = TRUE)

  output$graph_plot_prep <- renderPlotly({
    req(input$graph_type_prep)
    
    # show legend only if a class column is selected
    show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
    
    ############################################
    ## 1) Preprocessing preview  (existing)   ##
    ############################################
    if (input$graph_type_prep == "prep_preview") {
      df <- dat_multi()
      req(input$prep_preview_var)
      
      # Build X matrix as in PCA
      rng <- input$multi_var_range
      req(rng)
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      # Apply preprocessing
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      
      # Ensure same columns for X, Xp, and var_names
      var_names_full <- colnames(df)[vars_idx]
      p_X <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for preprocessing preview."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      # ID
      if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
          input$multi_id_col %in% names(df)) {
        ID <- df[[input$multi_id_col]]
      } else {
        ID <- seq_len(nrow(df))
      }
      ID_chr <- as.character(ID)
      
      # Class (for colors)
      if (!is.null(input$multi_class_col) && input$multi_class_col != "" &&
          input$multi_class_col %in% names(df)) {
        Class <- factor(df[[input$multi_class_col]])
      } else {
        Class <- factor(rep("All", nrow(df)))
      }
      
      selected <- input$prep_preview_var
      
      if (selected == "__ALL__") {
        raw_long <- as.data.frame(X)
        colnames(raw_long) <- var_names
        raw_long$Sample <- ID_chr
        raw_long$Class <- Class
        raw_long <- tidyr::pivot_longer(
          raw_long,
          cols      = all_of(var_names),
          names_to  = "Variable",
          values_to = "Value"
        )
        raw_long$Type <- "Raw"
        
        pre_long <- as.data.frame(Xp)
        colnames(pre_long) <- var_names
        pre_long$Sample <- ID_chr
        pre_long$Class <- Class
        pre_long <- tidyr::pivot_longer(
          pre_long,
          cols      = all_of(var_names),
          names_to  = "Variable",
          values_to = "Value"
        )
        pre_long$Type <- "Preprocessed"
        
        plot_df_long <- dplyr::bind_rows(raw_long, pre_long)
        plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
        
        n_vars <- length(var_names)
        n_breaks <- min(10, n_vars)
        idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
        var_breaks <- var_names[idx_breaks]
        
        p <- ggplot(
          plot_df_long,
          aes(
            x = Variable, y = Value,
            group = Sample, color = Class
          )
        ) +
          geom_line(alpha = 0.5) +
          facet_wrap(~Type, ncol = 1, scales = "free_y") +
          labs(
            x = "Variables",
            y = "Intensity"
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks, expand = c(0, 0)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (identical(input$pca_load_profile_type, "bar")) p <- esc_prep_profile_bar(plot_df_long, TRUE, var_breaks)
        plt <- esc_ggplotly(p)
        if (!show_legend) {
          plt <- plt %>% layout(showlegend = FALSE)
        }
        return(plt)
      } else {
        idx_row <- which(ID_chr == selected)
        validate(need(length(idx_row) == 1, "Selected sample not found."))
        
        raw <- X[idx_row, ]
        pre <- Xp[idx_row, ]
        
        plot_df <- data.frame(
          Variable = var_names,
          Raw      = as.numeric(raw),
          Pre      = as.numeric(pre)
        )
        
        plot_df_long <- tidyr::pivot_longer(
          plot_df,
          cols      = c("Raw", "Pre"),
          names_to  = "Type",
          values_to = "Value"
        )
        plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
        
        n_vars <- length(var_names)
        n_breaks <- min(10, n_vars)
        idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
        var_breaks <- var_names[idx_breaks]
        
        p <- ggplot(
          plot_df_long,
          aes(
            x = Variable, y = Value,
            group = Type,
            color = Type
          )
        ) +
          geom_line(alpha = 0.9, linewidth = 1) +
          facet_wrap(~Type, ncol = 1, scales = "free_y") +
          labs(
            x = "Variable",
            y = "Intensity"
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (identical(input$pca_load_profile_type, "bar")) p <- esc_prep_profile_bar(plot_df_long, FALSE, var_breaks)
        plt <- esc_ggplotly(p)
        if (!show_legend) {
          plt <- plt %>% layout(showlegend = FALSE)
        }
        return(plt)
      }
    }
    
    ############################################
    ## 2) Show box plots (new)               ##
    ############################################
    if (input$graph_type_prep == "prep_box") {
      df <- dat_multi()
      rng <- input$multi_var_range
      req(rng)
      
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      # Apply same preprocessing as PCA
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      
      # Variable names
      var_names_full <- colnames(df)[vars_idx]
      p_X <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for box plots."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      colnames(X) <- var_names
      colnames(Xp) <- var_names
      
      # long format: original vs preprocessed
      raw_long <- as.data.frame(X)
      raw_long$Type <- "Raw Data"
      pre_long <- as.data.frame(Xp)
      pre_long$Type <- "Preprocessed"
      
      plot_df <- rbind(raw_long, pre_long)
      
      plot_df_long <- tidyr::pivot_longer(
        plot_df,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      
      # order and thin x\u2011axis labels
      plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
      n_vars <- length(var_names)
      n_breaks <- min(10, n_vars) # show up to 10 labels
      idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
      var_breaks <- var_names[idx_breaks]
      
      
      p <- ggplot(
        plot_df_long,
        aes(x = Variable, y = Value)
      ) +
        geom_boxplot(outlier.size = 0.5, fill = "#3498db", alpha = 0.7) +
        facet_wrap(~Type, ncol = 2, scales = "free_y") +
        scale_x_discrete(breaks = var_breaks, labels = var_breaks) +
        labs(
          x = "Variables",
          y = "Intensity"
        ) +
        theme_minimal() +
        theme(
          axis.text.x  = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(face = "bold")
        )
      
      
      plt <- esc_ggplotly(p)
      if (!show_legend) {
        plt <- plt %>% layout(showlegend = FALSE)
      }
      return(plt)
    }
    
    
    
    ############################################
    ## 3) Scatter plot: Raw vs Preprocessed  ##
    ############################################
    if (input$graph_type_prep == "prep_scatter") {
      df  <- dat_multi()
      rng <- input$multi_var_range
      req(rng)
      
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      # Apply same preprocessing as PCA
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      # Variable names and common columns
      var_names_full <- colnames(df)[vars_idx]
      p_X  <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for scatter plot."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      colnames(X)  <- var_names
      colnames(Xp) <- var_names
      
      # Long format: one row per (Sample, Variable, Type)
      raw_long <- as.data.frame(X)
      pre_long <- as.data.frame(Xp)
      
      raw_long$Sample <- seq_len(nrow(raw_long))
      pre_long$Sample <- seq_len(nrow(pre_long))
      
      raw_long <- tidyr::pivot_longer(
        raw_long,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      raw_long$Type <- "Raw data"
      
      pre_long <- tidyr::pivot_longer(
        pre_long,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      pre_long$Type <- "Preprocessed"
      
      plot_df <- dplyr::bind_rows(raw_long, pre_long)
      
      plot_df$Variable <- factor(plot_df$Variable, levels = var_names)
      
      p <- ggplot(
        plot_df,
        aes(x = Sample, y = Value, color = Variable)
      ) +
        geom_point(alpha = 0.6, size = 1.5) +
        facet_wrap(~Type, ncol = 2, scales = "free_y") +
        labs(
          x = "Sample index",
          y = "Intensity",
          color = "Variable"
        ) +
        theme_minimal()
      
      plt <- esc_ggplotly(p)
      if (!show_legend) {
        plt <- plt %>% layout(showlegend = FALSE)
      }
      return(plt)
    }
    
    
  })
  
  
  
  
  # 3. Multivariate data (for PCA etc.) ------------------------------------
