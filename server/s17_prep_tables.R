# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s17_prep_tables  (sourced inside server(), local = TRUE)

  prep_data <- reactive({
    df <- dat_multi()
    rng <- input$multi_var_range
    req(rng)
    
    vars_idx <- seq(rng[1], rng[2])
    df_vars <- df[, vars_idx, drop = FALSE]
    
    X_num <- as.data.frame(
      lapply(df_vars, function(col) suppressWarnings(as.numeric(col)))
    )
    X_mat <- as.matrix(X_num)
    
    good_cols <- which(colSums(is.na(X_mat)) == 0)
    validate(
      need(
        length(good_cols) > 0,
        "No complete numeric variables available in the selected range."
      )
    )
    
    X <- X_mat[, good_cols, drop = FALSE]
    vars_idx_final <- vars_idx[good_cols]
    var_names <- colnames(df)[vars_idx_final]
    
    Xp <- pca_preprocess(
      X,
      spectral_steps = input$prep_spectral,
      scaling_step   = input$prep_scaling,
      centering_step = input$prep_centering,
      sg_width       = input$sg_width,
      sg_order       = input$sg_order,
      sg_deriv       = input$sg_deriv
    )
    
    
    Xp <- as.matrix(Xp)
    
    # Only set names if lengths match
    if (length(var_names) == ncol(Xp)) {
      colnames(Xp) <- var_names
    }
    
    Xp
  })
  
  
  output$prep_steps_text <- renderText({
    req(input$file_multi)
    # Spectral part
    spec <- switch(input$prep_spectral %||% "none",
                   "snv" = "SNV",
                   "msc" = "MSC",
                   "sg" = "Savitzky\u2013Golay",
                   "none" = NULL
    )
    
    # Scaling part
    scal <- switch(input$prep_scaling %||% "none",
                   "iqr"    = "IQR",
                   "range"  = "Range",
                   "pareto" = "Pareto",
                   "maxmin" = "Max\u2013min",
                   "log"    = "Log",
                   "auto"   = "Auto",
                   "none"   = NULL
    )
    
    # Centering part
    cent <- switch(input$prep_centering %||% "none",
                   "mc"     = "Mean",
                   "median" = "Median",
                   "none"   = NULL
    )
    
    steps <- c(spec, scal, cent)
    if (length(steps) == 0) {
      return("Preprocessing steps: None")
    }
    paste("Preprocessing steps (applied in order):", paste(steps, collapse = "  \u2192  "))
  })
  
  
  # (duplicate definition of output$prep_data_table removed; the later one is used)
  
  
  # what part of the matrix to show
  prep_view <- reactiveVal(list(nrows = 20L, ncols = 15L))
  
  observeEvent(input$prep_show_all, {
    prep_view(list(nrows = Inf, ncols = Inf))
  })
  
  observeEvent(input$prep_hide, {
    prep_view(list(nrows = 20L, ncols = 15L))
  })
  
  output$prep_view_info <- renderText({
    Xp <- prep_data()
    v <- prep_view()
    if (is.null(Xp)) {
      return("")
    }
    
    total_r <- nrow(Xp)
    total_c <- ncol(Xp)
    
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- if (is.infinite(v$ncols)) total_c else min(v$ncols, total_c)
    
    if (show_r == total_r && show_c == total_c) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d rows and first %d of %d columns.",
        show_r, total_r, show_c, total_c
      )
    }
  })
  
  tbl_prep_data_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      Xp <- prep_data()
      if (is.null(Xp)) {
        return(NULL)
      }
      
      v <- prep_view()
      
      nr <- nrow(Xp)
      nc <- ncol(Xp)
      
      r_show <- if (is.infinite(v$nrows)) nr else min(v$nrows, nr)
      c_show <- if (is.infinite(v$ncols)) nc else min(v$ncols, nc)
      
      Xp[seq_len(r_show), seq_len(c_show), drop = FALSE]
    }
  output$prep_data_table <- renderTable(tbl_prep_data_table(),
    rownames = TRUE
  )
  
  
