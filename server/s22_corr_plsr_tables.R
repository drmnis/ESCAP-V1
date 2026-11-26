# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s22_corr_plsr_tables  (sourced inside server(), local = TRUE)

  corr_spear_view <- reactiveVal(list(nrows = 20L))
  corr_pearson_view <- reactiveVal(list(nrows = 20L))
  
  observeEvent(input$corr_spear_show, {
    corr_spear_view(list(nrows = Inf))
  })
  observeEvent(input$corr_spear_hide, {
    corr_spear_view(list(nrows = 20L))
  })
  observeEvent(input$corr_pearson_show, {
    corr_pearson_view(list(nrows = Inf))
  })
  observeEvent(input$corr_pearson_hide, {
    corr_pearson_view(list(nrows = 20L))
  })
  
  output$corr_spear_view_info <- renderText({
    res <- corr_result()
    req(res)
    mat <- res$r_spearman
    total_r <- nrow(mat)
    total_c <- ncol(mat)
    v <- corr_spear_view()
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- show_r # square matrix so same
    
    if (show_r == total_r) {
      sprintf("Showing all %d variables (matrix is %d \u00d7 %d).", total_r, total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d variables (submatrix %d \u00d7 %d).",
        show_r, total_r, show_r, show_c
      )
    }
  })
  
  output$corr_pearson_view_info <- renderText({
    res <- corr_result()
    req(res)
    mat <- res$r_pearson
    total_r <- nrow(mat)
    total_c <- ncol(mat)
    v <- corr_pearson_view()
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- show_r
    
    if (show_r == total_r) {
      sprintf("Showing all %d variables (matrix is %d \u00d7 %d).", total_r, total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d variables (submatrix %d \u00d7 %d).",
        show_r, total_r, show_r, show_c
      )
    }
  })
  
  
  # r with significance stars (two-sided t-test of r = 0, n - 2 df; unadjusted)
  corr_fmt <- function(r, P) {
    st <- ifelse(is.na(P), "", ifelse(P < 0.001, "***", ifelse(P < 0.01, "**", ifelse(P < 0.05, "*", ""))))
    m <- matrix(paste0(formatC(r, format = "f", digits = 3), st), nrow(r), dimnames = dimnames(r))
    diag(m) <- "1"
    m
  }

  # Spearman (existing)
  tbl_corr_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- corr_result()
      req(res)
      mat <- corr_fmt(res$r_spearman, res$P_spearman)
      v <- corr_spear_view()
      total_r <- nrow(mat)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      mat[seq_len(show_r), seq_len(show_r), drop = FALSE]
    }
  output$corr_table <- renderTable(tbl_corr_table(),
    rownames = TRUE
  )
  
  tbl_corr_table_pearson <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- corr_result()
      req(res)
      mat <- corr_fmt(res$r_pearson, res$P_pearson)
      v <- corr_pearson_view()
      total_r <- nrow(mat)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      mat[seq_len(show_r), seq_len(show_r), drop = FALSE]
    }
  output$corr_table_pearson <- renderTable(tbl_corr_table_pearson(),
    rownames = TRUE
  )
  
  
  # --- View info for PLSR tables (rows only) ---
  output$plsr_var_view_info <- renderText({
    res <- plsr_result()
    if (is.null(res) || !is.null(res$error)) {
      return("")
    }
    tab <- res$block_var
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    show_all <- isTRUE(plsr_show$var)
    show_r <- if (show_all) total_r else min(10L, total_r)
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf("Showing first %d of %d rows and all %d columns.", show_r, total_r, total_c)
    }
  })
  
  output$plsr_perf_view_info <- renderText({
    res <- plsr_result()
    if (is.null(res) || !is.null(res$error) || is.null(res$perf)) {
      return("")
    }
    tab <- res$perf
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    # performance table always shows all rows (no show/hide buttons)
    sprintf("Showing all %d rows and %d columns.", total_r, total_c)
  })
  
  output$plsr_par_view_info <- renderText({
    res <- plsr_result()
    if (is.null(res) || !is.null(res$error)) {
      return("")
    }
    tab <- data.frame(
      Variable         = names(res$coef),
      Coefficient      = as.numeric(res$coef),
      VIP              = as.numeric(res$vip),
      SelectivityRatio = as.numeric(res$sr)
    )
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    show_all <- isTRUE(plsr_show$par)
    show_r <- if (show_all) total_r else min(15L, total_r)
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf("Showing first %d of %d rows and all %d columns.", show_r, total_r, total_c)
    }
  })
  
  output$plsr_qt_view_info <- renderText({
    res <- plsr_result()
    if (is.null(res) || !is.null(res$error)) {
      return("")
    }
    n <- length(res$Q)
    if (n == 0) {
      return("")
    }
    tab <- data.frame(
      Sample = seq_len(n),
      Q      = res$Q,
      T2     = res$T2
    )
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    show_all <- isTRUE(plsr_show$qt)
    show_r <- if (show_all) total_r else min(20L, total_r)
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf("Showing first %d of %d rows and all %d columns.", show_r, total_r, total_c)
    }
  })
  
  
  ### ADD HERE (PLSR) \u2013 performance & parameter tables
  
  
  tbl_plsr_var_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- plsr_result()
      if (is.null(res) || !is.null(res$error)) {
        return(data.frame(
          LV                       = "Error",
          `X-block LV (%)`         = NA,
          `X-block Cumulative (%)` = NA,
          `Y-block LV (%)`         = NA,
          `Y-block Cumulative (%)` = NA,
          check.names              = FALSE
        ))
      }
      tab <- res$block_var
      if (!isTRUE(plsr_show$var)) {
        tab <- head(tab, 10)
      }
      tab
    }
  output$plsr_var_table <- renderTable(tbl_plsr_var_table(),
    rownames = FALSE
  )
  
  
  tbl_plsr_perf_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- plsr_result()
      
      # Check if PLSR ran successfully
      if (is.null(res) || !is.null(res$error)) {
        error_msg <- if (!is.null(res$error)) res$error else "PLSR analysis failed to run"
        return(data.frame(
          Metric = "Error",
          Value  = error_msg
        ))
      }
      
      # Check if performance data exists
      if (is.null(res$perf) || nrow(res$perf) == 0) {
        return(data.frame(
          Metric = "Status",
          Value  = "No performance metrics available"
        ))
      }
      
      # Return the performance table
      res$perf
    }
  output$plsr_perf_table <- renderTable(tbl_plsr_perf_table(),
    rownames = FALSE
  )
  
  
  tbl_plsr_par_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- plsr_result()
      
      # Check if PLSR ran successfully
      if (is.null(res) || !is.null(res$error)) {
        error_msg <- if (!is.null(res$error)) res$error else "PLSR analysis failed to run"
        return(data.frame(
          Variable         = "Error",
          Coefficient      = error_msg,
          VIP              = NA,
          SelectivityRatio = NA
        ))
      }
      
      # Check if coefficients exist
      if (length(res$coef) == 0 || all(is.na(res$coef))) {
        return(data.frame(
          Variable         = "Status",
          Coefficient      = "No coefficients available",
          VIP              = NA,
          SelectivityRatio = NA
        ))
      }
      
      # Create parameter table
      vars <- names(res$coef)
      
      # Handle VIP and SR - they might be NA even if coefficients exist
      vip_vals <- if (length(res$vip) == length(vars)) res$vip[vars] else rep(NA, length(vars))
      sr_vals <- if (length(res$sr) == length(vars)) res$sr[vars] else rep(NA, length(vars))
      
      tab <- data.frame(
        Variable         = vars,
        Coefficient      = as.numeric(res$coef[vars]),
        VIP              = as.numeric(vip_vals),
        SelectivityRatio = as.numeric(sr_vals)
      )
      
      # Round values
      tab$Coefficient <- round(tab$Coefficient, 4)
      tab$VIP <- round(tab$VIP, 3)
      tab$SelectivityRatio <- round(tab$SelectivityRatio, 3)
      
      # Limit display if not showing all
      if (!isTRUE(plsr_show$par)) {
        tab <- head(tab, 15)
      }
      
      tab
    }
  output$plsr_par_table <- renderTable(tbl_plsr_par_table(),
    rownames = FALSE
  )
  
  
  tbl_plsr_qt_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      res <- plsr_result()
      if (is.null(res) || !is.null(res$error)) {
        error_msg <- if (!is.null(res$error)) res$error else "PLSR analysis failed to run"
        return(data.frame(
          Sample = "Error",
          Q      = error_msg,
          T2     = NA
        ))
      }
      if (length(res$Q) == 0 || length(res$T2) == 0) {
        return(data.frame(
          Sample = integer(0),
          Q      = numeric(0),
          T2     = numeric(0)
        ))
      }
      n <- length(res$Q)
      tab <- data.frame(
        Sample = seq_len(n),
        Q      = round(res$Q, 4),
        T2     = round(res$T2, 4)
      )
      if (!isTRUE(plsr_show$qt)) {
        tab <- head(tab, 20)
      }
      tab
    }
  output$plsr_qt_table <- renderTable(tbl_plsr_qt_table(),
    rownames = FALSE
  )
  
  
  observeEvent(plotly::event_data("plotly_click", source = "plsr_qt"), ignoreNULL = TRUE, {
    ed <- plotly::event_data("plotly_click", source = "plsr_qt")
    if (is.null(ed) || nrow(ed) == 0) return()
    
    clicked_id <- as.character(ed$key[1])
    if (is.na(clicked_id) || clicked_id == "") return()
    
    current <- plsr_qt_selected_ids()
    if (clicked_id %in% current) {
      plsr_qt_selected_ids(setdiff(current, clicked_id))
    } else {
      plsr_qt_selected_ids(c(current, clicked_id))
    }
  })
  
  
  observeEvent(plotly::event_data("plotly_selected", source = "plsr_qt"), ignoreNULL = TRUE, {
    ed <- plotly::event_data("plotly_selected", source = "plsr_qt")
    if (is.null(ed) || nrow(ed) == 0) return()
    new_ids <- as.character(ed$key)
    new_ids <- new_ids[!is.na(new_ids)]
    current <- plsr_qt_selected_ids()
    plsr_qt_selected_ids(unique(c(current, new_ids)))
  })
  
  
  observeEvent(input$plsr_qt_delete_selected, {
    req(state$current_data_source == "multi", !is.null(input$file_multi))
    res <- plsr_result()
    req(!is.null(res$fit), is.null(res$error))
    
    sel_ids <- unique(plsr_qt_selected_ids())
    if (length(sel_ids) == 0) {
      showNotification("Click (one or more) sample(s) in the Q vs T\u00b2 plot first.",
                       type = "error", duration = 4)
      return()
    }
    
    df <- dat_multi()
    if (nrow(df) == 0) return()
    
    if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
        input$multi_id_col %in% names(df)) {
      full_ID <- as.character(df[[input$multi_id_col]])
    } else {
      full_ID <- as.character(seq_len(nrow(df)))
    }
    
    idx <- which(full_ID %in% sel_ids)
    if (!length(idx)) {
      showNotification("Selected ID(s) do not match any current data row.",
                       type = "error", duration = 4)
      return()
    }
    
    keep <- data_store$multi_keep
    if (is.null(keep) || length(keep) < max(idx)) return()
    
    data_store$multi_keep <- keep[-idx]
    plsr_qt_selected_ids(character(0))  # clear selection
    
    showNotification(
      paste("Deleted", length(idx), "sample(s) from multivariate data (via Q vs T\u00b2)."),
      type = "message", duration = 4
    )
  })
  
  
  observeEvent(input$plsr_qt_clear_selection, {
    plsr_qt_selected_ids(character(0))
  })
  
  
  # 4. Univariate ANOVA + Tukey --------------------------------------------
