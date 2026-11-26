# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s16_pca  (sourced inside server(), local = TRUE)

  pca_result <- eventReactive(input$run_pca, {
    df <- dat_multi()
    rng <- input$multi_var_range
    req(rng)
    
    # Initial variable indices from slider
    vars_idx <- seq(rng[1], rng[2])
    
    # Extract candidate variable columns
    df_vars <- df[, vars_idx, drop = FALSE]
    
    # Coerce all to numeric, column-wise
    X_num <- as.data.frame(
      lapply(df_vars, function(col) suppressWarnings(as.numeric(col)))
    )
    X_mat <- as.matrix(X_num)
    
    # Identify columns that are fully numeric and have no NA
    good_cols <- which(colSums(is.na(X_mat)) == 0)
    bad_cols <- setdiff(seq_along(vars_idx), good_cols)
    
    if (length(good_cols) == 0) {
      validate(need(FALSE, "No complete numeric variables available for PCA in the selected range."))
    }
    
    # Subset to good columns only
    X <- X_mat[, good_cols, drop = FALSE]
    vars_idx_final <- vars_idx[good_cols]
    
    # Optional: notify user about dropped variables
    if (length(bad_cols) > 0) {
      n_drop <- length(bad_cols)
      showNotification(
        paste0("Dropped ", n_drop, " non-numeric or incomplete variables before PCA."),
        type     = "message",
        duration = 5
      )
    }
    
    
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
    
    # Drop zero-variance (constant) columns after preprocessing
    Xp <- as.matrix(Xp)
    # Savitzky-Golay removes edge variables: keep indices aligned with surviving columns
    if (!is.null(colnames(Xp)) && !is.null(colnames(X))) {
      vars_idx_final <- vars_idx_final[match(colnames(Xp), colnames(X))]
    }
    col_var <- apply(Xp, 2, var, na.rm = TRUE)
    good_var <- is.finite(col_var) & (col_var > 0)
    if (!any(good_var)) {
      validate(need(FALSE,
                    "PCA: all selected variables have zero variance after preprocessing. Please adjust your selection or preprocessing."
      ))
    }
    if (!all(good_var)) {
      n_drop_const <- sum(!good_var)
      showNotification(
        paste0(
          "PCA: dropped ", n_drop_const,
          " constant/zero-variance variable(s) after preprocessing."
        ),
        type = "message", duration = 6
      )
      Xp <- Xp[, good_var, drop = FALSE]
      # also update vars_idx_final to keep only surviving variables
      vars_idx_final <- vars_idx_final[good_var]
    }
    
    # ID (always character; serials if no ID column)
    if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
        input$multi_id_col %in% names(df)) {
      ID <- df[[input$multi_id_col]]
    } else {
      ID <- seq_len(nrow(df))
    }
    ID_chr <- as.character(ID)
    
    # Class
    if (!is.null(input$multi_class_col) && input$multi_class_col != "" &&
        input$multi_class_col %in% names(df)) {
      Class <- factor(df[[input$multi_class_col]])
    } else {
      Class <- factor(rep("All", nrow(df)))
    }
    
    # Run PCA on preprocessed numeric matrix
    pca <- prcomp(Xp, center = FALSE, scale. = FALSE)
    
    
    state$current_analysis <- "pca"
    
    list(
      pca       = pca,
      ID        = ID_chr, # now defined
      Class     = Class,
      var_idx   = vars_idx_final,
      var_names = colnames(df)[vars_idx_final]
    )
  })
  
  #After PCA is computed, update the slider\u2019s maximum
  observeEvent(pca_result(), {
    res <- pca_result()
    n_pc <- ncol(res$pca$x)
    
    # allow up to 10 PCs, or fewer if data has fewer
    max_pc <- max(2, min(n_pc, 10))
    # current value should not exceed max_pc
    cur_val <- input$pca_n_keep %||% 3
    if (is.na(cur_val) || cur_val < 2) cur_val <- 2
    if (cur_val > max_pc) cur_val <- max_pc
    
    updateSliderInput(
      session,
      "pca_n_keep",
      min   = 2,
      max   = max_pc,
      value = cur_val
    )
  })
  
  
  
  # Toggle PCA selection on each click
  observeEvent(plotly::event_data("plotly_click", source = "pca_scores"), ignoreNULL = TRUE, {
    ed <- plotly::event_data("plotly_click", source = "pca_scores")
    if (is.null(ed) || nrow(ed) == 0) return()
    
    # we mapped key = ID in the PCA score plot
    clicked_id <- as.character(ed$key[1])
    if (is.na(clicked_id) || clicked_id == "") return()
    
    current <- pca_selected_ids()
    if (clicked_id %in% current) {
      # toggle OFF
      pca_selected_ids(setdiff(current, clicked_id))
    } else {
      # toggle ON
      pca_selected_ids(c(current, clicked_id))
    }
  })
  
  observeEvent(plotly::event_data("plotly_selected", source = "pca_scores"), ignoreNULL = TRUE, {
    ed <- plotly::event_data("plotly_selected", source = "pca_scores")
    if (is.null(ed) || nrow(ed) == 0) return()
    new_ids <- as.character(ed$key)
    new_ids <- new_ids[!is.na(new_ids)]
    current <- pca_selected_ids()
    pca_selected_ids(unique(c(current, new_ids)))
  })
  
  observeEvent(input$pca_delete_selected, {
    req(state$current_data_source == "multi", !is.null(input$file_multi))
    
    sel_ids <- unique(pca_selected_ids())
    if (length(sel_ids) == 0) {
      showNotification("Click (one or more) sample(s) in the PCA score plot first.",
                       type = "error", duration = 4)
      return()
    }
    
    df <- dat_multi()
    if (nrow(df) == 0) return()
    
    # reconstruct IDs exactly as in pca_result()
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
    pca_selected_ids(character(0))  # clear selection after delete
    
    showNotification(
      paste("Deleted", length(idx), "sample(s) from multivariate data (via PCA scores)."),
      type = "message", duration = 4
    )
  })
  
  
  observeEvent(input$pca_clear_selection, {
    pca_selected_ids(character(0))
  })
  
  
  
  
  # Preprocessed X\u2011matrix (for Results -> Preprocessing tab)
