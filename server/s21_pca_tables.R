# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s21_pca_tables  (sourced inside server(), local = TRUE)

  pca_eig_view <- reactiveVal(list(nrows = 10L))
  pca_load_view <- reactiveVal(list(nrows = 10L))
  pca_scores_view <- reactiveVal(list(nrows = 10L))
  
  observeEvent(input$pca_eig_show, {
    pca_eig_view(list(nrows = Inf))
  })
  observeEvent(input$pca_eig_hide, {
    pca_eig_view(list(nrows = 10L))
  })
  observeEvent(input$pca_load_show, {
    pca_load_view(list(nrows = Inf))
  })
  observeEvent(input$pca_load_hide, {
    pca_load_view(list(nrows = 10L))
  })
  observeEvent(input$pca_scores_show_all_btn, {
    pca_scores_view(list(nrows = Inf))
  })
  observeEvent(input$pca_scores_hide_btn, {
    pca_scores_view(list(nrows = 10L))
  })
  
  output$pca_eig_view_info <- renderText({
    res <- pca_result()
    if (is.null(res)) {
      return("")
    }
    s <- res$pca$sdev
    var <- s^2
    prop <- var / sum(var)
    tab <- data.frame(
      PC                       = paste0("PC", seq_along(s)),
      Eigenvalue               = round(var, 3),
      `Explained variance (%)` = round(prop * 100, 2),
      check.names              = FALSE
    )
    v <- pca_eig_view()
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- total_c
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d rows and first %d of %d columns.",
        show_r, total_r, show_c, total_c
      )
    }
  })
  
  output$pca_load_view_info <- renderText({
    res <- pca_result()
    if (is.null(res)) {
      return("")
    }
    load <- res$pca$rotation
    k <- min(3, ncol(load))
    tab <- as.data.frame(load[, 1:k, drop = FALSE])
    tab <- cbind(Variable = rownames(load), tab)
    v <- pca_load_view()
    total_r <- nrow(tab)
    total_c <- ncol(tab)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- total_c
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d rows and first %d of %d columns.",
        show_r, total_r, show_c, total_c
      )
    }
  })
  
  output$pca_scores_view_info <- renderText({
    res <- pca_result()
    if (is.null(res)) {
      return("")
    }
    scores <- as.data.frame(res$pca$x)
    scores <- cbind(
      ID    = res$ID,
      Class = as.character(res$Class),
      round(scores, 3)
    )
    v <- pca_scores_view()
    total_r <- nrow(scores)
    total_c <- ncol(scores)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- total_c
    if (show_r == total_r) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d rows and first %d of %d columns.",
        show_r, total_r, show_c, total_c
      )
    }
  })
  
  
  # PCA eigenvalues and explained variance ---------------------------------
  tbl_pca_eigenvalues <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      if (is.null(pca_result())) {
        return(NULL)
      }
      res <- pca_result()
      s <- res$pca$sdev
      var <- s^2
      prop <- var / sum(var)
      tab <- data.frame(
        PC                       = paste0("PC", seq_along(s)),
        Eigenvalue               = round(var, 3),
        `Explained variance (%)` = round(prop * 100, 2),
        check.names              = FALSE
      )
      v <- pca_eig_view()
      total_r <- nrow(tab)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      tab[seq_len(show_r), , drop = FALSE]
    }
  output$pca_eigenvalues <- renderTable(tbl_pca_eigenvalues(),
    rownames = FALSE
  )
  
  
  # PCA loadings (first few PCs) -------------------------------------------
  tbl_pca_loadings <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      if (is.null(pca_result())) {
        return(NULL)
      }
      res <- pca_result()
      load <- res$pca$rotation
      k <- min(3, ncol(load))
      tab <- as.data.frame(load[, 1:k, drop = FALSE])
      tab <- round(tab, 3)
      tab <- cbind(Variable = rownames(load), tab)
      rownames(tab) <- NULL
      v <- pca_load_view()
      total_r <- nrow(tab)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      tab[seq_len(show_r), , drop = FALSE]
    }
  output$pca_loadings <- renderTable(tbl_pca_loadings(),
    rownames = FALSE
  )
  
  
  # PCA scores (PC coordinates of each observation) ------------------------
  tbl_pca_scores <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      if (is.null(pca_result())) {
        return(NULL)
      }
      res <- pca_result()
      scores <- as.data.frame(res$pca$x)
      scores <- cbind(
        ID    = res$ID,
        Class = as.character(res$Class),
        round(scores, 3)
      )
      v <- pca_scores_view()
      total_r <- nrow(scores)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      scores[seq_len(show_r), , drop = FALSE]
    }
  output$pca_scores <- renderTable(tbl_pca_scores(),
    rownames = FALSE
  )
  
  
  # --- View controllers for correlation tables ---
