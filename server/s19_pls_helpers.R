# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s19_pls_helpers  (sourced inside server(), local = TRUE)

  vip_pls <- function(pls_obj, ncomp = NULL) {
    if (is.null(ncomp)) ncomp <- pls_obj$ncomp
    
    # Ensure ncomp does not exceed what is in the object
    max_comp <- min(
      ncomp,
      pls_obj$ncomp,
      ncol(pls_obj$loading.weights)
    )
    if (max_comp < 1) {
      stop("vip_pls: no components available to compute VIP.")
    }
    ncomp <- max_comp
    
    W <- pls_obj$loading.weights[, 1:ncomp, drop = FALSE]
    T <- pls_obj$scores[, 1:ncomp, drop = FALSE]
    
    coef_array <- coef(pls_obj)
    if (length(dim(coef_array)) == 3) {
      # (nvar, nresp, ncomp)
      b_coef <- coef_array[1, 1, 1:ncomp]
    } else if (length(dim(coef_array)) == 2) {
      # (nvar, ncomp)
      b_coef <- coef_array[1, 1:ncomp]
    } else {
      b_coef <- rep(1, ncomp)
    }
    
    SSY <- colSums(T^2) * (b_coef^2)
    SSY_tot <- sum(SSY)
    if (SSY_tot == 0) SSY_tot <- 1
    
    p <- nrow(W)
    VIP <- sqrt(p * (W^2 %*% SSY / SSY_tot))
    as.numeric(VIP)
  }
  
  
  # Helper: simple selectivity ratio
  selectivity_ratio_pls <- function(X, y, pls_obj, ncomp = NULL) {
    if (is.null(ncomp)) ncomp <- pls_obj$ncomp
    
    coef_array <- coef(pls_obj)
    if (length(dim(coef_array)) == 3) {
      max_comp <- dim(coef_array)[3]
    } else if (length(dim(coef_array)) == 2) {
      max_comp <- ncol(coef_array)
    } else {
      max_comp <- 1
    }
    
    ncomp <- min(ncomp, pls_obj$ncomp, max_comp)
    if (ncomp < 1) {
      stop("selectivity_ratio_pls: no components available.")
    }
    
    yhat <- as.numeric(pls::predict(pls_obj, ncomp = ncomp))
    e <- y - yhat
    
    if (length(dim(coef_array)) == 3) {
      b <- coef_array[, 1, ncomp]
    } else if (length(dim(coef_array)) == 2) {
      b <- coef_array[, ncomp]
    } else {
      stop("Unexpected coef() dimensions in selectivity_ratio_pls.")
    }
    
    signal <- sweep(X, 2, b, `*`)
    SR <- apply(signal, 2, var, na.rm = TRUE) / var(e, na.rm = TRUE)
    as.numeric(SR)
  }
  
  
  plsr_show <- reactiveValues(
    var = FALSE, # X/Y block variance
    par = FALSE, # parameters (already used)
    qt  = FALSE # Q & T2
  )
  observeEvent(input$plsr_var_show, {
    plsr_show$var <- TRUE
  })
  observeEvent(input$plsr_var_hide, {
    plsr_show$var <- FALSE
  })
  observeEvent(input$plsr_par_show, {
    plsr_show$par <- TRUE
  })
  observeEvent(input$plsr_par_hide, {
    plsr_show$par <- FALSE
  })
  observeEvent(input$plsr_qt_show, {
    plsr_show$qt <- TRUE
  })
  observeEvent(input$plsr_qt_hide, {
    plsr_show$qt <- FALSE
  })
  
  
  ################################### PLSR Result Block (REPLACED) #############################
  
