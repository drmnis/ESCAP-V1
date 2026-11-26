# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s20_plsr  (sourced inside server(), local = TRUE)

  plsr_result <- eventReactive(input$run_plsr, {
    tryCatch(
      {
        cat("[DEBUG] PLSR starting\n")
        
        # 1. Basic data validation
        df <- dat_multi()
        req(input$plsr_y_col, input$multi_var_range)
        
        if (!input$plsr_y_col %in% names(df)) {
          stop("Y column '", input$plsr_y_col, "' not found in data")
        }
        
        # 2. Extract X and Y
        rng <- input$multi_var_range
        vars_idx <- seq(rng[1], rng[2])
        df_vars <- df[, vars_idx, drop = FALSE]
        
        # Coerce to numeric
        X_raw <- as.matrix(data.frame(
          lapply(df_vars, function(z) suppressWarnings(as.numeric(z)))
        ))
        y <- as.numeric(df[[input$plsr_y_col]])
        
        cat("[DEBUG] X_raw dim:", dim(X_raw), "\n")
        cat("[DEBUG] y length:", length(y), "\n")
        
        # 3. Remove incomplete cases
        complete_idx <- complete.cases(X_raw, y)
        X_raw <- X_raw[complete_idx, , drop = FALSE]
        y <- y[complete_idx]
        
        cat("[DEBUG] Complete cases:", nrow(X_raw), "\n")
        
        
        # --- ID handling for PLSR (after 'df <- dat_multi()' and 'complete_idx') ---
        # Get full ID for multivariate data
        if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
            input$multi_id_col %in% names(df)) {
          full_ID <- df[[input$multi_id_col]]
        } else {
          full_ID <- seq_len(nrow(df))
        }
        full_ID_chr <- as.character(full_ID)
        
        # Match IDs with complete cases
        ID_complete <- full_ID_chr[complete_idx]
        
        
        if (nrow(X_raw) < 5) {
          stop("Not enough complete rows for PLSR. Need at least 5, have ", nrow(X_raw))
        }
        
        X <- X_raw   # preprocessing is now done inside esc_plsr()

        
        # 5. Split data into calibration / prediction
        
        if (!is.null(input$show_plsr_sets_section) && !input$show_plsr_sets_section) {
          # User hid/disabled Calibration / prediction sets -> all calibration, no prediction
          n <- nrow(X)
          cal_idx <- seq_len(n)
          pred_idx <- integer(0)
        } else if (input$plsr_split_type == "column") {
          req(input$plsr_group_col)
          g <- df[[input$plsr_group_col]][complete_idx]
          g <- factor(g)
          
          if (nlevels(g) != 2) {
            stop("Group column must have exactly two levels. Found: ", paste(levels(g), collapse = ", "))
          }
          
          lev <- levels(g)
          cal_idx <- which(g == lev[1])
          pred_idx <- which(g == lev[2])
        } else {
          set.seed(123)
          n <- nrow(X)
          cal_n <- round(n * input$plsr_cal_prop)
          idx_all <- sample(seq_len(n))
          cal_idx <- idx_all[1:cal_n]
          pred_idx <- idx_all[(cal_n + 1):n]
        }
        
        
        # 5b. Class (if available) for coloring plots
        cal_class <- NULL
        pred_class <- NULL
        if (!is.null(input$multi_class_col) &&
            input$multi_class_col != "" &&
            input$multi_class_col %in% names(df)) {
          full_class <- factor(df[[input$multi_class_col]][complete_idx])
          cal_class <- full_class[cal_idx]
          pred_class <- full_class[pred_idx]
        }
        
        
        cat("[DEBUG] Calibration set size:", length(cal_idx), "\n")
        cat("[DEBUG] Prediction set size:", length(pred_idx), "\n")
        
        if (length(cal_idx) < 5) {
          stop("Calibration set too small. Need at least 5, have ", length(cal_idx))
        }
        
        X_cal <- X[cal_idx, , drop = FALSE]
        y_cal <- y[cal_idx]
        X_pred <- X[pred_idx, , drop = FALSE]
        y_pred <- y[pred_idx]
        
        cal_id <- ID_complete[cal_idx]
        pred_id <- ID_complete[pred_idx]
        
        # 6-12. Model with the validated ESCAP engine (R/chemometrics.R).
        #       Preprocessing is fitted on the calibration set only and re-fitted
        #       inside every CV fold; the prediction set uses calibration parameters.
        X_cal_raw <- X_raw[cal_idx, , drop = FALSE]
        X_pred_raw <- X_raw[pred_idx, , drop = FALSE]
        user_lv <- suppressWarnings(as.integer(input$plsr_lv_user))
        if (length(user_lv) == 0 || is.na(user_lv)) user_lv <- 0L
        max_possible_lv <- min(nrow(X_cal_raw) - 2, ncol(X_cal_raw))
        if (max_possible_lv < 1) stop("Too few calibration samples for PLSR.")
        max_lv <- min(max(10L, user_lv), max_possible_lv, 20L)
        if (user_lv > max_lv) {
          showNotification(paste0("Requested LV (", user_lv, ") exceeds what is possible (", max_lv, "). Using LV = ", max_lv, "."),
                           type = "warning", duration = 6)
          user_lv <- max_lv
        }
        cv_type <- input$plsr_cv_type %||% "vb"
        cv <- list(type = cv_type,
                   k = max(2, min(25, input$plsr_cv_splits %||% 5)),
                   thickness = max(1, min(20, input$plsr_cv_thickness %||% 1)),
                   seed = 123)
        prep_args <- list(spectral = input$prep_spectral, scaling = input$prep_scaling,
                          centering = input$prep_centering, sg_width = input$sg_width %||% 11,
                          sg_order = input$sg_order %||% 2, sg_deriv = input$sg_deriv %||% 0)
        eng <- esc_plsr(X_cal_raw, y_cal, X_pred_raw, y_pred, prep_args = prep_args,
                        max_lv = max_lv, cv = cv, lv = if (user_lv > 0) user_lv else NULL)
        X_cal <- eng$X_cal_prep
        X_pred <- if (is.null(eng$X_pred_prep)) X_cal[0, , drop = FALSE] else eng$X_pred_prep
        lv_for_params <- eng$lv
        p <- eng$perf
        r4 <- function(x) round(x, 4)
        perf_df <- data.frame(
          LV = paste0("LV-", p$LV), RMSEC = r4(p$RMSEC), RMSECV = r4(p$RMSECV), RMSEP = r4(p$RMSEP),
          `Bias (Cal)` = r4(p$BiasC), `Bias (CV)` = r4(p$BiasCV), `Bias (Pred)` = r4(p$BiasP),
          `R² Cal` = r4(p$R2C), `R² CV` = r4(p$R2CV), `R² Pred` = r4(p$R2P),
          SEC = r4(p$SEC), SECV = r4(p$SECV), SEP = r4(p$SEP),
          `RPD (CV)` = r4(p$RPD_CV), `RPD (Pred)` = r4(p$RPD_P),
          check.names = FALSE, stringsAsFactors = FALSE)
        bv <- eng$block_var
        block_var_df <- data.frame(
          LV = paste0("LV-", bv$LV), `X-block LV (%)` = round(bv$X_pct, 2),
          `X-block Cumulative (%)` = round(bv$X_cum, 2), `Y-block LV (%)` = round(bv$Y_pct, 2),
          `Y-block Cumulative (%)` = round(bv$Y_cum, 2), check.names = FALSE)
        var_names_cal <- colnames(X_cal) %||% paste0("X", seq_len(ncol(X_cal)))
        b <- stats::setNames(as.numeric(eng$coef), var_names_cal)
        vip_vals <- stats::setNames(as.numeric(eng$vip), var_names_cal)
        sr_vals <- stats::setNames(as.numeric(eng$sr), var_names_cal)
        plsr_fit <- eng$fit
        state$current_analysis <- "plsr"

        list(
          fit        = plsr_fit,
          engine     = eng,
          X_cal      = X_cal,
          y_cal      = y_cal,
          X_pred     = X_pred,
          y_pred     = y_pred,
          cal_class  = cal_class,
          pred_class = pred_class,
          cal_id     = cal_id,
          pred_id    = pred_id,
          max_lv     = eng$max_lv,
          lv_index   = seq_len(eng$max_lv),
          perf       = perf_df,
          coef       = b,
          intercept  = eng$intercept,
          vip        = vip_vals,
          sr         = sr_vals,
          Q          = eng$qt$Q,
          T2         = eng$qt$T2,
          Q_lim      = eng$qt$Q_lim,
          T2_lim     = eng$qt$T2_lim,
          y_cal_hat  = eng$yhat_cal[, lv_for_params],
          y_cv_hat   = eng$yhat_cv[, lv_for_params],
          y_pred_hat = if (is.null(eng$yhat_pred)) numeric(0) else eng$yhat_pred[, lv_for_params],
          chosen_lv  = lv_for_params,
          prep_label = esc_prep_label(input$prep_spectral, input$prep_scaling, input$prep_centering),
          block_var  = block_var_df
        )
      },
      error = function(e) {
        cat("[ERROR] PLSR failed:", conditionMessage(e), "\n")
        showNotification(paste("PLSR:", conditionMessage(e)), type = "error", duration = 10)
        list(
          fit = NULL,
          X_cal = NULL,
          y_cal = NULL,
          X_pred = NULL,
          y_pred = NULL,
          cal_class = NULL,
          pred_class = NULL,
          cal_id = character(0),
          pred_id = character(0),
          max_lv = 0,
          lv_index = numeric(0),
          perf = data.frame(
            LV               = character(0),
            RMSEC            = numeric(0),
            RMSECV           = numeric(0),
            RMSEP            = numeric(0),
            `Bias (Cal)`     = numeric(0),
            `Bias (CV)`      = numeric(0),
            `Bias (Pred)`    = numeric(0),
            `R² Cal`         = numeric(0),
            `R² CV`          = numeric(0),
            `R² Pred`        = numeric(0),
            SEP              = numeric(0),
            stringsAsFactors = FALSE
          ),
          block_var = data.frame(
            LV                       = character(0),
            `X-block LV (%)`         = numeric(0),
            `X-block Cumulative (%)` = numeric(0),
            `Y-block LV (%)`         = numeric(0),
            `Y-block Cumulative (%)` = numeric(0),
            check.names              = FALSE
          ),
          coef = numeric(0),
          vip = numeric(0),
          sr = numeric(0),
          Q = numeric(0),
          T2 = numeric(0),
          y_cal_hat = numeric(0),
          y_cv_hat = numeric(0),
          y_pred_hat = numeric(0),
          chosen_lv = NA_integer_,
          error = conditionMessage(e)
        )
      }
    )
  })
  
  
  # --- View controllers for PCA tables ---
