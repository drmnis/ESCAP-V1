# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s03_epi_helpers  (sourced inside server(), local = TRUE)

  chi_apply_references <- function(df, predictors) {
    for (v in predictors) {
      if (!v %in% names(df)) next
      if (!is.factor(df[[v]])) df[[v]] <- factor(df[[v]])
      inp_id <- paste0("chi_ref_", v)
      ref_choice <- isolate(input[[inp_id]])
      if (!is.null(ref_choice) && ref_choice %in% levels(df[[v]])) {
        df[[v]] <- stats::relevel(df[[v]], ref = ref_choice)
      }
    }
    df
  }
  
  
  sci_to_sup <- function(x) {
    # x: character vector
    gsub("e([+-][0-9]+)", "e<sup>\\1</sup>", x)
  }
  
  
  run_formatted_analysis <- function(data, pathogen_name, predictor_vars) {
    result_var <- paste0(pathogen_name, "_result")
    
    outcome_vec <- as.numeric(as.character(data[[result_var]]))
    if (sum(outcome_vec, na.rm = TRUE) == 0 ||
        all(outcome_vec == 1, na.rm = TRUE)) {
      return(NULL)
    }
    
    # logistic model
    formula_str <- paste(result_var, "~", paste(predictor_vars, collapse = " + "))
    model <- tryCatch(
      glm(as.formula(formula_str), data = data, family = binomial(link = "logit")),
      error = function(e) NULL
    )
    if (is.null(model)) {
      return(NULL)
    }
    
    # Odds ratios + 95% CI (profile likelihood)
    coefs <- coef(model)
    
    ci <- tryCatch(
      suppressMessages(confint(model)),   # profile\u2010likelihood CI
      error = function(e) NULL
    )
    
    if (is.null(ci)) {
      # fallback to Wald CI if profile fails
      se <- sqrt(diag(vcov(model)))
      ci <- cbind(
        coefs - 1.96 * se,
        coefs + 1.96 * se
      )
      rownames(ci) <- names(coefs)
    }
    
    # align by coefficient name
    common <- intersect(names(coefs), rownames(ci))
    OR_CI <- cbind(
      OR  = exp(coefs[common]),
      LCL = exp(ci[common, 1]),
      UCL = exp(ci[common, 2])
    )
    
    p_vals <- summary(model)$coefficients[, 4]
    
    
    # helper for p formatting (real value, scientific if needed, with stars)
    fmt_p <- function(p) {
      if (is.na(p)) return("")
      stars <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")
                      ))
      base  <- formatC(p, format = "g", digits = 3)   # "5.72e-08"
      base  <- sci_to_sup(base)                      # only "5.72e<sup>-08</sup>"
      paste0(base, stars)                            # stars added normally
    }
    
    
    
    
    build_rows <- function(var_name) {
      x <- data[[var_name]]
      y_fac <- data[[pathogen_name]]
      
      # ensure factor
      if (!is.factor(x)) x <- factor(x)
      y_fac <- factor(tolower(as.character(y_fac)))
      
      tab <- table(x, y_fac)
      chisq_res <- tryCatch(
        chisq.test(tab),
        error = function(e) list(p.value = NA, statistic = NA)
      )
      
      levs <- levels(x)
      rows <- lapply(seq_along(levs), function(i) {
        lv <- levs[i]
        idx <- which(x == lv)
        N <- length(idx)
        pos <- sum(y_fac[idx] == "positive", na.rm = TRUE)
        
        # coefficient name used by glm for non\u2011reference levels
        coef_name <- paste0(var_name, lv) # only used for i > 1
        
        # Chi\u2011square statistics (only shown on first row of this variable)
        chi2_val <- if (i == 1 && !is.na(chisq_res$statistic)) {
          round(as.numeric(chisq_res$statistic), 2)
        } else {
          NA
        }
        chi2_p <- if (i == 1) fmt_p(chisq_res$p.value) else ""
        
        # Logistic OR / CI / p:
        #  - First level is the reference: show "Ref." and blank CI / P
        #  - Other levels use the corresponding coefficient row
        if (i == 1) {
          OR_txt <- "Ref."
          CI_txt <- ""
          logit_p <- ""
        } else if (coef_name %in% rownames(OR_CI)) {
          OR_val <- OR_CI[coef_name, 1]
          LCL    <- OR_CI[coef_name, 2]
          UCL    <- OR_CI[coef_name, 3]
          
          # OR: scientific, 3 sig. digits, then superscript exponent
          OR_raw <- format(OR_val, scientific = TRUE, digits = 3)
          OR_txt <- sci_to_sup(OR_raw)
          
          # CI: scientific, 3 sig. digits, with superscript exponent
          LCL_raw <- format(LCL, scientific = TRUE, digits = 3)
          UCL_raw <- format(UCL, scientific = TRUE, digits = 3)
          
          CI_txt <- paste0(
            sci_to_sup(LCL_raw), " - ",
            sci_to_sup(UCL_raw)
          )
          
          logit_p <- fmt_p(p_vals[coef_name])
        } else {
          OR_txt <- ""
          CI_txt <- ""
          logit_p <- ""
        }
        
        data.frame(
          Variable = if (i == 1) var_name else "",
          Level = lv,
          N_raw = N, # keep numeric N
          Percent_raw = ifelse(N > 0, (pos / N) * 100, 0), # keep numeric %
          Chi2 = chi2_val,
          Chi2_p = chi2_p,
          OR = OR_txt,
          CI = CI_txt,
          Logit_p = logit_p,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
      
      out <- do.call(rbind, rows)
      out
    }
    
    final_df <- do.call(rbind, lapply(predictor_vars, build_rows))
    final_df
  }
  
  
