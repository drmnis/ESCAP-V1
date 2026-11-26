# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s23_anova  (sourced inside server(), local = TRUE)

  analysis_result <- eventReactive(list(trig_anova(), input$spread_type), {   # SD/SE switch updates the table at once
    req(trig_anova())
    df <- dat_raw()
    req(primary_factor(), uni_resp())
    
    cond_col <- uni_cond() # may be "" (None)
    fact_col <- primary_factor()
    resp_cols <- setdiff(uni_resp(), c(fact_col, cond_col))
    validate(need(length(resp_cols) > 0, "Choose at least one numeric response variable."))
    spread_type <- input$spread_type
    
    validate(
      need(fact_col %in% names(df), "Selected treatment column not found."),
      need(all(resp_cols %in% names(df)), "Some response columns not found.")
    )
    
    # Build list of columns we actually need
    use_cols <- c(fact_col, resp_cols)
    if (!is.null(cond_col) && cond_col != "" && cond_col %in% names(df)) {
      use_cols <- c(cond_col, use_cols)
    }
    use_cols <- unique(use_cols)
    
    df2 <- df[, use_cols, drop = FALSE]
    df2 <- df2[complete.cases(df2), , drop = FALSE]
    validate(
      need(nrow(df2) > 0, "No complete rows after removing missing values.")
    )
    
    # Determine condition levels and sub-data
    if (!is.null(cond_col) && cond_col != "" && cond_col %in% names(df2)) {
      has_condition <- TRUE
      cond_levels <- unique(df2[[cond_col]])
    } else {
      has_condition <- FALSE
      cond_levels <- "All"
    }
    
    cond_results <- map(cond_levels, function(clvl) {
      if (has_condition) {
        sub_df <- df2[df2[[cond_col]] == clvl, , drop = FALSE]
      } else {
        sub_df <- df2
      }
      
      validate(
        need(
          length(unique(sub_df[[fact_col]])) >= 2,
          paste("Condition", clvl, ": treatment factor must have at least two levels.")
        )
      )
      
      f <- as.factor(sub_df[[fact_col]])
      
      resp_results <- map(resp_cols, function(ycol) {
        y <- sub_df[[ycol]]
        validate(
          need(is.numeric(y), paste("Response", ycol, "must be numeric."))
        )
        
        # coded levels: agricolae mangles names with "&", ":" etc.; letters are mapped back below
        f_code <- factor(paste0("L", as.integer(f)), levels = paste0("L", seq_along(levels(f))))
        fit <- aov(y ~ f_code)
        an_tab <- anova(fit)
        
        df_treat <- an_tab[1, "Df"]
        df_resid <- an_tab[2, "Df"]
        p_val <- an_tab[1, "Pr(>F)"]
        
        tuk <- HSD.test(fit, "f_code", group = TRUE)
        
        grp <- tuk$groups
        grp$Treatment <- levels(f)[as.integer(sub("^L", "", rownames(grp)))]
        rownames(grp) <- NULL
        grp$Treatment <- as.character(grp$Treatment)   # NEW
        
        summ <- sub_df %>%
          group_by(.data[[fact_col]]) %>%
          summarise(
            mean    = mean(.data[[ycol]], na.rm = TRUE),
            sd      = sd(.data[[ycol]], na.rm = TRUE),
            n       = sum(!is.na(.data[[ycol]])),
            .groups = "drop"
          ) %>%
          mutate(se = sd / sqrt(n)) %>%
          rename(Treatment = !!fact_col)
        
        summ$Treatment <- as.character(summ$Treatment) # NEW
        
        out <- left_join(summ, grp, by = "Treatment")
        
        
        spread_col <- if (spread_type == "sd") "sd" else "se"
        
        out <- out %>%
          mutate(
            mean_fmt = sprintf("%.2f", mean),
            spread_fmt = sprintf("%.2f", .data[[spread_col]]),
            cell_plain = ifelse(
              is.na(groups),
              paste0(mean_fmt, " \u00b1 ", spread_fmt),
              paste0(mean_fmt, " \u00b1 ", spread_fmt, groups)
            ),
            cell_html = {
              v <- cell_plain
              m <- str_match(v, "^(.*?)([A-Za-z]+)$")
              ifelse(
                !is.na(m[, 1]) & m[, 3] != "",
                paste0(m[, 2], "<sup>", m[, 3], "</sup>"),
                v
              )
            }
          ) %>%
          select(Treatment, cell_plain, cell_html)
        
        cv_val <- tryCatch(
          {
            # agricolae::HSD.test stores CV in the statistics table
            as.numeric(tuk$statistics["CV"])
          },
          error = function(e) NA_real_
        )
        
        
        # NEW: compute LSD (5% level) for this variable
        lsd_val <- tryCatch(
          {
            lsd_res <- agricolae::LSD.test(fit, "f_code", group = FALSE)
            as.numeric(lsd_res$statistics$LSD)
          },
          error = function(e) NA_real_
        )
        
        an_df <- as.data.frame(an_tab)
        an_df$Term <- rownames(an_tab)
        rownames(an_df) <- NULL
        an_df <- an_df[, c("Term", setdiff(names(an_df), "Term"))]
        
        list(
          response    = ycol,
          table       = out,
          df_treat    = df_treat,
          df_resid    = df_resid,
          cv          = cv_val,
          lsd         = lsd_val,      # NEW
          p_value     = p_val,
          anova_table = an_df
        )
      })
      
      names(resp_results) <- resp_cols
      
      list(
        condition = clvl,
        responses = resp_results
      )
    })
    
    names(cond_results) <- cond_levels
    cond_results
  })
  
  
  # 5. Results header -------------------------------------------------------
  
  
  # --- View controllers for Univariate tables ---
  
  summary_view <- reactiveVal(list(nrows = 20L))
  anova_view <- reactiveVal(list(nrows = 20L))
  
  observeEvent(input$summary_show_all, {
    summary_view(list(nrows = Inf))
  })
  observeEvent(input$summary_hide, {
    summary_view(list(nrows = 20L))
  })
  
  observeEvent(input$anova_show_all, {
    anova_view(list(nrows = Inf))
  })
  observeEvent(input$anova_hide, {
    anova_view(list(nrows = 20L))
  })
  
  output$summary_view_info <- renderText({
    tab <- tryCatch(analysis_result(), error = function(e) NULL)
    if (is.null(tab)) {
      return("")
    }
    # build the full summary table once (your existing logic):
    cond_res <- tab
    all_tabs <- list()
    cond_names <- names(cond_res)
    for (i in seq_along(cond_names)) {
      cond_name <- cond_names[i]
      resp_list <- cond_res[[cond_name]]$responses
      tab_list <- lapply(resp_list, function(x) {
        t <- x$table %>% dplyr::select(Treatment, cell_html)
        names(t)[names(t) == "cell_html"] <- x$response
        t
      })
      cond_tab <- Reduce(function(a, b) dplyr::full_join(a, b, by = "Treatment"), tab_list)
      cond_tab <- cond_tab[order(cond_tab$Treatment), , drop = FALSE]
      cond_tab <- cbind(Condition = cond_name, cond_tab)
      all_tabs[[length(all_tabs) + 1]] <- cond_tab
      if (i < length(cond_names)) {
        sep_row <- as.list(rep("\u2014\u2014\u2014", ncol(cond_tab)))
        names(sep_row) <- colnames(cond_tab)
        all_tabs[[length(all_tabs) + 1]] <-
          as.data.frame(sep_row, stringsAsFactors = FALSE)
      }
    }
    out <- dplyr::bind_rows(all_tabs)
    if ("Condition" %in% names(out) &&
        length(unique(out$Condition)) == 1 &&
        unique(out$Condition) == "All") {
      out <- out[, setdiff(names(out), "Condition"), drop = FALSE]
    }
    
    v <- summary_view()
    total_r <- nrow(out)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    
    if (show_r == total_r) {
      sprintf("Showing all %d rows.", total_r)
    } else {
      sprintf("Showing first %d of %d rows.", show_r, total_r)
    }
  })
  
  output$anova_view_info <- renderText({
    tab <- tryCatch(analysis_result(), error = function(e) NULL)
    if (is.null(tab)) {
      return("")
    }
    
    # reuse your existing ANOVA-table building logic, but just to get nrow
    cond_res <- tab
    rows <- list()
    cond_names <- names(cond_res)
    for (i in seq_along(cond_names)) {
      cond_name <- cond_names[i]
      resp_list <- cond_res[[cond_name]]$responses
      for (resp_name in names(resp_list)) {
        x <- resp_list[[resp_name]]
        an_df <- x$anova_table
        an_df$Condition <- cond_name
        an_df$Variable <- x$response
        rows[[length(rows) + 1]] <- an_df
      }
      if (i < length(cond_names)) {
        ncols <- ncol(rows[[length(rows)]])
        sep <- as.list(rep("\u2014\u2014\u2014", ncols))
        names(sep) <- names(rows[[length(rows)]])
        rows[[length(rows) + 1]] <- as.data.frame(sep, stringsAsFactors = FALSE)
      }
    }
    rows <- lapply(rows, function(d) {
      as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
    })
    out <- dplyr::bind_rows(rows)
    out <- out[, c(
      "Condition", "Variable", "Term",
      setdiff(names(out), c("Condition", "Variable", "Term"))
    )]
    if ("Condition" %in% names(out) &&
        length(unique(out$Condition)) == 1 &&
        unique(out$Condition) == "All") {
      out <- out[, setdiff(names(out), "Condition"), drop = FALSE]
    }
    
    v <- anova_view()
    total_r <- nrow(out)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    
    if (show_r == total_r) {
      sprintf("Showing all %d rows.", total_r)
    } else {
      sprintf("Showing first %d of %d rows.", show_r, total_r)
    }
  })
  
  
  # 6. Summary table with superscript letters + separators ------------------
  tbl_summary_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      if (is.null(analysis_result())) {
        return(NULL)
      }
      cond_res <- analysis_result()
      all_tabs <- list()
      cond_names <- names(cond_res)
      
      for (i in seq_along(cond_names)) {
        cond_name <- cond_names[i]
        resp_list <- cond_res[[cond_name]]$responses
        
        tab_list <- lapply(resp_list, function(x) {
          t <- x$table %>% dplyr::select(Treatment, cell_html)
          names(t)[names(t) == "cell_html"] <- x$response
          t
        })
        
        cond_tab <- Reduce(function(a, b) dplyr::full_join(a, b, by = "Treatment"), tab_list)
        cond_tab <- cond_tab[order(cond_tab$Treatment), , drop = FALSE]
        cond_tab <- cbind(Condition = cond_name, cond_tab)
        all_tabs[[length(all_tabs) + 1]] <- cond_tab
        
        if (i < length(cond_names)) {
          sep_row <- as.list(rep("\u2014\u2014\u2014", ncol(cond_tab)))
          names(sep_row) <- colnames(cond_tab)
          all_tabs[[length(all_tabs) + 1]] <-
            as.data.frame(sep_row, stringsAsFactors = FALSE)
        }
      }
      
      out <- dplyr::bind_rows(all_tabs)
      if ("Condition" %in% names(out) &&
          length(unique(out$Condition)) == 1 &&
          unique(out$Condition) == "All") {
        out <- out[, setdiff(names(out), "Condition"), drop = FALSE]
      }
      
      v <- summary_view()
      total_r <- nrow(out)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      out <- out[seq_len(show_r), , drop = FALSE]
      
      out
    }
  output$summary_table <- renderTable(tbl_summary_table(),
    rownames = FALSE,
    sanitize.text.function = function(x) x
  )
  
  # 7. Footnote -------------------------------------------------------------
  output$footnote <- renderText({
    if (is.null(analysis_result())) {
      return("")
    }
    "Different superscript letters in the same column within each condition indicate significant differences between means at p < 0.05."
  })
  
  
  # 8. ANOVA table with 3-decimal formatting, stars, separators ------------
  tbl_anova_table <- function(full = FALSE) {
    if (isTRUE(full)) {
      inf <- function() list(nrows = Inf, ncols = Inf)
      summary_view <- anova_view <- chi_view <- prep_view <- pca_eig_view <- pca_load_view <- inf
      pca_scores_view <- corr_spear_view <- corr_pearson_view <- inf
      plsr_show <- list(var = TRUE, par = TRUE, qt = TRUE)
    }

      if (is.null(analysis_result())) {
        return(NULL)
      }
      cond_res <- analysis_result()
      rows <- list()
      cond_names <- names(cond_res)
      
      for (i in seq_along(cond_names)) {
        cond_name <- cond_names[i]
        resp_list <- cond_res[[cond_name]]$responses
        
        for (resp_name in names(resp_list)) {
          x <- resp_list[[resp_name]]
          an_df <- x$anova_table
          an_df$Condition <- cond_name
          an_df$Variable <- x$response
          
          num_cols <- c("Sum Sq", "Mean Sq", "F value")
          for (cc in intersect(num_cols, names(an_df))) {
            an_df[[cc]] <- sprintf("%.3f", as.numeric(an_df[[cc]]))
          }
          
          if ("Pr(>F)" %in% names(an_df)) {
            p_vec <- as.numeric(an_df[["Pr(>F)"]])
            p_fmt <- ifelse(
              is.na(p_vec),
              "",
              formatC(p_vec, format = "g", digits = 3)
            )
            stars <- ifelse(is.na(p_vec), "",
                            ifelse(p_vec < 0.001, "***",
                                   ifelse(p_vec < 0.01, "**",
                                          ifelse(p_vec < 0.05, "*", "")
                                   )
                            )
            )
            an_df[["Pr(>F)"]] <- p_fmt
            an_df[["Signif"]] <- stars
            names(an_df)[names(an_df) == "Pr(>F)"] <- "P value"
          } else {
            an_df[["Signif"]] <- ""
          }
          
          # NEW: add CV% and LSD columns (one value per Variable)
          cv_val  <- if (!is.null(x$cv))  x$cv  else NA_real_
          lsd_val <- if (!is.null(x$lsd)) x$lsd else NA_real_
          
          an_df[["CV%"]] <- ""
          an_df[["LSD"]] <- ""
          
          if (nrow(an_df) > 0) {
            if (is.numeric(cv_val) && length(cv_val) == 1 &&
                !is.na(cv_val) && is.finite(cv_val)) {
              an_df[["CV%"]][1] <- sprintf("%.2f", cv_val)
            }
            if (is.numeric(lsd_val) && length(lsd_val) == 1 &&
                !is.na(lsd_val) && is.finite(lsd_val)) {
              an_df[["LSD"]][1] <- sprintf("%.3f", lsd_val)
            }
          }
          
          
          
          rows[[length(rows) + 1]] <- an_df
          
        }
        
        if (i < length(cond_names)) {
          ncols <- ncol(rows[[length(rows)]])
          sep <- as.list(rep("\u2014\u2014\u2014", ncols))
          names(sep) <- names(rows[[length(rows)]])
          rows[[length(rows) + 1]] <- as.data.frame(sep, stringsAsFactors = FALSE)
        }
      }
      
      rows <- lapply(rows, function(d) {
        as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
      })
      
      out <- dplyr::bind_rows(rows)
      out <- out[, c(
        "Condition", "Variable", "Term",
        setdiff(names(out), c("Condition", "Variable", "Term"))
      )]
      
      if ("Condition" %in% names(out) &&
          length(unique(out$Condition)) == 1 &&
          unique(out$Condition) == "All") {
        out <- out[, setdiff(names(out), "Condition"), drop = FALSE]
      }
      
      v <- anova_view()
      total_r <- nrow(out)
      show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
      out[seq_len(show_r), , drop = FALSE]
    }
  output$anova_table <- renderTable(tbl_anova_table(),
    rownames = FALSE
  )
  
  
