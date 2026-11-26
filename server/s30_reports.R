# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s30_reports  (sourced inside server(), local = TRUE)
# Report definitions for each results tab. Each builder returns
# list(title, settings, sections = list(list(heading, table, note, html))).

  safe_tbl <- function(f) tryCatch(f(), error = function(e) NULL)
  prep_settings <- function() c(Preprocessing = esc_prep_label(input$prep_spectral, input$prep_scaling, input$prep_centering))

  report_builders <- list(
    rep_uni = function() {
      secs <- list()
      s1 <- safe_tbl(function() tbl_summary_table(TRUE))
      if (!is.null(s1)) secs[[length(secs) + 1]] <- list(heading = "Treatment means", table = s1,
        note = paste0("Values are mean &plusmn; ", toupper(input$spread_type %||% "sd"),
                      ". Means in a column followed by different letters differ significantly (Tukey HSD, P &lt; 0.05)."))
      s2 <- safe_tbl(function() tbl_anova_table(TRUE))
      if (!is.null(s2)) secs[[length(secs) + 1]] <- list(heading = "Analysis of variance", table = s2)
      ch <- safe_tbl(function() chi_original_table(chi_result(), html = TRUE))
      if (!is.null(ch)) {
        secs[[length(secs) + 1]] <- list(heading = "Prevalence, chi-square test and logistic regression", table = ch,
          note = chi_note_text())
        secs[[length(secs) + 1]] <- list(heading = "Multivariable model fit", table = safe_tbl(chi_fit_df))
      }
      list(title = "Epidemiological analysis",
           settings = c(`Species / stratum` = if (nzchar(uni_cond() %||% "")) uni_cond() else "None",
                        `Model entry P` = as.character(input$chi_p_enter %||% 0.2)),
           sections = secs)
    },
    rep_prep = function() {
      cap <- esc_cap_columns(as.data.frame(safe_tbl(function() tbl_prep_data_table(TRUE))))
      list(title = "Preprocessed data", settings = prep_settings(),
           sections = list(list(heading = "Preprocessed X matrix", table = cap$df, note = cap$note)))
    },
    rep_pca = function() {
      ld <- esc_cap_columns(as.data.frame(safe_tbl(function() tbl_pca_loadings(TRUE))))
      sc <- esc_cap_columns(as.data.frame(safe_tbl(function() tbl_pca_scores(TRUE))))
      list(title = "Principal component analysis", settings = prep_settings(), sections = list(
        list(heading = "Eigenvalues and explained variance", table = safe_tbl(function() tbl_pca_eigenvalues(TRUE))),
        list(heading = "Loadings", table = ld$df, note = ld$note),
        list(heading = "Scores", table = sc$df, note = sc$note)))
    },
    rep_plsr = function() {
      r <- tryCatch(plsr_result(), error = function(e) NULL)
      list(title = "Partial least squares regression", settings = c(prep_settings(),
             `Y variable` = input$plsr_y_col %||% "", `Cross-validation` = input$plsr_cv_type %||% "",
             `LV used` = if (!is.null(r$chosen_lv)) r$chosen_lv else ""),
           sections = list(
             list(heading = "Model performance per latent variable", table = safe_tbl(function() tbl_plsr_perf_table(TRUE)),
                  note = "R2 = 1 - SSE/SST. SEC, SECV, SEP: bias-corrected standard errors. RPD = SD(y) / RMSE. Preprocessing re-fitted inside each CV fold."),
             list(heading = "Explained variance (X and Y blocks)", table = safe_tbl(function() tbl_plsr_var_table(TRUE))),
             list(heading = "Regression coefficients, VIP and selectivity ratio", table = safe_tbl(function() tbl_plsr_par_table(TRUE)),
                  note = "VIP after Wold et al. (1993); selectivity ratio by target projection (Rajalahti et al., 2009)."),
             list(heading = "Q residuals and Hotelling T2", table = safe_tbl(function() tbl_plsr_qt_table(TRUE)),
                  note = if (!is.null(r$Q_lim)) sprintf("95%% limits: Q = %.4g, T2 = %.4g.", r$Q_lim, r$T2_lim) else NULL)))
    },
    rep_corr = function() {
      sp <- esc_cap_columns(as.data.frame(safe_tbl(function() tbl_corr_table(TRUE))))
      pe <- esc_cap_columns(as.data.frame(safe_tbl(function() tbl_corr_table_pearson(TRUE))))
      list(title = "Correlation analysis", sections = list(
        list(heading = "Spearman rank-order correlation", table = sp$df, note = sp$note),
        list(heading = "Pearson product-moment correlation", table = pe$df, note = pe$note)))
    }
  )

  # Called by new modules to register their own report
  register_report <- function(id, builder) {
    report_builders[[id]] <<- builder
    make_report_handlers(id)
  }

  report_file_name <- function(rep, ext) {
    t <- gsub("[^A-Za-z0-9]+", "_", rep$title %||% "ESCAP_report")
    paste0(t, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", ext)
  }

  make_report_handlers <- function(id) {
    get_rep <- function() {
      rep <- report_builders[[id]]()
      rep$sections <- Filter(function(s) !is.null(s$table) && NROW(s$table) > 0, rep$sections)
      validate(need(length(rep$sections) > 0, "Run the analysis first; there are no results to report."))
      rep
    }
    output[[paste0(id, "_pdf")]] <- downloadHandler(
      filename = function() report_file_name(tryCatch(get_rep(), error = function(e) list(title = id)), "pdf"),
      content = function(file) {
        rep <- get_rep()
        withProgress(message = "Building PDF report", value = 0.5, {
          esc_report_pdf(file, rep$title, rep$sections, paper = input[[paste0(id, "_paper")]] %||% "A4",
                         orientation = input[[paste0(id, "_orient")]] %||% "auto", settings = rep$settings)
        })
      }, contentType = "application/pdf")
    output[[paste0(id, "_docx")]] <- downloadHandler(
      filename = function() report_file_name(tryCatch(get_rep(), error = function(e) list(title = id)), "docx"),
      content = function(file) {
        rep <- get_rep()
        esc_report_docx(file, rep$title, rep$sections, paper = input[[paste0(id, "_paper")]] %||% "A4",
                        orientation = input[[paste0(id, "_orient")]] %||% "auto", settings = rep$settings)
      }, contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    output[[paste0(id, "_md")]] <- downloadHandler(
      filename = function() report_file_name(tryCatch(get_rep(), error = function(e) list(title = id)), "md"),
      content = function(file) { rep <- get_rep(); esc_report_md(file, rep$title, rep$sections, rep$settings) },
      contentType = "text/markdown")
    output[[paste0(id, "_csv")]] <- downloadHandler(
      filename = function() report_file_name(tryCatch(get_rep(), error = function(e) list(title = id)), "csv"),
      content = function(file) {
        rep <- get_rep(); con <- file(file, "w", encoding = "UTF-8"); on.exit(close(con))
        writeLines(paste0("\"", rep$title, "\""), con)
        for (s in rep$sections) {
          writeLines(c("", paste0("\"", s$heading %||% "", "\"")), con)
          df <- as.data.frame(lapply(s$table, function(x) esc_strip_html(as.character(x))), check.names = FALSE)
          rn <- rownames(s$table)
          if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(df))))) df <- cbind(` ` = rn, df)
          utils::write.table(df, con, sep = ",", row.names = FALSE, qmethod = "double")
        }
        writeLines(c("", "\"Generated by ESCAP: Easy Statistics, Chemometrics and Plotting\""), con)
      }, contentType = "text/csv")
  }
  for (id in names(report_builders)) local({ i <- id; make_report_handlers(i) })
