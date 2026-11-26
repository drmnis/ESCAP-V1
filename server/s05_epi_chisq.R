# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s05_epi_chisq  (sourced inside server(), local = TRUE)
# Epidemiological data: prevalence, chi-square / Fisher, crude and adjusted OR.
# Calculations live in R/epi.R.

  chi_prepare_data <- function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    colnames(df) <- gsub("\\.", "_", make.names(trimws(colnames(df)), unique = TRUE))
    pathogens <- names(df)[vapply(df, esc_is_outcome_col, logical(1))]
    species_col <- NULL
    if (!is.null(uni_cond()) && nzchar(uni_cond())) {
      cc <- gsub("\\.", "_", make.names(trimws(uni_cond())))
      if (cc %in% names(df)) species_col <- cc
    }
    if (is.null(species_col) && "Species" %in% colnames(df)) species_col <- "Species"
    id_cols <- names(df)[mapply(esc_is_id_col, names(df), df)]
    predictors <- setdiff(colnames(df), c(pathogens, species_col, id_cols))
    predictors <- predictors[vapply(df[predictors], function(x) {
      u <- length(unique(stats::na.omit(x))); u > 1 && u <= max(15, 0.5 * nrow(df))
    }, logical(1))]
    for (v in predictors) df[[v]] <- factor(trimws(as.character(df[[v]])))
    list(df = df, pathogens = pathogens, species_col = species_col, predictors = predictors)
  }

  chi_result <- eventReactive(trig_chi(), {
    prep <- chi_prepare_data(dat_raw())
    validate(
      need(length(prep$pathogens) > 0, "No outcome columns with positive/negative values were detected."),
      need(length(prep$predictors) > 0, "No suitable categorical predictor variables were found.")
    )
    refs <- stats::setNames(lapply(prep$predictors, function(v) isolate(input[[paste0("chi_ref_", v)]])),
                            prep$predictors)
    p_enter <- isolate(input$chi_p_enter) %||% 0.20
    df <- prep$df
    strata <- if (is.null(prep$species_col)) list(All = df) else split(df, df[[prep$species_col]], drop = TRUE)
    if (!is.null(prep$species_col) && length(strata) > 1 && isTRUE(isolate(input$chi_pooled)))
      strata <- c(list(`All (pooled)` = df), strata)
    out <- list()
    withProgress(message = "Epidemiological analysis", value = 0, {
      for (sp in names(strata)) for (pt in prep$pathogens) {
        incProgress(1 / (length(strata) * length(prep$pathogens)), detail = paste(sp, pt))
        r <- tryCatch(esc_epi_analysis(strata[[sp]], pt, prep$predictors, refs, p_enter),
                      error = function(e) {
                        showNotification(paste0(sp, " / ", pt, ": ", conditionMessage(e)), type = "warning", duration = 8)
                        NULL
                      })
        if (!is.null(r)) { r$species <- sp; r$pathogen <- pt; out[[paste(sp, pt)]] <- r }
      }
    })
    validate(need(length(out) > 0, "No stratum had both positive and negative cases."))
    state$current_analysis <- "epidemiology"
    out
  })

  # Flat, publication-style table (used for display, PDF report and CSV download)
  chi_flat_table <- function(res, html = TRUE) {
    dag <- if (html) "&dagger;" else "+"
    blocks <- lapply(res, function(r) {
      t <- r$table
      first <- !duplicated(t$Variable)
      in_model <- t$Variable %in% r$selected
      body <- data.frame(
        Variable = ifelse(first, t$Variable, ""),
        Level = t$Level, N = as.character(t$N), Positive = as.character(t$Positive),
        `Prevalence % (95% CI)` = sprintf("%.1f (%.1f-%.1f)", t$Prevalence, t$Prev_LCL, t$Prev_UCL),
        Chi2 = ifelse(first, sprintf("%.2f", t$Chi2), ""),
        P = ifelse(first, paste0(vapply(t$P_test, esc_fmt_p, "", html = html),
                                 ifelse(t$Test == "Fisher exact", dag, "")), ""),
        `COR (95% CI)` = ifelse(t$is_ref, "Ref.", mapply(esc_fmt_or, t$COR, t$COR_LCL, t$COR_UCL)),
        `P (COR)` = ifelse(t$is_ref, "", vapply(t$COR_P, esc_fmt_p, "", html = html)),
        `AOR (95% CI)` = ifelse(!in_model, "", ifelse(t$is_ref, "Ref.", mapply(esc_fmt_or, t$AOR, t$AOR_LCL, t$AOR_UCL))),
        `P (AOR)` = ifelse(!in_model | t$is_ref, "", vapply(t$AOR_P, esc_fmt_p, "", html = html)),
        check.names = FALSE, stringsAsFactors = FALSE)
      o <- r$overall
      hdr <- body[1, ]; hdr[1, ] <- ""
      lab <- paste(r$species, "|", r$pathogen)
      hdr$Variable <- if (html) paste0("<b>", lab, "</b>") else lab
      hdr$N <- as.character(o[["N"]]); hdr$Positive <- as.character(o[["Positive"]])
      hdr$`Prevalence % (95% CI)` <- sprintf("%.1f (%.1f-%.1f)", o[["Prevalence"]], o[["LCL"]], o[["UCL"]])
      rbind(hdr, body)
    })
    out <- do.call(rbind, blocks); rownames(out) <- NULL
    out
  }

  # Original ESCAP layout of the publication table: Variable | Level | N | % | chi2 | P | OR | 95% CI | P,
  # one bold "Species Pathogen" header row per block. OR, CI and P come from the multivariable logistic
  # model; variables that did not enter it show the crude OR (in italics).
  chi_original_table <- function(res, html = TRUE) {
    dag <- if (html) "&dagger;" else "+"
    num <- function(x) ifelse(!is.finite(x), "", ifelse(abs(x) < 0.01 | abs(x) >= 1000,
                       { v <- formatC(x, format = "e", digits = 2); if (html) gsub("e\\+?(-?)0*([0-9]+)", " \u00d7 10<sup>\\1\\2</sup>", v) else v },
                       sprintf("%.2f", x)))
    it <- function(x, on) if (html) ifelse(on & nzchar(x), paste0("<i>", x, "</i>"), x) else x
    blocks <- lapply(res, function(r) {
      t <- r$table; first <- !duplicated(t$Variable); in_model <- t$Variable %in% r$selected
      or <- ifelse(in_model, t$AOR, t$COR); lo <- ifelse(in_model, t$AOR_LCL, t$COR_LCL); hi <- ifelse(in_model, t$AOR_UCL, t$COR_UCL)
      pp <- ifelse(in_model, t$AOR_P, t$COR_P)
      body <- data.frame(
        Variable = ifelse(first, t$Variable, ""), Level = t$Level, N = as.character(t$N),
        Pct = sprintf("%.1f", t$Prevalence),
        Chi2 = ifelse(first, sprintf("%.2f", t$Chi2), ""),
        P1 = ifelse(first, paste0(vapply(t$P_test, esc_fmt_p, "", html = html), ifelse(t$Test == "Fisher exact", dag, "")), ""),
        OR = ifelse(t$is_ref, "Ref.", it(num(or), !in_model)),
        CI = ifelse(t$is_ref, "", it(ifelse(is.finite(lo), paste0(num(lo), " - ", num(hi)), ""), !in_model)),
        P2 = ifelse(t$is_ref, "", vapply(pp, esc_fmt_p, "", html = html)),
        stringsAsFactors = FALSE)
      hdr <- body[1, ]; hdr[1, ] <- ""
      lab <- paste(r$species, r$pathogen)
      hdr$Variable <- if (html) paste0("<b>", htmltools::htmlEscape(lab), "</b>") else lab
      rbind(hdr, body)
    })
    out <- do.call(rbind, blocks); rownames(out) <- NULL
    names(out) <- if (html) c("Variable", "Level", "<i>N</i>", "%", "<i>&chi;<sup>2</sup></i>", "<i>P value</i>", "OR", "95% CI", "<i>P value</i> ")
                  else c("Variable", "Level", "N", "%", "Chi2", "P value", "OR", "95% CI", "P value (OR)")
    out
  }

  output$chi_table <- renderTable({
    out <- chi_original_table(chi_result(), html = TRUE)
    v <- chi_view(); nr <- nrow(out)
    out[seq_len(if (is.infinite(v$nrows)) nr else min(v$nrows, nr)), , drop = FALSE]
  }, rownames = FALSE, sanitize.text.function = function(x) x, na = "")

  chi_fit_df <- reactive({
    res <- chi_result()
    fs <- lapply(res, function(r) if (!is.null(r$fit_stats)) cbind(Stratum = paste(r$species, "|", r$pathogen), r$fit_stats))
    fs <- Filter(Negate(is.null), fs)
    if (!length(fs)) return(data.frame(Note = "No variable reached the entry threshold for the multivariable model."))
    do.call(rbind, fs)
  })
  output$chi_fit_table <- renderTable(chi_fit_df(), rownames = FALSE)

  chi_note_text <- function() paste0(
    "N: number of animals; %: prevalence (positive / N). P value (first): Pearson chi-square test; &dagger; Fisher exact test ",
    "(an expected count was below 5). OR (95% CI) and P value (second): multivariable logistic regression; Ref.: reference level. ",
    "Variables with univariable P &ge; ", isolate(input$chi_p_enter) %||% 0.20, " did not enter the model and show the crude OR (italics). ",
    "Firth penalised likelihood is used automatically when separation occurs. * P &lt; 0.05, ** P &lt; 0.01, *** P &lt; 0.001.")
  output$chi_footnote <- renderUI({
    req(chi_result())
    HTML(paste0("<small>", chi_note_text(), "</small>"))
  })

  # Forest plot of odds ratios (used by the Graphs tab and downloads)
  chi_forest_plot <- function(which = c("AOR", "COR")) {
    which <- match.arg(which)
    res <- chi_result()
    d <- do.call(rbind, lapply(res, function(r) {
      t <- r$table[!r$table$is_ref, ]
      data.frame(Stratum = paste(r$species, "|", r$pathogen), Term = paste0(t$Variable, ": ", t$Level),
                 OR = t[[which]], L = t[[paste0(which, "_LCL")]], U = t[[paste0(which, "_UCL")]])
    }))
    d <- d[is.finite(d$OR) & is.finite(d$L) & is.finite(d$U), ]
    validate(need(nrow(d) > 0, paste("No", which, "values to plot.")))
    ggplot(d, aes(x = OR, y = Term)) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
      geom_errorbarh(aes(xmin = L, xmax = U), height = 0.2) +
      geom_point(size = 2.5, shape = 15) +
      scale_x_log10() + facet_wrap(~Stratum, scales = "free_y") +
      labs(x = paste(if (which == "AOR") "Adjusted" else "Crude", "odds ratio (95% CI, log scale)"), y = NULL) +
      theme_bw()
  }
  chi_prevalence_plot <- function() {
    res <- chi_result()
    d <- do.call(rbind, lapply(res, function(r) cbind(Stratum = paste(r$species, "|", r$pathogen), r$table)))
    ggplot(d, aes(x = Level, y = Prevalence)) +
      geom_col(fill = "grey70", colour = "black", width = 0.7) +
      geom_errorbar(aes(ymin = Prev_LCL, ymax = Prev_UCL), width = 0.2) +
      facet_grid(Stratum ~ Variable, scales = "free_x", space = "free_x") +
      labs(x = NULL, y = "Prevalence (%) with 95% CI") + theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
