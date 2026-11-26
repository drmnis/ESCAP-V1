# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s40_stats  (sourced inside server(), local = TRUE)
# Statistics tab. Calculations in R/stats.R, plots in R/stats_plots.R.

  st_data <- reactive(dat_raw())

  # Settings for the analysis chosen in "Analysis". The same columns drive the univariate graphs
  # (responses = st_y, factor(s) = st_A / st_B, condition / ID = st_cond), see uni_resp() etc. in s07.
  output$st_controls <- renderUI({
    an <- input$uni_analysis; if (is.null(an)) return(NULL)
    df <- tryCatch(st_data(), error = function(e) NULL)
    if (is.null(df)) return(helpText("Upload a data file to see the settings for this analysis."))
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    allc <- names(df)
    cat_guess <- names(df)[vapply(df, function(x) !is.numeric(x) || length(unique(x)) <= 12, logical(1))]
    if (!length(cat_guess)) cat_guess <- allc
    # default responses: numeric columns that are not factor-like (<= 12 levels) or ID / serial columns
    id_like <- grepl("(^|_)(id|no|sl|sn|serial|sample|code)(_|$)", tolower(num))
    num_resp <- num[!num %in% cat_guess & !id_like]
    if (!length(num_resp)) num_resp <- num
    keep <- function(id, choices, default) { v <- isolate(input[[id]]); if (!is.null(v) && all(v %in% choices) && length(v)) v else default }
    r2 <- function(a, b) fluidRow(column(6, a), column(6, b))
    r3 <- function(a, b, c) fluidRow(column(4, a), column(4, b), column(4, c))
    resp <- function(multi = TRUE, label = if (multi) "Response variable(s)" else "Response variable")
      selectizeInput("st_y", label, num, selected = keep("st_y", num, if (multi) num_resp else num_resp[1]), multiple = multi,
                     options = list(plugins = list("remove_button")))
    fac <- function(id, label, choices = cat_guess, none = FALSE, default = if (none) "" else choices[1])
      selectInput(id, label, if (none) c("None" = "", choices) else choices, selected = keep(id, c("", choices), default))
    cond <- function(label = "Condition / species / ID (optional)") fac("st_cond", label, allc, none = TRUE)
    ph <- selectInput("st_posthoc", "Post-hoc", c("Tukey HSD" = "HSD", "Fisher LSD" = "LSD", "Duncan (DMRT)" = "Duncan",
                                                  "SNK" = "SNK", "Scheffe" = "Scheffe", "Bonferroni" = "Bonferroni"))
    sst <- selectInput("st_sstype", "Sum of squares", c("Type II" = 2, "Type III" = 3, "Type I" = 1))
    alp <- numericInput("st_alpha", "Alpha", 0.05, 0.001, 0.2, 0.01)
    blk <- fac("st_block", "Block (RCBD, optional)", allc, none = TRUE)
    switch(an,
      chi = tagList(cond("Species / stratum column (optional)"),
                    helpText("Outcome columns (positive / negative) and categorical predictors are detected automatically.")),
      desc = tagList(resp(), r2(fac("st_A", "Group by (optional)", allc, none = TRUE),
                                numericInput("st_conf", "Confidence level", 0.95, 0.5, 0.999, 0.01))),
      ttest = tagList(resp(), radioButtons("st_ttype", NULL, c("Independent" = "independent", "Paired" = "paired", "One-sample" = "one"), inline = TRUE),
                      r2(fac("st_A", "Group column (2 groups)", cat_guess, none = TRUE, default = cat_guess[1]),
                         fac("st_pair", "Pair / subject ID (paired)", allc, none = TRUE)),
                      r3(numericInput("st_mu", "Test value (one-sample)", 0),
                         selectInput("st_alt", "Alternative", c("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")),
                         div(style = "margin-top:22px;", checkboxInput("st_vareq", "Equal variances", FALSE)))),
      anova1 = tagList(resp(), r2(fac("st_A", "Treatment factor"), blk),
                       r2(cond("Condition / species / ID (optional)"), div()), r3(ph, sst, alp),
                       helpText("Condition: one ANOVA and one table block per level (also the ID column for circular bar plots).")),
      anova2 = tagList(resp(), r2(fac("st_A", "Factor A", default = cat_guess[1]), fac("st_B", "Factor B", default = cat_guess[min(2, length(cat_guess))])),
                       r2(blk, div(style = "margin-top:22px;", checkboxInput("st_inter", "Include A \u00d7 B interaction", TRUE))),
                       r3(ph, sst, alp)),
      ancova = tagList(resp(), r3(fac("st_A", "Group factor"), selectInput("st_cov", "Covariate", num, selected = keep("st_cov", num, num[min(2, length(num))])), sst)),
      kruskal = tagList(resp(), r2(fac("st_A", "Group factor"),
                        selectInput("st_padj", "P adjustment (Dunn)", c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH (FDR)" = "BH", "None" = "none")))),
      cor = tagList(selectizeInput("st_y", "Variables (2 or more)", num, selected = keep("st_y", num, head(num, 4)), multiple = TRUE,
                                   options = list(plugins = list("remove_button"))),
                    r2(radioButtons("st_cormethod", "Method", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
                       selectInput("st_padj", "P adjustment", c("None" = "none", "Holm" = "holm", "Bonferroni" = "bonferroni", "BH (FDR)" = "BH")))),
      reg = tagList(r3(selectInput("st_y", "Response (y)", num, selected = keep("st_y", num, num[1])[1]),
                       selectInput("st_x", "Predictor (x)", num, selected = num[min(2, length(num))]),
                       numericInput("st_degree", "Degree", 1, 1, 4, 1))),
      kin = tagList(r3(selectInput("st_x", "Time", num), selectInput("st_y", "Concentration", num, selected = num[min(2, length(num))]),
                       fac("st_A", "Group (optional)", allc, none = TRUE)),
                    helpText("Zero-, first-, second-order and Weibull fits. If the group column holds temperatures (\u00b0C), an Arrhenius fit is added.")),
      NULL)
  })

  st_result <- eventReactive(trig_stats(), {
    df <- st_data(); tst <- input$uni_analysis
    ys <- input$st_y; A <- input$st_A; A <- if (is.null(A) || !nzchar(A)) NULL else A
    ys <- setdiff(ys, c(A, input$st_B, input$st_block, input$st_cov))   # a factor cannot also be a response
    validate(need(length(ys) > 0, "Select at least one response variable (different from the factor columns)."))
    out <- list(test = tst, label = names(ESC_STAT_TESTS)[ESC_STAT_TESTS == tst], items = list())
    run1 <- function(y) switch(tst,
      ttest = esc_ttest(df, y, A, input$st_ttype, mu = input$st_mu %||% 0, var_equal = isTRUE(input$st_vareq),
                        alternative = input$st_alt %||% "two.sided", pair_id = input$st_pair),
      anova1 = esc_anova(df, y, A, block = if (nzchar(input$st_block %||% "")) input$st_block, ss_type = as.integer(input$st_sstype),
                         posthoc = input$st_posthoc, alpha = input$st_alpha %||% 0.05),
      anova2 = { validate(need(!identical(input$st_A, input$st_B), "Factor A and Factor B must be different columns."))
                 esc_anova(df, y, A, input$st_B, block = if (nzchar(input$st_block %||% "")) input$st_block,
                           interaction = isTRUE(input$st_inter), ss_type = as.integer(input$st_sstype),
                           posthoc = input$st_posthoc, alpha = input$st_alpha %||% 0.05) },
      ancova = esc_ancova(df, y, A, input$st_cov, ss_type = as.integer(input$st_sstype)),
      kruskal = esc_kruskal(df, y, A, input$st_padj %||% "holm"),
      reg = esc_regression(df, y, input$st_x, input$st_degree %||% 1),
      kin = esc_kinetics(df, input$st_x, y, A))
    withProgress(message = "Running analysis", value = 0, {
      if (tst == "desc") out$items[["All responses"]] <- list(desc = esc_describe(df, ys, A, input$st_conf %||% 0.95))
      else if (tst == "cor") out$items[["Correlations"]] <- list(cor = esc_cor_test_table(df, ys, input$st_cormethod, input$st_padj))
      else if (tst == "anova1" && nzchar(input$st_cond %||% "") && input$st_cond %in% names(df) && !identical(input$st_cond, A)) {
        # one ANOVA per level of the condition column (e.g. species, storage day): one table block per level
        cc <- input$st_cond; full <- df; levs <- unique(as.character(full[[cc]])); levs <- levs[!is.na(levs)]
        out$cond <- cc; out$cond_items <- list()
        for (lv in levs) {
          df <- full[as.character(full[[cc]]) == lv & !is.na(full[[cc]]), , drop = FALSE]
          out$cond_items[[lv]] <- list()
          for (y in ys) {
            incProgress(1 / (length(ys) * length(levs)), detail = paste(lv, y))
            it <- tryCatch(run1(y), error = function(e) list(error = conditionMessage(e)))
            out$cond_items[[lv]][[y]] <- it
            out$items[[paste0(y, " [", cc, " = ", lv, "]")]] <- it
          }
        }
        df <- full
      }
      else for (y in ys) {
        incProgress(1 / length(ys), detail = y)
        out$items[[y]] <- tryCatch(run1(y), error = function(e) list(error = conditionMessage(e)))
      }
    })
    out$settings <- c(Analysis = out$label, Factors = paste(c(A, if (tst == "anova2") input$st_B), collapse = " x "),
                      if (!is.null(out$cond)) c(Condition = out$cond))
    out$labels <- c(A %||% "", input$st_B %||% "")
    out$posthoc <- c(HSD = "Tukey HSD", LSD = "Fisher LSD", Duncan = "Duncan multiple range test", SNK = "Student-Newman-Keuls",
                     Scheffe = "Scheffe", Bonferroni = "Bonferroni-adjusted LSD")[input$st_posthoc %||% "HSD"]
    out$padj <- input$st_padj %||% "holm"
    out
  })

  # ---- publication-ready table (top of Results > Univariate); follows the SD/SE choice
  st_pub <- reactive({
    r <- st_result(); sp <- input$spread_type %||% "sd"
    tryCatch({
      if (is.null(r$cond_items)) return(esc_publication_table(r, sp))
      # condition blocks: same layout as the classic table (Condition column, one block per level)
      parts <- lapply(names(r$cond_items), function(lv) {
        rr <- r; rr$items <- r$cond_items[[lv]]; pt <- esc_publication_table(rr, sp)
        if (is.null(pt) || !is.null(pt$error)) return(NULL)
        tb <- pt$table; cond <- rep("", nrow(tb)); cond[1] <- htmltools::htmlEscape(lv)
        list(table = cbind(setNames(data.frame(cond, stringsAsFactors = FALSE), r$cond), tb), note = pt$note)
      })
      parts <- Filter(Negate(is.null), parts)
      validate(need(length(parts) > 0, "The ANOVA could not be fitted within the condition levels."))
      sep <- function(tb) { z <- tb[1, , drop = FALSE]; z[1, ] <- ""; z }
      tb <- do.call(rbind, unlist(lapply(seq_along(parts), function(i)
        if (i < length(parts)) list(parts[[i]]$table, sep(parts[[i]]$table)) else list(parts[[i]]$table)), recursive = FALSE))
      rownames(tb) <- NULL
      list(table = tb, note = paste(parts[[1]]$note, "Letters compare treatments within each", r$cond, "level."))
    }, error = function(e) list(error = conditionMessage(e)))
  })
  output$st_pub <- renderUI({
    r <- st_result(); pt <- st_pub()
    if (is.null(pt)) return(NULL)
    if (!is.null(pt$error)) return(div(class = "alert alert-warning", "Publication table could not be built: ", pt$error))
    tagList(h4(paste("Publication-ready table:", r$label)),
            div(style = "overflow-x:auto;", class = "esc-pub", HTML(esc_df_to_html(pt$table))),
            tags$small(pt$note), tags$hr())
  })

  # ---- tables for one item (used for screen and report)
  st_fmt <- function(df, digits = 4) {
    if (is.null(df)) return(NULL)
    df <- as.data.frame(df, check.names = FALSE); rownames(df) <- NULL
    for (j in seq_along(df)) if (is.numeric(df[[j]])) {
      p_col <- grepl("^Pr\\(|^P$|p.value", names(df)[j])
      df[[j]] <- if (p_col) esc_fmtp(df[[j]]) else ifelse(is.na(df[[j]]), "", ifelse(abs(df[[j]]) < 1e-10, "0", formatC(df[[j]], digits = digits, format = "g")))
    }
    df
  }
  st_tables <- function(it, test) {
    if (!is.null(it$error)) return(list(list(heading = "Error", table = data.frame(Message = it$error))))
    switch(test,
      desc = list(list(heading = "Descriptive statistics", table = st_fmt(it$desc))),
      cor = list(list(heading = "Correlation tests", table = st_fmt(it$cor))),
      ttest = list(list(heading = "Group summary", table = st_fmt(it$descriptives)),
                   list(heading = "Test results (parametric and non-parametric)", table = it$tests),
                   list(heading = "Effect size", table = it$effect)),
      anova1 = , anova2 = , ancova = c(
        list(list(heading = paste0("ANOVA table (", deparse(it$formula), ")"), table = st_fmt(it$anova),
                  note = sprintf("CV = %.2f%%; MSE = %.4g. Partial eta-squared = SS(effect)/(SS(effect)+SS(error)).", it$cv, it$mse))),
        if (!is.null(it$slope_check)) list(list(heading = "ANCOVA assumption", table = st_fmt(it$slope_check))),
        if (!is.null(it$adjusted)) list(list(heading = "Covariate-adjusted means (emmeans)", table = st_fmt(it$adjusted$means)),
                                        list(heading = "Pairwise comparisons of adjusted means (Tukey)", table = st_fmt(it$adjusted$pairs))),
        list(list(heading = paste("Means and letters:", it$A), table = st_fmt(it$letters_A),
                  note = "Means sharing a letter are not significantly different.")),
        if (!is.null(it$letters_B)) list(list(heading = paste("Means and letters:", it$B), table = st_fmt(it$letters_B))),
        if (!is.null(it$letters_AB)) list(list(heading = paste("Means and letters: interaction", it$A, "x", it$B), table = st_fmt(it$letters_AB))),
        if (!is.null(it$simple)) list(list(heading = paste("Simple effects: pairwise", it$A, "within each", it$B, "(Tukey)"), table = st_fmt(it$simple))),
        list(list(heading = "Assumption checks", table = st_fmt(it$assumptions)))),
      kruskal = list(list(heading = "Group summary", table = st_fmt(it$summary)), list(heading = "Kruskal-Wallis test", table = st_fmt(it$test)),
                     list(heading = "Dunn post-hoc comparisons", table = it$dunn)),
      reg = list(list(heading = paste("Regression:", it$equation), table = it$coef), list(heading = "Model fit", table = it$fit_stats)),
      kin = c(list(list(heading = "Kinetic model parameters", table = st_fmt(it$table),
                        note = "k: rate constant; half-life in time units of the data; * marks the model with the lowest AIC per group.")),
              tryCatch({
                k1 <- it$table[it$table$Model == "First order", ]; tc <- suppressWarnings(as.numeric(k1$Group))
                if (nrow(k1) >= 3 && all(is.finite(tc))) list(list(heading = "Arrhenius analysis (first-order k)", table = st_fmt(esc_arrhenius(tc, k1$k)))) else NULL
              }, error = function(e) NULL)))
  }

  output$st_results <- renderUI({
    r <- st_result()
    blocks <- lapply(names(r$items), function(nm) {
      tabs <- st_tables(r$items[[nm]], r$test)
      tagList(h4(style = "color:#0A57B5; margin-top:18px;", nm),
              lapply(tabs, function(s) tagList(h5(strong(s$heading)),
                div(style = "overflow-x:auto;", HTML(esc_df_to_html(s$table, html = FALSE))),
                if (!is.null(s$note)) tags$small(HTML(s$note)))))
    })
    tagList(blocks)
  })

  # ---- plots
  st_plot_types <- function(test) switch(test,
    ttest = c("Box plot" = "box", "Violin plot" = "violin"),
    anova1 = c("Bar chart with letters" = "bars", "Box plot with letters" = "box", "Residual diagnostics" = "diag"),
    anova2 = c("Interaction plot" = "inter", "Grouped bars with letters" = "bars2", "Residual diagnostics" = "diag"),
    ancova = c("ANCOVA lines" = "ancova", "Residual diagnostics" = "diag"),
    kruskal = c("Box plot" = "box", "Violin plot" = "violin"),
    reg = c("Fitted curve" = "reg", "Residual diagnostics" = "diag"),
    kin = c("Kinetic model fits" = "kin"),
    desc = c("Box plot" = "box", "Violin plot" = "violin"),
    cor = c("Scatter-plot matrix" = "pairs"))
  output$st_plot_resp_ui <- renderUI({ r <- st_result(); selectInput("st_plot_resp", "Plot for", names(r$items)) })
  output$st_plot_type_ui <- renderUI({ r <- st_result(); selectInput("st_plot_type", "Plot type", st_plot_types(r$test)) })

  st_plot_obj <- reactive({
    r <- st_result(); req(input$st_plot_resp, input$st_plot_type)
    it <- r$items[[input$st_plot_resp]]; validate(need(is.null(it$error), it$error))
    df <- st_data(); ty <- input$st_plot_type; y <- input$st_plot_resp
    switch(ty,
      box = , violin = {
        g <- input$st_A; if (is.null(g) || !nzchar(g)) { df$All <- "All"; g <- "All" }
        lt <- if (r$test %in% c("anova1")) it$letters_A else NULL
        if (r$test == "desc") { y <- input$st_y[1] }
        esc_plot_box(df, y, g, lt, violin = ty == "violin")
      },
      bars = esc_plot_means_letters(it$letters_A, ylab = y, xlab = input$st_A, error = toupper(input$spread_type %||% "se")),
      inter = esc_plot_interaction(it, ylab = y, error = input$spread_type %||% "se"),
      bars2 = esc_plot_twoway_bars(it, ylab = y, error = input$spread_type %||% "se"),
      ancova = esc_plot_ancova(it, ylab = y),
      diag = esc_plot_diagnostics(if (!is.null(it$lm)) it$lm else it$fit),
      reg = esc_plot_regression(it, xlab = input$st_x, ylab = y),
      kin = esc_plot_kinetics(it, xlab = input$st_x, ylab = y),
      pairs = GGally::ggpairs(df[, input$st_y, drop = FALSE]) + esc_theme())
  })
  output$st_plot <- esc_renderPlot({ st_plot_obj() })

  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) local({
    f <- fmt
    output[[paste0("dl_stats_", f)]] <- downloadHandler(
      filename = function() paste0("ESCAP_", input$uni_analysis, "_", input$st_plot_type, ".", f),
      content = function(file) {
        p <- st_plot_obj(); if (inherits(p, "ggplot")) p <- esc_apply_style(p, esc_style_now())
        esc_open_device(file, f, input$dl_stats_w %||% 8, input$dl_stats_h %||% 6, input$dl_stats_dpi %||% 600)
        on.exit(grDevices::dev.off(), add = TRUE); esc_draw(p)
      }, contentType = esc_mime[[f]])
  })

  register_report("rep_stats", function() {
    r <- st_result(); pt <- tryCatch(st_pub(), error = function(e) NULL)
    secs <- unlist(lapply(names(r$items), function(nm) {
      tabs <- st_tables(r$items[[nm]], r$test)
      lapply(tabs, function(s) { s$heading <- paste0(nm, ": ", s$heading); s$html <- FALSE; s })
    }), recursive = FALSE)
    if (!is.null(pt) && is.null(pt$error)) secs <- c(list(list(heading = "Publication-ready table", table = pt$table,
                                                               note = htmltools::htmlEscape(pt$note))),
                                                     if (!is.null(pt$summary)) list(list(heading = "ANOVA summary table", table = pt$summary)),
                                                     secs)
    list(title = r$label, settings = c(r$settings, Spread = toupper(input$spread_type %||% "sd")), sections = secs)
  })
