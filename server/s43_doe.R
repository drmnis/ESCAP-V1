# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s43_doe  (sourced inside server(), local = TRUE)
# Design of Experiments. Engine: R/doe.R; plots: R/doe_plots.R.

  split_csv <- function(x) trimws(strsplit(x %||% "", ",")[[1]])

  dz_result <- eventReactive(input$dz_make, {
    k <- input$dz_k; nm <- split_csv(input$dz_names)
    if (length(nm) < k) nm <- c(nm, LETTERS[seq_len(k)][-(seq_along(nm))])[seq_len(k)]
    nm <- make.unique(nm[seq_len(k)])
    lo <- suppressWarnings(as.numeric(split_csv(input$dz_low))); hi <- suppressWarnings(as.numeric(split_csv(input$dz_high)))
    if (length(lo) < k || any(is.na(lo))) lo <- rep_len(c(lo[is.finite(lo)], -1), k)
    if (length(hi) < k || any(is.na(hi))) hi <- rep_len(c(hi[is.finite(hi)], 1), k)
    lo <- lo[seq_len(k)]; hi <- hi[seq_len(k)]
    validate(need(all(hi > lo), "Every high level must be larger than its low level."))
    r <- tryCatch(esc_design(input$dz_type, k, nm, lo, hi, levels = as.integer(split_csv(input$dz_levels)),
                             center = input$dz_center, reps = input$dz_reps, p = input$dz_p, alpha = input$dz_alpha,
                             pb_runs = as.integer(input$dz_pbruns), mix_m = input$dz_mixm, dopt_runs = input$dz_druns,
                             dopt_model = input$dz_dmodel, randomize = isTRUE(input$dz_random), seed = input$dz_seed),
                  error = function(e) validate(need(FALSE, conditionMessage(e))))
    for (y in split_csv(input$dz_resp)) if (nzchar(y)) r$design[[y]] <- NA_real_
    r
  })

  output$dz_info <- renderUI({
    r <- dz_result(); i <- r$info
    txt <- c(sprintf("<b>%d runs</b>", i$runs),
             if (!is.null(i$alpha)) sprintf("axial distance alpha = %s", i$alpha),
             if (!is.null(i$generators)) sprintf("generators: %s; defining relation: %s; resolution %s", i$generators, i$defining, i$resolution),
             if (!is.null(i$D)) sprintf("D-efficiency criterion = %s", i$D),
             if (r$mixture) "mixture proportions sum to 1")
    HTML(paste0("<p>", paste(txt, collapse = " &nbsp;|&nbsp; "), "</p>"))
  })
  output$dz_table <- DT::renderDataTable({
    d <- dz_result()$design
    DT::datatable(d, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, dom = "tip")) |>
      DT::formatRound(which(vapply(d, is.numeric, logical(1)) & !names(d) %in% c("Run", "Std")), 4)
  })
  dz_design_plot <- function() {
    d <- dz_result()$design
    fn <- setdiff(names(d)[-(1:2)], names(d)[grepl("coded", names(d))])
    fn <- fn[vapply(d[fn], function(x) is.numeric(x) && !all(is.na(x)), TRUE)]
    validate(need(length(fn) >= 2, "The design needs at least two factors to plot."))
    pd <- d[, fn[1:2]]; names(pd) <- c("a", "b")
    ggplot(pd, aes(a, b)) + geom_count(colour = "#0072B2", alpha = 0.8) + scale_size_area(max_size = 7, name = "Runs") +
      labs(x = fn[1], y = fn[2], title = "Design points (first two factors; size = replicated runs)") + esc_theme()
  }
  output$dz_csv <- downloadHandler(filename = function() paste0("ESCAP_design_", input$dz_type, ".csv"),
    content = function(file) utils::write.csv(dz_result()$design, file, row.names = FALSE, na = ""))
  output$dz_xlsx <- downloadHandler(filename = function() paste0("ESCAP_design_", input$dz_type, ".xlsx"),
    content = function(file) {
      d <- dz_result()$design
      if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(d, file)
      else if (requireNamespace("openxlsx", quietly = TRUE)) openxlsx::write.xlsx(d, file)
      else stop("Install the 'writexl' package for Excel export, or use the CSV download.")
    })

  # ------------------------------------------------------------------ analysis
  output$da2_cols_ui <- renderUI({
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) return(helpText("Upload the completed design in the sidebar (univariate upload)."))
    num <- names(df)[vapply(df, is.numeric, logical(1))]; num <- setdiff(num, c("Run", "Std"))
    num <- num[!grepl("\\.\\.coded\\.$|\\(coded\\)", num)]
    tagList(selectizeInput("dx_factors", "Factor columns (actual units)", num, head(num, 2), multiple = TRUE, options = list(plugins = list("remove_button"))),
            selectizeInput("dx_resp", "Response column(s)", num, tail(num, 1), multiple = TRUE, options = list(plugins = list("remove_button"))))
  })

  dx_models <- eventReactive(input$dx_run, {
    df <- dat_raw(); req(input$dx_factors, input$dx_resp)
    validate(need(length(intersect(input$dx_factors, input$dx_resp)) == 0, "A column cannot be both a factor and a response."))
    ms <- lapply(input$dx_resp, function(y) tryCatch(esc_doe_fit(df, y, input$dx_factors, input$dx_model, "auto",
                       reduce = isTRUE(input$dx_reduce), alpha = input$dx_alpha %||% 0.1, hierarchy = isTRUE(input$dx_hier)),
                     error = function(e) list(error = conditionMessage(e), y = y)))
    names(ms) <- input$dx_resp
    state$current_analysis <- "doe"
    ms
  })

  dx_tables <- function(m, nm) {
    if (!is.null(m$error)) return(list(list(heading = paste(nm, ": error"), table = data.frame(Message = m$error))))
    an <- m$anova; for (cc in c("SS", "MS", "F")) an[[cc]] <- ifelse(is.na(an[[cc]]), "", formatC(an[[cc]], digits = 4, format = "g"))
    an$P <- esc_fmtp(an$P)
    co <- m$coef; for (cc in c("Coefficient", "SE", "t", "95% CI low", "95% CI high", "VIF")) co[[cc]] <- ifelse(is.na(co[[cc]]), "", formatC(co[[cc]], digits = 4, format = "g"))
    co$P <- esc_fmtp(m$coef$P)
    c(list(list(heading = paste0(nm, ": fit statistics"), table = m$fit_stats),
           list(heading = paste0(nm, ": ANOVA (partial sums of squares, Type III)"), table = an,
                note = "Lack of fit is tested against pure error from replicated runs (e.g. centre points)."),
           list(heading = paste0(nm, ": coefficients (", if (m$mixture) "pseudo-components" else "coded factors, -1 to +1", ")"), table = co,
                note = paste("Final equation (coded):", htmltools::htmlEscape(m$equation)))),
      if (!is.null(m$canonical)) list(list(heading = paste0(nm, ": canonical analysis (", m$canonical$nature,
          if (m$canonical$inside) ", inside the design region" else ", outside the design region", ")"),
          table = cbind(m$canonical$table, Eigenvalue = m$canonical$eigen$Eigenvalue),
          note = sprintf("Predicted response at the stationary point: %.4g", m$canonical$predicted))))
  }

  output$dx_results <- renderUI({
    ms <- dx_models()
    tagList(lapply(names(ms), function(nm) tagList(h4(style = "color:#0A57B5; margin-top:14px;", nm),
      lapply(dx_tables(ms[[nm]], nm), function(s) tagList(h5(strong(s$heading)), div(style = "overflow-x:auto;", HTML(esc_df_to_html(s$table))),
                                                         if (!is.null(s$note)) tags$small(HTML(s$note)))))))
  })

  output$dx_goals_ui <- renderUI({
    ms <- tryCatch(dx_models(), error = function(e) NULL)
    if (is.null(ms)) return(helpText("Fit the models first."))
    tagList(lapply(names(ms), function(nm) {
      m <- ms[[nm]]; if (!is.null(m$error)) return(NULL)
      y <- m$data[[m$y]]; id <- make.names(nm)
      tagList(strong(nm), fluidRow(
        column(6, selectInput(paste0("dx_goal_", id), NULL, c("Maximise" = "maximize", "Minimise" = "minimize", "Target" = "target", "Ignore" = "none"))),
        column(3, numericInput(paste0("dx_lo_", id), "Low", signif(min(y), 4))),
        column(3, numericInput(paste0("dx_hi_", id), "High", signif(max(y), 4)))),
        numericInput(paste0("dx_tg_", id), "Target (if Target)", signif(mean(y), 4)))
    }))
  })

  dx_opt <- eventReactive(input$dx_opt, {
    ms <- Filter(function(m) is.null(m$error), dx_models()); validate(need(length(ms) > 0, "No valid models."))
    validate(need(!any(vapply(ms, `[[`, TRUE, "mixture")), "Desirability optimisation is available for process-factor models."))
    ids <- make.names(names(ms))
    goals <- vapply(ids, function(i) input[[paste0("dx_goal_", i)]] %||% "maximize", "")
    lows <- vapply(ids, function(i) input[[paste0("dx_lo_", i)]] %||% NA_real_, 0)
    highs <- vapply(ids, function(i) input[[paste0("dx_hi_", i)]] %||% NA_real_, 0)
    tg <- vapply(ids, function(i) input[[paste0("dx_tg_", i)]] %||% NA_real_, 0)
    validate(need(all(highs > lows), "For every response the High limit must exceed the Low limit."))
    o <- esc_desirability(unname(ms), goals, lows, highs, tg)
    o$settings$Factor <- vapply(o$settings$Factor, function(f) esc_lab(ms[[1]], f), "")
    o
  })
  output$dx_opt_results <- renderUI({
    o <- dx_opt()
    tagList(h4(style = "color:#0A57B5;", sprintf("Optimum (overall desirability D = %.3f)", o$desirability)),
            HTML(esc_df_to_html(st_fmt(o$settings))), HTML(esc_df_to_html(st_fmt(o$responses))),
            tags$small("Derringer-Suich desirability; multi-start bounded optimisation within the experimental region. Confirm the optimum experimentally."))
  })

  output$dx_plot_resp_ui <- renderUI({ ms <- dx_models(); selectInput("dx_plot_resp", "Response", names(ms)) })
  output$dx_f1_ui <- renderUI({ ms <- dx_models(); m <- ms[[1]]; if (!is.null(m$error)) return(NULL)
    selectInput("dx_f1", "X factor", stats::setNames(m$factors, vapply(m$factors, function(f) esc_lab(m, f), "")), m$factors[1]) })
  output$dx_f2_ui <- renderUI({ ms <- dx_models(); m <- ms[[1]]; if (!is.null(m$error)) return(NULL)
    selectInput("dx_f2", "Y factor", stats::setNames(m$factors, vapply(m$factors, function(f) esc_lab(m, f), "")), m$factors[min(2, length(m$factors))]) })

  observeEvent(input$dx_run, if (identical(input$dx_plot, "design")) updateSelectInput(session, "dx_plot", selected = "contour"), ignoreInit = TRUE)

  # How to get DoE graphs (shown until models are fitted)
  output$dx_graph_help <- renderUI({
    fitted <- !is.null(tryCatch(dx_models(), error = function(e) NULL))
    made <- !is.null(tryCatch(dz_result(), error = function(e) NULL))
    if (fitted) return(NULL)
    div(class = "esc-hint", HTML(paste0(
      "<b>To see DoE graphs:</b> (1) upload your completed experiment (factor columns + measured responses) in ",
      "<i>Univariate analysis</i>; (2) click <i>DoE (design of experiments)</i> and choose <i>2. Analyse and optimise</i>; ",
      "(3) select the factor and response columns and click <i>Fit models</i>. All graphs below then work; contour, 3-D surface, ",
      "interaction and desirability need at least two factors, desirability also needs <i>Optimise</i>.",
      if (made) "<br>Only <i>Design points</i> is available now (from the design you generated)." else
        "<br>Example file: <i>example_data/12_doe_CCD_ultrasound_extraction.csv</i>.")))
  })

  dx_plot_obj <- reactive({
    if (identical(input$dx_plot, "design")) {
      if (!is.null(tryCatch(dz_result(), error = function(e) NULL))) return(dz_design_plot())
      ms <- tryCatch(dx_models(), error = function(e) NULL)
      validate(need(!is.null(ms), "Generate a design (step 1) or fit models on your completed design (step 2) first."))
      m <- Filter(function(x) is.null(x$error), ms)[[1]]
      validate(need(length(m$factors) >= 2, "Design points need at least two factors."))
      d <- dat_raw(); fa <- input$dx_factors[1:2]
      pd <- data.frame(a = d[[fa[1]]], b = d[[fa[2]]])
      return(ggplot(pd, aes(a, b)) + geom_count(colour = "#0072B2", alpha = 0.8) + scale_size_area(max_size = 7, name = "Runs") +
               labs(x = fa[1], y = fa[2], title = "Design points (first two factors; size = replicated runs)") + esc_theme())
    }
    ms <- dx_models(); req(input$dx_plot_resp); m <- ms[[input$dx_plot_resp]]
    validate(need(is.null(m$error), m$error))
    f1 <- input$dx_f1 %||% m$factors[1]; f2 <- input$dx_f2 %||% m$factors[min(2, length(m$factors))]
    two <- function() validate(need(f1 != f2 && length(m$factors) >= 2, "Choose two different factors."))
    switch(input$dx_plot,
      contour = { two(); esc_plot_doe_contour(m, f1, f2) },
      surface = { two(); esc_plot_doe_surface(m, f1, f2, theta = input$dx_theta %||% 35) },
      pareto = esc_plot_doe_pareto(m), main = esc_plot_doe_main(m), perturb = esc_plot_doe_perturbation(m),
      inter = { two(); esc_plot_doe_interaction(m, f1, f2) },
      pva = esc_plot_doe_pred_actual(m), diag = esc_plot_diagnostics(m$fit),
      halfnorm = esc_plot_doe_halfnormal(m), runorder = esc_plot_doe_runorder(m),
      ternary = { validate(need(m$mixture && length(m$factors) == 3, "Ternary plots need a three-component mixture model.")); esc_plot_mixture(m) },
      desir = { two(); o <- dx_opt(); esc_plot_desirability(Filter(function(x) is.null(x$error), ms), o, f1, f2) })
  })
  output$dx_plot_out <- esc_renderPlot({ p <- dx_plot_obj(); if (is.function(p)) { p(); invisible(NULL) } else p })

  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) local({
    f <- fmt
    output[[paste0("dl_doe_", f)]] <- downloadHandler(
      filename = function() paste0("ESCAP_DoE_", input$dx_plot, "_", input$dx_plot_resp, ".", f),
      content = function(file) {
        p <- dx_plot_obj(); if (inherits(p, "ggplot")) p <- esc_apply_style(p, esc_style_now())
        esc_open_device(file, f, input$dl_doe_w %||% 8, input$dl_doe_h %||% 6, input$dl_doe_dpi %||% 600)
        on.exit(grDevices::dev.off(), add = TRUE); esc_draw(p)
      }, contentType = esc_mime[[f]])
  })

  register_report("rep_doe", function() {
    ms <- dx_models()
    secs <- unlist(lapply(names(ms), function(nm) dx_tables(ms[[nm]], nm)), recursive = FALSE)
    o <- tryCatch(dx_opt(), error = function(e) NULL)
    if (!is.null(o)) secs <- c(secs, list(list(heading = sprintf("Optimum settings (desirability D = %.3f)", o$desirability), table = st_fmt(o$settings)),
                                          list(heading = "Predicted responses at the optimum", table = st_fmt(o$responses))))
    list(title = "Design of Experiments: response surface analysis",
         settings = c(Model = input$dx_model, Factors = paste(input$dx_factors, collapse = ", ")), sections = secs)
  })
