# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s44_dispatch  (sourced inside server(), local = TRUE)
# One "Run analysis" button for every univariate analysis; navigation to the right
# Results / Graphs tab; graph type and download routing for the added graphs.

  trig_anova <- reactiveVal(NULL); trig_chi <- reactiveVal(NULL); trig_stats <- reactiveVal(NULL)
  bump <- function(rv) rv((rv() %||% 0) + 1)
  go_results <- function(sub) {
    updateTabsetPanel(session, "main_tabs", selected = "Results")
    updateTabsetPanel(session, "results_subtab", selected = sub)
  }
  observeEvent(input$uni_run, {
    a <- input$uni_analysis %||% "anova1"
    if (a == "chi") { bump(trig_chi); go_results("Univariate") }
    else { bump(trig_stats); go_results("Univariate") }
  })
  observeEvent(input$da_run, go_results("PLS-DA"), ignoreInit = TRUE)
  observeEvent(input$doe_open, if (input$doe_open %% 2 == 1) go_results("DoE"), ignoreInit = TRUE)
  observeEvent(input$dz_make, go_results("DoE"), ignoreInit = TRUE)
  observeEvent(input$dx_run, go_results("DoE"), ignoreInit = TRUE)

  # graph type now also covers the added General graphs and the PLS-DA / DoE subtabs
  graph_type_v1 <- graph_type
  graph_type <- reactive({
    tab <- input$graphs_subtab
    if (identical(tab, "PLS-DA")) return(paste0("da_", input$da_plot %||% "err"))
    if (identical(tab, "DoE")) return(paste0("dx_", input$dx_plot %||% "contour"))
    if (identical(tab, "General") && (identical(input$graph_type_uni, "st_plot") || grepl("^ps_", input$graph_type_uni %||% "")))
      return(input$graph_type_uni)
    graph_type_v1()
  })
  # added PCA / PLSR diagnostic graphs (R/extra_plots.R)
  extra_plot_obj <- reactive({
    gt <- graph_type()
    if (gt %in% c("pca_qt", "pca_contrib", "pca_cumvar")) {
      r <- pca_result(); k <- max(as.integer(strsplit(input$pca_pc_pair %||% "1_2", "_")[[1]]))
      return(switch(gt, pca_qt = esc_plot_pca_qt(r$pca, k, r$Class, r$ID), pca_contrib = esc_plot_pca_contrib(r$pca, k),
                    pca_cumvar = esc_plot_pca_cumvar(r$pca)))
    }
    e <- plsr_result()$engine
    switch(gt, plsr_resid = esc_plot_plsr_resid(e), plsr_var = esc_plot_plsr_var(e$block_var, e$lv),
           plsr_weights = esc_plot_pls_weights(e$fit, 1:min(2, e$max_lv), bar = identical(input$pca_load_profile_type, "bar")))
  })
  draw_current_graph_v1 <- draw_current_graph
  draw_current_graph <- function() {
    gt <- graph_type()
    p <- if (identical(gt, "st_plot")) st_plot_obj()
         else if (grepl("^ps_", gt)) ps_plot_obj()
         else if (gt %in% ESC_EXTRA_GRAPHS) extra_plot_obj()
         else if (grepl("^da_", gt)) da_plot_obj()
         else if (grepl("^dx_", gt)) dx_plot_obj()
         else return(draw_current_graph_v1())
    if (inherits(p, "ggplot")) p <- esc_apply_style(p, esc_style_now())
    esc_draw(p)
  }


  # The plot object on screen, unstyled (used to offer facet variables)
  current_graph_object <- function() {
    gt <- graph_type()
    p <- if (identical(gt, "st_plot")) st_plot_obj()
         else if (grepl("^ps_", gt)) ps_plot_obj()
         else if (gt %in% ESC_EXTRA_GRAPHS) extra_plot_obj()
         else if (grepl("^da_", gt)) da_plot_obj()
         else if (grepl("^dx_", gt)) dx_plot_obj()
         else NULL
    if (!is.null(p)) return(p)
    esc_capture$p <- NULL; esc_capture$on <- TRUE
    on.exit(esc_capture$on <- FALSE, add = TRUE)
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    try(draw_current_graph_v1(), silent = TRUE)
    esc_capture$p
  }
