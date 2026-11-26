# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s07_univariate_inputs  (sourced inside server(), local = TRUE)

  dat_raw <- reactive({
    req(data_store$uni_raw, data_store$uni_keep)
    data_store$uni_raw[data_store$uni_keep, , drop = FALSE]
  })
  
  
  # 2. Column roles used by the univariate graphs ----------------------------
  # The three old selectors (condition, treatment/factor, responses) were replaced by the
  # settings of the chosen analysis (sidebar > Analysis). Graphs read the roles from here.
  uni_resp <- reactive({
    df <- dat_raw(); num <- names(df)[vapply(df, is.numeric, logical(1))]
    y <- input$st_y; y <- y[y %in% num]
    if (length(y)) y else num
  })
  uni_factors <- reactive({
    df <- dat_raw()
    f <- c(input$st_A, if (identical(input$uni_analysis, "anova2")) input$st_B)
    f <- unique(f[!is.na(f) & nzchar(f) & f %in% names(df)])
    if (!length(f)) {                         # fallback: first text column
      cand <- names(df)[!vapply(df, is.numeric, logical(1))]
      f <- head(cand, 1)
    }
    f
  })
  # original column headers (e.g. "RH (%)"), keyed by the cleaned names, for axis labels
  uni_orig_names <- reactive({
    o <- data_store$uni_orig; df <- dat_raw()
    if (is.null(o)) return(stats::setNames(names(df), names(df)))
    o[names(df)]
  })
  uni_cond <- reactive({
    cc <- input$st_cond
    if (is.null(cc) || !nzchar(cc) || !cc %in% names(dat_raw())) "" else cc
  })

  primary_factor <- reactive({
    fc <- uni_factors()
    if (length(fc) < 1) return(NULL)
    fc[1]
  })

  secondary_factor <- reactive({
    fc <- uni_factors()
    if (length(fc) < 2) return(NULL)
    fc[2]
  })

  # Hint above the General graphs: which columns the graphs use and what each graph needs
  output$graph_roles_hint <- renderUI({
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) return(div(class = "esc-hint", "Upload univariate data in the sidebar first."))
    f <- tryCatch(uni_factors(), error = function(e) character(0)); y <- tryCatch(uni_resp(), error = function(e) character(0))
    cc <- tryCatch(uni_cond(), error = function(e) "")
    need <- switch(input$graph_type_uni %||% "",
      linear_group = "Grouped bar plots need two factors: choose Two-way ANOVA in Analysis and set Factor A and Factor B.",
      radial = "Circular bar plots need an ID column: choose One-way ANOVA and set Condition / species / ID.",
      simple_bar = , ggbetween = "These plots use the treatment factor and the response variable(s) of the chosen analysis.",
      st_plot = "Run an analysis first; this shows the plots for that analysis.",
      NULL)
    div(class = "esc-hint",
        HTML(paste0("<b>Columns used:</b> responses = ", htmltools::htmlEscape(paste(utils::head(y, 6), collapse = ", ")),
                    if (length(y) > 6) ", ..." else "",
                    "; factor(s) = ", htmltools::htmlEscape(if (length(f)) paste(f, collapse = " \u00d7 ") else "none"),
                    "; condition / ID = ", htmltools::htmlEscape(if (nzchar(cc)) cc else "none"),
                    ". Change them under <i>Univariate analysis \u2192 Analysis</i>.")),
        if (!is.null(need)) tags$div(style = "margin-top:3px;", need))
  })

  # plot selector
  # Selector for Graphs -> General tab
  output$general_selector_ui <- renderUI({
    req(input$graph_type_uni)
    
    
    # NEW: selector for Density & Histogram
    if (input$graph_type_uni == "ggpairs") {
      df <- tryCatch(dat_raw(), error = function(e) NULL)
      if (is.null(df)) return(NULL)
      vars <- uni_resp()
      vars <- vars[vars %in% names(df)]
      if (length(vars) < 1) return(NULL)
      
      # If facet is used, there is only ONE plot
      n_choices <- if (isTRUE(input$dens_hist_use_facet)) 1L else length(vars)
      
      return(
        selectInput(
          "simple_bar_which",
          label    = "Select to view & download",
          choices  = as.list(seq_len(n_choices)),
          selected = 1,
          width    = "220px"
        )
      )
    }
    
    
    
    # 1) PCA dendrogram
    if (input$graph_type_uni == "pca_dendrogram") {
      return(
        selectInput(
          "simple_bar_which",
          label    = "Choose using dropdown",
          choices  = as.list(1:7),
          selected = 1,
          width    = "220px"
        )
      )
    }
    
    # 2) Heatmaps
    if (input$graph_type_uni == "heatmaps") {
      return(
        selectInput(
          "simple_bar_which",
          label    = "Choose using dropdown",
          choices  = as.list(1:6),
          selected = 1,
          width    = "220px"
        )
      )
    }
    
    # 3) Simple bar / Grouped bar
    if (input$graph_type_uni %in% c("simple_bar", "linear_group")) {
      d <- tryCatch(
        {
          if (input$graph_type_uni == "simple_bar") {
            simple_bar_data()
          } else {
            linear_group_data()
          }
        },
        error = function(e) NULL
      )
      if (is.null(d)) return(NULL)
      
      plots <- if (input$graph_type_uni == "simple_bar") {
        simple_bar_plots()
      } else {
        linear_group_plots()
      }
      n <- length(plots)
      if (n < 1) return(NULL)
      
      return(
        selectInput(
          "simple_bar_which",
          label    = "Choose Plot # to download",
          choices  = as.list(seq_len(n)),
          selected = 1,
          width    = "220px"
        )
      )
    }
    
    # For pairs, radial, violin: no selector
    NULL
  })
  
  # Selector for Graphs -> Correlation tab
  output$corr_selector_ui <- renderUI({
    req(input$graph_type_corr)
    if (!input$graph_type_corr %in% c("corr_spearman", "corr_pearson")) {
      return(NULL)
    }
    
    selectInput(
      "simple_bar_which",
      label    = "Choose using dropdown",
      choices  = as.list(1:5),
      selected = 1,
      width    = "220px"
    )
  })
  
  
  
  
