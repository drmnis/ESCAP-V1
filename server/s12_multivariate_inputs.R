# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s12_multivariate_inputs  (sourced inside server(), local = TRUE)

  observeEvent(input$file_multi, {
    req(input$file_multi)
    df <- tryCatch(esc_read_data(input$file_multi$datapath, input$file_multi$name),
                   error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error"); NULL })
    req(df)
    validate(
      need(is.data.frame(df), "Multivariate file is not a data.frame."),
      need(ncol(df) >= 2, "Need at least two columns for PCA/Correlation.")
    )
    data_store$multi_raw  <- df
    data_store$multi_keep <- seq_len(nrow(df))  # keep all initially
  })
  
  dat_multi <- reactive({
    req(data_store$multi_raw, data_store$multi_keep)
    data_store$multi_raw[data_store$multi_keep, , drop = FALSE]
  })
  
  
  
  # 0\u20131 column-wise normalization
  normalize_01 <- function(m) {
    apply(m, 2, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
        return(rep(0, length(x)))
      }
      (x - rng[1]) / diff(rng)
    })
  }
  
  # 0\u20131 column-wise normalization (unchanged)
  normalize_01 <- function(m) {
    apply(m, 2, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
        return(rep(0, length(x)))
      }
      (x - rng[1]) / diff(rng)
    })
  }
  
  
  # Use UNIVARIATE data + selected response columns
  heatmap_matrix <- reactive({
    # If heatmaps are requested from correlation results
    if (heatmap_state$source == "corr" && !is.null(heatmap_state$corr_mat)) {
      m <- as.matrix(heatmap_state$corr_mat)
      # ensure row/col names exist
      if (is.null(rownames(m))) rownames(m) <- colnames(m)
      if (is.null(colnames(m))) colnames(m) <- rownames(m)
      m_norm <- normalize_01(m)
      return(m_norm)
    }
    
    # --- original behavior (Univariate data) ---
    df <- dat_raw()
    req(uni_resp())
    
    # keep only the selected response columns
    cols <- uni_resp()[uni_resp() %in% names(df)]
    validate(
      need(length(cols) >= 1, "Heatmaps: select at least one response variable.")
    )
    
    df_num <- df[, cols, drop = FALSE]
    
    # coerce to numeric
    for (nm in cols) {
      df_num[[nm]] <- suppressWarnings(as.numeric(df_num[[nm]]))
    }
    
    # remove rows that are all NA
    keep_rows <- rowSums(!is.na(df_num)) > 0
    df_num <- df_num[keep_rows, , drop = FALSE]
    
    validate(
      need(nrow(df_num) >= 1, "Heatmaps: no non\u2011missing values in selected response columns.")
    )
    
    m <- as.matrix(df_num)
    m_norm <- normalize_01(m)
    rownames(m_norm) <- rownames(m)
    colnames(m_norm) <- colnames(m)
    m_norm
  })
  
  
  heatmap_matrix_raw <- reactive({
    # If heatmaps are requested from correlation results
    if (heatmap_state$source == "corr" && !is.null(heatmap_state$corr_mat)) {
      m <- as.matrix(heatmap_state$corr_mat)
      if (is.null(rownames(m))) rownames(m) <- colnames(m)
      if (is.null(colnames(m))) colnames(m) <- rownames(m)
      return(m)
    }
    
    # --- original behavior (Univariate data) ---
    df <- dat_raw()
    req(uni_resp())
    
    cols <- uni_resp()[uni_resp() %in% names(df)]
    validate(
      need(length(cols) >= 1, "Heatmaps: select at least one response variable.")
    )
    
    df_num <- df[, cols, drop = FALSE]
    for (nm in cols) {
      df_num[[nm]] <- suppressWarnings(as.numeric(df_num[[nm]]))
    }
    
    # same row filtering as above
    keep_rows <- rowSums(!is.na(df_num)) > 0
    df_num <- df_num[keep_rows, , drop = FALSE]
    
    as.matrix(df_num)
  })
  
  
  output$linear_group_all <- esc_renderPlot({
    req(input$graph_type_uni == "linear_group")
    plots <- linear_group_plots()
    patchwork::wrap_plots(plots, ncol = 2)
  })
  
  
  # ID column: or use serial number if "none"
  output$multi_id_ui <- renderUI({
    df <- dat_multi()
    selectInput(
      "multi_id_col",
      "Select ID (optional):",
      choices  = c("None (use serial number)" = "", names(df)),
      selected = ""
    )
  })
  
  # Class column (e.g. Species)
  output$multi_class_ui <- renderUI({
    req(input$file_multi) # Ensure file is uploaded
    df <- dat_multi()
    
    selectizeInput(
      "multi_class_col",
      "Select Class:",
      choices  = c("None" = "", names(df)),
      selected = ""
    )
  })
  
  output$plsr_group_col_ui <- renderUI({
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    selectInput(
      "plsr_group_col",
      "Group column (Cal / Pred):",
      choices  = names(df),
      selected = names(df)[1]
    )
  })
  
  
  # Update the selectizeInput when the file is uploaded.
  # Test data loading
  # (debug observer that re-read the uploaded file was removed for speed)
  
  
  # Variable range selector (by column index)
  output$multi_vars_ui <- renderUI({
    df <- dat_multi()
    n <- ncol(df)
    num_cols <- names(df)[sapply(df, function(z) suppressWarnings(!all(is.na(as.numeric(z)))))]
    
    tagList(
      sliderInput(
        "multi_var_range", "Variable columns (range by index):",
        min = 1, max = n, value = c(1, min(4, n)), step = 1
      ),
      helpText("Use the slider to choose which columns (by index) are treated as numeric variables."),
      
      # --- PLSR \u2013 Y and PLS-DA \u2013 class side by side; both share the options below ---
      fluidRow(
        column(6, checkboxInput("show_plsr_y_section", label = strong("PLSR \u2013 Y"), value = FALSE)),
        column(6, checkboxInput("show_da_section", label = strong("PLS-DA \u2013 class"), value = FALSE))
      ),
      fluidRow(
        column(6, conditionalPanel(
          condition = "input.show_plsr_y_section",
          selectInput("plsr_y_col", "Y (response) column:", choices = num_cols, selected = num_cols[1])
        )),
        column(6, conditionalPanel(
          condition = "input.show_da_section",
          uiOutput("da_class_ui")
        ))
      ),
      tags$hr(style = "margin-top:8px; margin-bottom:8px;"),

      # --- Shared PLSR / PLS-DA options ---
      conditionalPanel(
        condition = "input.show_plsr_y_section || input.show_da_section",

        checkboxInput("show_plsr_lv_section", label = strong("Number of latent variables (LV)"), value = TRUE),
        conditionalPanel(
          condition = "input.show_plsr_lv_section",
          sliderInput("plsr_lv_user", "Number of latent variables to use (optional):", min = 0, max = 15, value = 0, step = 1),
          helpText("0 = automatic. PLSR shows performance for all LVs (up to 15); PLS-DA picks the LV with the lowest CV error.")
        ),
        tags$hr(style = "margin-top:8px; margin-bottom:8px;"),

        checkboxInput("show_plsr_sets_section", label = strong("Calibration / prediction sets"), value = TRUE),
        conditionalPanel(
          condition = "input.show_plsr_sets_section",
          radioButtons("plsr_split_type", "How to define sets:",
                       choices = c("Use a column (e.g., Cal / Pred)" = "column", "Random split (by proportion)" = "random"),
                       selected = "random"),
          conditionalPanel(
            condition = "input.plsr_split_type == 'column'",
            uiOutput("plsr_group_col_ui"),
            helpText("Group column must contain exactly two levels (e.g., 'Cal' and 'Pred').")
          ),
          conditionalPanel(
            condition = "input.plsr_split_type == 'random'",
            sliderInput("plsr_cal_prop", "Calibration proportion:", min = 0.5, max = 1, value = 0.7, step = 0.05),
            helpText("PLS-DA splits each class in this proportion (stratified).")
          )
        ),
        conditionalPanel(
          condition = "input.show_da_section",
          tags$hr(style = "margin-top:8px; margin-bottom:8px;"),
          numericInput("da_perm", "PLS-DA permutation tests (0 = none)", 0, 0, 1000, 50)
        )
      )
    )
  })
  
  
  
  output$plsr_qt_click_debug <- renderPrint({
    # Only show when that graph is active
    if (!identical(input$plsr_graph_type, "q_vs_t2")) {
      return(invisible())
    }
    ids <- unique(plsr_qt_selected_ids())
    if (length(ids) == 0) {
      cat("Click sample(s) in the Q vs T\u00b2 plot to build a selection.\n")
    } else {
      cat("Selected Sample ID(s):", paste(ids, collapse = ", "), "\n")
    }
  })
  
  
  
  # Preprocessing options (combinable) -------------------------------------
  output$multi_prep_ui <- renderUI({
    
    req(input$file_multi)
    
    tagList(
      # --- Preprocessing header + show/hide ---
      checkboxInput(
        "show_prep_section",
        label = strong("Preprocessing"),
        value = TRUE
      ),
      conditionalPanel(
        condition = "input.show_prep_section",
        radioButtons(
          "prep_spectral",
          label = NULL,
          choices = c(
            "SNV"            = "snv",
            "MSC"            = "msc",
            "Savitzky\u2013Golay" = "sg",
            "None"           = "none"
          ),
          selected = "none"
        ),
        tags$hr(style = "margin-top:8px; margin-bottom:8px;"),
        
        # ---- Scaling + Centering on same row ----
        fluidRow(
          column(
            width = 6,
            h5(strong("Scaling")),
            radioButtons(
              "prep_scaling",
              label = NULL,
              choices = c(
                "IQR"        = "iqr",
                "Range"      = "range",
                "Pareto"     = "pareto",
                "Max\u2013min"    = "maxmin",
                "Log"        = "log",
                "Auto"       = "auto",
                "None"       = "none"
              ),
              selected = "auto"
            )
          ),
          column(
            width = 6,
            h5(strong("Centering")),
            radioButtons(
              "prep_centering",
              label = NULL,
              choices = c(
                "Mean"   = "mc",
                "Median" = "median",
                "None"   = "none"
              ),
              selected = "none"
            )
          )
        ),
        conditionalPanel(
          condition = "input.prep_spectral == 'sg'",
          h5("Savitzky\u2013Golay parameters"),
          sliderInput(
            "sg_width", "Filter width (odd):",
            min = 3, max = 21, value = 7, step = 2
          ),
          numericInput(
            "sg_order", "Polynomial order:",
            value = 2, min = 2, max = 5, step = 1
          ),
          numericInput(
            "sg_deriv", "Derivative order:",
            value = 0, min = 0, max = 2, step = 1
          )
        )
      ),
      tags$hr(style = "margin-top:8px; margin-bottom:8px;"),
      
      # --- Cross\u2011validation (PLSR) \u2013 only if PLSR is enabled ---
      conditionalPanel(
        condition = "input.show_plsr_y_section || input.show_da_section", # PLSR or PLS-DA
        checkboxInput(
          "show_plsr_cv_section",
          label = strong("Cross\u2011validation"),
          value = TRUE
        ),
        conditionalPanel(
          condition = "input.show_plsr_cv_section",
          radioButtons(
            "plsr_cv_type", "CV type:",
            choices = c(
              "None"            = "none",
              "Leave\u2011one\u2011out"   = "loo",
              "Venetian blinds" = "vb",
              "Random subsets"  = "rand",
              "Contiguous blocks" = "cont"
            ),
            selected = "none"
          ),
          conditionalPanel(
            condition = "input.plsr_cv_type == 'vb' || input.plsr_cv_type == 'rand' || input.plsr_cv_type == 'cont'",
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "plsr_cv_splits",
                  "# of Data Splits:",
                  value = 5, min = 2, max = 25, step = 1
                )
              ),
              column(
                width = 6,
                numericInput(
                  "plsr_cv_thickness",
                  "Samples per Blind:",
                  value = 3, min = 1, max = 20, step = 1
                )
              )
            )
          )
        )
      )
    )
  })
  
  
  output$plsr_group_col_ui <- renderUI({
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    selectInput(
      "plsr_group_col",
      "Group column (Cal / Pred):",
      choices  = names(df),
      selected = names(df)[1]
    )
  })
  
  
  
  
  ######################## DENDROGRAM HELPERS (NEW) ########################
  # cluster-aware dendro_data (from your example, slightly generalized)
