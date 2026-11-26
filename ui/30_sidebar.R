# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_sidebar

ui_sidebar <- sidebarPanel(
      
      # Part 1: Univariate analysis
      div(
        style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
        div(
          style = "margin-top:4px; margin-bottom:0;",   # <- vertical tweak
          checkboxInput(
            "enable_uni",
            label = NULL,
            value = TRUE,
            width = "20px"
          )
        ),
        h4("Univariate analysis")
      ),
      
      # Univariate controls (hide when enable_uni is unticked)
      conditionalPanel(
        condition = "input.enable_uni",
        
        fileInput("file_uni", "Upload data", accept = ESC_ACCEPT),
        #helpText("Each row is one observation."),
        selectInput("uni_analysis", "Analysis", ESC_UNI_ANALYSES, selected = "anova1"),
        div(class = "esc-compact", uiOutput("st_controls")),
        conditionalPanel(
          condition = "input.uni_analysis == 'chi'",
          uiOutput("chi_ref_ui"),
          fluidRow(
            column(6, numericInput("chi_p_enter", "Entry P for multivariable model", value = 0.20, min = 0.01, max = 1, step = 0.05)),
            column(6, div(style = "margin-top:25px;", checkboxInput("chi_pooled", "Also analyse all species pooled", FALSE)))
          )
        ),
        
        conditionalPanel(
          condition = "['cor', 'reg', 'kin', 'chi'].indexOf(input.uni_analysis) < 0",
          radioButtons(
            "spread_type", "Show variation as:",
            choices = c(
              "Standard deviation (SD)" = "sd",
              "Standard error (SE)"     = "se"
            ),
            selected = "sd",
            inline   = TRUE
          )
        )
        
        ,
        
        
        
        fluidRow(
          column(width = 6, actionButton("uni_run", "Run analysis", class = "btn-primary", width = "100%")),
          column(width = 6, actionButton("doe_open", "DoE (design of experiments)", width = "100%"))
        ),
        br(),
        ui_doe_controls,
        tags$hr()
      ),
      
      # Plot selector: always available when a relevant plot is chosen,
      # independent of enable_uni
      #fluidRow(
      #  column(
      #    width = 4,
      #    uiOutput("simple_bar_selector_ui")
      #  )
      #),
      
      #########################################################
      #################### MULTIVARIATE ANALYSIS ##############
      #########################################################
      # Part 2: Multivariate analysis (PCA first)
      h4("Multivariate analysis"),
      fileInput("file_multi", "Upload data",
                accept = ESC_ACCEPT
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("multi_id_ui")
        ),
        column(
          width = 6,
          uiOutput("multi_class_ui")
        )
      ),
      uiOutput("multi_vars_ui"),
      
      
      # Two columns under the slider:
      # left = preprocessing; right = PCA-score plot options
      fluidRow(
        column(
          width = 4,
          #####################################################
          #################### PREPROCESSING OPTIONS ##########
          #####################################################
          uiOutput("multi_prep_ui")
        ),
        column(
          width = 2,
          conditionalPanel(
            condition = "input.show_prep_section",   # <- tie to Preprocessing tick
            uiOutput("prep_preview_var_ui")          # "Show" dropdown
          )
        ),
        column(width = 6)
      ),
      fluidRow(
        column(width = 3, actionButton("run_pca", "PCA", width = "100%")),
        column(width = 3, actionButton("run_plsr", "PLSR", width = "100%")),
        column(width = 3, actionButton("da_run", "PLS-DA", width = "100%")),
        column(width = 3, actionButton("run_corr", "Correlation", width = "100%"))
      ),
      ui_da_controls,
      tags$hr(),
      
      
      #########################################################
      #################### APP CONTROLS #######################
      #########################################################
      actionButton("clear_all", "Clear all inputs"),
      # tags$hr(),
      
      
      tags$hr(),
      h5("Total visits to this app:"),
      textOutput("visit_count"),
      tags$br(),
      HTML("&copy; 2026 by Dr. Md. Nahidul Islam. All rights reserved.")
    )
