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
        
        #file input and Epidemiological data
        fluidRow(
          column(
            width = 8,
            fileInput("file_uni", "Upload data",
                      accept = ESC_ACCEPT
            )
          ),
          column(
            width = 4,
            div(style = "margin-top:25px;",  # adjust vertical alignment
                checkboxInput(
                  "do_chi",
                  "Epidemiological data",
                  value = FALSE
                )
            )
          )
        ),
        
        
        #helpText("Each row is one observation."),
        uiOutput("condition_ui"),
        uiOutput("factor_ui"),
        uiOutput("response_ui"),
        
        conditionalPanel(
          condition = "input.do_chi",
          uiOutput("chi_ref_ui"),
          fluidRow(
            column(6, numericInput("chi_p_enter", "Entry P for multivariable model", value = 0.20, min = 0.01, max = 1, step = 0.05)),
            column(6, div(style = "margin-top:25px;", checkboxInput("chi_pooled", "Also analyse all species pooled", FALSE)))
          )
        ),
        
        radioButtons(
          "spread_type", "Choose:",
          choices = c(
            "Standard deviation (SD)" = "sd",
            "Standard error (SE)"     = "se"
          ),
          selected = "sd",
          inline   = TRUE
        )
        
        ,
        
        
        
        fluidRow(
          column(
            width = 6,
            actionButton("run", "One-way ANOVA and Tukey", width = "100%")
          ),
          column(
            width = 6,
            actionButton(
              "run_chi",
              HTML("&chi;<sup>2</sup> &amp; Logistic Reg."),
              width = "100%"
            )
          )
        ),
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
        column(
          width = 6,
          conditionalPanel(
            condition = "input.run_pca > 0 || input.run_plsr > 0",
            
            # --- Score Plot Options header + show/hide ---
            checkboxInput(
              "show_score_options",
              label = strong("Score Plot Options"),
              value = TRUE
            ),
            conditionalPanel(
              condition = "
        input.show_score_options &&
        (
          input.graphs_subtab == 'PCA' ||
          (
            input.graphs_subtab == 'PLSR' &&
            (
              input.plsr_graph_type == 'lv_scores'      ||
              input.plsr_graph_type == 'y_meas_vs_cv'   ||
              input.plsr_graph_type == 'y_meas_vs_pred' ||
              input.plsr_graph_type == 'q_vs_t2'
            )
          )
        )
      ",
              selectInput(
                "pca_pc_pair",
                "Choose: For plottings",
                choices = c(
                  "PC1 vs PC2" = "1_2",
                  "PC1 vs PC3" = "1_3",
                  "PC2 vs PC3" = "2_3"
                ),
                selected = "1_2"
              ),
              checkboxGroupInput(
                "pca_scores_overlays",
                label = NULL,
                choices = c(
                  "Confidence ellipse" = "ellipse",
                  "Convex hull"        = "hull",
                  "Spokes"             = "spokes"
                ),
                selected = NULL
              ),
              checkboxInput(
                "pca_show_id",
                "Show ID labels",
                value = FALSE
              ),
              checkboxInput(
                "pca_color_cos2",
                "Use cos²",
                value = FALSE
              ),
              tags$hr(style = "margin-top:8px; margin-bottom:8px;"),
              
            ),
            
            
            # --- Loading profile: Spectral Data ---
            checkboxInput(
              "show_loading_profile",
              label = strong("Loading profile: Spectral Data"),
              value = TRUE
            ),
            conditionalPanel(
              condition = "input.show_loading_profile",
              radioButtons(
                "pca_load_profile_type", NULL,
                choices = c("Line" = "line", "Bar" = "bar"),
                selected = "line"
              )
            ),
            tags$hr(style = "margin-top:8px; margin-bottom:8px;")
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          actionButton("run_pca", "PCA", width = "100%")
        ),
        column(
          width = 4,
          actionButton("run_plsr", "PLSR", width = "100%")
        ),
        column(
          width = 4,
          actionButton("run_corr", "Correlation", width = "100%")
        )
      ),
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
