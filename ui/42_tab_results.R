# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_results

ui_tab_results <- tabPanel(
          "Results",
          tabsetPanel(
            id = "results_subtab",
            tabPanel(
              "Univariate",
              br(),
              # every analysis from the "Analysis" drop-down: publication table first, then the statistical tables
              tags$style(HTML("
                #st_pub table, #st_results table{border-collapse:collapse;margin:4px 0 6px 0;font-size:13px}
                #st_pub th, #st_pub td, #st_results th, #st_results td{padding:3px 12px;text-align:left;white-space:nowrap}
                #st_pub table{border-top:2px solid #000;border-bottom:2px solid #000}
                #st_pub th{border-bottom:1px solid #000}
                #st_results th{border-bottom:1px solid #999;background:#f4f7fb}
                #st_results tr:nth-child(even) td{background:#fafafa}
                #da_results table, #dx_results table{border-collapse:collapse;margin:4px 0 6px 0;font-size:13px}
                #da_results th, #da_results td, #dx_results th, #dx_results td{padding:3px 12px;text-align:left;white-space:nowrap}
                #da_results th, #dx_results th{border-bottom:1px solid #999;background:#f4f7fb}
              ")),
              conditionalPanel(
                condition = "input.uni_analysis != 'chi'",
                esc_gate("ok_uni", "Upload univariate data, choose an analysis and its settings in the sidebar, then click <b>Run analysis</b>.",
                esc_report_bar("rep_stats"),
                uiOutput("st_pub"),
                uiOutput("st_results"))
              ),
              conditionalPanel(
              condition = "input.uni_analysis == 'chi'",
              esc_gate("ok_chi", "Upload prevalence data (positive / negative outcome columns), choose the species / stratum column, then click <b>Run analysis</b>.",
              esc_report_bar("rep_uni"),
              conditionalPanel(
              condition = "input.uni_analysis == 'chi'",
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Publication Ready Chi Square Table"),
                div(
                  actionButton("chi_show_all", "Show all"),
                  tags$span(" "),
                  actionButton("chi_hide", "Hide")
                )
              ),
              textOutput("chi_view_info"),
              tableOutput("chi_table"),
              uiOutput("chi_footnote"),
              h4("Multivariable logistic model: fit statistics"),
              tableOutput("chi_fit_table")
              )))
            ),
            tabPanel("DoE", br(), ui_doe_results),
            tabPanel(
              "Preprocessing",
              br(),
              esc_gate("ok_prep", "Upload multivariate data and choose the X-variable range and preprocessing in the sidebar; the preprocessed X-matrix appears here.",
              esc_report_bar("rep_prep"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                div(
                  h4("Preprocessed X\u2011matrix"),
                  textOutput("prep_steps_text"),
                  textOutput("prep_view_info")
                ),
                div(
                  actionButton("prep_show_all", "Show all"),
                  tags$span(" "),
                  actionButton("prep_hide", "Hide")
                )
              ),
              br(),
              tableOutput("prep_data_table")
              )
            ),
            tabPanel(
              "PCA",
              br(),
              esc_gate("ok_pca", "Upload multivariate data, set the X-variable range and preprocessing, then click <b>PCA</b> in the sidebar.",
              esc_report_bar("rep_pca"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("PCA \u2013 Eigenvalues and Explained Variance"),
                div(
                  actionButton("pca_eig_show", "Show all"),
                  tags$span(" "),
                  actionButton("pca_eig_hide", "Hide")
                )
              ),
              textOutput("pca_eig_view_info"),
              tableOutput("pca_eigenvalues"),
              br(),
              sliderInput(
                "pca_n_keep",
                "Number of PCs to keep for plotting:",
                min   = 2,
                max   = 10,   # will be updated after PCA runs
                value = 3,
                step  = 1
              ),
              br(),
              
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("PCA \u2013 Loadings (first few PCs)"),
                div(
                  actionButton("pca_load_show", "Show all"),
                  tags$span(" "),
                  actionButton("pca_load_hide", "Hide")
                )
              ),
              textOutput("pca_load_view_info"),
              tableOutput("pca_loadings"),
              br(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("PCA \u2013 Scores"),
                div(
                  actionButton("pca_scores_show_all_btn", "Show all"),
                  tags$span(" "),
                  actionButton("pca_scores_hide_btn", "Hide"),
                  tags$span(" "),
                  actionButton("pca_make_dendro", "Cluster Dendrogram")  # NEW button here
                )
              ),
              textOutput("pca_scores_view_info"),
              tableOutput("pca_scores")
              )
            ),
            tabPanel(
              "PLSR",
              br(),
              esc_gate("ok_plsr", "Tick <b>PLSR \u2013 Y</b> in the multivariate sidebar, choose the Y column, sets and cross-validation, then click <b>PLSR</b>.",
              esc_report_bar("rep_plsr"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("X/Y block variance"),
                div(
                  actionButton("plsr_var_show", "Show all"),
                  tags$span(" "),
                  actionButton("plsr_var_hide", "Hide")
                )
              ),
              textOutput("plsr_var_view_info"),
              tableOutput("plsr_var_table"),
              tags$hr(),
              h4("PLSR \u2013 Model performance"),
              textOutput("plsr_perf_view_info"),
              tableOutput("plsr_perf_table"),
              tags$hr(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Regression coefficients, VIP, Selectivity Ratio"),
                div(
                  actionButton("plsr_par_show", "Show all"),
                  tags$span(" "),
                  actionButton("plsr_par_hide", "Hide")
                )
              ),
              textOutput("plsr_par_view_info"),
              tableOutput("plsr_par_table"),
              tags$hr(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Q residuals and Hotelling T\u00b2"),
                div(
                  actionButton("plsr_qt_show", "Show all"),
                  tags$span(" "),
                  actionButton("plsr_qt_hide", "Hide")
                )
              ),
              textOutput("plsr_qt_view_info"),
              tableOutput("plsr_qt_table")
              )
            ),
            tabPanel("PLS-DA", br(), ui_da_results),
            tabPanel(
              "Correlation",
              br(),
              esc_gate("ok_corr", "Upload multivariate data, set the X-variable range, then click <b>Correlation</b> in the sidebar.",
              esc_report_bar("rep_corr"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Correlation matrix: (Spearman's rank-order correlation)"),
                div(
                  actionButton("corr_spear_show", "Show all"),
                  tags$span(" "),
                  actionButton("corr_spear_hide", "Hide"),
                  tags$span(" "),
                  actionButton("corr_spear_make_heat", "Make Heatmaps")
                )
              ),
              textOutput("corr_spear_view_info"),
              tableOutput("corr_table"),
              br(), br(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Correlation matrix: (Pearson product\u2013moment correlation)"),
                div(
                  actionButton("corr_pearson_show", "Show all"),
                  tags$span(" "),
                  actionButton("corr_pearson_hide", "Hide"),
                  tags$span(" "),
                  actionButton("corr_pearson_make_heat", "Make Heatmaps")
                )
              ),
              textOutput("corr_pearson_view_info"),
              tableOutput("corr_table_pearson"),
              tags$small("Coefficients are r (Pearson) or \u03c1 (Spearman). * P < 0.05, ** P < 0.01, *** P < 0.001 (two-sided test of zero correlation, n \u2212 2 df; not adjusted for multiple comparisons).")
              )
            )
          )
        )
