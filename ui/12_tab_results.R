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
              esc_report_bar("rep_uni"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Publication Ready ANOVA Table"),
                div(
                  actionButton("summary_show_all", "Show all"),
                  tags$span(" "),
                  actionButton("summary_hide", "Hide")
                )
              ),
              textOutput("summary_view_info"),
              br(),
              tableOutput("summary_table"),
              br(),
              textOutput("footnote"),
              tags$hr(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("ANOVA Summary Table"),
                div(
                  actionButton("anova_show_all", "Show all"),
                  tags$span(" "),
                  actionButton("anova_hide", "Hide")
                )
              ),
              textOutput("anova_view_info"),
              tableOutput("anova_table"),
              tags$hr(),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("Publication Ready Chi Square Table"),
                div(
                  actionButton("chi_show_all", "Show all"),
                  tags$span(" "),
                  actionButton("chi_hide", "Hide")
                )
              ),
              HTML('<p style="color:red; margin-top:4px;">
                    If your dataset contains more than one species, please select that column in
                    <strong>"Select condition (ANOVA) optional /Species / *ID column (circular plot)"</strong>.
                    </p>'),
              textOutput("chi_view_info"),
              tableOutput("chi_table"),
              uiOutput("chi_footnote"),
              h4("Multivariable logistic model: fit statistics"),
              tableOutput("chi_fit_table")
            ),
            tabPanel(
              "Preprocessing",
              br(),
              esc_report_bar("rep_prep"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                div(
                  h4("Preprocessed X‑matrix"),
                  helpText("Preprocessed Data: Uploaded in the Multivariate field (with chosen variables)."),
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
            ),
            tabPanel(
              "PCA",
              br(),
              esc_report_bar("rep_pca"),
              div(
                style = "display:flex; justify-content:space-between; align-items:center;",
                h4("PCA – Eigenvalues and Explained Variance"),
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
                h4("PCA – Loadings (first few PCs)"),
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
                h4("PCA – Scores"),
                div(
                  actionButton("pca_scores_show_all_btn", "Show all"),
                  tags$span(" "),
                  actionButton("pca_scores_hide_btn", "Hide"),
                  tags$span(" "),
                  actionButton("pca_make_dendro", "Cluster Dendrogram")  # NEW button here
                )
              ),
              textOutput("pca_scores_view_info"),
              tableOutput("pca_scores"),
              
              
              
            ),
            tabPanel(
              "PLSR",
              br(),
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
              h4("PLSR – Model performance"),
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
                h4("Q residuals and Hotelling T²"),
                div(
                  actionButton("plsr_qt_show", "Show all"),
                  tags$span(" "),
                  actionButton("plsr_qt_hide", "Hide")
                )
              ),
              textOutput("plsr_qt_view_info"),
              tableOutput("plsr_qt_table")
            ),
            tabPanel(
              "Correlation",
              br(),
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
                h4("Correlation matrix: (Pearson product–moment correlation)"),
                div(
                  actionButton("corr_pearson_show", "Show all"),
                  tags$span(" "),
                  actionButton("corr_pearson_hide", "Hide"),
                  tags$span(" "),
                  actionButton("corr_pearson_make_heat", "Make Heatmaps")
                )
              ),
              textOutput("corr_pearson_view_info"),
              tableOutput("corr_table_pearson")
            )
          )
        )
