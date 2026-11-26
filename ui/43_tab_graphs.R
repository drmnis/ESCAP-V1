# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_graphs

ui_tab_graphs <- tabPanel(
          "Graphs",
          ui_style_panel,          # moved by JavaScript to sit just above the visible plot
          tabsetPanel(
            id = "graphs_subtab",
            
            # 1) General
            tabPanel(
              "Univariate", value = "General",
              br(),
              uiOutput("graph_roles_hint"),
              div(
                style = "display:flex; align-items:center; gap:12px;",
                div(
                  selectInput(
                    "graph_type_uni", "Choose graph type",
                    choices = ESC_UNI_GRAPHS
                  )
                ),
                div(
                  style = "margin-top:25px;",
                  
                  # Grouped bar plots: select which response
                  uiOutput("linear_group_resp_ui"),
                  
                  # Violin plot: select which response
                  uiOutput("ggbetween_resp_ui")
                  
                )
                
              ),
              br(),
              uiOutput("general_selector_ui"),
              
              br(),
              
              conditionalPanel(
                condition = "input.graph_type_uni == 'ggpairs'",
                plotOutput("pairs_plot", height = "400px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'radial'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  div(),
                  downloadButton("download_radial_example", "Download Sample Data")
                ),
                plotOutput("radial_plot", height = "500px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'ggbetween'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  h5("Violin/box plot with statistical comparisons"),
                  downloadButton("download_ggbetween_example", "Download Sample Data")
                ),
                plotOutput("ggbetween_plot", height = "500px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'simple_bar'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  div(),
                  downloadButton("download_simple_bar_example", "Download Sample Data")
                ),
                plotOutput("simple_bar_all", height = "600px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'linear_group'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  div(),
                  downloadButton("download_linear_group_example", "Download Sample Data")
                ),
                plotOutput("linear_group_all", height = "600px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'heatmaps'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  h5(
                    style = "color:#C14A09;",
                    "Use the selector above to switch between heatmaps"
                  ),
                  HTML('<p style="color:#C14A09; margin-top:4px;">
                      Row and column clustering use Euclidean distance with hierarchical (complete-linkage) clustering.
                      </p>'),
                  
                  downloadButton("download_heatmap_example", "Download Sample Data")
                ),
                plotlyOutput("heatmaps_all", height = "800px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'pca_dendrogram'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  h5(
                    style = "color:#C14A09;",
                    "Use the selector above to switch between dendrograms"
                  ),
                  HTML('<p style="color:#C14A09; margin-top:4px;">
                      Dendrograms are computed from Euclidean distances on PCA scores using hierarchical clustering (Ward.D2 linkage).
                      </p>')
                  
                ),
                plotOutput("dendro_plot", height = "800px")
              )
              ,
              conditionalPanel(condition = "input.graph_type_uni == 'st_plot'", ui_stats_graph),
              conditionalPanel(condition = "input.graph_type_uni.indexOf('ps_') == 0", ui_studio_graph)
              
              
              
            ),
            
            # 2) Preprocessing
            tabPanel("DoE", br(), ui_doe_graphs),

            tabPanel(
              "Preprocessing",
              br(),
              selectInput(
                "graph_type_prep", "Choose preprocessing graph",
                choices = c(
                  "Preprocessing preview" = "prep_preview",
                  "Show box plots"        = "prep_box",
                  "Scatter plot"          = "prep_scatter"
                )
              ),
              br(),
              plotlyOutput("graph_plot_prep", height = "400px")
            ),
            
            # 3) PCA
            tabPanel(
              "PCA",
              br(),
              selectInput(
                "graph_type_pca", "Choose PCA graph type",
                choices = c(
                  "Scree Plot"                       = "pca_scree",
                  "PCA score plot"                   = "pca_scores",
                  "PCA loading plot"                 = "pca_loadings",
                  "PCA loading plot (Spectral view)" = "pca_loadings_profile",
                  "PCA biplot"                       = "pca_biplot",
                  "Cumulative explained variance"    = "pca_cumvar",
                  "Hotelling T\u00b2 vs Q residuals (outliers)" = "pca_qt",
                  "Variable contributions"           = "pca_contrib"
                )
              ),
              
              
              
              plotlyOutput("graph_plot", height = "400px"),
              br(),
              conditionalPanel(
                condition = "input.graph_type_pca == 'pca_scores'",
                verbatimTextOutput("pca_click_debug"),
                actionButton("pca_delete_selected", "Delete selected sample(s)"),
                actionButton("pca_clear_selection", "Clear PCA selection")
              )
            ),
            
            
            
            # 4) PLSR
            tabPanel(
              "PLSR",
              br(),
              selectInput(
                "plsr_graph_type", "Choose PLSR graph:",
                choices = c(
                  "LV vs RMSEC / RMSECV" = "lv_rmse",
                  "Score plot" = "lv_scores",
                  "Measured vs CV\u2011predicted Y" = "y_meas_vs_cv",
                  "Measured vs Predicted Y (external)" = "y_meas_vs_pred",
                  "Q residuals vs Hotelling T\u00b2" = "q_vs_t2",
                  "Regression vector" = "reg_vector",
                  "VIP scores" = "vip",
                  "Selectivity Ratio" = "sr",
                  "Residuals vs predicted" = "plsr_resid",
                  "Explained variance (X and Y) vs LV" = "plsr_var",
                  "Loading weights (LV1, LV2)" = "plsr_weights"
                )
              ),
              br(),
              plotlyOutput("plsr_graph", height = "400px"),
              br(),
              conditionalPanel(
                condition = "input.plsr_graph_type == 'q_vs_t2'",
                verbatimTextOutput("plsr_qt_click_debug"),
                actionButton("plsr_qt_delete_selected", "Delete selected sample(s)"),
                actionButton("plsr_qt_clear_selection", "Clear Q vs T\u00b2 selection")
              )
            ),
            
            
            
            
            tabPanel("PLS-DA", br(), ui_da_graphs),
            # 5) Correlation
            tabPanel(
              "Correlation",
              br(),
              selectInput(
                "graph_type_corr", "Choose correlation graph type",
                choices = c(
                  "Spearman's rank-order correlation"  = "corr_spearman",
                  "Pearson product\u2013moment correlation" = "corr_pearson"
                )
              ),
              br(),
              uiOutput("corr_selector_ui"),         # <- new
              br(),
              
              tags$small(style = "color:#666;",
                "Red = negative, blue = positive. Style 1 prints r with stars; style 2 marks significance with stars; style 3 leaves non-significant cells (P \u2265 0.05) blank. * P < 0.05, ** P < 0.01, *** P < 0.001."),
              plotOutput("graph_plot_corr", height = "1000px")
            )
          ),
          br(),
          h5(strong("Download the graph shown")),
          esc_download_bar("download_graph")
        )
