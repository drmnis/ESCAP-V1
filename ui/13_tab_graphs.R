# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_graphs

ui_tab_graphs <- tabPanel(
          "Graphs",
          tabsetPanel(
            id = "graphs_subtab",
            
            # 1) General
            tabPanel(
              "General",
              br(),
              div(
                style = "display:flex; align-items:center; gap:12px;",
                div(
                  selectInput(
                    "graph_type_uni", "Choose graph type",
                    choices = c(
                      "Density & Histogram"             = "ggpairs",
                      "Simple bar plots"                = "simple_bar",
                      "Grouped bar plots"               = "linear_group",
                      "Circular stacked bar plot"       = "radial",
                      "Violin plot with statistics"     = "ggbetween",
                      "Heatmaps"                        = "heatmaps",
                      "PCA based dendrogram"            = "pca_dendrogram"
                    )
                  )
                ),
                div(
                  style = "margin-top:25px;",
                  
                  # Controls for Simple bar plots
                  conditionalPanel(
                    condition = "input.graph_type_uni == 'simple_bar'",
                    tagList(
                      checkboxInput("use_facet",      "Use Facet",    value = FALSE),
                      checkboxInput("use_unicolor",   "Unicolor",     value = FALSE),
                      checkboxInput("use_multicolor", "Multicolor",   value = FALSE)
                    )
                  ),
                  
                  # Controls for Density & Histogram (you already have this)
                  conditionalPanel(
                    condition = "input.graph_type_uni == 'ggpairs'",
                    tagList(
                      checkboxInput(
                        "dens_hist_use_facet",
                        "Use Facet",
                        value = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.dens_hist_use_facet",
                        radioButtons(
                          "dens_hist_facet_type",
                          "Facet type",
                          choices  = c("Density plot" = "density",
                                       "Histogram"    = "hist"),
                          selected = "density"
                        )
                      ),
                      checkboxInput(
                        "dens_hist_use_bins",
                        "Modify bins",
                        value = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.dens_hist_use_bins",
                        numericInput(
                          "dens_hist_bins",
                          "Histogram bins",
                          value = 30, min = 5, max = 200, step = 1
                        )
                      )
                    )
                  ),
                  
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
                  div(
                    h5(
                      style = "color:red; font-weight:bold;",
                      "Please select ID, Treatment, and Responses from the left Panel"
                    )
                  ),
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
                  div(
                    h5(
                      style = "color:red; font-weight:bold;",
                      "Select Treatment and Response Variables from the left Panel"
                    ),
                    h5(
                      style = "color:#C14A09; margin-top:4px;",
                      "and choose the plot number to download"
                    )
                  ),
                  downloadButton("download_simple_bar_example", "Download Sample Data")
                ),
                plotOutput("simple_bar_all", height = "600px")
              ),
              conditionalPanel(
                condition = "input.graph_type_uni == 'linear_group'",
                div(
                  style = "display:flex; justify-content:space-between; align-items:center;",
                  div(
                    h5(
                      style = "color:red; font-weight:bold;",
                      "Select Two Factors and Response Variables from the left Panel (Factor which have lower leves, should select first)"
                    ),
                    h5(
                      style = "color:#C14A09; margin-top:4px;",
                      "and choose the plot number to download"
                    )
                  ),
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
                    "Use the selector from the left panel to toggle between HEATMAPS"
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
                    "Use the selector from the left panel to toggle between Dendograms"
                  ),
                  HTML('<p style="color:#C14A09; margin-top:4px;">
                      Dendrograms are computed from Euclidean distances on PCA scores using hierarchical clustering (Ward.D2 linkage).
                      </p>')
                  
                ),
                plotOutput("dendro_plot", height = "800px")
              )
              
              
              
            ),
            
            # 2) Preprocessing
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
                  "PCA biplot"                       = "pca_biplot"
                )
              ),
              
              
              
              # --- PCA score plot: Modify view (secondary IDs) ---
              conditionalPanel(
                condition = "input.graph_type_pca == 'pca_scores'",
                fluidRow(
                  column(
                    width = 3,
                    checkboxInput(
                      "pca_modify_view",
                      "Modify view",
                      value = FALSE
                    )
                  ),
                  column(
                    width = 9,
                    conditionalPanel(
                      condition = "input.pca_modify_view",
                      # three Secondary ID selectors, side by side
                      fluidRow(
                        column(
                          width = 4,
                          uiOutput("pca_secid_1_ui")
                        ),
                        column(
                          width = 4,
                          uiOutput("pca_secid_2_ui")
                        ),
                        column(
                          width = 4,
                          uiOutput("pca_secid_3_ui")
                        )
                      )
                    )
                  )
                ),
                # plot controls, side by side
                conditionalPanel(
                  condition = "input.pca_modify_view",
                  fluidRow(
                    column(
                      width = 4,
                      checkboxInput(
                        "pca_shape_by_secid",
                        "Change shape by Secondary ID 1",
                        value = FALSE
                      )
                    ),
                    column(
                      width = 4,
                      checkboxInput(
                        "pca_size_by_secid",
                        "Change size by Secondary ID 2",
                        value = FALSE
                      )
                    ),
                    column(
                      width = 4,
                      checkboxInput(
                        "pca_color_by_secid",
                        "Change color by Secondary ID 3",
                        value = FALSE
                      )
                    )
                  )
                )
              ),
              
              
              
              
              
              
              br(),
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
                  "Measured vs CV‑predicted Y" = "y_meas_vs_cv",
                  "Measured vs Predicted Y (external)" = "y_meas_vs_pred",
                  "Q residuals vs Hotelling T²" = "q_vs_t2",
                  "Regression vector" = "reg_vector",
                  "VIP scores" = "vip",
                  "Selectivity Ratio" = "sr"
                )
              ),
              br(),
              plotlyOutput("plsr_graph", height = "400px"),
              br(),
              conditionalPanel(
                condition = "input.plsr_graph_type == 'q_vs_t2'",
                verbatimTextOutput("plsr_qt_click_debug"),
                actionButton("plsr_qt_delete_selected", "Delete selected sample(s)"),
                actionButton("plsr_qt_clear_selection", "Clear Q vs T² selection")
              )
            ),
            
            
            
            
            # 5) Correlation
            tabPanel(
              "Correlation",
              br(),
              selectInput(
                "graph_type_corr", "Choose correlation graph type",
                choices = c(
                  "Spearman's rank-order correlation"  = "corr_spearman",
                  "Pearson product–moment correlation" = "corr_pearson"
                )
              ),
              br(),
              uiOutput("corr_selector_ui"),         # <- new
              br(),
              
              plotOutput("graph_plot_corr", height = "1000px")
            )
          ),
          br(),
          h5(strong("Download the graph shown")),
          esc_download_bar("download_graph")
        )
