# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI components for the additional plots listed in Graphs -> General
# (scatter, time series, dual Y axis, contour, spider)

ESC_STUDIO_PLOTS <- c("Scatter plot (symbols, lines, fits)" = "ps_scatter", "Time series / line plot" = "ps_ts",
                      "Dual Y-axis plot" = "ps_dual", "Contour plot" = "ps_contour", "Spider (radar) plot" = "ps_spider")

ui_studio_graph <- fluidRow(
  column(3, wellPanel(
    radioButtons("ps_source", "Data", c("Univariate upload" = "uni", "Multivariate upload" = "multi"), inline = TRUE),
    uiOutput("ps_controls"))),
  column(9, plotOutput("ps_plot", height = "600px"))
)

# Graphs > Univariate: plot list grouped by purpose
ESC_UNI_GRAPHS <- list(
  `Treatment comparison` = c("Simple bar plots" = "simple_bar", "Grouped bar plots" = "linear_group",
                             "Violin plot with statistics" = "ggbetween", "Statistics result plot" = "st_plot"),
  `Distribution` = c("Density & Histogram" = "ggpairs"),
  `Relationships and trends` = c("Scatter plot (symbols, lines, fits)" = "ps_scatter", "Time series / line plot" = "ps_ts",
                                 "Dual Y-axis plot" = "ps_dual", "Contour plot" = "ps_contour"),
  `Profiles` = c("Spider (radar) plot" = "ps_spider", "Circular stacked bar plot" = "radial"),
  `Patterns and clustering` = c("Heatmaps" = "heatmaps", "PCA based dendrogram" = "pca_dendrogram")
)
