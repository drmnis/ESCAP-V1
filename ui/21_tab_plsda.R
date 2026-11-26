# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI components for PLS-DA (settings in the sidebar, results and graphs in their tabs)

ui_da_controls <- NULL   # PLS-DA options now share the PLSR panel (server/s12_multivariate_inputs.R)

ui_da_results <- tagList(
  esc_gate("ok_da", "Tick <b>PLS-DA \u2013 class</b> in the multivariate sidebar and choose the class column; the X-variable range, preprocessing, sets and cross-validation are shared with PLSR. Then click <b>PLS-DA</b>.",
    esc_report_bar("rep_plsda"),
    uiOutput("da_results"))
)

ui_da_graphs <- tagList(
  fluidRow(
    column(4, selectInput("da_plot", "Choose PLS-DA graph", c(
      "Classification error vs LV" = "err", "Score plot" = "scores", "Predicted class value (CV)" = "yhat",
      "Confusion matrix (CV)" = "cm", "ROC curves (CV, one-vs-rest)" = "roc", "VIP scores" = "vip",
      "Regression coefficients" = "coef", "Selectivity ratio" = "sr", "Q residuals vs Hotelling T2" = "qt",
      "Permutation test" = "perm", "Predicted class values by class (box plot)" = "prob",
      "Explained X variance vs LV" = "xvar", "Loading weights (LV1, LV2)" = "weights")))
  ),
  plotOutput("da_plot_out", height = "520px")
)
