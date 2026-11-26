# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI components for the univariate analysis drop-down (sidebar) and its results / graphs.
# ESC_STAT_TESTS is defined in R/stats.R.

ESC_UNI_ANALYSES <- list(
  `Compare treatments / groups` = c(
    "One-way ANOVA + post-hoc (CRD / RCBD)" = "anova1",
    "Two-way ANOVA with interaction (CRD / RCBD)" = "anova2",
    "ANCOVA (covariate-adjusted means)" = "ancova",
    "t-test (one-sample, independent, paired)" = "ttest",
    "Kruskal-Wallis + Dunn (non-parametric)" = "kruskal"),
  `Describe and relate variables` = c(
    "Descriptive statistics" = "desc",
    "Correlation tests (Pearson / Spearman / Kendall)" = "cor",
    "Linear / polynomial regression" = "reg"),
  `Food science` = c(
    "Reaction kinetics (zero, first, second order, Weibull)" = "kin"),
  `Epidemiology` = c(
    "Chi-square & logistic regression (prevalence data)" = "chi")
)

ui_stats_graph <- tagList(
  fluidRow(column(4, uiOutput("st_plot_resp_ui")), column(4, uiOutput("st_plot_type_ui"))),
  plotOutput("st_plot", height = "560px")
)
