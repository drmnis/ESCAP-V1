# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Example data (files in example_data/) shown on the "Example data" tab and served by server/s24.

ESC_EXAMPLES <- data.frame(
  section = c(rep("Univariate statistics", 8), "Epidemiology", "Design of experiments",
              "Chemometrics", rep("Graphs", 3)),
  file = c("one_way_anova_RCBD.csv", "two_way_anova_heating_x_treatment.csv", "ancova_coating_weight_loss.csv",
           "t_test_independent_drying.csv", "t_test_paired_panel.csv", "kruskal_hedonic_scores.csv",
           "correlation_regression_mango_quality.csv", "kinetics_ascorbic_acid.csv",
           "chi_square_prevalence.csv", "doe_CCD_ultrasound_extraction.csv",
           "NIR_spectra_octane_PCA_PLSR_PLSDA.csv",
           "storage_time_series.csv", "circular_bar_plot.csv", "sensory_profile_spider.csv"),
  upload = c(rep("Univariate", 6), "Univariate or Multivariate", rep("Univariate", 2),
             "Univariate (DoE)", "Multivariate", rep("Univariate", 3)),
  analysis = c("One-way ANOVA + post-hoc (CRD or RCBD)",
               "Two-way ANOVA; one-way ANOVA by condition",
               "ANCOVA",
               "t-test (independent)",
               "t-test (paired)",
               "Kruskal-Wallis + Dunn's test; descriptive statistics",
               "Correlation tests; regression; correlation heatmap",
               "Reaction kinetics with Arrhenius fit",
               "Prevalence, chi-square and logistic regression",
               "Analyze and optimize a completed central composite design",
               "Preprocessing, PCA, PLSR and PLS-DA",
               "Scatter, time series, dual Y-axis and contour plots; two-way ANOVA",
               "Circular stacked bar plot",
               "Spider (radar) plot"),
  settings = c("Treatment factor = Treatment; Block = Block (RCBD) or None (CRD); responses = any quality trait",
               "Factor A = Treatment, Factor B = Heating condition. One-way: Condition = Heating condition gives one table per condition",
               "Group factor = Coating; Covariate = Initial weight (g); response = Weight loss (%)",
               "Group column = Drying method",
               "Group column = Session; Pair / subject ID = Panelist",
               "Group factor = Formulation; responses = hedonic attributes (9-point scale)",
               "Correlation: choose the quality traits. Regression: y = Firmness (N), x = TSS (°Brix)",
               "Time = Time (day); Concentration = Ascorbic acid; Group = Temperature (°C)",
               "Species / stratum column = Species",
               "Factors = Temperature (°C), Time (min); responses = Yield (%), TPC",
               "X range = the NIR columns; PLSR: Y = octane; PLS-DA: class = Class",
               "x = Storage (day); group = Treatment; dual axis: L* and Firmness (N)",
               "Treatment factor = group; Condition / ID = individual",
               "Group = Product; attributes = the eight sensory columns"),
  stringsAsFactors = FALSE)

# size of each file (rows x columns), read once at startup
ESC_EXAMPLES$size <- vapply(ESC_EXAMPLES$file, function(f) {
  p <- file.path("example_data", f)
  if (!file.exists(p)) return("")
  hdr <- readLines(p, n = 1L, encoding = "UTF-8", warn = FALSE)
  n <- length(readLines(p, encoding = "UTF-8", warn = FALSE)) - 1L
  sprintf("%d × %d", n, length(strsplit(hdr, ",", fixed = TRUE)[[1]]))
}, character(1), USE.NAMES = FALSE)
