# ESCAP 1.0.0 (26 November 2026)

First public release.

## Analyses
* Univariate statistics in one list: one-way ANOVA with post-hoc letters (CRD or RCBD; Tukey, LSD, Duncan, SNK,
  Scheffe, Bonferroni) with an optional condition column that gives one table per condition, two-way ANOVA with
  interaction and simple effects, ANCOVA, t-tests, Kruskal-Wallis + Dunn, descriptive statistics, correlation tests,
  linear and polynomial regression, reaction kinetics with Arrhenius fit.
* Epidemiology: prevalence (Wilson CI), chi-square or Fisher exact tests, crude and adjusted odds ratios, Firth
  correction under separation, model fit statistics.
* Chemometrics: SNV, MSC, detrend, Savitzky-Golay, six column scalings; PCA; PLSR and PLS-DA with preprocessing
  re-fitted inside each cross-validation fold; VIP, selectivity ratio, Q residuals and Hotelling T2 with limits;
  permutation test and ROC curves for PLS-DA.
* Design of experiments: factorial, fractional factorial, Plackett-Burman, central composite, Box-Behnken,
  Taguchi L9, mixture and D-optimal designs; response-surface analysis with lack of fit, canonical analysis and
  desirability optimization.

## Results, graphs and reports
* Publication-ready tables first on every results tab; results appear only after an analysis is run, with a hint
  saying what to do before that.
* Enhance Plots panel above each plot (Graphs and Maps): text, color, bars and symbols, axes (titles, limits,
  angle, break, second Y axis) and facets.
* Loading profiles (line or bar) for PCA, PLSR and PLS-DA, preprocessing variable profiles, PCA-style score plots
  for PLSR and PLS-DA.
* Reports as PDF, Word (.docx), Markdown or CSV for every results tab.

## Help and data
* Example data tab with 14 ready-to-upload data sets and the settings to use for each.
* Rewritten About (instructions for users) and Legal (terms of use) tabs.
* User guide (docs/USER_GUIDE.md).

## Validation
* dev/ holds scripts that compare the chemometric engine with the `pls` and `prospectr` packages and run every
  module through `shiny::testServer()`.
