# ESCAP: Easy Statistics, Chemometrics and Plotting

**Version 1.0.0** (November 2026) | **Online:** <https://analysis.gau.edu.bd> | **License:** GNU GPL v3.0

ESCAP is a free R/Shiny web application for analyzing experimental data without writing code. It was built for
food science, agriculture and related laboratory research, where the same analyses recur in almost every study:
comparing treatments, relating quality attributes, modeling changes during processing or storage, optimizing a
process and calibrating spectra.

**Developer:** Dr. Md. Nahidul Islam, Department of Food Engineering, Faculty of Agricultural and Bioresources
Engineering, Gazipur Agricultural University (GAU), Gazipur-1706, Bangladesh. <nahidul.islam@gau.edu.bd>

## What ESCAP does

| Area | Methods |
|---|---|
| Data input | `.csv`, `.txt`, `.tsv`, `.dat`, `.xls`, `.xlsx`; separator and decimal mark detected automatically; special characters and units in names are kept |
| Univariate statistics | descriptive statistics; assumption checks (Shapiro-Wilk, Brown-Forsythe/Levene, Bartlett); one-sample, independent (Student/Welch) and paired t-tests with Hedges' g; one-way ANOVA (CRD or RCBD) with Tukey, LSD, Duncan, SNK, Scheffe or Bonferroni letters and an optional condition column; two-way ANOVA with interaction (Type I/II/III SS, partial eta-squared, simple effects); ANCOVA (slope homogeneity, adjusted means); Kruskal-Wallis + Dunn; Pearson/Spearman/Kendall correlation tests; linear and polynomial regression; reaction kinetics (zero, first, second order, Weibull) with Arrhenius fit |
| Epidemiology | prevalence with Wilson CI; chi-square or Fisher exact tests; crude and adjusted odds ratios; Firth correction under separation; Hosmer-Lemeshow, Nagelkerke R2, AUC |
| Chemometrics | SNV, MSC, detrend, Savitzky-Golay smoothing and derivatives; autoscaling, Pareto, range, IQR, max-min, log; PCA; PLSR and PLS-DA with preprocessing re-fitted inside each cross-validation fold; RMSEC/RMSECV/RMSEP, R2, SEC/SECV/SEP, RPD; VIP; selectivity ratio; Q residuals and Hotelling T2 with limits; confusion matrices, kappa, ROC/AUC and permutation test |
| Design of experiments | full and fractional factorial, Plackett-Burman, central composite, Box-Behnken, Taguchi L9, simplex-lattice and simplex-centroid mixtures, D-optimal; response-surface ANOVA with lack of fit, predicted R2, adequate precision, canonical analysis and desirability optimization |
| Graphs | bar, grouped bar, circular bar, violin with statistics, density, pairs, heatmaps, dendrograms, scatter with fits, time series, dual Y axis, contour, 3-D surface, spider/radar, PCA/PLS score, loading and diagnostic plots, maps of Bangladesh |
| Enhance Plots | fonts, colors (incl. color-blind-safe palettes), bar width and spacing, symbols, axis titles, limits and angles, broken axis, second Y axis, facets |
| Export | figures as PNG, TIFF, PDF, SVG or EPS; reports of every results tab as PDF, Word (.docx), Markdown or CSV |

## Quick start for users

1. Open <https://analysis.gau.edu.bd>.
2. Download a file from the **Example data** tab to see the expected layout: one row per sample or replicate,
   column names in the first row, group labels in their own columns.
3. Upload your file under **Univariate analysis** (statistics, epidemiology, DoE, univariate graphs) or
   **Multivariate analysis** (spectra and other variable sets).
4. Choose the analysis, set its columns and options and click **Run analysis** (or PCA, PLSR, PLS-DA, Correlation).
5. Read the **Results** tab, make figures in **Graphs** (restyle them with **Enhance Plots**) and download the
   report bar's PDF, Word, Markdown or CSV report.

The full guide with every analysis is in [`docs/USER_GUIDE.md`](docs/USER_GUIDE.md).

## Project structure

```
global.R            packages and global options
ui.R                assembles the interface from ui/
server.R            sources every file in server/ inside server()
R/                  calculation engines and helpers (loaded automatically by Shiny)
  io.R              data import
  stats.R           t-tests, ANOVA, ANCOVA, non-parametric tests, regression, kinetics
  stats_pub.R       publication-ready tables
  epi.R             prevalence, chi-square/Fisher, odds ratios
  chemometrics.R    preprocessing, PLSR, PLS-DA, VIP, selectivity ratio, Q/T2
  doe.R             design generation and response-surface analysis
  *_plots.R         plotting functions
  plot_style.R      Enhance Plots layer and export devices (incl. EPS)
  report.R          PDF, Word, Markdown and CSV reports
  example_data.R    list of example data sets
  version.R         version number
ui/                 one file per interface component or tab
server/             one file per server block (s00 ... s45)
example_data/       14 example data sets (Example data tab)
www/                images and map data
dev/                validation and integration tests (run in R)
docs/               user guide
```

## Validation

`dev/test_chemometrics.R` compares the engine with the `pls` package (fitted values, regression vector,
cross-validated RMSE), Mevik's VIP formula and `prospectr::msc`. The other `dev/test_*.R` scripts run every module
through `shiny::testServer()`; `dev/test_examples.R` runs every example data set.

## Citation

To be included.

## License and disclaimer

ESCAP is free software under the GNU General Public License v3.0 (see `LICENSE`). It is provided as is, without
warranty; users are responsible for choosing appropriate methods and checking results before publication.
