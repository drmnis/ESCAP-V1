# ESCAP 1.0.0 user guide

This guide walks through ESCAP from data preparation to the final report. The same instructions, in short form,
are on the **About** tab of the app, and ready-made files for every analysis are on the **Example data** tab.

## 1. Prepare the data

* File types: `.csv`, `.txt`, `.tsv`, `.dat`, `.xls`, `.xlsx`. The separator (comma, semicolon, tab, space) and
  the decimal mark are detected automatically.
* First row = column names. Units and symbols in names are fine, for example `TSS (°Brix)`, `L*`, `Weight loss (%)`.
* One row per observation (sample, replicate, animal or spectrum). Put treatment, block, group or class labels in
  their own columns; numbers are responses.
* Spectra: one row per sample, the first columns for references (for example `octane`, `Class`) and then one
  column per wavelength.
* Epidemiology: the outcome column is coded `positive` / `negative`.

## 2. The screen

* **Left panel.** *Univariate analysis* (upload, choose the analysis, set options, *Run analysis*, *DoE* button)
  and *Multivariate analysis* (upload, X-variable range, preprocessing, *PCA*, *PLSR*, *PLS-DA*, *Correlation*).
  *Clear all inputs* resets everything before you load a different data set.
* **Tabs.** About, View Data, Results, Graphs, Maps, Example data, Legal, R Info.
* **Results** and **Graphs** stay empty until an analysis has been run; until then each sub-tab tells you what to do.

## 3. Univariate statistics

Select the analysis in the *Analysis* list. Its options appear underneath; choose *Standard deviation (SD)* or
*Standard error (SE)* for the mean ± variation columns and click **Run analysis**.

| Analysis | Main settings | Output |
|---|---|---|
| One-way ANOVA + post-hoc (CRD / RCBD) | treatment factor; block (RCBD) or None (CRD); optional *Condition* column for one table per condition; responses; post-hoc test (Tukey, LSD, Duncan, SNK, Scheffe, Bonferroni); alpha | publication table (mean ± SD/SE with letters), ANOVA, assumption checks, comparisons |
| Two-way ANOVA with interaction | factor A, factor B, optional block, interaction, sum of squares type | ANOVA with partial eta-squared, interaction letters, simple effects |
| ANCOVA | group factor, covariate, response | slope-homogeneity test, adjusted means with letters |
| t-test | independent, paired (with pair ID) or one-sample; alternative; equal variances | t, df, P, mean difference with CI, Hedges' g |
| Kruskal-Wallis + Dunn | group factor; P adjustment | H test, Dunn comparisons, median letters |
| Descriptive statistics | optional group | n, mean, SD, SE, CV, CI, quartiles, range, skewness, kurtosis |
| Correlation tests | Pearson, Spearman or Kendall; P adjustment | r and P matrices |
| Regression | response y, predictor x, polynomial degree | coefficients, R2, adjusted R2, ANOVA, residual checks |
| Reaction kinetics | time, concentration, optional temperature group | zero, first, second order and Weibull fits, rate constants, half-lives, Arrhenius Ea |
| Chi-square & logistic regression | species/stratum column, outcome, risk factors, P threshold | prevalence (Wilson CI), chi-square/Fisher, crude and adjusted OR (Firth when needed) |

## 4. Design of experiments

Click **DoE (design of experiments)**.
1. *Create a design*: choose the design type (full or fractional factorial, Plackett-Burman, central composite,
   Box-Behnken, Taguchi L9, mixture, D-optimal), factors and levels, then download the run sheet.
2. Carry out the runs, add the response columns and upload the completed sheet.
3. *Analyze and optimize*: choose the factors and responses and the model (linear, interaction, quadratic or
   Scheffe for mixtures). Results give the ANOVA with lack of fit, R2, adjusted and predicted R2, adequate
   precision and coefficients; Graphs give contour, 3-D surface, Pareto, perturbation, interaction and desirability
   plots. Set goals for each response to find the optimum.

## 5. Multivariate analysis and chemometrics

1. Upload the file under *Multivariate analysis* and choose the X-variable range (for example the wavelength
   columns).
2. Choose preprocessing: SNV, MSC, detrend or Savitzky-Golay (window, polynomial order, derivative), then
   scaling and centering. Check the effect in *Graphs > Preprocessing* (preview, box plots, variable profile as
   line or bar).
3. Click **PCA** for exploration (scree, scores, loadings, spectral loading profile, biplot, T2 vs Q, contributions).
4. Click **PLSR** for calibration: choose Y, the cross-validation (venetian blinds, contiguous blocks, random subsets
   or leave-one-out) and optionally a test set. Results report RMSEC, RMSECV, RMSEP, R2, bias, SEC/SECV/SEP and RPD;
   graphs show measured vs predicted, RMSE vs latent variables, VIP, selectivity ratio, regression vector,
   loadings and score plots.
5. Click **PLS-DA** for classification: choose the class column. Results give confusion matrices, sensitivity,
   specificity, F1, balanced accuracy, kappa, ROC/AUC and a permutation test.
6. Click **Correlation** for correlation matrices and heatmaps of all selected variables.

Preprocessing is fitted on calibration samples only and re-fitted in every cross-validation fold, so the
cross-validated errors are not optimistic.

## 6. Graphs and Enhance Plots

In **Graphs**, pick the plot; the note above it says which columns are used. **Enhance Plots**, just above the
plot, has five tabs:
* *This plot*: options specific to the selected graph (for example line or bar profiles, ellipses, labels);
* *Text*: font family, sizes and title;
* *Color*: palettes (including color-blind-safe ones) and individual colors;
* *Bars & symbols*: bar width and spacing, error bars, symbol shape and size, line width;
* *Axes*: axis titles, limits, label angle, broken axis, second Y axis, facets (panels).

Download the figure as PNG, TIFF, PDF, SVG or EPS with your own width, height and resolution.

## 7. Reports

Every results sub-tab has a report bar: choose the paper size (A4, Letter, Legal, A3, A5) and orientation (automatic,
portrait or landscape; automatic follows the table widths) and download **PDF** or **Word**, **Markdown** or the tables as **CSV**. Reports list the
settings used, so an analysis can be repeated.

## 8. Good practice

* Read the assumption checks and notes under each table before reporting letters or P values.
* With a significant interaction in two-way ANOVA, compare treatments within each level of the other factor
  (simple effects) rather than pooled means.
* Report the validation scheme, number of latent variables and RMSECV/RMSEP for PLS models, and the permutation
  P value for PLS-DA.
* Keep the downloaded report with your raw data.

## 9. Help

Email <nahidul.islam@gau.edu.bd> with a short description of the problem and, if possible, the data file.
