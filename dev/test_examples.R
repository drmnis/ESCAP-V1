# ESCAP dev: every example data file with its intended analysis; plus profile plots (line / bar, colors)
out <- "D:/API_R/dev/test_examples_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/ex_"
step <- function(name, expr) {
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-62s %s\n", name, r))
}
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
ed <- "D:/API_R/ESCAP/example_data/"
up <- function(f) data.frame(name = f, size = file.size(paste0(ed, f)), type = "text/csv", datapath = paste0(ed, f), stringsAsFactors = FALSE)
save_dl <- function(f, name) file.copy(f, paste0(dev, name), overwrite = TRUE)
nm <- function(df, pat) grep(pat, names(df), value = TRUE)[1]

res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(main_tabs = "Graphs", graphs_subtab = "General", download_graph_w = 7, download_graph_h = 5, download_graph_dpi = 120)
  common <- list(st_block = "", st_posthoc = "HSD", st_sstype = "2", st_alpha = 0.05, spread_type = "sd", st_cond = "")
  run <- function(file, an, f) {
    session$setInputs(enable_uni = TRUE, file_uni = up(file)); df <- dat_raw()
    do.call(session$setInputs, utils::modifyList(c(common, list(uni_analysis = an)), f(df)))
    session$setInputs(uni_run = sample(1e6, 1)); x <- output$st_results
    pt <- st_pub(); if (!is.null(pt$error)) stop(pt$error); tb <- pt$table
    for (j in seq_along(tb)) tb[[j]] <- gsub("<[^>]+>", "", tb[[j]]); print(utils::head(tb, 8), row.names = FALSE); df
  }
  step("01 one-way ANOVA RCBD", run("01_one_way_anova_RCBD.csv", "anova1", function(d)
    list(st_A = "Treatment", st_block = "Block", st_y = names(d)[3:5])))
  step("02 two-way ANOVA", run("02_two_way_anova_packaging_x_treatment.csv", "anova2", function(d)
    list(st_A = "Treatment", st_B = "Packaging", st_inter = TRUE, st_y = names(d)[3:4])))
  step("02 one-way ANOVA with Condition = Packaging", run("02_two_way_anova_packaging_x_treatment.csv", "anova1", function(d)
    list(st_A = "Treatment", st_cond = "Packaging", st_y = names(d)[3])))
  step("03 ANCOVA", run("03_ancova_coating_weight_loss.csv", "ancova", function(d)
    list(st_A = "Coating", st_cov = nm(d, "Initial"), st_y = nm(d, "Weight_loss"))))
  step("04 independent t-test", run("04_t_test_independent_drying.csv", "ttest", function(d)
    list(st_ttype = "independent", st_A = nm(d, "Drying"), st_pair = "", st_mu = 0, st_vareq = FALSE, st_alt = "two.sided", st_y = names(d)[3:5])))
  step("05 paired t-test", run("05_t_test_paired_panel.csv", "ttest", function(d)
    list(st_ttype = "paired", st_A = "Session", st_pair = "Panelist", st_mu = 0, st_vareq = FALSE, st_alt = "two.sided", st_y = names(d)[3])))
  step("06 Kruskal-Wallis", run("06_kruskal_hedonic_scores.csv", "kruskal", function(d)
    list(st_A = "Formulation", st_padj = "holm", st_y = names(d)[3:6])))
  step("06 descriptive", run("06_kruskal_hedonic_scores.csv", "desc", function(d) list(st_A = "Formulation", st_conf = 0.95, st_y = names(d)[3:6])))
  step("07 correlation", run("07_correlation_regression_mango_quality.csv", "cor", function(d)
    list(st_cormethod = "spearman", st_padj = "holm", st_y = names(d)[3:8])))
  step("07 regression", run("07_correlation_regression_mango_quality.csv", "reg", function(d)
    list(st_y = nm(d, "Firmness"), st_x = nm(d, "TSS"), st_degree = 1)))
  step("08 kinetics + Arrhenius", run("08_kinetics_ascorbic_acid.csv", "kin", function(d)
    list(st_x = nm(d, "Time"), st_y = nm(d, "Ascorbic"), st_A = nm(d, "Temperature"))))
  step("09 chi-square", { session$setInputs(file_uni = up("09_chi_square_prevalence.csv"), uni_analysis = "chi", st_cond = "Species", uni_run = 99)
    x <- output$chi_table })
  step("11 circular bar plot", { session$setInputs(file_uni = up("11_circular_bar_plot.csv"), uni_analysis = "anova1"); d <- dat_raw()
    session$setInputs(st_A = "group", st_cond = "individual", st_y = names(d)[3:6], graphs_subtab = "General", graph_type_uni = "radial")
    x <- output$radial_plot })
  step("12 DoE analyse completed CCD", { session$setInputs(file_uni = up("12_doe_CCD_ultrasound_extraction.csv")); d <- dat_raw()
    session$setInputs(doe_step = "analyse", dx_factors = names(d)[2:3], dx_resp = names(d)[4:5], dx_model = "quadratic", dx_reduce = FALSE, dx_alpha = 0.1, dx_hier = TRUE, dx_run = 1)
    m <- dx_models(); if (!is.null(m[[1]]$error)) stop(m[[1]]$error); cat("   DoE R2 (Yield):", round(summary(m[[1]]$fit)$r.squared, 3), "\n") })
  step("13 time series + dual axis", { session$setInputs(file_uni = up("13_storage_time_series.csv")); d <- dat_raw()
    session$setInputs(graph_type_uni = "ps_ts", ps_source = "uni", ps_x = names(d)[2], ps_ys = names(d)[4:5], ps_group = "Treatment",
                      ps_points = TRUE, ps_facet_vars = TRUE, ps_smooth = FALSE, ps_datefmt = "")
    p <- ps_plot_obj(); save_dl(output$download_graph_png, "timeseries.png")
    session$setInputs(graph_type_uni = "ps_dual", ps_y1 = names(d)[4], ps_y2 = names(d)[8], ps_col1 = "#0072B2", ps_col2 = "#D55E00")
    p <- ps_plot_obj(); save_dl(output$download_graph_png, "dual.png") })
  step("14 spider plot", { session$setInputs(file_uni = up("14_sensory_profile_spider.csv")); d <- dat_raw()
    session$setInputs(graph_type_uni = "ps_spider", ps_group = "Product", ps_vars = names(d)[3:10], ps_fill = TRUE)
    p <- ps_plot_obj(); save_dl(output$download_graph_png, "spider.png") })
  # -------- multivariate: 10 NIR
  session$setInputs(file_multi = up("10_NIR_spectra_octane_PCA_PLSR_PLSDA.csv")); df <- dat_multi()
  session$setInputs(multi_var_range = c(3, ncol(df)), multi_class_col = "Class", prep_spectral = "snv", prep_scaling = "none",
                    prep_centering = "mc", run_pca = 1, show_prep_section = TRUE, main_tabs = "Graphs",
                    download_graph_w = 7, download_graph_h = 5, download_graph_dpi = 120)
  step("PCA loading profile, line", { session$setInputs(graphs_subtab = "PCA", graph_type_pca = "pca_loadings_profile", pca_load_profile_type = "line")
    x <- output$graph_plot; save_dl(output$download_graph_png, "pca_profile_line.png") })
  step("PCA loading profile, bar + palette", { session$setInputs(pca_load_profile_type = "bar", sty_custom_colours = "darkgreen, orange")
    x <- output$graph_plot; save_dl(output$download_graph_png, "pca_profile_bar_colors.png") })
  step("PLSR VIP line with own color", { session$setInputs(show_plsr_y_section = TRUE, plsr_y_col = "octane", plsr_cv_type = "vb",
      plsr_cv_splits = 5, plsr_cv_thickness = 1, plsr_lv_user = 0, show_plsr_sets_section = FALSE, run_plsr = 1)
    session$setInputs(graphs_subtab = "PLSR", plsr_graph_type = "vip", pca_load_profile_type = "line")
    x <- output$plsr_graph; save_dl(output$download_graph_png, "plsr_vip_color.png") })
  step("Preprocessing preview as bars (all samples / one sample)", {
    session$setInputs(sty_custom_colours = "", graphs_subtab = "Preprocessing", graph_type_prep = "prep_preview", prep_preview_var = "__ALL__",
                      pca_load_profile_type = "bar")
    x <- output$graph_plot_prep; save_dl(output$download_graph_png, "prep_bar_all.png")
    session$setInputs(prep_preview_var = "1"); x <- output$graph_plot_prep; save_dl(output$download_graph_png, "prep_bar_one.png") })
  step("Y axis angle", { session$setInputs(sty_y_angle = 45, pca_load_profile_type = "line"); save_dl(output$download_graph_png, "prep_yangle.png") })
}), error = function(e) cat("testServer ERROR:", conditionMessage(e), "\n"))
sink(type = "message"); sink(); close(zz)
