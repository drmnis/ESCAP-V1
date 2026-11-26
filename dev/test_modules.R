# ESCAP dev: integration test of the new modules (statistics, PLS-DA, plot studio, DoE, reports)
out <- "D:/API_R/dev/test_modules_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/"
step <- function(name, expr) {
  t0 <- Sys.time()
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-50s %-6s %5.2fs\n", name, substr(r, 1, 6), as.numeric(Sys.time() - t0, units = "secs")))
  if (r != "OK") cat("    ", r, "\n")
}
save_out <- function(f, name) file.copy(f, paste0(dev, name), overwrite = TRUE)
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
www <- "D:/API_R/ESCAP/www/"
up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
# a CCD data set with a known quadratic response for the DoE test
set.seed(7)
ccd <- data.frame(Temp = c(40, 80, 40, 80, 40, 80, 40, 80, 26.4, 93.6, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60),
                  Time = c(10, 10, 30, 30, 10, 10, 30, 30, 20, 20, 3.2, 36.8, 20, 20, 20, 20, 20, 20, 20, 20),
                  Conc = c(1, 1, 1, 1, 5, 5, 5, 5, 3, 3, 3, 3, -0.36, 6.36, 3, 3, 3, 3, 3, 3))
cx <- with(ccd, cbind((Temp - 60) / 20, (Time - 20) / 10, (Conc - 3) / 2))
ccd$Yield <- round(80 + 4 * cx[, 1] + 2.5 * cx[, 2] - 1 * cx[, 3] - 5 * cx[, 1]^2 - 3 * cx[, 2]^2 + 2 * cx[, 1] * cx[, 2] + rnorm(20, 0, 0.8), 2)
ccd$Colour <- round(30 - 3 * cx[, 1] + 1.5 * cx[, 3] + rnorm(20, 0, 0.5), 2)
write.csv(ccd, paste0(dev, "ccd_test.csv"), row.names = FALSE)
ex <- read.csv("D:/API_R/ESCAP/www/example_data.csv"); write.csv(ex[ex$treatment %in% c("Control", "P82"), ], paste0(dev, "ttest_test.csv"), row.names = FALSE)

for (f in list.files("R", full.names = TRUE)) source(f)   # helpers used directly in the montage step
res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(enable_uni = TRUE, file_uni = up(paste0(www, "example_data.csv")))
    step("Two-way ANOVA (condition x treatment, Duncan)", {
    session$setInputs(uni_analysis = "anova2", st_y = c("Flavonoid", "TSS"), st_A = "condition", st_B = "treatment",
                      st_inter = TRUE, st_block = "", st_posthoc = "Duncan", st_sstype = "2", st_alpha = 0.05, uni_run = 1)
    r <- st_result(); it <- r$items$Flavonoid; if (!is.null(it$error)) stop(it$error)
    print(it$anova); cat("CV:", round(it$cv, 2), "\n"); print(head(it$letters_AB, 6))
    x <- output$st_results
    session$setInputs(st_plot_resp = "Flavonoid", st_plot_type = "inter")
    f <- output$dl_stats_eps; save_out(f, "test_interaction.eps")
    session$setInputs(st_plot_type = "bars2"); f <- output$dl_stats_png; save_out(f, "test_twoway_bars.png")
  })
  step("Stats report PDF", { f <- output$rep_stats_pdf; save_out(f, "test_report_stats.pdf"); cat("    size", file.size(f), "\n") })
  step("Stats report Markdown", { f <- output$rep_stats_md; save_out(f, "test_report_stats.md") })
  step("One-way ANOVA RCBD (block = min), Tukey", {
    session$setInputs(uni_analysis = "anova1", st_y = "TSS", st_A = "treatment", st_block = "min", st_posthoc = "HSD", uni_run = 2)
    it <- st_result()$items$TSS; if (!is.null(it$error)) stop(it$error); print(it$anova); print(it$assumptions)
  })
  step("ANCOVA (TSS ~ treatment + TA)", {
    session$setInputs(uni_analysis = "ancova", st_y = "TSS", st_A = "treatment", st_cov = "TA", st_sstype = "2", uni_run = 3)
    it <- st_result()$items$TSS; if (!is.null(it$error)) stop(it$error); print(it$slope_check); print(head(it$adjusted$means))
    session$setInputs(st_plot_resp = "TSS", st_plot_type = "ancova"); f <- output$dl_stats_pdf; save_out(f, "test_ancova.pdf")
  })
  step("t-test (Welch, condition P vs UP1 via treatment Control vs P82)", {
    session$setInputs(file_uni = up(paste0(dev, "ttest_test.csv")))
    session$setInputs(uni_analysis = "ttest", st_y = "TSS", st_ttype = "independent", st_A = "treatment", st_pair = "",
                      st_mu = 0, st_vareq = FALSE, st_alt = "two.sided", uni_run = 9)
    it <- st_result()$items$TSS; if (!is.null(it$error)) stop(it$error); print(it$tests); print(it$effect)
    session$setInputs(file_uni = up(paste0(www, "example_data.csv")))
  })
  step("Kruskal-Wallis + Dunn", {
    session$setInputs(uni_analysis = "kruskal", st_y = "Flavonoid", st_A = "treatment", st_padj = "holm", uni_run = 4)
    it <- st_result()$items$Flavonoid; if (!is.null(it$error)) stop(it$error); print(it$test)
  })
  step("Regression (Flavonoid ~ TSS, quadratic)", {
    session$setInputs(uni_analysis = "reg", st_y = "Flavonoid", st_x = "TSS", st_degree = 2, uni_run = 5)
    it <- st_result()$items$Flavonoid; if (!is.null(it$error)) stop(it$error); cat("   ", it$equation, "\n")
  })
  step("Kinetics (Flavonoid vs min by condition)", {
    session$setInputs(uni_analysis = "kin", st_x = "min", st_y = "Flavonoid", st_A = "condition", uni_run = 6)
    it <- st_result()$items$Flavonoid; if (!is.null(it$error)) stop(it$error); print(head(it$table, 4))
  })
  step("Descriptive + correlation", {
    session$setInputs(uni_analysis = "desc", st_y = c("TSS", "TA"), st_A = "treatment", st_conf = 0.95, uni_run = 7); x <- output$st_results
    session$setInputs(uni_analysis = "cor", st_y = c("TSS", "TA", "Flavonoid"), st_cormethod = "pearson", st_padj = "holm", uni_run = 8); x <- output$st_results
  })
  # ---------- plot studio
  step("Studio: scatter with fit, EPS", {
    session$setInputs(ps_source = "uni", graphs_subtab = "General", graph_type_uni = "ps_scatter", ps_x = "TSS", ps_y = "Flavonoid", ps_group = "condition",
                      ps_shape_group = TRUE, ps_shape = "16", ps_size = 2.5, ps_line = "lm", ps_se = TRUE, ps_eq = TRUE, ps_labels = "", ps_facet = "")
    f <- output$dl_studio_eps; save_out(f, "test_scatter.eps")
  })
  step("Studio: time series", {
    session$setInputs(graph_type_uni = "ps_ts", ps_x = "min", ps_ys = c("TSS", "TA"), ps_group = "condition", ps_err = "se",
                      ps_points = TRUE, ps_facet_vars = TRUE, ps_smooth = FALSE, ps_datefmt = "%d %b")
    f <- output$dl_studio_png; save_out(f, "test_timeseries.png")
  })
  step("Studio: dual Y", {
    session$setInputs(graph_type_uni = "ps_dual", ps_x = "treatment", ps_y1 = "TSS", ps_type1 = "bar", ps_y2 = "Flavonoid", ps_col1 = "#4D4D4D", ps_col2 = "#D55E00")
    f <- output$dl_studio_png; save_out(f, "test_dual.png")
  })
  step("Studio: contour (L, a, b)", {
    session$setInputs(graph_type_uni = "ps_contour", ps_x = "L", ps_y = "a", ps_z = "TSS", ps_ctype = "filled", ps_interp = "linear",
                      ps_bins = 10, ps_pal = "viridis", ps_cpoints = TRUE)
    f <- output$dl_studio_png; save_out(f, "test_contour.png")
  })
  step("Studio: spider", {
    session$setInputs(graph_type_uni = "ps_spider", ps_vars = c("Flavonoid", "TSS", "TA", "L", "a", "b"), ps_group = "treatment",
                      ps_scale = "minmax", ps_fill = TRUE, ps_sfacet = FALSE)
    f <- output$dl_studio_png; save_out(f, "test_spider.png")
  })
  # ---------- PLS-DA on gasoline (Class column)
  step("PLS-DA (gasoline Class, SNV, VB CV, 30 perms)", {
    session$setInputs(file_multi = up(paste0(www, "gasoline.csv")))
    df <- dat_multi()
    session$setInputs(multi_var_range = c(3, ncol(df)), prep_spectral = "snv", prep_scaling = "none", prep_centering = "mc",
                      sg_width = 11, sg_order = 2, sg_deriv = 1, show_da_section = TRUE, da_class = "Class", plsr_split_type = "random", plsr_cal_prop = 0.7,
                      plsr_cv_type = "vb", plsr_cv_splits = 5, plsr_cv_thickness = 1, plsr_lv_user = 0, da_perm = 30, da_run = 1)
    r <- da_result(); cat("    classes:", r$classes, " LV:", r$lv, " CV acc:", round(r$metrics_cv$accuracy, 3),
                          " pred acc:", if (!is.null(r$metrics_pred)) round(r$metrics_pred$accuracy, 3) else NA, " perm p:", r$permutation$p_value, "\n")
    x <- output$da_results
    session$setInputs(da_plot = "scores", pca_pc_pair = "1_2", pca_scores_overlays = c("ellipse", "hull", "spokes"), pca_show_id = TRUE, pca_color_cos2 = FALSE)
    f <- output$dl_plsda_png; save_out(f, "test_plsda_scores.png")
    session$setInputs(da_plot = "roc"); f <- output$dl_plsda_png; save_out(f, "test_plsda_roc.png")
    f <- output$rep_plsda_pdf; save_out(f, "test_report_plsda.pdf")
  })
  # ---------- DoE
  step("DoE: generate CCD (rotatable, 3 factors)", {
    session$setInputs(dz_type = "ccd", dz_k = 3, dz_names = "Temp, Time, Conc", dz_low = "40, 10, 1", dz_high = "80, 30, 5",
                      dz_alpha = "rotatable", dz_center = 6, dz_reps = 1, dz_random = TRUE, dz_seed = 1, dz_resp = "Yield", dz_make = 1)
    d <- dz_result()$design; cat("    runs:", nrow(d), " alpha:", dz_result()$info$alpha, "\n")
    session$setInputs(dz_type = "frac", dz_k = 5, dz_p = 1, dz_names = "A,B,C,D,E", dz_low = "-1,-1,-1,-1,-1", dz_high = "1,1,1,1,1", dz_center = 0, dz_make = 2)
    cat("    2^(5-1):", nrow(dz_result()$design), "runs;", dz_result()$info$defining, "res", as.character(dz_result()$info$resolution), "\n")
    session$setInputs(dz_type = "bbd", dz_k = 3, dz_names = "A,B,C", dz_low = "0,0,0", dz_high = "1,1,1", dz_center = 3, dz_make = 3)
    cat("    BBD k=3:", nrow(dz_result()$design), "runs\n")
    session$setInputs(dz_type = "dopt", dz_k = 3, dz_druns = 14, dz_dmodel = "quadratic", dz_make = 4)
    cat("    D-optimal:", nrow(dz_result()$design), "runs\n")
  })
  step("DoE: fit quadratic + optimise (simulated CCD)", {
    session$setInputs(file_uni = up(paste0(dev, "ccd_test.csv")))
    session$setInputs(dx_factors = c("Temp", "Time", "Conc"), dx_resp = c("Yield", "Colour"), dx_model = "quadratic",
                      dx_reduce = FALSE, dx_run = 1)
    m <- dx_models()$Yield; if (!is.null(m$error)) stop(m$error)
    print(m$fit_stats); print(m$coef[, c("Term", "Coefficient", "P")]); print(m$canonical$table); cat("    nature:", m$canonical$nature, "\n")
    session$setInputs(dx_goal_Yield = "maximize", dx_lo_Yield = 70, dx_hi_Yield = 82, dx_tg_Yield = 80,
                      dx_goal_Colour = "minimize", dx_lo_Colour = 26, dx_hi_Colour = 34, dx_tg_Colour = 30, dx_opt = 1)
    o <- dx_opt(); print(o$settings); print(o$responses); cat("    D =", round(o$desirability, 3), "\n")
    session$setInputs(dx_plot_resp = "Yield", dx_plot = "contour", dx_f1 = "Temp", dx_f2 = "Time")
    f <- output$dl_doe_png; save_out(f, "test_doe_contour.png")
    session$setInputs(dx_plot = "surface", dx_theta = 35); f <- output$dl_doe_eps; save_out(f, "test_doe_surface.eps")
    f <- output$dl_doe_png; save_out(f, "test_doe_surface.png")
    session$setInputs(dx_plot = "pareto"); f <- output$dl_doe_png; save_out(f, "test_doe_pareto.png")
    f <- output$rep_doe_pdf; save_out(f, "test_report_doe.pdf")
  })
  step("New graphs: PCA, PLSR, PLS-DA, DoE (montage)", {
    plots <- list()
    session$setInputs(dx_plot = "halfnorm"); plots$doe_halfnorm <- dx_plot_obj()
    session$setInputs(dx_plot = "runorder"); plots$doe_runorder <- dx_plot_obj()
    for (ty in c("prob", "xvar", "weights")) { session$setInputs(da_plot = ty); plots[[paste0("da_", ty)]] <- da_plot_obj() }
    session$setInputs(file_multi = up(paste0(www, "gasoline.csv"))); df <- dat_multi()
    session$setInputs(multi_var_range = c(3, ncol(df)), multi_class_col = "Class", run_pca = 1, graphs_subtab = "PCA", pca_pc_pair = "1_2")
    for (ty in c("pca_cumvar", "pca_qt", "pca_contrib")) { session$setInputs(graph_type_pca = ty); plots[[ty]] <- extra_plot_obj(); x <- output$graph_plot }
    ycol <- names(df)[vapply(df, is.numeric, TRUE)][1]
    session$setInputs(show_plsr_y_section = TRUE, plsr_y_col = ycol, plsr_cv_type = "vb", run_plsr = 1, graphs_subtab = "PLSR")
    for (ty in c("plsr_resid", "plsr_var", "plsr_weights")) { session$setInputs(plsr_graph_type = ty); plots[[ty]] <- extra_plot_obj(); x <- output$plsr_graph }
    session$setInputs(sty_theme = "bw", sty_family = "serif", sty_base_size = 13, sty_palette = "default")
    st <- esc_style_now()
    png(paste0(dev, "test_new_graphs.png"), width = 2400, height = 2400, res = 110)
    print(patchwork::wrap_plots(lapply(plots, function(p) esc_apply_style(p + ggplot2::ggtitle(NULL), st)), ncol = 3))
    dev.off(); cat("    plots:", paste(names(plots), collapse = ", "), "\n")
  })
  "done"
}), error = function(e) paste("TESTSERVER ERROR:", conditionMessage(e)))
cat(res, "\n")
sink(type = "message"); sink(); close(zz)
cat("TEST MODULES FINISHED\n")
