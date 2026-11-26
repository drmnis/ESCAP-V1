# ESCAP dev: integration test of the whole server with shiny::testServer()
out <- "D:/API_R/dev/test_app_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
step <- function(name, expr) {
  t0 <- Sys.time()
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-45s %-8s %5.2fs\n", name, substr(r, 1, 8), as.numeric(Sys.time() - t0, units = "secs")))
  if (r != "OK") cat("    ", r, "\n")
}
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
www <- "D:/API_R/ESCAP/www/"
up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  # ---------- epidemiology
  step("upload chi data", session$setInputs(enable_uni = TRUE, file_uni = up(paste0(www, "chi_square_data.csv"))))
  step("chi-square + logistic", {
    session$setInputs(uni_analysis = "chi", chi_p_enter = 0.2, st_cond = "", uni_run = 1)
    x <- output$chi_table; cat(substr(gsub("<[^>]+>", " ", x), 1, 600), "\n")
  })
  step("chi fit table", { x <- output$chi_fit_table })
  # ---------- one-way ANOVA
  step("upload example_data", session$setInputs(file_uni = up(paste0(www, "example_data.csv"))))
  step("one-way ANOVA", {
    session$setInputs(uni_analysis = "anova1", st_cond = "condition", st_A = "treatment", st_block = "",
                      st_posthoc = "HSD", st_sstype = "2", st_alpha = 0.05,
                      st_y = c("Flavonoid", "TSS"), spread_type = "sd", uni_run = 2)
    pt <- st_pub(); if (!is.null(pt$error)) stop(pt$error)
    tb <- pt$table; for (j in seq_along(tb)) tb[[j]] <- gsub("<[^>]+>", "", tb[[j]]); print(tb, row.names = FALSE); cat(pt$note, "\n")
    x <- output$st_pub; x <- output$st_results
    session$setInputs(spread_type = "se"); pt <- st_pub(); cat("SE first cell:", gsub("<[^>]+>", "", pt$table[1, 3]), "\n")
    session$setInputs(st_cond = "", uni_run = 3); pt <- st_pub(); cat("without condition, rows:", nrow(pt$table), "\n")
    session$setInputs(st_cond = "condition", spread_type = "sd")
  })
  step("View Data table", { x <- output$data_preview_dt })
  for (g in list(c("ggpairs", "pairs_plot"), c("simple_bar", "simple_bar_all"), c("radial", "radial_plot"),
                 c("ggbetween", "ggbetween_plot"), c("heatmaps", "heatmaps_all")))
    step(paste("General graph:", g[1]), { session$setInputs(graphs_subtab = "General", graph_type_uni = g[1], simple_bar_which = "1")
                                          x <- output[[g[2]]] })
  step("General graph: linear_group (two-way roles)", {
    session$setInputs(uni_analysis = "anova2", st_A = "treatment", st_B = "condition", st_y = c("Flavonoid", "TSS"))
    session$setInputs(graph_type_uni = "linear_group"); x <- output$linear_group_all; x <- output$graph_roles_hint
    session$setInputs(uni_analysis = "anova1", st_cond = "condition", st_A = "treatment")
  })
  step("Enhance Plots on classic simple bar (before/after)", {
    session$setInputs(graphs_subtab = "General", graph_type_uni = "simple_bar", simple_bar_which = "1",
                      download_graph_w = 6, download_graph_h = 4, download_graph_dpi = 150)
    f <- output$download_graph_png; file.copy(f, "D:/API_R/dev/test_style_before.png", overwrite = TRUE)
    session$setInputs(sty_theme = "classic", sty_family = "Times New Roman", sty_base_size = 14, sty_palette = "Greys (journal print)",
                      sty_bar_on = TRUE, sty_bar_width = 0.4, sty_bar_gap = 0.1, sty_x_angle = 45)
    f <- output$download_graph_png; file.copy(f, "D:/API_R/dev/test_style_after.png", overwrite = TRUE)
    session$setInputs(sty_theme = "keep", sty_family = "keep", sty_base_size = NA, sty_palette = "default", sty_bar_on = FALSE, sty_x_angle = 0)
  })
  # ---------- multivariate: PCA and PLSR on gasoline
  step("upload gasoline", session$setInputs(file_multi = up(paste0(www, "gasoline.csv"))))
  df <- read.csv(paste0(www, "gasoline.csv"))
  cat("gasoline.csv:", dim(df), "first cols:", head(names(df), 3), "\n")
  step("PCA (SNV + mean centring)", {
    session$setInputs(multi_var_range = c(2, ncol(df)), prep_spectral = "snv", prep_scaling = "none",
                      prep_centering = "mc", sg_width = 11, sg_order = 2, sg_deriv = 1, run_pca = 1)
    x <- output$pca_eigenvalues
  })
  step("PCA with Savitzky-Golay", {
    session$setInputs(prep_spectral = "sg", run_pca = 2); x <- output$pca_loadings
  })
  step("PLSR (SG, VB CV, 70/30 split)", {
    session$setInputs(plsr_y_col = names(df)[1], show_plsr_sets_section = TRUE, plsr_split_type = "random",
                      plsr_cal_prop = 0.7, plsr_cv_type = "vb", plsr_cv_splits = 5, plsr_cv_thickness = 1,
                      plsr_lv_user = 0, run_plsr = 1)
    r <- plsr_result(); if (!is.null(r$error)) stop(r$error)
    print(head(r$perf[, c("LV", "RMSEC", "RMSECV", "RMSEP", "R² CV", "R² Pred", "RPD (Pred)")], 8))
    cat("chosen LV:", r$chosen_lv, " top VIP:", paste(head(names(sort(r$vip, decreasing = TRUE)), 3), collapse = ", "), "\n")
    x <- output$plsr_par_table
  })
  # ---------- graphs + style + EPS
  step("graph: PLSR VIP, styled, EPS", {
    session$setInputs(graphs_subtab = "PLSR", plsr_graph_type = "vip", sty_enable = TRUE, sty_theme = "classic",
                      sty_family = "Times New Roman", sty_base_size = 12, sty_palette = "Okabe-Ito (colour-blind safe)",
                      download_graph_w = 7, download_graph_h = 5, download_graph_dpi = 300)
    f <- output$download_graph_eps; cat("    EPS:", f, file.size(f), "bytes\n")
    file.copy(f, "D:/API_R/dev/test_vip.eps", overwrite = TRUE)
  })
  step("graph: PLSR VIP, PNG", { f <- output$download_graph_png; file.copy(f, "D:/API_R/dev/test_vip.png", overwrite = TRUE) })
  step("graph: PCA scores, PDF", {
    session$setInputs(graphs_subtab = "PCA", graph_type_pca = "pca_scores", pca_pc_pair = "1_2")
    f <- output$download_graph_pdf; file.copy(f, "D:/API_R/dev/test_pca.pdf", overwrite = TRUE)
  })
  "done"
}), error = function(e) paste("TESTSERVER ERROR:", conditionMessage(e)))
cat(res, "\n")
sink(type = "message"); sink(); close(zz)
cat("TEST APP FINISHED\n")
