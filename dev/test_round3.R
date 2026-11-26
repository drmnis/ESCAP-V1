# ESCAP dev: round-3 checks (score-plot options in PLSR / PLS-DA, axis titles and limits, facets,
# line / bar profiles, correlation styles with significance, map downloads)
out <- "D:/API_R/dev/test_round3_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/r3_"
step <- function(name, expr) {
  t0 <- Sys.time()
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-58s %s  %.2fs\n", name, r, as.numeric(Sys.time() - t0, units = "secs")))
}
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
www <- "D:/API_R/ESCAP/www/"
up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
save_dl <- function(f, name) file.copy(f, paste0(dev, name), overwrite = TRUE)

res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(main_tabs = "Graphs", enable_uni = TRUE, file_uni = up(paste0(www, "example_data.csv")),
                    uni_analysis = "anova2", st_A = "treatment", st_B = "condition", st_y = c("Flavonoid", "TSS"),
                    download_graph_w = 7, download_graph_h = 5, download_graph_dpi = 120)
  step("simple bar: axis titles + Y start/end", {
    session$setInputs(graphs_subtab = "General", graph_type_uni = "simple_bar", simple_bar_which = "5",
                      sty_xlab = "Treatment (custom)", sty_ylab = "Content (custom)", sty_ymin = 0, sty_ymax = 120)
    save_dl(output$download_graph_png, "bar_axes.png")
    session$setInputs(sty_xlab = "", sty_ylab = "", sty_ymin = NA, sty_ymax = NA)
  })
  step("facet choices for grouped bar + facetted download", {
    session$setInputs(graph_type_uni = "linear_group", simple_bar_which = "1", sty_facet_on = TRUE)
    ui <- output$sty_facet_ui; cat("   facet UI choices:", gsub(".*?<option value=\"([^\"]+)\".*?", "\\1 ", paste(ui$html, collapse = "")) |> substr(1, 200), "\n")
    p <- current_graph_object(); d <- p$data
    fv <- names(d)[vapply(d, function(x) is.factor(x) || is.character(x), TRUE)]
    cat("   discrete columns:", paste(fv, collapse = ", "), "\n")
    session$setInputs(sty_facet_by = fv[1], sty_facet_layout = "wrap", sty_facet_ncol = 2, sty_facet_scales = "free_y")
    save_dl(output$download_graph_png, "facet_grouped.png")
    session$setInputs(sty_facet_on = FALSE)
  })
  step("scatter plot facet by a grouping column", {
    session$setInputs(graph_type_uni = "ps_scatter", ps_source = "uni", ps_x = "TSS", ps_y = "Flavonoid", ps_group = "treatment")
    p0 <- current_graph_object(); gcol <- names(p0$data)[vapply(p0$data, function(x) is.factor(x) || is.character(x), TRUE)][1]
    session$setInputs(sty_facet_on = TRUE); x <- output$sty_facet_ui
    p <- current_graph_object(); cat("   scatter data columns:", paste(names(p$data), collapse = ", "), "\n")
    session$setInputs(sty_facet_by = gcol, sty_facet_layout = "wrap", sty_facet_ncol = 3)
    save_dl(output$download_graph_png, "facet_scatter.png"); session$setInputs(sty_facet_on = FALSE)
  })
  # ---------------- multivariate
  session$setInputs(file_multi = up(paste0(www, "gasoline.csv"))); df <- dat_multi()
  session$setInputs(multi_var_range = c(3, ncol(df)), multi_class_col = "Class", prep_spectral = "snv", prep_scaling = "none",
                    prep_centering = "mc", run_pca = 1)
  step("PLSR score plot: ellipse + hull + spokes + labels", {
    session$setInputs(show_plsr_y_section = TRUE, plsr_y_col = names(df)[1], plsr_cv_type = "vb", plsr_cv_splits = 5,
                      plsr_cv_thickness = 1, plsr_lv_user = 0, show_plsr_sets_section = FALSE, run_plsr = 1)
    session$setInputs(graphs_subtab = "PLSR", plsr_graph_type = "lv_scores", pca_pc_pair = "1_2",
                      pca_scores_overlays = c("ellipse", "hull", "spokes"), pca_show_id = TRUE, pca_color_cos2 = FALSE)
    x <- output$plsr_graph; save_dl(output$download_graph_png, "plsr_scores_overlays.png")
  })
  step("PLSR score plot: cos2 colouring, LV1 vs LV3", {
    session$setInputs(pca_pc_pair = "1_3", pca_color_cos2 = TRUE, pca_show_id = FALSE)
    x <- output$plsr_graph; save_dl(output$download_graph_png, "plsr_scores_cos2.png")
    session$setInputs(pca_color_cos2 = FALSE, pca_pc_pair = "1_2")
  })
  step("PLSR VIP as bars + styled (theme, axis title)", {
    session$setInputs(plsr_graph_type = "vip", pca_load_profile_type = "bar", sty_theme = "bw", sty_ylab = "VIP")
    x <- output$plsr_graph; save_dl(output$download_graph_png, "plsr_vip_bar.png")
    session$setInputs(pca_load_profile_type = "line", sty_theme = "keep", sty_ylab = "")
  })
  step("PLS-DA score plot with shared options (+ hull, labels)", {
    session$setInputs(show_da_section = TRUE, da_class = "Class", plsr_split_type = "random", plsr_cal_prop = 0.7,
                      show_plsr_sets_section = TRUE, da_perm = 0, da_run = 1)
    session$setInputs(graphs_subtab = "PLS-DA", da_plot = "scores", pca_pc_pair = "1_2",
                      pca_scores_overlays = c("ellipse", "hull"), pca_show_id = TRUE)
    save_dl(output$download_graph_png, "plsda_scores.png")
  })
  step("PLS-DA coefficients as bars / VIP as line", {
    session$setInputs(da_plot = "coef", pca_load_profile_type = "bar"); save_dl(output$download_graph_png, "plsda_coef_bar.png")
    session$setInputs(da_plot = "vip", pca_load_profile_type = "line"); save_dl(output$download_graph_png, "plsda_vip_line.png")
    session$setInputs(da_plot = "weights", pca_load_profile_type = "bar"); x <- da_plot_obj()
  })
  # ---------------- correlation (example data as multivariate)
  step("correlation results + styles 1-5", {
    session$setInputs(file_multi = up(paste0(www, "example_data.csv"))); d2 <- dat_multi()
    num <- which(vapply(d2, is.numeric, TRUE)); session$setInputs(multi_var_range = c(min(num), ncol(d2)), run_corr = 1)
    r <- corr_result(); x <- output$corr_table; cat("   Pearson r[1,2] =", round(r$r_pearson[1, 2], 4),
      " check cor():", round(cor(as.matrix(d2[, colnames(r$r_pearson)[1:2]]))[1, 2], 4),
      "| Spearman:", round(r$r_spearman[1, 2], 4), " check:", round(cor(d2[[colnames(r$r_pearson)[1]]], d2[[colnames(r$r_pearson)[2]]], method = "spearman"), 4),
      "| P:", signif(r$P_pearson[1, 2], 3), " check:", signif(cor.test(d2[[colnames(r$r_pearson)[1]]], d2[[colnames(r$r_pearson)[2]]])$p.value, 3), "\n")
    session$setInputs(graphs_subtab = "Correlation", graph_type_corr = "corr_pearson")
    for (k in 1:5) { session$setInputs(simple_bar_which = as.character(k)); save_dl(output$download_graph_png, paste0("corr_style", k, ".png")) }
    session$setInputs(graph_type_corr = "corr_spearman", simple_bar_which = "1"); save_dl(output$download_graph_png, "corr_spearman1.png")
  })
  step("map download (PNG + EPS, styled)", {
    b <- bgd_sf_data(); lv <- b[b$Level == "ADM1", ]; nm <- if (nrow(lv)) as.character(lv$Name)[1:2] else unique(as.character(b$Name))[1:2]
    cat("   map areas:", nm, "\n")
    session$setInputs(main_tabs = "Maps", map_level = "ADM1", map_names = nm, sty_family = "serif",
                      download_map_w = 8, download_map_h = 5, download_map_dpi = 150)
    save_dl(output$download_map_png, "map.png"); save_dl(output$download_map_eps, "map.eps")
  })
}), error = function(e) cat("testServer ERROR:", conditionMessage(e), "\n"))
sink(type = "message"); sink(); close(zz)
