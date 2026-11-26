# ESCAP dev: data with special characters in names and values (UTF-8 and Windows-1252 files)
out <- "D:/API_R/dev/test_special_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/"
step <- function(name, expr) {
  t0 <- Sys.time()
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-52s %-6s %5.2fs\n", name, substr(r, 1, 6), as.numeric(Sys.time() - t0, units = "secs")))
  if (r != "OK") cat("    ", r, "\n")
}
set.seed(3)
n <- 24
d <- data.frame(
  `Sample no.` = 1:n,
  `Treatment & Co. <A>` = rep(c("Control \"raw\"", "Heat 80°C", "O'Brien & Sons", "pH<5>"), each = 6),
  `Storage (days)` = rep(c(0, 7), 12),
  `Color (L*)??? $@#@#` = round(rnorm(n, 60, 3), 2),
  `a* (redness)` = round(rnorm(n, 5, 1), 2),
  `Moisture (%)` = round(rnorm(n, 70, 2), 2),
  `Temp °C / µg·g⁻¹` = round(rnorm(n, 25, 1), 2),
  `রং মান` = round(rnorm(n, 10, 1), 2),
  `Result` = sample(c("Positive", "negative "), n, TRUE),
  check.names = FALSE, stringsAsFactors = FALSE)
d$`Color (L*)??? $@#@#` <- d$`Color (L*)??? $@#@#` + rep(c(0, 3, 6, 9), each = 6)
utils::write.csv(d, paste0(dev, "special_utf8.csv"), row.names = FALSE, fileEncoding = "UTF-8")
d2 <- d[, -8]; names(d2)[7] <- "Temp \u00b0C / \u00b5g per g"; utils::write.csv(d2, paste0(dev, "special_latin1.csv"), row.names = FALSE, fileEncoding = "latin1")
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
source("R/io.R")
for (f in c("special_utf8.csv", "special_latin1.csv")) {
  r <- esc_read_data(paste0(dev, f)); cat(f, ":", paste(names(r), collapse = " | "), "\n")
  cat("   levels:", paste(unique(r[[2]]), collapse = " | "), " valid UTF-8:", all(validUTF8(unlist(r[vapply(r, is.character, TRUE)]))), "\n")
}
up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  for (f in c("special_utf8.csv", "special_latin1.csv")) {
    cat("----", f, "\n")
    step("upload univariate", session$setInputs(file_uni = up(paste0(dev, f))))
    nm <- names(dat_raw()); resp <- nm[4:6]; trt <- nm[2]
    step("view data table", { x <- output$data_preview_dt })
    step("classic one-way ANOVA", {
      session$setInputs(uni_analysis = "anova1", st_cond = "", st_A = trt, st_y = resp,
                        spread_type = "sd", st_block = "", st_posthoc = "HSD", st_sstype = "2", st_alpha = 0.05, uni_run = sample(1e6, 1)); x <- output$st_pub; x <- output$st_results })
    step("chi-square & logistic", {
      session$setInputs(uni_analysis = "chi", st_cond = "", chi_p_enter = 0.2, uni_run = sample(1e6, 1)); x <- output$chi_table })
    step("two-way ANOVA", {
      session$setInputs(uni_analysis = "anova2", st_y = resp, st_A = trt, st_B = nm[3], st_inter = TRUE, st_block = "",
                        st_posthoc = "HSD", st_sstype = "2", st_alpha = 0.05, uni_run = sample(1e6, 1)); x <- output$st_results })
    step("graph simple bar + style PNG", {
      session$setInputs(graphs_subtab = "General", graph_type_uni = "simple_bar", sty_theme = "classic", sty_family = "Arial",
                        download_graph_w = 6, download_graph_h = 4, download_graph_dpi = 100)
      f2 <- output$download_graph_png; file.copy(f2, paste0(dev, "test_special_bar.png"), overwrite = TRUE) })
    step("scatter plot", {
      session$setInputs(graph_type_uni = "ps_scatter", ps_source = "uni", ps_x = resp[1], ps_y = resp[2], ps_group = trt,
                        ps_shape_group = TRUE, ps_shape = "16", ps_size = 2, ps_line = "lm", ps_se = TRUE, ps_eq = TRUE, ps_labels = "", ps_facet = "")
      x <- output$ps_plot })
    step("upload multivariate", session$setInputs(file_multi = up(paste0(dev, f))))
    step("PCA", { session$setInputs(multi_var_range = c(4, 7), prep_spectral = "none", prep_scaling = "auto", prep_centering = "mc", run_pca = sample(1e6, 1))
      x <- output$pca_eigenvalues; x <- output$pca_loadings })
    step("PLSR", { session$setInputs(multi_var_range = c(5, 7), plsr_y_col = names(dat_multi())[4], show_plsr_sets_section = FALSE,
                                     plsr_cv_type = "loo", plsr_lv_user = 0, run_plsr = sample(1e6, 1)); x <- output$plsr_perf_table })
    step("PLS-DA", { session$setInputs(multi_var_range = c(4, 7), show_da_section = TRUE, da_class = names(dat_multi())[2], show_plsr_sets_section = FALSE, plsr_cv_type = "vb",
                                       plsr_cv_splits = 4, plsr_cv_thickness = 1, plsr_lv_user = 0, da_perm = 0, da_run = sample(1e6, 1)); x <- output$da_results })
  }
  "done"
}), error = function(e) paste("TESTSERVER ERROR:", conditionMessage(e)))
cat(res, "\n")
sink(type = "message"); sink(); close(zz)
cat("TEST SPECIAL FINISHED\n")
