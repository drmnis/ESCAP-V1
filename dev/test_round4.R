# ESCAP dev: original-layout chi-square and one-way ANOVA tables, Word reports, every DoE graph
out <- "D:/API_R/dev/test_round4_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/r4_"
step <- function(name, expr) {
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-58s %s\n", name, r))
}
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
ed <- "D:/API_R/ESCAP/example_data/"
up <- function(f) data.frame(name = f, size = file.size(paste0(ed, f)), type = "text/csv", datapath = paste0(ed, f), stringsAsFactors = FALSE)
save_dl <- function(f, name) file.copy(f, paste0(dev, name), overwrite = TRUE)
plain <- function(tb) { for (j in seq_along(tb)) tb[[j]] <- gsub("<sup>(.*?)</sup>", "^\\1", gsub("<[^>]+>|&dagger;|&chi;", "", tb[[j]]))
  names(tb) <- gsub("<[^>]+>|&chi;", "", names(tb)); print(tb, row.names = FALSE) }

res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(main_tabs = "Results", enable_uni = TRUE, file_uni = up("09_chi_square_prevalence.csv"),
                    uni_analysis = "chi", st_cond = "Species", chi_p_enter = 0.2, rep_uni_paper = "A4", rep_uni_orient = "auto")
  step("chi-square original-layout table", { session$setInputs(uni_run = 1)
    x <- output$chi_table; plain(utils::head(chi_original_table(chi_result(), html = TRUE), 24)) })
  step("chi-square Word + PDF report", { save_dl(output$rep_uni_docx, "chi_report.docx"); save_dl(output$rep_uni_pdf, "chi_report.pdf") })
  step("one-way ANOVA original layout (condition)", {
    session$setInputs(file_uni = up("02_two_way_anova_packaging_x_treatment.csv")); d <- dat_raw()
    session$setInputs(uni_analysis = "anova1", st_A = "Treatment", st_cond = "Packaging", st_block = "", st_posthoc = "HSD",
                      st_sstype = "2", st_alpha = 0.05, spread_type = "sd", st_y = names(d)[3:4], uni_run = 2)
    pt <- st_pub(); if (!is.null(pt$error)) stop(pt$error)
    plain(utils::head(pt$table, 14)); plain(utils::head(pt$summary, 8)); cat(pt$note, "\n"); x <- output$st_pub })
  step("one-way ANOVA Word + PDF report", { session$setInputs(rep_stats_paper = "A4", rep_stats_orient = "auto")
    save_dl(output$rep_stats_docx, "anova_report.docx"); save_dl(output$rep_stats_pdf, "anova_report.pdf") })
  # ---------------- DoE
  session$setInputs(file_uni = up("12_doe_CCD_ultrasound_extraction.csv")); d <- dat_raw()
  step("DoE help text before fitting", { h <- output$dx_graph_help; cat("   ", substr(gsub("<[^>]+>", "", h$html), 1, 120), "\n") })
  session$setInputs(doe_step = "analyse", dx_factors = names(d)[2:3], dx_resp = names(d)[4:5], dx_model = "quadratic",
                    dx_reduce = FALSE, dx_alpha = 0.1, dx_hier = TRUE, dx_run = 1, main_tabs = "Graphs", graphs_subtab = "DoE",
                    dx_plot_resp = names(d)[4], dx_f1 = names(d)[2], dx_f2 = names(d)[3], dx_theta = 35,
                    download_graph_w = 6, download_graph_h = 4.5, download_graph_dpi = 110)
  ids <- make.names(names(d)[4:5])
  do.call(session$setInputs, c(setNames(as.list(c("maximize", "maximize")), paste0("dx_goal_", ids)),
                               setNames(as.list(c(10, 30)), paste0("dx_lo_", ids)), setNames(as.list(c(20, 45)), paste0("dx_hi_", ids))))
  session$setInputs(dx_opt = 1)
  for (ty in c("design", "contour", "surface", "pareto", "main", "inter", "perturb", "pva", "diag", "halfnorm", "runorder", "desir"))
    step(paste("DoE graph:", ty), { session$setInputs(dx_plot = ty); x <- output$dx_plot_out; save_dl(output$download_graph_png, paste0("doe_", ty, ".png")) })
  step("DoE Word report", { session$setInputs(rep_doe_paper = "A4", rep_doe_orient = "auto"); save_dl(output$rep_doe_docx, "doe_report.docx") })
}), error = function(e) cat("testServer ERROR:", conditionMessage(e), "\n"))
sink(type = "message"); sink(); close(zz)
