# ESCAP dev: Results-tab gates (FALSE before an analysis, TRUE after) and no errors on a fresh session
out <- "D:/API_R/dev/test_gates_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
setwd("D:/API_R/ESCAP"); suppressPackageStartupMessages(library(shiny))
ed <- "D:/API_R/ESCAP/example_data/"
up <- function(f) data.frame(name = f, size = file.size(paste0(ed, f)), type = "text/csv", datapath = paste0(ed, f), stringsAsFactors = FALSE)
flags <- c("ok_uni", "ok_chi", "ok_doe_design", "ok_doe_fit", "ok_doe_any", "ok_prep", "ok_pca", "ok_plsr", "ok_da", "ok_corr")
res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  show <- function(lbl) cat(sprintf("%-28s %s\n", lbl, paste(paste0(flags, "=", vapply(flags, function(f) as.character(tryCatch(output[[f]], error = function(e) "ERR")), "")), collapse = " ")))
  show("fresh session")
  x <- tryCatch(output$prep_steps_text, error = function(e) paste("prep_steps_text:", conditionMessage(e))); cat("prep_steps_text before upload:", format(x), "\n")
  session$setInputs(enable_uni = TRUE, file_uni = up("01_one_way_anova_RCBD.csv")); d <- dat_raw()
  session$setInputs(uni_analysis = "anova1", st_A = "Treatment", st_block = "", st_cond = "", st_posthoc = "HSD", st_sstype = "2",
                    st_alpha = 0.05, st_y = names(d)[3], uni_run = 1)
  show("after one-way ANOVA")
  session$setInputs(file_multi = up("10_NIR_spectra_octane_PCA_PLSR_PLSDA.csv")); df <- dat_multi()
  session$setInputs(multi_var_range = c(3, ncol(df)), prep_spectral = "snv", prep_scaling = "none", prep_centering = "mc")
  show("after multivariate upload")
  session$setInputs(run_pca = 1, run_corr = 1); show("after PCA + correlation")
}), error = function(e) cat("testServer ERROR:", conditionMessage(e), "\n"))
sink(type = "message"); sink(); close(zz)
