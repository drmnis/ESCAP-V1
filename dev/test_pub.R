# ESCAP dev: publication-ready tables for every univariate analysis (SD and SE), plus esc_cld check
out <- "D:/API_R/dev/test_pub_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
dev <- "D:/API_R/dev/"
step <- function(name, expr) {
  r <- tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("== %-55s %s\n", name, r))
}
setwd("D:/API_R/ESCAP")
suppressPackageStartupMessages(library(shiny))
www <- "D:/API_R/ESCAP/www/"
up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
plain <- function(pt) { t <- pt$table; for (j in seq_along(t)) t[[j]] <- gsub("<sup>(.*?)</sup>", "^\\1", gsub("</?b>", "", t[[j]]))
  print(t, row.names = FALSE); cat("Note:", pt$note, "\n\n") }

# esc_cld against multcompView-like known answer
source("R/stats_pub.R")
m <- c(A = 10, B = 9.5, C = 7, D = 6.8)
P <- matrix(1, 4, 4, dimnames = list(names(m), names(m))); P["A", "C"] <- P["C", "A"] <- 0.01; P["A", "D"] <- P["D", "A"] <- 0.01
P["B", "D"] <- P["D", "B"] <- 0.02
cat("CLD (expect a, ab, bc, c):", esc_cld(m, P), "\n\n")

ex <- read.csv(paste0(www, "example_data.csv")); write.csv(ex[ex$treatment %in% c("Control", "P82"), ], paste0(dev, "ttest_test.csv"), row.names = FALSE)
res <- tryCatch(testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(enable_uni = TRUE, file_uni = up(paste0(www, "example_data.csv")), spread_type = "sd")
  run <- function(lab, ...) step(lab, {
    session$setInputs(..., uni_run = sample(1e6, 1))
    for (sp in c("sd", "se")) { session$setInputs(spread_type = sp); pt <- st_pub(); if (!is.null(pt$error)) stop(pt$error)
      cat("--", lab, "[", sp, "]\n"); plain(pt) }
    x <- output$st_pub; x <- output$st_results
  })
  run("One-way ANOVA (Tukey)", uni_analysis = "anova1", st_y = c("TSS", "Flavonoid"), st_A = "treatment", st_block = "", st_posthoc = "HSD", st_sstype = "2", st_alpha = 0.05)
  run("Two-way ANOVA (condition x treatment)", uni_analysis = "anova2", st_y = c("TSS"), st_A = "condition", st_B = "treatment", st_inter = TRUE)
  run("ANCOVA", uni_analysis = "ancova", st_y = "TSS", st_A = "treatment", st_cov = "TA")
  run("Kruskal-Wallis", uni_analysis = "kruskal", st_y = c("Flavonoid", "TSS"), st_A = "treatment", st_padj = "holm")
  run("Descriptive", uni_analysis = "desc", st_y = c("TSS", "TA"), st_A = "treatment", st_conf = 0.95)
  run("Correlation", uni_analysis = "cor", st_y = c("TSS", "TA", "Flavonoid"), st_cormethod = "pearson", st_padj = "none")
  run("Regression", uni_analysis = "reg", st_y = "Flavonoid", st_x = "TSS", st_degree = 1)
  run("Kinetics", uni_analysis = "kin", st_x = "min", st_y = "Flavonoid", st_A = "condition")
  step("Stats report PDF with publication table", { f <- output$rep_stats_pdf; file.copy(f, paste0(dev, "test_report_pub.pdf"), overwrite = TRUE) })
  session$setInputs(file_uni = up(paste0(dev, "ttest_test.csv")))
  run("t-test", uni_analysis = "ttest", st_y = c("TSS", "Flavonoid"), st_ttype = "independent", st_A = "treatment", st_pair = "", st_mu = 0, st_vareq = FALSE, st_alt = "two.sided")
}), error = function(e) cat("testServer ERROR:", conditionMessage(e), "\n"))
sink(type = "message"); sink(); close(zz)
