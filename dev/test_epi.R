# ESCAP dev test: epidemiological engine on chi_square_data.csv
out <- "D:/API_R/dev/test_epi_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
res <- tryCatch({
  setwd("D:/API_R/ESCAP")
  if (!requireNamespace("logistf", quietly = TRUE)) install.packages("logistf", repos = "https://cloud.r-project.org")
  source("R/chemometrics.R"); source("R/epi.R")
  df <- read.csv("www/chi_square_data.csv", stringsAsFactors = FALSE, check.names = FALSE)
  colnames(df) <- gsub("\\.", "_", make.names(trimws(colnames(df)), unique = TRUE))
  cat("columns:", paste(names(df), collapse = " | "), "\n")
  cat("outcome cols:", names(df)[vapply(df, esc_is_outcome_col, logical(1))], "\n")
  cat("id cols:", names(df)[mapply(esc_is_id_col, names(df), df)], "\n")
  preds <- c("Location", "Age", "Sex", "Breed", "BCS", "Farming_System", "Access_of_dogs_and_cats")
  for (pt in c("Sarco", "Neo")) {
    r <- esc_epi_analysis(df, pt, preds, refs = list(), p_enter = 0.2)
    cat("\n=====", pt, "overall:", round(r$overall, 2), "\n")
    t <- r$table
    t[] <- lapply(t, function(x) if (is.numeric(x)) signif(x, 3) else x)
    print(t[, c("Variable", "Level", "N", "Positive", "Prevalence", "Chi2", "P_test", "Test", "COR", "COR_LCL", "COR_UCL", "AOR", "AOR_P")], row.names = FALSE)
    print(r$fit_stats, row.names = FALSE)
    # cross-check one chi-square with base R
    tb <- table(df$Breed, esc_binary_outcome(df[[pt]]))
    cat("check Breed chisq:", round(chisq.test(tb, correct = FALSE)$statistic, 3), "\n")
  }
  "done"
}, error = function(e) paste("ERROR:", conditionMessage(e), paste(deparse(conditionCall(e)), collapse = "")))
cat(res, "\n")
sink(type = "message"); sink(); close(zz)
cat("TEST EPI FINISHED\n")
