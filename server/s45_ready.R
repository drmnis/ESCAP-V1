# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# TRUE / FALSE flags used by the Results tabs (esc_gate in ui/04_report_bar.R): headings, tables and
# report buttons appear only once the corresponding analysis has produced results.

  esc_ready <- function(id, expr) {
    output[[id]] <- reactive(isTRUE(tryCatch(!is.null(expr()), error = function(e) FALSE)))
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  esc_ready("ok_uni", function() st_result())
  esc_ready("ok_chi", function() chi_result())
  esc_ready("ok_doe_design", function() dz_result())
  esc_ready("ok_doe_fit", function() dx_models())
  esc_ready("ok_doe_any", function() if (is.null(tryCatch(dz_result(), error = function(e) NULL))) dx_models() else TRUE)
  esc_ready("ok_prep", function() { req(input$file_multi); prep_data() })
  esc_ready("ok_pca", function() pca_result())
  esc_ready("ok_plsr", function() plsr_result())
  esc_ready("ok_da", function() da_result())
  esc_ready("ok_corr", function() corr_result())
