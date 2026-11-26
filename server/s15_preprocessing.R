# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s15_preprocessing  (sourced inside server(), local = TRUE)

  # Preprocessing wrapper: delegates to the validated engine in R/chemometrics.R.
  # (The former in-line version lost variable names after Savitzky-Golay, could
  #  divide by zero in SNV, and applied log() to negative values.)
  pca_preprocess <- function(X, spectral_steps, scaling_step, centering_step,
                             sg_width, sg_order, sg_deriv) {
    tryCatch(
      esc_preprocess(X, spectral = spectral_steps %||% "none", scaling = scaling_step %||% "none",
                     centering = centering_step %||% "none", sg_width = sg_width %||% 11,
                     sg_order = sg_order %||% 2, sg_deriv = sg_deriv %||% 0),
      error = function(e) validate(need(FALSE, paste("Preprocessing:", conditionMessage(e))))
    )
  }
