# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Publication-style plots for the statistics module (all ggplot, so the style panel applies).

esc_theme <- function() ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"), strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(face = "bold"))

esc_plot_box <- function(df, y, group, letters = NULL, ylab = y, violin = FALSE) {
  d <- data.frame(y = df[[y]], g = factor(df[[group]])); d <- d[is.finite(d$y) & !is.na(d$g), ]
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y, fill = g))
  p <- if (violin) p + ggplot2::geom_violin(alpha = 0.5, trim = FALSE) + ggplot2::geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white")
       else p + ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6)
  p <- p + ggplot2::geom_jitter(width = 0.08, height = 0, size = 1.6, alpha = 0.7, shape = 21, fill = "white") +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2.6, fill = "black") +
    ggplot2::labs(x = group, y = ylab) + esc_theme() + ggplot2::theme(legend.position = "none")
  if (!is.null(letters)) {
    top <- tapply(d$y, d$g, max, na.rm = TRUE); rng <- diff(range(d$y))
    lt <- data.frame(g = factor(letters$Level, levels = levels(d$g)), y = top[letters$Level] + 0.06 * rng, lab = trimws(letters$Group))
    p <- p + ggplot2::geom_text(data = lt, ggplot2::aes(g, y, label = lab), inherit.aes = FALSE, fontface = "bold", size = 4.2)
  }
  p
}

esc_plot_interaction <- function(res, ylab = res$y, error = "se") {
  d <- res$data; A <- res$A; B <- res$B
  s <- stats::aggregate(d[[res$y]], list(A = d[[A]], B = d[[B]]), function(x) c(m = mean(x), sd = stats::sd(x), n = length(x)))
  s <- data.frame(A = s$A, B = s$B, m = s$x[, "m"], sd = s$x[, "sd"], n = s$x[, "n"])
  s$e <- if (error == "sd") s$sd else s$sd / sqrt(s$n)
  ggplot2::ggplot(s, ggplot2::aes(A, m, colour = B, group = B, shape = B)) +
    ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(0.15)) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(0.15)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = m - e, ymax = m + e), width = 0.12, position = ggplot2::position_dodge(0.15)) +
    ggplot2::labs(x = A, y = paste0(ylab, " (mean \u00b1 ", toupper(error), ")"), colour = B, shape = B) + esc_theme()
}

esc_plot_twoway_bars <- function(res, ylab = res$y, error = "se") {
  d <- res$data; A <- res$A; B <- res$B
  s <- stats::aggregate(d[[res$y]], list(A = d[[A]], B = d[[B]]), function(x) c(m = mean(x), sd = stats::sd(x), n = length(x)))
  s <- data.frame(A = s$A, B = s$B, m = s$x[, "m"], sd = s$x[, "sd"], n = s$x[, "n"])
  s$e <- if (error == "sd") s$sd else s$sd / sqrt(s$n)
  if (!is.null(res$letters_AB)) {
    key <- paste(s$A, s$B, sep = ":"); s$lab <- trimws(res$letters_AB$Group[match(key, res$letters_AB$Level)])
  } else s$lab <- ""
  ggplot2::ggplot(s, ggplot2::aes(A, m, fill = B)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7, colour = "black", linewidth = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = m - e, ymax = m + e), position = ggplot2::position_dodge(0.8), width = 0.25) +
    ggplot2::geom_text(ggplot2::aes(y = m + e, label = lab), position = ggplot2::position_dodge(0.8), vjust = -0.5, size = 3.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = A, y = paste0(ylab, " (mean \u00b1 ", toupper(error), ")"), fill = B) + esc_theme()
}

esc_plot_ancova <- function(res, ylab = res$y) {
  d <- res$data; d$.fit <- stats::fitted(res$lm)
  ggplot2::ggplot(d, ggplot2::aes(.data[[res$covariate]], .data[[res$y]], colour = .data[[res$A]], shape = .data[[res$A]])) +
    ggplot2::geom_point(size = 2.4) + ggplot2::geom_line(ggplot2::aes(y = .fit), linewidth = 0.9) +
    ggplot2::labs(x = res$covariate, y = ylab, colour = res$A, shape = res$A,
                  caption = "Lines: ANCOVA fit with a common slope (parallel lines)") + esc_theme()
}

esc_plot_diagnostics <- function(fit) {
  d <- data.frame(f = stats::fitted(fit), r = stats::rstandard(fit))
  p1 <- ggplot2::ggplot(d, ggplot2::aes(f, r)) + ggplot2::geom_hline(yintercept = c(-2, 0, 2), linetype = c(3, 2, 3)) +
    ggplot2::geom_point(alpha = 0.7) + ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x, colour = "red", linewidth = 0.6) +
    ggplot2::labs(x = "Fitted values", y = "Standardised residuals", title = "Residuals vs fitted") + esc_theme()
  p2 <- ggplot2::ggplot(d, ggplot2::aes(sample = r)) + ggplot2::stat_qq() + ggplot2::stat_qq_line(colour = "red") +
    ggplot2::labs(x = "Theoretical quantiles", y = "Standardised residuals", title = "Normal Q-Q") + esc_theme()
  p1 + p2
}

esc_plot_regression <- function(reg, xlab = "x", ylab = "y") {
  d <- reg$data; nd <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
  pr <- stats::predict(reg$fit, newdata = nd, interval = "confidence"); nd <- cbind(nd, pr)
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_ribbon(data = nd, ggplot2::aes(x, ymin = lwr, ymax = upr), inherit.aes = FALSE, alpha = 0.2) +
    ggplot2::geom_line(data = nd, ggplot2::aes(x, fit), linewidth = 0.9) + ggplot2::geom_point(size = 2.2) +
    ggplot2::annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
                      label = sprintf("%s\nR\u00b2 = %.4f", reg$equation, reg$r2)) +
    ggplot2::labs(x = xlab, y = ylab) + esc_theme()
}

esc_plot_kinetics <- function(kin, xlab = "Time", ylab = "Concentration") {
  ggplot2::ggplot(kin$data, ggplot2::aes(t, C, colour = Group)) + ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(data = kin$curves, ggplot2::aes(t, C, colour = Group), linewidth = 0.8) +
    ggplot2::facet_wrap(~Model) + ggplot2::labs(x = xlab, y = ylab) + esc_theme()
}

esc_plot_means_letters <- function(letters, ylab = "Response", xlab = "Treatment", error = "SE") {
  d <- letters; d$Level <- factor(d$Level, levels = d$Level[order(-d$Mean)]); d$e <- if (error == "SD") d$SD else d$SE
  ggplot2::ggplot(d, ggplot2::aes(Level, Mean)) + ggplot2::geom_col(fill = "grey75", colour = "black", width = 0.65) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Mean - e, ymax = Mean + e), width = 0.2) +
    ggplot2::geom_text(ggplot2::aes(y = Mean + e, label = trimws(Group)), vjust = -0.6, size = 4) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = xlab, y = paste0(ylab, " (mean \u00b1 ", error, ")")) + esc_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (nrow(d) > 6) 45 else 0, hjust = if (nrow(d) > 6) 1 else 0.5))
}
