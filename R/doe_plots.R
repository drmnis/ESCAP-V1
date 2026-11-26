# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Design-of-Experiments plots: Pareto, main effects, interaction, contour, 3-D surface,
# perturbation, predicted vs actual, mixture ternary contour, desirability.

esc_lab <- function(m, f) unname(m$labels[[f]] %||% f)

esc_doe_grid <- function(m, f1, f2, fixed = NULL, n = 60) {
  base <- as.list((m$region_low + m$region_high) / 2); if (!is.null(fixed)) base[names(fixed)] <- fixed
  g <- expand.grid(a = seq(m$region_low[f1], m$region_high[f1], length.out = n), b = seq(m$region_low[f2], m$region_high[f2], length.out = n))
  nd <- as.data.frame(lapply(m$factors, function(f) if (f == f1) g$a else if (f == f2) g$b else rep(base[[f]], nrow(g))))
  names(nd) <- m$factors
  g$z <- esc_doe_predict(m, nd); g
}

esc_plot_doe_pareto <- function(m) {
  e <- esc_doe_effects(m); e$Term <- factor(e$Term, levels = e$Term[order(e$t)])
  ggplot2::ggplot(e, ggplot2::aes(t, Term, fill = Sign)) + ggplot2::geom_col(width = 0.7, colour = "black", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = e$limit[1], linetype = 2, colour = "red") +
    ggplot2::scale_fill_manual(values = c(Positive = "#E69F00", Negative = "#56B4E9")) +
    ggplot2::labs(x = "Standardised effect |t|", y = NULL, fill = NULL, caption = sprintf("Dashed line: t(0.975, %d) = %.2f", m$fit$df.residual, e$limit[1])) + esc_theme()
}

esc_plot_doe_main <- function(m, n = 50) {
  d <- do.call(rbind, lapply(m$factors, function(f) {
    base <- as.list((m$region_low + m$region_high) / 2); x <- seq(m$region_low[f], m$region_high[f], length.out = n)
    nd <- as.data.frame(lapply(m$factors, function(g) if (g == f) x else rep(base[[g]], n))); names(nd) <- m$factors
    data.frame(Factor = esc_lab(m, f), x = x, y = esc_doe_predict(m, nd))
  }))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_line(linewidth = 0.9, colour = "#0072B2") +
    ggplot2::facet_wrap(~Factor, scales = "free_x") + ggplot2::labs(x = "Factor level (other factors at centre)", y = esc_lab(m, m$y)) + esc_theme()
}

esc_plot_doe_perturbation <- function(m, n = 50) {
  d <- do.call(rbind, lapply(m$factors, function(f) {
    base <- as.list((m$low + m$high) / 2); x <- seq(-1, 1, length.out = n)
    act <- (m$high[f] + m$low[f]) / 2 + x * (m$high[f] - m$low[f]) / 2
    nd <- as.data.frame(lapply(m$factors, function(g) if (g == f) act else rep(base[[g]], n))); names(nd) <- m$factors
    data.frame(Factor = esc_lab(m, f), x = x, y = esc_doe_predict(m, nd))
  }))
  ggplot2::ggplot(d, ggplot2::aes(x, y, colour = Factor, linetype = Factor)) + ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = "Deviation from reference point (coded units)", y = esc_lab(m, m$y)) + esc_theme()
}

esc_plot_doe_interaction <- function(m, f1, f2) {
  base <- as.list((m$region_low + m$region_high) / 2); x <- seq(m$region_low[f1], m$region_high[f1], length.out = 50)
  d <- do.call(rbind, lapply(c(m$region_low[f2], m$region_high[f2]), function(v) {
    nd <- as.data.frame(lapply(m$factors, function(g) if (g == f1) x else if (g == f2) rep(v, 50) else rep(base[[g]], 50))); names(nd) <- m$factors
    data.frame(x = x, y = esc_doe_predict(m, nd), lev = sprintf("%s = %g", esc_lab(m, f2), v))
  }))
  ggplot2::ggplot(d, ggplot2::aes(x, y, colour = lev, linetype = lev)) + ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = esc_lab(m, f1), y = esc_lab(m, m$y), colour = NULL, linetype = NULL) + esc_theme()
}

esc_plot_doe_contour <- function(m, f1, f2, fixed = NULL, bins = 12, points = TRUE) {
  g <- esc_doe_grid(m, f1, f2, fixed)
  p <- ggplot2::ggplot(g, ggplot2::aes(a, b, z = z)) + ggplot2::geom_contour_filled(bins = bins) +
    ggplot2::geom_contour(bins = bins, colour = "grey20", linewidth = 0.25) +
    ggplot2::scale_fill_viridis_d(name = esc_lab(m, m$y)) +
    ggplot2::labs(x = esc_lab(m, f1), y = esc_lab(m, f2),
                  caption = if (length(m$factors) > 2) paste("Other factors held at", paste(sprintf("%s = %g", vapply(setdiff(m$factors, c(f1, f2)), function(f) esc_lab(m, f), ""),
                    vapply(setdiff(m$factors, c(f1, f2)), function(f) (fixed[[f]] %||% ((m$region_low[f] + m$region_high[f]) / 2)), 0)), collapse = ", ")) else NULL) +
    ggplot2::coord_cartesian(expand = FALSE) + esc_theme()
  if (points) p <- p + ggplot2::geom_point(data = m$data, ggplot2::aes(.data[[f1]], .data[[f2]]), inherit.aes = FALSE, shape = 21, fill = "white", size = 2)
  p
}

# 3-D response surface (base graphics; returned as a function so it can be drawn on any device)
esc_plot_doe_surface <- function(m, f1, f2, fixed = NULL, theta = 35, phi = 25) {
  g <- esc_doe_grid(m, f1, f2, fixed, n = 40)
  x <- unique(g$a); y <- unique(g$b); z <- matrix(g$z, length(x), length(y))
  function() {
    op <- graphics::par(mar = c(1, 1, 2, 1)); on.exit(graphics::par(op))
    zf <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
    cols <- grDevices::hcl.colors(100, "viridis")[cut(zf, 100)]
    graphics::persp(x, y, z, theta = theta, phi = phi, col = cols, border = "grey40", lwd = 0.3, ticktype = "detailed",
                    xlab = esc_lab(m, f1), ylab = esc_lab(m, f2), zlab = esc_lab(m, m$y), expand = 0.7, cex.axis = 0.7, cex.lab = 0.9)
  }
}

esc_plot_doe_pred_actual <- function(m) {
  d <- data.frame(Actual = m$data[[m$y]], Predicted = stats::fitted(m$fit))
  r <- range(c(d$Actual, d$Predicted))
  ggplot2::ggplot(d, ggplot2::aes(Predicted, Actual)) + ggplot2::geom_abline(linetype = 2, colour = "grey50") +
    ggplot2::geom_point(size = 2.6, shape = 21, fill = "#56B4E9") + ggplot2::coord_equal(xlim = r, ylim = r) +
    ggplot2::labs(caption = sprintf("R\u00b2 = %.4f", summary(m$fit)$r.squared)) + esc_theme()
}

# Ternary contour for three-component mixtures
esc_plot_mixture <- function(m, bins = 12, n = 80) {
  validate_k <- length(m$factors) == 3
  if (!validate_k) stop("The ternary plot needs exactly three mixture components.")
  g <- expand.grid(i = 0:n, j = 0:n); g <- g[g$i + g$j <= n, ]
  nd <- data.frame(g$i / n, g$j / n, 1 - (g$i + g$j) / n); names(nd) <- m$factors
  z <- as.numeric(stats::predict(m$fit, nd))
  xy <- data.frame(x = nd[[2]] + nd[[3]] / 2, y = nd[[3]] * sqrt(3) / 2, z = z)
  ip <- interp::interp(xy$x, xy$y, xy$z, nx = 120, ny = 120, duplicate = "mean")
  gg <- expand.grid(x = ip$x, y = ip$y); gg$z <- as.numeric(ip$z); gg <- gg[is.finite(gg$z), ]
  tri <- data.frame(x = c(0, 1, 0.5, 0), y = c(0, 0, sqrt(3) / 2, 0))
  lab <- data.frame(x = c(-0.04, 1.04, 0.5), y = c(-0.04, -0.04, sqrt(3) / 2 + 0.05), t = vapply(m$factors, function(f) esc_lab(m, f), ""))
  ggplot2::ggplot(gg, ggplot2::aes(x, y, z = z)) + ggplot2::geom_contour_filled(bins = bins) +
    ggplot2::geom_contour(bins = bins, colour = "grey20", linewidth = 0.2) + ggplot2::geom_path(data = tri, ggplot2::aes(x, y), inherit.aes = FALSE) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x, y, label = t), inherit.aes = FALSE, fontface = "bold") +
    ggplot2::scale_fill_viridis_d(name = esc_lab(m, m$y)) + ggplot2::coord_equal() + ggplot2::theme_void() +
    ggplot2::labs(caption = "Vertices = pure components")
}

esc_plot_desirability <- function(models, opt, f1, f2, n = 50) {
  m <- models[[1]]; base <- stats::setNames(as.list(opt$settings$Optimum), m$factors)
  g <- expand.grid(a = seq(m$region_low[f1], m$region_high[f1], length.out = n), b = seq(m$region_low[f2], m$region_high[f2], length.out = n))
  g$D <- apply(g, 1, function(r) { x <- unlist(base); x[f1] <- r[1]; x[f2] <- r[2]; opt$D(x) })
  ggplot2::ggplot(g, ggplot2::aes(a, b, z = D)) + ggplot2::geom_contour_filled(breaks = seq(0, 1, 0.1)) +
    ggplot2::annotate("point", x = base[[f1]], y = base[[f2]], shape = 4, size = 4, stroke = 1.5, colour = "red") +
    ggplot2::scale_fill_viridis_d(name = "Desirability") + ggplot2::labs(x = esc_lab(m, f1), y = esc_lab(m, f2)) +
    ggplot2::coord_cartesian(expand = FALSE) + esc_theme()
}
