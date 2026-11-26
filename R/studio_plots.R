# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Plot studio: scatter (symbols, lines, fits), time series, dual Y axis, contour and spider plots.

esc_summarise_xy <- function(d, by) {
  s <- stats::aggregate(d$y, d[by], function(v) c(m = mean(v, na.rm = TRUE), sd = stats::sd(v, na.rm = TRUE), n = sum(is.finite(v))))
  out <- s[by]; out$y <- s$x[, "m"]; out$sd <- s$x[, "sd"]; out$se <- s$x[, "sd"] / sqrt(s$x[, "n"]); out
}

esc_parse_time <- function(x) {
  if (inherits(x, c("Date", "POSIXt")) || is.numeric(x)) return(x)
  for (f in c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%Y/%m/%d", "%d.%m.%Y")) {
    d <- as.Date(as.character(x), format = f); if (mean(!is.na(d)) > 0.9) return(d)
  }
  x
}

# ---- scatter
esc_plot_scatter <- function(df, x, y, group = NULL, shape_by_group = TRUE, shape = 16, size = 2.5,
                             line = c("none", "connect", "lm", "loess", "poly2"), se = TRUE, labels = NULL,
                             facet = NULL, alpha = 0.9, show_eq = FALSE) {
  line <- match.arg(line)
  d <- data.frame(x = df[[x]], y = df[[y]])
  d$g <- if (!is.null(group) && nzchar(group)) factor(df[[group]]) else factor("All")
  if (!is.null(facet) && nzchar(facet)) d$f <- df[[facet]]
  if (!is.null(labels) && nzchar(labels)) d$lab <- as.character(df[[labels]])
  d <- d[is.finite(d$x) & is.finite(d$y), ]
  multi <- nlevels(d$g) > 1
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g, fill = g, group = g))
  if (multi && shape_by_group) p <- p + ggplot2::geom_point(ggplot2::aes(shape = g), size = size, alpha = alpha) +
                                   ggplot2::scale_shape_manual(values = rep(c(16, 17, 15, 18, 21, 22, 24, 23, 25, 3, 4, 8), 3))
  else p <- p + ggplot2::geom_point(shape = shape, size = size, alpha = alpha)
  if (line == "connect") p <- p + ggplot2::geom_line(data = d[order(d$g, d$x), ], linewidth = 0.7)
  if (line == "lm") p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = se, linewidth = 0.8, alpha = 0.15)
  if (line == "poly2") p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = se, linewidth = 0.8, alpha = 0.15)
  if (line == "loess") p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = se, linewidth = 0.8, alpha = 0.15)
  if (show_eq && line %in% c("lm", "poly2")) {
    eq <- do.call(rbind, lapply(split(d, d$g), function(s) {
      f <- if (line == "lm") stats::lm(y ~ x, s) else stats::lm(y ~ poly(x, 2, raw = TRUE), s); b <- stats::coef(f)
      lab <- if (line == "lm") sprintf("y = %.3g %s %.3gx, R\u00b2 = %.3f", b[1], ifelse(b[2] < 0, "-", "+"), abs(b[2]), summary(f)$r.squared)
             else sprintf("y = %.3g + %.3gx + %.3gx\u00b2, R\u00b2 = %.3f", b[1], b[2], b[3], summary(f)$r.squared)
      data.frame(g = s$g[1], lab = lab)
    }))
    eq$y <- Inf; eq$vj <- 1.5 * seq_len(nrow(eq))
    p <- p + ggplot2::geom_text(data = eq, ggplot2::aes(x = -Inf, y = y, label = lab, vjust = vj), hjust = -0.05, size = 3.4, show.legend = FALSE, inherit.aes = FALSE,
                                colour = if (multi) scales::hue_pal()(nrow(eq)) else "black")
  }
  if (!is.null(d$lab)) p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = lab), size = 3, show.legend = FALSE)
  if (!is.null(d$f)) p <- p + ggplot2::facet_wrap(~f, scales = "free")
  p + ggplot2::labs(x = x, y = y, colour = group, fill = group, shape = group) + esc_theme() +
    (if (!multi) ggplot2::theme(legend.position = "none") else NULL)
}

# ---- time series (several responses, optional groups, replicate means +- SE)
esc_plot_timeseries <- function(df, time, ys, group = NULL, error = c("se", "sd", "none"), points = TRUE,
                                facet_vars = TRUE, smooth = FALSE, date_format = NULL) {
  error <- match.arg(error)
  tt <- esc_parse_time(df[[time]])
  long <- do.call(rbind, lapply(ys, function(v) data.frame(t = tt, y = df[[v]], Variable = v,
                      g = if (!is.null(group) && nzchar(group)) as.character(df[[group]]) else "All")))
  long <- long[!is.na(long$t) & is.finite(long$y), ]
  s <- esc_summarise_xy(long, c("t", "Variable", "g"))
  s$e <- switch(error, se = s$se, sd = s$sd, none = 0); s$e[!is.finite(s$e)] <- 0
  multi_g <- length(unique(s$g)) > 1
  map <- if (multi_g) ggplot2::aes(t, y, colour = g, group = g, shape = g) else if (!facet_vars && length(ys) > 1)
    ggplot2::aes(t, y, colour = Variable, group = Variable, shape = Variable) else ggplot2::aes(t, y, group = 1)
  p <- ggplot2::ggplot(s, map) + ggplot2::geom_line(linewidth = 0.8)
  if (points) p <- p + ggplot2::geom_point(size = 2.3)
  if (error != "none") p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = y - e, ymax = y + e), width = 0, alpha = 0.7)
  if (smooth) p <- p + ggplot2::geom_smooth(data = long, ggplot2::aes(t, y), method = "loess", formula = y ~ x, se = TRUE,
                                             inherit.aes = FALSE, colour = "grey30", linewidth = 0.5, linetype = 2)
  if (facet_vars && length(ys) > 1) p <- p + ggplot2::facet_wrap(~Variable, scales = "free_y")
  if (inherits(s$t, "Date")) p <- p + ggplot2::scale_x_date(date_labels = date_format %||% "%d %b %Y")
  p + ggplot2::labs(x = time, y = if (length(ys) == 1) ys else "Value",
                    caption = if (error != "none") paste("Points: mean; bars:", toupper(error)) else NULL,
                    colour = if (multi_g) group else "Variable", shape = if (multi_g) group else "Variable") + esc_theme()
}

# ---- dual Y axis: y1 on the left (bars or line), y2 on the right (line), automatic rescaling
esc_plot_dual_y <- function(df, x, y1, y2, type1 = c("bar", "line"), col1 = "#4D4D4D", col2 = "#D55E00") {
  type1 <- match.arg(type1)
  d <- data.frame(x = df[[x]], a = df[[y1]], b = df[[y2]])
  num_x <- is.numeric(d$x)
  s <- stats::aggregate(cbind(a, b) ~ x, d, mean)
  ra <- range(c(0, s$a), na.rm = TRUE); rb <- range(s$b, na.rm = TRUE)
  k <- diff(ra) / diff(rb); if (!is.finite(k) || k == 0) k <- 1
  off <- ra[1] - rb[1] * k
  s$b2 <- s$b * k + off
  if (!num_x) s$x <- factor(s$x, levels = unique(d$x))
  p <- ggplot2::ggplot(s, ggplot2::aes(x))
  p <- if (type1 == "bar") p + ggplot2::geom_col(ggplot2::aes(y = a), fill = col1, width = 0.6)
       else p + ggplot2::geom_line(ggplot2::aes(y = a, group = 1), colour = col1, linewidth = 0.9) + ggplot2::geom_point(ggplot2::aes(y = a), colour = col1, size = 2.4)
  p + ggplot2::geom_line(ggplot2::aes(y = b2, group = 1), colour = col2, linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = b2), colour = col2, size = 2.6, shape = 17) +
    ggplot2::scale_y_continuous(name = y1, sec.axis = ggplot2::sec_axis(transform = ~ (. - off) / k, name = y2)) +
    ggplot2::labs(x = x) + esc_theme() +
    ggplot2::theme(axis.title.y.left = ggplot2::element_text(colour = col1), axis.text.y.left = ggplot2::element_text(colour = col1),
                   axis.title.y.right = ggplot2::element_text(colour = col2), axis.text.y.right = ggplot2::element_text(colour = col2))
}

# ---- contour (irregular data interpolated, gridded data used directly)
esc_grid_xyz <- function(x, y, z, n = 100, method = c("linear", "spline", "loess")) {
  method <- match.arg(method)
  ok <- is.finite(x) & is.finite(y) & is.finite(z); x <- x[ok]; y <- y[ok]; z <- z[ok]
  if (method == "loess") {
    fit <- stats::loess(z ~ x * y, span = 0.75, degree = 2)
    g <- expand.grid(x = seq(min(x), max(x), length.out = n), y = seq(min(y), max(y), length.out = n))
    g$z <- as.numeric(stats::predict(fit, g)); return(g)
  }
  agg <- stats::aggregate(z ~ x + y, data = data.frame(x = x, y = y, z = z), FUN = mean)
  ip <- interp::interp(agg$x, agg$y, agg$z, nx = n, ny = n, method = method, extrap = FALSE, duplicate = "mean")
  g <- expand.grid(x = ip$x, y = ip$y); g$z <- as.numeric(ip$z); g
}

esc_plot_contour <- function(df, x, y, z, type = c("filled", "lines", "heat"), bins = 10, n = 100,
                             method = "linear", palette = "viridis", show_points = TRUE, label_lines = TRUE) {
  type <- match.arg(type)
  g <- esc_grid_xyz(df[[x]], df[[y]], df[[z]], n, method); g <- g[is.finite(g$z), ]
  pts <- data.frame(x = df[[x]], y = df[[y]])
  p <- ggplot2::ggplot(g, ggplot2::aes(x, y, z = z))
  if (type == "filled") p <- p + ggplot2::geom_contour_filled(bins = bins) + ggplot2::geom_contour(bins = bins, colour = "grey20", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_d(option = switch(palette, viridis = "D", magma = "A", inferno = "B", plasma = "C", cividis = "E", "D"), name = z)
  if (type == "heat") p <- p + ggplot2::geom_raster(ggplot2::aes(fill = z), interpolate = TRUE) +
    ggplot2::geom_contour(bins = bins, colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(option = switch(palette, viridis = "D", magma = "A", inferno = "B", plasma = "C", cividis = "E", "D"), name = z)
  if (type == "lines") {
    p <- p + ggplot2::geom_contour(ggplot2::aes(colour = ggplot2::after_stat(level)), bins = bins, linewidth = 0.7) +
      ggplot2::scale_colour_viridis_c(name = z)
    if (label_lines && requireNamespace("metR", quietly = TRUE)) p <- p + metR::geom_text_contour(bins = bins, size = 3, stroke = 0.2)
  }
  if (show_points) p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(x, y), inherit.aes = FALSE, shape = 3, size = 1.6, colour = "black")
  p + ggplot2::labs(x = x, y = y) + ggplot2::coord_cartesian(expand = FALSE) + esc_theme()
}

# ---- spider / radar
esc_coord_radar <- function(theta = "x", start = 0, direction = 1) {
  r <- if (theta == "x") "y" else "x"
  ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar, theta = theta, r = r, start = start,
                   direction = sign(direction), clip = "off", is_linear = function(coord) TRUE)
}

esc_plot_spider <- function(df, vars, group = NULL, scale = c("minmax", "none", "zscore"), fill = TRUE,
                            facet = FALSE, alpha = 0.18, max_groups = 12) {
  scale <- match.arg(scale)
  g <- if (!is.null(group) && nzchar(group)) as.character(df[[group]]) else rep("All", nrow(df))
  m <- stats::aggregate(df[, vars, drop = FALSE], list(Group = g), mean, na.rm = TRUE)
  if (nrow(m) > max_groups) m <- m[seq_len(max_groups), ]
  M <- as.matrix(m[, vars, drop = FALSE])
  if (scale == "minmax") M <- apply(M, 2, function(v) if (diff(range(v)) == 0) rep(0.5, length(v)) else (v - min(v)) / diff(range(v)))
  if (scale == "zscore") M <- apply(M, 2, function(v) if (stats::sd(v) == 0) v * 0 else (v - mean(v)) / stats::sd(v))
  M <- matrix(M, nrow = nrow(m), dimnames = list(m$Group, vars))
  long <- data.frame(Group = rep(m$Group, times = length(vars)), Variable = factor(rep(vars, each = nrow(m)), levels = vars),
                     Value = as.numeric(M))
  long <- long[order(long$Group, long$Variable), ]
  closed <- do.call(rbind, lapply(split(long, long$Group), function(s) {
    s <- s[order(s$Variable), ]; s$id <- seq_len(nrow(s))
    first <- s[1, ]; first$id <- nrow(s) + 1; rbind(s, first)
  }))
  lims <- range(c(0, closed$Value))
  p <- ggplot2::ggplot(closed, ggplot2::aes(id, Value, group = Group, colour = Group, fill = Group))
  if (fill) p <- p + ggplot2::geom_polygon(alpha = alpha, linewidth = 0.8) else p <- p + ggplot2::geom_path(linewidth = 0.9)
  p <- p + ggplot2::geom_point(size = 2) + esc_coord_radar() +
    ggplot2::scale_x_continuous(breaks = seq_along(vars), labels = vars, limits = c(1, length(vars) + 1)) +
    ggplot2::scale_y_continuous(limits = lims) +
    ggplot2::labs(x = NULL, y = NULL, caption = switch(scale, minmax = "Group means scaled 0-1 per attribute",
                                                       zscore = "Group means as z-scores", none = "Group means")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey80"), axis.text.y = ggplot2::element_text(size = 8, colour = "grey40"),
                   axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                   plot.margin = ggplot2::margin(15, 30, 10, 30))   # room for attribute labels at the rim
  if (facet) p <- p + ggplot2::facet_wrap(~Group)
  p
}
