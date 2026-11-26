# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Additional diagnostic graphs for PCA, PLSR, PLS-DA and DoE (ggplot, so Enhance Plots applies).

ESC_EXTRA_GRAPHS <- c("pca_qt", "pca_contrib", "pca_cumvar", "plsr_resid", "plsr_var", "plsr_weights")
ESC_SET_COLS <- c(Calibration = "#0072B2", Prediction = "#D55E00", CV = "#009E73")

# ---------------------------------------------------------------- PCA
# Hotelling T2 vs Q residuals (k PCs); 95% limits: F distribution for T2, Box chi-square approximation for Q
esc_pca_qt <- function(pca, k = 2, alpha = 0.95) {
  k <- max(1, min(k, ncol(pca$x)))
  n <- nrow(pca$x); lam <- pca$sdev^2
  T2 <- rowSums(sweep(pca$x[, 1:k, drop = FALSE]^2, 2, lam[1:k], "/"))
  E <- pca$x[, -(1:k), drop = FALSE]; Q <- if (ncol(E)) rowSums(E^2) else rep(0, n)
  t2lim <- k * (n - 1) * (n + 1) / (n * (n - k)) * stats::qf(alpha, k, max(1, n - k))
  rest <- lam[-(1:k)]; th <- sapply(1:3, function(i) sum(rest^i))
  # Box (1954) chi-square approximation: robust when the Jackson-Mudholkar h0 term is <= 0
  qlim <- if (th[1] > 0 && th[2] > 0) (th[2] / th[1]) * stats::qchisq(alpha, th[1]^2 / th[2]) * (n - 1) / n else NA
  list(T2 = T2, Q = Q, T2_lim = t2lim, Q_lim = qlim, k = k)
}

esc_plot_pca_qt <- function(pca, k = 2, Class = NULL, ID = NULL, label_out = TRUE) {
  q <- esc_pca_qt(pca, k)
  d <- data.frame(T2 = q$T2, Q = q$Q, Class = if (is.null(Class)) "All" else Class, ID = ID %||% seq_along(q$T2))
  d$out <- d$T2 > q$T2_lim | (is.finite(q$Q_lim) & d$Q > q$Q_lim)
  p <- ggplot2::ggplot(d, ggplot2::aes(T2, Q, colour = Class)) + ggplot2::geom_point(size = 2.4, alpha = 0.85) +
    ggplot2::geom_vline(xintercept = q$T2_lim, linetype = 2, colour = "red") +
    ggplot2::labs(x = sprintf("Hotelling T\u00b2 (%d PC)", q$k), y = "Q residuals (SPE)",
                  caption = "Dashed lines: 95% limits (F distribution for T\u00b2; Box chi-square approximation for Q). Labelled points exceed a limit.") +
    esc_theme()
  if (is.finite(q$Q_lim)) p <- p + ggplot2::geom_hline(yintercept = q$Q_lim, linetype = 2, colour = "red")
  if (label_out && any(d$out)) p <- p + ggplot2::geom_text(data = d[d$out, ], ggplot2::aes(label = ID), vjust = -0.7, size = 3, show.legend = FALSE)
  if (identical(unique(d$Class), "All")) p <- p + ggplot2::scale_colour_manual(values = "#0072B2", guide = "none")
  p
}

# Variable contribution (%) to the first k PCs, weighted by eigenvalue
esc_plot_pca_contrib <- function(pca, k = 2, top = 20) {
  k <- max(1, min(k, ncol(pca$rotation))); lam <- pca$sdev[1:k]^2
  ctr <- 100 * rowSums(sweep(pca$rotation[, 1:k, drop = FALSE]^2, 2, lam, "*")) / sum(lam)
  d <- data.frame(Variable = names(ctr) %||% paste0("V", seq_along(ctr)), C = ctr)
  d <- utils::head(d[order(-d$C), ], top); d$Variable <- factor(d$Variable, levels = rev(d$Variable))
  ggplot2::ggplot(d, ggplot2::aes(C, Variable)) + ggplot2::geom_col(width = 0.7, fill = "#0072B2", colour = "black", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 100 / nrow(pca$rotation), linetype = 2, colour = "red") +
    ggplot2::labs(x = sprintf("Contribution to PC1-PC%d (%%)", k), y = NULL,
                  caption = "Dashed line: expected contribution if all variables contributed equally.") + esc_theme()
}

esc_plot_pca_cumvar <- function(pca, max_pc = 15) {
  v <- 100 * pca$sdev^2 / sum(pca$sdev^2); m <- min(max_pc, length(v))
  d <- data.frame(PC = seq_len(m), Individual = v[1:m], Cumulative = cumsum(v)[1:m])
  ggplot2::ggplot(d, ggplot2::aes(PC)) +
    ggplot2::geom_col(ggplot2::aes(y = Individual), fill = "grey70", colour = "black", width = 0.7, linewidth = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = Cumulative), colour = "#D55E00", linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = Cumulative), colour = "#D55E00", size = 2.5) +
    ggplot2::geom_text(ggplot2::aes(y = Cumulative, label = sprintf("%.1f", Cumulative)), vjust = -0.8, size = 3) +
    ggplot2::geom_hline(yintercept = c(80, 90), linetype = 3, colour = "grey40") +
    ggplot2::scale_x_continuous(breaks = d$PC) + ggplot2::scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    ggplot2::labs(x = "Principal component", y = "Explained variance (%)", caption = "Bars: individual; line: cumulative.") + esc_theme()
}

# ---------------------------------------------------------------- PLSR (engine from esc_plsr)
esc_plot_plsr_resid <- function(eng) {
  a <- eng$lv
  d <- data.frame(Pred = eng$yhat_cv[, a], Res = eng$y_cal - eng$yhat_cv[, a], Set = "CV")
  if (all(is.na(d$Pred))) d <- data.frame(Pred = eng$yhat_cal[, a], Res = eng$y_cal - eng$yhat_cal[, a], Set = "Calibration")
  if (!is.null(eng$yhat_pred) && length(eng$y_pred))
    d <- rbind(d, data.frame(Pred = eng$yhat_pred[, a], Res = eng$y_pred - eng$yhat_pred[, a], Set = "Prediction"))
  s <- stats::sd(d$Res, na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(Pred, Res, colour = Set, shape = Set)) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = c(-2, 2) * s, linetype = 2, colour = "red") + ggplot2::geom_point(size = 2.4, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = ESC_SET_COLS) +
    ggplot2::labs(x = "Predicted Y", y = "Residual (measured - predicted)", colour = NULL, shape = NULL,
                  caption = sprintf("LV = %d. Dashed lines: \u00b12 SD of residuals.", a)) + esc_theme()
}

esc_plot_plsr_var <- function(bv, lv = NULL) {
  d <- rbind(data.frame(LV = bv$LV, Block = "X block", Cum = bv$X_cum), data.frame(LV = bv$LV, Block = "Y block", Cum = bv$Y_cum))
  p <- ggplot2::ggplot(d, ggplot2::aes(LV, Cum, colour = Block, shape = Block)) + ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`X block` = "#0072B2", `Y block` = "#D55E00")) + ggplot2::scale_x_continuous(breaks = bv$LV) +
    ggplot2::labs(x = "Latent variables", y = "Cumulative explained variance (%)", colour = NULL, shape = NULL) + esc_theme()
  if (!is.null(lv)) p <- p + ggplot2::geom_vline(xintercept = lv, linetype = 2, colour = "grey50")
  p
}

# Loading weights (W) of the first LVs against the variable axis
esc_plot_pls_weights <- function(fit, lvs = 1:2, bar = NULL) {
  W <- fit$loading.weights; lvs <- lvs[lvs <= ncol(W)]
  vars <- rownames(W) %||% paste0("V", seq_len(nrow(W))); x <- esc_var_index(vars)
  d <- do.call(rbind, lapply(lvs, function(a) data.frame(x = x, w = W[, a], LV = paste0("LV", a), var = vars)))
  spectral <- if (is.null(bar)) nrow(W) > 25 else !isTRUE(bar)   # line profile vs bars
  p <- ggplot2::ggplot(d, ggplot2::aes(x, w, colour = LV)) + ggplot2::geom_hline(yintercept = 0, colour = "grey60")
  p <- if (spectral) p + ggplot2::geom_line(linewidth = 0.8) else
    p + ggplot2::geom_col(ggplot2::aes(fill = LV), position = ggplot2::position_dodge(0.8), width = 0.75,
                          colour = if (nrow(W) <= 40) "black" else NA, linewidth = 0.2) +
      (if (nrow(W) <= 40) ggplot2::scale_x_continuous(breaks = x, labels = vars) else NULL)
  few <- !spectral && nrow(W) <= 40
  p + ggplot2::labs(x = if (few) NULL else esc_var_xlab(vars), y = "Loading weight (w)", colour = NULL, fill = NULL) + esc_theme() +
    if (few) ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# ---------------------------------------------------------------- PLS-DA
esc_plot_da_xvar <- function(r) {
  v <- r$expl_var; d <- data.frame(LV = seq_along(v), Ind = v, Cum = cumsum(v))
  ggplot2::ggplot(d, ggplot2::aes(LV)) + ggplot2::geom_col(ggplot2::aes(y = Ind), fill = "grey70", colour = "black", width = 0.7, linewidth = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = Cum), colour = "#0072B2", linewidth = 0.9) + ggplot2::geom_point(ggplot2::aes(y = Cum), colour = "#0072B2", size = 2.5) +
    ggplot2::geom_vline(xintercept = r$lv, linetype = 2, colour = "grey50") + ggplot2::scale_x_continuous(breaks = d$LV) +
    ggplot2::labs(x = "Latent variables", y = "Explained X variance (%)", caption = "Bars: individual; line: cumulative; dashed: selected LV.") + esc_theme()
}

esc_plot_da_prob <- function(r) {
  Y <- if (!is.null(r$Yhat_cv)) r$Yhat_cv else r$Yhat_cal
  d <- do.call(rbind, lapply(colnames(Y), function(l) data.frame(Model = paste("Class model:", l), Observed = r$cls_cal, yhat = Y[, l])))
  ggplot2::ggplot(d, ggplot2::aes(Observed, yhat, fill = Observed)) + ggplot2::geom_hline(yintercept = 0.5, linetype = 2, colour = "red") +
    ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6) + ggplot2::geom_jitter(width = 0.1, size = 1.4, alpha = 0.7, shape = 21) +
    ggplot2::facet_wrap(~Model) + ggplot2::labs(x = "Observed class", y = sprintf("Predicted class value (%s)", if (!is.null(r$Yhat_cv)) "CV" else "calibration")) +
    esc_theme() + ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# ---------------------------------------------------------------- DoE
esc_plot_doe_halfnormal <- function(m) {
  e <- esc_doe_effects(m); e <- e[order(e$t), ]; k <- nrow(e)
  e$q <- stats::qnorm(0.5 + 0.5 * (seq_len(k) - 0.5) / k)
  e$sig <- e$t > e$limit
  ggplot2::ggplot(e, ggplot2::aes(t, q, colour = Sign)) + ggplot2::geom_point(size = 3) +
    ggplot2::geom_abline(slope = { ns <- if (sum(!e$sig) >= 2) !e$sig else rep(TRUE, k); stats::median(e$q[ns]) / stats::median(e$t[ns]) },
                         intercept = 0, linetype = 2, colour = "grey50") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
    ggplot2::geom_text(data = e[e$sig, ], ggplot2::aes(label = Term), hjust = -0.2, size = 3.3, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(Positive = "#E69F00", Negative = "#56B4E9")) +
    ggplot2::labs(x = "|Standardised effect| (|t|)", y = "Half-normal quantile", colour = NULL,
                  caption = "Labelled terms are significant (P < 0.05); points off the line indicate active effects.") + esc_theme()
}

esc_plot_doe_runorder <- function(m) {
  r <- stats::rstandard(m$fit); d <- data.frame(Run = seq_along(r), r = r)
  ggplot2::ggplot(d, ggplot2::aes(Run, r)) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = c(-3, 3), linetype = 2, colour = "red") + ggplot2::geom_line(colour = "grey60") +
    ggplot2::geom_point(size = 2.6, colour = "#0072B2") +
    ggplot2::labs(x = "Run order", y = "Standardised residual", caption = "Trends or runs indicate time drift; points beyond \u00b13 are outliers.") + esc_theme()
}

# ---------------------------------------------------------------- shared score plot (PLSR, PLS-DA)
# Same options as the PCA score plot: LV pair, 95% ellipse, convex hull, spokes to the class
# centroid, sample ID labels and cos2 colouring (quality of representation on the two LVs).
esc_pls_explvar <- function(fit) {
  if (!is.null(fit$Xvar) && !is.null(fit$Xtotvar)) return(100 * fit$Xvar / fit$Xtotvar)
  T <- fit$scores; if (is.null(T)) return(NULL); v <- colSums(T^2); 100 * v / sum(v)   # share of fitted X variance
}

esc_plot_scores <- function(T, a = 1, b = 2, Class = NULL, ID = NULL, expl = NULL, overlays = character(0),
                            show_id = FALSE, cos2 = FALSE, Set = NULL, class_title = "Class", prefix = "LV",
                            repel = TRUE, colour_by = NULL, colour_title = NULL) {
  T <- as.matrix(T); n <- nrow(T)
  d <- data.frame(x = T[, a], y = T[, b], Class = factor(if (is.null(Class)) rep("All", n) else Class),
                  ID = if (is.null(ID) || length(ID) != n) seq_len(n) else ID,
                  Set = if (is.null(Set)) "Calibration" else Set)
  d$cos2 <- (T[, a]^2 + T[, b]^2) / pmax(rowSums(T^2), .Machine$double.eps)
  one_class <- nlevels(d$Class) == 1
  lab <- function(k) if (!is.null(expl) && length(expl) >= k && is.finite(expl[k])) sprintf("%s%d (%.1f%%)", prefix, k, expl[k]) else paste0(prefix, k)
  cal <- d[d$Set == "Calibration", , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80") + ggplot2::geom_vline(xintercept = 0, colour = "grey80")
  # overlays (calibration samples, classes with enough points)
  cont_col <- isTRUE(cos2) || (!is.null(colour_by) && one_class)   # points use a continuous colour scale
  big <- function(k) cal[cal$Class %in% names(which(table(cal$Class) >= k)), , drop = FALSE]
  if ("hull" %in% overlays) {
    h <- do.call(rbind, lapply(split(big(3), droplevels(big(3)$Class)), function(g) g[grDevices::chull(g$x, g$y), ]))
    if (!is.null(h) && nrow(h)) p <- p + ggplot2::geom_polygon(data = h, ggplot2::aes(fill = Class, group = Class), alpha = 0.15, colour = NA)
  }
  if ("ellipse" %in% overlays && nrow(big(4)))
    p <- p + if (cont_col) ggplot2::stat_ellipse(data = big(4), ggplot2::aes(group = Class), colour = "grey40", type = "norm", level = 0.95, linewidth = 0.8)
             else ggplot2::stat_ellipse(data = big(4), ggplot2::aes(colour = Class, group = Class), type = "norm", level = 0.95, linewidth = 0.8)
  if ("spokes" %in% overlays) {
    cen <- stats::aggregate(cbind(cx = x, cy = y) ~ Class, data = cal, FUN = mean)
    sp <- merge(cal, cen, by = "Class")
    p <- p + if (cont_col) ggplot2::geom_segment(data = sp, ggplot2::aes(x = cx, y = cy, xend = x, yend = y), colour = "grey50", alpha = 0.5, linewidth = 0.4)
             else ggplot2::geom_segment(data = sp, ggplot2::aes(x = cx, y = cy, xend = x, yend = y, colour = Class), alpha = 0.4, linewidth = 0.4)
  }
  two_sets <- length(unique(d$Set)) > 1
  if (isTRUE(cos2)) {
    m <- if (one_class) ggplot2::aes(colour = cos2) else ggplot2::aes(colour = cos2, shape = Class)
    p <- p + ggplot2::geom_point(m, size = 2.6) +
      ggplot2::scale_colour_gradient(low = "#9ecae1", high = "#08306b", limits = c(0, 1), name = "cos\u00b2")
  } else if (!is.null(colour_by) && one_class) {
    d$cb <- colour_by
    m <- if (two_sets) ggplot2::aes(colour = cb, shape = Set) else ggplot2::aes(colour = cb)
    p <- p + ggplot2::geom_point(data = d, m, size = 2.6) + ggplot2::scale_colour_viridis_c(name = colour_title %||% "Y")
  } else {
    m <- if (two_sets) ggplot2::aes(colour = Class, shape = Set) else ggplot2::aes(colour = Class)
    p <- p + ggplot2::geom_point(m, size = 2.6, alpha = 0.85)
  }
  if (isTRUE(show_id)) {
    p <- p + if (repel && requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(ggplot2::aes(label = ID), size = 3, max.overlaps = 50, show.legend = FALSE)
    else ggplot2::geom_text(ggplot2::aes(label = ID), size = 3, vjust = -0.7, show.legend = FALSE)
  }
  p + ggplot2::labs(x = lab(a), y = lab(b), colour = if (isTRUE(cos2)) "cos\u00b2" else class_title, fill = class_title, shape = NULL) +
    esc_theme() + (if (one_class && !isTRUE(cos2) && is.null(colour_by)) ggplot2::theme(legend.position = "none") else NULL)
}

# PCA loading profile (PC1 and PC2) against the variable axis; line or bar
esc_plot_loading_profile <- function(rot, var_names = rownames(rot), pcs = 1:2, bar = FALSE) {
  pcs <- pcs[pcs <= ncol(rot)]
  if (is.null(var_names) || length(var_names) != nrow(rot)) var_names <- rownames(rot) %||% paste0("V", seq_len(nrow(rot)))
  x <- esc_var_index(var_names)
  d <- do.call(rbind, lapply(pcs, function(k) data.frame(x = x, Loading = rot[, k], PC = paste0("PC", k), Variable = var_names)))
  d$PC <- factor(d$PC, levels = paste0("PC", pcs))
  few <- nrow(rot) <= 40
  p <- ggplot2::ggplot(d, ggplot2::aes(x, Loading)) + ggplot2::geom_hline(yintercept = 0, colour = "grey60")
  p <- if (bar) p + ggplot2::geom_col(ggplot2::aes(fill = PC), colour = if (few) "black" else NA, linewidth = 0.2, width = if (few) 0.7 else NULL) +
        ggplot2::facet_wrap(~PC, ncol = 1, scales = "free_y")
       else p + ggplot2::geom_line(ggplot2::aes(colour = PC), linewidth = 0.9) +
        (if (few) ggplot2::geom_point(ggplot2::aes(colour = PC), size = 1.8) else NULL)
  if (few) p <- p + ggplot2::scale_x_continuous(breaks = x, labels = var_names)
  p + ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    ggplot2::scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    ggplot2::labs(x = if (few) NULL else esc_var_xlab(var_names), y = "Loading", colour = NULL, fill = NULL) + esc_theme() +
    (if (few) ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) else NULL) +
    (if (bar) ggplot2::theme(legend.position = "none") else NULL)
}

# Preprocessing preview as bars: mean per variable (and class) for all samples, or one sample
esc_prep_profile_bar <- function(d, all_samples = TRUE, var_breaks = NULL) {
  if (all_samples) {
    d <- stats::aggregate(Value ~ Variable + Class + Type, data = d, FUN = mean)
    p <- ggplot2::ggplot(d, ggplot2::aes(Variable, Value, fill = Class)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(0.85), width = 0.8) + ggplot2::labs(y = "Mean intensity")
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(Variable, Value, fill = Type)) + ggplot2::geom_col(width = 0.8) + ggplot2::labs(y = "Intensity")
  }
  p + ggplot2::facet_wrap(~Type, ncol = 1, scales = "free_y") + ggplot2::labs(x = "Variables") +
    ggplot2::scale_x_discrete(breaks = var_breaks %||% ggplot2::waiver()) + ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
