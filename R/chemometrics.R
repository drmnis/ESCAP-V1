# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Chemometrics engine: preprocessing (fit/apply), PLSR, PLS-DA, cross-validation,
# VIP, selectivity ratio, Q residuals, Hotelling T2. Pure functions, no Shiny code.

# ---------------------------------------------------------------------------
# 1. Preprocessing. Fitted on calibration data, then applied unchanged to any
#    new data, so CV folds and prediction sets never leak information.
#    Order: spectral (SNV / MSC / SG / detrend)  ->  scaling  ->  centering.
# ---------------------------------------------------------------------------
esc_prep_fit <- function(X, spectral = "none", scaling = "none", centering = "none",
                         sg_width = 11, sg_order = 2, sg_deriv = 0) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  par <- list(spectral = spectral %||% "none", scaling = scaling %||% "none",
              centering = centering %||% "none", in_names = colnames(X))
  if (identical(par$scaling, "auto")) par$centering <- "mc"   # autoscaling = centring + unit variance
  if ("msc" %in% par$spectral) par$msc_ref <- colMeans(X, na.rm = TRUE)
  if ("sg" %in% par$spectral) {
    w <- as.integer(sg_width); if (w %% 2 == 0) w <- w + 1L
    p <- as.integer(sg_order); m <- as.integer(sg_deriv)
    if (p >= w) stop("Savitzky-Golay: polynomial order must be smaller than the window width.")
    if (m > p) stop("Savitzky-Golay: derivative order cannot exceed the polynomial order.")
    if (ncol(X) <= w) stop("Savitzky-Golay: window width must be smaller than the number of variables.")
    par$sg <- list(w = w, p = p, m = m)
  }
  Xs <- esc_prep_spectral(X, par)
  if (par$scaling == "log") {
    if (any(Xs <= -1, na.rm = TRUE))
      stop("Log scaling: values <= -1 found (often after SNV, MSC or derivatives). Choose another scaling.")
    Xs <- log1p(Xs)
  }
  sdv <- function(M) apply(M, 2, stats::sd, na.rm = TRUE)
  rgv <- function(M) apply(M, 2, function(x) diff(range(x, na.rm = TRUE)))
  div <- switch(par$scaling, auto = sdv(Xs), pareto = sqrt(sdv(Xs)),
                iqr = apply(Xs, 2, stats::IQR, na.rm = TRUE), range = rgv(Xs), maxmin = rgv(Xs) / 2, NULL)
  if (!is.null(div)) div[!is.finite(div) | div == 0] <- 1
  par[["dvec"]] <- div
  par[["ovec"]] <- if (par$scaling == "maxmin") apply(Xs, 2, min, na.rm = TRUE) + div else NULL  # maps to [-1, 1]
  Xsc <- esc_prep_scale(Xs, par)
  par[["cvec"]] <- switch(par$centering, mc = colMeans(Xsc, na.rm = TRUE),
                       median = apply(Xsc, 2, stats::median, na.rm = TRUE), NULL)
  par$out_names <- colnames(Xs)
  structure(par, class = "esc_prep")
}

esc_prep_scale <- function(Xs, par) {
  if (!is.null(par[["ovec"]]))  Xs <- sweep(Xs, 2, par[["ovec"]], "-")
  if (!is.null(par[["dvec"]])) Xs <- sweep(Xs, 2, par[["dvec"]], "/")
  Xs
}

esc_prep_spectral <- function(X, par) {
  X <- as.matrix(X)
  if ("snv" %in% par$spectral) {
    mu <- rowMeans(X, na.rm = TRUE)
    s  <- apply(X, 1, stats::sd, na.rm = TRUE); s[!is.finite(s) | s == 0] <- 1
    X  <- (X - mu) / s
  }
  if ("msc" %in% par$spectral) {
    ref <- par$msc_ref; rc <- ref - mean(ref); sxx <- sum(rc^2)
    b <- as.numeric((X - rowMeans(X)) %*% rc) / sxx          # slope per sample
    a <- rowMeans(X) - b * mean(ref)                          # intercept per sample
    b[!is.finite(b) | b == 0] <- 1
    X <- (X - a) / b
  }
  if ("detrend" %in% par$spectral) {
    tt <- seq_len(ncol(X)); tt <- tt - mean(tt)
    X <- X - rowMeans(X) - outer(as.numeric(X %*% tt) / sum(tt^2), tt)
  }
  if ("sg" %in% par$spectral) {
    nm <- colnames(X)
    X <- prospectr::savitzkyGolay(X, m = par$sg$m, p = par$sg$p, w = par$sg$w)
    if (is.null(colnames(X)) && !is.null(nm)) {
      h <- (par$sg$w - 1) / 2
      colnames(X) <- nm[(h + 1):(length(nm) - h)]
    }
    X <- as.matrix(X)
  }
  X
}

esc_prep_apply <- function(par, X) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  Xs <- esc_prep_spectral(X, par)
  if (par$scaling == "log") Xs <- log1p(Xs)
  Xs <- esc_prep_scale(Xs, par)
  if (!is.null(par[["cvec"]])) Xs <- sweep(Xs, 2, par[["cvec"]], "-")
  colnames(Xs) <- par$out_names
  Xs
}

# Convenience: fit on X and return the transformed X (used for PCA and previews)
esc_preprocess <- function(X, ...) {
  par <- esc_prep_fit(X, ...)
  out <- esc_prep_apply(par, X)
  attr(out, "prep") <- par
  out
}

esc_prep_label <- function(spectral, scaling, centering) {
  s <- c(none = NA, snv = "SNV", msc = "MSC", sg = "Savitzky-Golay", detrend = "Detrend")[spectral %||% "none"]
  k <- c(none = NA, auto = "Autoscaling", pareto = "Pareto", iqr = "IQR", range = "Range",
         maxmin = "Max-min [-1, 1]", log = "Log(1+x)")[scaling %||% "none"]
  c0 <- c(none = NA, mc = "Mean centring", median = "Median centring")[centering %||% "none"]
  if (identical(scaling, "auto")) c0 <- NA
  st <- stats::na.omit(c(s, k, c0))
  if (!length(st)) "None" else paste(st, collapse = " -> ")
}

# ---------------------------------------------------------------------------
# 2. Cross-validation segments
# ---------------------------------------------------------------------------
esc_cv_segments <- function(n, type = c("vb", "rand", "cont", "loo"), k = 5, thickness = 1, seed = 123) {
  type <- match.arg(type)
  if (type == "loo") return(as.list(seq_len(n)))
  k <- max(2, min(k, n))
  if (type == "rand") {
    set.seed(seed)
    id <- sample(rep(seq_len(k), length.out = n))
  } else if (type == "cont") {
    id <- rep(seq_len(k), each = ceiling(n / k), length.out = n)
  } else {                                        # venetian blinds with thickness
    thickness <- max(1, thickness)
    blk <- ceiling(seq_len(n) / thickness)
    id <- ((blk - 1) %% k) + 1
  }
  split(seq_len(n), id)
}

# ---------------------------------------------------------------------------
# 3. PLS core (kernel algorithm from 'pls'); X already preprocessed, pls centres
# ---------------------------------------------------------------------------
esc_pls_fit <- function(X, Y, ncomp) {
  Y <- as.matrix(Y)
  ncomp <- max(1, min(ncomp, nrow(X) - 1, ncol(X)))
  pls::kernelpls.fit(X, Y, ncomp = ncomp, center = TRUE, stripped = FALSE)
}
esc_pls_predict <- function(fit, Xnew, a) {
  d <- dim(fit$coefficients)
  B <- matrix(fit$coefficients[, , a], nrow = d[1], ncol = d[2])
  Yint <- fit$Ymeans - as.numeric(fit$Xmeans %*% B)
  sweep(as.matrix(Xnew) %*% B, 2, Yint, "+")
}

# VIP (Wold et al., 1993; Chong & Jun, 2005). Works for one or several responses.
esc_vip <- function(fit, a) {
  W  <- fit$loading.weights[, 1:a, drop = FALSE]
  W  <- sweep(W, 2, sqrt(colSums(W^2)), "/")
  Tt <- fit$scores[, 1:a, drop = FALSE]
  Q  <- matrix(fit$Yloadings, ncol = ncol(fit$scores))[, 1:a, drop = FALSE]
  SS <- colSums(Q^2) * colSums(Tt^2)
  p  <- nrow(W)
  v  <- sqrt(p * as.numeric(W^2 %*% SS) / sum(SS))
  stats::setNames(v, rownames(fit$loading.weights))
}

# Selectivity ratio by target projection (Kvalheim & Karstang, 1989; Rajalahti et al., 2009)
esc_selectivity_ratio <- function(Xc, b) {
  Xc <- as.matrix(Xc); b <- as.numeric(b)
  w  <- b / sqrt(sum(b^2))
  t  <- as.numeric(Xc %*% w)
  p  <- as.numeric(crossprod(Xc, t)) / sum(t^2)
  Xtp <- outer(t, p)
  E <- Xc - Xtp
  sr <- colSums(Xtp^2) / colSums(E^2)
  sr[!is.finite(sr)] <- NA
  stats::setNames(sr, colnames(Xc))
}

# Q residuals and Hotelling T2 with 95 % (or other) limits
esc_q_t2 <- function(fit, Xc_cal, a, Xc_new = NULL, alpha = 0.05) {
  Tcal <- fit$scores[, 1:a, drop = FALSE]
  P    <- fit$loadings[, 1:a, drop = FALSE]
  R    <- fit$projection[, 1:a, drop = FALSE]
  s2   <- apply(Tcal, 2, stats::var)
  Qcal <- rowSums((Xc_cal - Tcal %*% t(P))^2)
  T2cal <- rowSums(sweep(Tcal^2, 2, s2, "/"))
  n <- nrow(Tcal)
  t2lim <- a * (n - 1) / (n - a) * stats::qf(1 - alpha, a, n - a)
  m <- mean(Qcal); v <- stats::var(Qcal)
  qlim <- if (v > 0) (v / (2 * m)) * stats::qchisq(1 - alpha, 2 * m^2 / v) else NA
  out <- list(Q = Qcal, T2 = T2cal, Q_lim = qlim, T2_lim = t2lim)
  if (!is.null(Xc_new) && nrow(Xc_new) > 0) {
    Tn <- Xc_new %*% R
    out$Q_new  <- rowSums((Xc_new - Tn %*% t(P))^2)
    out$T2_new <- rowSums(sweep(Tn^2, 2, s2, "/"))
    out$T_new <- Tn
  }
  out
}

esc_reg_stats <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat); y <- y[ok]; yhat <- yhat[ok]
  n <- length(y); if (n < 2) return(c(RMSE = NA, R2 = NA, Bias = NA, SE = NA, RPD = NA, n = n))
  e <- y - yhat
  rmse <- sqrt(mean(e^2))
  c(RMSE = rmse, R2 = 1 - sum(e^2) / sum((y - mean(y))^2), Bias = mean(-e),
    SE = sqrt(sum((e - mean(e))^2) / (n - 1)), RPD = stats::sd(y) / rmse, n = n)
}

# ---------------------------------------------------------------------------
# 4. PLSR with fold-wise preprocessing
# ---------------------------------------------------------------------------
esc_plsr <- function(X_cal, y_cal, X_pred = NULL, y_pred = NULL, prep_args = list(),
                     max_lv = 10, cv = list(type = "vb", k = 5, thickness = 1), lv = NULL) {
  X_cal <- as.matrix(X_cal); y_cal <- as.numeric(y_cal)
  par <- do.call(esc_prep_fit, c(list(X_cal), prep_args))
  Xp <- esc_prep_apply(par, X_cal)
  max_lv <- max(1, min(max_lv, nrow(Xp) - 2, ncol(Xp)))
  fit <- esc_pls_fit(Xp, y_cal, max_lv)
  max_lv <- fit$ncomp %||% dim(fit$coefficients)[3]
  yhat_cal <- matrix(sapply(seq_len(max_lv), function(a) esc_pls_predict(fit, Xp, a)[, 1]), nrow = nrow(Xp))
  # cross-validation, preprocessing re-fitted inside each fold
  yhat_cv <- matrix(NA_real_, nrow(Xp), max_lv)
  if (!identical(cv$type, "none")) {
    segs <- esc_cv_segments(nrow(X_cal), cv$type, cv$k %||% 5, cv$thickness %||% 1, cv$seed %||% 123)
    for (s in segs) {
      tr <- setdiff(seq_len(nrow(X_cal)), s)
      pf <- do.call(esc_prep_fit, c(list(X_cal[tr, , drop = FALSE]), prep_args))
      Xtr <- esc_prep_apply(pf, X_cal[tr, , drop = FALSE])
      Xte <- esc_prep_apply(pf, X_cal[s, , drop = FALSE])
      ff <- esc_pls_fit(Xtr, y_cal[tr], max_lv)
      amax <- dim(ff$coefficients)[3]
      for (a in seq_len(min(amax, max_lv))) yhat_cv[s, a] <- esc_pls_predict(ff, Xte, a)[, 1]
    }
  }
  has_pred <- !is.null(X_pred) && nrow(as.matrix(X_pred)) > 0
  Xpp <- if (has_pred) esc_prep_apply(par, X_pred) else NULL
  yhat_pred <- if (has_pred) sapply(seq_len(max_lv), function(a) esc_pls_predict(fit, Xpp, a)[, 1]) else NULL
  if (has_pred && is.null(dim(yhat_pred))) yhat_pred <- matrix(yhat_pred, nrow = nrow(Xpp))
  perf <- do.call(rbind, lapply(seq_len(max_lv), function(a) {
    c_ <- esc_reg_stats(y_cal, yhat_cal[, a]); v_ <- esc_reg_stats(y_cal, yhat_cv[, a])
    p_ <- if (has_pred && length(y_pred)) esc_reg_stats(y_pred, yhat_pred[, a]) else rep(NA, 6)
    data.frame(LV = a, RMSEC = c_[1], RMSECV = v_[1], RMSEP = p_[1],
               R2C = c_[2], R2CV = v_[2], R2P = p_[2],
               BiasC = c_[3], BiasCV = v_[3], BiasP = p_[3],
               SEC = c_[4], SECV = v_[4], SEP = p_[4], RPD_CV = v_[5], RPD_P = p_[5])
  }))
  # LV choice: user value, else the smallest LV whose RMSECV is within 1 SE-like 2 % of the minimum
  if (is.null(lv) || is.na(lv) || lv < 1) {
    r <- perf$RMSECV
    lv <- if (all(is.na(r))) min(2, max_lv) else min(which(r <= min(r, na.rm = TRUE) * 1.02))
  }
  lv <- max(1, min(lv, max_lv))
  b  <- fit$coefficients[, 1, lv]
  Xc <- sweep(Xp, 2, fit$Xmeans, "-")
  qt <- esc_q_t2(fit, Xc, lv, if (has_pred) sweep(Xpp, 2, fit$Xmeans, "-") else NULL)
  ev <- 100 * fit$Xvar / fit$Xtotvar
  r2y <- sapply(seq_len(max_lv), function(a) 1 - sum((y_cal - yhat_cal[, a])^2) / sum((y_cal - mean(y_cal))^2))
  list(fit = fit, prep = par, X_cal_prep = Xp, X_pred_prep = Xpp, y_cal = y_cal, y_pred = y_pred,
       yhat_cal = yhat_cal, yhat_cv = yhat_cv, yhat_pred = yhat_pred, perf = perf, lv = lv,
       max_lv = max_lv, coef = b, intercept = fit$Ymeans - sum(fit$Xmeans * b),
       vip = esc_vip(fit, lv), sr = esc_selectivity_ratio(Xc, b), qt = qt,
       block_var = data.frame(LV = seq_len(max_lv), X_pct = ev, X_cum = cumsum(ev),
                              Y_pct = c(r2y[1], diff(r2y)) * 100, Y_cum = r2y * 100))
}

# ---------------------------------------------------------------------------
# 5. PLS-DA (dummy-coded Y, class = arg max of predicted Y)
# ---------------------------------------------------------------------------
esc_class_metrics <- function(truth, pred, levels) {
  truth <- factor(truth, levels = levels); pred <- factor(pred, levels = levels)
  cm <- table(Observed = truth, Predicted = pred)
  n <- sum(cm); acc <- sum(diag(cm)) / n
  pe <- sum(rowSums(cm) * colSums(cm)) / n^2
  per <- do.call(rbind, lapply(levels, function(l) {
    tp <- cm[l, l]; fn <- sum(cm[l, ]) - tp; fp <- sum(cm[, l]) - tp; tn <- n - tp - fn - fp
    se <- tp / (tp + fn); sp <- tn / (tn + fp); pr <- tp / (tp + fp)
    data.frame(Class = l, n = tp + fn, Sensitivity = se, Specificity = sp, Precision = pr,
               F1 = 2 * pr * se / (pr + se), Balanced_accuracy = (se + sp) / 2)
  }))
  list(confusion = cm, accuracy = acc, error_rate = 1 - acc,
       kappa = if (pe < 1) (acc - pe) / (1 - pe) else NA, per_class = per)
}

esc_auc <- function(score, positive) {   # Mann-Whitney AUC, one-vs-rest
  ok <- is.finite(score); score <- score[ok]; positive <- positive[ok]
  n1 <- sum(positive); n0 <- sum(!positive)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score); (sum(r[positive]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
esc_roc_curve <- function(score, positive) {
  o <- order(score, decreasing = TRUE); p <- positive[o]
  data.frame(FPR = c(0, cumsum(!p) / sum(!p)), TPR = c(0, cumsum(p) / sum(p)))
}

esc_plsda <- function(X_cal, cls_cal, X_pred = NULL, cls_pred = NULL, prep_args = list(),
                      max_lv = 10, cv = list(type = "vb", k = 5, thickness = 1), lv = NULL,
                      n_perm = 0) {
  X_cal <- as.matrix(X_cal); cls_cal <- droplevels(factor(cls_cal))
  lev <- levels(cls_cal)
  if (length(lev) < 2) stop("PLS-DA needs at least two classes.")
  Yd <- stats::model.matrix(~ cls_cal - 1); colnames(Yd) <- lev
  par <- do.call(esc_prep_fit, c(list(X_cal), prep_args))
  Xp <- esc_prep_apply(par, X_cal)
  max_lv <- max(1, min(max_lv, nrow(Xp) - 2, ncol(Xp)))
  fit <- esc_pls_fit(Xp, Yd, max_lv); max_lv <- dim(fit$coefficients)[3]
  pred_cls <- function(Yh) factor(lev[max.col(Yh, ties.method = "first")], levels = lev)
  Yhat_cal <- lapply(seq_len(max_lv), function(a) esc_pls_predict(fit, Xp, a))
  Yhat_cv <- lapply(seq_len(max_lv), function(a) matrix(NA_real_, nrow(Xp), length(lev), dimnames = list(NULL, lev)))
  run_cv <- function(y_fac) {
    Yd_ <- stats::model.matrix(~ y_fac - 1)
    out <- lapply(seq_len(max_lv), function(a) matrix(NA_real_, nrow(Xp), length(lev)))
    segs <- esc_cv_segments(nrow(X_cal), cv$type, cv$k %||% 5, cv$thickness %||% 1, cv$seed %||% 123)
    for (s in segs) {
      tr <- setdiff(seq_len(nrow(X_cal)), s)
      pf <- do.call(esc_prep_fit, c(list(X_cal[tr, , drop = FALSE]), prep_args))
      ff <- esc_pls_fit(esc_prep_apply(pf, X_cal[tr, , drop = FALSE]), Yd_[tr, , drop = FALSE], max_lv)
      Xte <- esc_prep_apply(pf, X_cal[s, , drop = FALSE])
      for (a in seq_len(min(max_lv, dim(ff$coefficients)[3]))) out[[a]][s, ] <- esc_pls_predict(ff, Xte, a)
    }
    out
  }
  if (!identical(cv$type, "none")) Yhat_cv <- lapply(run_cv(cls_cal), function(m) { colnames(m) <- lev; m })
  err <- data.frame(LV = seq_len(max_lv),
    Error_cal = sapply(Yhat_cal, function(m) mean(pred_cls(m) != cls_cal)),
    Error_CV  = sapply(Yhat_cv, function(m) if (all(is.na(m))) NA else mean(pred_cls(m) != cls_cal)))
  if (is.null(lv) || is.na(lv) || lv < 1) {
    e <- err$Error_CV; lv <- if (all(is.na(e))) min(2, max_lv) else which.min(e)
  }
  lv <- max(1, min(lv, max_lv))
  has_pred <- !is.null(X_pred) && nrow(as.matrix(X_pred)) > 0
  Xpp <- if (has_pred) esc_prep_apply(par, X_pred) else NULL
  Yhat_pred <- if (has_pred) esc_pls_predict(fit, Xpp, lv) else NULL
  m_cal <- esc_class_metrics(cls_cal, pred_cls(Yhat_cal[[lv]]), lev)
  m_cv  <- if (!all(is.na(Yhat_cv[[lv]]))) esc_class_metrics(cls_cal, pred_cls(Yhat_cv[[lv]]), lev) else NULL
  m_pred <- if (has_pred && !is.null(cls_pred)) esc_class_metrics(cls_pred, pred_cls(Yhat_pred), lev) else NULL
  roc_src <- if (!is.null(m_cv)) Yhat_cv[[lv]] else Yhat_cal[[lv]]
  auc <- sapply(lev, function(l) esc_auc(roc_src[, l], cls_cal == l))
  roc <- do.call(rbind, lapply(lev, function(l) cbind(Class = l, esc_roc_curve(roc_src[, l], cls_cal == l))))
  # permutation test on CV error (Westerhuis et al., 2008)
  perm <- NULL
  if (n_perm > 0 && !identical(cv$type, "none")) {
    set.seed(321)
    perm_err <- vapply(seq_len(n_perm), function(i) {
      yp <- sample(cls_cal); m <- run_cv(yp)[[lv]]; mean(pred_cls(m) != yp)
    }, numeric(1))
    perm <- list(errors = perm_err, observed = err$Error_CV[lv],
                 p_value = (sum(perm_err <= err$Error_CV[lv]) + 1) / (n_perm + 1))
  }
  Xc <- sweep(Xp, 2, fit$Xmeans, "-")
  B <- fit$coefficients[, , lv]; colnames(B) <- lev
  sr <- sapply(lev, function(l) esc_selectivity_ratio(Xc, B[, l]))
  rownames(sr) <- colnames(Xp)
  list(fit = fit, prep = par, classes = lev, cls_cal = cls_cal, cls_pred = cls_pred, lv = lv,
       max_lv = max_lv, error = err, Yhat_cal = Yhat_cal[[lv]], Yhat_cv = Yhat_cv[[lv]],
       Yhat_pred = Yhat_pred, pred_cal = pred_cls(Yhat_cal[[lv]]),
       pred_cv = if (!is.null(m_cv)) pred_cls(Yhat_cv[[lv]]) else NULL,
       pred_pred = if (has_pred) pred_cls(Yhat_pred) else NULL,
       metrics_cal = m_cal, metrics_cv = m_cv, metrics_pred = m_pred, auc = auc, roc = roc,
       coef = B, vip = esc_vip(fit, lv), sr = sr, qt = esc_q_t2(fit, Xc, lv,
         if (has_pred) sweep(Xpp, 2, fit$Xmeans, "-") else NULL),
       scores = fit$scores[, 1:max(2, min(lv, max_lv)), drop = FALSE],
       scores_pred = if (has_pred) sweep(Xpp, 2, fit$Xmeans, "-") %*% fit$projection[, 1:max(2, min(lv, max_lv)), drop = FALSE] else NULL,
       expl_var = 100 * fit$Xvar / fit$Xtotvar, permutation = perm)
}

# Numeric axis for spectral variables: "NIR.908.nm", "X1206 nm", "w_450" -> 908, 1206, 450
esc_var_index <- function(vars) {
  num <- suppressWarnings(as.numeric(gsub(",", ".", sub("^[^0-9.-]*([-]?[0-9]+[.]?[0-9]*).*$", "\\1", vars))))
  if (length(vars) > 2 && all(is.finite(num)) && !anyDuplicated(num) && all(diff(num) > 0)) num else seq_along(vars)
}
esc_var_xlab <- function(vars) {
  if (identical(esc_var_index(vars), seq_along(vars))) return("Variable index")
  if (any(grepl("nm", vars, ignore.case = TRUE))) "Wavelength (nm)" else if (any(grepl("cm", vars))) "Wavenumber (cm-1)" else "Variable"
}

# Align raw and preprocessed matrices by column name (Savitzky-Golay drops edge variables)
esc_align_cols <- function(X, Xp, names_full = colnames(X)) {
  if (!is.null(colnames(Xp)) && !is.null(colnames(X)) && all(colnames(Xp) %in% colnames(X))) {
    ix <- match(colnames(Xp), colnames(X)); return(list(ix = ix, ip = seq_along(ix)))
  }
  k <- min(ncol(X), ncol(Xp), length(names_full)); list(ix = seq_len(k), ip = seq_len(k))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
