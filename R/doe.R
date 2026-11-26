# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Design of Experiments: design generation (full and fractional factorial, Plackett-Burman,
# central composite, Box-Behnken, Taguchi L9, mixture, D-optimal) and analysis of
# response-surface models (coded coefficients, ANOVA with lack of fit, fit statistics,
# canonical analysis, desirability optimisation). Pure functions.

# ------------------------------------------------------------------ helpers
esc_ff2 <- function(k) as.matrix(expand.grid(rep(list(c(-1, 1)), k)))[, k:1, drop = FALSE]  # standard (Yates) order
esc_decode <- function(coded, low, high) sweep(sweep(coded, 2, (high - low) / 2, "*"), 2, (high + low) / 2, "+")
esc_code <- function(actual, low, high) sweep(sweep(actual, 2, (high + low) / 2, "-"), 2, (high - low) / 2, "/")

ESC_FRAC_GEN <- list(   # minimum-aberration generators (Box, Hunter & Hunter)
  `4-1` = list(D = "ABC"), `5-1` = list(E = "ABCD"), `5-2` = list(D = "AB", E = "AC"),
  `6-1` = list(F = "ABCDE"), `6-2` = list(E = "ABC", F = "BCD"), `6-3` = list(D = "AB", E = "AC", F = "BC"),
  `7-1` = list(G = "ABCDEF"), `7-2` = list(F = "ABCD", G = "ABDE"), `7-3` = list(E = "ABC", F = "BCD", G = "ACD"),
  `7-4` = list(D = "AB", E = "AC", F = "BC", G = "ABC"), `8-4` = list(E = "BCD", F = "ACD", G = "ABC", H = "ABD"))
ESC_PB_ROWS <- list(`8` = "+++-+--", `12` = "++-+++---+-", `16` = "++++-+-++--+---",
                    `20` = "++--++++-+-+----++-", `24` = "+++++-+-++--++--+-+----")

esc_design <- function(type, k, factor_names = LETTERS[seq_len(k)], low = rep(-1, k), high = rep(1, k),
                       levels = NULL, center = 0, reps = 1, p = 1, alpha = "rotatable", pb_runs = 12,
                       mix_m = 2, dopt_runs = NULL, dopt_model = "quadratic", randomize = TRUE, seed = 123) {
  info <- list()
  X <- switch(type,
    full2 = esc_ff2(k),
    general = { lv <- rep_len(levels %||% 3, k); as.matrix(expand.grid(lapply(lv, function(n) seq(-1, 1, length.out = n)))) },
    frac = {
      key <- paste0(k, "-", p); gen <- ESC_FRAC_GEN[[key]]
      if (is.null(gen)) stop("Fractional design 2^(", k, "-", p, ") is not available. Use k = 4-8 with a listed fraction.")
      base <- esc_ff2(k - p); colnames(base) <- LETTERS[seq_len(k - p)]
      for (g in names(gen)) base <- cbind(base, apply(base[, strsplit(gen[[g]], "")[[1]], drop = FALSE], 1, prod))
      colnames(base) <- LETTERS[seq_len(k)]
      words <- paste0(names(gen), "=", unlist(gen))
      def <- vapply(names(gen), function(g) paste0("I=", paste(sort(c(strsplit(gen[[g]], "")[[1]], g)), collapse = "")), "")
      info$generators <- paste(words, collapse = ", "); info$defining <- paste(def, collapse = ", ")
      info$resolution <- as.roman(min(nchar(sub("I=", "", def))))
      base },
    pb = {
      r <- ESC_PB_ROWS[[as.character(pb_runs)]]; if (is.null(r)) stop("Plackett-Burman runs must be 8, 12, 16, 20 or 24.")
      v <- ifelse(strsplit(r, "")[[1]] == "+", 1, -1); n <- length(v)
      if (k > n) stop("A ", pb_runs, "-run Plackett-Burman design screens at most ", n, " factors.")
      M <- t(sapply(0:(n - 1), function(i) v[((seq_len(n) - 1 - i) %% n) + 1])); M <- rbind(M, rep(-1, n))
      M[, seq_len(k), drop = FALSE] },
    ccd = {
      a <- switch(alpha, rotatable = (2^k)^(1 / 4), face = 1, orthogonal = sqrt(k), spherical = sqrt(k), as.numeric(alpha))
      F <- esc_ff2(k); S <- do.call(rbind, lapply(seq_len(k), function(j) { m <- matrix(0, 2, k); m[, j] <- c(-a, a); m }))
      info$alpha <- round(a, 4); rbind(F, S) },
    bbd = {
      if (!k %in% 3:5) stop("Box-Behnken designs are provided for 3, 4 or 5 factors.")
      pr <- utils::combn(k, 2)
      do.call(rbind, lapply(seq_len(ncol(pr)), function(j) { m <- matrix(0, 4, k); m[, pr[, j]] <- as.matrix(expand.grid(c(-1, 1), c(-1, 1))); m })) },
    taguchi = {
      if (k > 4) stop("Taguchi L9 handles up to 4 three-level factors.")
      L9 <- matrix(c(1,1,1,1, 1,2,2,2, 1,3,3,3, 2,1,2,3, 2,2,3,1, 2,3,1,2, 3,1,3,2, 3,2,1,3, 3,3,2,1), ncol = 4, byrow = TRUE)
      (L9[, seq_len(k), drop = FALSE] - 2) },
    mix_lattice = {
      g <- expand.grid(rep(list(0:mix_m), k)); g <- g[rowSums(g) == mix_m, , drop = FALSE] / mix_m
      as.matrix(g[order(-apply(g, 1, max)), , drop = FALSE]) },
    mix_centroid = {
      do.call(rbind, lapply(seq_len(k), function(s) { cb <- utils::combn(k, s)
        t(apply(cb, 2, function(ix) { v <- rep(0, k); v[ix] <- 1 / s; v })) })) },
    dopt = {
      cand <- as.matrix(expand.grid(rep(list(c(-1, 0, 1)), k)))
      colnames(cand) <- factor_names
      fml <- switch(dopt_model, linear = ~ ., interaction = ~ .^2,
                    quadratic = stats::as.formula(paste("~ (", paste(factor_names, collapse = "+"), ")^2 +",
                                                        paste0("I(", factor_names, "^2)", collapse = "+"))))
      nrun <- dopt_runs %||% (ncol(stats::model.matrix(fml, as.data.frame(cand))) + 4)
      set.seed(seed)
      od <- AlgDesign::optFederov(fml, data = as.data.frame(cand), nTrials = nrun, criterion = "D")
      info$D <- round(od$D, 4); info$runs <- nrun
      as.matrix(od$design) },
    stop("Unknown design type."))
  X <- as.matrix(X); colnames(X) <- factor_names[seq_len(ncol(X))]
  is_mix <- type %in% c("mix_lattice", "mix_centroid")
  if (center > 0 && !is_mix) X <- rbind(X, matrix(0, center, ncol(X), dimnames = list(NULL, colnames(X))))
  if (center > 0 && is_mix) X <- rbind(X, matrix(1 / k, center, k, dimnames = list(NULL, colnames(X))))
  if (reps > 1) X <- X[rep(seq_len(nrow(X)), reps), , drop = FALSE]
  std <- seq_len(nrow(X))
  run <- if (randomize) { set.seed(seed); sample(std) } else std
  actual <- if (is_mix) X else esc_decode(X, low, high)
  colnames(actual) <- colnames(X)
  coded <- X; colnames(coded) <- paste0(colnames(X), " (coded)")
  out <- data.frame(Run = order(run), Std = std, actual, coded, check.names = FALSE)
  out <- out[order(out$Run), , drop = FALSE]; rownames(out) <- NULL
  info$runs <- nrow(out); info$type <- type
  list(design = out, info = info, mixture = is_mix)
}

# ------------------------------------------------------------------ model terms
esc_doe_formula <- function(y, factors, model = c("linear", "2fi", "quadratic", "cubic_mix", "mix_linear", "mix_quadratic", "mix_special")) {
  model <- match.arg(model); f <- factors
  rhs <- switch(model,
    linear = paste(f, collapse = " + "),
    `2fi` = paste0("(", paste(f, collapse = " + "), ")^2"),
    quadratic = paste0("(", paste(f, collapse = " + "), ")^2 + ", paste0("I(", f, "^2)", collapse = " + ")),
    mix_linear = paste("-1 +", paste(f, collapse = " + ")),
    mix_quadratic = paste0("-1 + (", paste(f, collapse = " + "), ")^2"),
    mix_special = paste0("-1 + (", paste(f, collapse = " + "), ")^3"),
    cubic_mix = paste0("-1 + (", paste(f, collapse = " + "), ")^3"))
  stats::as.formula(paste(y, "~", rhs))
}

esc_term_label <- function(x) { x <- gsub("`", "", x); x <- gsub("I\\((.*)\\^2\\)", "\\1\u00b2", x); gsub(":", " x ", x) }

# ------------------------------------------------------------------ analysis of one response
esc_doe_fit <- function(df, y, factors, model = "quadratic", coding = c("auto", "none"), reduce = FALSE,
                        alpha = 0.05, hierarchy = TRUE) {
  coding <- match.arg(coding)
  d <- df[stats::complete.cases(df[, c(factors, y)]), c(factors, y)]
  labs <- stats::setNames(c(factors, y), make.names(c(factors, y), unique = TRUE))
  names(d) <- names(labs); factors <- names(labs)[seq_along(factors)]; y <- names(labs)[length(labs)]
  esc_term_label <- function(x) { for (nm in names(labs)) x <- gsub(paste0("(?<![A-Za-z0-9._])", gsub(".", "\\.", nm, fixed = TRUE), "(?![A-Za-z0-9._])"), labs[[nm]], x, perl = TRUE)
    x <- gsub("I\\((.*)\\^2\\)", "\\1\u00b2", x); gsub(":", " x ", x) }
  mix <- grepl("^mix", model)
  # coding range: factorial levels = -1/+1. For central composite designs (5 distinct levels:
  # -alpha, -1, 0, +1, +alpha) the 2nd and 4th levels are the factorial levels.
  rng_of <- function(v) { u <- sort(unique(round(v, 10)))
    if (length(u) == 5 && isTRUE(all.equal(u[2] + u[4], 2 * u[3])) && isTRUE(all.equal(u[1] + u[5], 2 * u[3]))) c(u[2], u[4]) else range(v) }
  lo <- vapply(factors, function(f) rng_of(d[[f]])[1], 0); hi <- vapply(factors, function(f) rng_of(d[[f]])[2], 0)
  names(lo) <- names(hi) <- factors
  lo_all <- vapply(factors, function(f) min(d[[f]]), 0); hi_all <- vapply(factors, function(f) max(d[[f]]), 0)
  dc <- d
  if (!mix && coding == "auto") for (f in factors) dc[[f]] <- if (hi[f] > lo[f]) (d[[f]] - (hi[f] + lo[f]) / 2) / ((hi[f] - lo[f]) / 2) else 0
  if (mix) { s <- rowSums(d[, factors]); for (f in factors) dc[[f]] <- d[[f]] / s }
  fml <- esc_doe_formula(y, factors, model)
  fit <- stats::lm(fml, data = dc)
  if (any(is.na(stats::coef(fit)))) stop("The model has more terms than the design can estimate (aliased terms). Choose a simpler model.")
  removed <- character(0)
  if (reduce && !mix) repeat {
    co <- summary(fit)$coefficients; co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
    if (!nrow(co)) break
    terms <- rownames(co); cand <- terms
    if (hierarchy) {           # keep main effects that appear in a retained higher-order term
      keep <- vapply(terms, function(t) {
        if (grepl(":|\\^2", t)) return(FALSE)
        f <- gsub("`", "", t); any(grepl(f, setdiff(terms, t), fixed = TRUE))
      }, logical(1))
      cand <- terms[!keep]
    }
    pv <- co[cand, 4]; if (!length(pv) || max(pv) <= alpha) break
    drop <- names(which.max(pv)); removed <- c(removed, drop)
    fit <- stats::update(fit, stats::as.formula(paste(". ~ . -", drop)))
  }
  # ANOVA with grouped terms, lack of fit and pure error
  sm <- summary(fit); n <- nrow(dc); p <- length(stats::coef(fit)); res <- stats::residuals(fit)
  if (mix) { sm$r.squared <- 1 - sum(res^2) / sum((dc[[y]] - mean(dc[[y]]))^2)
    sm$adj.r.squared <- 1 - (1 - sm$r.squared) * (n - 1) / (n - p) }
  tl <- attr(stats::terms(fit), "term.labels")
  ss_res <- sum(res^2); df_res <- fit$df.residual
  sst <- sum((dc[[y]] - mean(dc[[y]]))^2)
  ss_model <- sst - ss_res; df_model <- length(tl)
  if (mix) df_model <- p - 1
  cls <- function(t) if (grepl(":", t)) (if (lengths(regmatches(t, gregexpr(":", t))) > 1) "Cubic blend" else if (mix) "Quadratic blend" else "Interaction (2FI)") else if (grepl("\\^2", t)) "Quadratic" else if (mix) "Linear blend" else "Linear"
  grp <- vapply(tl, cls, "")
  partial <- function(drop) { if (!length(drop)) return(c(0, 0))
    r <- stats::update(fit, stats::as.formula(paste(". ~ . -", paste(drop, collapse = " - "))))
    c(sum(stats::residuals(r)^2) - ss_res, r$df.residual - df_res) }
  rows <- list(data.frame(Source = "Model", SS = ss_model, df = df_model))
  if (!mix) {
    for (g in unique(grp)) { v <- partial(tl[grp == g]); rows[[length(rows) + 1]] <- data.frame(Source = paste0("  ", g, " terms"), SS = v[1], df = v[2]) }
    for (t in tl) { v <- partial(t); rows[[length(rows) + 1]] <- data.frame(Source = paste0("    ", esc_term_label(t)), SS = v[1], df = v[2]) }
  } else {
    for (g in setdiff(unique(grp), "Linear blend")) { v <- partial(tl[grp == g]); rows[[length(rows) + 1]] <- data.frame(Source = paste0("  ", g, " terms"), SS = v[1], df = v[2]) }
  }
  rows[[length(rows) + 1]] <- data.frame(Source = "Residual", SS = ss_res, df = df_res)
  key <- do.call(paste, c(lapply(factors, function(f) round(dc[[f]], 6)), sep = "_"))
  pe <- tapply(dc[[y]], key, function(v) sum((v - mean(v))^2)); pe_df <- tapply(dc[[y]], key, length) - 1
  ss_pe <- sum(pe); df_pe <- sum(pe_df)
  lof <- NULL
  if (df_pe > 0 && df_res - df_pe > 0) {
    rows[[length(rows) + 1]] <- data.frame(Source = "  Lack of fit", SS = ss_res - ss_pe, df = df_res - df_pe)
    rows[[length(rows) + 1]] <- data.frame(Source = "  Pure error", SS = ss_pe, df = df_pe)
  }
  rows[[length(rows) + 1]] <- data.frame(Source = "Total (corrected)", SS = sst, df = n - 1)
  an <- do.call(rbind, rows); an$MS <- an$SS / an$df
  ms_res <- ss_res / df_res
  an$F <- ifelse(an$Source %in% c("Residual", "  Pure error", "Total (corrected)"), NA, an$MS / ms_res)
  if (df_pe > 0 && df_res - df_pe > 0) { i <- an$Source == "  Lack of fit"; an$F[i] <- an$MS[i] / (ss_pe / df_pe)
    an$P <- NA; an$P[i] <- stats::pf(an$F[i], an$df[i], df_pe, lower.tail = FALSE) }
  else an$P <- NA
  j <- !is.na(an$F) & is.na(an$P); an$P[j] <- stats::pf(an$F[j], an$df[j], df_res, lower.tail = FALSE)
  an$Signif <- esc_stars(an$P)
  # fit statistics
  h <- stats::hatvalues(fit); press <- sum((res / (1 - h))^2)
  pred_range <- diff(range(stats::fitted(fit))); avg_var <- p * ms_res / n
  fs <- data.frame(Statistic = c("R\u00b2", "Adjusted R\u00b2", "Predicted R\u00b2 (PRESS)", "Adequate precision", "Std. deviation (RMSE)",
                                 "Mean", "CV (%)", "PRESS", "Lack-of-fit P", "Observations", "Terms removed (backward, P > alpha)"),
                   Value = c(sprintf("%.4f", sm$r.squared), sprintf("%.4f", sm$adj.r.squared), sprintf("%.4f", 1 - press / sst),
                             sprintf("%.2f", pred_range / sqrt(avg_var)), sprintf("%.4g", sqrt(ms_res)), sprintf("%.4g", mean(dc[[y]])),
                             sprintf("%.2f", 100 * sqrt(ms_res) / mean(dc[[y]])), sprintf("%.4g", press),
                             if (!is.null(an$P[an$Source == "  Lack of fit"]) && any(an$Source == "  Lack of fit")) esc_fmtp(an$P[an$Source == "  Lack of fit"]) else "n.a. (no replicates)",
                             n, if (length(removed)) paste(esc_term_label(removed), collapse = ", ") else "none"))
  co <- sm$coefficients; ci <- stats::confint(fit)
  coef <- data.frame(Term = esc_term_label(rownames(co)), Coefficient = co[, 1], SE = co[, 2], t = co[, 3], P = co[, 4],
                     `95% CI low` = ci[, 1], `95% CI high` = ci[, 2], VIF = NA, check.names = FALSE, row.names = NULL)
  vif <- tryCatch({ X <- stats::model.matrix(fit)[, -1, drop = FALSE]; if (ncol(X) > 1) diag(solve(stats::cor(X))) else 1 }, error = function(e) NULL)
  if (!is.null(vif) && !mix) coef$VIF[-1] <- vif
  coef$Signif <- esc_stars(coef$P)
  # equation in coded and actual units
  b <- stats::coef(fit)
  eq_coded <- paste0(labs[[y]], " = ", paste0(sprintf("%+.4g", b), ifelse(names(b) == "(Intercept)", "", paste0(" ", esc_term_label(names(b)))), collapse = " "))
  eq_coded <- sub("= \\+", "= ", eq_coded)
  # canonical analysis (quadratic, process factors)
  canon <- NULL
  if (!mix && model == "quadratic") canon <- tryCatch(esc_canonical(fit, factors, lo, hi), error = function(e) NULL)
  names(lo) <- names(hi) <- factors
  list(fit = fit, data = d, coded = dc, y = y, factors = factors, labels = labs, model = model, low = lo, high = hi,
       region_low = lo_all, region_high = hi_all, mixture = mix,
       coding = coding, anova = an, fit_stats = fs, coef = coef, equation = eq_coded, canonical = canon, removed = removed)
}

# Stationary point and eigen-analysis of the fitted quadratic surface (coded units)
esc_canonical <- function(fit, factors, lo, hi) {
  b <- stats::coef(fit); k <- length(factors); q <- factors
  getb <- function(nm) { v <- b[nm]; if (is.na(v)) 0 else unname(v) }
  bl <- vapply(q, function(f) getb(f), 0)
  B <- matrix(0, k, k)
  for (i in seq_len(k)) { B[i, i] <- getb(paste0("I(", q[i], "^2)"))
    for (j in seq_len(k)) if (i < j) { v <- getb(paste0(q[i], ":", q[j])); if (v == 0) v <- getb(paste0(q[j], ":", q[i])); B[i, j] <- B[j, i] <- v / 2 } }
  xs <- -0.5 * solve(B, bl); ev <- eigen(B, symmetric = TRUE)
  ys <- unname(stats::predict(fit, as.data.frame(stats::setNames(as.list(xs), factors))))
  nature <- if (all(ev$values < 0)) "Maximum" else if (all(ev$values > 0)) "Minimum" else "Saddle point"
  inside <- all(abs(xs) <= 1.0001)
  list(table = data.frame(Factor = factors, `Stationary point (coded)` = xs, `Stationary point (actual)` = (hi + lo) / 2 + xs * (hi - lo) / 2,
                          check.names = FALSE),
       eigen = data.frame(Eigenvalue = ev$values), predicted = ys, nature = nature, inside = inside)
}

esc_doe_predict <- function(m, newdata_actual) {
  nd <- as.data.frame(newdata_actual)
  if (!m$mixture && m$coding == "auto") for (f in m$factors) nd[[f]] <- (nd[[f]] - (m$high[f] + m$low[f]) / 2) / ((m$high[f] - m$low[f]) / 2)
  as.numeric(stats::predict(m$fit, nd))
}

# Derringer-Suich desirability optimisation across responses
esc_desirability <- function(models, goals, lows, highs, targets = NULL, weights = NULL, n_start = 30, seed = 1) {
  fac <- models[[1]]$factors; lo <- models[[1]]$region_low; hi <- models[[1]]$region_high
  dfun <- function(yhat, g, L, U, T, w) switch(g,
    maximize = pmin(1, pmax(0, (yhat - L) / (U - L)))^w,
    minimize = pmin(1, pmax(0, (U - yhat) / (U - L)))^w,
    target = ifelse(yhat < T, pmin(1, pmax(0, (yhat - L) / (T - L))), pmin(1, pmax(0, (U - yhat) / (U - T))))^w,
    none = rep(1, length(yhat)))
  D <- function(x) {
    nd <- as.data.frame(stats::setNames(as.list(x), fac))
    ds <- vapply(seq_along(models), function(i) dfun(esc_doe_predict(models[[i]], nd), goals[i], lows[i], highs[i],
                                                     targets[i] %||% NA, weights[i] %||% 1), 0)
    act <- goals != "none"; if (!any(act)) return(0)
    prod(ds[act])^(1 / sum(act))
  }
  set.seed(seed)
  starts <- rbind((lo + hi) / 2, t(replicate(n_start, stats::runif(length(fac), lo, hi))))
  best <- NULL
  for (i in seq_len(nrow(starts))) {
    o <- tryCatch(stats::optim(starts[i, ], function(x) -D(x), method = "L-BFGS-B", lower = lo, upper = hi), error = function(e) NULL)
    if (!is.null(o) && (is.null(best) || o$value < best$value)) best <- o
  }
  xs <- stats::setNames(best$par, fac); nd <- as.data.frame(as.list(xs))
  preds <- vapply(models, function(m) esc_doe_predict(m, nd), 0)
  list(settings = data.frame(Factor = fac, Optimum = xs, Low = lo, High = hi, row.names = NULL),
       responses = data.frame(Response = vapply(models, `[[`, "", "y"), Goal = goals, Predicted = preds, row.names = NULL),
       desirability = -best$value, D = D)
}

# Standardised effects (|t|) for the Pareto chart
esc_doe_effects <- function(m) {
  co <- m$coef[m$coef$Term != "(Intercept)", ]
  df_res <- m$fit$df.residual
  data.frame(Term = co$Term, t = abs(co$t), Sign = ifelse(co$Coefficient >= 0, "Positive", "Negative"),
             limit = stats::qt(0.975, max(1, df_res)))
}
