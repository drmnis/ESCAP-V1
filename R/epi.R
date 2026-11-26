# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Epidemiological analysis: prevalence with Wilson CI, chi-square / Fisher exact
# tests, crude and adjusted odds ratios (Firth correction when separation occurs).

esc_wilson <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(NA, NA))
  z <- stats::qnorm(1 - (1 - conf) / 2); p <- x / n
  den <- 1 + z^2 / n; ctr <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, ctr - hw), min(1, ctr + hw))
}

# Normalise outcome text: "Positive ", "POS", "1", "yes" -> 1 ; "negetive", "neg", "0", "no" -> 0
esc_binary_outcome <- function(x) {
  v <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(v))
  out[v %in% c("positive", "pos", "+", "1", "yes", "y", "present", "infected", "reactive")] <- 1
  out[v %in% c("negative", "negetive", "neg", "-", "0", "no", "n", "absent", "non-reactive", "nonreactive")] <- 0
  out
}
esc_is_outcome_col <- function(x) {
  v <- tolower(trimws(as.character(stats::na.omit(x))))
  length(v) > 0 && any(v %in% c("positive", "negative", "negetive", "pos", "neg")) &&
    mean(!is.na(esc_binary_outcome(v))) > 0.9
}
esc_is_id_col <- function(name, x) {
  nm <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
  grepl("(^|_)(id|no|sl|sn|serial|sample|code|name)(_|$)", nm) ||
    (length(unique(x)) == length(x) && length(x) > 10)
}

# Logistic regression with automatic Firth correction under (quasi-)separation
esc_logit <- function(formula, data) {
  fit <- suppressWarnings(stats::glm(formula, data = data, family = stats::binomial()))
  sep <- !fit$converged || any(fit$fitted.values < 1e-8 | fit$fitted.values > 1 - 1e-8) ||
    any(sqrt(diag(stats::vcov(fit))) > 50, na.rm = TRUE)
  if (sep && requireNamespace("logistf", quietly = TRUE)) {
    ff <- logistf::logistf(formula, data = data, plconf = NULL)
    tab <- data.frame(term = names(ff$coefficients), estimate = ff$coefficients,
                      lcl = ff$ci.lower, ucl = ff$ci.upper, p = ff$prob, row.names = NULL)
    return(list(table = tab, method = "Firth penalised likelihood", n = ff$n, aic = NA,
                fit = ff, separation = TRUE))
  }
  co <- summary(fit)$coefficients
  ci <- tryCatch(suppressMessages(stats::confint(fit)), error = function(e) NULL)
  if (is.null(ci) || any(!is.finite(ci))) ci <- stats::confint.default(fit)
  ci <- matrix(ci, ncol = 2, dimnames = list(rownames(co), NULL))
  tab <- data.frame(term = rownames(co), estimate = co[, 1], lcl = ci[rownames(co), 1],
                    ucl = ci[rownames(co), 2], p = co[, 4], row.names = NULL)
  list(table = tab, method = if (sep) "Maximum likelihood (separation detected; install 'logistf' for Firth correction)" else "Maximum likelihood (profile CI)",
       n = stats::nobs(fit), aic = stats::AIC(fit), fit = fit, separation = sep)
}

# Hosmer-Lemeshow goodness of fit (g groups)
esc_hosmer_lemeshow <- function(y, p, g = 10) {
  q <- unique(stats::quantile(p, probs = seq(0, 1, 1 / g)))
  if (length(q) < 3) return(c(chisq = NA, df = NA, p = NA))
  grp <- cut(p, q, include.lowest = TRUE)
  o <- tapply(y, grp, sum); e <- tapply(p, grp, sum); n <- tapply(y, grp, length)
  chi <- sum((o - e)^2 / (e * (1 - e / n)), na.rm = TRUE); df <- length(o) - 2
  c(chisq = chi, df = df, p = stats::pchisq(chi, df, lower.tail = FALSE))
}

# Main analysis for one outcome within one stratum
esc_epi_analysis <- function(data, outcome, predictors, refs = list(), p_enter = 0.20, conf = 0.95) {
  d <- data
  d$.y <- esc_binary_outcome(d[[outcome]])
  d <- d[!is.na(d$.y), , drop = FALSE]
  for (v in predictors) {
    d[[v]] <- factor(trimws(as.character(d[[v]])))
    if (!is.null(refs[[v]]) && refs[[v]] %in% levels(d[[v]])) d[[v]] <- stats::relevel(d[[v]], refs[[v]])
  }
  if (sum(d$.y) == 0 || sum(d$.y) == nrow(d)) return(NULL)
  uni <- list(); uni_p <- c()
  for (v in predictors) {
    dv <- d[!is.na(d[[v]]), , drop = FALSE]; dv[[v]] <- droplevels(dv[[v]])
    if (nlevels(dv[[v]]) < 2) next
    tab <- table(dv[[v]], dv$.y)
    if (ncol(tab) < 2) next
    ex <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
    use_fisher <- any(ex < 5)
    chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    p_test <- if (use_fisher) tryCatch(stats::fisher.test(tab, simulate.p.value = nrow(tab) > 2 && sum(tab) > 200, B = 1e4)$p.value,
                                       error = function(e) chi$p.value) else chi$p.value
    lg <- esc_logit(stats::as.formula(paste(".y ~", paste0("`", v, "`"))), dv)
    uni_p[v] <- p_test
    rows <- lapply(levels(dv[[v]]), function(l) {
      n <- sum(dv[[v]] == l); x <- sum(dv$.y[dv[[v]] == l]); ci <- esc_wilson(x, n, conf)
      term <- paste0("`", v, "`", l); tt <- lg$table[lg$table$term %in% c(term, paste0(v, l)), ]
      data.frame(Variable = v, Level = l, N = n, Positive = x, Prevalence = 100 * x / n,
                 Prev_LCL = 100 * ci[1], Prev_UCL = 100 * ci[2],
                 Chi2 = unname(chi$statistic), df = unname(chi$parameter), P_test = p_test,
                 Test = if (use_fisher) "Fisher exact" else "Pearson chi-square",
                 COR = if (nrow(tt)) exp(tt$estimate) else NA, COR_LCL = if (nrow(tt)) exp(tt$lcl) else NA,
                 COR_UCL = if (nrow(tt)) exp(tt$ucl) else NA, COR_P = if (nrow(tt)) tt$p else NA,
                 is_ref = l == levels(dv[[v]])[1], stringsAsFactors = FALSE)
    })
    uni[[v]] <- do.call(rbind, rows)
  }
  if (!length(uni)) return(NULL)
  uni_tab <- do.call(rbind, uni)
  # multivariable model: predictors with univariable p < p_enter
  sel <- names(uni_p)[uni_p < p_enter]
  multi <- NULL; fit_stats <- NULL
  if (length(sel) >= 1) {
    dm <- d[stats::complete.cases(d[, sel, drop = FALSE]), , drop = FALSE]
    lg <- esc_logit(stats::as.formula(paste(".y ~", paste0("`", sel, "`", collapse = " + "))), dm)
    tt <- lg$table
    uni_tab$AOR <- uni_tab$AOR_LCL <- uni_tab$AOR_UCL <- uni_tab$AOR_P <- NA
    for (i in seq_len(nrow(uni_tab))) {
      if (!uni_tab$Variable[i] %in% sel) next
      term <- c(paste0("`", uni_tab$Variable[i], "`", uni_tab$Level[i]), paste0(uni_tab$Variable[i], uni_tab$Level[i]))
      r <- tt[tt$term %in% term, ]
      if (nrow(r)) {
        uni_tab$AOR[i] <- exp(r$estimate); uni_tab$AOR_LCL[i] <- exp(r$lcl)
        uni_tab$AOR_UCL[i] <- exp(r$ucl); uni_tab$AOR_P[i] <- r$p
      }
    }
    events <- sum(dm$.y); n_par <- nrow(tt) - 1
    if (inherits(lg$fit, "glm")) {
      p_hat <- stats::fitted(lg$fit)
      ll1 <- as.numeric(stats::logLik(lg$fit)); ll0 <- as.numeric(stats::logLik(stats::update(lg$fit, . ~ 1)))
    } else {                                   # logistf object (penalised log-likelihoods)
      p_hat <- lg$fit$predict; ll1 <- lg$fit$loglik[["full"]] %||% lg$fit$loglik[2]
      ll0 <- lg$fit$loglik[["null"]] %||% lg$fit$loglik[1]
      lg$aic <- -2 * ll1 + 2 * nrow(tt)
    }
    hl <- esc_hosmer_lemeshow(dm$.y, p_hat)
    n <- nrow(dm); r2cs <- 1 - exp(2 * (ll0 - ll1) / n); r2n <- r2cs / (1 - exp(2 * ll0 / n))
    auc <- esc_auc(p_hat, dm$.y == 1)
    fit_stats <- data.frame(
      Statistic = c("Method", "Variables entered (univariable P < threshold)", "Observations", "Events",
                    "Events per parameter", "AIC", "Nagelkerke R2", "Hosmer-Lemeshow chi-square (P)", "AUC (ROC)"),
      Value = c(lg$method, paste(sel, collapse = ", "), nrow(dm), events, round(events / max(1, n_par), 1),
                round(lg$aic, 2), round(r2n, 3), sprintf("%.2f (%.3f)", hl[1], hl[3]), round(auc, 3)),
      stringsAsFactors = FALSE)
    multi <- lg
  } else {
    uni_tab$AOR <- uni_tab$AOR_LCL <- uni_tab$AOR_UCL <- uni_tab$AOR_P <- NA
  }
  list(table = uni_tab, fit_stats = fit_stats, multi = multi, selected = sel,
       overall = c(N = nrow(d), Positive = sum(d$.y), Prevalence = 100 * mean(d$.y),
                   LCL = 100 * esc_wilson(sum(d$.y), nrow(d), conf)[1], UCL = 100 * esc_wilson(sum(d$.y), nrow(d), conf)[2]))
}

esc_fmt_p <- function(p, html = TRUE) {
  if (is.na(p)) return("")
  s <- if (p < 0.001) "<0.001" else formatC(p, format = "f", digits = 3)
  st <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
  paste0(if (html && p < 0.05) paste0("<b>", s, "</b>") else s, st)
}
esc_fmt_or <- function(or, l, u) {
  if (is.na(or)) return("")
  f <- function(x) if (!is.finite(x)) "Inf" else if (x >= 1000 || x < 0.01) formatC(x, format = "e", digits = 2) else sprintf("%.2f", x)
  sprintf("%s (%s-%s)", f(or), f(l), f(u))
}
