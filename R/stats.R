# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Classical statistics for food and agricultural experiments: descriptive statistics,
# assumption checks, t-tests, one- and two-way ANOVA (CRD / RCBD, with interaction),
# post-hoc tests with letters, ANCOVA with adjusted means, non-parametric tests,
# correlation / regression and reaction-kinetics models. Pure functions.

ESC_STAT_TESTS <- c(
  "Descriptive statistics" = "desc",
  "t-test (one-sample, independent, paired)" = "ttest",
  "One-way ANOVA + post-hoc (CRD / RCBD)" = "anova1",
  "Two-way ANOVA with interaction (CRD / RCBD)" = "anova2",
  "ANCOVA (covariate-adjusted means)" = "ancova",
  "Kruskal-Wallis + Dunn (non-parametric)" = "kruskal",
  "Correlation tests (Pearson / Spearman / Kendall)" = "cor",
  "Linear / polynomial regression" = "reg",
  "Reaction kinetics (zero, first, second order, Weibull)" = "kin"
)

esc_fmtp <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
esc_stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))
esc_q <- function(x) paste0("`", x, "`")

# ---------------------------------------------------------------- descriptive
esc_describe <- function(df, vars, group = NULL, conf = 0.95) {
  sk <- function(x) { x <- x[is.finite(x)]; n <- length(x); if (n < 3) return(NA); m <- mean(x); s <- stats::sd(x)
    n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3) }
  ku <- function(x) { x <- x[is.finite(x)]; n <- length(x); if (n < 4) return(NA); m <- mean(x); s <- stats::sd(x)
    n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3)) }
  one <- function(x) {
    x <- x[is.finite(x)]; n <- length(x); m <- mean(x); s <- stats::sd(x); se <- s / sqrt(n)
    tq <- if (n > 1) stats::qt(1 - (1 - conf) / 2, n - 1) else NA
    q <- stats::quantile(x, c(0, .25, .5, .75, 1), names = FALSE)
    data.frame(n = n, Mean = m, SD = s, SE = se, `CV (%)` = 100 * s / m, CI_low = m - tq * se, CI_high = m + tq * se,
               Min = q[1], Q1 = q[2], Median = q[3], Q3 = q[4], Max = q[5], Skewness = sk(x), Kurtosis = ku(x),
               check.names = FALSE)
  }
  out <- list()
  for (v in vars) {
    if (is.null(group)) out[[v]] <- cbind(Variable = v, Group = "All", one(df[[v]]))
    else for (g in sort(unique(stats::na.omit(df[[group]])))) out[[paste(v, g)]] <- cbind(Variable = v, Group = g, one(df[[v]][df[[group]] == g]))
  }
  r <- do.call(rbind, out); rownames(r) <- NULL
  names(r)[names(r) == "CI_low"] <- sprintf("%.0f%% CI low", 100 * conf); names(r)[names(r) == "CI_high"] <- sprintf("%.0f%% CI high", 100 * conf)
  r
}

# ---------------------------------------------------------------- assumptions
esc_assumptions <- function(fit, data, groups_formula = NULL) {
  r <- stats::residuals(fit)
  sw <- if (length(r) >= 3 && length(r) <= 5000) stats::shapiro.test(r) else NULL
  out <- data.frame(Test = "Shapiro-Wilk (residual normality)", Statistic = if (!is.null(sw)) unname(sw$statistic) else NA,
                    df = "", P = if (!is.null(sw)) sw$p.value else NA, stringsAsFactors = FALSE)
  if (!is.null(groups_formula)) {
    lv <- tryCatch(car::leveneTest(groups_formula, data = data, center = stats::median), error = function(e) NULL)
    if (!is.null(lv)) out <- rbind(out, data.frame(Test = "Levene / Brown-Forsythe (equal variances)", Statistic = lv[1, "F value"],
                                                  df = paste(lv$Df, collapse = ", "), P = lv[1, "Pr(>F)"]))
    bt <- tryCatch(stats::bartlett.test(groups_formula, data = data), error = function(e) NULL)
    if (!is.null(bt)) out <- rbind(out, data.frame(Test = "Bartlett (equal variances, assumes normality)", Statistic = unname(bt$statistic),
                                                  df = as.character(bt$parameter), P = bt$p.value))
  }
  out$Decision <- ifelse(is.na(out$P), "", ifelse(out$P < 0.05, "Assumption violated (P < 0.05)", "Assumption met"))
  out
}

# ---------------------------------------------------------------- t-tests
esc_cohen_d <- function(x, y = NULL, paired = FALSE, mu = 0) {
  if (is.null(y)) { d <- (mean(x) - mu) / stats::sd(x); n <- length(x); se <- sqrt(1 / n + d^2 / (2 * n)); J <- 1 - 3 / (4 * (n - 1) - 1) }
  else if (paired) { dd <- x - y; d <- mean(dd) / stats::sd(dd); n <- length(dd); se <- sqrt(1 / n + d^2 / (2 * n)); J <- 1 - 3 / (4 * (n - 1) - 1) }
  else { n1 <- length(x); n2 <- length(y); sp <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / (n1 + n2 - 2))
    d <- (mean(x) - mean(y)) / sp; se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2))); J <- 1 - 3 / (4 * (n1 + n2) - 9) }
  g <- J * d
  c(d = d, g = g, g_low = g - 1.96 * se * J, g_high = g + 1.96 * se * J)
}

esc_ttest <- function(df, y, group = NULL, type = c("independent", "paired", "one"), mu = 0, var_equal = FALSE,
                      alternative = "two.sided", conf = 0.95, pair_id = NULL) {
  type <- match.arg(type)
  if (type == "one") {
    x <- df[[y]][is.finite(df[[y]])]
    tt <- stats::t.test(x, mu = mu, alternative = alternative, conf.level = conf)
    wt <- suppressWarnings(stats::wilcox.test(x, mu = mu, alternative = alternative, exact = FALSE))
    es <- esc_cohen_d(x, mu = mu)
    desc <- data.frame(Group = y, n = length(x), Mean = mean(x), SD = stats::sd(x), SE = stats::sd(x) / sqrt(length(x)))
    lab <- sprintf("One-sample t-test (mu = %g)", mu); nplab <- "Wilcoxon signed-rank test"
  } else {
    g <- factor(df[[group]]); lev <- levels(droplevels(g[!is.na(df[[y]])]))
    if (length(lev) != 2) stop("The grouping column must have exactly two groups (found ", length(lev), ").")
    if (type == "paired") {
      d1 <- df[g == lev[1], , drop = FALSE]; d2 <- df[g == lev[2], , drop = FALSE]
      if (!is.null(pair_id) && nzchar(pair_id)) { d1 <- d1[order(d1[[pair_id]]), ]; d2 <- d2[match(d1[[pair_id]], d2[[pair_id]]), ] }
      if (nrow(d1) != nrow(d2)) stop("Paired test needs the same number of observations in both groups.")
      x <- d1[[y]]; z <- d2[[y]]; ok <- is.finite(x) & is.finite(z); x <- x[ok]; z <- z[ok]
      tt <- stats::t.test(x, z, paired = TRUE, alternative = alternative, conf.level = conf)
      wt <- suppressWarnings(stats::wilcox.test(x, z, paired = TRUE, alternative = alternative, exact = FALSE))
      es <- esc_cohen_d(x, z, paired = TRUE); lab <- "Paired t-test"; nplab <- "Wilcoxon signed-rank test (paired)"
    } else {
      x <- df[[y]][g == lev[1]]; z <- df[[y]][g == lev[2]]; x <- x[is.finite(x)]; z <- z[is.finite(z)]
      tt <- stats::t.test(x, z, var.equal = var_equal, alternative = alternative, conf.level = conf)
      wt <- suppressWarnings(stats::wilcox.test(x, z, alternative = alternative, exact = FALSE))
      es <- esc_cohen_d(x, z); lab <- if (var_equal) "Student two-sample t-test" else "Welch two-sample t-test"
      nplab <- "Mann-Whitney U (Wilcoxon rank-sum) test"
    }
    desc <- data.frame(Group = lev, n = c(length(x), length(z)), Mean = c(mean(x), mean(z)),
                       SD = c(stats::sd(x), stats::sd(z)), SE = c(stats::sd(x) / sqrt(length(x)), stats::sd(z) / sqrt(length(z))))
  }
  res <- data.frame(
    Test = c(lab, nplab),
    Statistic = c(sprintf("t = %.3f", tt$statistic), sprintf("%s = %.1f", names(wt$statistic), wt$statistic)),
    df = c(sprintf("%.2f", tt$parameter), ""),
    `Mean difference` = c(sprintf("%.4g", if (length(tt$estimate) == 2) diff(rev(tt$estimate)) else tt$estimate - if (type == "one") mu else 0), ""),
    `95% CI` = c(sprintf("%.4g to %.4g", tt$conf.int[1], tt$conf.int[2]), ""),
    P = c(esc_fmtp(tt$p.value), esc_fmtp(wt$p.value)), Signif = esc_stars(c(tt$p.value, wt$p.value)),
    check.names = FALSE, stringsAsFactors = FALSE)
  eff <- data.frame(`Effect size` = c("Cohen's d", "Hedges' g (95% CI)"),
                    Value = c(sprintf("%.3f", es["d"]), sprintf("%.3f (%.3f to %.3f)", es["g"], es["g_low"], es["g_high"])),
                    Magnitude = rep(cut(abs(es["g"]), c(-Inf, 0.2, 0.5, 0.8, Inf), c("negligible", "small", "medium", "large")), 2),
                    check.names = FALSE)
  list(descriptives = desc, tests = res, effect = eff, p_t = tt$p.value, p_np = wt$p.value,
       t_stat = unname(tt$statistic), df = unname(tt$parameter))
}

# ---------------------------------------------------------------- ANOVA engine (one- or two-way, CRD or RCBD)
esc_anova <- function(df, y, A, B = NULL, block = NULL, interaction = TRUE, ss_type = 2,
                      posthoc = "HSD", alpha = 0.05, covariate = NULL) {
  d <- df[, unique(c(y, A, B, block, covariate)), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  for (f in c(A, B, block)) d[[f]] <- factor(d[[f]])
  names(d) <- make.names(names(d)); y2 <- make.names(y); A2 <- make.names(A)
  B2 <- if (!is.null(B)) make.names(B); bl2 <- if (!is.null(block)) make.names(block)
  cv2 <- if (!is.null(covariate)) make.names(covariate)
  rhs <- c(bl2, cv2, if (!is.null(B2) && interaction) paste(A2, B2, sep = "*") else c(A2, B2))
  fml <- stats::as.formula(paste(y2, "~", paste(rhs, collapse = " + ")))
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  fit <- stats::aov(fml, data = d)
  lmfit <- stats::lm(fml, data = d)
  at <- if (ss_type == 1) { a <- stats::anova(lmfit); a } else car::Anova(lmfit, type = ss_type)
  at <- as.data.frame(at); at$Term <- rownames(at); rownames(at) <- NULL
  if (ss_type != 1) { names(at)[names(at) == "Sum Sq"] <- "Sum Sq"; at$`Mean Sq` <- at$`Sum Sq` / at$Df }
  ss_res <- at$`Sum Sq`[at$Term == "Residuals"]
  at$`Partial eta2` <- ifelse(at$Term == "Residuals", NA, at$`Sum Sq` / (at$`Sum Sq` + ss_res))
  at <- at[, c("Term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)", "Partial eta2")]
  at$Term <- gsub(":", " x ", at$Term)
  at$Signif <- esc_stars(at$`Pr(>F)`)
  mse <- ss_res / at$Df[at$Term == "Residuals"]
  cv <- 100 * sqrt(mse) / mean(d[[y2]])
  # post hoc letters. agricolae mangles level names containing "&", ":" and similar characters, so the
  # letters are computed on coded levels (L1, L2, ...) and mapped back to the original names.
  dcode <- d; lev_map <- list()
  for (f in c(A2, B2, bl2)) { lv <- levels(d[[f]]); cd <- paste0("L", seq_along(lv)); lev_map[[f]] <- stats::setNames(lv, cd)
    dcode[[f]] <- factor(cd[as.integer(d[[f]])], levels = cd) }
  fit_c <- stats::aov(fml, data = dcode)
  decode <- function(lv, trt) vapply(strsplit(lv, ":", fixed = TRUE), function(z)
    paste(vapply(seq_along(z), function(i) lev_map[[trt[i]]][[z[i]]] %||% z[i], ""), collapse = ":"), "")
  ph <- function(trt) {
    f <- switch(posthoc,
      HSD = agricolae::HSD.test, LSD = agricolae::LSD.test, Duncan = agricolae::duncan.test,
      SNK = agricolae::SNK.test, Scheffe = agricolae::scheffe.test, Bonferroni = agricolae::LSD.test)
    args <- list(fit_c, trt, alpha = alpha, group = TRUE)
    if (posthoc == "Bonferroni") args$p.adj <- "bonferroni"
    r <- do.call(f, args)
    g <- r$groups; g$Level <- rownames(g); rownames(g) <- NULL
    m <- r$means; m$Level <- rownames(m)
    out <- merge(m[, c("Level", "r", "std")], g[, c("Level", y2, "groups")], by = "Level", sort = FALSE)
    names(out) <- c("Level", "n", "SD", "Mean", "Group")
    out$SE <- out$SD / sqrt(out$n)
    out$Level <- decode(as.character(out$Level), trt)
    out <- out[order(-out$Mean), c("Level", "n", "Mean", "SD", "SE", "Group")]; rownames(out) <- NULL; out
  }
  letters_A <- tryCatch(ph(A2), error = function(e) NULL)
  letters_B <- if (!is.null(B2)) tryCatch(ph(B2), error = function(e) NULL)
  letters_AB <- if (!is.null(B2) && interaction) tryCatch(ph(c(A2, B2)), error = function(e) NULL)
  # simple effects (emmeans) when interaction is present
  simple <- NULL
  if (!is.null(B2) && interaction) {
    simple <- tryCatch({
      em <- emmeans::emmeans(lmfit, stats::as.formula(paste("pairwise ~", A2, "|", B2)), adjust = "tukey")
      s <- as.data.frame(em$contrasts); s$P <- esc_fmtp(s$p.value); s$Signif <- esc_stars(s$p.value)
      s[, c(B2, "contrast", "estimate", "SE", "df", "t.ratio", "P", "Signif")]
    }, error = function(e) NULL)
  }
  # covariate-adjusted means (ANCOVA)
  adj <- NULL
  if (!is.null(cv2)) {
    adj <- tryCatch({
      em <- emmeans::emmeans(lmfit, stats::as.formula(paste("pairwise ~", A2)), adjust = "tukey")
      list(means = as.data.frame(em$emmeans), pairs = as.data.frame(em$contrasts))
    }, error = function(e) NULL)
  }
  list(fit = fit, lm = lmfit, data = d, y = y2, A = A2, B = B2, block = bl2, covariate = cv2, formula = fml,
       anova = at, cv = cv, mse = mse, letters_A = letters_A, letters_B = letters_B, letters_AB = letters_AB,
       simple = simple, adjusted = adj,
       assumptions = esc_assumptions(lmfit, d, if (is.null(cv2)) stats::as.formula(paste(y2, "~", paste(c(A2, B2), collapse = "*"))) else
                                       stats::as.formula(paste(y2, "~", A2))))
}

# ANCOVA: homogeneity of regression slopes + adjusted means
esc_ancova <- function(df, y, group, covariate, ss_type = 2) {
  d <- df[stats::complete.cases(df[, c(y, group, covariate)]), c(y, group, covariate)]
  names(d) <- make.names(names(d)); y2 <- make.names(y); g2 <- make.names(group); c2 <- make.names(covariate)
  d[[g2]] <- factor(d[[g2]])
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  slopes <- stats::lm(stats::as.formula(paste(y2, "~", c2, "*", g2)), data = d)
  hs <- as.data.frame(car::Anova(slopes, type = ss_type)); hs$Term <- gsub(":", " x ", rownames(hs))
  p_int <- hs[grepl(" x ", hs$Term), "Pr(>F)"]
  res <- esc_anova(df, y, group, covariate = covariate, ss_type = ss_type)
  slope_tab <- data.frame(Check = "Homogeneity of regression slopes (covariate x group)",
                          F = hs[grepl(" x ", hs$Term), "F value"], P = esc_fmtp(p_int),
                          Decision = ifelse(p_int < 0.05, "Slopes differ: ANCOVA assumption violated", "Slopes parallel: ANCOVA valid"))
  co <- stats::coef(res$lm)[c2]
  res$slope_check <- slope_tab; res$common_slope <- co
  res
}

# ---------------------------------------------------------------- non-parametric
esc_kruskal <- function(df, y, group, p_adjust = "holm") {
  d <- df[stats::complete.cases(df[, c(y, group)]), c(y, group)]; names(d) <- c("y", "g"); d$g <- factor(d$g)
  kt <- stats::kruskal.test(y ~ g, data = d)
  n <- nrow(d); k <- nlevels(d$g); eps2 <- (kt$statistic - k + 1) / (n - k)
  test <- data.frame(Test = "Kruskal-Wallis", `Chi-square` = unname(kt$statistic), df = unname(kt$parameter),
                     P = esc_fmtp(kt$p.value), Signif = esc_stars(kt$p.value), `Epsilon-squared` = unname(eps2), check.names = FALSE)
  # Dunn test with tie correction
  r <- rank(d$y); N <- n; ti <- table(d$y); tie <- sum(ti^3 - ti) / (12 * (N - 1))
  mr <- tapply(r, d$g, mean); ni <- tapply(r, d$g, length); lv <- levels(d$g)
  pr <- utils::combn(lv, 2)
  dunn <- do.call(rbind, lapply(seq_len(ncol(pr)), function(j) {
    a <- pr[1, j]; b <- pr[2, j]
    se <- sqrt((N * (N + 1) / 12 - tie) * (1 / ni[a] + 1 / ni[b])); z <- (mr[a] - mr[b]) / se
    data.frame(Comparison = paste(a, "-", b), Z = unname(z), P_raw = 2 * stats::pnorm(-abs(z)))
  }))
  dunn$P_adj <- stats::p.adjust(dunn$P_raw, p_adjust); p_adj_num <- dunn$P_adj
  dunn$Signif <- esc_stars(dunn$P_adj); dunn$P_raw <- esc_fmtp(dunn$P_raw); dunn$P_adj <- esc_fmtp(dunn$P_adj)
  med <- do.call(rbind, lapply(lv, function(l) { x <- d$y[d$g == l]; data.frame(Group = l, n = length(x), Median = stats::median(x),
    IQR = stats::IQR(x), `Mean rank` = unname(mr[l]), Mean = mean(x), SD = stats::sd(x), SE = stats::sd(x) / sqrt(length(x)),
    check.names = FALSE) }))
  list(test = test, dunn = dunn, summary = med, p_value = kt$p.value, p_adj = p_adj_num)
}

# ---------------------------------------------------------------- correlation / regression
esc_cor_test_table <- function(df, vars, method = "pearson", p_adjust = "none") {
  pr <- utils::combn(vars, 2)
  out <- do.call(rbind, lapply(seq_len(ncol(pr)), function(j) {
    a <- pr[1, j]; b <- pr[2, j]; ok <- is.finite(df[[a]]) & is.finite(df[[b]])
    ct <- suppressWarnings(stats::cor.test(df[[a]][ok], df[[b]][ok], method = method, exact = FALSE))
    data.frame(`Variable 1` = a, `Variable 2` = b, n = sum(ok), r = unname(ct$estimate),
               `95% CI` = if (!is.null(ct$conf.int)) sprintf("%.3f to %.3f", ct$conf.int[1], ct$conf.int[2]) else "",
               P_raw = ct$p.value, check.names = FALSE)
  }))
  out$P <- esc_fmtp(stats::p.adjust(out$P_raw, p_adjust)); out$Signif <- esc_stars(stats::p.adjust(out$P_raw, p_adjust))
  out$P_raw <- NULL
  out
}

esc_regression <- function(df, y, x, degree = 1) {
  d <- df[is.finite(df[[y]]) & is.finite(df[[x]]), c(y, x)]; names(d) <- c("y", "x")
  fit <- stats::lm(y ~ stats::poly(x, degree, raw = TRUE), data = d)
  s <- summary(fit); co <- s$coefficients
  rownames(co) <- c("Intercept", if (degree == 1) "Slope" else paste0("x^", seq_len(degree)))
  coef_tab <- data.frame(Term = rownames(co), Estimate = co[, 1], SE = co[, 2], t = co[, 3], P = esc_fmtp(co[, 4]),
                         Signif = esc_stars(co[, 4]), row.names = NULL)
  ci <- stats::confint(fit); coef_tab$`95% CI` <- sprintf("%.4g to %.4g", ci[, 1], ci[, 2])
  f <- s$fstatistic
  fit_tab <- data.frame(Statistic = c("n", "R2", "Adjusted R2", "RMSE", "F", "P (model)", "AIC"),
    Value = c(nrow(d), sprintf("%.4f", s$r.squared), sprintf("%.4f", s$adj.r.squared), sprintf("%.4g", sqrt(mean(stats::residuals(fit)^2))),
              sprintf("%.3f on %d and %d df", f[1], f[2], f[3]), esc_fmtp(stats::pf(f[1], f[2], f[3], lower.tail = FALSE)),
              sprintf("%.2f", stats::AIC(fit))))
  b <- stats::coef(fit)
  eq <- paste0("y = ", sprintf("%.4g", b[1]), paste0(ifelse(b[-1] >= 0, " + ", " - "), sprintf("%.4g", abs(b[-1])),
               c("x", paste0("x^", 2:max(2, degree)))[seq_len(degree)], collapse = ""))
  list(fit = fit, data = d, coef = coef_tab, fit_stats = fit_tab, equation = eq, r2 = s$r.squared)
}

# ---------------------------------------------------------------- reaction kinetics (food science)
esc_kinetics <- function(df, t, conc, group = NULL) {
  models <- list(
    `Zero order`   = list(f = function(t, C0, k) C0 - k * t, start = function(t, C) list(C0 = C[which.min(t)], k = -stats::coef(stats::lm(C ~ t))[2])),
    `First order`  = list(f = function(t, C0, k) C0 * exp(-k * t), start = function(t, C) { C <- pmax(C, 1e-9)
                        list(C0 = C[which.min(t)], k = max(1e-6, -stats::coef(stats::lm(log(C) ~ t))[2])) }),
    `Second order` = list(f = function(t, C0, k) C0 / (1 + k * C0 * t), start = function(t, C) { C <- pmax(C, 1e-9)
                        list(C0 = C[which.min(t)], k = max(1e-6, stats::coef(stats::lm(I(1 / C) ~ t))[2])) }),
    `Weibull`      = list(f = function(t, C0, k, beta) C0 * exp(-(k * t)^beta), start = function(t, C) { C <- pmax(C, 1e-9)
                        list(C0 = C[which.min(t)], k = max(1e-6, -stats::coef(stats::lm(log(C) ~ t))[2]), beta = 1) }))
  grp <- if (is.null(group) || !nzchar(group)) rep("All", nrow(df)) else as.character(df[[group]])
  res <- list(); curves <- list()
  for (g in unique(grp)) {
    d <- data.frame(t = df[[t]][grp == g], C = df[[conc]][grp == g]); d <- d[is.finite(d$t) & is.finite(d$C), ]
    if (length(unique(d$t)) < 3) next
    for (m in names(models)) {
      mm <- models[[m]]
      fit <- tryCatch(stats::nls(stats::as.formula(paste("C ~ mm$f(t,", paste(names(mm$start(d$t, d$C)), collapse = ","), ")")),
                                 data = d, start = mm$start(d$t, d$C), control = stats::nls.control(maxiter = 200, warnOnly = TRUE)),
                      error = function(e) NULL)
      if (is.null(fit)) next
      pr <- stats::predict(fit); n <- nrow(d); kk <- length(stats::coef(fit)); sse <- sum((d$C - pr)^2)
      co <- summary(fit)$coefficients
      k <- co["k", 1]
      res[[paste(g, m)]] <- data.frame(Group = g, Model = m, C0 = co["C0", 1], k = k, `SE(k)` = co["k", 2],
        beta = if ("beta" %in% rownames(co)) co["beta", 1] else NA,
        `Half-life` = switch(m, `Zero order` = co["C0", 1] / (2 * k), `First order` = log(2) / k,
                             `Second order` = 1 / (k * co["C0", 1]), Weibull = (log(2))^(1 / co["beta", 1]) / k),
        R2 = 1 - sse / sum((d$C - mean(d$C))^2), RMSE = sqrt(sse / n), AIC = n * log(sse / n) + 2 * kk, check.names = FALSE)
      tt <- seq(min(d$t), max(d$t), length.out = 100)
      curves[[paste(g, m)]] <- data.frame(Group = g, Model = m, t = tt, C = stats::predict(fit, newdata = data.frame(t = tt)))
    }
  }
  if (!length(res)) stop("Kinetic models could not be fitted. Each group needs at least 3 distinct time points with changing values.")
  tab <- do.call(rbind, res); rownames(tab) <- NULL
  tab <- do.call(rbind, lapply(split(tab, tab$Group), function(x) { x$`Best (min AIC)` <- ifelse(x$AIC == min(x$AIC), "*", ""); x }))
  list(table = tab, curves = do.call(rbind, curves),
       data = data.frame(Group = grp, t = df[[t]], C = df[[conc]]))
}

# Arrhenius: k values at several temperatures (deg C) -> Ea (kJ/mol)
esc_arrhenius <- function(temp_c, k) {
  T <- temp_c + 273.15; fit <- stats::lm(log(k) ~ I(1 / T)); co <- summary(fit)$coefficients
  data.frame(Parameter = c("Ea (kJ/mol)", "ln(A)", "R2"),
             Estimate = c(-co[2, 1] * 8.314462618 / 1000, co[1, 1], summary(fit)$r.squared),
             SE = c(co[2, 2] * 8.314462618 / 1000, co[1, 2], NA))
}
