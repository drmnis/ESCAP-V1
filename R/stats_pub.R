# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Publication-ready tables for every univariate analysis: treatments in rows, responses in
# columns, "mean +- SD (or SE)" with superscript letters, and test statistics as footer rows.

# Compact letter display from a pairwise P-value matrix (insert-and-absorb; Piepho, 2004).
# 'means' is a named vector; letters start with "a" for the largest mean.
esc_cld <- function(means, pmat, alpha = 0.05) {
  lev <- names(sort(means, decreasing = TRUE)); k <- length(lev)
  if (k == 0) return(character(0))
  L <- matrix(TRUE, k, 1, dimnames = list(lev, NULL))
  for (i in seq_len(k - 1)) for (j in (i + 1):k) {
    p <- pmat[lev[i], lev[j]]
    if (is.na(p) || p >= alpha) next
    cols <- which(L[i, ] & L[j, ])
    for (cc in cols) {
      a <- L[, cc]; b <- L[, cc]; a[i] <- FALSE; b[j] <- FALSE
      L[, cc] <- a; L <- cbind(L, b)
    }
    # absorb: drop columns contained in another column, and empty columns
    keep <- rep(TRUE, ncol(L))
    for (x in seq_len(ncol(L))) for (y in seq_len(ncol(L))) {
      if (x != y && keep[x] && keep[y] && all(L[, x] <= L[, y]) && (any(L[, x] < L[, y]) || x > y)) keep[x] <- FALSE
    }
    L <- L[, keep & colSums(L) > 0, drop = FALSE]
  }
  first <- apply(L, 2, function(v) which(v)[1]); L <- L[, order(first), drop = FALSE]
  lt <- vapply(seq_len(k), function(i) paste(letters[which(L[i, ])], collapse = ""), "")
  stats::setNames(lt, lev)[names(means)]
}

# Pairwise P matrix from "A - B" contrast labels
esc_pmat_from_pairs <- function(levels, contrast, p) {
  # Matches "A - B" labels exactly against all level pairs, so level names that contain
  # brackets, "-" or spaces (e.g. "F1 (0% moringa)") are handled; emmeans may wrap names in ( ).
  M <- matrix(NA_real_, length(levels), length(levels), dimnames = list(levels, levels))
  v <- function(x) c(x, paste0("(", x, ")"))
  key <- character(0); ia <- integer(0); ib <- integer(0)
  for (i in seq_along(levels)) for (j in seq_along(levels)) if (i != j)
    for (x in v(levels[i])) for (y in v(levels[j])) { key <- c(key, paste(x, "-", y)); ia <- c(ia, i); ib <- c(ib, j) }
  for (k in seq_along(contrast)) {
    m <- match(trimws(as.character(contrast[k])), key)
    if (!is.na(m)) M[ia[m], ib[m]] <- M[ib[m], ia[m]] <- p[k]
  }
  M
}

esc_dec <- function(x) { m <- max(abs(x), na.rm = TRUE); if (!is.finite(m)) 2 else if (m < 0.1) 4 else if (m < 1) 3 else 2 }
esc_cell <- function(m, s, lt = "", d = 2, html = TRUE) {
  base <- ifelse(is.na(m), "", paste0(formatC(m, format = "f", digits = d), " \u00b1 ", formatC(s, format = "f", digits = d)))
  ifelse(is.na(lt) | !nzchar(lt) | is.na(m), base, paste0(base, if (html) paste0("<sup>", lt, "</sup>") else lt))
}
esc_pstar <- function(p) ifelse(is.na(p), "", paste0(esc_fmtp(p), esc_stars(p)))

# Combine per-response cell vectors into one wide table, then add footer rows
esc_pub_assemble <- function(row_df, cols, footer = NULL) {
  for (j in seq_along(row_df)) row_df[[j]] <- htmltools::htmlEscape(as.character(row_df[[j]]))
  out <- row_df
  for (nm in names(cols)) out[[nm]] <- cols[[nm]]
  if (!is.null(footer) && nrow(footer)) {
    pad <- as.data.frame(matrix("", nrow(footer), ncol(row_df), dimnames = list(NULL, names(row_df))), stringsAsFactors = FALSE)
    pad[[1]] <- paste0("<b>", htmltools::htmlEscape(footer$Statistic), "</b>")
    for (nm in names(cols)) pad[[nm]] <- footer[[nm]] %||% ""
    out <- rbind(out, pad)
  }
  rownames(out) <- NULL
  out
}

esc_publication_table <- function(res, spread = "sd", data = NULL, html = TRUE) {
  test <- res$test; items <- Filter(function(it) is.null(it$error), res$items)
  if (!length(items)) return(if (length(res$items)) list(error = res$items[[1]]$error) else NULL)
  sp <- if (identical(spread, "se")) "SE" else "SD"
  note_letters <- "Means within a column followed by different superscript letters differ significantly (P < 0.05)."
  switch(test,
    anova1 = {
      lev <- sort(unique(unlist(lapply(items, function(it) it$letters_A$Level))))
      cols <- lapply(items, function(it) { l <- it$letters_A[match(lev, it$letters_A$Level), ]
        esc_cell(l$Mean, l[[sp]], trimws(l$Group), esc_dec(l$Mean), html) })
      foot <- data.frame(Statistic = c("F value", "P-value", "CV (%)"))
      for (nm in names(items)) { it <- items[[nm]]; r <- it$anova[it$anova$Term == it$A, ]
        foot[[nm]] <- c(sprintf("%.2f", r$`F value`), esc_pstar(r$`Pr(>F)`), sprintf("%.2f", it$cv)) }
      list(table = esc_pub_assemble(data.frame(Treatment = lev, stringsAsFactors = FALSE), cols, foot),
           note = paste0("Values are mean \u00b1 ", sp, ". ", note_letters, " Post-hoc test: ", res$posthoc %||% "Tukey HSD", "."))
    },
    anova2 = {
      it1 <- items[[1]]; d <- it1$data
      combos <- unique(d[, c(it1$A, it1$B)]); combos <- combos[order(combos[[1]], combos[[2]]), ]
      key <- paste(combos[[1]], combos[[2]], sep = ":")
      cols <- lapply(items, function(it) {
        s <- stats::aggregate(it$data[[it$y]], list(a = it$data[[it$A]], b = it$data[[it$B]]),
                              function(v) c(m = mean(v), sd = stats::sd(v), n = length(v)))
        sk <- paste(s$a, s$b, sep = ":"); i <- match(key, sk)
        m <- s$x[i, "m"]; e <- if (sp == "SE") s$x[i, "sd"] / sqrt(s$x[i, "n"]) else s$x[i, "sd"]
        lt <- if (!is.null(it$letters_AB)) trimws(it$letters_AB$Group[match(key, it$letters_AB$Level)]) else ""
        esc_cell(m, e, lt, esc_dec(m), html) })
      terms <- c(it1$A, it1$B, paste(it1$A, "x", it1$B))
      foot <- data.frame(Statistic = c(paste0("P (", res$labels[1], ")"), paste0("P (", res$labels[2], ")"),
                                       paste0("P (", res$labels[1], " \u00d7 ", res$labels[2], ")"), "CV (%)"))
      for (nm in names(items)) { it <- items[[nm]]
        pv <- vapply(terms, function(t) { r <- it$anova$`Pr(>F)`[it$anova$Term == t]; if (length(r)) r else NA_real_ }, 0)
        foot[[nm]] <- c(esc_pstar(pv), sprintf("%.2f", it$cv)) }
      rows <- data.frame(a = as.character(combos[[1]]), b = as.character(combos[[2]]), stringsAsFactors = FALSE)
      names(rows) <- res$labels[1:2]
      rows[[1]][duplicated(rows[[1]])] <- ""
      list(table = esc_pub_assemble(rows, cols, foot),
           note = paste0("Values are mean \u00b1 ", sp, ". ", if (!is.null(it1$letters_AB)) paste("Letters compare all treatment combinations.", note_letters) else "",
                         " Post-hoc test: ", res$posthoc %||% "Tukey HSD", ". * P < 0.05, ** P < 0.01, *** P < 0.001."))
    },
    ancova = {
      lev <- sort(unique(unlist(lapply(items, function(it) as.character(it$adjusted$means[[1]])))))
      cols <- lapply(items, function(it) {
        mm <- it$adjusted$means; g <- as.character(mm[[1]]); m <- stats::setNames(mm$emmean, g)
        pm <- esc_pmat_from_pairs(g, it$adjusted$pairs$contrast, it$adjusted$pairs$p.value)
        lt <- esc_cld(m, pm); i <- match(lev, g)
        e <- mm$SE[i]; if (sp == "SD") e <- e * sqrt(as.numeric(table(it$data[[it$A]])[g[i]]))
        esc_cell(mm$emmean[i], e, lt[g][i], esc_dec(mm$emmean), html) })
      foot <- data.frame(Statistic = c("P (group)", "P (covariate)", "Slope homogeneity P"))
      for (nm in names(items)) { it <- items[[nm]]
        pg <- it$anova$`Pr(>F)`[it$anova$Term == it$A]; pc <- it$anova$`Pr(>F)`[it$anova$Term == it$covariate]
        foot[[nm]] <- c(esc_pstar(pg), esc_pstar(pc), it$slope_check$P) }
      list(table = esc_pub_assemble(data.frame(Group = lev, stringsAsFactors = FALSE), cols, foot),
           note = paste0("Values are covariate-adjusted means \u00b1 ", sp, " (model-based, estimated at the mean covariate value",
                         if (sp == "SD") "; SD = SE \u00d7 \u221an" else "", "). ", note_letters, " Tukey-adjusted comparisons."))
    },
    ttest = {
      lev <- items[[1]]$descriptives$Group
      cols <- lapply(items, function(it) {
        ds <- it$descriptives; lt <- if (nrow(ds) == 2) (if (isTRUE(it$p_t < 0.05)) c("a", "b")[rank(-ds$Mean, ties.method = "first")] else c("a", "a")) else ""
        esc_cell(ds$Mean, ds[[sp]], lt, esc_dec(ds$Mean), html) })
      foot <- data.frame(Statistic = c("t (df)", "P (t-test)", "P (non-parametric)", "Hedges' g"))
      for (nm in names(items)) { it <- items[[nm]]
        foot[[nm]] <- c(sprintf("%.3f (%.1f)", it$t_stat, it$df), esc_pstar(it$p_t), esc_pstar(it$p_np), it$effect$Value[2]) }
      list(table = esc_pub_assemble(data.frame(Group = lev, stringsAsFactors = FALSE), cols, foot),
           note = paste0("Values are mean \u00b1 ", sp, ". Different letters indicate a significant difference (P < 0.05, ", items[[1]]$tests$Test[1], ")."))
    },
    kruskal = {
      lev <- sort(unique(unlist(lapply(items, function(it) it$summary$Group))))
      cols <- lapply(items, function(it) {
        s <- it$summary; m <- stats::setNames(s$Mean, s$Group)
        pm <- esc_pmat_from_pairs(s$Group, it$dunn$Comparison, it$p_adj)
        lt <- esc_cld(stats::setNames(s$`Mean rank`, s$Group), pm); i <- match(lev, s$Group)
        esc_cell(s$Mean[i], s[[sp]][i], lt[s$Group][i], esc_dec(s$Mean), html) })
      foot <- data.frame(Statistic = c("Kruskal-Wallis chi-square", "P-value"))
      for (nm in names(items)) { it <- items[[nm]]
        foot[[nm]] <- c(sprintf("%.2f", it$test$`Chi-square`), esc_pstar(it$p_value)) }
      list(table = esc_pub_assemble(data.frame(Group = lev, stringsAsFactors = FALSE), cols, foot),
           note = paste0("Values are mean \u00b1 ", sp, ". Letters from Dunn's test on ranks (", res$padj %||% "Holm", " adjustment); different letters indicate P < 0.05."))
    },
    desc = {
      ds <- items[[1]]$desc; vars <- unique(ds$Variable); grp <- unique(ds$Group)
      cols <- stats::setNames(lapply(vars, function(v) { x <- ds[ds$Variable == v, ][match(grp, ds$Group[ds$Variable == v]), ]
        paste0(esc_cell(x$Mean, x[[sp]], "", esc_dec(x$Mean), html), " (", x$n, ")") }), vars)
      list(table = esc_pub_assemble(data.frame(Group = grp, stringsAsFactors = FALSE), cols),
           note = paste0("Values are mean \u00b1 ", sp, " (n)."))
    },
    cor = {
      ct <- items[[1]]$cor; v <- unique(c(ct$`Variable 1`, ct$`Variable 2`))
      M <- matrix("", length(v), length(v), dimnames = list(v, v)); diag(M) <- "1"
      for (i in seq_len(nrow(ct))) {
        a <- ct$`Variable 1`[i]; b <- ct$`Variable 2`[i]
        st <- if (ct$Signif[i] == "ns") "" else ct$Signif[i]
        ia <- match(c(a, b), v); M[v[max(ia)], v[min(ia)]] <- paste0(sprintf("%.3f", ct$r[i]), if (html && nzchar(st)) paste0("<sup>", st, "</sup>") else st)
      }
      list(table = cbind(Variable = v, as.data.frame(M, stringsAsFactors = FALSE), stringsAsFactors = FALSE),
           note = "Correlation coefficients (lower triangle). * P < 0.05, ** P < 0.01, *** P < 0.001.")
    },
    reg = {
      rows <- do.call(rbind, lapply(names(items), function(nm) { it <- items[[nm]]; f <- it$fit_stats
        data.frame(Response = nm, Equation = it$equation, R2 = f$Value[2], `Adjusted R2` = f$Value[3], RMSE = f$Value[4],
                   `P (model)` = f$Value[6], n = f$Value[1], check.names = FALSE) }))
      list(table = rows, note = "Least-squares regression; equation in original units.")
    },
    kin = {
      rows <- do.call(rbind, lapply(names(items), function(nm) { t <- items[[nm]]$table
        data.frame(Response = nm, Group = t$Group, Model = t$Model, C0 = sprintf("%.3f", t$C0),
                   `k ± SE` = sprintf("%.4g \u00b1 %.2g", t$k, t$`SE(k)`), beta = ifelse(is.na(t$beta), "", sprintf("%.3f", t$beta)),
                   `Half-life` = sprintf("%.3g", t$`Half-life`), R2 = sprintf("%.4f", t$R2), RMSE = sprintf("%.4g", t$RMSE),
                   AIC = sprintf("%.2f", t$AIC), Best = t$`Best (min AIC)`, check.names = FALSE) }))
      rows$Response[duplicated(rows$Response)] <- ""
      list(table = rows, note = "Non-linear least squares fits; * marks the model with the lowest AIC per group. Half-life in the time units of the data.")
    },
    NULL)
}
