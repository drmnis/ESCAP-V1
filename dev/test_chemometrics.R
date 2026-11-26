# ESCAP dev test: chemometrics engine vs. reference implementations
out <- "D:/API_R/dev/test_chemometrics_log.txt"
zz <- file(out, open = "wt"); sink(zz); sink(zz, type = "message")
res <- tryCatch({
  setwd("D:/API_R/ESCAP")
  suppressPackageStartupMessages({ library(pls); library(prospectr) })
  source("R/chemometrics.R")
  ok <- function(name, cond) cat(sprintf("%-55s %s\n", name, if (isTRUE(cond)) "PASS" else paste("FAIL", format(cond))))
  data(gasoline, package = "pls")
  X <- unclass(gasoline$NIR); y <- gasoline$octane
  cat("gasoline:", dim(X), "\n")

  # 1. predictions equal pls::plsr
  r <- esc_plsr(X, y, max_lv = 8, cv = list(type = "none"), lv = 5)
  ref <- plsr(y ~ X, ncomp = 8, data = data.frame(y = y, X = I(X)))
  ok("1 fitted values equal pls::plsr (LV5)", isTRUE(all.equal(as.numeric(r$yhat_cal[, 5]), as.numeric(fitted(ref)[, 1, 5]))))
  ok("1 regression vector equals coef() (LV5)", isTRUE(all.equal(as.numeric(r$coef), as.numeric(coef(ref, ncomp = 5)))))

  # 2. VIP vs Mevik reference formula
  VIPref <- function(object) {
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  }
  vr <- VIPref(ref)[5, ]
  ok("2 VIP equals reference (Mevik) formula", isTRUE(all.equal(as.numeric(r$vip), as.numeric(vr))))
  ok("2 mean(VIP^2) == 1", isTRUE(all.equal(mean(r$vip^2), 1)))
  # old ESCAP VIP (as in app.R) for comparison
  W <- ref$loading.weights[, 1:5]; Tt <- ref$scores[, 1:5]; cf <- coef(ref, ncomp = 1:5, intercept = FALSE)
  b_old <- cf[, 1, 1:5]
  SSY <- colSums(Tt^2) * colSums(b_old^2)
  vip_old <- sqrt(nrow(W) * (W^2 %*% SSY / sum(SSY)))
  cat("   old app VIP: max abs diff from correct =", signif(max(abs(vip_old - vr)), 3),
      " rank correlation =", round(cor(as.numeric(vip_old), as.numeric(vr), method = "spearman"), 3), "\n")

  # 3. CV equals pls interleaved CV (no preprocessing)
  r10 <- esc_plsr(X, y, max_lv = 8, cv = list(type = "vb", k = 10, thickness = 1))
  refcv <- plsr(y ~ X, ncomp = 8, data = data.frame(y = y, X = I(X)), validation = "CV", segments = 10, segment.type = "interleaved")
  rmsecv_ref <- sqrt(colMeans((y - refcv$validation$pred[, 1, ])^2))
  ok("3 RMSECV equals pls (10 interleaved segments)", isTRUE(all.equal(as.numeric(r10$perf$RMSECV), as.numeric(rmsecv_ref))))
  print(round(r10$perf[, c("LV", "RMSEC", "RMSECV", "R2C", "R2CV")], 4))
  cat("   LV chosen automatically:", r10$lv, "\n")

  # 4. Preprocessing checks
  Xsg <- esc_preprocess(X, spectral = "sg", sg_width = 11, sg_order = 2, sg_deriv = 1)
  ok("4 SG keeps variable names", !is.null(colnames(Xsg)) && ncol(Xsg) == ncol(X) - 10)
  Xa <- esc_preprocess(X, scaling = "auto")
  ok("4 autoscaling: mean 0, sd 1", isTRUE(all.equal(unname(apply(Xa, 2, sd)), rep(1, ncol(X)))) && max(abs(colMeans(Xa))) < 1e-10)
  Xm <- esc_preprocess(X, scaling = "maxmin")
  ok("4 max-min scaling within [-1, 1]", isTRUE(all.equal(range(Xm), c(-1, 1))))
  Xs <- esc_preprocess(X, spectral = "snv")
  ok("4 SNV rows mean 0 sd 1", max(abs(rowMeans(Xs))) < 1e-10 && isTRUE(all.equal(unname(apply(Xs, 1, sd)), rep(1, nrow(X)))))
  Xmsc <- esc_preprocess(X, spectral = "msc")
  Xmsc_ref <- prospectr::msc(X)
  ok("4 MSC equals prospectr::msc", isTRUE(all.equal(unname(Xmsc), unname(as.matrix(Xmsc_ref)), tolerance = 1e-6)))
  par <- esc_prep_fit(X[1:40, ], spectral = "snv", scaling = "pareto", centering = "mc")
  ok("4 apply() on new data uses calibration parameters", isTRUE(all.equal(esc_prep_apply(par, X[1:40, ]), esc_preprocess(X[1:40, ], spectral = "snv", scaling = "pareto", centering = "mc"), check.attributes = FALSE)))

  # 5. Q residuals: correct (centred) vs old app (uncentred X)
  rq <- esc_plsr(X, y, max_lv = 8, cv = list(type = "none"), lv = 3)
  Tq <- scores(ref)[, 1:3]; Pq <- loadings(ref)[, 1:3]
  Q_old <- rowSums((X - Tq %*% t(Pq))^2)
  Q_ref <- rowSums(residuals_X <- (sweep(X, 2, colMeans(X)) - Tq %*% t(Pq))^2)
  ok("5 Q residuals equal centred reference", isTRUE(all.equal(as.numeric(rq$qt$Q), as.numeric(Q_ref))))
  cat("   old app Q / correct Q (median ratio):", signif(median(Q_old / Q_ref), 4), "\n")
  cat("   Q limit:", signif(rq$qt$Q_lim, 4), " T2 limit:", signif(rq$qt$T2_lim, 4), "\n")

  # 6. Selectivity ratio sanity: target-projected + residual = centred X
  Xc <- sweep(X, 2, colMeans(X)); b <- rq$coef
  sr <- esc_selectivity_ratio(Xc, b)
  ok("6 SR finite and positive", all(is.finite(sr)) && all(sr >= 0))
  cat("   top 5 SR variables:", paste(names(sort(sr, decreasing = TRUE))[1:5], collapse = ", "), "\n")

  # 7. Calibration / prediction split with fold-wise preprocessing
  set.seed(1); idx <- sample(60, 45)
  rp <- esc_plsr(X[idx, ], y[idx], X[-idx, ], y[-idx],
                 prep_args = list(spectral = "sg", sg_width = 15, sg_order = 2, sg_deriv = 1, scaling = "none", centering = "mc"),
                 max_lv = 10, cv = list(type = "vb", k = 5, thickness = 1))
  print(round(rp$perf[1:6, c("LV", "RMSEC", "RMSECV", "RMSEP", "R2P", "RPD_P")], 3))

  # 8. PLS-DA on iris
  t0 <- Sys.time()
  pd <- esc_plsda(as.matrix(iris[, 1:4]), iris$Species, prep_args = list(scaling = "auto"),
                  max_lv = 4, cv = list(type = "rand", k = 10), n_perm = 50)
  cat("8 PLS-DA iris: LV =", pd$lv, " CV error =", round(pd$error$Error_CV[pd$lv], 3),
      " kappa(CV) =", round(pd$metrics_cv$kappa, 3), " perm p =", signif(pd$permutation$p_value, 3),
      " time =", round(as.numeric(Sys.time() - t0, units = "secs"), 2), "s\n")
  print(pd$metrics_cv$confusion); print(round(pd$auc, 3))
  ok("8 PLS-DA VIP mean square 1", isTRUE(all.equal(mean(pd$vip^2), 1)))

  # 9. timing of a full PLSR with LOO
  t0 <- Sys.time(); invisible(esc_plsr(X, y, max_lv = 10, cv = list(type = "loo"),
     prep_args = list(spectral = "snv", centering = "mc")))
  cat("9 PLSR LOO (60 x 401, 10 LV, SNV):", round(as.numeric(Sys.time() - t0, units = "secs"), 2), "s\n")
  "done"
}, error = function(e) paste("ERROR:", conditionMessage(e), "\n", paste(deparse(conditionCall(e)), collapse = "")))
cat(res, "\n")
sink(type = "message"); sink(); close(zz)
cat("TEST CHEMOMETRICS FINISHED\n")
