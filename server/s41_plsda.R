# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s41_plsda  (sourced inside server(), local = TRUE)
# PLS-DA with fold-wise preprocessing, cross-validation, permutation test (R/chemometrics.R).

  output$da_class_ui <- renderUI({
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) return(helpText("Upload multivariate data in the sidebar."))
    cand <- names(df)[vapply(df, function(x) length(unique(stats::na.omit(x))) %in% 2:30, logical(1))]
    sel <- if (!is.null(input$multi_class_col) && input$multi_class_col %in% cand) input$multi_class_col else cand[1]
    selectInput("da_class", "Class column:", cand, sel)
  })
  da_xmatrix <- function(df) {
    rng <- input$multi_var_range; req(rng)
    Xdf <- df[, seq(rng[1], rng[2]), drop = FALSE]
    X <- as.matrix(as.data.frame(lapply(Xdf, function(z) suppressWarnings(as.numeric(z)))))
    X <- X[, colSums(is.na(X)) == 0, drop = FALSE]
    validate(need(ncol(X) >= 2, "Select at least two complete numeric X variables in the sidebar range."))
    X
  }

  da_result <- eventReactive(input$da_run, {
    df <- dat_multi()
    validate(need(isTRUE(input$show_da_section) && !is.null(input$da_class),
                  "Tick \"PLS-DA \u2013 class\" in the multivariate sidebar and choose the class column, then click PLS-DA."))
    X <- da_xmatrix(df)
    validate(need(!input$da_class %in% colnames(X), "The class column is inside the X-variable range. Narrow the range in the sidebar."))
    cls <- factor(df[[input$da_class]]); ok <- !is.na(cls); X <- X[ok, , drop = FALSE]; cls <- droplevels(cls[ok])
    id <- if (!is.null(input$multi_id_col) && nzchar(input$multi_id_col) && input$multi_id_col %in% names(df))
      as.character(df[[input$multi_id_col]][ok]) else as.character(which(ok))
    validate(need(nlevels(cls) >= 2, "The class column needs at least two classes."),
             need(min(table(cls)) >= 2, "Every class needs at least two samples."))
    n <- nrow(X)
    # calibration / prediction sets, CV and LV come from the options shared with PLSR
    use_sets <- !isFALSE(input$show_plsr_sets_section)
    split <- if (!use_sets) "none" else input$plsr_split_type %||% "random"
    if (split == "random" && (input$plsr_cal_prop %||% 1) < 1) {
      set.seed(123)
      cal <- unlist(lapply(split(seq_len(n), cls), function(ix) ix[sample.int(length(ix), max(2, round(length(ix) * input$plsr_cal_prop)))]))
      cal <- sort(cal)
    } else if (split == "column") {
      req(input$plsr_group_col); g <- factor(df[[input$plsr_group_col]][ok])
      validate(need(nlevels(g) == 2, "The Cal / Pred column must have exactly two levels."))
      cal <- which(g == levels(g)[1])
    } else cal <- seq_len(n)
    pred <- setdiff(seq_len(n), cal)
    prep_args <- list(spectral = input$prep_spectral, scaling = input$prep_scaling, centering = input$prep_centering,
                      sg_width = input$sg_width %||% 11, sg_order = input$sg_order %||% 2, sg_deriv = input$sg_deriv %||% 0)
    cv_type <- input$plsr_cv_type %||% "vb"
    if (isFALSE(input$show_plsr_cv_section) || cv_type == "none") cv_type <- "vb"   # PLS-DA always needs CV to choose the LV
    n_perm <- input$da_perm %||% 0
    t0 <- Sys.time()
    r <- withProgress(message = "PLS-DA", detail = if (n_perm > 0) "running permutation tests" else NULL, value = 0.3,
      tryCatch(esc_plsda(X[cal, , drop = FALSE], cls[cal], if (length(pred)) X[pred, , drop = FALSE], if (length(pred)) cls[pred],
                         prep_args = prep_args, max_lv = 15,
                         cv = list(type = cv_type, k = max(2, min(25, input$plsr_cv_splits %||% 5)),
                                   thickness = max(1, min(20, input$plsr_cv_thickness %||% 1)), seed = 123),
                         lv = if ((input$plsr_lv_user %||% 0) > 0) input$plsr_lv_user else NULL, n_perm = n_perm),
               error = function(e) { validate(need(FALSE, paste("PLS-DA:", conditionMessage(e)))) }))
    r$id_cal <- id[cal]; r$id_pred <- id[pred]; r$seconds <- as.numeric(Sys.time() - t0, units = "secs")
    r$prep_label <- esc_prep_label(input$prep_spectral, input$prep_scaling, input$prep_centering)
    state$current_analysis <- "plsda"
    r
  })

  da_tables <- function(r) {
    f4 <- function(x) sprintf("%.4f", x)
    over <- function(m, set) if (is.null(m)) NULL else data.frame(Set = set, n = sum(m$confusion), Accuracy = f4(m$accuracy),
      `Error rate` = f4(m$error_rate), `Cohen's kappa` = f4(m$kappa), check.names = FALSE)
    cmtab <- function(m) { if (is.null(m)) return(NULL); x <- as.data.frame.matrix(m$confusion); cbind(Observed = rownames(x), x) }
    pc <- function(m, set) if (is.null(m)) NULL else cbind(Set = set, m$per_class)
    imp <- data.frame(Variable = names(r$vip), VIP = f4(r$vip), check.names = FALSE)
    imp <- cbind(imp, setNames(as.data.frame(apply(r$coef, 2, f4)), paste0("b (", colnames(r$coef), ")")),
                 setNames(as.data.frame(apply(r$sr, 2, f4)), paste0("SR (", colnames(r$sr), ")")))
    ev <- data.frame(LV = seq_along(r$expl_var), `X variance (%)` = sprintf("%.2f", r$expl_var),
                     `Cumulative (%)` = sprintf("%.2f", cumsum(r$expl_var)), check.names = FALSE)
    err <- r$error; err$Error_cal <- f4(err$Error_cal); err$Error_CV <- f4(err$Error_CV)
    names(err) <- c("LV", "Error (calibration)", "Error (cross-validation)")
    pcls <- do.call(rbind, list(pc(r$metrics_cal, "Calibration"), pc(r$metrics_cv, "Cross-validation"), pc(r$metrics_pred, "Prediction")))
    for (j in 3:ncol(pcls)) pcls[[j]] <- f4(pcls[[j]])
    list(
      list(heading = "Classification error vs number of latent variables", table = err,
           note = sprintf("LV used: %d. Preprocessing (%s) re-fitted inside each CV fold. Class = arg max of predicted dummy Y.", r$lv, r$prep_label)),
      list(heading = "Overall performance", table = do.call(rbind, list(over(r$metrics_cal, "Calibration"),
                                                                       over(r$metrics_cv, "Cross-validation"), over(r$metrics_pred, "Prediction")))),
      list(heading = "Per-class performance", table = pcls),
      list(heading = "Confusion matrix: cross-validation", table = cmtab(r$metrics_cv)),
      list(heading = "Confusion matrix: calibration", table = cmtab(r$metrics_cal)),
      if (!is.null(r$metrics_pred)) list(heading = "Confusion matrix: prediction set", table = cmtab(r$metrics_pred)),
      list(heading = "Area under ROC curve (CV, one-vs-rest)", table = data.frame(Class = names(r$auc), AUC = f4(r$auc))),
      if (!is.null(r$permutation)) list(heading = "Permutation test", table = data.frame(
        Permutations = length(r$permutation$errors), `Observed CV error` = f4(r$permutation$observed),
        `Mean permuted error` = f4(mean(r$permutation$errors)), P = esc_fmtp(r$permutation$p_value), check.names = FALSE),
        note = "P = (number of permuted CV errors <= observed + 1) / (permutations + 1)."),
      list(heading = "Explained X variance", table = ev),
      list(heading = "Variable importance: VIP, regression coefficients and selectivity ratio", table = imp))
  }

  output$da_results <- renderUI({
    r <- da_result()
    tabs <- Filter(Negate(is.null), da_tables(r))
    tagList(tags$small(sprintf("Computed in %.2f s.", r$seconds)),
      lapply(tabs, function(s) {
        tb <- s$table; long <- nrow(tb) > 25
        tagList(h5(strong(s$heading)),
                div(style = paste0("overflow:auto;", if (long) " max-height:320px;"), HTML(esc_df_to_html(tb))),
                if (!is.null(s$note)) tags$small(s$note))
      }))
  })

  da_plot_obj <- reactive({
    r <- da_result(); ty <- input$da_plot
    vars <- names(r$vip); bar <- identical(input$pca_load_profile_type, "bar")   # line / bar profile choice
    switch(ty,
      prob = esc_plot_da_prob(r), xvar = esc_plot_da_xvar(r), weights = esc_plot_pls_weights(r$fit, 1:min(2, r$max_lv), bar = bar),
      err = { d <- tidyr::pivot_longer(r$error, -LV, names_to = "Set", values_to = "Error")
        ggplot(d, aes(LV, Error, colour = Set, shape = Set)) + geom_line() + geom_point(size = 2.5) +
          geom_vline(xintercept = r$lv, linetype = 2, colour = "grey50") + scale_x_continuous(breaks = r$error$LV) +
          labs(x = "Latent variables", y = "Classification error rate") + esc_theme() },
      scores = {
        idx <- as.integer(strsplit(input$pca_pc_pair %||% "1_2", "_")[[1]]); a <- idx[1]; b <- idx[2]
        validate(need(max(a, b) <= ncol(r$scores), "Choose LVs within the fitted number."))
        Tm <- rbind(r$scores, if (!is.null(r$scores_pred)) r$scores_pred)
        esc_plot_scores(Tm, a, b, Class = c(as.character(r$cls_cal), if (!is.null(r$scores_pred)) as.character(r$cls_pred)),
                        ID = c(r$id_cal, if (!is.null(r$scores_pred)) r$id_pred), expl = r$expl_var,
                        overlays = input$pca_scores_overlays %||% character(0), show_id = isTRUE(input$pca_show_id),
                        cos2 = isTRUE(input$pca_color_cos2), class_title = input$da_class %||% "Class",
                        Set = c(rep("Calibration", nrow(r$scores)), if (!is.null(r$scores_pred)) rep("Prediction", nrow(r$scores_pred)))) },
      yhat = {
        Y <- if (!all(is.na(r$Yhat_cv))) r$Yhat_cv else r$Yhat_cal
        d <- data.frame(Index = seq_len(nrow(Y)), True = r$cls_cal, Y)
        d <- tidyr::pivot_longer(d, -c(Index, True), names_to = "Model", values_to = "Pred")
        ggplot(d, aes(Index, Pred, colour = True)) + geom_hline(yintercept = 0.5, linetype = 2) + geom_point(size = 1.8) +
          facet_wrap(~Model) + labs(x = "Sample", y = "Predicted class value (CV)", colour = "True class") + esc_theme() },
      cm = {
        m <- if (!is.null(r$metrics_cv)) r$metrics_cv$confusion else r$metrics_cal$confusion
        d <- as.data.frame(m); d$Pct <- d$Freq / ave(d$Freq, d$Observed, FUN = sum) * 100
        ggplot(d, aes(Predicted, Observed, fill = Pct)) + geom_tile(colour = "white") +
          geom_text(aes(label = sprintf("%d\n(%.0f%%)", Freq, Pct)), size = 3.8) +
          scale_fill_gradient(low = "white", high = "#2b8cbe", limits = c(0, 100)) + labs(fill = "% of row") +
          scale_y_discrete(limits = rev) + esc_theme() },
      roc = ggplot(r$roc, aes(FPR, TPR, colour = Class)) + geom_abline(linetype = 2, colour = "grey60") + geom_step(linewidth = 0.9) +
        coord_equal() + labs(x = "False positive rate (1 - specificity)", y = "True positive rate (sensitivity)",
                             caption = paste(sprintf("AUC %s = %.3f", names(r$auc), r$auc), collapse = "; ")) + esc_theme(),
      vip = { d <- data.frame(x = esc_var_index(vars), VIP = r$vip)
        ggplot(d, aes(x, VIP)) + geom_hline(yintercept = 1, linetype = 2, colour = "red") +
          (if (bar) geom_col(fill = "#0072B2") else geom_line(colour = "#0072B2")) +
          labs(x = esc_var_xlab(vars), y = "VIP score") + esc_theme() },
      coef = { d <- data.frame(x = esc_var_index(vars), r$coef, check.names = FALSE)
        d <- tidyr::pivot_longer(d, -x, names_to = "Class", values_to = "b")
        ggplot(d, aes(x, b, colour = Class)) + geom_hline(yintercept = 0, colour = "grey60") +
          (if (bar) geom_col(aes(fill = Class), position = "dodge", colour = NA) else geom_line()) +
          labs(x = esc_var_xlab(vars), y = "Regression coefficient") + esc_theme() },
      sr = { d <- data.frame(x = esc_var_index(vars), r$sr, check.names = FALSE)
        d <- tidyr::pivot_longer(d, -x, names_to = "Class", values_to = "SR")
        ggplot(d, aes(x, SR, colour = Class)) + (if (bar) geom_col(aes(fill = Class), position = "dodge", colour = NA) else geom_line()) +
          labs(x = esc_var_xlab(vars), y = "Selectivity ratio") + esc_theme() },
      qt = { d <- data.frame(T2 = r$qt$T2, Q = r$qt$Q, Class = r$cls_cal, ID = r$id_cal)
        p <- ggplot(d, aes(T2, Q, colour = Class)) + geom_point(size = 2.4) +
          geom_vline(xintercept = r$qt$T2_lim, linetype = 2) + geom_hline(yintercept = r$qt$Q_lim, linetype = 2) +
          labs(x = "Hotelling T\u00b2", y = "Q residuals", caption = "Dashed lines: 95% limits") + esc_theme()
        if (isTRUE(input$pca_show_id)) p <- p + ggrepel::geom_text_repel(aes(label = ID), size = 3, show.legend = FALSE)
        p },
      perm = { validate(need(!is.null(r$permutation), "Set 'Permutation tests' above 0 and run again."))
        ggplot(data.frame(e = r$permutation$errors), aes(e)) + geom_histogram(bins = 25, fill = "grey70", colour = "black") +
          geom_vline(xintercept = r$permutation$observed, colour = "red", linewidth = 1) +
          labs(x = "CV error with permuted class labels", y = "Count",
               caption = sprintf("Red line: observed CV error; P = %s", esc_fmtp(r$permutation$p_value))) + esc_theme() })
  })
  output$da_plot_out <- esc_renderPlot({ da_plot_obj() })

  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) local({
    f <- fmt
    output[[paste0("dl_plsda_", f)]] <- downloadHandler(
      filename = function() paste0("ESCAP_PLSDA_", input$da_plot, ".", f),
      content = function(file) {
        p <- esc_apply_style(da_plot_obj(), esc_style_now())
        esc_open_device(file, f, input$dl_plsda_w %||% 8, input$dl_plsda_h %||% 6, input$dl_plsda_dpi %||% 600)
        on.exit(grDevices::dev.off(), add = TRUE); esc_draw(p)
      }, contentType = esc_mime[[f]])
  })

  register_report("rep_plsda", function() {
    r <- da_result()
    list(title = "PLS-DA classification", settings = c(Class = input$da_class, Preprocessing = r$prep_label,
         `Cross-validation` = input$plsr_cv_type %||% "vb", `LV` = r$lv), sections = Filter(Negate(is.null), da_tables(r)))
  })
