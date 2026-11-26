# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s10_grouped_bar  (sourced inside server(), local = TRUE)

  linear_group_data <- reactive({
    df <- dat_raw()
    req(uni_factors(), uni_resp())
    
    validate(
      need(
        length(uni_factors()) >= 2,
        "You may swap the factors according to your requirements"
      )
    )
    
    x_col <- uni_factors()[1] # first factor: x\u2011axis (dose)
    g_col <- uni_factors()[2] # second factor: group (supp)
    
    validate(
      need(
        x_col %in% names(df),
        "First factor not found in data."
      ),
      need(
        g_col %in% names(df),
        "Second factor not found in data."
      )
    )
    
    # response = first selected numeric response column
    resp_candidates <- uni_resp()
    resp_candidates <- resp_candidates[resp_candidates %in% names(df)]
    is_num <- sapply(df[resp_candidates], is.numeric)
    resp_candidates <- resp_candidates[is_num]
    validate(
      need(
        length(resp_candidates) >= 1,
        "Grouped line/bar: select at least one numeric response column."
      )
    )
    
    # NEW: if user chose a specific variable for grouped bar, use it
    if (!is.null(input$linear_group_resp) &&
        input$linear_group_resp %in% resp_candidates) {
      y_col <- input$linear_group_resp
    } else {
      y_col <- resp_candidates[1]
    }
    
    d <- df[, c(y_col, g_col, x_col), drop = FALSE]
    colnames(d) <- c("len", "supp", "dose")
    
    d$len <- suppressWarnings(as.numeric(d$len))
    d$supp <- as.factor(d$supp)
    d$dose <- as.factor(d$dose)
    
    validate(
      need(
        !all(is.na(d$len)),
        "The selected response column must be numeric."
      )
    )
    
    attr(d, "y_name") <- y_col
    attr(d, "x_name") <- x_col
    
    d
  })
  
  
  linear_group_plots <- reactive({
    d <- linear_group_data()
    df <- dat_raw()
    
    n_groups <- nlevels(d$supp)
    base_cols <- RColorBrewer::brewer.pal(12, "Paired")
    pal_dyn <- colorRampPalette(base_cols)(n_groups)
    
    
    # --- Recover original header names from CSV header line ------------------
    orig_names_vec <- NULL
    orig_names_vec <- tryCatch(uni_orig_names(), error = function(e) NULL)
    
    # x-axis label: use original header for the x factor (dose)
    x_name <- attr(d, "x_name") # set in linear_group_data()
    if (!is.null(orig_names_vec) &&
        !is.null(x_name) &&
        x_name %in% names(orig_names_vec)) {
      x_lab <- orig_names_vec[[x_name]] # exact header (e.g. "RH (%)")
    } else if (!is.null(x_name)) {
      x_lab <- x_name # fallback: sanitized name
    } else {
      x_lab <- "Factor"
    }
    
    # y-axis label: use original header for response column if possible
    y_name <- attr(d, "y_name")
    if (!is.null(orig_names_vec) &&
        !is.null(y_name) &&
        y_name %in% names(orig_names_vec)) {
      y_lab <- orig_names_vec[[y_name]]
    } else if (!is.null(y_name)) {
      y_lab <- y_name
    } else {
      y_lab <- "Response"
    }
    
    # dynamic palette for any number of supp levels
    n_groups <- nlevels(d$supp)
    base_cols <- RColorBrewer::brewer.pal(12, "Paired")
    pal_dyn <- colorRampPalette(base_cols)(n_groups)
    
    
    # Summary by dose, supp
    df_sum <- d %>%
      dplyr::group_by(dose, supp) %>%
      dplyr::summarise(
        sd      = sd(len, na.rm = TRUE),
        len     = mean(len, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 1) Line + error bars (mean \u00b1 SD)
    p1 <- ggplot(df_sum, aes(x = dose, y = len)) +
      geom_line(aes(linetype = supp, group = supp)) +
      geom_point() +
      geom_errorbar(
        aes(ymin = len - sd, ymax = len + sd, group = supp),
        width = 0.2
      ) +
      scale_linetype_discrete(name = NULL) +
      ggpubr::theme_pubclean() +
      ggtitle("Plot 1: Line + error bars (mean \u00b1 SD)") +
      labs(x = x_lab, y = y_lab)+ transparent_legend
    
    
    # 2) Bar + upper error bars
    p2 <- ggplot(df_sum, aes(x = dose, y = len)) +
      geom_bar(
        aes(fill = supp),
        stat = "identity",
        position = position_dodge(0.8), width = 0.7
      ) +
      geom_errorbar(
        aes(ymin = len, ymax = len + sd, group = supp),
        width    = 0.2,
        position = position_dodge(0.8)
      ) +
      scale_fill_brewer(palette = "Dark2", name = NULL) + # <- change here
      ggpubr::theme_pubclean() +
      ggtitle("Plot 2: Bar + upper error bars") +
      labs(x = x_lab, y = y_lab)+ transparent_legend
    
    
    # 3) ggline with mean \u00b1 SD + jitter
    # 3) ggline with mean \u00b1 SD + jitter
    p3 <- ggpubr::ggline(
      d,
      x = "dose", y = "len",
      add = c("mean_sd", "jitter"),
      color = "supp",
      palette = pal_dyn
    ) +
      ggtitle("Plot 3: ggline (mean \u00b1 SD + jitter)") +
      labs(
        x     = x_lab,
        y     = y_lab,
        color = NULL
      )+ transparent_legend
    
    
    # 4) ggbarplot with mean \u00b1 SE + jitter
    p4 <- ggpubr::ggbarplot(
      d,
      x = "dose", y = "len",
      add = c("mean_se", "jitter"),
      color = "supp",
      palette = pal_dyn,
      position = position_dodge(0.8)
    ) +
      ggtitle("Plot 4: ggbarplot (mean \u00b1 SE + jitter)") +
      labs(
        x     = x_lab,
        y     = y_lab,
        color = NULL
      )+ transparent_legend
    
    
    # 5) Box + jitter, one-sample test vs 0 per group + between-group brackets
    lvls <- levels(d$dose)
    if (length(lvls) >= 2) {
      my_comparisons <- combn(lvls, 2, simplify = FALSE)
    } else {
      my_comparisons <- list()
    }
    
    # dynamic palette for doses
    dose_cols_fill  <- RColorBrewer::brewer.pal(max(3, length(lvls)), "Set2")[seq_along(lvls)]
    names(dose_cols_fill) <- lvls
    dose_cols_point <- RColorBrewer::brewer.pal(max(3, length(lvls)), "Dark2")[seq_along(lvls)]
    names(dose_cols_point) <- lvls
    
    p5 <- ggplot(d, aes(x = dose, y = len, fill = dose, colour = dose)) +
      # horizontal reference at 0 (as in your example)
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
      # boxplot
      geom_boxplot(
        outlier.shape = NA,
        alpha  = 0.5,
        colour = "black",
        linewidth = 0.5,
        width  = 0.6
      ) +
      # jittered points
      geom_jitter(
        width = 0.12, size = 2.0, alpha = 0.8, stroke = 0
      ) +
      # one-sample test vs 0 per dose (t.test; change to wilcox.test if you prefer)
      stat_summary(
        fun.data = function(x) {
          pv <- tryCatch(t.test(x)$p.value, error = function(e) NA_real_)
          stars <- ifelse(is.na(pv), "",
                          ifelse(pv < 0.001, "***",
                                 ifelse(pv < 0.01,  "**",
                                        ifelse(pv < 0.05,  "*", "ns"))))
          data.frame(
            y     = min(x, na.rm = TRUE) - 0.07 * diff(range(d$len, na.rm = TRUE)),
            label = stars
          )
        },
        geom = "text", size = 4, colour = "black"
      ) +
      # between-dose brackets (change method if needed)
      ggpubr::stat_compare_means(
        comparisons = my_comparisons,
        method       = "wilcox.test",
        label        = "p.format",
        bracket.size = 0.4,
        tip.length   = 0.01,
        size         = 3.2,
        vjust        = -0.3
      ) +
      scale_fill_manual(values = dose_cols_fill) +
      scale_colour_manual(values = dose_cols_point) +
      labs(
        x = x_lab,
        y = y_lab,
        fill  = NULL,
        colour = NULL,
        title = "Plot 5: Box + jitter with within- and between-group tests"
      ) +
      theme_classic(base_size = 11) +
      theme(
        legend.position = "none",
        axis.title      = element_text(size = 11),
        axis.text       = element_text(size = 10)
      )
    
    
    
    list(p1, p2, p3, p4, p5)
  })
  
  
  # Preprocessing graphs (Graphs -> Preprocessing sub\u2011tab)
