# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s08_density_hist  (sourced inside server(), local = TRUE)

  density_hist_plots <- reactive({
    req(uni_resp())
    df <- dat_raw()
    
    vars <- uni_resp()
    vars <- vars[vars %in% names(df)]
    validate(
      need(length(vars) >= 1, "Select at least one numeric response variable.")
    )
    
    # Coerce to numeric and drop NA
    num_list <- lapply(vars, function(v) {
      x_vec <- suppressWarnings(as.numeric(df[[v]]))
      x_vec[!is.na(x_vec)]
    })
    
    for (i in seq_along(vars)) {
      validate(
        need(length(num_list[[i]]) > 1, paste("Variable", vars[i], "has no numeric data."))
      )
    }
    
    # ---- BINS (used in all modes) ----
    n_total <- sum(vapply(num_list, length, integer(1)))
    if (!isTRUE(input$dens_hist_use_bins)) {
      bins <- ceiling(log2(max(n_total, 2)) + 1)  # Sturges
    } else {
      bins <- input$dens_hist_bins %||% 30
      if (is.na(bins) || bins < 1) bins <- 30
    }
    
    dens_adjust <- scales::rescale(
      bins,
      to   = c(0.5, 2.0),
      from = c(5, 200)
    )
    dens_adjust <- max(0.1, dens_adjust)
    
    # ------------------------------------------------------------------
    # FACET MODE: ONE plot, densities OR histograms, with 4 preferred colors
    #            (same quartile-binning logic as non-facet mode)
    # ------------------------------------------------------------------
    if (isTRUE(input$dens_hist_use_facet)) {
      facet_type <- input$dens_hist_facet_type %||% "density"
      
      # Build a long data frame with quartile bins per variable
      df_all_list <- lapply(seq_along(vars), function(i) {
        v     <- vars[i]
        x_vec <- num_list[[i]]
        
        # Quartile bins for this variable
        bin_factor <- as.factor(ntile(x_vec, 4))
        data.frame(
          Variable = v,
          x        = x_vec,
          bin      = bin_factor,
          stringsAsFactors = FALSE
        )
      })
      
      df_all <- do.call(rbind, df_all_list)
      df_all$Variable <- factor(df_all$Variable, levels = vars)
      df_all$bin      <- factor(df_all$bin)  # 1\u20134
      
      # Your 4 preferred colors, exactly as in non-facet mode
      base_pal <- c("#2e00fa", "#a000bc", "#ca0086", "#e40058")
      
      if (facet_type == "hist") {
        p <- ggplot(df_all, aes(x = x)) +
          geom_histogram(
            aes(fill = bin, color = bin),  # border same mapping as fill
            alpha = 0.7,
            bins  = bins,
            linewidth = 0.1                 # thin border
          ) +
          scale_fill_manual(values = base_pal, guide = "none") +
          scale_color_manual(values = base_pal, guide = "none") +
          facet_wrap(~ Variable, scales = "free_x") +
          labs(x = NULL, y = "Count") +
          theme_minimal(base_size = 10) +
          theme(
            strip.text = element_text(size = 8),
            axis.text  = element_text(size = 7)
          )
      } else { # density
        p <- ggplot(df_all, aes(x = x)) +
          geom_density(
            aes(fill = bin),
            color  = NA,
            alpha  = 0.7,
            adjust = dens_adjust
          ) +
          scale_fill_manual(values = base_pal, guide = "none") +
          facet_wrap(~ Variable, scales = "free_x") +
          labs(x = NULL, y = "Density") +
          theme_minimal(base_size = 10) +
          theme(
            strip.text = element_text(size = 8),
            axis.text  = element_text(size = 7)
          )
      }
      
      return(list(p))
    }
    
    # ------------------------------------------------------------------
    # ORIGINAL PER\u2011VARIABLE MODE: one 2\u2011panel plot (density + hist) per variable
    # ------------------------------------------------------------------
    plots <- lapply(seq_along(vars), function(i) {
      v     <- vars[i]
      x_vec <- num_list[[i]]
      
      bin_factor <- as.factor(ntile(x_vec, 4))
      df_small   <- data.frame(x = x_vec, bin = bin_factor)
      
      base_pal <- c("#2e00fa", "#a000bc", "#ca0086", "#e40058")
      n_groups <- nlevels(bin_factor)
      pal      <- colorRampPalette(base_pal)(n_groups)
      
      p_density <- df_small %>%
        ggdensity(
          x       = "x",
          add     = "mean",
          rug     = FALSE,
          color   = "bin",
          fill    = "bin",
          xlab    = v,
          ylab    = "Density",
          palette = pal,
          adjust  = dens_adjust
        )
      
      p_hist <- df_small %>%
        gghistogram(
          x       = "x",
          add     = "mean",
          rug     = FALSE,
          color   = "bin",   # border uses same mapping as fill
          fill    = "bin",
          xlab    = v,
          ylab    = "Count",
          bins    = bins,
          palette = pal
        ) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::guides(color = "none") +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::theme(
          panel.border = ggplot2::element_blank()
        )
      
      
      ggpubr::ggarrange(
        p_density, p_hist,
        ncol = 2, nrow = 1,
        widths = c(1, 1)
      )
    })
    
    plots
  })
  
  
  
  
  
  
  
  
  
  # 1) Data for Simple bar plots ---------------------------------------------
  simple_bar_data <- reactive({
    df <- dat_raw()
    req(primary_factor(), uni_resp())
    
    x_col  <- primary_factor()        # treatment factor
    y_cols <- uni_resp()     # numeric responses
    
    # subset
    df_sub <- df[, c(x_col, y_cols), drop = FALSE]
    
    # coerce responses to numeric
    for (nm in y_cols) {
      df_sub[[nm]] <- suppressWarnings(as.numeric(df_sub[[nm]]))
    }
    
    # drop rows that are all NA in responses
    keep_rows <- rowSums(!is.na(df_sub[, y_cols, drop = FALSE])) > 0
    df_sub <- df_sub[keep_rows, , drop = FALSE]
    
    # treatment as factor
    df_sub[[x_col]] <- factor(df_sub[[x_col]])
    
    # summary: mean, sd, se, n per (treatment, response)
    summ <- df_sub |>
      dplyr::group_by(.data[[x_col]]) |>
      dplyr::summarise(
        dplyr::across(
          all_of(y_cols),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd   = ~ sd(.x,   na.rm = TRUE),
            n    = ~ sum(!is.na(.x))
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
    
    # compute se = sd / sqrt(n)
    for (nm in y_cols) {
      sd_name <- paste0(nm, "_sd")
      n_name  <- paste0(nm, "_n")
      se_name <- paste0(nm, "_se")
      summ[[se_name]] <- summ[[sd_name]] / sqrt(pmax(summ[[n_name]], 1))
    }
    
    # Tukey letters per response, when possible
    # (\u22653 obs per treatment, \u22652 treatment levels)
    tukey_letters <- list()
    
    for (y in y_cols) {
      this_y <- df_sub[[y]]
      this_f <- df_sub[[x_col]]
      
      # check: at least 2 levels, at least one level with n>=3
      tbl_n <- tapply(this_y, this_f, function(z) sum(!is.na(z)))
      if (length(tbl_n) >= 2 && any(tbl_n >= 3)) {
        tuk <- tryCatch(
          {
            lv_f <- levels(factor(this_f)); this_code <- factor(paste0("L", as.integer(factor(this_f))), levels = paste0("L", seq_along(lv_f)))
            fit <- aov(this_y ~ this_code)
            tk <- agricolae::HSD.test(fit, "this_code", group = TRUE)
            rownames(tk$groups) <- lv_f[as.integer(sub("^L", "", rownames(tk$groups)))]   # agricolae mangles names with "&" etc.
            tk
          },
          error = function(e) NULL
        )
        if (!is.null(tuk)) {
          g <- tuk$groups
          g$Treatment <- rownames(g)
          rownames(g) <- NULL
          tukey_letters[[y]] <- g[, c("Treatment", "groups")]
        } else {
          tukey_letters[[y]] <- NULL
        }
      } else {
        tukey_letters[[y]] <- NULL
      }
    }
    
    # join letters into summary
    summ_long_list <- list()
    for (y in y_cols) {
      mean_name <- paste0(y, "_mean")
      sd_name   <- paste0(y, "_sd")
      se_name   <- paste0(y, "_se")
      n_name    <- paste0(y, "_n")
      
      tmp <- summ[, c(x_col, mean_name, sd_name, se_name, n_name), drop = FALSE]
      names(tmp) <- c("x", "mean", "sd", "se", "n")
      tmp$group  <- y
      
      # join letters if present
      if (!is.null(tukey_letters[[y]])) {
        g <- tukey_letters[[y]]
        names(g) <- c("x", "letters")
        tmp <- dplyr::left_join(tmp, g, by = "x")
      } else {
        tmp$letters <- NA_character_
      }
      
      summ_long_list[[length(summ_long_list) + 1L]] <- tmp
    }
    
    long <- dplyr::bind_rows(summ_long_list)
    
    long$x     <- factor(long$x, levels = unique(long$x))
    long$group <- factor(long$group)
    
    validate(
      need(
        !all(is.na(long$mean)),
        "Simple bar plots: numeric values could not be read from the selected response columns."
      )
    )
    
    long
  })
  
  
  
