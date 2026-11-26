# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s28_draw_current_graph  (sourced inside server(), local = TRUE)

  graph_type <- reactive({
    # Which sub\u2011tab in "Graphs" is active?
    tab <- input$graphs_subtab
    
    if (is.null(tab)) {
      return(NULL)
    }
    
    if (tab == "General") {
      return(input$graph_type_uni)
    } # "ggpairs", etc.
    if (tab == "PCA") {
      return(input$graph_type_pca)
    } # "pca_scree", "pca_scores", ...
    if (tab == "Correlation") {
      return(input$graph_type_corr)
    } # "corr_spearman" / "corr_pearson"
    if (tab == "Preprocessing") {
      return(input$graph_type_prep)
    } # "prep_preview" / "prep_box"
    if (tab == "PLSR") {
      return(input$plsr_graph_type)
    } # "lv_rmse", "lv_scores", ...
    
    NULL
  })
  
  
  #################### Draw Current Graph ###########################
  
  draw_current_graph <- function() {
    req(graph_type())
    
    if (graph_type() == "ggpairs") {
      plots <- density_hist_plots()
      n <- length(plots)
      if (n < 1) return(invisible())
      
      idx <- suppressWarnings(as.integer(input$simple_bar_which))
      if (is.na(idx) || idx < 1 || idx > n) idx <- 1
      
      print(plots[[idx]])
      return(invisible())
    } else if (graph_type() == "simple_bar") {
      plots <- simple_bar_plots()
      n <- length(plots)
      if (n < 1) {
        return()
      }
      
      # Which plot did the user select?
      idx <- suppressWarnings(as.integer(input$simple_bar_which))
      if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > n) idx <- 1
      
      # For download: remove title, make background transparent
      p_dl <- plots[[idx]] +
        ggplot2::labs(title = NULL, subtitle = NULL) +
        ggplot2::theme(
          plot.title       = element_blank(),
          plot.subtitle    = element_blank(),
          panel.background = element_rect(fill = NA, color = NA),
          plot.background  = element_rect(fill = NA, color = NA)
        )
      
      print(p_dl)
    } else if (graph_type() == "heatmaps") {
      plots <- heatmap_plots()
      n <- length(plots)
      if (n < 1) {
        return()
      }
      
      idx <- suppressWarnings(as.integer(input$simple_bar_which))
      if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > n) idx <- 1
      
      p_plotly <- plots[[idx]]
      
      # Try to save as static image
      tryCatch(
        {
          img <- save_heatmaply_static(p_plotly)
          grid::grid.raster(img)
        },
        error = function(e) {
          # Fallback: create a simple ggplot heatmap
          m_norm <- heatmap_matrix()
          if (!is.null(m_norm) && nrow(m_norm) > 0 && ncol(m_norm) > 0) {
            df_long <- as.data.frame(m_norm) %>%
              tibble::rownames_to_column("Row") %>%
              tidyr::pivot_longer(
                cols      = -Row,
                names_to  = "Col",
                values_to = "Value"
              )
            
            p <- ggplot(df_long, aes(x = Col, y = Row, fill = Value)) +
              geom_tile() +
              scale_fill_viridis_c() +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              labs(title = "Static Heatmap (interactive version available in browser)")
            
            print(p)
          } else {
            p <- ggplot() +
              annotate("text",
                       x = 0.5, y = 0.5,
                       label = "No data available for heatmap",
                       size = 4
              ) +
              theme_void()
            print(p)
          }
        }
      )
    } else if (graph_type() == "ggbetween") {
      req(primary_factor(), uni_resp())
      
      df <- dat_raw()
      
      # x = primary factor (same as on-screen plot)
      x_col <- primary_factor()
      validate(
        need(x_col %in% names(df), "Selected factor column not found in data.")
      )
      
      df[[x_col]] <- as.factor(df[[x_col]])
      
      # y = chosen response (ggbetween_resp if available, else first selected)
      resp <- uni_resp()
      resp <- resp[resp %in% names(df)]
      validate(
        need(length(resp) >= 1, "Select at least one numeric response variable.")
      )
      
      if (!is.null(input$ggbetween_resp) &&
          input$ggbetween_resp %in% resp) {
        y_col <- input$ggbetween_resp
      } else {
        y_col <- resp[1]
      }
      
      df[[y_col]] <- suppressWarnings(as.numeric(df[[y_col]]))
      validate(
        need(
          !all(is.na(df[[y_col]])),
          "Selected response column must contain numeric values."
        )
      )
      
      x_lab <- gsub("_", " ", x_col, fixed = TRUE)
      y_lab <- gsub("_", " ", y_col, fixed = TRUE)
      
      plt <- ggstatsplot::ggbetweenstats(
        data                 = df,
        x                    = !!rlang::sym(x_col),
        y                    = !!rlang::sym(y_col),
        pairwise.comparisons = TRUE,
        pairwise.display     = "significant",
        messages             = FALSE
      ) +
        labs(
          x        = x_lab,
          y        = y_lab,
          title    = NULL,
          subtitle = NULL
        ) +
        theme(
          text                = element_text(size = 8, color = "black"),
          plot.title          = element_blank(),
          plot.subtitle       = element_blank(),
          plot.title.position = "plot",
          axis.text           = element_text(size = 10, color = "black"),
          axis.title          = element_text(size = 12),
          axis.ticks          = element_blank(),
          axis.line           = element_line(colour = "grey50"),
          panel.grid          = element_line(color = "#b4aea9"),
          panel.grid.minor    = element_blank(),
          panel.grid.major.x  = element_blank(),
          panel.grid.major.y  = element_line(linetype = "dashed"),
          panel.background    = element_rect(fill = NA, color = NA),
          plot.background     = element_rect(fill = NA, color = NA)
        )
      
      print(plt)
    } else if (graph_type() %in% c("corr_spearman", "corr_pearson")) {
      res <- corr_result()
      req(res)
      
      if (input$graph_type_corr == "corr_pearson") {
        r <- res$r_pearson
        P <- res$P_pearson
      } else {
        r <- res$r_spearman
        P <- res$P_spearman
      }
      
      vars <- colnames(r)
      n <- length(vars)
      
      
      # Same style selector as screen: 1\u20136
      if (is.null(input$simple_bar_which)) {
        idx <- 1L
      } else {
        idx <- suppressWarnings(as.integer(input$simple_bar_which))
        if (length(idx) == 0L || is.na(idx) || idx < 1L || idx > 5L) idx <- 1L
      }
      
      
      
      
      
      # Transparent background for base / ggplot / corrplot
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(bg = NA)
      
      # ---------- 1) Existing ellipse ggplot ----------
      if (idx == 1) {
        ell_list <- list()
        num_list <- list()
        k <- 1
        
        for (i in seq_len(n)) {
          for (j in seq_len(n)) {
            rho <- r[i, j]
            mat2 <- matrix(c(1, rho, rho, 1), nrow = 2)
            ell <- ellipse::ellipse(mat2, t = 0.43)
            ell_df <- data.frame(
              Var1 = vars[j],
              Var2 = vars[n + 1 - i],
              x    = ell[, 1] + j,
              y    = ell[, 2] + (n + 1 - i),
              Corr = rho
            )
            ell_list[[k]] <- ell_df
            
            pij <- P[i, j]
            num_list[[k]] <- data.frame(
              Var1 = vars[j],
              Var2 = vars[n + 1 - i],
              x    = j,
              y    = n + 1 - i,
              Corr = rho,
              Lab  = if (i == j) "1" else paste0(sprintf("%.2f", rho),
                       if (is.na(pij)) "" else if (pij < 0.001) "***" else if (pij < 0.01) "**" else if (pij < 0.05) "*" else "")
            )
            k <- k + 1
          }
        }
        
        df_ell <- do.call(rbind, ell_list)
        df_num <- do.call(rbind, num_list)
        
        p <- ggplot() +
          geom_polygon(
            data = df_ell,
            aes(x = x, y = y, group = interaction(Var1, Var2), fill = Corr),
            color = NA,
            alpha = 0.9
          ) +
          geom_text(
            data = df_num,
            aes(x = x, y = y, label = Lab),
            size = 3
          ) +
          scale_fill_gradient2(                      # same convention as the corrplot styles: negative red, positive blue
            low = "#B2182B", mid = "white", high = "#2166AC",
            midpoint = 0, limits = c(-1, 1)
          ) +
          scale_x_continuous(
            breaks = seq_len(n),
            labels = vars,
            expand = c(0.1, 0.1)
          ) +
          scale_y_continuous(
            breaks = seq_len(n),
            labels = rev(vars),
            expand = c(0.1, 0.1)
          ) +
          coord_fixed() +
          theme_minimal() +
          theme(
            axis.text.x      = element_text(angle = 45, hjust = 1),
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            panel.background = element_rect(fill = NA, colour = NA),
            plot.background  = element_rect(fill = NA, colour = NA)
          )
        
        print(p)
        return(invisible())
      }
      
      cor_mat <- r
      p.mat <- P; p.mat[is.na(p.mat)] <- 0; diag(p.mat) <- 0
      n_rect <- if (ncol(cor_mat) >= 4) min(4L, ncol(cor_mat) - 1L) else NULL   # hclust boxes need enough variables
      
      # ---------- 2) Square-like style ----------
      if (idx == 2) {
        corrplot::corrplot(
          cor_mat,
          method      = "square",
          outline     = TRUE,
          addgrid.col = "darkgray",
          order       = "hclust",
          mar         = c(4, 0, 4, 0),
          addrect     = n_rect,
          p.mat       = p.mat, sig.level = c(0.001, 0.01, 0.05), insig = "label_sig", pch.cex = 0.8,
          rect.col    = "black",
          rect.lwd    = 2, cl.length = 5,
          cl.pos      = "b",
          tl.col      = "indianred4",
          tl.cex      = 1,
          cl.cex      = 0.8,
          bg          = "transparent"
        )
        return(invisible())
      }
      
      # ---------- 3) Color + coefficients ----------
      if (idx == 3) {
        cols <- colorRampPalette(c("darkred", "white", "midnightblue"))(100)
        corrplot::corrplot(
          cor_mat,
          method        = "color",
          outline       = TRUE,
          addgrid.col   = "darkgray",
          order         = "hclust",
          addrect       = n_rect,
          p.mat         = p.mat, sig.level = 0.05, insig = "blank",
          rect.col      = "black",
          rect.lwd      = 2, cl.length = 5,
          cl.pos        = "b",
          tl.col        = "indianred4",
          tl.cex        = 1,
          cl.cex        = 0.8,
          addCoef.col   = "white",
          number.digits = 2,
          number.cex    = 0.75,
          col           = cols,
          bg            = "transparent"
        )
        return(invisible())
      }
      
      # ---------- 4) Upper + lower combined ----------
      if (idx == 4) {
        ord <- hclust(as.dist(1 - cor_mat))$order
        cm_ord <- cor_mat[ord, ord]
        
        corrplot::corrplot(
          cm_ord,
          outline     = TRUE,
          addgrid.col = "darkgray",
          cl.pos      = "r",
          tl.col      = "indianred4",
          tl.cex      = 1,
          cl.cex      = 0.8,
          mar         = c(4, 0, 4, 0),
          type        = "upper",
          tl.pos      = "tl",
          bg          = "azure2"
        )
        corrplot::corrplot(
          cm_ord,
          outline     = TRUE,
          addgrid.col = "darkgray",
          cl.pos      = "r",
          tl.col      = "indianred4",
          tl.cex      = 1,
          cl.cex      = 0.8,
          mar         = c(4, 0, 4, 0),
          type        = "lower",
          tl.pos      = "tl",
          add         = TRUE,
          bg          = "azure"
        )
        return(invisible())
      }
      
      # (we removed style 5)
      
      # ---------- 6) Upper-triangle only ----------
      if (idx == 5) {
        corrplot::corrplot(
          cor_mat,
          type   = "upper",
          order  = "hclust",
          tl.col = "black",
          tl.srt = 45,
          bg     = "transparent"
        )
        return(invisible())
      }
      
      # Fallback: style 2
      corrplot::corrplot(
        cor_mat,
        method      = "square",
        outline     = TRUE,
        addgrid.col = "darkgray",
        order       = "hclust",
        mar         = c(4, 0, 4, 0),
        addrect     = n_rect,
        rect.col    = "black",
        rect.lwd    = 2, cl.length = 5,
        cl.pos      = "b",
        tl.col      = "indianred4",
        tl.cex      = 1,
        cl.cex      = 0.8,
        bg          = "transparent"
      )
      
      vars <- colnames(r)
      n <- length(vars)
      
      ell_list <- list()
      num_list <- list()
      k <- 1
      
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          rho <- r[i, j]
          mat <- matrix(c(1, rho, rho, 1), nrow = 2)
          ell <- ellipse::ellipse(mat, t = 0.43)
          ell_df <- data.frame(
            Var1 = vars[j],
            Var2 = vars[n + 1 - i],
            x    = ell[, 1] + j,
            y    = ell[, 2] + (n + 1 - i),
            Corr = rho
          )
          ell_list[[k]] <- ell_df
          
          num_list[[k]] <- data.frame(
            Var1 = vars[j],
            Var2 = vars[n + 1 - i],
            x    = j,
            y    = n + 1 - i,
            Corr = rho
          )
          
          k <- k + 1
        }
      }
      
      df_ell <- do.call(rbind, ell_list)
      df_num <- do.call(rbind, num_list)
      
      p <- ggplot() +
        geom_polygon(
          data = df_ell,
          aes(x = x, y = y, group = interaction(Var1, Var2), fill = Corr),
          color = NA,
          alpha = 0.9
        ) +
        geom_text(
          data = df_num,
          aes(x = x, y = y, label = round(Corr, 2)),
          size = 3
        ) +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 0, limits = c(-1, 1)
        ) +
        scale_x_continuous(
          breaks = seq_len(n),
          labels = vars,
          expand = c(0.1, 0.1)
        ) +
        scale_y_continuous(
          breaks = seq_len(n),
          labels = rev(vars),
          expand = c(0.1, 0.1)
        ) +
        coord_fixed() +
        theme_minimal() +
        theme(
          axis.text.x  = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      
      print(p)
    } else if (graph_type() == "radial") {
      df_rad <- radial_df() # <- correct reactive
      rad <- prepare_radial_data(df_rad, empty_bar = 2)
      data <- rad$data
      label_data <- rad$label_data
      base_data <- rad$base_data
      grid_data <- rad$grid_data
      number_of_bar <- rad$number_of_bar
      
      label_size <- dplyr::case_when(
        number_of_bar > 80 ~ 2.5,
        number_of_bar > 50 ~ 3,
        number_of_bar > 30 ~ 3.5,
        TRUE ~ 4
      )
      group_size <- dplyr::case_when(
        nrow(base_data) > 8 ~ 3,
        TRUE ~ 4
      )
      
      max_tot <- max(label_data$tot, na.rm = TRUE)
      if (!is.finite(max_tot)) max_tot <- 0
      
      p <- ggplot(data) +
        geom_bar(
          aes(x = as.factor(id), y = value, fill = observation),
          stat  = "identity",
          alpha = 0.6
        ) +
        scale_fill_viridis_d(option = "C") +
        geom_segment(
          data = grid_data,
          aes(x = end, y = 0, xend = start, yend = 0),
          colour = "grey70", size = 0.3, inherit.aes = FALSE
        ) +
        geom_segment(
          data = grid_data,
          aes(x = end, y = 0.25 * max_tot, xend = start, yend = 0.25 * max_tot),
          colour = "grey70", size = 0.3, inherit.aes = FALSE
        ) +
        geom_segment(
          data = grid_data,
          aes(x = end, y = 0.50 * max_tot, xend = start, yend = 0.50 * max_tot),
          colour = "grey70", size = 0.3, inherit.aes = FALSE
        ) +
        geom_segment(
          data = grid_data,
          aes(x = end, y = 0.75 * max_tot, xend = start, yend = 0.75 * max_tot),
          colour = "grey70", size = 0.3, inherit.aes = FALSE
        ) +
        geom_segment(
          data = grid_data,
          aes(x = end, y = max_tot, xend = start, yend = max_tot),
          colour = "grey70", size = 0.3, inherit.aes = FALSE
        ) +
        ggplot2::annotate(
          "text",
          x     = rep(max(data$id), 5),
          y     = c(0, 0.25, 0.5, 0.75, 1) * max_tot,
          label = round(c(0, 0.25, 0.5, 0.75, 1) * max_tot, 0),
          color = "grey40",
          size  = 3,
          hjust = 1
        ) +
        ylim(-0.3 * max_tot, max_tot) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position  = "right",
          axis.text        = element_blank(),
          axis.title       = element_blank(),
          panel.grid       = element_blank(),
          plot.margin      = unit(rep(-1, 4), "cm"),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background  = element_rect(fill = NA, colour = NA)
        ) +
        coord_polar() +
        geom_text(
          data = label_data,
          aes(
            x = id, y = tot + 0.05 * max_tot,
            label = individual, hjust = hjust
          ),
          color = "black",
          fontface = "bold",
          alpha = 0.7,
          size = label_size,
          angle = label_data$angle,
          inherit.aes = FALSE
        ) +
        geom_segment(
          data = base_data,
          aes(
            x = start, y = -0.02 * max_tot,
            xend = end, yend = -0.02 * max_tot
          ),
          colour = "black",
          alpha = 0.8,
          size = 0.6,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = base_data,
          aes(x = title, y = -0.07 * max_tot, label = group),
          colour = "black",
          alpha = 0.8,
          size = group_size,
          fontface = "bold",
          inherit.aes = FALSE
        )
      
      print(p)
    } else if (graph_type() == "linear_group") {
      plots <- linear_group_plots()
      n <- length(plots)
      if (n < 1) {
        return()
      }
      
      # Re\u2011use the same selector as for simple_bar
      idx <- suppressWarnings(as.integer(input$simple_bar_which))
      if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > n) idx <- 1
      
      
      # For download: remove title, make background transparent
      p_dl <- plots[[idx]] +
        ggplot2::labs(title = NULL, subtitle = NULL) +
        ggplot2::theme(
          plot.title       = element_blank(),
          plot.subtitle    = element_blank(),
          panel.background = element_rect(fill = NA, color = NA),
          plot.background  = element_rect(fill = NA, color = NA)
        )
      
      print(p_dl)
    } else if (graph_type() == "pca_loadings_profile") {
      res <- pca_result()
      req(res)
      print(esc_plot_loading_profile(res$pca$rotation, res$var_names, 1:2, identical(input$pca_load_profile_type, "bar")))
    } else if (graph_type() == "prep_preview") {
      
      # Legend only if a Class is selected in "Select Class:"
      show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
      
      df <- dat_multi()
      req(input$prep_preview_var)
      rng <- input$multi_var_range
      req(rng)
      
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      # Preprocess
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      
      var_names_full <- colnames(df)[vars_idx]
      p_X <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for preprocessing preview."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      # ID
      if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
          input$multi_id_col %in% names(df)) {
        ID <- df[[input$multi_id_col]]
      } else {
        ID <- seq_len(nrow(df))
      }
      ID_chr <- as.character(ID)
      
      # Class
      if (!is.null(input$multi_class_col) && input$multi_class_col != "" &&
          input$multi_class_col %in% names(df)) {
        Class <- factor(df[[input$multi_class_col]])
      } else {
        Class <- factor(rep("All", nrow(df)))
      }
      
      selected <- input$prep_preview_var
      
      if (selected == "__ALL__") {
        raw_long <- as.data.frame(X)
        colnames(raw_long) <- var_names
        raw_long$Sample <- ID_chr
        raw_long$Class <- Class
        raw_long <- tidyr::pivot_longer(
          raw_long,
          cols      = tidyselect::all_of(var_names),
          names_to  = "Variable",
          values_to = "Value"
        )
        raw_long$Type <- "Raw"
        
        pre_long <- as.data.frame(Xp)
        colnames(pre_long) <- var_names
        pre_long$Sample <- ID_chr
        pre_long$Class <- Class
        pre_long <- tidyr::pivot_longer(
          pre_long,
          cols      = tidyselect::all_of(var_names),
          names_to  = "Variable",
          values_to = "Value"
        )
        pre_long$Type <- "Preprocessed"
        
        plot_df_long <- dplyr::bind_rows(raw_long, pre_long)
        plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
        
        n_vars <- length(var_names)
        n_breaks <- min(10, n_vars)
        idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
        var_breaks <- var_names[idx_breaks]
        
        p <- ggplot(
          plot_df_long,
          aes(
            x = Variable, y = Value,
            group = Sample, color = Class
          )
        ) +
          geom_line(alpha = 0.5) +
          facet_wrap(~Type, ncol = 1, scales = "free_y") +
          labs(
            x = "Variables",
            y = "Intensity"
            #title = "Preprocessing preview: all samples"
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks, expand = c(0, 0)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (!show_legend) {
          p <- p + theme(legend.position = "none")
        }
        if (identical(input$pca_load_profile_type, "bar")) p <- esc_prep_profile_bar(plot_df_long, TRUE, var_breaks)
        print(p)
      } else {
        idx_row <- which(ID_chr == selected)
        validate(need(length(idx_row) == 1, "Selected sample not found."))
        
        raw <- X[idx_row, ]
        pre <- Xp[idx_row, ]
        
        plot_df <- data.frame(
          Variable = var_names,
          Raw      = as.numeric(raw),
          Pre      = as.numeric(pre)
        )
        
        plot_df_long <- tidyr::pivot_longer(
          plot_df,
          cols      = c("Raw", "Pre"),
          names_to  = "Type",
          values_to = "Value"
        )
        plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
        
        n_vars <- length(var_names)
        n_breaks <- min(10, n_vars)
        idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
        var_breaks <- var_names[idx_breaks]
        
        p <- ggplot(
          plot_df_long,
          aes(
            x = Variable, y = Value,
            group = Type,
            color = Type
          )
        ) +
          geom_line(alpha = 0.9, linewidth = 1) +
          facet_wrap(~Type, ncol = 1, scales = "free_y") +
          labs(
            x = "Variable (wavelength / wavenumber)",
            y = "Intensity"
            #title = paste("Preprocessing preview:", selected)
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (!show_legend) {
          p <- p + theme(legend.position = "none")
        }
        if (identical(input$pca_load_profile_type, "bar")) p <- esc_prep_profile_bar(plot_df_long, FALSE, var_breaks)
        print(p)
      }
    } else if (graph_type() == "prep_box") {
      
      # Legend only if a Class is selected
      show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
      
      # cat("DEBUG: entering prep_box branch in draw_current_graph()\n")
      df <- dat_multi()
      rng <- input$multi_var_range
      req(rng)
      
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      
      var_names_full <- colnames(df)[vars_idx]
      p_X <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for box plots."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      colnames(X) <- var_names
      colnames(Xp) <- var_names
      
      raw_long <- as.data.frame(X)
      raw_long$Type <- "Raw data"
      pre_long <- as.data.frame(Xp)
      pre_long$Type <- "Preprocessed"
      
      plot_df <- rbind(raw_long, pre_long)
      
      plot_df_long <- tidyr::pivot_longer(
        plot_df,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      
      
      plot_df_long$Variable <- factor(plot_df_long$Variable, levels = var_names)
      n_vars <- length(var_names)
      n_breaks <- min(10, n_vars)
      idx_breaks <- unique(round(seq(1, n_vars, length.out = n_breaks)))
      var_breaks <- var_names[idx_breaks]
      
      p <- ggplot(
        plot_df_long,
        aes(x = Variable, y = Value)
      ) +
        geom_boxplot(outlier.size = 0.5, fill = "#3498db", alpha = 0.7) +
        facet_wrap(~Type, ncol = 2, scales = "free_y") +
        scale_x_discrete(breaks = var_breaks, labels = var_breaks) +
        labs(
          x = "Variables",
          y = "Intensity"
        ) +
        theme_minimal() +
        theme(
          axis.text.x  = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(face = "bold")
        )
      
      
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      }
      print(p)
      
      
    } else if (graph_type() == "prep_scatter") {
      
      
      # Legend only if a Class is selected
      show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
      
      df  <- dat_multi()
      rng <- input$multi_var_range
      req(rng)
      
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      var_names_full <- colnames(df)[vars_idx]
      p_X  <- ncol(X)
      p_Xp <- ncol(Xp)
      p_nm <- length(var_names_full)
      p_common <- min(p_X, p_Xp, p_nm)
      validate(need(p_common > 0, "No variables available for scatter plot."))
      
      al <- esc_align_cols(X, Xp, var_names_full)  # align by name (SG trims edges)
      X <- X[, al$ix, drop = FALSE]
      Xp <- Xp[, al$ip, drop = FALSE]
      var_names <- var_names_full[al$ix]
      
      colnames(X)  <- var_names
      colnames(Xp) <- var_names
      
      raw_long <- as.data.frame(X)
      pre_long <- as.data.frame(Xp)
      raw_long$Sample <- seq_len(nrow(raw_long))
      pre_long$Sample <- seq_len(nrow(pre_long))
      
      raw_long <- tidyr::pivot_longer(
        raw_long,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      raw_long$Type <- "Raw data"
      
      pre_long <- tidyr::pivot_longer(
        pre_long,
        cols      = all_of(var_names),
        names_to  = "Variable",
        values_to = "Value"
      )
      pre_long$Type <- "Preprocessed"
      
      plot_df <- dplyr::bind_rows(raw_long, pre_long)
      plot_df$Variable <- factor(plot_df$Variable, levels = var_names)
      
      p <- ggplot(
        plot_df,
        aes(x = Sample, y = Value, color = Variable)
      ) +
        geom_point(alpha = 0.6, size = 1.5) +
        facet_wrap(~Type, ncol = 2, scales = "free_y") +
        labs(
          x = "Sample index",
          y = "Intensity",
          color = "Variable"
        ) +
        theme_minimal()
      
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      }
      print(p)
    } else if (graph_type() == "pca_scores") {
      res <- pca_result()
      req(res)
      
      pair <- input$pca_pc_pair %||% "1_2"
      idx  <- as.integer(strsplit(pair, "_")[[1]])
      pcx  <- idx[1]
      pcy  <- idx[2]
      
      n_pc <- ncol(res$pca$x)
      validate(
        need(
          pcx <= n_pc && pcy <= n_pc,
          paste0(
            "Selected PC pair (PC", pcx, " vs PC", pcy,
            ") not available; only ", n_pc, " PCs computed."
          )
        )
      )
      
      s    <- res$pca$sdev
      prop <- s^2 / sum(s^2)
      xlab_txt <- sprintf("PC%d (%.2f%%)", pcx, prop[pcx] * 100)
      ylab_txt <- sprintf("PC%d (%.2f%%)", pcy, prop[pcy] * 100)
      
      scores <- as.data.frame(res$pca$x[, c(pcx, pcy), drop = FALSE])
      colnames(scores) <- c("PCx", "PCy")
      scores$Class <- res$Class
      scores$ID    <- res$ID
      
      # --- attach secondary IDs from multivariate data ---
      df_multi <- tryCatch(dat_multi(), error = function(e) NULL)
      if (!is.null(df_multi) && nrow(df_multi) == nrow(scores)) {
        for (i in 1:3) {
          sec_name <- input[[paste0("pca_secid_", i)]]
          if (!is.null(sec_name) && nzchar(sec_name) && sec_name %in% names(df_multi)) {
            scores[[paste0("secid", i)]] <- df_multi[[sec_name]]
          }
        }
      }
      
      # base aesthetics: color by Class
      aes_args <- list(
        x     = quote(PCx),
        y     = quote(PCy),
        color = quote(Class)
      )
      
      # override by secondary IDs if requested
      if (isTRUE(input$pca_modify_view)) {
        if (isTRUE(input$pca_shape_by_secid) && "secid1" %in% names(scores)) {
          aes_args$shape <- quote(secid1)
        }
        if (isTRUE(input$pca_size_by_secid) && "secid2" %in% names(scores)) {
          aes_args$size <- quote(secid2)
        }
        if (isTRUE(input$pca_color_by_secid) && "secid3" %in% names(scores)) {
          aes_args$color <- quote(secid3)
        }
      }
      
      # build base plot, taking care not to override mapped size
      if (!isTRUE(input$pca_modify_view) || !"size" %in% names(aes_args)) {
        p <- ggplot(scores, do.call(aes, aes_args)) +
          geom_point(size = 3, alpha = 0.6)
      } else {
        p <- ggplot(scores, do.call(aes, aes_args)) +
          geom_point(alpha = 0.6)
      }
      
      # ID labels
      if (isTRUE(input$pca_show_id) &&
          !is.null(scores$ID) &&
          length(scores$ID) == nrow(scores)) {
        p <- p +
          ggrepel::geom_text_repel(
            data  = scores,
            aes(x = PCx, y = PCy, label = ID),
            size = 3,
            max.overlaps = 50,
            show.legend  = FALSE
          )
      }
      
      # legend title for color: Class vs Secondary ID 3
      color_title <- if (!isTRUE(input$pca_modify_view) ||
                         !"color" %in% names(aes_args) ||
                         identical(aes_args$color, quote(Class))) {
        class_legend_title()
      } else {
        "Secondary ID 3"
      }
      
      # Legend titles: use original variable names instead of secid1/2/3
      sec1_name <- input$pca_secid_1
      sec2_name <- input$pca_secid_2
      sec3_name <- input$pca_secid_3
      
      shape_title <- NULL
      size_title  <- NULL
      color_title <- class_legend_title()
      
      if (isTRUE(input$pca_modify_view)) {
        if (isTRUE(input$pca_shape_by_secid) &&
            "shape" %in% names(aes_args) &&
            !is.null(sec1_name) && nzchar(sec1_name)) {
          shape_title <- sec1_name
        }
        if (isTRUE(input$pca_size_by_secid) &&
            "size" %in% names(aes_args) &&
            !is.null(sec2_name) && nzchar(sec2_name)) {
          size_title <- sec2_name
        }
        if (isTRUE(input$pca_color_by_secid) &&
            "color" %in% names(aes_args) &&
            !is.null(sec3_name) && nzchar(sec3_name)) {
          color_title <- sec3_name
        }
      }
      
      p <- p +
        labs(
          x     = xlab_txt,
          y     = ylab_txt,
          color = color_title,
          shape = shape_title,
          size  = size_title
        ) +
        theme_minimal()
      
      
      
      # overlays (same as before)
      ov <- input$pca_scores_overlays %||% character(0)
      
      if ("ellipse" %in% ov) {
        p <- p +
          stat_ellipse(
            aes(group = Class, color = Class),
            type      = "norm",
            level     = 0.95,
            linewidth = 1
          )
      }
      
      if ("hull" %in% ov) {
        hull_df <- scores |>
          dplyr::group_by(Class) |>
          dplyr::slice(chull(PCx, PCy)) |>
          dplyr::ungroup()
        
        p <- p +
          geom_polygon(
            data        = hull_df,
            aes(x = PCx, y = PCy, group = Class, fill = Class),
            color       = NA,
            alpha       = 0.15,
            inherit.aes = FALSE
          )
      }
      
      if ("spokes" %in% ov) {
        centers <- scores |>
          dplyr::group_by(Class) |>
          dplyr::summarise(
            cx      = mean(PCx, na.rm = TRUE),
            cy      = mean(PCy, na.rm = TRUE),
            .groups = "drop"
          )
        
        scores_spokes <- scores |>
          dplyr::left_join(centers, by = "Class")
        
        p <- p +
          geom_segment(
            data = scores_spokes,
            aes(x = cx, y = cy, xend = PCx, yend = PCy, color = Class),
            alpha = 0.4,
            linewidth = 0.5,
            inherit.aes = FALSE
          )
      }
      
      print(p)
      
      
    } else if (graph_type() == "pca_scree") {
      res <- pca_result()
      req(res)
      
      s <- res$pca$sdev
      var <- s^2
      prop <- var / sum(var) * 100 # percentage
      
      df <- data.frame(
        PC      = seq_along(s),
        Prop    = prop
      )
      
      p <- ggplot(df, aes(x = PC, y = Prop)) +
        geom_col(fill = "#3498db", alpha = 0.8, width = 0.7) +
        geom_line(aes(y = Prop), color = "red", linewidth = 1) +
        geom_point(aes(y = Prop), color = "red", size = 2) +
        geom_text(
          aes(label = sprintf("%.1f%%", Prop), y = Prop),
          vjust = -0.4, size = 3
        ) +
        scale_x_continuous(
          breaks = df$PC,
          labels = paste0("PC", df$PC)
        ) +
        labs(
          x = "Principal Components",
          y = "Percentage of explained variances",
          title = "Scree Plot"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5)
        )
      
      print(p)
      
    } else if (graph_type() == "pca_dendrogram") {
      hc <- pca_cluster_state$hclust
      if (is.null(hc)) {
        # simple message if clustering wasn't run
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "No clustering found.\nRun PCA, then click 'Cluster Dendrogram' in Results \u2192 PCA.",
            size = 5, color = "red"
          ) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
        print(p)
        return(invisible())
      }
      
      # Choose style from selector
      style_idx <- suppressWarnings(as.integer(input$simple_bar_which))
      if (is.na(style_idx)) style_idx <- 1L
      
      plot_pca_dendro_style(style_idx, hc)
      
      
    } else if (graph_type() == "pca_loadings") {
      res <- pca_result()
      req(res)
      s <- res$pca$sdev
      prop <- s^2 / sum(s^2)
      xlab_txt <- sprintf("PC1 (%.2f%%)", prop[1] * 100)
      ylab_txt <- sprintf("PC2 (%.2f%%)", prop[2] * 100)
      
      rot <- res$pca$rotation
      ld  <- as.data.frame(rot[, 1:2, drop = FALSE])
      colnames(ld) <- c("PC1", "PC2")
      ld$Variable <- rownames(ld)
      ld$cos2     <- ld$PC1^2 + ld$PC2^2
      
      ov <- input$pca_scores_overlays %||% character(0)
      
      if (isTRUE(input$pca_color_cos2)) {
        p <- ggplot(ld, aes(x = PC1, y = PC2, color = cos2)) +
          { if ("spokes" %in% ov)
            geom_segment(
              aes(x = 0, y = 0, xend = PC1, yend = PC2, color = cos2),
              linewidth = 0.4,
              alpha     = 0.6
            )
            else
              NULL } +
          geom_point(size = 3) +
          geom_text(
            aes(label = Variable),
            color       = "black",
            size        = 3,
            vjust       = -0.5,
            show.legend = FALSE
          ) +
          scale_color_gradientn(
            colours = c("#00AFBB", "#E7B800", "#FC4E07"),
            name    = expression(cos^2)
          ) +
          labs(x = xlab_txt, y = ylab_txt) +
          theme_minimal()
      } else {
        p <- ggplot(ld, aes(x = PC1, y = PC2)) +
          { if ("spokes" %in% ov)
            geom_segment(
              aes(x = 0, y = 0, xend = PC1, yend = PC2),
              color     = "grey60",
              linewidth = 0.4,
              alpha     = 0.6
            )
            else
              NULL } +
          geom_point(color = "darkblue", size = 3, alpha = 0.5) +
          labs(x = xlab_txt, y = ylab_txt) +
          theme_minimal()
      }
      
      print(p)
    } else if (graph_type() == "pca_biplot") {
      res <- pca_result()
      req(res)
      
      # which PCs to use
      pair <- input$pca_pc_pair %||% "1_2"
      idx  <- as.integer(strsplit(pair, "_")[[1]])
      pcx  <- idx[1]
      pcy  <- idx[2]
      
      n_pc <- ncol(res$pca$x)
      validate(
        need(
          pcx <= n_pc && pcy <= n_pc,
          paste0(
            "Selected PC pair (PC", pcx, " vs PC", pcy,
            ") not available; only ", n_pc, " PCs computed."
          )
        )
      )
      
      s    <- res$pca$sdev
      prop <- s^2 / sum(s^2)
      xlab_txt <- sprintf("PC%d (%.2f%%)", pcx, prop[pcx] * 100)
      ylab_txt <- sprintf("PC%d (%.2f%%)", pcy, prop[pcy] * 100)
      
      scores <- as.data.frame(res$pca$x[, c(pcx, pcy), drop = FALSE])
      colnames(scores) <- c("PCx", "PCy")
      scores$ID    <- res$ID
      scores$Class <- res$Class
      
      rot <- as.data.frame(res$pca$rotation[, c(pcx, pcy), drop = FALSE])
      colnames(rot) <- c("PCx", "PCy")
      rot$Variable <- rownames(rot)
      rot$cos2     <- rot$PCx^2 + rot$PCy^2
      
      max_x <- max(abs(scores$PCx))
      max_y <- max(abs(scores$PCy))
      
      ## ---- SCORES (POINTS) ----
      
      if (isTRUE(input$pca_color_cos2)) {
        p <- ggplot() +
          geom_point(
            data  = scores,
            aes(x = PCx, y = PCy, fill = Class),
            shape = 21,          # <- IMPORTANT: filled circle
            alpha = 0.6,
            size  = 3
          ) +
          scale_fill_discrete(name = class_legend_title())
      } else {
        # Original behavior: color by Class
        p <- ggplot() +
          geom_point(
            data = scores,
            aes(x = PCx, y = PCy, color = Class),
            alpha = 0.6,
            size  = 3
          ) +
          scale_color_discrete(name = class_legend_title())
      }
      
      # Optional ID labels
      if (isTRUE(input$pca_show_id)) {
        p <- p +
          ggrepel::geom_text_repel(
            data  = scores,
            aes(x = PCx, y = PCy, label = ID),
            size  = 3,
            max.overlaps = 50,
            show.legend  = FALSE
          )
      }
      
      ## ---- LOADINGS (ARROWS) ----
      
      if (isTRUE(input$pca_color_cos2)) {
        # Arrow line color = cos\u00b2 (continuous), labels black
        p <- p +
          geom_segment(
            data = rot,
            aes(
              x = 0, y = 0,
              xend = PCx * max_x,
              yend = PCy * max_y,
              color = cos2
            ),
            arrow = arrow(length = unit(0.02, "npc"))
          ) +
          geom_text(
            data = rot,
            aes(
              x     = PCx * max_x,
              y     = PCy * max_y,
              label = Variable
            ),
            color = "black",
            size  = 3,
            vjust = -0.5
          ) +
          scale_color_gradientn(
            colours = c("#00AFBB", "#E7B800", "#FC4E07")
          )
      } else {
        # Original red arrows + red labels
        p <- p +
          geom_segment(
            data = rot,
            aes(
              x = 0, y = 0,
              xend = PCx * max_x,
              yend = PCy * max_y
            ),
            arrow = arrow(length = unit(0.02, "npc")),
            color = "red"
          ) +
          geom_text(
            data = rot,
            aes(
              x     = PCx * max_x,
              y     = PCy * max_y,
              label = Variable
            ),
            color = "red",
            size  = 3,
            vjust = -0.5
          )
      }
      
      ## ---- OVERLAYS (ELLIPSE / HULL / SPOKES) ----
      
      ov <- input$pca_scores_overlays %||% character(0)
      
      # Ellipse
      if ("ellipse" %in% ov) {
        if (isTRUE(input$pca_color_cos2)) {
          # Filled ellipses in the same colors as the Class points (fill scale)
          p <- p +
            stat_ellipse(
              data  = scores,
              aes(x = PCx, y = PCy, group = Class, fill = Class),
              geom  = "polygon",
              color = NA,
              alpha = 0.2,
              type  = "norm",
              level = 0.95,
              inherit.aes = FALSE
            )
        } else {
          p <- p +
            stat_ellipse(
              data  = scores,
              aes(x = PCx, y = PCy, group = Class, color = Class),
              type  = "norm",
              level = 0.95,
              linewidth = 1
            )
        }
      }
      
      
      # Hull
      if ("hull" %in% ov) {
        hull_df <- scores |>
          dplyr::group_by(Class) |>
          dplyr::slice(chull(PCx, PCy)) |>
          dplyr::ungroup()
        
        if (isTRUE(input$pca_color_cos2)) {
          p <- p +
            geom_polygon(
              data  = hull_df,
              aes(x = PCx, y = PCy, group = Class, fill = Class),
              color = NA,
              alpha = 0.2,
              inherit.aes = FALSE
            )
        } else {
          p <- p +
            geom_polygon(
              data = hull_df,
              aes(x = PCx, y = PCy, group = Class, fill = Class),
              color = NA,
              alpha = 0.15,
              inherit.aes = FALSE
            )
        }
      }
      
      # Spokes
      if ("spokes" %in% ov) {
        centers <- scores |>
          dplyr::group_by(Class) |>
          dplyr::summarise(
            cx      = mean(PCx, na.rm = TRUE),
            cy      = mean(PCy, na.rm = TRUE),
            .groups = "drop"
          )
        
        scores_spokes <- scores |>
          dplyr::left_join(centers, by = "Class")
        
        if (isTRUE(input$pca_color_cos2)) {
          p <- p +
            geom_segment(
              data  = scores_spokes,
              aes(x = cx, y = cy, xend = PCx, yend = PCy),
              shape = 21,         # <- key: shape with fill
              alpha = 0.4,
              linewidth = 0.5,
              inherit.aes = FALSE
            )
        } else {
          p <- p +
            geom_segment(
              data = scores_spokes,
              aes(x = cx, y = cy, xend = PCx, yend = PCy, color = Class),
              alpha = 0.4,
              linewidth = 0.5,
              inherit.aes = FALSE
            )
        }
      }
      
      ## ---- AXES / THEME ----
      
      p <- p +
        labs(
          x = xlab_txt,
          y = ylab_txt
        ) +
        theme_minimal()
      
      print(p)
    }
    else if (graph_type() == "lv_rmse") {
      # PLSR: LV vs RMSEC / RMSECV
      res <- plsr_result()
      perf <- res$perf
      if (is.null(perf) || nrow(perf) == 0) {
        return()
      }
      
      lv_num <- as.numeric(sub("LV-", "", perf$LV))
      df <- data.frame(
        LV     = lv_num,
        RMSEC  = perf$RMSEC,
        RMSECV = perf$RMSECV
      )
      df_long <- tidyr::pivot_longer(
        df,
        cols      = c("RMSEC", "RMSECV"),
        names_to  = "Metric",
        values_to = "Value"
      )
      
      p <- ggplot(df_long, aes(x = LV, y = Value, color = Metric)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        scale_x_continuous(breaks = lv_num) +
        labs(
          x = "Number of latent variables (LV)",
          y = "RMSE"
        )
      print(p)
    } else if (graph_type() == "lv_scores") {
      res <- plsr_result()
      if (is.null(res$fit)) return()
      print(plsr_score_gg(res, repel = TRUE))
    } else if (graph_type() == "y_meas_vs_cv") {
      res <- plsr_result()
      req(length(res$y_cal) > 0, length(res$y_cv_hat) > 0)
      
      valid_idx <- !is.na(res$y_cal) & !is.na(res$y_cv_hat)
      if (sum(valid_idx) < 2) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "No cross\u2011validation predictions available.\nCheck CV type and data.",
            size = 5, color = "red"
          ) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
        print(p)
        return(invisible())
      }
      
      y_obs  <- res$y_cal[valid_idx]
      y_pred <- res$y_cv_hat[valid_idx]
      resid  <- y_obs - y_pred
      
      if (!is.null(res$cal_id) && length(res$cal_id) == length(res$y_cal)) {
        ID <- res$cal_id[valid_idx]
      } else {
        ID <- seq_len(length(y_obs))
      }
      
      df <- data.frame(
        Observed  = y_obs,
        Predicted = y_pred,
        Residual  = resid,
        ID        = ID
      )
      
      lLim <- min(df$Predicted, df$Observed, na.rm = TRUE)
      uLim <- max(df$Predicted, df$Observed, na.rm = TRUE)
      rLim <- (uLim - lLim) * 0.15
      
      zeroLine <- seq(lLim - rLim, uLim + rLim, length.out = 200)
      
      fit <- lm(Observed ~ Predicted, data = df)
      predLine <- predict(fit, newdata = data.frame(Predicted = zeroLine))
      
      p <- ggplot(df, aes(x = Predicted, y = Observed, color = Residual)) +
        geom_point(alpha = 0.75, size = 2.2) +
        {
          if (isTRUE(input$pca_show_id)) {
            geom_text(
              aes(label = ID),
              size = 3,
              vjust = -0.7,
              show.legend = FALSE
            )
          } else {
            NULL
          }
        } +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = "dashed", color = "red", linewidth = 1
        ) +
        geom_line(
          data = data.frame(Predicted = zeroLine, Observed = predLine),
          aes(x = Predicted, y = Observed),
          color = "black", linetype = "dashed", linewidth = 0.8,
          inherit.aes = FALSE
        ) +
        coord_equal(xlim = c(lLim - rLim, uLim + rLim),
                    ylim = c(lLim - rLim, uLim + rLim)) +
        scale_color_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(
          x     = "Predicted Y (CV)",
          y     = "Observed Y (Calibration)",
          color = "Residual\n(Obs \u2212 Pred)"
        )
      
      print(p)
    } else if (graph_type() == "q_vs_t2") {
      res <- plsr_result()
      df <- data.frame(
        Q  = res$Q,
        T2 = res$T2
      )
      if (!is.null(res$cal_class) && length(res$cal_class) == nrow(df)) {
        df$Class <- res$cal_class
        aes_map <- aes(x = T2, y = Q, color = Class)
      } else {
        aes_map <- aes(x = T2, y = Q)
      }
      # IDs
      if (!is.null(res$cal_id) && length(res$cal_id) == nrow(df)) {
        df$ID <- res$cal_id
      } else {
        df$ID <- seq_len(nrow(df))
      }
      
      show_legend <- !is.null(input$multi_class_col) && nzchar(input$multi_class_col)
      
      p <- ggplot(df, aes_map) +
        geom_point(alpha = 0.7, size = 2)
      
      if (isTRUE(input$pca_show_id)) {
        p <- p +
          geom_text(
            aes(label = ID),
            color = "black",
            size = 3,
            vjust = -0.5,
            show.legend = FALSE
          )
      }
      
      p <- p +
        theme_minimal() +
        labs(
          x     = "Hotelling T\u00b2",
          y     = "Q residuals",
          color = class_legend_title()
        )
      
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      }
      
      print(p)
    } else if (graph_type() == "y_meas_vs_pred") {
      res <- plsr_result()
      req(length(res$y_pred) > 0, length(res$y_pred_hat) > 0)
      
      valid_idx <- !is.na(res$y_pred) & !is.na(res$y_pred_hat)
      if (sum(valid_idx) < 2) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "No external predictions available.\nCheck calibration/prediction split and data.",
            size = 5, color = "red"
          ) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
        print(p)
        return(invisible())
      }
      
      y_obs  <- res$y_pred[valid_idx]
      y_pred <- res$y_pred_hat[valid_idx]
      resid  <- y_obs - y_pred
      
      if (!is.null(res$pred_id) && length(res$pred_id) == length(res$y_pred)) {
        ID <- res$pred_id[valid_idx]
      } else {
        ID <- seq_len(length(y_obs))
      }
      
      df <- data.frame(
        Observed  = y_obs,
        Predicted = y_pred,
        Residual  = resid,
        ID        = ID
      )
      
      lLim <- min(df$Predicted, df$Observed, na.rm = TRUE)
      uLim <- max(df$Predicted, df$Observed, na.rm = TRUE)
      rLim <- (uLim - lLim) * 0.15
      
      zeroLine <- seq(lLim - rLim, uLim + rLim, length.out = 200)
      
      fit <- lm(Observed ~ Predicted, data = df)
      predLine <- predict(fit, newdata = data.frame(Predicted = zeroLine))
      
      p <- ggplot(df, aes(x = Predicted, y = Observed, color = Residual)) +
        geom_point(alpha = 0.75, size = 2.2) +
        {
          if (isTRUE(input$pca_show_id)) {
            geom_text(
              aes(label = ID),
              size = 3,
              vjust = -0.7,
              show.legend = FALSE
            )
          } else {
            NULL
          }
        } +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = "dashed", color = "red", linewidth = 1
        ) +
        geom_line(
          data = data.frame(Predicted = zeroLine, Observed = predLine),
          aes(x = Predicted, y = Observed),
          color = "black", linetype = "dashed", linewidth = 0.8,
          inherit.aes = FALSE
        ) +
        coord_equal(xlim = c(lLim - rLim, uLim + rLim),
                    ylim = c(lLim - rLim, uLim + rLim)) +
        scale_color_viridis_c(option = "plasma") +
        theme_minimal() +
        labs(
          x     = "Predicted Y (External)",
          y     = "Observed Y (External)",
          color = "Residual\n(Obs \u2212 Pred)"
        )
      
      print(p)
    }
    
    else if (graph_type() == "reg_vector") {
      res <- plsr_result()
      vars <- names(res$coef)
      if (length(vars) == 0) {
        return()
      }
      
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$coef[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Regression coefficient"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Regression coefficient"
          )
      }
      print(p)
    } else if (graph_type() == "vip") {
      res <- plsr_result()
      vars <- names(res$vip)
      if (length(vars) == 0) {
        return()
      }
      
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$vip[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "VIP score"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "VIP score"
          )
      }
      print(p)
    } else if (graph_type() == "sr") {
      res <- plsr_result()
      vars <- names(res$sr)
      if (length(vars) == 0) {
        return()
      }
      
      df <- data.frame(
        Variable = vars,
        Index    = esc_var_index(vars),
        Value    = as.numeric(res$sr[vars])
      )
      if (identical(input$pca_load_profile_type, "bar")) {
        p <- ggplot(df, aes(x = Index, y = Value, fill = Value)) +
          geom_col() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1) +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Selectivity ratio"
          )
      } else {
        p <- ggplot(df, aes(x = Index, y = Value)) +
          geom_line(color = "blue") +
          geom_point(color = "red", size = 1) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
          theme_minimal() +
          labs(
            x = esc_var_xlab(vars),
            y = "Selectivity ratio"
          )
      }
      print(p)
    }
  }
  
  
  # PNG download -----------------------------------------------------------
