# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s26_graph_render  (sourced inside server(), local = TRUE)

  output$graph_plot <- renderPlotly({
    req(input$graph_type_pca) # only used in Graphs -> PCA sub\u2011tab
    if (input$graph_type_pca %in% ESC_EXTRA_GRAPHS) return(esc_ggplotly(extra_plot_obj()))
    
    if (input$graph_type_pca == "prep_preview") {
      df <- dat_multi()
      req(input$prep_preview_var)
      
      # Build X matrix as in PCA
      rng <- input$multi_var_range
      req(rng)
      vars_idx <- seq(rng[1], rng[2])
      X <- as.matrix(df[, vars_idx, drop = FALSE])
      X <- apply(X, 2, as.numeric)
      X <- as.matrix(X)
      
      # Apply preprocessing
      Xp <- pca_preprocess(
        X,
        spectral_steps = input$prep_spectral,
        scaling_step   = input$prep_scaling,
        centering_step = input$prep_centering,
        sg_width       = input$sg_width,
        sg_order       = input$sg_order,
        sg_deriv       = input$sg_deriv
      )
      
      
      # Ensure same columns for X, Xp, and var_names
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
      
      # Class (for colors)
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
          cols      = all_of(var_names),
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
          cols      = all_of(var_names),
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
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks, expand = c(0, 0)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        esc_ggplotly(p)
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
          ) +
          scale_x_discrete(breaks = var_breaks, labels = var_breaks) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        esc_ggplotly(p)
      }
      
      
      
      
    } else if (input$graph_type_pca == "pca_loadings_profile") {
      res <- pca_result()
      req(res)
      esc_ggplotly(esc_plot_loading_profile(res$pca$rotation, res$var_names, 1:2, identical(input$pca_load_profile_type, "bar")))
    } else if (input$graph_type_pca == "pca_scree") {
      res <- pca_result()
      req(res)
      
      s <- res$pca$sdev
      var <- s^2
      prop <- var / sum(var) * 100 # percentage
      
      df <- data.frame(
        PC        = seq_along(s),
        Prop      = prop,
        CumProp   = cumsum(prop)
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
      
      return(esc_ggplotly(p))
    } else if (input$graph_type_pca == "pca_scores") {
      res <- pca_result()
      req(res)
      
      # --- determine which PCs to use ---
      pair <- input$pca_pc_pair %||% "1_2"
      idx <- as.integer(strsplit(pair, "_")[[1]])
      pcx <- idx[1]
      pcy <- idx[2]
      
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
      
      s <- res$pca$sdev
      prop <- s^2 / sum(s^2)
      xlab_txt <- sprintf("PC%d (%.2f%%)", pcx, prop[pcx] * 100)
      ylab_txt <- sprintf("PC%d (%.2f%%)", pcy, prop[pcy] * 100)
      
      scores <- as.data.frame(res$pca$x[, c(pcx, pcy), drop = FALSE])
      colnames(scores) <- c("PCx", "PCy")
      scores$Class <- res$Class
      scores$ID    <- res$ID
      
      # Attach secondary IDs from multivariate data (row\u2011aligned)
      df_multi <- tryCatch(dat_multi(), error = function(e) NULL)
      if (!is.null(df_multi) && nrow(df_multi) == nrow(scores)) {
        for (i in 1:3) {
          sec_name <- input[[paste0("pca_secid_", i)]]
          if (!is.null(sec_name) && nzchar(sec_name) && sec_name %in% names(df_multi)) {
            scores[[paste0("secid", i)]] <- df_multi[[sec_name]]
          }
        }
      }
      
      # Base aesthetics: same as before
      aes_args <- list(
        x   = quote(PCx),
        y   = quote(PCy),
        key = quote(ID),
        color = quote(Class)
      )
      
      # If Modify view is active, override shape/size/color where possible
      if (isTRUE(input$pca_modify_view)) {
        # Shape by Secondary ID 1
        if (isTRUE(input$pca_shape_by_secid) && "secid1" %in% names(scores)) {
          aes_args$shape <- quote(secid1)
        }
        # Size by Secondary ID 2
        if (isTRUE(input$pca_size_by_secid) && "secid2" %in% names(scores)) {
          aes_args$size <- quote(secid2)
        }
        # Color by Secondary ID 3
        if (isTRUE(input$pca_color_by_secid) && "secid3" %in% names(scores)) {
          aes_args$color <- quote(secid3)
        }
      }
      
      # If size is mapped via aes_args, do NOT fix size in geom_point
      if (!isTRUE(input$pca_modify_view) || !"size" %in% names(aes_args)) {
        p <- ggplot(
          scores,
          do.call(aes, aes_args)
        ) +
          geom_point(size = 3, alpha = 0.6)
      } else {
        p <- ggplot(
          scores,
          do.call(aes, aes_args)
        ) +
          geom_point(alpha = 0.6)  # no fixed size; use mapped size
      }
      
      
      # optional ID labels (simple text for plotly)
      if (isTRUE(input$pca_show_id) &&
          !is.null(scores$ID) &&
          length(scores$ID) == nrow(scores)) {
        p <- p +
          geom_text(
            data = scores,
            aes(x = PCx, y = PCy, label = ID),
            color = "black", # fixed color, not mapped
            size = 3,
            vjust = -0.5,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
      
      # Legend titles: use original variable names instead of secid1/2/3
      sec1_name <- input$pca_secid_1
      sec2_name <- input$pca_secid_2
      sec3_name <- input$pca_secid_3
      
      # default titles
      shape_title <- NULL
      size_title  <- NULL
      color_title <- class_legend_title()
      
      if (isTRUE(input$pca_modify_view)) {
        # shape mapped?
        if (isTRUE(input$pca_shape_by_secid) &&
            "shape" %in% names(aes_args) &&
            !is.null(sec1_name) && nzchar(sec1_name)) {
          shape_title <- sec1_name
        }
        # size mapped?
        if (isTRUE(input$pca_size_by_secid) &&
            "size" %in% names(aes_args) &&
            !is.null(sec2_name) && nzchar(sec2_name)) {
          size_title <- sec2_name
        }
        # color mapped?
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
      
      
      
      # overlays (ellipse/hull/spokes) \u2013 unchanged
      ov <- input$pca_scores_overlays %||% character(0)
      
      if ("ellipse" %in% ov) {
        p <- p +
          stat_ellipse(
            aes(group = Class, color = Class),
            type        = "norm",
            level       = 0.95,
            linewidth   = 1,
            show.legend = FALSE   # <- NEW
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
            inherit.aes = FALSE,
            show.legend = FALSE   # <- NEW
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
      
      esc_ggplotly(p, source = "pca_scores") %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_selected") %>%   # <-- NEW for lasso / box
        plotly::layout(
          dragmode = "lasso",                            # default drag = lasso
          selectdirection = "any"                        # (allow any direction)
        )
    } else if (input$graph_type_pca == "pca_loadings") {
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
        # cos\u00b2 coloring
        p <- ggplot(ld, aes(x = PC1, y = PC2, color = cos2)) +
          # spokes from origin when user ticks "Spokes"
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
        # no cos\u00b2; optional spokes in a fixed color
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
          geom_text(aes(label = Variable), size = 3, vjust = -0.5) +
          labs(x = xlab_txt, y = ylab_txt) +
          theme_minimal()
      }
      
      esc_ggplotly(p)
    } else if (input$graph_type_pca == "pca_biplot") {
      res <- pca_result()
      req(res)
      
      # --- determine which PCs to use ---
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
            shape = 21,
            color = NA,
            alpha = 0.6,
            size  = 3
          ) +
          scale_fill_discrete(name = class_legend_title())
      } else {
        p <- ggplot() +
          geom_point(
            data = scores,
            aes(x = PCx, y = PCy, color = Class),
            alpha = 0.6,
            size  = 3
          ) +
          scale_color_discrete(name = class_legend_title())
      }
      
      # optional ID labels (simple text for plotly)
      if (isTRUE(input$pca_show_id) &&
          !is.null(scores$ID) &&
          length(scores$ID) == nrow(scores)) {
        p <- p +
          geom_text(
            data = scores,
            aes(x = PCx, y = PCy, label = ID),
            size = 3,
            vjust = -0.5,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
      
      ## ---- LOADINGS (ARROWS) ----
      if (isTRUE(input$pca_color_cos2)) {
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
        
        p <- p +
          geom_polygon(
            data = hull_df,
            aes(x = PCx, y = PCy, group = Class, fill = Class),
            color = NA,
            alpha = 0.2,
            inherit.aes = FALSE
          )
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
              color = "grey60",
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
      
      esc_ggplotly(p)
      
    } else if (input$graph_type_pca == "pca_dendrogram") {
      req(pca_cluster_state$hclust)
      
      hc <- pca_cluster_state$hclust
      
      # Convert hclust to plotly dendrogram
      # Use plotly's as.dendrogram + ggplotly via ggplot2 or directly plotly
      dend <- as.dendrogram(hc)
      # Build a basic dendrogram with ggplot2 + ggdendro
      dd <- ggdendro::dendro_data(dend, type = "rectangle")
      seg <- dd$segments
      lab <- dd$labels
      
      p <- ggplot() +
        geom_segment(data = seg,
                     aes(x = x, y = y, xend = xend, yend = yend)) +
        # use label positions and names from ggdendro
        scale_x_continuous(
          breaks = lab$x,
          labels = lab$label
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
        labs(
          x = NULL,
          y = "Height",
          title = "Hierarchical clustering of PCA scores"
        ) +
        theme_minimal() +
        theme(
          axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.ticks.x = element_line()
        )
      
      return(esc_ggplotly(p))
    }
    
  })
  
  
  ### ADD HERE (PLSR) \u2013 main PLSR graph output
  
