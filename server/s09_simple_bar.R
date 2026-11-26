# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s09_simple_bar  (sourced inside server(), local = TRUE)

  simple_bar_plots <- reactive({
    d  <- simple_bar_data()
    df <- dat_raw()
    
    # --- y\u2011axis label from primary factor ---
    y_lab <- primary_factor()
    orig_names_vec <- NULL
    orig_names_vec <- tryCatch(uni_orig_names(), error = function(e) NULL)
    pf <- primary_factor()
    if (!is.null(pf) && !is.null(orig_names_vec) && pf %in% names(orig_names_vec)) y_lab <- orig_names_vec[[pf]]

    # relabel group levels in legend to original header text
    d_plot <- d
    resp_order <- uni_resp()
    resp_order <- resp_order[resp_order %in% levels(d_plot$group)]
    if (length(resp_order) > 0) {
      d_plot$group <- factor(d_plot$group, levels = resp_order)
    }
    if (!is.null(orig_names_vec) && !is.null(d_plot$group)) {
      levs <- levels(d_plot$group)
      if (!is.null(levs)) {
        new_levs <- vapply(
          levs,
          function(z) if (z %in% names(orig_names_vec)) orig_names_vec[[z]] else z,
          character(1)
        )
        levels(d_plot$group) <- new_levs
      }
    }
    
    n_groups   <- nlevels(d_plot$group)
    spread_col <- if (identical(input$spread_type, "sd")) "sd" else "se"
    
    # ---- COLOR MODE: none / unicolor / multicolor ----
    color_mode <- if (isTRUE(input$use_multicolor)) {
      "multi"
    } else if (isTRUE(input$use_unicolor)) {
      "uni"
    } else {
      "none"
    }
    
    ## =========================
    ## CASE 1: ONE RESPONSE VAR
    ## =========================
    if (n_groups == 1L) {
      this_name <- levels(d_plot$group)[1]
      
      if (color_mode == "multi") {
        # different color per treatment (x)
        base_cols <- RColorBrewer::brewer.pal(12, "Paired")
        pal_x   <- colorRampPalette(base_cols)(length(unique(d_plot$x)))
        p_single <- ggplot(d_plot, aes(x = x, y = mean, fill = x)) +
          geom_col(
            color = "black",
            width = 0.7,
            alpha = 0.3
          ) +
          scale_fill_manual(values = pal_x)
      } else if (color_mode == "uni") {
        p_single <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_col(
            fill  = "#3498db",
            color = "black",
            width = 0.7,
            alpha = 0.3
          )
      } else {
        p_single <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_col(
            fill  = NA,
            color = "black",
            width = 0.7
          )
      }
      
      p_single <- p_single +
        geom_errorbar(
          aes(
            ymin = mean - .data[[spread_col]],
            ymax = mean + .data[[spread_col]]
          ),
          width = 0.15
        ) +
        geom_text(
          data = subset(d_plot, !is.na(letters)),
          aes(
            y     = mean + .data[[spread_col]] * 1.25,
            label = letters
          ),
          vjust = 0,
          size  = 3.2
        ) +
        ggpubr::theme_pubclean() +
        ggtitle(paste("Bar plot (", this_name, ")", sep = "")) +
        labs(
          x = NULL,
          y = y_lab,
          fill = NULL
        ) +
        theme(
          axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
        ) +
        transparent_legend
      
      return(list(p_single))
    }
    
    ## ==========================================
    ## CASE 2: MULTIPLE RESPONSES + USE FACET
    ## ==========================================
    if (n_groups > 1L && isTRUE(input$use_facet)) {
      # base palette for multicolor x, if needed
      base_cols <- RColorBrewer::brewer.pal(12, "Paired")
      pal_x   <- colorRampPalette(base_cols)(length(unique(d_plot$x)))
      
      if (color_mode == "multi") {
        p_facets <- ggplot(d_plot, aes(x = x, y = mean, fill = x)) +
          geom_col(
            color = "black",
            width = 0.7,
            alpha = 0.3
          ) +
          scale_fill_manual(values = pal_x)
      } else if (color_mode == "uni") {
        p_facets <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_col(
            fill  = "#3498db",
            color = "black",
            width = 0.7,
            alpha = 0.3
          )
      } else {
        p_facets <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_col(
            fill  = NA,
            color = "black",
            width = 0.7
          )
      }
      
      p_facets <- p_facets +
        geom_errorbar(
          aes(
            ymin = mean - .data[[spread_col]],
            ymax = mean + .data[[spread_col]]
          ),
          width = 0.15
        ) +
        geom_text(
          data = subset(d_plot, !is.na(letters)),
          aes(
            y     = mean + .data[[spread_col]] * 1.25,
            label = letters
          ),
          vjust = 0,
          size  = 3.2
        ) +
        facet_wrap(~ group, ncol = 2, scales = "free_y") +
        ggpubr::theme_pubclean() +
        labs(
          x = NULL,
          y = y_lab
        ) +
        theme(
          axis.title.y    = element_text(margin = margin(r = 12), hjust = 0.5),
          strip.text      = element_text(face = "bold", size = 9,
                                         margin = margin(2, 2, 2, 2)),
          strip.background = element_rect(
            fill   = scales::alpha("grey80", 0.4),
            colour = NA
          ),
          panel.spacing   = unit(0.2, "lines"),
          legend.position = "none"
        )
      
      return(list(p_facets))
    }
    
    ## ==========================================
    ## CASE 3: MULTIPLE RESPONSES, NO FACET
    ## ==========================================
    if (n_groups > 1L && !isTRUE(input$use_facet)) {
      
      # base palette by group
      base_cols <- RColorBrewer::brewer.pal(12, "Paired")
      pal_dyn   <- colorRampPalette(base_cols)(n_groups)
      
      # --------- ORIGINAL LOOK when no color_mode chosen ----------
      if (color_mode == "none") {
        # Plot 1: stacked bar (original)
        p1 <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_bar(
            aes(color = group, fill = group),
            stat = "identity",
            position = position_stack()
          ) +
          scale_color_manual(values = pal_dyn) +
          scale_fill_manual(values = pal_dyn) +
          ggpubr::theme_pubclean() +
          ggtitle("Plot 1: Stacked bar") +
          labs(
            x     = NULL,
            y     = y_lab,
            color = NULL,
            fill  = NULL
          ) +
          theme(
            axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
          ) +
          transparent_legend
        
        # Plot 2: dodged bar (original)
        p2 <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_bar(
            aes(color = group, fill = group),
            stat = "identity",
            position = position_dodge(0.8),
            width = 0.7
          ) +
          scale_color_manual(values = pal_dyn) +
          scale_fill_manual(values = pal_dyn) +
          ggpubr::theme_pubclean() +
          ggtitle("Plot 2: Dodged bar") +
          labs(
            x     = NULL,
            y     = y_lab,
            color = NULL,
            fill  = NULL
          ) +
          theme(
            axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
          ) +
          transparent_legend
        
        # Plot 3 & 4: as before
        p3 <- ggpubr::ggdotchart(
          d_plot,
          x       = "x",
          y       = "mean",
          color   = "group",
          palette = pal_dyn,
          size    = 3,
          add     = "segment",
          add.params = list(color = "lightgray", size = 1.5),
          position = position_dodge(0.3),
          sorting  = "none"
        ) +
          ggtitle("Plot 3: Dot chart with segments") +
          labs(x = NULL, y = y_lab, color = NULL) +
          theme(axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)) +
          transparent_legend
        
        p4 <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_linerange(
            aes(x = x, ymin = 0, ymax = mean, group = group),
            color = "lightgray", size = 1.5,
            position = position_dodge(0.3)
          ) +
          geom_point(
            aes(color = group),
            position = position_dodge(0.3), size = 3
          ) +
          scale_color_manual(values = pal_dyn) +
          ggpubr::theme_pubclean() +
          ggtitle("Plot 4: Linerange + points") +
          labs(
            x     = NULL,
            y     = y_lab,
            color = NULL
          ) +
          theme(
            axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
          ) +
          transparent_legend
        
        # Plot 5: dodged bar + error bars + Tukey letters (original)
        dodge_w <- 0.8
        p5 <- ggplot(d_plot, aes(x = x, y = mean)) +
          geom_bar(
            aes(color = group, fill = group),
            stat     = "identity",
            position = position_dodge(dodge_w),
            width    = 0.7
          ) +
          geom_errorbar(
            aes(
              ymin  = mean - .data[[spread_col]],
              ymax  = mean + .data[[spread_col]],
              group = group
            ),
            width    = 0.1,
            size     = 0.4,
            position = position_dodge(dodge_w)
          ) +
          geom_text(
            data = subset(d_plot, !is.na(letters)),
            aes(
              y     = mean + .data[[spread_col]] * 1.25,
              label = letters,
              group = group
            ),
            position   = position_dodge(dodge_w),
            vjust      = 0,
            size       = 3.2,
            color      = "black",
            show.legend = FALSE
          ) +
          scale_color_manual(values = pal_dyn) +
          scale_fill_manual(values  = pal_dyn) +
          ggpubr::theme_pubclean() +
          ggtitle("Plot 5: Dodged bar with SD/SE and Tukey letters") +
          labs(
            x     = NULL,
            y     = y_lab,
            color = NULL,
            fill  = NULL
          ) +
          theme(
            axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
          ) +
          transparent_legend
        
        return(list(p1, p2, p3, p4, p5))
      }
      
      # --------- NEW behavior when Unicolor / Multicolor are chosen ----------
      # helper: map for stacked/dodged bars
      bar_aes <- if (color_mode == "multi") {
        aes(color = group, fill = group)
      } else {
        aes()
      }
      
      fill_manual <- if (color_mode == "multi") {
        list(
          scale_color_manual(values = pal_dyn),
          scale_fill_manual(values  = pal_dyn)
        )
      } else {
        list()
      }
      
      # 1) Stacked bar
      p1 <- ggplot(d_plot, aes(x = x, y = mean)) +
        {
          if (color_mode == "multi") {
            geom_bar(
              bar_aes,
              stat     = "identity",
              position = position_stack(),
              alpha    = 0.3
            )
          } else if (color_mode == "uni") {
            geom_bar(
              stat     = "identity",
              position = position_stack(),
              fill     = "#3498db",
              color    = "black",
              alpha    = 0.3
            )
          } else {
            geom_bar(
              stat     = "identity",
              position = position_stack(),
              fill     = NA,
              color    = "black"
            )
          }
        } +
        fill_manual +
        ggpubr::theme_pubclean() +
        ggtitle("Plot 1: Stacked bar") +
        labs(
          x     = NULL,
          y     = y_lab,
          color = NULL,
          fill  = NULL
        ) +
        theme(
          axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
        ) +
        transparent_legend
      
      # 2) Dodged bar
      p2 <- ggplot(d_plot, aes(x = x, y = mean)) +
        {
          if (color_mode == "multi") {
            geom_bar(
              bar_aes,
              stat     = "identity",
              position = position_dodge(0.8),
              width    = 0.7,
              alpha    = 0.3
            )
          } else if (color_mode == "uni") {
            geom_bar(
              stat     = "identity",
              position = position_dodge(0.8),
              width    = 0.7,
              fill     = "#3498db",
              color    = "black",
              alpha    = 0.3
            )
          } else {
            geom_bar(
              stat     = "identity",
              position = position_dodge(0.8),
              width    = 0.7,
              fill     = NA,
              color    = "black"
            )
          }
        } +
        fill_manual +
        ggpubr::theme_pubclean() +
        ggtitle("Plot 2: Dodged bar") +
        labs(
          x     = NULL,
          y     = y_lab,
          color = NULL,
          fill  = NULL
        ) +
        theme(
          axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
        ) +
        transparent_legend
      
      # 3) Dot chart (unchanged)
      p3 <- ggpubr::ggdotchart(
        d_plot,
        x       = "x",
        y       = "mean",
        color   = "group",
        palette = pal_dyn,
        size    = 3,
        add     = "segment",
        add.params = list(color = "lightgray", size = 1.5),
        position = position_dodge(0.3),
        sorting  = "none"
      ) +
        ggtitle("Plot 3: Dot chart with segments") +
        labs(x = NULL, y = y_lab, color = NULL) +
        theme(axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)) +
        transparent_legend
      
      # 4) Linerange + points (unchanged)
      p4 <- ggplot(d_plot, aes(x = x, y = mean)) +
        geom_linerange(
          aes(x = x, ymin = 0, ymax = mean, group = group),
          color = "lightgray", size = 1.5,
          position = position_dodge(0.3)
        ) +
        geom_point(
          aes(color = group),
          position = position_dodge(0.3), size = 3
        ) +
        scale_color_manual(values = pal_dyn) +
        ggpubr::theme_pubclean() +
        ggtitle("Plot 4: Linerange + points") +
        labs(
          x     = NULL,
          y     = y_lab,
          color = NULL
        ) +
        theme(
          axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
        ) +
        transparent_legend
      
      # 5) Dodged bar + error bars + Tukey letters
      dodge_w <- 0.8
      p5 <- ggplot(d_plot, aes(x = x, y = mean)) +
        {
          if (color_mode == "multi") {
            geom_bar(
              bar_aes,
              stat     = "identity",
              position = position_dodge(dodge_w),
              width    = 0.7,
              alpha    = 0.3
            )
          } else if (color_mode == "uni") {
            geom_bar(
              stat     = "identity",
              position = position_dodge(dodge_w),
              width    = 0.7,
              fill     = "#3498db",
              color    = "black",
              alpha    = 0.3
            )
          } else {
            geom_bar(
              stat     = "identity",
              position = position_dodge(dodge_w),
              width    = 0.7,
              fill     = NA,
              color    = "black"
            )
          }
        } +
        geom_errorbar(
          aes(
            ymin  = mean - .data[[spread_col]],
            ymax  = mean + .data[[spread_col]],
            group = group
          ),
          width    = 0.1,
          size     = 0.4,
          position = position_dodge(dodge_w)
        ) +
        geom_text(
          data = subset(d_plot, !is.na(letters)),
          aes(
            y     = mean + .data[[spread_col]] * 1.25,
            label = letters,
            group = group
          ),
          position   = position_dodge(dodge_w),
          vjust      = 0,
          size       = 3.2,
          color      = "black",
          show.legend = FALSE
        ) +
        fill_manual +
        ggpubr::theme_pubclean() +
        ggtitle("Plot 5: Dodged bar with SD/SE and Tukey letters") +
        labs(
          x     = NULL,
          y     = y_lab,
          color = NULL,
          fill  = NULL
        ) +
        theme(
          axis.title.y = element_text(margin = margin(r = 12), hjust = 0.5)
        ) +
        transparent_legend
      
      return(list(p1, p2, p3, p4, p5))
    }
  })
  
  
  
  
  
  
  
  
  
