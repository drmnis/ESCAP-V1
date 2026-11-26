# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s04_study_area_map  (sourced inside server(), local = TRUE)

  make_study_area_figure <- function(level, areas, bgd_sf) {
    bgd_lvl <- bgd_sf %>% dplyr::filter(Level == level)
    sel_sf <- bgd_lvl %>% dplyr::filter(Name %in% areas)
    
    if (nrow(sel_sf) == 0) {
      return(NULL)
    }
    
    # Colors for selected areas
    n_areas <- length(areas)
    base_pal <- RColorBrewer::brewer.pal(8, "Set2")
    pal <- colorRampPalette(base_pal)(n_areas)
    names(pal) <- areas
    
    # Theme for small boxes (insets)
    inset_theme <- theme_minimal(base_size = 8) +
      theme(
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 6),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        # more top margin so titles don't get squeezed when figure is resized
        plot.margin = unit(c(10, 4, 4, 4), "pt"),
        plot.title = element_text(
          hjust    = 0.5,
          face     = "bold",
          size     = 9 # fixed size, stable across PNG/PDF/SVG
        )
      )
    
    # Theme for country map (no title)
    country_theme <- theme_minimal(base_size = 9) +
      theme(
        panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        axis.title        = element_blank(),
        axis.text.x       = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y       = element_text(size = 7),
        plot.background   = element_rect(fill = NA, colour = NA),
        panel.background  = element_rect(fill = NA, colour = NA),
        plot.margin       = unit(c(4, 4, 8, 4), "pt"),
        plot.title        = element_blank(),
        legend.position   = "none"
      )
    
    # Bounding box for manual bottom\u2011center arrow
    bb <- sf::st_bbox(bgd_lvl)
    cx <- (bb["xmin"] + bb["xmax"]) / 2
    y0 <- bb["ymin"] + 0.02 * (bb["ymax"] - bb["ymin"])
    y1 <- y0 + 0.05 * (bb["ymax"] - bb["ymin"])
    
    # ---------- Big country map (right quarter) ----------
    p_country <- ggplot() +
      geom_sf(
        data      = bgd_lvl,
        fill      = alpha("grey90", 0.5),
        color     = alpha("grey60", 0.7),
        linewidth = 0.15
      ) +
      geom_sf(
        data = sel_sf,
        aes(fill = Name),
        color = alpha("black", 0.9),
        linewidth = 0.25,
        alpha = 0.7
      ) +
      scale_fill_manual(values = pal, guide = "none") +
      annotation_scale(
        location   = "bl",
        width_hint = 0.25,
        style      = "bar",
        text_cex   = 0.5
      ) +
      # Manual north arrow: small, bottom\u2011center of the map
      geom_segment(
        aes(x = cx, xend = cx, y = y0, yend = y1),
        arrow = arrow(length = unit(0.08, "cm")),
        lineend = "round",
        color = "black",
        linewidth = 0.3,
        inherit.aes = FALSE
      ) +
      annotate(
        "text",
        x = cx,
        y = y1 + 0.01 * (bb["ymax"] - bb["ymin"]),
        label = "N",
        size = 3,
        fontface = "bold"
      ) +
      coord_sf(expand = FALSE) +
      country_theme
    
    # ---------- Insets (left three\u2011quarters), all same box size ----------
    asp_inset <- 0.8 # same for all chosen areas (landscape-ish)
    
    make_inset <- function(area_name) {
      dat <- sel_sf %>% dplyr::filter(Name == area_name)
      if (nrow(dat) == 0) {
        return(NULL)
      }
      
      # Work in a projected CRS (UTM 46N ~ Bangladesh)
      dat_utm <- sf::st_transform(dat, 32646)
      bb <- sf::st_bbox(dat_utm)
      dx <- (bb["xmax"] - bb["xmin"])
      dy <- (bb["ymax"] - bb["ymin"])
      
      # Choose bar length as a nice fraction of width (in meters)
      # here: 1/4 of width, then rounded to nearest 10 km
      raw_len_m <- 0.25 * dx
      len_km <- max(1, round(raw_len_m / 10000) * 10) # e.g. 10, 20, 30 km
      bar_len_m <- len_km * 1000
      
      # Bar position (bottom-left-ish, a bit higher)
      x0 <- bb["xmin"] + 0.05 * dx
      x1 <- x0 + bar_len_m
      ybar <- bb["ymin"] + 0.12 * dy
      
      xlab <- (x0 + x1) / 2
      ylab <- ybar + 0.05 * dy
      scale_label <- paste(len_km, "km")
      
      ggplot(dat_utm) +
        geom_sf(
          aes(fill = Name),
          color = alpha("black", 0.9),
          linewidth = 0.25,
          alpha = 0.7
        ) +
        scale_fill_manual(values = pal, guide = "none") +
        # Simple manual scale bar in meters (UTM)
        geom_segment(
          aes(x = x0, xend = x1, y = ybar, yend = ybar),
          inherit.aes = FALSE,
          color = "black",
          linewidth = 0.4
        ) +
        geom_segment( # left tick
          aes(
            x = x0, xend = x0,
            y = ybar - 0.015 * dy, yend = ybar + 0.015 * dy
          ),
          inherit.aes = FALSE,
          color = "black",
          linewidth = 0.4
        ) +
        geom_segment( # right tick
          aes(
            x = x1, xend = x1,
            y = ybar - 0.015 * dy, yend = ybar + 0.015 * dy
          ),
          inherit.aes = FALSE,
          color = "black",
          linewidth = 0.4
        ) +
        annotate(
          "text",
          x        = xlab,
          y        = ylab,
          label    = scale_label,
          size     = 2.4,
          vjust    = 0,
          fontface = "plain"
        ) +
        labs(title = area_name) +
        coord_sf(expand = FALSE) +
        inset_theme +
        theme(aspect.ratio = asp_inset)
    }
    
    
    inset_plots <- lapply(areas, make_inset)
    inset_plots <- inset_plots[!vapply(inset_plots, is.null, logical(1))]
    if (length(inset_plots) == 0) {
      return(p_country)
    }
    
    # Layout rules for number of inset columns
    k <- length(inset_plots)
    ncol_insets <- dplyr::case_when(
      k <= 2 ~ 1L,
      k <= 4 ~ 2L,
      k <= 6 ~ 3L,
      k <= 9 ~ 3L,
      TRUE ~ 4L
    )
    
    inset_wrap <- patchwork::wrap_plots(inset_plots, ncol = ncol_insets) +
      patchwork::plot_annotation(title = "Study Area") &
      theme(
        plot.title      = element_text(face = "bold", hjust = 0.5, size = 9),
        plot.background = element_rect(fill = NA, colour = NA)
      )
    
    # Divide the whole width into 4 parts: left 3 for insets, right 1 for country
    full_fig <- inset_wrap | p_country +
      patchwork::plot_layout(widths = c(3, 1))
    
    full_fig
  }
  
  
  # On\u2011screen map in "Map" tab
  output$map_full_fig <- esc_renderPlot({
    dat_all <- bgd_sf_data()
    lvl <- input$map_level
    areas <- input$map_names
    
    # If nothing selected yet, don't draw anything
    if (is.null(areas) || !length(areas)) {
      return(NULL)
    }
    
    fig <- make_study_area_figure(
      level  = lvl,
      areas  = areas,
      bgd_sf = dat_all
    )
    
    validate(
      need(!is.null(fig), "No data for the selected areas.")
    )
    
    fig
  })
  
  
  # --- Downloads: PNG, TIFF, PDF, SVG, EPS (Enhance Plots styling included) ---
  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) local({
    f <- fmt
    output[[paste0("download_map_", f)]] <- downloadHandler(
      filename = function() paste0("ESCAP_study_area_map_", format(Sys.time(), "%Y%m%d_%H%M"), ".", f),
      content = function(file) {
        areas <- input$map_names
        validate(need(!is.null(areas) && length(areas) > 0, "Select at least one area before downloading."))
        fig <- make_study_area_figure(level = input$map_level, areas = areas, bgd_sf = bgd_sf_data())
        validate(need(!is.null(fig), "No data for the selected areas."))
        w <- max(1, input$download_map_w %||% 8); h <- max(1, input$download_map_h %||% 6); dpi <- max(72, input$download_map_dpi %||% 600)
        esc_open_device(file, f, w, h, dpi)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(fig)          # server print(): applies Enhance Plots
      },
      contentType = esc_mime[[f]]
    )
  })


  # ========== PLACE THE HELPER FUNCTION HERE ==========
