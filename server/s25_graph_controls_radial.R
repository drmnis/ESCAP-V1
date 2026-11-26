# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s25_graph_controls_radial  (sourced inside server(), local = TRUE)

  output$graph_var_ui <- renderUI({
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      return(NULL)
    }
    selectInput("graph_var", "Select variable for simple graph",
                choices = num_cols, selected = num_cols[1]
    )
  })
  
  
  output$ggpairs_vars_ui <- renderUI({
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      return(NULL)
    }
    selectInput(
      "ggpairs_vars",
      "Select numeric variables for pairs plot",
      choices  = num_cols,
      selected = num_cols[1:min(4, length(num_cols))],
      multiple = TRUE
    )
  })
  
  output$prep_preview_var_ui <- renderUI({
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    
    # Only show this control for the line preview mode
    if (!identical(input$graph_type_prep, "prep_preview")) {
      return(NULL)
    }
    
    if (!is.null(input$multi_id_col) && input$multi_id_col != "" &&
        input$multi_id_col %in% names(df)) {
      ID <- df[[input$multi_id_col]]
    } else {
      ID <- seq_len(nrow(df))
    }
    ID_chr <- as.character(ID)
    
    selectInput(
      "prep_preview_var",
      "Show",
      choices  = c("All" = "__ALL__", ID_chr),
      selected = "__ALL__"
    )
  })
  
  
  output$pairs_plot <- esc_renderPlot({
    req(input$graph_type_uni == "ggpairs")
    plots <- density_hist_plots()
    n <- length(plots)
    validate(need(n >= 1, "No plots available."))
    
    idx <- suppressWarnings(as.integer(input$simple_bar_which))
    if (is.na(idx) || idx < 1 || idx > n) idx <- 1
    
    print(plots[[idx]])
  })
  
  
  
  output$simple_bar_all <- esc_renderPlot({
    req(input$graph_type_uni == "simple_bar")
    plots <- simple_bar_plots()
    
    # Show them all together; 3 rows x 2 columns (last cell empty)
    patchwork::wrap_plots(plots, ncol = 2)
  })
  
  
  output$ggbetween_plot <- esc_renderPlot({
    req(input$graph_type_uni == "ggbetween")
    
    df <- dat_raw()
    req(primary_factor(), uni_resp())
    
    # Use primary factor as x, first selected response as y
    x_col <- primary_factor()
    # NEW: choose y based on user selection if available
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
    
    validate(
      need(x_col %in% names(df), "Selected factor column not found in data."),
      need(y_col %in% names(df), "Selected response column not found in data.")
    )
    
    # x must be factor, y must be numeric
    df[[x_col]] <- as.factor(df[[x_col]])
    df[[y_col]] <- suppressWarnings(as.numeric(df[[y_col]]))
    
    validate(
      need(
        !all(is.na(df[[y_col]])),
        "Selected response column must contain numeric values."
      )
    )
    
    # Axis labels: use column names (or your preferred label logic)
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
    
    plt
  })
  
  
  radial_df <- reactive({
    df <- dat_raw()
    req(uni_cond(), primary_factor(), uni_resp())
    
    ind_col <- uni_cond() # e.g. "individual"
    grp_col <- primary_factor() # e.g. "group"
    resp_cols <- uni_resp()
    
    validate(
      need(
        ind_col %in% names(df),
        "Radial plot: selected condition column not found in data."
      ),
      need(
        grp_col %in% names(df),
        "Radial plot: selected factor column not found in data."
      ),
      need(
        all(resp_cols %in% names(df)),
        "Radial plot: some selected response columns are not in the data."
      )
    )
    
    out <- df[, c(ind_col, grp_col, resp_cols), drop = FALSE]
    
    # Rename to the structure expected by prepare_radial_data:
    # col1 = individual, col2 = group, col3+ = numeric values
    names(out)[1:2] <- c("individual", "group")
    
    # types as expected by prepare_radial_data
    out[[1]] <- as.character(out[[1]]) # individual
    out[[2]] <- as.factor(out[[2]]) # group
    for (j in 3:ncol(out)) {
      out[[j]] <- suppressWarnings(as.numeric(out[[j]]))
    }
    
    out
  })
  
  
  output$radial_plot <- esc_renderPlot({
    req(input$graph_type_uni == "radial")
    
    # NEW: use the selected condition/factor/response columns
    df <- radial_df()
    rad <- prepare_radial_data(df, empty_bar = 2)
    
    data <- rad$data
    label_data <- rad$label_data
    base_data <- rad$base_data
    grid_data <- rad$grid_data
    number_of_bar <- rad$number_of_bar
    
    # Auto-scaling of label sizes based on number of bars
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
    
    # Choose more vibrant colors (viridis "C"), keep bars semi-transparent
    max_tot <- max(label_data$tot, na.rm = TRUE)
    if (!is.finite(max_tot)) max_tot <- 0
    
    p <- ggplot(data) +
      # Stacked bars
      geom_bar(
        aes(x = as.factor(id), y = value, fill = observation),
        stat  = "identity",
        alpha = 0.6 # transparent bars
      ) +
      scale_fill_viridis_d(
        option       = "C",
        name         = NULL, # remove legend title "observation"
        na.translate = FALSE # do not show NA level in legend
      ) +
      
      # Circular grid lines (values)
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
      
      # Value labels at outer edge
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
        plot.background  = element_rect(fill = NA, colour = NA) # transparent
      ) +
      coord_polar() +
      
      # Labels for each individual
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
      
      # Group baselines and labels
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
    
    p
  })
  
  
  observe({
    tab    <- input$graphs_subtab
    g_plsr <- input$plsr_graph_type
    
    # Helper to build all i<j pairs up to k
    make_pairs <- function(k, prefix) {
      if (is.null(k) || is.na(k) || k < 2) return(NULL)
      idx <- combn(seq_len(k), 2)
      codes <- apply(idx, 2, function(v) paste(v, collapse = "_"))
      labels <- apply(idx, 2, function(v) sprintf("%s%d vs %s%d", prefix, v[1], prefix, v[2]))
      choices <- setNames(as.list(codes), labels)
      choices
    }
    
    # --- PCA tab: PC pairs ---
    if (!is.null(tab) && tab == "PCA") {
      res <- tryCatch(pca_result(), error = function(e) NULL)
      if (is.null(res)) {
        # before PCA is run: simple default
        updateSelectInput(
          session,
          "pca_pc_pair",
          label   = "Choose: For plottings",
          choices = c("PC1 vs PC2" = "1_2"),
          selected = "1_2"
        )
        return()
      }
      
      n_pc <- ncol(res$pca$x)
      if (n_pc < 2) return()
      
      # how many PCs to keep for plotting (capped by actual PCs)
      keep <- input$pca_n_keep %||% 3
      if (is.na(keep) || keep < 2) keep <- 2
      keep <- min(keep, n_pc)
      
      choices <- make_pairs(keep, "PC")
      if (is.null(choices)) return()
      
      # keep current selection if still valid, else default to first
      cur <- isolate(input$pca_pc_pair)
      if (is.null(cur) || !cur %in% unlist(choices)) {
        cur <- choices[[1]]
      }
      
      updateSelectInput(
        session,
        "pca_pc_pair",
        label    = "Choose: For plottings",
        choices  = choices,
        selected = cur
      )
      return()
    }
    
    # --- PLS-DA tab: LV pairs for the score plot ---
    if (!is.null(tab) && tab == "PLS-DA") {
      r <- tryCatch(da_result(), error = function(e) NULL)
      n_lv <- if (!is.null(r$scores)) ncol(r$scores) else 2
      choices <- make_pairs(max(2, min(10, n_lv)), "LV")
      cur <- isolate(input$pca_pc_pair)
      if (is.null(cur) || !cur %in% unlist(choices)) cur <- choices[[1]]
      updateSelectInput(session, "pca_pc_pair", label = "LV pair", choices = choices, selected = cur)
      return()
    }

    # --- PLSR tab: LV pairs (for score plot etc.) ---
    if (!is.null(tab) && tab == "PLSR") {
      res_pls <- tryCatch(plsr_result(), error = function(e) NULL)
      if (is.null(res_pls) || is.null(res_pls$fit)) {
        updateSelectInput(
          session,
          "pca_pc_pair",
          label   = "Choose: For plottings",
          choices = c("LV1 vs LV2" = "1_2"),
          selected = "1_2"
        )
        return()
      }
      
      # how many LVs are actually in the model (cap at e.g. 10)
      n_lv <- ncol(scores(res_pls$fit))
      if (is.null(n_lv) || n_lv < 2) return()
      keep_lv <- min(10, n_lv)
      
      choices <- make_pairs(keep_lv, "LV")
      if (is.null(choices)) return()
      
      cur <- isolate(input$pca_pc_pair)
      if (is.null(cur) || !cur %in% unlist(choices)) {
        cur <- choices[[1]]
      }
      
      updateSelectInput(
        session,
        "pca_pc_pair",
        label    = "Choose: For plottings",
        choices  = choices,
        selected = cur
      )
      return()
    }
  })
  
  
  
