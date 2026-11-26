# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s06_session_data_view  (sourced inside server(), local = TRUE)

  save_heatmaply_static <- function(heatmaply_plot, width = 1200, height = 800) {
    # Create a temporary HTML file
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(heatmaply_plot, temp_html)
    
    # Create temporary PNG file
    temp_png <- tempfile(fileext = ".png")
    
    # Use webshot to capture (try webshot2 first, then webshot)
    if (requireNamespace("webshot2", quietly = TRUE)) {
      webshot2::webshot(
        url     = temp_html,
        file    = temp_png,
        vwidth  = width,
        vheight = height,
        delay   = 2
      )
    } else if (requireNamespace("webshot", quietly = TRUE)) {
      webshot::webshot(
        url     = temp_html,
        file    = temp_png,
        vwidth  = width,
        vheight = height,
        delay   = 2
      )
    } else {
      stop("Please install 'webshot' or 'webshot2' package for heatmap downloads")
    }
    
    # Read and return the image
    img <- png::readPNG(temp_png)
    return(img)
  }
  
  
  counter_file <- "visit_counter.txt"
  
  # track which data / analysis is "current"
  state <- reactiveValues(
    current_data_source = NULL, # "uni" or "multi"
    current_analysis    = NULL # "uni" or "pca"
  )
  
  viewdata_show_all <- reactiveValues(
    on = FALSE
  )
  
  
  
  selected_overlays <- reactive({
    input$pca_scores_overlays %||% character(0)
  })
  
  
  heatmap_state <- reactiveValues(
    source  = "uni", # "uni" (default) or "corr"
    corr_mat = NULL # will store a correlation matrix
  )
  
  observeEvent(input$file_uni, {
    heatmap_state$source <- "uni"
    heatmap_state$corr_mat <- NULL
  })
  
  
  # --- View controller for data_preview ---
  data_view <- reactiveVal(list(nrows = 20L, ncols = 15L))
  
  observeEvent(input$data_show_all, {
    data_view(list(nrows = Inf, ncols = Inf))
  })
  
  observeEvent(input$data_hide, {
    data_view(list(nrows = 20L, ncols = 15L))
  })
  
  output$data_view_info <- renderText({
    # Determine which data frame is current (same logic you already use)
    df <- NULL
    if (state$current_data_source == "uni" && !is.null(input$file_uni)) {
      df <- dat_raw()
    } else if (state$current_data_source == "multi" && !is.null(input$file_multi)) {
      df <- dat_multi()
    } else if (!is.null(input$file_uni)) {
      df <- dat_raw()
    } else if (!is.null(input$file_multi)) {
      df <- dat_multi()
    }
    if (is.null(df)) {
      return("")
    }
    
    v <- data_view()
    total_r <- nrow(df)
    total_c <- ncol(df)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    show_c <- if (is.infinite(v$ncols)) total_c else min(v$ncols, total_c)
    
    if (show_r == total_r && show_c == total_c) {
      sprintf("Showing all %d rows and %d columns.", total_r, total_c)
    } else {
      sprintf(
        "Showing first %d of %d rows and first %d of %d columns.",
        show_r, total_r, show_c, total_c
      )
    }
  })
  
  
  pca_show <- reactiveValues(
    eig    = FALSE,
    load   = FALSE,
    scores = FALSE
  )
  
  
  pca_cluster_state <- reactiveValues(
    hclust = NULL,
    updated_at = NULL
  )
  
  
  observeEvent(input$pca_make_dendro, {
    res <- tryCatch(pca_result(), error = function(e) NULL)
    if (is.null(res)) {
      showNotification("Run PCA first, then click 'Cluster Dendrogram'.",
                       type = "error", duration = 5)
      return()
    }
    
    scores <- as.data.frame(res$pca$x)
    if (nrow(scores) < 2) {
      showNotification("Not enough samples for clustering.", type = "error", duration = 5)
      return()
    }
    
    # --- IMPORTANT: use ID (or serials) as rownames for clustering ---
    # res$ID already comes from multi_id_col if selected,
    # otherwise it is 1:n as character.
    if (!is.null(res$ID) && length(res$ID) == nrow(scores)) {
      rownames(scores) <- res$ID
    } else {
      rownames(scores) <- seq_len(nrow(scores))
    }
    
    # Use first few PCs (e.g., up to 5) for distance
    k <- min(5, ncol(scores))
    dist_mat <- dist(scores[, seq_len(k), drop = FALSE])
    hc <- hclust(dist_mat, method = "ward.D2")
    
    pca_cluster_state$hclust     <- hc
    pca_cluster_state$updated_at <- Sys.time()
    
    showNotification("Hierarchical clustering computed from PCA scores.",
                     type = "message", duration = 4)
  })
  
  
  
  
  observeEvent(input$clear_all, {
    session$reload()
  })
  
  
  # Initialize counter from file (or 0 if file doesn't exist)
  if (file.exists(counter_file)) {
    visit_counter <<- as.integer(readLines(counter_file, warn = FALSE)[1])
    if (is.na(visit_counter)) visit_counter <<- 0L
  } else {
    visit_counter <<- 0L
  }
  
  # visit counter: increment once per session
  observeEvent(session$clientData$url_hostname,
               {
                 visit_counter <<- visit_counter + 1L
                 # write back to file
                 writeLines(as.character(visit_counter), counter_file)
                 output$visit_count <- renderText(format(visit_counter, big.mark = ","))
               },
               once = TRUE
  )
  
  
  observeEvent(input$file_uni, {
    req(input$file_uni)
    state$current_data_source <- "uni"
  })
  observeEvent(input$file_multi, {
    req(input$file_multi)
    state$current_data_source <- "multi"
  })
  
  # 1. Read data and basic checks (univariate) ------------------------------
  
  
  chi_view <- reactiveVal(list(nrows = 50L))
  
  observeEvent(input$chi_show_all, {
    chi_view(list(nrows = Inf))
  })
  observeEvent(input$chi_hide, {
    chi_view(list(nrows = 50L))
  })
  
  output$chi_view_info <- renderText({
    tab <- chi_original_table(chi_result(), html = FALSE)
    if (is.null(tab)) {
      return("")
    }
    v <- chi_view()
    total_r <- nrow(tab)
    show_r <- if (is.infinite(v$nrows)) total_r else min(v$nrows, total_r)
    if (show_r == total_r) {
      sprintf("Showing all %d rows.", total_r)
    } else {
      sprintf("Showing first %d of %d rows.", show_r, total_r)
    }
  })
  
  
  observeEvent(input$file_uni, {
    req(input$file_uni)
    df <- tryCatch(esc_read_data(input$file_uni$datapath, input$file_uni$name),
                   error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error"); NULL })
    req(df)
    validate(
      need(is.data.frame(df), "Uploaded file is not a data.frame."),
      need(ncol(df) >= 2, "Need at least two columns (treatment and response)."),
      need(anyDuplicated(names(df)) == 0, "Column names must be unique.")
    )
    data_store$uni_orig <- attr(df, "orig_names")
    data_store$uni_raw  <- df
    data_store$uni_keep <- seq_len(nrow(df))  # keep all rows initially
  })
  
