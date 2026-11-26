# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s01_state  (sourced inside server(), local = TRUE)

  # Place the helper function here, right after server() definition
  save_heatmaply_static <- function(heatmaply_plot, width = 1200, height = 800) {
    # Create a temporary HTML file
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(heatmaply_plot, temp_html)
    
    # Create temporary PNG file
    temp_png <- tempfile(fileext = ".png")
    
    # Use webshot to capture
    webshot2::webshot(
      url     = temp_html,
      file    = temp_png,
      vwidth  = width,
      vheight = height,
      delay   = 2 # Give time for rendering
    )
    
    # Read and return the image
    img <- png::readPNG(temp_png)
    return(img)
  }
  
  # Then continue with your existing code...
  counter_file <- "visit_counter.txt"
  
  # track which data / analysis is "current"
  state <- reactiveValues(
    current_data_source = NULL, # "uni" or "multi"
    current_analysis    = NULL # "uni" or "pca"
  )
  
  pca_selected_ids <- reactiveVal(character(0))
  plsr_qt_selected_ids <- reactiveVal(character(0))
  
  data_store <- reactiveValues(
    uni_raw   = NULL,  # original univariate data from file
    uni_keep  = NULL,  # integer row indices to keep
    multi_raw = NULL,  # original multivariate data
    multi_keep = NULL  # integer row indices to keep
  )
  
  class_legend_title <- reactive({
    if (!is.null(input$multi_class_col) && nzchar(input$multi_class_col)) {
      input$multi_class_col
    } else {
      NULL
    }
  })
  
  
  
  transparent_legend <- ggplot2::theme(
    legend.background     = ggplot2::element_rect(fill = NA, colour = NA),
    legend.box.background = ggplot2::element_rect(fill = NA, colour = NA)
  )
  
  
  
  # ---------- MAP: load bgd_sf lazily ----------
  bgd_sf_cache <- reactiveVal(NULL)
  
  bgd_sf_data <- reactive({
    dat <- bgd_sf_cache()
    if (!is.null(dat)) {
      return(dat)
    }
    
    # load once from www
    load("www/bgd_sf.RData") # should create object bgd_sf
    bgd_sf_cache(bgd_sf)
    bgd_sf
  })
  
  
  # Data for selected admin level
  map_level_sf <- reactive({
    req(input$map_level)
    bgd_sf_data() %>% dplyr::filter(Level == input$map_level)
  })
  
  # Update name choices when level changes
  observeEvent(map_level_sf(), {
    nm <- sort(unique(map_level_sf()$Name))
    updateSelectizeInput(session, "map_names", choices = nm, server = TRUE)
  })
  
  # Selected polygons (possibly 0 rows)
  map_sel_sf <- reactive({
    dat <- map_level_sf()
    if (is.null(input$map_names) || length(input$map_names) == 0) {
      dat[0, ]
    } else {
      dat %>% dplyr::filter(Name %in% input$map_names)
    }
  })
  
  
  output$pca_click_debug <- renderPrint({
    ids <- unique(pca_selected_ids())
    if (length(ids) == 0) {
      cat("Click sample(s) in the PCA score plot to build a selection.\n")
    } else {
      cat("Selected Sample ID(s):", paste(ids, collapse = ", "), "\n")
    }
  })
  
  
  
  
