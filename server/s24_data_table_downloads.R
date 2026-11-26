# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s24_data_table_downloads  (sourced inside server(), local = TRUE)

  observeEvent(input$data_show_all, {
    viewdata_show_all$on <- TRUE
  })
  
  observeEvent(input$data_hide, {
    viewdata_show_all$on <- FALSE
  })
  
  
  # 9. View Uploaded Data (latest file) ------------------------------------
  output$data_preview_dt <- DT::renderDataTable({
    # decide which filtered data is currently active
    df <- NULL
    path <- NULL
    if (state$current_data_source == "uni" && !is.null(input$file_uni)) {
      df <- dat_raw()
      path <- input$file_uni$datapath
    } else if (state$current_data_source == "multi" && !is.null(input$file_multi)) {
      df <- dat_multi()
      path <- input$file_multi$datapath
    } else if (!is.null(input$file_uni)) {
      df <- dat_raw()
      path <- input$file_uni$datapath
    } else if (!is.null(input$file_multi)) {
      df <- dat_multi()
      path <- input$file_multi$datapath
    } else {
      return(NULL)
    }
    
    ## NEW: apply Show all / Hide limits
    v <- data_view()
    nr <- nrow(df)
    nc <- ncol(df)
    r_show <- if (is.infinite(v$nrows)) nr else min(v$nrows, nr)
    c_show <- if (is.infinite(v$ncols)) nc else min(v$ncols, nc)
    df <- df[seq_len(r_show), seq_len(c_show), drop = FALSE]
    
    # Show the cleaned column names: these are the names used in every drop-down of the app.
    # (The old code re-read the header with a plain comma split, which broke on quoted names,
    #  Excel files and other encodings and caused the DataTables "searchable" warning.)
    df_print <- df
    names(df_print) <- make.unique(ifelse(is.na(names(df_print)) | !nzchar(names(df_print)), "V", names(df_print)))
    DT::datatable(
      df_print,
      rownames  = TRUE,
      selection = list(mode = "multiple", target = "row"),
      options   = list(
        paging       = FALSE,   # all rows on one page
        searching    = TRUE,
        ordering     = TRUE,
        info         = TRUE,
        lengthChange = FALSE,
        scrollX      = TRUE,
        autoWidth    = FALSE
      ),
      width = "100%"
    )
  })
  
  
  
  
  observeEvent(input$delete_selected_rows, {
    sel <- input$data_preview_dt_rows_selected
    if (length(sel) == 0) return()
    
    if (state$current_data_source == "uni" && !is.null(input$file_uni)) {
      keep <- data_store$uni_keep
      if (is.null(keep) || length(keep) == 0) return()
      # sel indexes rows in the filtered data -> positions in keep
      data_store$uni_keep <- keep[-sel]
      
    } else if (state$current_data_source == "multi" && !is.null(input$file_multi)) {
      keep <- data_store$multi_keep
      if (is.null(keep) || length(keep) == 0) return()
      data_store$multi_keep <- keep[-sel]
    }
  })
  
  observeEvent(input$use_selected_rows, {
    sel <- input$data_preview_dt_rows_selected
    if (length(sel) == 0) return()
    
    if (state$current_data_source == "uni" && !is.null(input$file_uni)) {
      keep <- data_store$uni_keep
      if (is.null(keep) || length(keep) == 0) return()
      # keep only selected positions
      data_store$uni_keep <- keep[sel]
      
    } else if (state$current_data_source == "multi" && !is.null(input$file_multi)) {
      keep <- data_store$multi_keep
      if (is.null(keep) || length(keep) == 0) return()
      data_store$multi_keep <- keep[sel]
    }
  })
  
  
  
  observeEvent(input$reset_row_filter, {
    if (!is.null(data_store$uni_raw)) {
      data_store$uni_keep <- seq_len(nrow(data_store$uni_raw))
    }
    if (!is.null(data_store$multi_raw)) {
      data_store$multi_keep <- seq_len(nrow(data_store$multi_raw))
    }
  })
  
  
  
  # 10. Example file downloads ---------------------------------------------
  # example data listed on the About tab (files in example_data/)
  for (i in seq_len(nrow(ESC_EXAMPLES))) local({
    f <- ESC_EXAMPLES$file[i]
    output[[paste0("ex_dl_", i)]] <- downloadHandler(filename = function() f,
      content = function(file) file.copy(file.path("example_data", f), file), contentType = "text/csv")
  })
  output$download_example_csv <- downloadHandler(
    filename = function() "example_data.csv",
    content = function(file) {
      file.copy("www/example_data.csv", file)
    },
    contentType = "text/csv"
  )
  
  
  # Radial plot example CSV (for Graphs -> Univariate -> Circular stacked bar)
  output$download_radial_example <- downloadHandler(
    filename = function() "circular_stacked_bar_plot_data.csv",
    content = function(file) {
      # Put a file named 'radial_example.csv' under www/ in your app directory
      file.copy("www/circular_stacked_bar_plot_data.csv", file)
    },
    contentType = "text/csv"
  )
  
  # Simple Bar example CSV (for Graphs -> Univariate simple bar)
  output$download_simple_bar_example <- downloadHandler(
    filename = function() "simple_bar_plot_data.csv",
    content = function(file) {
      file.copy("www/simple_bar_plot_data.csv", file)
    },
    contentType = "text/csv"
  )
  
  
  # Simple Bar example CSV (for Graphs -> Univariate simple bar)
  output$download_linear_group_example <- downloadHandler(
    filename = function() "grouped_bar_plot_data.csv",
    content = function(file) {
      file.copy("www/grouped_bar_plot_data.csv", file)
    },
    contentType = "text/csv"
  )
  
  
  # Violin plot example CSV
  output$download_ggbetween_example <- downloadHandler(
    filename = function() "violin_plot_with_statistics_data.csv",
    content = function(file) {
      file.copy("www/violin_plot_with_statistics_data.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$download_heatmap_example <- downloadHandler(
    filename = function() "mtcars.csv",
    content = function(file) {
      file.copy("www/mtcars.csv", file)
    },
    contentType = "text/csv"
  )
  
  
  output$download_example_pdf <- downloadHandler(
    filename = function() "example_instructions.pdf",
    content = function(file) {
      file.copy("www/example_instructions.pdf", file)
    },
    contentType = "application/pdf"
  )
  
  # Citation (EndNote) download ---------------------------------------------
  output$download_citation <- downloadHandler(
    filename = function() "citation.enw",
    content = function(file) {
      file.copy("www/citation.enw", file)
    },
    contentType = "application/x-endnote-refer"
  )
  
  # 11. R session info ------------------------------------------------------
  output$session_info <- renderPrint({
    sessionInfo()
  })
  
  # 12. Graph controls + PNG download --------------------------------------
