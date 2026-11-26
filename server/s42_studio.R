# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s42_studio  (sourced inside server(), local = TRUE)
# Plot studio: plotting functions are in R/studio_plots.R.

  ps_data <- reactive(if (identical(input$ps_source, "multi")) dat_multi() else dat_raw())
  ps_type <- reactive(sub("^ps_", "", input$graph_type_uni %||% "ps_scatter"))

  output$ps_controls <- renderUI({
    df <- tryCatch(ps_data(), error = function(e) NULL)
    if (is.null(df)) return(helpText("Upload a data file in the sidebar first."))
    num <- names(df)[vapply(df, is.numeric, logical(1))]; allc <- names(df)
    none <- c("None" = "")
    switch(ps_type(),
      scatter = tagList(
        selectInput("ps_x", "X", num, num[1]), selectInput("ps_y", "Y", num, num[min(2, length(num))]),
        selectInput("ps_group", "Colour / symbol by", c(none, allc)),
        checkboxInput("ps_shape_group", "Different symbol per group", TRUE),
        selectInput("ps_shape", "Symbol (single group)", ESC_SHAPES, 16),
        sliderInput("ps_size", "Symbol size", 0.5, 8, 2.5, 0.5),
        selectInput("ps_line", "Line", c("None" = "none", "Connect points" = "connect", "Linear fit" = "lm",
                                         "Quadratic fit" = "poly2", "LOESS smooth" = "loess")),
        checkboxInput("ps_se", "Confidence band", TRUE), checkboxInput("ps_eq", "Show equation and R\u00b2", FALSE),
        selectInput("ps_labels", "Point labels", c(none, allc)), selectInput("ps_facet", "Panels by", c(none, allc))),
      ts = tagList(
        selectInput("ps_x", "Time / date column", allc, allc[1]),
        selectizeInput("ps_ys", "Response(s)", num, num[1], multiple = TRUE, options = list(plugins = list("remove_button"))),
        selectInput("ps_group", "Group / treatment", c(none, allc)),
        radioButtons("ps_err", "Error bars", c("SE" = "se", "SD" = "sd", "None" = "none"), inline = TRUE),
        checkboxInput("ps_points", "Show points", TRUE), checkboxInput("ps_facet_vars", "One panel per response", TRUE),
        checkboxInput("ps_smooth", "Add LOESS trend", FALSE), textInput("ps_datefmt", "Date label format", "%d %b %Y")),
      dual = tagList(
        selectInput("ps_x", "X (category or time)", allc, allc[1]),
        selectInput("ps_y1", "Left Y", num, num[1]), radioButtons("ps_type1", "Left Y as", c("Bars" = "bar", "Line" = "line"), inline = TRUE),
        selectInput("ps_y2", "Right Y (line)", num, num[min(2, length(num))]),
        textInput("ps_col1", "Left colour", "#4D4D4D"), textInput("ps_col2", "Right colour", "#D55E00"),
        helpText("Replicates are averaged per X. The right axis is rescaled automatically to share the plotting area.")),
      contour = tagList(
        selectInput("ps_x", "X", num, num[1]), selectInput("ps_y", "Y", num, num[min(2, length(num))]),
        selectInput("ps_z", "Z (response)", num, num[min(3, length(num))]),
        selectInput("ps_ctype", "Style", c("Filled contour" = "filled", "Contour lines" = "lines", "Heat map + contours" = "heat")),
        selectInput("ps_interp", "Surface", c("Linear interpolation" = "linear", "Spline (Akima)" = "spline", "LOESS smoothing" = "loess")),
        sliderInput("ps_bins", "Contour levels", 4, 25, 10, 1),
        selectInput("ps_pal", "Palette", c("viridis", "magma", "inferno", "plasma", "cividis")),
        checkboxInput("ps_cpoints", "Show design points", TRUE)),
      spider = tagList(
        selectizeInput("ps_vars", "Attributes (3 or more)", num, head(num, 6), multiple = TRUE, options = list(plugins = list("remove_button"))),
        selectInput("ps_group", "Group (one polygon each, means)", c(none, allc)),
        radioButtons("ps_scale", "Scale", c("0-1 per attribute" = "minmax", "Raw means" = "none", "z-score" = "zscore"), inline = TRUE),
        checkboxInput("ps_fill", "Filled polygons", TRUE), checkboxInput("ps_sfacet", "One panel per group", FALSE))
    )
  })

  ps_plot_obj <- reactive({
    df <- ps_data(); ty <- ps_type()
    nz <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
    switch(ty,
      scatter = { req(input$ps_x, input$ps_y)
        esc_plot_scatter(df, input$ps_x, input$ps_y, nz(input$ps_group), isTRUE(input$ps_shape_group),
                         as.numeric(input$ps_shape %||% 16), input$ps_size %||% 2.5, input$ps_line %||% "none",
                         isTRUE(input$ps_se), nz(input$ps_labels), nz(input$ps_facet), show_eq = isTRUE(input$ps_eq)) },
      ts = { req(input$ps_x, input$ps_ys)
        esc_plot_timeseries(df, input$ps_x, input$ps_ys, nz(input$ps_group), input$ps_err %||% "se", isTRUE(input$ps_points),
                            isTRUE(input$ps_facet_vars), isTRUE(input$ps_smooth), input$ps_datefmt) },
      dual = { req(input$ps_x, input$ps_y1, input$ps_y2)
        esc_plot_dual_y(df, input$ps_x, input$ps_y1, input$ps_y2, input$ps_type1 %||% "bar", input$ps_col1, input$ps_col2) },
      contour = { req(input$ps_x, input$ps_y, input$ps_z)
        validate(need(length(unique(c(input$ps_x, input$ps_y, input$ps_z))) == 3, "X, Y and Z must be three different columns."))
        tryCatch(esc_plot_contour(df, input$ps_x, input$ps_y, input$ps_z, input$ps_ctype, input$ps_bins, 100,
                                  input$ps_interp, input$ps_pal, isTRUE(input$ps_cpoints)),
                 error = function(e) validate(need(FALSE, paste("Contour:", conditionMessage(e),
                   "- the X-Y points must span a 2-D region (at least 3 distinct X and Y values)."))))
      },
      spider = { req(length(input$ps_vars) >= 3)
        esc_plot_spider(df, input$ps_vars, nz(input$ps_group), input$ps_scale %||% "minmax", isTRUE(input$ps_fill), isTRUE(input$ps_sfacet)) })
  })
  output$ps_plot <- esc_renderPlot({ ps_plot_obj() })

  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) local({
    f <- fmt
    output[[paste0("dl_studio_", f)]] <- downloadHandler(
      filename = function() paste0("ESCAP_", ps_type(), "_plot.", f),
      content = function(file) {
        p <- esc_apply_style(ps_plot_obj(), esc_style_now())
        esc_open_device(file, f, input$dl_studio_w %||% 8, input$dl_studio_h %||% 6, input$dl_studio_dpi %||% 600)
        on.exit(grDevices::dev.off(), add = TRUE); esc_draw(p)
      }, contentType = esc_mime[[f]])
  })
