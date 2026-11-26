# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s00_style  (sourced first inside server(), local = TRUE)
# Collects the author's plot-style choices and applies them to every ggplot that is
# printed, rendered or downloaded anywhere in the app.

  esc_current_style <- reactive({
    st <- esc_style_defaults()
    num <- function(x, d = NA) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0 || is.na(v)) d else v }
    st$theme <- input$sty_theme %||% "keep"
    st$family <- input$sty_family %||% "keep"
    st$base_size <- num(input$sty_base_size)
    st$title_size <- num(input$sty_title_size)
    st$axis_title_size <- num(input$sty_axis_title_size)
    st$axis_text_size <- num(input$sty_axis_text_size)
    st$font_colour <- trimws(input$sty_font_colour %||% "")
    st$bold <- input$sty_bold %||% "keep"
    st$palette <- input$sty_palette %||% "default"
    st$custom_colours <- input$sty_custom_colours %||% ""
    st$bar_width <- if (isTRUE(input$sty_bar_on)) num(input$sty_bar_width) else NA
    st$bar_gap <- if (isTRUE(input$sty_bar_on)) num(input$sty_bar_gap) else NA
    st$point_shape <- if (identical(input$sty_point_shape, "keep")) NA else num(input$sty_point_shape)
    st$point_size <- num(input$sty_point_size)
    st$line_width <- num(input$sty_line_width)
    st$x_angle <- num(input$sty_x_angle, 0)
    st$y_angle <- num(input$sty_y_angle, 0)
    st$legend <- input$sty_legend %||% "keep"
    st$grid <- input$sty_grid %||% "keep"
    st$break_on <- isTRUE(input$sty_break_on)
    st$break_from <- num(input$sty_break_from); st$break_to <- num(input$sty_break_to)
    st$sec_on <- isTRUE(input$sty_sec_on)
    st$sec_mult <- num(input$sty_sec_mult, 1); st$sec_add <- num(input$sty_sec_add, 0)
    st$sec_name <- input$sty_sec_name %||% ""
    st$xlab <- input$sty_xlab %||% ""; st$ylab <- input$sty_ylab %||% ""
    st$xmin <- num(input$sty_xmin); st$xmax <- num(input$sty_xmax)
    st$ymin <- num(input$sty_ymin); st$ymax <- num(input$sty_ymax)
    st$facet_by <- if (isTRUE(input$sty_facet_on)) setdiff(input$sty_facet_by %||% character(0), "") else character(0)
    st$facet_layout <- input$sty_facet_layout %||% "grid"
    st$facet_ncol <- num(input$sty_facet_ncol)
    st$facet_scales <- input$sty_facet_scales %||% "fixed"
    st
  })
  esc_style_now <- function() tryCatch(esc_current_style(), error = function(e) NULL)

  # Every ggplot printed by server code (graph downloads, draw_current_graph) is styled
  print <- function(x, ...) {
    if (inherits(x, "ggplot") && isTRUE(esc_capture$on)) { esc_capture$p <- x; return(invisible(x)) }  # facet-choice probe
    if (inherits(x, "ggplot")) x <- esc_apply_style(x, esc_style_now())
    base::print(x, ...)
  }
  # Interactive (plotly) views get the same styling before conversion
  esc_ggplotly <- function(p, ...) {
    if (inherits(p, "ggplot")) p <- esc_apply_style(p, esc_style_now())
    plotly::ggplotly(p, ...)
  }
  # renderPlot() wrapper: styles a returned ggplot, leaves base graphics untouched
  esc_renderPlot <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr)
    wrapped <- bquote({
      .r <- withVisible(.(expr))
      if (inherits(.r$value, "ggplot")) .r$value <- esc_apply_style(.r$value, esc_style_now())
      if (.r$visible) .r$value else invisible(.r$value)
    })
    shiny::renderPlot(wrapped, ..., env = env, quoted = TRUE)
  }

  observeEvent(input$sty_reset, {
    updateSelectInput(session, "sty_theme", selected = "keep")
    updateSelectInput(session, "sty_family", selected = "keep")
    for (i in c("sty_base_size", "sty_title_size", "sty_axis_title_size", "sty_axis_text_size", "sty_point_size", "sty_line_width",
                "sty_break_from", "sty_break_to")) updateNumericInput(session, i, value = NA)
    updateTextInput(session, "sty_font_colour", value = "")
    updateSelectInput(session, "sty_bold", selected = "keep"); updateSelectInput(session, "sty_legend", selected = "keep")
    updateSelectInput(session, "sty_grid", selected = "keep"); updateSelectInput(session, "sty_point_shape", selected = "keep")
    updateNumericInput(session, "sty_x_angle", value = 0); updateNumericInput(session, "sty_y_angle", value = 0)
    updateSelectInput(session, "sty_palette", selected = "default")
    updateTextInput(session, "sty_custom_colours", value = "")
    updateCheckboxInput(session, "sty_break_on", value = FALSE)
    updateCheckboxInput(session, "sty_sec_on", value = FALSE)
    updateCheckboxInput(session, "sty_bar_on", value = FALSE)
    for (i in c("sty_xmin", "sty_xmax", "sty_ymin", "sty_ymax")) updateNumericInput(session, i, value = NA)
    updateTextInput(session, "sty_xlab", value = ""); updateTextInput(session, "sty_ylab", value = "")
    updateCheckboxInput(session, "sty_facet_on", value = FALSE)
  })

  # ---- facet choices: the grouping columns of the plot that is on screen ----------------
  esc_capture <- new.env(); esc_capture$on <- FALSE
  output$sty_facet_ui <- renderUI({
    req(isTRUE(input$sty_facet_on))
    p <- tryCatch(current_graph_object(), error = function(e) NULL)
    if (inherits(p, "patchwork") && length(p$patches$plots)) p <- p$patches$plots[[1]]
    d <- if (inherits(p, "ggplot") && is.data.frame(p$data)) p$data else NULL
    cand <- if (is.null(d)) character(0) else
      names(d)[vapply(d, function(x) (is.factor(x) || is.character(x) || is.logical(x)) && length(unique(x)) %in% 2:30, logical(1))]
    if (!length(cand)) return(helpText("This plot has no grouping variable that can be split into panels."))
    cur <- isolate(input$sty_facet_by)
    tagList(
      fluidRow(
        column(5, selectizeInput("sty_facet_by", "Split into panels by (1 or 2)", cand, selected = intersect(cur, cand),
                                 multiple = TRUE, options = list(maxItems = 2, placeholder = "choose variable(s)"))),
        column(3, selectInput("sty_facet_layout", "Layout", c("Grid (rows \u00d7 columns)" = "grid", "Wrap" = "wrap"),
                              selected = isolate(input$sty_facet_layout) %||% "grid")),
        column(2, numericInput("sty_facet_ncol", "Columns (wrap)", isolate(input$sty_facet_ncol) %||% NA, 1, 10, 1)),
        column(2, selectInput("sty_facet_scales", "Axes", c("Same" = "fixed", "Free y" = "free_y", "Free x" = "free_x", "Free" = "free"),
                              selected = isolate(input$sty_facet_scales) %||% "fixed"))
      ),
      tags$small(style = "color:#666;", "Grid: the first variable gives the rows, the second the columns. The downloaded figure keeps the panels.")
    )
  })
