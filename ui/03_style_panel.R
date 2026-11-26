# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: plot style panel and download bar (shared by all graphs)

# Options that belong to one kind of plot only. Each block appears only while that plot is shown.
esc_plot_options <- function() {
  on_uni <- function(types) sprintf("input.graphs_subtab == 'General' && [%s].indexOf(input.graph_type_uni) >= 0",
                                   paste0("'", types, "'", collapse = ", "))
  score_cond <- paste(
    "(input.graphs_subtab == 'PCA' && ['pca_scores','pca_biplot','pca_loadings','pca_qt','pca_contrib'].indexOf(input.graph_type_pca) >= 0)",
    "(input.graphs_subtab == 'PLSR' && ['lv_scores','y_meas_vs_cv','y_meas_vs_pred','q_vs_t2'].indexOf(input.plsr_graph_type) >= 0)",
    "(input.graphs_subtab == 'PLS-DA' && ['scores','qt'].indexOf(input.da_plot) >= 0)", sep = " || ")
  profile_cond <- paste(
    "(input.graphs_subtab == 'PCA' && input.graph_type_pca == 'pca_loadings_profile')",
    "(input.graphs_subtab == 'Preprocessing' && input.graph_type_prep == 'prep_preview')",
    "(input.graphs_subtab == 'PLSR' && ['reg_vector','vip','sr','plsr_weights'].indexOf(input.plsr_graph_type) >= 0)",
    "(input.graphs_subtab == 'PLS-DA' && ['vip','coef','sr','weights'].indexOf(input.da_plot) >= 0)", sep = " || ")
  chk <- function(...) div(class = "esc-chk", checkboxInput(...))
  tagList(
    conditionalPanel(on_uni("simple_bar"),
      fluidRow(column(3, chk("use_facet", "Facet (one panel per response)", FALSE)),
               column(2, chk("use_unicolor", "Unicolor", FALSE)),
               column(2, chk("use_multicolor", "Multicolor", FALSE)))),
    conditionalPanel(on_uni("ggpairs"),
      fluidRow(column(2, chk("dens_hist_use_facet", "Use facet", FALSE)),
               column(3, conditionalPanel("input.dens_hist_use_facet",
                 radioButtons("dens_hist_facet_type", "Facet type", c("Density" = "density", "Histogram" = "hist"), inline = TRUE))),
               column(2, chk("dens_hist_use_bins", "Modify bins", FALSE)),
               column(2, conditionalPanel("input.dens_hist_use_bins", numericInput("dens_hist_bins", "Bins", 30, 5, 200, 1))))),
    conditionalPanel("input.graphs_subtab == 'PCA' && input.graph_type_pca == 'pca_scores'",
      fluidRow(column(2, chk("pca_modify_view", "Modify view", FALSE)),
               column(10, conditionalPanel("input.pca_modify_view",
                 fluidRow(column(4, uiOutput("pca_secid_1_ui"), checkboxInput("pca_shape_by_secid", "Change shape by Secondary ID 1", FALSE)),
                          column(4, uiOutput("pca_secid_2_ui"), checkboxInput("pca_size_by_secid", "Change size by Secondary ID 2", FALSE)),
                          column(4, uiOutput("pca_secid_3_ui"), checkboxInput("pca_color_by_secid", "Change color by Secondary ID 3", FALSE))))))),
    conditionalPanel(score_cond,
      fluidRow(column(3, selectInput("pca_pc_pair", "Component pair", c("PC1 vs PC2" = "1_2", "PC1 vs PC3" = "1_3", "PC2 vs PC3" = "2_3"), "1_2")),
               column(5, checkboxGroupInput("pca_scores_overlays", "Overlays",
                                            c("Confidence ellipse" = "ellipse", "Convex hull" = "hull", "Spokes" = "spokes"), inline = TRUE)),
               column(2, chk("pca_show_id", "Show ID labels", FALSE)),
               column(2, chk("pca_color_cos2", "Use cos\u00b2", FALSE)))),
    conditionalPanel(profile_cond,
      radioButtons("pca_load_profile_type", "Variable profile", c("Line" = "line", "Bar" = "bar"), "line", inline = TRUE)),
    conditionalPanel("input.main_tabs == 'Graphs'",
      fluidRow(column(4, chk("sty_facet_on", "Split into panels (facets)", FALSE))),
      conditionalPanel("input.sty_facet_on", uiOutput("sty_facet_ui")))
  )
}

esc_style_controls <- function() {
  c2 <- function(...) column(2, ...)
  tagList(
    tabsetPanel(
      id = "sty_tabs", type = "pills",
      tabPanel("This plot", esc_plot_options()),
      tabPanel("Text",
        fluidRow(
          c2(selectInput("sty_theme", "Theme",
            c("Keep" = "keep", "Classic" = "classic", "Black & white" = "bw",
              "Minimal" = "minimal", "Light" = "light", "Line drawing" = "linedraw"))),
          c2(selectInput("sty_family", "Font", ESC_FONTS, selected = "keep")),
          column(1, numericInput("sty_base_size", "Base", NA, 6, 30, 1)),
          column(1, numericInput("sty_title_size", "Title", NA, 6, 40, 1)),
          column(1, numericInput("sty_axis_title_size", "Axis title", NA, 6, 40, 1)),
          column(1, numericInput("sty_axis_text_size", "Axis text", NA, 4, 40, 1)),
          c2(textInput("sty_font_colour", "Font colour", "", placeholder = "black, #333")),
          c2(selectInput("sty_bold", "Titles", c("Keep" = "keep", "Bold" = "bold", "Plain" = "plain")))
        )),
      tabPanel("Color",
        fluidRow(
          column(3, selectInput("sty_palette", "Palette", c("Keep" = "default", names(ESC_PALETTES)))),
          column(5, textInput("sty_custom_colours", "Own colours (comma separated)", placeholder = "#1b9e77, #d95f02, grey40")),
          c2(selectInput("sty_legend", "Legend", c("Keep" = "keep", "Right" = "right", "Top" = "top",
                                                  "Bottom" = "bottom", "Left" = "left", "None" = "none"))),
          c2(selectInput("sty_grid", "Grid", c("Keep" = "keep", "Major only" = "major", "None" = "none")))
        )),
      tabPanel("Bars & symbols",
        fluidRow(
          c2(div(class = "esc-chk", checkboxInput("sty_bar_on", "Set bar size", FALSE))),
          c2(numericInput("sty_bar_width", "Bar width", 0.7, 0.1, 1, 0.05)),
          c2(numericInput("sty_bar_gap", "Gap in group", 0.1, 0, 0.9, 0.05)),
          c2(selectInput("sty_point_shape", "Symbol", c("Keep" = "keep", ESC_SHAPES))),
          c2(numericInput("sty_point_size", "Symbol size", NA, 0.2, 15, 0.5)),
          c2(numericInput("sty_line_width", "Line width", NA, 0.1, 5, 0.1))
        )),
      tabPanel("Axes",
        fluidRow(
          column(1, numericInput("sty_x_angle", "X angle", 0, -90, 90, 15)),
          column(1, numericInput("sty_y_angle", "Y angle", 0, -90, 90, 15)),
          column(1, div(class = "esc-chk", checkboxInput("sty_break_on", "Break", FALSE))),
          column(1, numericInput("sty_break_from", "From", NA)),
          column(1, numericInput("sty_break_to", "To", NA)),
          c2(div(class = "esc-chk", checkboxInput("sty_sec_on", "Second Y axis", FALSE))),
          column(1, numericInput("sty_sec_mult", "\u00d7 factor", 1)),
          column(1, numericInput("sty_sec_add", "+ offset", 0)),
          column(2, textInput("sty_sec_name", "Second axis title", ""))
        ),
        fluidRow(
          column(3, textInput("sty_xlab", "X axis title", "", placeholder = "blank = keep")),
          column(3, textInput("sty_ylab", "Y axis title", "", placeholder = "blank = keep")),
          column(1, numericInput("sty_xmin", "X start", NA)),
          column(1, numericInput("sty_xmax", "X end", NA)),
          column(1, numericInput("sty_ymin", "Y start", NA)),
          column(1, numericInput("sty_ymax", "Y end", NA))
        ))
    ),
    div(style = "display:flex; justify-content:space-between; align-items:center;",
        tags$small(style = "color:#666;", "Blank or 'Keep' leaves the graph unchanged. Second Y axis = primary \u00d7 factor + offset."),
        actionButton("sty_reset", "Reset", class = "btn-xs"))
  )
}

esc_download_bar <- function(prefix = "download_graph") {
  tagList(
    fluidRow(
      column(2, numericInput(paste0(prefix, "_w"), "Width (in)", 8, 2, 30, 0.5)),
      column(2, numericInput(paste0(prefix, "_h"), "Height (in)", 6, 2, 30, 0.5)),
      column(2, numericInput(paste0(prefix, "_dpi"), "DPI (PNG/TIFF)", 600, 72, 1200, 50))
    ),
    downloadButton(paste0(prefix, "_png"), "PNG"), tags$span(" "),
    downloadButton(paste0(prefix, "_tiff"), "TIFF"), tags$span(" "),
    downloadButton(paste0(prefix, "_pdf"), "PDF"), tags$span(" "),
    downloadButton(paste0(prefix, "_svg"), "SVG"), tags$span(" "),
    downloadButton(paste0(prefix, "_eps"), "EPS")
  )
}

ui_style_panel <- tags$details(
  class = "esc-style",
  tags$head(tags$style(HTML("
    .esc-style{border:1px solid #c9d6e3;border-radius:6px;padding:3px 10px;margin:6px 0;background:#f7fbff;
               max-width:1100px;font-size:12px}
    .esc-style summary{cursor:pointer;font-size:13px}
    .esc-style .nav-pills>li>a{padding:3px 10px;font-size:12px}
    .esc-style .tab-content{padding-top:4px}
    .esc-style .form-group{margin-bottom:3px}
    .esc-style label{font-size:11px;font-weight:600;margin-bottom:1px;white-space:nowrap}
    .esc-style .form-control,.esc-style .selectize-input{height:26px;min-height:26px;padding:2px 6px;font-size:12px}
    .esc-style .selectize-input{line-height:20px}
    .esc-style .row{margin-left:-4px;margin-right:-4px}
    .esc-style [class*='col-']{padding-left:4px;padding-right:4px}
    .esc-style .esc-chk{margin-top:18px}
    .esc-style .esc-chk .checkbox{margin:0}
    .esc-hint{background:#fff8e1;border-left:3px solid #f0ad4e;padding:5px 9px;margin:0 0 8px 0;font-size:12px}
    .esc-compact .form-group{margin-bottom:6px}
    .esc-compact label{font-size:12px;margin-bottom:2px}
    .esc-compact .row{margin-left:-5px;margin-right:-5px}
    .esc-compact [class*='col-']{padding-left:5px;padding-right:5px}
    .esc-compact .radio-inline{margin-right:6px}
    .esc-compact .help-block{font-size:11px;margin-top:0}
  "))),
  tags$head(tags$script(HTML("
    function escPlaceStyle() {
      var p = document.querySelector('details.esc-style'); if (!p) return;
      var g = document.querySelector('.tab-pane.active[data-value=\\'Graphs\\'], .tab-pane.active[data-value=\\'Maps\\']'); if (!g) return;
      var panes = Array.prototype.filter.call(g.querySelectorAll('.tab-pane.active'), function (e) { return !p.contains(e); });
      var sub = panes[panes.length - 1] || g;
      var plots = Array.prototype.filter.call(sub.querySelectorAll('.shiny-plot-output, .html-widget-output'),
                                              function (e) { return e.offsetParent !== null && !p.contains(e); });
      if (plots.length) { if (plots[0].previousElementSibling !== p) plots[0].parentNode.insertBefore(p, plots[0]); }
      else if (sub.firstChild !== p) sub.insertBefore(p, sub.firstChild);
    }
    $(document).on('shown.bs.tab', function () { setTimeout(escPlaceStyle, 60); });
    $(document).on('change', 'select, input', function (e) { if (!$(e.target).closest('details.esc-style').length) setTimeout(escPlaceStyle, 400); });
    $(document).on('shiny:value', function () { setTimeout(escPlaceStyle, 200); });
  "))),
  tags$summary(strong("Enhance Plots")),
  esc_style_controls()
)
