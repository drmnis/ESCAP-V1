# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_examples (example data sets, listed in R/example_data.R)

ui_tab_examples <- tabPanel(
  "Example data",
  tags$style(HTML("
    .esc-ex { width:100%; max-width:1200px; border-collapse:collapse; font-size:13px; margin-top:6px; }
    .esc-ex th { background:#1f4e79; color:#fff; padding:7px 9px; text-align:left; font-weight:600; }
    .esc-ex td { padding:6px 9px; border-bottom:1px solid #e3e8ee; vertical-align:top; }
    .esc-ex tr.esc-ex-sec td { background:#eaf1f8; color:#1f4e79; font-weight:700; padding:6px 9px; border-bottom:1px solid #c9d8e8; }
    .esc-ex tbody tr:not(.esc-ex-sec):hover td { background:#f7fafc; }
    .esc-ex td.esc-ex-size { white-space:nowrap; color:#555; text-align:right; }
    .esc-ex a.shiny-download-link { font-weight:600; }
    .esc-ex .esc-ex-dl { font-size:12px; margin-right:5px; }
  ")),
  br(),
  h4("Example data"),
  p("Each file is ready to upload and shows one type of analysis. Click a file name to download it, upload it in the panel ",
    "shown under ", em("Upload to"), ", then choose the analysis and settings listed. Column names keep their units and ",
    "symbols (\u00b0Brix, L*, %); ESCAP handles them."),
  tags$table(class = "esc-ex",
    tags$thead(tags$tr(tags$th("File"), tags$th("Upload to"), tags$th("Analysis or graph"),
                       tags$th("Settings"), tags$th(style = "text-align:right;", "Rows \u00d7 columns"))),
    tags$tbody({
      rows <- list(); last <- ""
      for (i in seq_len(nrow(ESC_EXAMPLES))) {
        ex <- ESC_EXAMPLES[i, ]
        if (ex$section != last) {
          rows[[length(rows) + 1]] <- tags$tr(class = "esc-ex-sec", tags$td(colspan = 5, ex$section))
          last <- ex$section
        }
        rows[[length(rows) + 1]] <- tags$tr(
          tags$td(icon("download", class = "esc-ex-dl"), downloadLink(paste0("ex_dl_", i), ex$file)),
          tags$td(ex$upload), tags$td(ex$analysis), tags$td(ex$settings),
          tags$td(class = "esc-ex-size", ex$size))
      }
      rows
    })),
  br(),
  p(style = "font-size:12px; color:#555;",
    "The NIR file is the public gasoline data set (60 samples, 401 wavelengths, 900\u20131700 nm; Kalivas, 1997) distributed with the R package ",
    em("pls"), ". The other files are provided for demonstration and practice.")
)
