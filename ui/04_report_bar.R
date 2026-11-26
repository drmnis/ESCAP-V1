# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: report bar (PDF / Markdown / CSV download of the tables in a results tab)

esc_report_bar <- function(id) {
  div(
    style = "display:flex; flex-wrap:wrap; gap:8px; align-items:flex-end; background:#eef5ec; border:1px solid #cfe3c9; border-radius:6px; padding:6px 10px; margin-bottom:10px;",
    div(style = "width:110px;", selectInput(paste0(id, "_paper"), "Paper", names(ESC_PAPERS), "A4")),
    div(style = "width:140px;", selectInput(paste0(id, "_orient"), "Orientation",
                                            c("Automatic" = "auto", "Portrait" = "portrait", "Landscape" = "landscape"))),
    div(style = "margin-bottom:15px;", downloadButton(paste0(id, "_pdf"), "Report (PDF)")),
    div(style = "margin-bottom:15px;", downloadButton(paste0(id, "_docx"), "Report (Word)")),
    div(style = "margin-bottom:15px;", downloadButton(paste0(id, "_md"), "Report (Markdown)")),
    div(style = "margin-bottom:15px;", downloadButton(paste0(id, "_csv"), "Tables (CSV)"))
  )
}

# Show results only after the analysis has produced them; before that a short hint says what to do.
# 'flag' is a server output (TRUE / FALSE) defined in server/s45_ready.R.
esc_gate <- function(flag, hint, ...) {
  tagList(
    conditionalPanel(sprintf("output.%s", flag), ...),
    conditionalPanel(sprintf("!output.%s", flag), div(class = "esc-hint", style = "margin-top:6px;", HTML(hint)))
  )
}
