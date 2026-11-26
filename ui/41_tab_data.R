# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_data

ui_tab_data <- tabPanel(
          "View Data",
          br(),
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            h4("Uploaded data (latest file; first 20 rows; first 15 columns)"),
            div(
              actionButton("data_show_all", "Show all"),
              tags$span(" "),
              actionButton("data_hide", "Hide")
            )
          ),
          textOutput("data_view_info"),
          div(
            style = "width:100%; max-width:100%; overflow-x:auto;",
            DT::dataTableOutput("data_preview_dt")
          ),
          br(),
          actionButton("use_selected_rows",   "Use selected rows"),
          tags$span(" "),
          actionButton("delete_selected_rows","Delete selected rows"),
          tags$span(" "),
          actionButton("reset_row_filter",    "Reset to original data")
        )
