# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_rinfo

ui_tab_rinfo <- tabPanel(
          "R Info",
          br(),
          verbatimTextOutput("session_info")
        )
