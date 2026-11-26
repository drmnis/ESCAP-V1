# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_maps

ui_tab_maps <- tabPanel(
          "Maps",
          br(),
          fluidRow(
            column(
              width = 4,
              h4("Study areas"),
              # Admin level
              selectInput(
                "map_level",
                "Administration Regions:",
                choices = c(
                  "Entire Bangladesh"       = "ADM0",
                  "Divisions"               = "ADM1",
                  "Districts/Zilas"         = "ADM2",
                  "Upazilas"                = "ADM3",
                  "Thanas/Unions"           = "ADM4"
                ),
                selected = "ADM2"
              ),
              # Area names (per level)
              selectizeInput(
                "map_names",
                "Select area names:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Type to search..."
                )
              ),
              tags$hr(),
              downloadButton("download_map_png", "Download PNG"),
              tags$span(" "),
              downloadButton("download_map_pdf", "Download PDF"),
              tags$span(" "),
              downloadButton("download_map_svg", "Download SVG")
            ),
            column(
              width = 8,
              plotOutput("map_full_fig", height = "700px")
            )
          )
        )
