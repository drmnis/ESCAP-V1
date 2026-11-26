# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_about

ui_tab_about <- tabPanel(
          "About",
          br(),
          h4("About this tool"),
          p("ESCAP (Easy Statistics, Chemometrics and Plotting) is a user‑friendly application designed to escape the complexity of data analysis. It brings together descriptive and inferential statistics (t-tests, one- and two-way ANOVA, ANCOVA, non-parametric tests, regression, reaction kinetics), epidemiological analysis, design of experiments with response-surface optimisation, chemometrics (preprocessing, PCA, PLSR, PLS-DA), advanced plots and maps in a single, intuitive interface. Whether you are exploring experimental data, teaching chemometrics, or conducting routine analysis, ESCAP helps you move quickly from raw data to clear, reproducible insights with zero coding. ESCAP is accessible at: analysis.gau.edu.bd"),
          br(),
          h4("Example files"),
          p("Download an example CSV dataset and an example PDF with instructions:"),
          downloadButton("download_example_csv", "Download example CSV"),
          tags$span(" "),
          downloadButton("download_example_pdf", "Download example PDF"),
          br(), br(),
          h4("Sample data"),
          tags$ul(
            tags$li(
              icon("download", lib = "font-awesome"), " ",
              "Download sample data for PCA: ",
              tags$a(
                href = "https://gist.githubusercontent.com/netj/8836201/raw/iris.csv",
                "iris.csv",
                target = "_blank"
              )
            ),
            tags$li(
              icon("download", lib = "font-awesome"), " ",
              "Download sample data for Correlation analysis: ",
              tags$a(
                href = "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/mtcars.csv",
                "mtcars.csv",
                target = "_blank"
              )
            ),
            tags$li(
              icon("download", lib = "font-awesome"), " ",
              "Download sample data for Multivariate analysis: ",
              tags$a(
                href = "https://raw.githubusercontent.com/drmnis/data/refs/heads/main/Beer%20Data%20set.csv",
                "beer_data.csv",
                target = "_blank"
              )
            ),
            tags$li(
              icon("download", lib = "font-awesome"), " ",
              "Download people data for PLSR: ",
              tags$a(
                href = "https://raw.githubusercontent.com/drmnis/data/refs/heads/main/people.csv",
                "people.csv",
                target = "_blank"
              )
            ),
            tags$li(
              icon("download", lib = "font-awesome"), " ",
              "Download gasoline data for PLSR: ",
              tags$a(
                href = "https://raw.githubusercontent.com/drmnis/data/refs/heads/main/gasoline.csv",
                "gasoline.csv",
                target = "_blank"
              )
            )
          ),
          br(),
          h4("Instructions for Users"),
          tags$ol(
            tags$li("Please see the attached PDF."),
            tags$li("Upload data as .csv, .txt, .tsv, .xls or .xlsx (first row = column names, one observation per row)."),
            tags$li("Remember to click 'Clear all inputs' when switching between univariate and multivariate analysis."),
            tags$li("Figures are publication quality. Use the Plot style panel (top) for colours, fonts, bar size, symbols, axis break and a second Y axis; download as PNG, TIFF, PDF, SVG or EPS."),
            tags$li("Every results tab has a report bar: download the tables as a PDF report (paper size of your choice, orientation chosen automatically), Markdown or CSV."),
            tags$li("If you find any problem, please email me.")
          ),
          br(),
          h4("Author"),
          div(
            style = "display:flex; align-items:flex-start;",
            img(
              src = "nahid_photo.jpg", height = "100px",
              style = "margin-right:10px;", alt = "Dr. Md. Nahidul Islam"
            ),
            div(
              strong("Dr. Md. Nahidul Islam"), br(),
              "Assistant Professor", br(),
              "Department of Food Engineering", br(),
              "Faculty of Agricultural and Bioresources Engineering", br(),
              "Gazipur Agricultural University (GAU), Gazipur-1706, Bangladesh", br(),
              tags$a(href = "mailto:nahidul.islam@gau.edu.bd", "nahidul.islam@gau.edu.bd"),
              br(), br(),
              h5("Download citation (EndNote format)"),
              downloadButton("download_citation", "Download citation.enw")
            )
          )
        )
