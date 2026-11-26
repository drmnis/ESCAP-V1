# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_about

ui_tab_about <- tabPanel(
          "About",
          br(),
          h4("About this tool"),
          p(strong("ESCAP (Easy Statistics, Chemometrics and Plotting)"),
            " is a free web application for analyzing experimental data without writing code. It was built for food science, ",
            "agriculture and related laboratory research, where the same steps are repeated in almost every study: comparing ",
            "treatments, checking relationships between quality attributes, modeling changes during processing or storage, and ",
            "analyzing spectra."),
          p("In one interface, ESCAP covers:"),
          tags$ul(
            tags$li(strong("Univariate statistics:"), " descriptive statistics, t-tests, one- and two-way ANOVA (CRD and RCBD) with post-hoc letters, ",
                    "ANCOVA, Kruskal-Wallis with Dunn's test, correlation tests, linear and polynomial regression, and reaction kinetics with Arrhenius analysis."),
            tags$li(strong("Epidemiology:"), " prevalence with confidence intervals, chi-square or Fisher's exact test, and crude and adjusted odds ratios from logistic regression."),
            tags$li(strong("Design of experiments:"), " factorial, central composite, Box-Behnken and mixture designs, response-surface models and desirability optimization."),
            tags$li(strong("Chemometrics:"), " spectral preprocessing (SNV, MSC, Savitzky-Golay, scaling, centering), PCA, PLSR and PLS-DA with cross-validation, ",
                    "VIP scores, selectivity ratio, and Q residual / Hotelling T\u00b2 diagnostics."),
            tags$li(strong("Graphs and reports:"), " publication-ready figures (PNG, TIFF, PDF, SVG, EPS) that can be restyled with Enhance Plots, ",
                    "and results tables as PDF, Word, Markdown or CSV reports.")
          ),
          p("ESCAP is available at ", tags$a(href = "https://analysis.gau.edu.bd", target = "_blank", "analysis.gau.edu.bd"),
            paste0(". Version ", ESC_VERSION, " (", ESC_RELEASE, ").")),
          br(),
          h4("Instructions for users"),
          tags$ol(
            tags$li(strong("Prepare the data."), " Save it as .csv, .txt, .tsv, .xls or .xlsx with column names in the first row and one ",
                    "observation (sample or replicate) per row. Keep treatment or group labels in their own columns. Special characters in ",
                    "names and values are accepted. Ready-made files for every analysis are on the ", em("Example data"), " tab."),
            tags$li(strong("Upload."), " Use ", em("Univariate analysis"), " for statistics, epidemiology, DoE and the univariate graphs; use ",
                    em("Multivariate analysis"), " for spectra and other variable sets (preprocessing, PCA, PLSR, PLS-DA, correlation). ",
                    "Check the file under ", em("View Data"), "."),
            tags$li(strong("Choose the analysis."), " Select it in the ", em("Analysis"), " list, set its columns and options (for example the ",
                    "treatment factor, block or post-hoc test), choose SD or SE, and click ", em("Run analysis"), ". For multivariate data, set ",
                    "the X-variable range and preprocessing, then click PCA, PLSR, PLS-DA or Correlation."),
            tags$li(strong("Read the results."), " The ", em("Results"), " tab opens with a publication-ready table first, followed by the ",
                    "supporting tables (ANOVA, assumption checks, post-hoc comparisons, model statistics)."),
            tags$li(strong("Make the graphs."), " In ", em("Graphs"), ", pick a plot. The note at the top says which columns are used. ",
                    em("Enhance Plots"), " (above each plot) changes fonts, colors, bar size, symbols, axis titles and limits, axis break, ",
                    "second Y axis and panels (facets). Download the figure as PNG, TIFF, PDF, SVG or EPS at the size and resolution you need."),
            tags$li(strong("Save the report."), " Every results tab has a report bar: choose the paper size and download a PDF or Word report ",
                    "(orientation is chosen automatically), Markdown, or the tables as CSV."),
            tags$li(strong("Start a new analysis."), " Click ", em("Clear all inputs"), " before switching to a different data set."),
            tags$li(strong("Check before you report."), " Read the assumption checks and notes under each table, and confirm that the design ",
                    "and model suit your experiment."),
            tags$li(strong("Need help?"), " If something does not work as expected, email the author with a short description and, if possible, the data file.")
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
