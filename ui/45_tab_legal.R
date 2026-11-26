# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_tab_legal

ui_tab_legal <- tabPanel(
          "Legal",
          br(),
          h3("Terms of use"),
          p("ESCAP is free to use for research, teaching and routine analysis. By using it you agree to the terms below."),
          h4("Disclaimer"),
          p("ESCAP is provided \u201cas is\u201d, without warranty of any kind, express or implied, including warranties of accuracy, ",
            "completeness, merchantability, fitness for a particular purpose or non-infringement. The methods are standard and have ",
            "been checked against established R packages, but no software can guarantee that every result is correct for every data set."),
          p("ESCAP supports research and teaching. It does not replace professional statistical, scientific, medical, veterinary, ",
            "legal or regulatory advice, and its results are not a certification of product quality or safety."),
          h4("Your responsibility"),
          p("You are responsible for:"),
          tags$ul(
            tags$li("choosing data, designs, models and settings that suit your study;"),
            tags$li("checking the assumptions, notes and diagnostics reported with each analysis;"),
            tags$li("verifying tables, figures and reports before you use or publish them; and"),
            tags$li("any conclusion, decision or publication based on results from ESCAP.")
          ),
          h4("Limitation of liability"),
          p("To the extent permitted by law, the author and Gazipur Agricultural University are not liable for any loss or damage arising from ",
            "the use of, or inability to use, ESCAP. This includes direct, indirect, incidental or consequential damage, loss of data, ",
            "loss of income, and claims by third parties."),
          h4("Your data"),
          p("Files you upload are used only to run your analyses during your session. ESCAP does not publish, share or sell your data and does ",
            "not use it for any other purpose. Uploaded files are held temporarily by the server while the session is open. Do not upload ",
            "personal or confidential information that you are not permitted to process. Only an anonymous visit counter is recorded."),
          h4("Citation"),
          p("If ESCAP contributes to a thesis, report or publication, please cite it. The citation can be downloaded from the About tab."),
          h4("Copyright and third-party software"),
          p("\u00a9 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University. All rights reserved. ESCAP is built with R and Shiny and uses ",
            "open-source R packages (for example ggplot2, pls, agricolae, car, emmeans, plotly); each remains under its own license. ",
            "Example data sets are provided for demonstration only."),
          h4("Contact"),
          p("Questions, problems or suggestions: ", tags$a(href = "mailto:nahidul.islam@gau.edu.bd", "nahidul.islam@gau.edu.bd"), "."),
          p(em("By accessing or using ESCAP, you confirm that you have read and accept these terms."))
        )
