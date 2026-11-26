# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_banner

ui_banner <- tagList(
  
  # Header with ESCAP logo + text on left, GAU logo on right
  div(
    style = "display:flex; justify-content:space-between; align-items:center;",
    
    # LEFT: ESCAP logo + text
    div(
      style = "display:flex; align-items:center;",
      img(
        src    = "ESCAP.png",
        height = "80px",
        alt    = "ESCAP: Easy Statistics, Chemometrics and Plotting"
      ),
      div(
        style = "margin-left:10px;",
        tags$span(
          "Easy Statistics, Chemometrics and Plotting",
          style = "font-size:40px; font-weight:normal; color:#003366;"
        )
      )
    ),
    
    # RIGHT: GAU logo
    div(
      img(
        src    = "gau_logo.png",
        height = "90px",
        alt    = "Gazipur Agricultural University"
      )
    )
  ),
  
  
  tags$hr(),
  HTML(
    "<p>Thank you for using this app. Please cite this app as:<br/>
  Islam, M. N. (2022). <i>Chemometrics in nondestructive quality evaluation</i>.
In <i>Nondestructive quality assessment techniques for fresh fruits and vegetables</i> (pp. 331\u2013355).
Singapore: Springer Nature Singapore.</p>"
  ),
  tags$hr(),
  strong("Data files: .csv, .txt, .tsv, .xls or .xlsx (first row = column names)."),
  tags$br()
)
