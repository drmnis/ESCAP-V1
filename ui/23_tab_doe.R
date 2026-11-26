# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI components for Design of Experiments: settings in the sidebar (DoE button),
# tables in Results -> DoE, graphs in Graphs -> DoE.

ui_doe_controls <- conditionalPanel(
  condition = "input.doe_open % 2 == 1",
  wellPanel(
    style = "background:#fff7e6;",
    h4("Design of Experiments"),
    radioButtons("doe_step", NULL, c("1. Create design" = "create", "2. Analyse and optimise" = "analyse"), inline = TRUE),
    conditionalPanel("input.doe_step == 'create'",
            selectInput("dz_type", "Design", c(
              "Two-level full factorial (2^k)" = "full2", "General full factorial (multi-level)" = "general",
              "Fractional factorial 2^(k-p)" = "frac", "Plackett-Burman (screening)" = "pb",
              "Central composite (CCD)" = "ccd", "Box-Behnken (BBD)" = "bbd", "Taguchi L9 (3 levels)" = "taguchi",
              "Mixture: simplex lattice" = "mix_lattice", "Mixture: simplex centroid" = "mix_centroid",
              "D-optimal (AlgDesign)" = "dopt"), selected = "ccd"),
            numericInput("dz_k", "Number of factors (k)", 3, 2, 11),
            textInput("dz_names", "Factor names (comma separated)", "Temperature, Time, Concentration"),
            textInput("dz_low", "Low levels (actual units)", "40, 10, 1"),
            textInput("dz_high", "High levels (actual units)", "80, 30, 5"),
            conditionalPanel("input.dz_type == 'general'", textInput("dz_levels", "Levels per factor", "3, 3, 2")),
            conditionalPanel("input.dz_type == 'frac'", numericInput("dz_p", "Fraction p (2^(k-p))", 1, 1, 4)),
            conditionalPanel("input.dz_type == 'ccd'", selectInput("dz_alpha", "Axial distance (alpha)",
              c("Rotatable" = "rotatable", "Face-centred (alpha = 1)" = "face", "Spherical (alpha = sqrt k)" = "spherical"))),
            conditionalPanel("input.dz_type == 'pb'", selectInput("dz_pbruns", "Runs", c(8, 12, 16, 20, 24), 12)),
            conditionalPanel("input.dz_type == 'mix_lattice'", numericInput("dz_mixm", "Lattice degree m", 2, 1, 4)),
            conditionalPanel("input.dz_type == 'dopt'", numericInput("dz_druns", "Runs", 15, 4, 200),
                             selectInput("dz_dmodel", "Model", c("Quadratic" = "quadratic", "Two-factor interactions" = "interaction", "Linear" = "linear"))),
            fluidRow(column(6, numericInput("dz_center", "Centre points", 5, 0, 20)), column(6, numericInput("dz_reps", "Replicates", 1, 1, 10))),
            checkboxInput("dz_random", "Randomise run order", TRUE),
            numericInput("dz_seed", "Random seed", 123),
            textInput("dz_resp", "Response column names to add", "Y1"),
            actionButton("dz_make", "Generate design", class = "btn-primary", width = "100%")
    ),
    conditionalPanel("input.doe_step == 'analyse'",
            helpText("Uses the univariate upload (your completed design)."),
            uiOutput("da2_cols_ui"),
            selectInput("dx_model", "Model", c("Quadratic (response surface)" = "quadratic", "Two-factor interaction (2FI)" = "2fi",
                                              "Linear (main effects)" = "linear", "Mixture: linear (Scheffe)" = "mix_linear",
                                              "Mixture: quadratic (Scheffe)" = "mix_quadratic", "Mixture: special cubic" = "mix_special")),
            checkboxInput("dx_reduce", "Backward elimination of non-significant terms", FALSE),
            conditionalPanel("input.dx_reduce", numericInput("dx_alpha", "Alpha to remove", 0.10, 0.01, 0.5, 0.01),
                             checkboxInput("dx_hier", "Keep model hierarchy", TRUE)),
            actionButton("dx_run", "Fit models", class = "btn-primary", width = "100%"),
            tags$hr(),
            h5(strong("Numerical optimisation (desirability)")),
            uiOutput("dx_goals_ui"),
            actionButton("dx_opt", "Optimise", width = "100%")
    )
  )
)

ui_doe_results <- tagList(
  esc_gate("ok_doe_any", paste0("<b>Design of experiments.</b> In the sidebar click <i>DoE (design of experiments)</i>. ",
                                "<i>1. Create design</i>: generate a design and download it. ",
                                "<i>2. Analyse and optimise</i>: upload the completed design (factors + measured responses) as univariate data, ",
                                "choose the factor and response columns and click <i>Fit models</i>."),
    esc_report_bar("rep_doe"),
    conditionalPanel("output.ok_doe_design",
      h4("Design"),
      uiOutput("dz_info"),
      DT::dataTableOutput("dz_table"),
      br(),
      downloadButton("dz_csv", "Download design (CSV)"), tags$span(" "),
      downloadButton("dz_xlsx", "Download design (Excel)"),
      helpText("Fill in the response column(s) after running the experiments, save the file, upload it as univariate data, then choose '2. Analyse and optimise'."),
      tags$hr()),
    conditionalPanel("output.ok_doe_fit",
      h4("Response-surface analysis"),
      uiOutput("dx_results"),
      uiOutput("dx_opt_results"))
  )
)

ui_doe_graphs <- tagList(
  uiOutput("dx_graph_help"),
  fluidRow(
    column(3, selectInput("dx_plot", "Choose DoE graph", c("Design points" = "design", "Contour plot" = "contour", "3-D response surface" = "surface",
      "Pareto chart of effects" = "pareto", "Main effects" = "main", "Interaction plot" = "inter",
      "Perturbation plot" = "perturb", "Predicted vs actual" = "pva", "Residual diagnostics" = "diag", "Half-normal plot of effects" = "halfnorm", "Residuals vs run order" = "runorder",
      "Mixture ternary contour" = "ternary", "Desirability contour" = "desir"))),
    column(3, uiOutput("dx_plot_resp_ui")),
    column(2, uiOutput("dx_f1_ui")), column(2, uiOutput("dx_f2_ui")),
    column(2, numericInput("dx_theta", "3-D rotation", 35, -180, 180, 5))
  ),
  plotOutput("dx_plot_out", height = "560px")
)
