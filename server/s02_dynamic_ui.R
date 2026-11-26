# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s02_dynamic_ui  (sourced inside server(), local = TRUE)

  output$linear_group_resp_ui <- renderUI({
    req(input$graph_type_uni == "linear_group")
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    
    resp <- uni_resp()
    resp <- resp[resp %in% names(df)]
    if (length(resp) <= 1) return(NULL)
    
    selectInput(
      "linear_group_resp",
      "Select variable",
      choices  = resp,
      selected = resp[1],
      width    = "200px"
    )
  })
  
  
  output$ggbetween_resp_ui <- renderUI({
    req(input$graph_type_uni == "ggbetween")
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    
    resp <- uni_resp()
    resp <- resp[resp %in% names(df)]
    if (length(resp) <= 1) return(NULL)
    
    selectInput(
      "ggbetween_resp",
      "Select variable",
      choices  = resp,
      selected = resp[1],
      width    = "200px"
    )
  })
  
  
  
  output$chi_ref_ui <- renderUI({
    req(identical(input$uni_analysis, "chi"))
    df <- tryCatch(dat_raw(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    
    prep <- chi_prepare_data(df)
    preds <- prep$predictors
    if (length(preds) == 0) {
      return(NULL)
    }
    
    df2 <- prep$df
    
    tagList(
      h5(strong("Reference levels for logistic regression")),
      # Wrap the selects into a grid: 4 columns per row (width = 3)
      fluidRow(
        lapply(seq_along(preds), function(i) {
          v <- preds[i]
          levs <- levels(df2[[v]])
          if (is.null(levs) || length(levs) < 2) {
            return(NULL)
          }
          
          column(
            width = 3,
            selectInput(
              inputId = paste0("chi_ref_", v),
              label = v, # just variable name, no "Reference level for"
              choices = levs,
              selected = levs[1]
            )
          )
        })
      )
    )
  })
  
  
  
  
  # Secondary ID 1
  output$pca_secid_1_ui <- renderUI({
    req(input$graph_type_pca == "pca_scores", input$pca_modify_view)
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    selectInput(
      "pca_secid_1",
      "Secondary ID 1",
      choices  = names(df),
      selected = names(df)[1]
    )
  })
  
  # Secondary ID 2
  output$pca_secid_2_ui <- renderUI({
    req(input$graph_type_pca == "pca_scores", input$pca_modify_view)
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    selectInput(
      "pca_secid_2",
      "Secondary ID 2",
      choices  = names(df),
      selected = if (ncol(df) >= 2) names(df)[2] else names(df)[1]
    )
  })
  
  # Secondary ID 3
  output$pca_secid_3_ui <- renderUI({
    req(input$graph_type_pca == "pca_scores", input$pca_modify_view)
    df <- tryCatch(dat_multi(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    selectInput(
      "pca_secid_3",
      "Secondary ID 3",
      choices  = names(df),
      selected = if (ncol(df) >= 3) names(df)[3] else names(df)[1]
    )
  })
  
  
  
  
  
  
  
