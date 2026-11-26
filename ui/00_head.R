# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# UI component: ui_head

ui_head <- #############################################################
  tags$head(
    tags$style(HTML("
/* Sidebar background */
  .well {
    background-color:       #b6e2f7;
    border-radius   : 8px;
  }
/* Overall page background */
  body {
    background-color:   #fbfbfc;
  }
/* Univariate section header */
  h4:contains('Univariate analysis') {
    color:   #0A57B5;
  }
/* Multivariate section header */
  h4:contains('Multivariate analysis') {
    color:   #0A57B5;
  }
/* Graphs section header */
  h4:contains('Graphs') {
    color:   #16a085;
  }
/* Tabs header bar */
  .nav-tabs > li > a {
    background-color:   #b1dff2;
    color           :   #333;
  }
.nav-tabs > li.active > a {
  background-color:            #ebecf3 !important;
  border-top      : 3px solid  #2980b9 !important;
  color           :            #25185e !important;
}


  /* Subtabs in Results */
                      #results_subtab > .nav-tabs > li > a {
  background-color:   #cdc9c6;   /* new inactive background */
  color            :  #003366;              /* new text color */
  }
                                #results_subtab > .nav-tabs > li.active > a,
                                #results_subtab > .nav-tabs > li.active > a:focus,
                                #results_subtab > .nav-tabs > li.active > a:hover {
  background-color:             #cdc9c6;
  border-top       : 3px solid  #003366;
  color            :            #003366 !important;
  }


  /* Subtabs in Graphs */
                      #graphs_subtab > .nav-tabs > li > a {
  background-color:   #cdc9c6;   /* new inactive background */
  color            :  #0b4f3c;
  }
                                #graphs_subtab > .nav-tabs > li.active > a,
                                #graphs_subtab > .nav-tabs > li.active > a:focus,
                                #graphs_subtab > .nav-tabs > li.active > a:hover {
  background-color:             #cdc9c6;
  border-top       : 3px solid  #0b4f3c;
  color            :            #0b4f3c !important;
  }
/* General text wrapping on small screens */
body, label, .control-label, .form-group, .shiny-output-error-validation {
  word-wrap: break-word;
  overflow-wrap: break-word;
}

/* Make tab labels wrap instead of overflowing */
.nav-tabs > li > a {
  white-space: normal;        /* allow multi-line */
  line-height: 1.2;
  text-align: center;
}

/* Wrap button labels too */
.btn {
  white-space: normal;
}

/* DataTable headers wrapping */
table.dataTable th {
  white-space: normal !important;
}


/* Mobile-specific adjustments */
@media (max-width: 768px) {
  /* Slightly smaller headings & labels */
  h4, h5 {
    font-size: 1rem;
  }
  label, .control-label {
    font-size: 0.9rem;
  }

  /* Reduce padding/margins in sidebar and main */
  .well {
    padding: 8px;
  }
  .container-fluid {
    padding-left: 8px;
    padding-right: 8px;
  }

  /* Let sidebar / controls use full width, avoid cramped rows */
  .col-sm-4, .col-sm-6 {
    float: none;
    width: 100%;
  }

  /* Tab labels: smaller text, more wrapping */
  .nav-tabs > li > a {
    font-size: 0.8rem;
    padding: 6px;
  }
}



/* --- Mobile fixes for 'Reference levels for logistic regression' --- */
@media (max-width: 768px) {

  /* Make each reference selector use half width instead of 1/4 */
  #chi_ref_ui .col-sm-3,
  #chi_ref_ui .col-md-3,
  #chi_ref_ui .col-lg-3 {
    float: left;
    width: 50%;
  }

  /* Allow labels (variable names) to wrap */
  #chi_ref_ui label {
    white-space: normal !important;
    overflow-wrap: break-word;
    word-break: break-word;
    font-size: 0.8rem;           /* a bit smaller so they fit better */
  }

  /* Allow the selected value text to wrap inside the selectize box */
  #chi_ref_ui .selectize-control.single .selectize-input {
    white-space: normal !important;
    overflow-wrap: break-word;
    word-break: break-word;
    line-height: 1.2;
    min-height: 32px;
  }

  /* Dropdown options: wrap long level names too */
  #chi_ref_ui .selectize-dropdown .option {
    white-space: normal;
    overflow-wrap: break-word;
    word-break: break-word;
  }
}



"))
  )
