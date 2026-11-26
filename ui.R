# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# User interface assembly

for (f in sort(list.files("ui", pattern = "\\.R$", full.names = TRUE))) source(f, local = TRUE)

###############################################################
ui <- fluidPage(
  #############################################################
  #################### CSS STYLES & HEADER ####################
  ui_head,
  
  ui_banner,
  #############################################################
  #################### SIDEBAR LAYOUT #########################
  #############################################################
  
  
  ################################# Sidebar Panel #############################
  sidebarLayout(
    #########################################################
    #################### UNIVARIATE ANALYSIS ################
    #########################################################
    ui_sidebar,
    
    
    ################################# Mainpanel #############################
    ################################# Mainpanel #############################
    
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        ui_tab_about,
        ui_tab_data,
        
        ############################ Ui for RESULTS (wrapper) #############################
        ui_tab_results,
        
        ############################ Ui for Graphs #############################
        ui_tab_graphs,
        ui_tab_maps,
        
        
        # --- ADD BACK Legal tab ---
        ui_tab_examples,
        ui_tab_legal,
        
        # --- ADD BACK R Session Info tab ---
        ui_tab_rinfo
      )
    )
  )
)

