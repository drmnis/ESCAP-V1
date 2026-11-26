# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Global setup: packages and options

library(shiny)
library(dplyr)
library(agricolae)
library(purrr)
library(stringr)
library(prospectr)
library(ggplot2)
library(GGally)
library(plotly)
library(tidyr)
library(ggrepel)
library(Hmisc)
library(corrplot)
library(ellipse)
library(pls)
library(ggstatsplot)
library(glue)
library(rlang)
library(ggpubr)
library(heatmaply)
library(gplots)
library(plotrix)
library(RColorBrewer)
library(webshot2)
library(sf)
library(ggspatial)
library(patchwork)
library(ggdendro)
library(ggraph)
library(igraph)
library(dendextend)
library(circlize)
library(DT)



###############################################################
#################### GLOBAL SETUP & OPTIONS ###################
###############################################################

options(shiny.error = function() {
  cat("[ERROR] Shiny error occurred\n")
  traceback()
})

# simple in‑memory visit counter (per R session)
visit_counter <- 0L
