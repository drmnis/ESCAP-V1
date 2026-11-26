# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server assembly

server <- function(input, output, session) {
  for (f in sort(list.files("server", pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = TRUE)
  }
}
