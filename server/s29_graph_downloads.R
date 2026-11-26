# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s29_graph_downloads  (sourced inside server(), local = TRUE)
# One download handler per format (PNG, TIFF, PDF, SVG, EPS) for the graph on screen.

  graph_dims <- function(prefix = "download_graph") {
    w <- input[[paste0(prefix, "_w")]] %||% 8; h <- input[[paste0(prefix, "_h")]] %||% 6
    dpi <- input[[paste0(prefix, "_dpi")]] %||% 600
    if (identical(tryCatch(graph_type(), error = function(e) ""), "pca_dendrogram") && w == 8 && h == 6) { w <- 11; h <- 8.5 }
    list(w = max(1, w), h = max(1, h), dpi = max(72, dpi))
  }

  make_graph_download <- function(fmt) {
    downloadHandler(
      filename = function() {
        gt <- tryCatch(graph_type(), error = function(e) NULL) %||% "graph"
        paste0("ESCAP_", gt, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", fmt)
      },
      content = function(file) {
        d <- graph_dims()
        esc_open_device(file, fmt, d$w, d$h, d$dpi)
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_current_graph()
      },
      contentType = esc_mime[[fmt]]
    )
  }
  for (fmt in c("png", "tiff", "pdf", "svg", "eps")) {
    local({
      f <- fmt
      output[[paste0("download_graph_", f)]] <- make_graph_download(f)
    })
  }
