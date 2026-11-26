# Trace the Enhance Plots recursion
log <- "D:/API_R/dev/debug_overflow_log.txt"
out <- character(0)
suppressPackageStartupMessages({ library(shiny); library(ggplot2) })
setwd("D:/API_R/ESCAP")
for (f in list.files("R", full.names = TRUE)) source(f)
st <- esc_style_defaults(); st$theme <- "classic"
p <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(gear))) + geom_col(position = "dodge")
grab <- function(tag, expr) {
  op <- options(expressions = 300); on.exit(options(op))
  r <- withCallingHandlers(tryCatch({ force(expr); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e))),
    error = function(e) {
      cs <- sys.calls()
      out <<- c(out, paste("==", tag, "depth", length(cs)),
                vapply(head(cs, 25), function(x) substr(paste(deparse(x), collapse = " "), 1, 160), ""), "...",
                vapply(tail(cs, 12), function(x) substr(paste(deparse(x), collapse = " "), 1, 160), ""))
    })
  out <<- c(out, paste(tag, "->", r))
}
out <- c(out, paste("ggplot2", packageVersion("ggplot2"), "class:", paste(class(p), collapse = ",")))
grab("theme only", { q <- esc_apply_style(p, st); ggplot_build(q) })
st2 <- st; st2$bar_width <- 0.5; st2$bar_gap <- 0.1
grab("bar width", { q <- esc_apply_style(p, st2); ggplot_build(q) })
st3 <- st; st3$palette <- "Set1"
grab("palette", { q <- esc_apply_style(p, st3); ggplot_build(q) })
st4 <- st; st4$break_on <- TRUE; st4$break_from <- 10; st4$break_to <- 20
grab("axis break", { q <- esc_apply_style(p, st4); ggplot_build(q) })
pw <- patchwork::wrap_plots(p, p)
grab("patchwork", { q <- esc_apply_style(pw, st); print(q) })
# inside the app: print override
grab("app print override", testServer(shinyAppDir("D:/API_R/ESCAP"), {
  session$setInputs(sty_theme = "classic")
  grDevices::png(tempfile(fileext = ".png")); on.exit(grDevices::dev.off())
  print(p)
}))
writeLines(out, log); cat("DEBUG DONE\n")
