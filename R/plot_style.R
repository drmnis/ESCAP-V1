# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Author-controlled plot styling applied to any ggplot, plus export devices (PNG, TIFF,
# PDF, SVG, EPS). Styling is applied after a plot is built, so every graph in the app
# (old and new) gets the same controls.

ESC_FONTS <- c("Keep original" = "keep", "Arial", "Helvetica", "Times New Roman", "Calibri", "Cambria", "Georgia",
               "Garamond", "Palatino Linotype", "Book Antiqua", "Century Gothic", "Tahoma",
               "Verdana", "Trebuchet MS", "Segoe UI", "Courier New", "sans", "serif", "mono")

ESC_PALETTES <- list(
  `Okabe-Ito (colour-blind safe)` = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
  `Greys (journal print)` = c("#252525", "#636363", "#969696", "#bdbdbd", "#d9d9d9", "#f0f0f0"),
  `Set1` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
  `Dark2` = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
  `Paired` = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
  `Viridis` = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
  `Nature (NPG-like)` = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", "#7E6148"),
  `Pastel` = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC")
)

ESC_SHAPES <- c("Circle (filled)" = 16, "Circle (open)" = 1, "Circle (fill + border)" = 21,
                "Square (filled)" = 15, "Square (open)" = 0, "Square (fill + border)" = 22,
                "Triangle (filled)" = 17, "Triangle (open)" = 2, "Triangle (fill + border)" = 24,
                "Diamond (filled)" = 18, "Diamond (open)" = 5, "Diamond (fill + border)" = 23,
                "Inverted triangle" = 25, "Plus" = 3, "Cross" = 4, "Asterisk" = 8)

esc_style_defaults <- function() list(
  theme = "keep", family = "keep", base_size = NA, font_colour = "",
  title_size = NA, axis_title_size = NA, axis_text_size = NA, bold = "keep",
  palette = "default", custom_colours = "", bar_width = NA, bar_gap = NA,
  point_shape = NA, point_size = NA, line_width = NA, x_angle = 0, y_angle = 0, legend = "keep",
  grid = "keep", break_on = FALSE, break_from = NA, break_to = NA,
  sec_on = FALSE, sec_mult = 1, sec_add = 0, sec_name = "", width = 8, height = 6, dpi = 600,
  xlab = "", ylab = "", xmin = NA, xmax = NA, ymin = NA, ymax = NA,
  facet_by = character(0), facet_layout = "grid", facet_ncol = NA, facet_scales = "fixed")

esc_style_colours <- function(st, n = 30) {
  cc <- trimws(strsplit(st$custom_colours %||% "", "[,; ]+")[[1]])
  cc <- cc[nzchar(cc)]
  ok <- vapply(cc, function(x) !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error"), logical(1))
  cc <- cc[ok]
  if (length(cc)) return(rep_len(cc, n))
  if (!is.null(st$palette) && st$palette %in% names(ESC_PALETTES)) return(rep_len(ESC_PALETTES[[st$palette]], n))
  NULL
}

# Broken y axis: values in (from, to) are removed and the axis is joined at 'from'
esc_break_trans <- function(from, to) {
  gap <- to - from
  scales::trans_new("esc_break",
    transform = function(x) ifelse(x <= from, x, ifelse(x >= to, x - gap, from)),
    inverse   = function(x) ifelse(x <= from, x, x + gap))
}

.esc_is_discrete_scale <- function(built, aes) {
  sc <- built$plot$scales$get_scales(aes)
  !is.null(sc) && inherits(sc, "ScaleDiscrete")
}

# Shallow copy of a ggproto layer (same parent, own bindings). A ggproto child object
# (ggproto(NULL, layer)) recurses forever in ggplot2 >= 4.0, so it must not be used.
esc_clone_layer <- function(L) {
  e <- rlang::env_clone(L)
  attributes(e) <- attributes(L)
  e
}

esc_style_active <- function(st) {
  if (is.null(st)) return(FALSE)
  num_set <- function(x) length(x) == 1 && is.finite(suppressWarnings(as.numeric(x)))
  !identical(st$theme %||% "keep", "keep") || !identical(st$family %||% "keep", "keep") ||
    any(vapply(st[c("base_size", "title_size", "axis_title_size", "axis_text_size", "bar_width", "point_shape",
                    "point_size", "line_width")], num_set, logical(1))) ||
    nzchar(st$font_colour %||% "") || !identical(st$palette %||% "default", "default") || nzchar(st$custom_colours %||% "") ||
    isTRUE(suppressWarnings(as.numeric(st$x_angle)) != 0) || isTRUE(suppressWarnings(as.numeric(st$y_angle)) != 0) || !identical(st$legend %||% "keep", "keep") ||
    !identical(st$grid %||% "keep", "keep") || isTRUE(st$break_on) || isTRUE(st$sec_on) || !identical(st$bold %||% "keep", "keep") ||
    nzchar(st$xlab %||% "") || nzchar(st$ylab %||% "") || any(vapply(st[c("xmin", "xmax", "ymin", "ymax")], num_set, logical(1))) ||
    length(st$facet_by %||% character(0)) > 0
}

# Facet panels chosen by the author (variables of the plot's own data)
.esc_add_facets <- function(p, st) {
  fb <- st$facet_by %||% character(0)
  if (!length(fb) || !is.data.frame(p$data)) return(p)
  fb <- utils::head(fb[fb %in% names(p$data)], 2)
  if (!length(fb)) return(p)
  sc <- st$facet_scales %||% "fixed"
  if (identical(st$facet_layout, "wrap")) {
    nc <- suppressWarnings(as.integer(st$facet_ncol)); if (!is.finite(nc)) nc <- NULL
    return(p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(fb)), ncol = nc, scales = sc))
  }
  if (length(fb) == 1) p + ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(fb[1])), scales = sc)
  else p + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(fb[1])), cols = ggplot2::vars(!!rlang::sym(fb[2])), scales = sc)
}

# Axis titles and axis limits chosen by the author (limits zoom, they never drop data)
.esc_add_axes <- function(p, st) {
  flip <- inherits(p$coordinates, "CoordFlip")
  xl <- st$xlab %||% ""; yl <- st$ylab %||% ""
  if (flip) { tmp <- xl; xl <- yl; yl <- tmp }       # titles refer to the axes as seen on screen
  if (nzchar(xl)) p <- p + ggplot2::labs(x = xl)
  if (nzchar(yl)) p <- p + ggplot2::labs(y = yl)
  nz <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 1 && is.finite(v)) v else NA_real_ }
  xr <- c(nz(st$xmin), nz(st$xmax)); yr <- c(nz(st$ymin), nz(st$ymax))
  if (all(is.na(c(xr, yr)))) return(p)
  cls <- class(p$coordinates)[1]
  if (!cls %in% c("CoordCartesian", "CoordFlip", "CoordFixed")) return(p)   # polar / sf / radial: leave as is
  b <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL); if (is.null(b)) return(p)
  pp <- b$layout$panel_params[[1]]
  cont <- function(r) is.numeric(r) && length(r) == 2
  fill <- function(lim, cur) { if (!cont(cur)) return(NULL); if (all(is.na(lim))) return(NULL); lim[is.na(lim)] <- cur[is.na(lim)]; lim }
  if (flip) { tmp <- xr; xr <- yr; yr <- tmp }
  xlim <- fill(xr, p$coordinates$limits$x %||% pp$x.range); ylim <- fill(yr, p$coordinates$limits$y %||% pp$y.range)
  xlim <- xlim %||% p$coordinates$limits$x; ylim <- ylim %||% p$coordinates$limits$y
  args <- list(xlim = xlim, ylim = ylim, expand = TRUE)
  suppressMessages(p + switch(cls, CoordFlip = do.call(ggplot2::coord_flip, args),
                              CoordFixed = do.call(ggplot2::coord_fixed, c(list(ratio = p$coordinates$ratio), args)),
                              do.call(ggplot2::coord_cartesian, args)))
}

esc_apply_style <- function(p, st) {
  if (is.null(st) || !inherits(p, "ggplot") || !esc_style_active(st)) return(p)
  # patchwork: style every panel, then the base plot
  if (inherits(p, "patchwork") && !is.null(p$patches$plots) && length(p$patches$plots)) {
    p$patches$plots <- lapply(p$patches$plots, function(q) if (inherits(q, "ggplot")) esc_apply_style(q, st) else q)
  }
  nz <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 1 && is.finite(v)) v else NULL }
  fam <- if (identical(st$family %||% "keep", "keep")) NULL else st$family
  col <- if (nzchar(st$font_colour %||% "")) st$font_colour else NULL
  face <- switch(st$bold %||% "keep", bold = "bold", plain = "plain", NULL)
  bs <- nz(st$base_size)
  th <- switch(st$theme %||% "keep",
    classic = ggplot2::theme_classic(base_size = bs %||% 11, base_family = fam %||% ""),
    bw      = ggplot2::theme_bw(base_size = bs %||% 11, base_family = fam %||% ""),
    minimal = ggplot2::theme_minimal(base_size = bs %||% 11, base_family = fam %||% ""),
    light   = ggplot2::theme_light(base_size = bs %||% 11, base_family = fam %||% ""),
    linedraw = ggplot2::theme_linedraw(base_size = bs %||% 11, base_family = fam %||% ""),
    NULL)
  if (!is.null(th)) p <- p + th
  et <- function(size = NULL, face = NULL) {
    args <- Filter(Negate(is.null), list(family = fam, colour = col, size = size, face = face))
    if (length(args)) do.call(ggplot2::element_text, args) else NULL
  }
  tl <- Filter(Negate(is.null), list(
    text = et(bs), axis.text = et(nz(st$axis_text_size)), axis.title = et(nz(st$axis_title_size), face),
    plot.title = et(nz(st$title_size), face), legend.text = et(nz(st$axis_text_size)),
    legend.title = et(nz(st$axis_text_size), face), strip.text = et(nz(st$axis_text_size), face)))
  if (length(tl)) p <- p + do.call(ggplot2::theme, tl)
  ang <- nz(st$x_angle)
  if (!is.null(ang) && ang != 0)
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ang, hjust = 1, vjust = 1))
  yang <- nz(st$y_angle)
  if (!is.null(yang) && yang != 0)
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = yang, hjust = 0.5, vjust = 0.5))
  if (!identical(st$legend %||% "keep", "keep")) p <- p + ggplot2::theme(legend.position = st$legend)
  if (identical(st$grid, "none")) p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())
  if (identical(st$grid, "major")) p <- p + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  # geometry: bar width / spacing, point symbol, line width
  bw <- suppressWarnings(as.numeric(st$bar_width)); gap <- suppressWarnings(as.numeric(st$bar_gap))
  for (i in seq_along(p$layers)) {
    L <- esc_clone_layer(p$layers[[i]]); g <- class(L$geom)[1]  # child copy: never mutate the cached plot
    if (g %in% c("GeomBar", "GeomCol", "GeomErrorbar", "GeomCrossbar", "GeomBoxplot", "GeomTile") && is.finite(bw)) {
      inner <- if (is.finite(gap)) bw * (1 - gap) else bw
      if (inherits(L$position, "PositionDodge") || inherits(L$position, "PositionDodge2")) {
        L$position <- ggplot2::position_dodge(width = bw)
        if (g %in% c("GeomBar", "GeomCol", "GeomBoxplot")) L$geom_params$width <- inner
        if (g == "GeomErrorbar") L$geom_params$width <- inner * 0.4
      } else if (g %in% c("GeomBar", "GeomCol", "GeomBoxplot")) {
        L$geom_params$width <- bw
      }
      if (g %in% c("GeomBar", "GeomCol")) L$stat_params$width <- NULL
    }
    if (g == "GeomPoint") {
      shp <- suppressWarnings(as.numeric(st$point_shape)); sz <- suppressWarnings(as.numeric(st$point_size))
      if (is.finite(shp) && is.null(L$mapping$shape)) L$aes_params$shape <- shp
      if (is.finite(sz) && is.null(L$mapping$size)) L$aes_params$size <- sz
    }
    if (g %in% c("GeomLine", "GeomPath", "GeomStep", "GeomSmooth", "GeomErrorbar")) {
      lw <- suppressWarnings(as.numeric(st$line_width))
      if (is.finite(lw)) L$aes_params$linewidth <- lw
    }
    p$layers[[i]] <- L
  }

  # colours (only for discrete fill / colour mappings)
  cols <- esc_style_colours(st)
  if (!is.null(cols)) {
    built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    if (!is.null(built)) {
      dfill <- .esc_is_discrete_scale(built, "fill"); dcol <- .esc_is_discrete_scale(built, "colour")
      if (dfill) p <- suppressMessages(p + ggplot2::scale_fill_manual(values = cols))
      if (dcol) p <- suppressMessages(p + ggplot2::scale_colour_manual(values = cols))
      # continuous colour / fill (gradients such as |loading|, cos2, heatmaps): ramp through the chosen colours
      grad <- unique(cols)[seq_len(min(3, length(unique(cols))))]
      if (length(grad) >= 2) {
        cont <- function(a) { sc <- built$plot$scales$get_scales(a); !is.null(sc) && !sc$is_discrete() }
        if (cont("fill")) p <- suppressMessages(p + ggplot2::scale_fill_gradientn(colours = grad, name = built$plot$scales$get_scales("fill")$name))
        if (cont("colour")) p <- suppressMessages(p + ggplot2::scale_colour_gradientn(colours = grad, name = built$plot$scales$get_scales("colour")$name))
      }
      if (!dfill && !dcol) {        # single-colour bars, lines and points take the first chosen colour
        area_geoms <- c("GeomBar", "GeomCol", "GeomArea", "GeomDensity", "GeomViolin", "GeomBoxplot")
        has_area <- any(vapply(p$layers, function(L) class(L$geom)[1] %in% area_geoms, logical(1)))
        line_col <- if (has_area && length(cols) > 1) cols[2] else cols[1]   # lines over bars stay visible
        for (i in seq_along(p$layers)) {
          g <- class(p$layers[[i]]$geom)[1]
          L <- esc_clone_layer(p$layers[[i]])
          if (g %in% area_geoms && is.null(L$mapping$fill) && is.null(p$mapping$fill)) L$aes_params$fill <- cols[1]
          if (g %in% c("GeomPoint", "GeomLine", "GeomPath", "GeomStep") && is.null(L$mapping$colour) && is.null(p$mapping$colour))
            L$aes_params$colour <- line_col
          p$layers[[i]] <- L
        }
      }
    }
  }

  # y axis: break and / or secondary axis (one scale call so neither overrides the other)
  brk <- isTRUE(st$break_on) && is.finite(st$break_from) && is.finite(st$break_to) && st$break_to > st$break_from
  sec <- isTRUE(st$sec_on) && is.finite(st$sec_mult) && st$sec_mult != 0
  if (brk || sec) {
    built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    ysc <- if (!is.null(built)) built$plot$scales$get_scales("y") else NULL
    if (is.null(ysc) || inherits(ysc, "ScaleContinuous")) {
      args <- list()
      if (!is.null(ysc) && !is.null(ysc$name) && !inherits(ysc$name, "waiver")) args$name <- ysc$name
      if (brk) {
        args$transform <- esc_break_trans(st$break_from, st$break_to)
        rng <- if (!is.null(built)) built$layout$panel_params[[1]]$y.range else NULL
        args$breaks <- function(lim) {
          lo <- scales::extended_breaks()(c(lim[1], st$break_from))
          hi <- scales::extended_breaks()(c(st$break_to, lim[2] + (st$break_to - st$break_from)))
          b <- c(lo[lo <= st$break_from], hi[hi >= st$break_to]); unique(b)
        }
      }
      if (sec) {
        k <- st$sec_mult; a <- st$sec_add %||% 0
        args$sec.axis <- ggplot2::sec_axis(transform = ~ . * k + a, name = st$sec_name %||% "")
      }
      p <- suppressMessages(p + do.call(ggplot2::scale_y_continuous, args))
      if (brk) p <- p + ggplot2::geom_hline(yintercept = st$break_from, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
        ggplot2::labs(caption = sprintf("Axis break: %g to %g omitted", st$break_from, st$break_to))
    }
  }
  p <- .esc_add_facets(p, st)
  p <- .esc_add_axes(p, st)
  p
}

# ---------------------------------------------------------------------------
# Export devices. EPS uses cairo_ps (embeds fonts, rasterises transparency at 600 dpi).
# ---------------------------------------------------------------------------
esc_open_device <- function(file, format = c("png", "tiff", "pdf", "svg", "eps"), width = 8, height = 6, dpi = 600) {
  format <- match.arg(format)
  switch(format,
    png  = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(file, width, height, units = "in", res = dpi, background = "white")
           else grDevices::png(file, width, height, units = "in", res = dpi, type = if (capabilities("cairo")) "cairo" else "windows"),
    tiff = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff(file, width, height, units = "in", res = dpi, compression = "lzw", background = "white")
           else grDevices::tiff(file, width, height, units = "in", res = dpi, compression = "lzw"),
    pdf  = grDevices::cairo_pdf(file, width = width, height = height),
    svg  = grDevices::svg(file, width = width, height = height),
    eps  = grDevices::cairo_ps(file, width = width, height = height, onefile = FALSE, fallback_resolution = dpi))
}

esc_mime <- c(png = "image/png", tiff = "image/tiff", pdf = "application/pdf",
              svg = "image/svg+xml", eps = "application/postscript")

# Draw a plot object of any kind (ggplot, ggmatrix, grob, base-graphics function)
esc_draw <- function(x) {
  if (is.function(x)) x()
  else if (inherits(x, c("gg", "ggplot", "ggmatrix", "patchwork"))) print(x)
  else if (inherits(x, "grob") || inherits(x, "gtable")) { grid::grid.newpage(); grid::grid.draw(x) }
  else print(x)
}
