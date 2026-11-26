# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s13_dendrogram  (sourced inside server(), local = TRUE)

  dendro_data_k <- function(hc, k = 3L) {
    hcdata    <- ggdendro::dendro_data(hc, type = "rectangle")
    seg       <- hcdata$segments
    labclust  <- cutree(hc, k)[hc$order]
    segclust  <- rep(0L, nrow(seg))
    heights   <- sort(hc$height, decreasing = TRUE)
    if (k >= 2 && length(heights) >= k) {
      height <- mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
    } else {
      height <- max(heights, na.rm = TRUE)
    }
    for (i in seq_len(k)) {
      xi   <- hcdata$labels$x[labclust == i]
      if (!length(xi)) next
      idx1 <- seg$x    >= min(xi) & seg$x    <= max(xi)
      idx2 <- seg$xend >= min(xi) & seg$xend <= max(xi)
      idx3 <- seg$yend <  height
      idx  <- idx1 & idx2 & idx3
      segclust[idx] <- i
    }
    idx                   <- which(segclust == 0L)
    segclust[idx]         <- segclust[pmin(idx + 1L, length(segclust))]
    hcdata$segments$clust <- segclust
    hcdata$segments$line  <- as.integer(segclust < 1L)
    hcdata$labels$clust   <- labclust
    hcdata
  }
  
  set_labels_params <- function(nbLabels,
                                direction = c("tb", "bt", "lr", "rl"),
                                fan       = FALSE) {
    direction <- match.arg(direction)
    if (fan) {
      angle      <- 360 / nbLabels * seq_len(nbLabels) + 90
      idx        <- angle >= 90 & angle <= 270
      angle[idx] <- angle[idx] + 180
      hjust      <- rep(0, nbLabels)
      hjust[idx] <- 1
    } else {
      angle <- rep(0, nbLabels)
      hjust <- 0
      if (direction %in% c("tb", "bt")) angle <- angle + 45
      if (direction %in% c("tb", "rl")) hjust <- 1
    }
    list(angle = angle, hjust = hjust, vjust = 0.5)
  }
  
  plot_ggdendro <- function(hcdata,
                            direction   = c("lr", "rl", "tb", "bt"),
                            fan         = FALSE,
                            scale.color = NULL,
                            branch.size = 1,
                            label.size  = 3,
                            nudge.label = 0.01,
                            expand.y    = 0.1) {
    
    direction <- match.arg(direction)
    ybreaks   <- pretty(ggdendro::segment(hcdata)$y, n = 5)
    ymax      <- max(ggdendro::segment(hcdata)$y)
    
    # branches
    p <- ggplot() +
      geom_segment(
        data        = ggdendro::segment(hcdata),
        aes(
          x     = x,
          y     = y,
          xend  = xend,
          yend  = yend,
          linetype = factor(line),
          colour   = factor(clust)
        ),
        lineend     = "round",
        show.legend = FALSE,
        linewidth   = branch.size
      )
    
    # orientation
    if (fan) {
      p <- p +
        coord_polar(direction = -1) +
        scale_x_continuous(
          breaks = NULL,
          limits = c(0, nrow(ggdendro::label(hcdata)))
        ) +
        scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_x_continuous(breaks = NULL)
      if (direction %in% c("rl", "lr")) {
        p <- p + coord_flip()
      }
      if (direction %in% c("bt", "lr")) {
        p <- p + scale_y_reverse(breaks = ybreaks)
      } else {
        p <- p + scale_y_continuous(breaks = ybreaks)
        nudge.label <- -nudge.label
      }
    }
    
    # labels
    labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
    hcdata$labels$angle <- labelParams$angle
    
    p <- p +
      geom_text(
        data        = ggdendro::label(hcdata),
        aes(
          x     = x,
          y     = y,
          label = label,
          colour = factor(clust),
          angle  = angle
        ),
        vjust       = labelParams$vjust,
        hjust       = labelParams$hjust,
        nudge_y     = ymax * nudge.label,
        size        = label.size,
        show.legend = FALSE
      )
    
    # colors
    if (!is.null(scale.color)) {
      p <- p + scale_color_manual(values = scale.color)
    }
    
    ylim <- -round(ymax * expand.y, 1)
    p    <- p + expand_limits(y = ylim)
    
    p
  }
  ######################## END DENDROGRAM HELPERS ########################
  
  
  
  
  #Cluster Helper
  #Cluster Helper \u2013 7 styles (1\u20136 from example, keep your current style #2)
  plot_pca_dendro_style <- function(style, hc) {
    style <- as.integer(style)
    if (is.na(style) || style < 1 || style > 7) style <- 1
    
    # choose number of color clusters (3 by default, capped by data)
    n_leaves <- length(hc$order)
    k_clust  <- max(2L, min(5L, floor(n_leaves / 2L)))
    cols_default <- c("#a9a9a9", RColorBrewer::brewer.pal(5, "Set1"))
    cols <- cols_default[seq_len(k_clust + 1L)]
    
    hcdata <- dendro_data_k(hc, k = k_clust)
    
    ### STYLE 1: left\u2013right rectangular dendrogram (example Plot 1)
    if (style == 1) {
      p <- plot_ggdendro(
        hcdata,
        direction   = "lr",
        fan         = FALSE,
        scale.color = cols,
        branch.size = 0.7,
        label.size  = 3,
        nudge.label = 0.02,
        expand.y    = 0.15
      ) +
        theme_minimal() +
        xlab(NULL) + ylab(NULL) +           # <-- remove \u201cx\u201d and \u201cy\u201d labels
        theme(
          axis.text.x  = element_text(color = "grey20"),
          axis.text.y  = element_text(color = "grey20"),
          axis.title.x = element_blank(),   # (extra safety)
          axis.title.y = element_blank(),
          panel.grid   = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background  = element_rect(fill = NA, colour = NA)
        )
      print(p)
      return(invisible())
    }
    
    
    
    
    
    ### STYLE 2: your current circular ggraph dendrogram (unchanged)
    if (style == 2) {
      dend <- as.dendrogram(hc)
      p <- ggraph(dend, layout = "dendrogram", circular = TRUE) +
        geom_edge_diagonal(colour = "grey60") +
        geom_node_point(
          aes(
            filter = leaf,
            x = x * 1.07,
            y = y * 1.07
          ),
          size  = 2,
          color = "steelblue"
        ) +
        geom_node_text(
          aes(
            x = x * 1.15,
            y = y * 1.15,
            filter = leaf,
            label  = label
          ),
          size  = 2.2,
          hjust = 0.5
        ) +
        theme_void()
      print(p)
      return(invisible())
    }
    
    ### STYLE 3: top\u2013bottom rectangular with soft grid (example Plots 2\u20133 type)
    if (style == 3) {
      p <- plot_ggdendro(
        hcdata,
        direction   = "tb",
        fan         = FALSE,
        scale.color = cols,
        branch.size = 0.6,
        label.size  = 2.5,
        nudge.label = 0.01,
        expand.y    = 0.2
      )
      mytheme <- theme(
        axis.text          = element_text(color = "#50505030"),
        panel.grid.major.y = element_line(color = "#50505030", linewidth = 0.25),
        panel.grid.minor   = element_blank(),
        panel.background   = element_rect(fill = NA, colour = NA),
        plot.background    = element_rect(fill = NA, colour = NA)
      )
      p <- p + theme_void() + mytheme +
        expand_limits(x = c(-1, nrow(hcdata$labels) + 1))
      print(p)
      return(invisible())
    }
    
    ### STYLE 4: fan / circular dendrogram (example Plot 4 basic)
    if (style == 4) {
      p <- plot_ggdendro(
        hcdata,
        fan         = TRUE,
        scale.color = cols,
        branch.size = 0.7,
        label.size  = 4,
        nudge.label = 0.03,
        expand.y    = 0.4
      )
      mytheme <- theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
      p <- p + theme_void() + mytheme
      print(p)
      return(invisible())
    }
    
    ### STYLE 5: fan + class \u201cring\u201d under labels (example Plot 4 with extra points)
    if (style == 5) {
      p <- plot_ggdendro(
        hcdata,
        fan         = TRUE,
        scale.color = cols,
        branch.size = 0.7,
        label.size  = 4,
        nudge.label = 0.15,
        expand.y    = 0.8
      )
      mytheme <- theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
      p <- p + theme_void() + mytheme
      
      # add a strip of points below, colored by PCA Class if available
      res_pca <- tryCatch(pca_result(), error = function(e) NULL)
      if (!is.null(res_pca)) {
        labs   <- hcdata$labels$label
        ID     <- res_pca$ID
        Class  <- res_pca$Class
        id_idx <- match(labs, ID)
        df_ring <- data.frame(
          x     = seq_along(labs),
          y     = -0.7,
          Class = factor(Class[id_idx])
        )
        df_ring <- df_ring[!is.na(df_ring$Class), , drop = FALSE]
        if (nrow(df_ring) > 0) {
          p <- p +
            geom_point(
              data = df_ring,
              aes(x = x, y = y, fill = Class),
              size = 4,
              shape = 21,
              show.legend = FALSE
            ) +
            scale_fill_brewer(palette = "Set1")
        }
      }
      
      print(p)
      return(invisible())
    }
    
    ### STYLE 6: two-linkage comparison (average vs Ward) + matching lines (example Plot 5)
    if (style == 6) {
      res_pca <- tryCatch(pca_result(), error = function(e) NULL)
      if (is.null(res_pca)) {
        tmp <- plot_ggdendro(
          hcdata,
          direction   = "lr",
          scale.color = cols,
          expand.y    = 0.2
        ) + theme_void()
        print(tmp)
        return(invisible())
      }
      scores <- as.data.frame(res_pca$pca$x)
      if (nrow(scores) < 2) {
        tmp <- plot_ggdendro(
          hcdata,
          direction   = "lr",
          scale.color = cols,
          expand.y    = 0.2
        ) + theme_void()
        print(tmp)
        return(invisible())
      }
      
      D   <- dist(scores)
      hc1 <- hclust(D, method = "average")
      hc2 <- hclust(D, method = "ward.D2")
      
      hcdata1 <- dendro_data_k(hc1, k = k_clust)
      hcdata2 <- dendro_data_k(hc2, k = k_clust)
      
      p1 <- plot_ggdendro(
        hcdata1,
        direction   = "lr",
        scale.color = cols,
        expand.y    = 0.2
      ) + theme_void()
      
      p2 <- plot_ggdendro(
        hcdata2,
        direction   = "rl",
        scale.color = cols,
        expand.y    = 0.2
      ) + theme_void()
      
      idx_df <- data.frame(
        y1 = seq_len(nrow(hcdata1$labels)),
        y2 = match(hcdata1$labels$label, hcdata2$labels$label)
      )
      
      p_mid <- ggplot() +
        geom_segment(
          data = idx_df,
          aes(x = 0, y = y1, xend = 1, yend = y2),
          color = "grey"
        ) +
        theme_void()
      
      gridExtra::grid.arrange(p1, p_mid, p2, ncol = 3, widths = c(2, 1, 2))
      return(invisible())
    }
    
    ### STYLE 7: left dendrogram + right heat/bubble map (example Plot 6)
    if (style == 7) {
      res_pca <- tryCatch(pca_result(), error = function(e) NULL)
      if (is.null(res_pca)) {
        tmp <- plot_ggdendro(
          hcdata,
          direction   = "lr",
          scale.color = cols,
          expand.y    = 0.2
        ) + theme_void()
        print(tmp)
        return(invisible())
      }
      
      # left dendrogram (orientation lr)
      p1 <- plot_ggdendro(
        hcdata,
        direction   = "lr",
        scale.color = cols,
        expand.y    = 0.15
      ) +
        theme(
          axis.text.x      = element_text(color = "#ffffff"),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.ticks       = element_blank(),
          plot.background  = element_rect(fill = NA, colour = NA)
        ) +
        xlab(NULL) +
        ylab(NULL)
      
      # right \u201cbubble map\u201d using PCA scores (scaled 0\u20131)
      scores <- as.data.frame(res_pca$pca$x)
      if (!requireNamespace("data.table", quietly = TRUE)) {
        scores_dt <- scores
      } else {
        scores_dt <- data.table::as.data.table(lapply(scores, scales::rescale))
      }
      
      if (!requireNamespace("data.table", quietly = TRUE)) {
        scores_dt$Sample <- seq_len(nrow(scores_dt))
        melted <- tidyr::pivot_longer(
          scores_dt,
          cols      = colnames(scores),
          names_to  = "Variable",
          values_to = "Value"
        )
        idx <- seq_len(nrow(hcdata$labels))
        melted$car <- rep(idx, each = ncol(scores))
      } else {
        DT <- scores_dt
        DT[, Sample := .I]
        melted <- data.table::melt(DT,
                                   measure.vars = colnames(scores),
                                   variable.name = "Variable",
                                   value.name    = "Value")
        idx <- seq_len(nrow(hcdata$labels))
        melted[, car := rep(idx, length.out = .N)]
      }
      
      p2 <- ggplot(melted) +
        geom_point(
          aes(
            x    = Variable,
            y    = car,
            size = Value,
            color = Value
          ),
          show.legend = FALSE
        ) +
        scale_color_viridis_c(direction = -1) +
        theme_minimal() +
        theme(
          axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y      = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background  = element_rect(fill = NA, colour = NA)
        ) +
        xlab(NULL) +
        ylab(NULL)
      
      gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(3, 2))
      return(invisible())
    }
  }
  
  
  
  
  # Radial plot helper
