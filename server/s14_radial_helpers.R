# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Server part: s14_radial_helpers  (sourced inside server(), local = TRUE)

  prepare_radial_data <- function(df, empty_bar = 2) {
    # Expect: col1 = individual, col2 = group, col3+ = numeric values
    validate(
      need(ncol(df) >= 3, "Radial plot: need at least 3 columns: individual, group, and one value column.")
    )
    
    # Coerce to expected types
    df[[1]] <- as.character(df[[1]]) # individual
    df[[2]] <- as.factor(df[[2]]) # group
    for (j in 3:ncol(df)) {
      df[[j]] <- suppressWarnings(as.numeric(df[[j]]))
    }
    
    # Long format
    long <- df |>
      tidyr::pivot_longer(
        cols      = 3:ncol(df),
        names_to  = "observation",
        values_to = "value"
      )
    
    nObsType <- nlevels(as.factor(long$observation))
    
    # Add empty bars after each group
    to_add <- data.frame(
      matrix(NA, empty_bar * nlevels(long$group) * nObsType, ncol(long))
    )
    colnames(to_add) <- colnames(long)
    to_add$group <- rep(levels(long$group), each = empty_bar * nObsType)
    long <- rbind(long, to_add)
    
    long <- long |>
      dplyr::arrange(group, !!rlang::sym(names(df)[1])) # order by group, individual
    
    long$id <- rep(seq_len(nrow(long) / nObsType), each = nObsType)
    
    # Label data per individual
    label_data <- long |>
      dplyr::group_by(id, individual = !!rlang::sym(names(df)[1])) |>
      dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop")
    
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
    
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # Base data per group
    base_data <- long |>
      dplyr::group_by(group) |>
      dplyr::summarise(
        start   = min(id),
        end     = max(id) - empty_bar,
        .groups = "drop"
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(title = mean(c(start, end))) |>
      dplyr::ungroup()
    
    # Grid data for circular baselines
    grid_data <- base_data
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:(nrow(grid_data) - 1))] + 1
    grid_data$start <- grid_data$start - 1
    if (nrow(grid_data) > 0) {
      grid_data <- grid_data[-1, , drop = FALSE]
    }
    
    list(
      data          = long,
      label_data    = label_data,
      base_data     = base_data,
      grid_data     = grid_data,
      number_of_bar = number_of_bar
    )
  }
  
  
  # Preprocessing helper ----------------------------------------------------
