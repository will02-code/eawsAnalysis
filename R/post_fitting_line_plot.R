#' Generate post-fitting line plot
#' @param data Cleaned data from `clean_model_output()`
#' @param decade_analysis Logical indicating if decadal analysis was performed
#' @param grouping_cols Character vector indicating grouping columns used (if any)
#' @param metric Metric name for y-axis label
#' @param trend_line Logical indicating if Mann-Kendall trend lines should be added
#'
post_fitting_line_plot <- function(
  data,
  decade_analysis,
  grouping_cols = NULL,
  metric,
  trend_line = TRUE
) {
  metric_title <- if (trend_line) {
    paste0(metric |> stringr::str_to_title(), " (logged)")
  } else {
    stringr::str_to_title(metric)
  }

  # Determine prediction vs value column
  y_var <- if (
    "predictions" %in% names(data) && any(!is.na(data$predictions))
  ) {
    "predictions"
  } else {
    "value"
  }

  # Apply log transformation if plotting trend lines
  if (trend_line) {
    data <- data |> dplyr::mutate(value = log1p(value))
  }

  # Setup base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = surv_year))

  # 1. Handle Mapping & Aesthetics dynamically
  if (!is.null(grouping_cols) && length(grouping_cols) > 0) {
    col_var <- rlang::sym(grouping_cols[1])
    facet_var <- if (length(grouping_cols) >= 2) {
      rlang::sym(grouping_cols[2])
    } else {
      NULL
    }

    # Apply global color/grouping aesthetics
    p <- p +
      ggplot2::aes(
        group = interaction(!!!rlang::syms(grouping_cols)),
        col = !!col_var
      )
  } else {
    col_var <- NULL
    facet_var <- NULL
  }

  # 2. Add Geoms (Points and Lines are universal here)
  p <- p +
    ggplot2::geom_point(ggplot2::aes(y = value)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[y_var]]), alpha = 0.5)

  # 3. Handle Trend Line Overlay
  if (trend_line) {
    if (!is.null(col_var)) {
      # Calculate grouped slopes/intercepts
      group_trends <- data |>
        dplyr::group_by(!!!rlang::syms(grouping_cols)) |>
        dplyr::summarise(
          mk_logslope = dplyr::first(mk_logslope),
          mk_intercept = dplyr::first(mk_intercept),
          .groups = "drop"
        )

      p <- p +
        ggplot2::geom_abline(
          data = group_trends,
          ggplot2::aes(
            slope = mk_logslope,
            intercept = mk_intercept,
            col = !!col_var
          ),
          linetype = "dashed"
        )
    } else {
      # Single global trend line
      p <- p +
        ggplot2::geom_abline(
          slope = data$mk_logslope[1],
          intercept = data$mk_intercept[1],
          linetype = "dashed"
        )
    }
  }

  # 4. Themes & Labels
  # Apply theme base depending on trend_line status
  if (trend_line) {
    p <- p + my_theme()
  } else {
    p <- p + my_theme(legend.position = "bottom")
  }

  p <- p + ggplot2::labs(x = "Year", y = metric_title)

  if (!is.null(col_var)) {
    p <- p + ggplot2::labs(color = grouping_cols[1])
  }

  # 5. Faceting Logic
  if (decade_analysis) {
    if (!is.null(facet_var)) {
      p <- p +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet_var),
          cols = ggplot2::vars(decade),
          scales = "free_x"
        )
    } else {
      p <- p + ggplot2::facet_wrap(~decade, scales = "free_x", nrow = 1)
    }
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    if (!is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_var))
    }
  }

  return(p)
}
