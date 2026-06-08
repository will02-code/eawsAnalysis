#' Generate post-fitting line plot
#' @param data Cleaned data from `clean_model_output()`
#' @param decade_analysis Logical indicating if decadal analysis was performed
#' @param grouping_cols Character string indicating grouping column used (if any)
#' @param metric Metric name for y-axis label
#'
post_fitting_line_plot <- function(
  data,
  decade_analysis,
  grouping_cols,
  metric
) {
  metric_title <- stringr::str_to_title(metric)
  y_var <- if (
    "predictions" %in% names(data) && any(!is.na(data$predictions))
  ) {
    "predictions"
  } else {
    "value"
  }

  # Identify our specific mapping columns
  col_var <- if (length(grouping_cols) >= 1) {
    rlang::sym(grouping_cols[1])
  } else {
    NULL
  }
  facet_var <- if (length(grouping_cols) >= 2) {
    rlang::sym(grouping_cols[2])
  } else {
    NULL
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = surv_year))

  # Only add the group aesthetic if grouping_cols is not NULL/empty
  if (length(grouping_cols) > 0) {
    p <- p + ggplot2::aes(group = interaction(!!!rlang::syms(grouping_cols)))
  }

  # Initialize plot with the primary color grouping
  p <- p +
    ggplot2::geom_point(ggplot2::aes(y = value, col = !!col_var)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[y_var]], col = !!col_var),
      alpha = 0.5
    ) +
    my_theme(legend.position = "bottom") +
    ggplot2::labs(x = "Year", y = metric_title)

  # Logic for Faceting
  if (decade_analysis) {
    if (!is.null(facet_var)) {
      # Decades across the top, 2nd grouping variable down the side
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
