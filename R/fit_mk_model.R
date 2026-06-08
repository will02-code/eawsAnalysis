#' Fit Mann-Kendall trend model.
#'
#' @param data Dataframe containing the data to fit the model on. Usually a nested dataframe.
#' @param decade_analysis Logical indicating if the analysis is decadal.
#'
#' @returns A list containing the following elements:
#' - model: The fitted Mann-Kendall model object.
#' - tau: The Kendall's tau statistic from the model.
#' - sl: The p-value associated with the tau statistic.
#' - log_slope: The Theil-Sen slope estimate on the log-transformed data.
#' - percent_slope: The percent slope, calculated from the log_slope.
#' - intercept: The intercept of the Theil-Sen line on the log-transformed data.
#' - warnings: A character vector of any warnings captured during model fitting.
#' - has_warnings: A logical indicating if any warnings were captured.
#'@description
#' For detection of trend in a time series, and a Theil-Sen slope estimate.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' mk_output = purrr::map(
#'        data,
#'        fit_mk_with_warnings,
#'        decade_analysis = decade_analysis
#'      )
#' }

fit_mk_model <- function(data, decade_analysis) {
  if (decade_analysis) {
    data <- data %>% dplyr::arrange(surv_year_in_decade)
    x_raw <- data$surv_year_in_decade
  } else {
    data <- data %>% dplyr::arrange(surv_year)
    x_raw <- data$surv_year
  }
  y_raw <- data$value

  keep <- !is.na(x_raw) & !is.na(y_raw)
  x <- x_raw[keep]
  y <- log(1 + y_raw[keep])
  n <- length(y)

  ts_slope <- NA_real_
  ts_intercept <- NA_real_
  result <- list(tau = NA, sl = NA)

  if (n >= 2) {
    # Generate all pair-wise slopes
    all_slopes <- unlist(lapply(1:(n - 1), function(i) {
      (y[(i + 1):n] - y[i]) / (x[(i + 1):n] - x[i])
    }))
    raw_ts_slope <- median(all_slopes, na.rm = TRUE) # The 'math' slope
    ts_intercept <- median(y - (raw_ts_slope * x), na.rm = TRUE)

    pct_trend <- (exp(raw_ts_slope) - 1) * 100
    console_output <- capture.output(
      {
        # MK usually needs at least 3 points for a p-value
        tryCatch(
          {
            result <- Kendall::MannKendall(y)
          },
          error = function(e) result <- list(tau = NA, sl = NA)
        )
      },
      type = "output"
    )

    warnings_list <- console_output[grepl(
      "WARNING",
      console_output,
      ignore.case = TRUE
    )]
  } else {
    warning("Not enough data points for Mann-Kendall test")
    warnings_list <- character(0)
    raw_ts_slope <- NA_real_
    pct_trend <- NA_real_
    ts_intercept <- NA_real_
  }

  # Extract any lines containing "WARNING"

  return(list(
    model = result,
    tau = result$tau,
    sl = result$sl,
    log_slope = raw_ts_slope,
    intercept = ts_intercept,
    percent_slope = pct_trend,
    warnings = warnings_list,
    has_warnings = length(warnings_list) > 0
  ))
}
