#' Main function which cleans data, runs models, and cleans output
#'
#' @param dataset Dataset for analysis. Should be output from `clip_and_save_waterbird_data()`
#' @param programs Which survey programs to include
#' @param metric Metric to analyse. One of "abundance", "richness", "nests", "broods", "nest_richness", or "pct_filled"
#' @param wetlands Which wetlands to include (default all)
#' @param valleys Which valleys to include (default all)
#' @param grouping_cols Column to group by (e.g. "ValleyName", "Wetland", "spp_code"). Default NULL
#' @param decade_analysis Logical. If TRUE, runs models separately for each decade
#' @param by_condition character string indicating the condition column used in the analysis (if any). This should be one of "wetland_condition", "valley_condition", "basin_condition", or "division_condition". Default NULL. If specified, this will group by the condition of the wetland/valley/basin/division that each survey point belongs to. However, this does rely on the SWC of those units being joined to the dataset. It will rarely be used.
#' @param basinDiv Character string indicating north or south basin
#'
#'
#'
#'
#' @returns
#' A list containing:
#' 1. A cleaned data frame with model results
#' 2. A ggplot object with the post-fitting line plot
#' 3. A summary data frame with summary statistics and model results by group (if
#'
#' @export
clean_data_run_models <- function(
  dataset,
  programs,
  metric,
  wetlands,
  valleys,
  basinDiv,
  grouping_cols,
  decade_analysis,
  by_condition = NULL
) {
  d2 <- get_bird_data(
    dataset,
    programs = programs,
    metric = metric,
    wetlands = wetlands,
    valleys = valleys,
    basinDiv = basinDiv,
    grouping_cols = grouping_cols,
    by_condition = by_condition
  )
  output <- run_mk_models(
    d2,
    decade_analysis = decade_analysis,
    grouping_cols = grouping_cols,
    by_condition = by_condition
  )
  cleaned <- clean_model_output(
    output,
    decade_analysis = decade_analysis,
    grouping_cols = grouping_cols,
    by_condition = by_condition
  )
  # Build grouping vector (empty if no groups)

  group_vars <- c(
    if (decade_analysis) "decade",
    if (!is.null(grouping_cols)) grouping_cols,
    if (!is.null(by_condition)) by_condition
  )

  # One definitive summary using .by (works with 0, 1, or 2 grouping vars)
  cleaned_summary <- cleaned %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      min = min(value),
      q1 = stats::quantile(value, 0.25),
      median = stats::median(value),
      q3 = stats::quantile(value, 0.75),
      max = max(value),
      long_term_avg = mean(value),
      dplyr::across(
        tidyselect::any_of(c(
          "percent_estimate",
          "percent_p.value",
          "mk_tau",
          "mk_p_value",

          'mk_logslope',
          'mk_percentslope',
          'mk_intercept',
          "n_rows"
        )),
        first
      ),

      .by = tidyselect::all_of(group_vars)
    )

  p <- post_fitting_line_plot(
    cleaned,
    decade_analysis = decade_analysis,
    grouping_cols = grouping_cols,
    metric = metric
  )

  return(list(cleaned, p, cleaned_summary))
}
