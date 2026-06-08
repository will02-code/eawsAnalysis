#' Run trend models models on cleaned dataset from `get_bird_data()`, using `fit_mk_model()` for Mann-Kendall trend detection and Theil-Sen slope estimation.
#'
#'
#' @param cleaned_dataset A cleaned dataset obtained from the `clean_data` function.
#' @param grouping_cols An optional character string specifying a column name in the dataset to group by before modeling. E.g. "spp_code" or "ValleyName".
#' @param decade_analysis A logical indicating whether to perform decade-wise analysis. If TRUE, models are fitted separately for each decade.
#' @param by_condition by_condition whether to group by surface water condition. This should be one of "wetland_condition", "valley_condition", "basin_condition", or "division_condition". Default NULL. If specified, this will group by the condition of the wetland/valley/basin/division that each survey point belongs to. However, this does rely on the SWC of those units being joined to the dataset. It will rarely be used.
#' @returns A nested tibble containing the original data, model results, and warnings (if grouping_cols is provided) or overall.
#'
#' @export
#' @examples
#' \dontrun{
#' output <- run_mk_models(
#'    cleaned_data,
#'    decade_analysis = decade_analysis,
#'    family_for_glm = family_for_glm,
#'    grouping_cols = grouping_cols,
#'    by_condition = by_condition
#' )
#' }
#'
#'
run_mk_models <- function(
  cleaned_dataset,
  grouping_cols = NULL,
  decade_analysis = FALSE,
  by_condition = NULL
) {
  df_names <- names(cleaned_dataset)
  if (!is.null(grouping_cols)) {
    # Use all() to check if EVERY provided column exists in the data
    if (!all(grouping_cols %in% df_names)) {
      missing <- grouping_cols[!(grouping_cols %in% df_names)]
      stop(paste(
        "Grouping column(s) not found in dataset:",
        paste(missing, collapse = ", ")
      ))
    }

    # Use syms() instead of sym() to handle a vector/list of names
    grouping_cols_syms <- rlang::syms(grouping_cols)
  } else {
    grouping_cols_syms <- NULL
  }

  if (!is.null(by_condition)) {
    if (
      by_condition %in%
        c(
          "wetland_condition",
          "valley_condition",
          "basin_condition",
          "division_condition"
        )
    ) {
      grouping_cols_syms_with_cond <- c(
        grouping_cols_syms,
        rlang::sym(by_condition)
      )
    } else {
      stop(
        "No valid by_condition provided. Choose from 'wetland_condition', 'valley_condition', 'basin_condition', 'division_condition'."
      )
    }
  } else {
    grouping_cols_syms_with_cond <- grouping_cols_syms
  }

  nested_result <- cleaned_dataset %>%
    {
      if (decade_analysis == TRUE) {
        dplyr::group_by(., !!!grouping_cols_syms_with_cond, decade)
      } else {
        dplyr::group_by(., !!!grouping_cols_syms_with_cond)
      }
    } %>%
    tidyr::nest()

  nested_result <- nested_result |>
    dplyr::mutate(
      mk_output = purrr::map(
        data,
        fit_mk_model,
        decade_analysis = decade_analysis
      ),
      mk_tau = purrr::map_dbl(mk_output, \(x) x$tau),
      mk_p_value = purrr::map_dbl(mk_output, \(x) x$sl),
      mk_logslope = purrr::map_dbl(mk_output, \(x) x$log_slope),
      mk_percentslope = purrr::map_dbl(mk_output, \(x) x$percent_slope),
      mk_intercept = purrr::map_dbl(mk_output, \(x) x$intercept),
      mk_warnings = purrr::map(mk_output, \(x) x$warnings),
      mk_has_warnings = purrr::map_lgl(mk_output, \(x) x$has_warnings),
    )

  nested_result
}
