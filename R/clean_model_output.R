#' Clean output from run_mk_models()
#'
#' @param output output from run_mk_models()
#' @param decade_analysis logical indicating if decadal analysis was performed
#' @param grouping_cols character string indicating the grouping column used in the analysis (if any)
#' @param by_condition character string indicating the condition column used in the analysis (if any)
#' @returns A cleaned data frame with model results
#'
#' @export
#' @examples
#' \dontrun{
#' cleaned <- clean_model_output(
#'    output,
#'    decade_analysis = decade_analysis,
#'    grouping_cols = grouping_cols,
#'    by_condition = by_condition
#'  )
#' }
clean_model_output <- function(
  output,
  decade_analysis,
  grouping_cols,
  by_condition = NULL
) {
  grouping_cols_syms <- if (!is.null(grouping_cols)) {
    rlang::syms(grouping_cols)
  } else {
    NULL
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

  mk_out <- output %>%
    dplyr::mutate(n_rows = purrr::map_int(data, nrow)) %>%
    tidyr::unnest(data) %>%
    {
      if (decade_analysis) {
        dplyr::select(
          .,
          "metric",
          surv_year_in_decade,
          surv_year,
          value,
          !!!grouping_cols_syms_with_cond,
          decade,
          mk_tau,
          mk_p_value,
          mk_logslope,
          mk_percentslope,
          mk_intercept,
          mk_warnings,
          mk_has_warnings,
          n_rows
        )
      } else {
        dplyr::select(
          .,
          "metric",
          surv_year,
          value,
          !!!grouping_cols_syms_with_cond,
          mk_tau,
          mk_p_value,
          mk_logslope,
          mk_percentslope,
          mk_intercept,
          mk_warnings,
          mk_has_warnings,
          n_rows
        )
      }
    }
}
