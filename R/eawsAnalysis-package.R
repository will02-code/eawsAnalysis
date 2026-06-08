#' eawsAnalysis: Analyse data from the Eastern Australian Waterbird Survey
#'
#' @section Main Functions:
#' The core functions you'll use in this package:
#' \itemize{
#'   \item \code{\link{clean_data_run_models}}: Main function which cleans data, runs models, and cleans output.
#'   \item \code{\link{run_mk_models}}: Side-car modeling function.
#'   \item \code{\link{fit_mk_model}}: Fit Mann-Kendall model to a single group of data.
#'   \item \code{\link{clean_clip_raw_data}}: Clip and clean raw waterbird data.
#'   \item \code{\link{get_bird_data}}: Extract cleaned waterbird data.
#'   \item \code{\link{clean_model_output}}: Clean output from run_mk_models().
#'   \item \code{\link{post_fitting_line_plot}}: Plot model results with ggplot2.
#' }
#'
#' @keywords internal
#'
#' ## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang expr
#' ## usethis namespace: end
"_PACKAGE"
