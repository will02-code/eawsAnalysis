#' Custom ggplot2 Theme for Package Plots
#'
#' A standardized, clean theme based on ggplot2::theme_bw with specific font and size presets.
#'
#' @param title_size Numeric. Font size for plot and legend titles. Default is 22.
#' @param text_size Numeric. Font size for axis text, strip text, and legend text. Default is 20.
#' @param ... Additional arguments passed to \code{ggplot2::theme()}.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @importFrom ggplot2 theme_bw theme element_text element_rect
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + my_theme()
#' }
my_theme <- function(title_size = 22, text_size = 20, ...) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 22,
        face = "bold",
        colour = "black",
        family = "tachoma" # Note: check spelling, did you mean "tachoma" or "Tahoma"?
      ),
      strip.text = ggplot2::element_text(size = text_size),
      strip.background.x = ggplot2::element_rect(
        fill = "white",
        colour = "black"
      ),
      axis.text = ggplot2::element_text(size = text_size, colour = "black"),
      axis.title = ggplot2::element_text(size = title_size, colour = "black"),
      legend.title = ggplot2::element_text(size = title_size, colour = "black"),
      legend.text = ggplot2::element_text(size = text_size, colour = "black"),
      ...
    )
}
