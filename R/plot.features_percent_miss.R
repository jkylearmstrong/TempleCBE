#' Plot method for features_percent_miss objects
#'
#' Creates a bar chart visualizing feature missingness percentages for objects
#' produced by \code{\link{features_percent_miss}}.
#'
#' @param x An object of class \code{features_percent_miss}.
#' @param top_n Optional integer to limit plot to top N features with highest missingness.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object representing feature missingness.
#'
#' @examples
#' res <- features_percent_miss(mtcars)
#' plot(res)
#'
#' @export
#' @rdname plot_features_percent_miss

plot.features_percent_miss <- function(x, top_n = NULL, ...) {
  plot_features_percent_miss(data = x, top_n = top_n)
}
