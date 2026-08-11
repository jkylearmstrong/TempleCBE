#' Manhattan Plot
#'
#' @param .data A data frame, e.g. the output of \code{\link{multiple_t_test}}.
#' @param var Unquoted column of variable labels.
#' @param log_p Unquoted column of \code{-log10(p.value)}.
#' @param highlight_significant Logical (default \code{TRUE}); shade the
#'   region above the p < 0.05 line.
#' @return A ggplot object.
#' @export
#' @examples
#' mtcars |>
#'   dplyr::mutate(am = factor(am)) |>
#'   multiple_t_test(.class = "am") |>
#'   manhattan_plot(var = var, log_p = log_p)
manhattan_plot <- function(.data, var, log_p, highlight_significant = TRUE) {
  p <- ggplot2::ggplot(.data, ggplot2::aes(x = {{ var }}, y = {{ log_p }}, color = {{ var }})) +
    ggplot2::geom_point()

  if (isTRUE(highlight_significant)) {
    p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                                ymin = -log10(0.05), ymax = Inf, alpha = 0.2, fill = "yellow")
  }

  p + ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "none") +
    ggplot2::labs(x = "Variable", y = "-log10(p-value)", title = "Manhattan Plot")
}

#' Volcano Plot
#'
#' @param .data A data frame, e.g. the output of \code{\link{multiple_t_test}}.
#' @param log2_fold_change Unquoted column of log2 fold-change.
#' @param log_p Unquoted column of \code{-log10(p.value)}.
#' @param var Unquoted column of variable labels.
#' @param highlight_significant Logical (default \code{TRUE}); shade the
#'   region above the p < 0.05 line.
#' @return A ggplot object.
#' @export
#' @examples
#' mtcars |>
#'   dplyr::mutate(am = factor(am)) |>
#'   multiple_t_test(.class = "am") |>
#'   volcano_plot(log2_fold_change = log2_fold_change, log_p = log_p, var = var)
volcano_plot <- function(.data, log2_fold_change, log_p, var, highlight_significant = TRUE) {
  p <- ggplot2::ggplot(.data, ggplot2::aes(x = {{ log2_fold_change }}, y = {{ log_p }}, color = {{ var }})) +
    ggplot2::geom_point()

  if (isTRUE(highlight_significant)) {
    p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                                ymin = -log10(0.05), ymax = Inf, alpha = 0.2, fill = "yellow")
  }

  p + ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "log2(Fold-Change)", y = "-log10(p-value)", title = "Volcano Plot")
}
