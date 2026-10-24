#' Compare One Variable Before and After Normalization
#'
#' Two panels side by side: the variable in its original units and the same values after rescaling
#' (for example with [min_max_norm()]), each with its mean and standard deviation in the subtitle.
#' Needs the `patchwork` package.
#'
#' @param raw_vec,norm_vec Numeric vectors of the same length: the values before and after normalization.
#'   Pairs where either is missing are dropped.
#' @param var_label Name of the variable for the titles.
#' @param unit_label Optional unit of the original values (added to the x-axis label).
#' @param type `"density"` (default) or `"histogram"`.
#' @param bins Number of histogram bins (default 30).
#' @return A `patchwork` object (a ggplot-like object that prints as two panels).
#' @seealso [min_max_norm()], [distribution_plot()]
#' @export
#' @examples
#' x <- mtcars$hp
#' plot_normalization_comparison(x, min_max_norm(x), "Horsepower", "hp")
plot_normalization_comparison <- function(raw_vec,
                                          norm_vec,
                                          var_label,
                                          unit_label = NULL,
                                          type = c("density", "histogram"),
                                          bins = 30) {
  type <- match.arg(type)
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for plot_normalization_comparison().", call. = FALSE)
  }
  if (!is.numeric(raw_vec) || !is.numeric(norm_vec)) stop("`raw_vec` and `norm_vec` must be numeric.", call. = FALSE)
  if (length(raw_vec) != length(norm_vec)) stop("`raw_vec` and `norm_vec` must have the same length.", call. = FALSE)
  if (!is.character(var_label) || length(var_label) != 1L || is.na(var_label)) {
    stop("`var_label` must be a single string.", call. = FALSE)
  }
  ok <- !is.na(raw_vec) & !is.na(norm_vec)
  if (sum(ok) < 2L) stop("At least two complete pairs are needed.", call. = FALSE)
  raw_vec <- raw_vec[ok]
  norm_vec <- norm_vec[ok]

  panel <- function(v, xlab, title) {
    d <- data.frame(v = v)
    m <- mean(v)
    s <- stats::sd(v)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$v))
    p <- if (type == "density") {
      p + ggplot2::geom_density(fill = "#9D2235", alpha = 0.3, color = "#9D2235", na.rm = TRUE)
    } else {
      p + ggplot2::geom_histogram(bins = bins, fill = "#9D2235", alpha = 0.6, color = "white")
    }
    p +
      ggplot2::geom_vline(xintercept = m, linetype = "dashed", color = "grey30") +
      theme_cbe() +
      ggplot2::labs(title = title, subtitle = sprintf("mean %s, SD %s", signif(m, 3), signif(s, 3)), x = xlab, y = NULL)
  }
  raw_lab <- if (is.null(unit_label)) var_label else paste0(var_label, " (", unit_label, ")")
  patchwork::wrap_plots(
    panel(raw_vec, raw_lab, "Original units"),
    panel(norm_vec, paste0(var_label, " (normalized)"), "Normalized"),
    nrow = 1
  )
}
