#' Distribution Plot
#'
#' Ridgeline density plot of every numeric column in \code{data}, optionally
#' normalized/standardized first so columns on different scales stay
#' comparable.
#'
#' @param data A data frame or tibble.
#' @param method Normalization applied before plotting: \code{"range"}
#'   (default, via \code{\link{range_norm}}), \code{"min_max"} (via
#'   \code{\link{min_max_norm}}), \code{"z"} (via \code{\link{z_norm}}), or
#'   \code{"none"}/\code{"raw"} to plot the values as-is.
#' @return A ggplot object.
#' @importFrom stats ecdf
#' @export
#' @examples
#' df <- data.frame(normal_dist = rnorm(1000, -10, .5), poisson_dist = rpois(1000, 5))
#' distribution_plot(df)
#' distribution_plot(df, method = "z")
#' distribution_plot(df, method = "none")
distribution_plot <- function(data, method = "range") {
  if (!(is.data.frame(data) || tibble::is_tibble(data))) {
    data <- try(tibble::as_tibble(data), silent = TRUE)
    if (inherits(data, "try-error")) {
      stop("Error: Must be an object that can be converted to a data frame.")
    }
  }

  data <- dplyr::select(data, dplyr::where(is.numeric))

  valid_methods <- c("range", "min-max", "min_max", "z", "z-score", "z_score", "raw", "none")
  if (!(method %in% valid_methods)) {
    stop(paste0("`method` must be one of: ", paste(paste0("'", valid_methods, "'"), collapse = ", ")))
  }

  if (method == "range") data <- range_norm(data)
  if (method %in% c("min-max", "min_max")) data <- min_max_norm(data)
  if (method %in% c("z", "z-score", "z_score")) data <- z_norm(data)

  long <- data |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::mutate(name = stats::reorder(.data$name, .data$value, FUN = mean))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$name,
                                           fill = ggplot2::after_stat((0.5 - abs(0.5 - ecdf)) * 100))) +
    ggridges::stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
    ggplot2::scale_fill_viridis_c(name = "Tail probability", direction = -1)

  if (method %in% c("raw", "none")) {
    p <- p + ggplot2::labs(title = "Distribution of Columns", x = "Value", y = "Column")
  } else if (method %in% c("range", "min-max", "min_max")) {
    p <- p + ggplot2::geom_vline(xintercept = 0.5, color = "red") +
      ggplot2::labs(title = "Distribution of Normalized Columns", x = "Normalized Value", y = "Column")
  } else {
    p <- p + ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = "Distribution of Standardized Columns", x = "Standardized Value", y = "Column")
  }
  p
}
