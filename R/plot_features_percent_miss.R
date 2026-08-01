#' Plot Missing Data Percentages Across Features
#'
#' Generates a horizontal bar chart visualizing the percentage of missing values per column.
#'
#' @param data A data frame, tibble, or the output of `features_percent_miss()`.
#' @param top_n Optional integer to limit to the top N features with highest missingness.
#' @return A ggplot object.
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
#' plot_features_percent_miss(df)
plot_features_percent_miss <- function(data, top_n = NULL) {
  if (!("PctNa" %in% names(data))) {
    df_miss <- features_percent_miss(data)
  } else {
    df_miss <- data
  }
  
  if (!is.null(top_n) && is.numeric(top_n)) {
    df_miss <- dplyr::slice_head(df_miss, n = top_n)
  }
  
  ggplot2::ggplot(df_miss, ggplot2::aes(x = stats::reorder(.data$feature, .data$PctNa), y = .data$PctNa * 100)) +
    ggplot2::geom_col(fill = "#d95f02", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title = "Feature Missingness Summary",
      x = "Feature",
      y = "Percentage Missing (%)"
    ) +
    ggplot2::theme_minimal()
}
