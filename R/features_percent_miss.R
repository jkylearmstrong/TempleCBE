#' Calculate Percentage of Missing Data Per Feature
#'
#' Computes the count and percentage of missing (`NA`) and complete values for each column in a dataset.
#'
#' @param data A data frame or tibble.
#' @return A tibble with columns `feature`, `SumNa`, `SumComp`, `PctNa`, `PctComp` sorted descending by `PctNa`.
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
#' features_percent_miss(df)
features_percent_miss <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  
  n_rows <- nrow(data)
  if (n_rows == 0) {
    return(tibble::tibble(
      feature = character(),
      SumNa = integer(),
      SumComp = integer(),
      PctNa = double(),
      PctComp = double()
    ))
  }

  res <- purrr::map_dfr(names(data), function(col) {
    vec <- data[[col]]
    s_na <- sum(is.na(vec))
    s_comp <- n_rows - s_na
    pct_na <- s_na / n_rows
    pct_comp <- s_comp / n_rows
    tibble::tibble(
      feature = col,
      SumNa = as.integer(s_na),
      SumComp = as.integer(s_comp),
      PctNa = pct_na,
      PctComp = pct_comp
    )
  })
  
  res <- dplyr::arrange(res, dplyr::desc(.data$PctNa))
  class(res) <- c("features_percent_miss", class(res))
  res
}
