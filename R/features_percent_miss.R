#' Calculate Percentage of Missing Data Per Feature
#'
#' Computes the count and percentage of missing and complete values for each
#' column in a dataset.
#'
#' @param data A data frame or tibble.
#' @param na_list Optional vector of additional values to treat as missing,
#'   passed to \code{\link{SumNa}} (beyond actual \code{NA}).
#' @return A tibble with columns `feature`, `SumNa`, `SumComp`, `PctNa`, `PctComp` sorted descending by `PctNa`.
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
#' features_percent_miss(df)
#'
#' df2 <- data.frame(a = c(1, "NA", 3), b = c("", 2, 3))
#' features_percent_miss(df2, na_list = c("NA", ""))
features_percent_miss <- function(data, na_list = NULL) {
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
    s_na <- SumNa(vec, na_list)
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
