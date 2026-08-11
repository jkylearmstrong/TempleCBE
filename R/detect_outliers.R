#' Calculate Inner and Outer IQR Fences
#'
#' @param col A numeric vector or column.
#' @return A one-row tibble with columns \code{lower_inner_fence},
#'   \code{upper_inner_fence} (at 1.5 x IQR — conventional "mild" outlier
#'   boundary) and \code{lower_outer_fence}, \code{upper_outer_fence} (at
#'   3 x IQR — "extreme" outlier boundary).
#' @export
#' @examples
#' calculate_fences(c(1, 2, 3, 4, 5, 100))
calculate_fences <- function(col) {
  col_no_na <- stats::na.omit(col)
  q1 <- stats::quantile(col_no_na, 0.25)
  q3 <- stats::quantile(col_no_na, 0.75)
  iqr <- stats::IQR(col_no_na)

  tibble::tibble(
    lower_inner_fence = unname(q1 - 1.5 * iqr),
    upper_inner_fence = unname(q3 + 1.5 * iqr),
    lower_outer_fence = unname(q1 - 3 * iqr),
    upper_outer_fence = unname(q3 + 3 * iqr)
  )
}

#' Flag and Classify Outliers
#'
#' @param col A numeric vector or column.
#' @return A one-column-input-turned-tibble with \code{value}, \code{.outlier}
#'   (logical) and \code{.outlier_type} (factor: \code{"NONE"}, \code{"MILD"},
#'   or \code{"EXTREME"}).
#' @export
#' @examples
#' flag_outliers(c(1, 2, 3, 4, 5, 100))
flag_outliers <- function(col) {
  fences <- calculate_fences(col)

  tibble::tibble(value = col) |>
    dplyr::mutate(
      .outlier = col <= fences$lower_inner_fence | col >= fences$upper_inner_fence,
      .outlier_type = dplyr::case_when(
        col <= fences$lower_outer_fence | col >= fences$upper_outer_fence ~ "EXTREME",
        col <= fences$lower_inner_fence | col >= fences$upper_inner_fence ~ "MILD",
        TRUE ~ "NONE"
      )
    ) |>
    dplyr::mutate(
      .outlier = factor(.data$.outlier),
      .outlier_type = factor(.data$.outlier_type, levels = c("NONE", "MILD", "EXTREME"))
    )
}

#' Detect Outliers Across a Data Frame's Numeric Columns
#'
#' Runs \code{\link{flag_outliers}} on every numeric column of \code{data}.
#'
#' @param data A matrix, data frame, or tibble.
#' @param outliers_only Logical (default \code{TRUE}); if \code{TRUE}, only
#'   rows actually flagged as outliers are returned.
#' @return A tibble with a \code{column} identifying which feature each row
#'   came from, plus \code{value}, \code{.outlier}, and \code{.outlier_type}.
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 4, 100), b = c(10, 12, 11, 9, 8))
#' detect_outliers(df)
#' detect_outliers(df, outliers_only = FALSE)
detect_outliers <- function(data, outliers_only = TRUE) {
  if (is.numeric(data) && !is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(value = data)
  } else if (is.matrix(data)) {
    data <- as.data.frame(data)
  } else if (!is.data.frame(data)) {
    stop("Input 'data' must be a numeric vector, matrix, data frame, or tibble.")
  }

  numeric_cols <- names(dplyr::select(data, dplyr::where(is.numeric)))

  outliers_detected <- purrr::map(
    stats::setNames(numeric_cols, numeric_cols),
    \(col_name) flag_outliers(data[[col_name]])
  ) |>
    purrr::list_rbind(names_to = "column")

  if (isTRUE(outliers_only)) {
    outliers_detected <- dplyr::filter(outliers_detected, .data$.outlier == TRUE)
  }
  outliers_detected
}
