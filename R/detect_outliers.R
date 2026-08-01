#' Interquartile Range (IQR) Outlier Fences
#'
#' Computes lower and upper IQR fences for detecting numerical outliers.
#'
#' @param x A numeric vector.
#' @param k Multiplier for IQR (default 1.5).
#' @param na.rm Logical; whether to ignore NAs (default TRUE).
#' @return A named vector containing `lower` and `upper` bounds.
#' @export
#' @examples
#' calculate_fences(c(1, 2, 3, 4, 5, 100))
calculate_fences <- function(x, k = 1.5, na.rm = TRUE) {
  if (!is.numeric(x)) stop("Input x must be numeric.")
  qs <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- qs[2] - qs[1]
  c(lower = unname(qs[1] - k * iqr), upper = unname(qs[2] + k * iqr))
}

#' Flag Numerical Outliers
#'
#' Returns a logical vector indicating whether each value falls outside the IQR fences.
#'
#' @param x A numeric vector.
#' @param k IQR multiplier (default 1.5).
#' @param na.rm Logical; whether to ignore NAs (default TRUE).
#' @return Logical vector of length `length(x)`.
#' @export
#' @examples
#' flag_outliers(c(1, 2, 3, 4, 5, 100))
flag_outliers <- function(x, k = 1.5, na.rm = TRUE) {
  fences <- calculate_fences(x, k = k, na.rm = na.rm)
  (x < fences["lower"]) | (x > fences["upper"])
}

#' Detect Outliers Across Features
#'
#' Identifies outliers in a data frame using IQR fence thresholding.
#'
#' @param data A data frame or tibble.
#' @param k IQR multiplier (default 1.5).
#' @return A summary tibble listing variable names, outlier counts, and fence boundaries.
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 4, 100), b = c(10, 12, 11, 9, 8))
#' detect_outliers(df)
detect_outliers <- function(data, k = 1.5) {
  if (is.numeric(data)) {
    fences <- calculate_fences(data, k = k)
    out_flags <- flag_outliers(data, k = k)
    return(tibble::tibble(
      feature = "value",
      n_outliers = sum(out_flags, na.rm = TRUE),
      pct_outliers = mean(out_flags, na.rm = TRUE) * 100,
      lower_fence = fences["lower"],
      upper_fence = fences["upper"]
    ))
  }
  
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a numeric vector, data frame, or tibble.")
  }

  num_cols <- names(data)[sapply(data, is.numeric)]
  purrr::map_dfr(num_cols, function(col) {
    vec <- data[[col]]
    fences <- calculate_fences(vec, k = k)
    out_flags <- flag_outliers(vec, k = k)
    tibble::tibble(
      feature = col,
      n_outliers = sum(out_flags, na.rm = TRUE),
      pct_outliers = mean(out_flags, na.rm = TRUE) * 100,
      lower_fence = fences["lower"],
      upper_fence = fences["upper"]
    )
  })
}
