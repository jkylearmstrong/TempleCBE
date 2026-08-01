#' Min-Max Data Normalization
#'
#' Normalizes numerical values into a fixed range `[min_val, max_val]`, by default `[0, 1]`.
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param min_val Lower bound of target range (default 0).
#' @param max_val Upper bound of target range (default 1).
#' @param na.rm Logical; whether to remove NA values when computing min and max (default TRUE).
#' @return Normalized object of same dimensions and class.
#' @export
#' @examples
#' min_max_norm(c(10, 20, 30, 40, 50))
min_max_norm <- function(x, min_val = 0, max_val = 1, na.rm = TRUE) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = na.rm)
    if (rng[1] == rng[2]) {
      return(rep(min_val, length(x)))
    }
    return((x - rng[1]) / (rng[2] - rng[1]) * (max_val - min_val) + min_val)
  } else if (is.data.frame(x)) {
    x[] <- lapply(x, function(col) if (is.numeric(col)) min_max_norm(col, min_val, max_val, na.rm) else col)
    return(x)
  } else {
    stop("Input must be a numeric vector or data frame.")
  }
}

#' @rdname min_max_norm
#' @export
range_norm <- min_max_norm
