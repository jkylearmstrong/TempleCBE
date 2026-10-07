#' Z-Score Standard Normalization
#'
#' Standardizes numeric features to have mean = 0 and standard deviation = 1.
#' For a matrix or data frame each (numeric) column is standardized on its own.
#' A constant column (or a lone observation) has no spread to divide by and comes
#' back as 0, with \code{NA}s kept as \code{NA}. With \code{na.rm = FALSE}, a
#' column containing an \code{NA} has no defined mean and comes back all
#' \code{NA}.
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param na.rm Logical; whether to ignore NA values (default TRUE).
#' @return Z-score standardized numeric object.
#' @export
#' @examples
#' z_norm(c(10, 20, 30, 40, 50))
z_norm <- function(x, na.rm = TRUE) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, function(col) if (is.numeric(col)) z_norm(col, na.rm) else col)
    return(x)
  }
  if (is.matrix(x) && is.numeric(x)) {
    x[] <- apply(x, 2, z_norm, na.rm = na.rm)
    return(x)
  }
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector, matrix, or data frame.")
  }
  if (!na.rm && anyNA(x)) {
    return(x * NA_real_)
  }
  m <- mean(x, na.rm = na.rm)
  s <- stats::sd(x, na.rm = na.rm)
  if (is.na(s) || s == 0) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  (x - m) / s
}
