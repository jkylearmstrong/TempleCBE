#' Z-Score Standard Normalization
#'
#' Standardizes numeric features to have mean = 0 and standard deviation = 1.
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param na.rm Logical; whether to ignore NA values (default TRUE).
#' @return Z-score standardized numeric object.
#' @export
#' @examples
#' z_norm(c(10, 20, 30, 40, 50))
z_norm <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    m <- mean(x, na.rm = na.rm)
    s <- stats::sd(x, na.rm = na.rm)
    if (is.na(s) || s == 0) {
      return(ifelse(is.na(x), NA_real_, 0))
    }
    return((x - m) / s)
  } else if (is.data.frame(x)) {
    x[] <- lapply(x, function(col) if (is.numeric(col)) z_norm(col, na.rm) else col)
    return(x)
  } else {
    stop("Input must be a numeric vector or data frame.")
  }
}
