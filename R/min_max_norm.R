#' Min-Max Data Normalization
#'
#' Normalizes numerical values into \code{[0, 1]}. For a data frame, matrix,
#' or tibble, each numeric column is normalized independently against its
#' own min/max, and \strong{non-numeric columns are dropped} — matching the
#' original implementation this was migrated from.
#'
#' @details Missing values are preserved and ignored when finding the min/max;
#'   only finite values define the range. A constant input (zero range) maps
#'   every finite value to \code{0} instead of \code{NaN}, and an input with no
#'   finite values comes back as all \code{NA}.
#'
#' @param obj A numeric vector, matrix, data frame, or tibble.
#' @return A normalized object: a numeric vector stays a vector; a matrix
#'   stays a matrix; a data frame or tibble is reduced to just its (now
#'   normalized) numeric columns.
#' @export
#' @examples
#' min_max_norm(c(10, 20, 30, 40, 50))
#' min_max_norm(data.frame(x = c(1, 2, 3), y = c(-10, 0, 10), id = c("a", "b", "c")))
min_max_norm <- function(obj) {
  if (is_df_col(obj)) {
    return(min_max_norm_vector(obj))
  }
  if (is.matrix(obj) && !is.data.frame(obj)) {
    return(as.matrix(min_max_norm_data.frame(tibble::as_tibble(obj, .name_repair = "universal"))))
  }
  if (is.data.frame(obj)) {
    return(min_max_norm_data.frame(obj))
  }
  obj <- try(tibble::as_tibble(obj), silent = TRUE)
  if (inherits(obj, "try-error")) {
    stop("Error: Must be an object that can be converted to a vector or data frame.")
  }
  min_max_norm_data.frame(obj)
}

#' @keywords internal
#' @noRd
is_df_col <- function(obj) {
  is.atomic(obj) && is.vector(obj) && !(is.data.frame(obj) || tibble::is_tibble(obj) || is.matrix(obj))
}

#' @keywords internal
#' @noRd
min_max_norm_vector <- function(col) {
  if (!is.numeric(col) && !is.logical(col)) {
    stop("`obj` must be numeric (or logical), not <", class(col)[1], ">.", call. = FALSE)
  }
  rng <- finite_range(col)
  rescale_01(col, rng[1], rng[2])
}

# Range of the finite values only, so NA/NaN/Inf never poison the scaling and an
# all-missing input gives c(NA, NA) instead of Inf/-Inf plus a warning.
#' @keywords internal
#' @noRd
finite_range <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(c(NA_real_, NA_real_))
  }
  c(min(x), max(x))
}

# (x - lo) / (hi - lo), with two edge cases handled explicitly: a zero-width
# range maps every finite value to 0 (rather than NaN from 0/0), and a missing
# range (nothing finite to scale against) maps everything to NA. Non-finite
# inputs pass through unchanged.
#' @keywords internal
#' @noRd
rescale_01 <- function(x, lo, hi) {
  x <- x + 0
  if (is.na(lo) || is.na(hi)) {
    x[] <- NA_real_
    return(x)
  }
  if (hi == lo) {
    x[is.finite(x)] <- 0
    return(x)
  }
  (x - lo) / (hi - lo)
}

#' @keywords internal
#' @noRd
min_max_norm_data.frame <- function(data) {
  data |>
    dplyr::select(dplyr::where(is.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), min_max_norm_vector))
}

#' Range Normalization
#'
#' Like \code{\link{min_max_norm}}, but treats every numeric column as
#' coming from a single combined distribution — one global min/max is used
#' to rescale all of them together, rather than normalizing each column
#' independently. Unlike \code{min_max_norm}, non-numeric columns are kept
#' (only the numeric ones are transformed) — matching the original
#' implementation this was migrated from.
#'
#' @param obj A numeric vector, matrix, data frame, or tibble.
#' @return A normalized object of the same dimensions and class as \code{obj}.
#' @export
#' @examples
#' range_norm(data.frame(x = c(1, 2, 3), y = c(-10, 0, 10)))
range_norm <- function(obj) {
  if (is_df_col(obj)) {
    return(min_max_norm_vector(obj))
  }
  if (is.data.frame(obj)) {
    return(range_norm_data.frame(obj))
  }
  if (is.matrix(obj)) {
    return(as.matrix(range_norm_data.frame(tibble::as_tibble(obj, .name_repair = "universal"))))
  }
  obj <- try(tibble::as_tibble(obj), silent = TRUE)
  if (inherits(obj, "try-error")) {
    stop("Error: Must be an object that can be converted to a vector or data frame.")
  }
  range_norm_data.frame(obj)
}

#' @keywords internal
#' @noRd
range_norm_data.frame <- function(data) {
  long <- data |> dplyr::select(dplyr::where(is.numeric)) |> tidyr::pivot_longer(dplyr::everything())
  grng <- finite_range(long$value)
  data |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) rescale_01(x, grng[1], grng[2])))
}
