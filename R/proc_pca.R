#' Process and Plot Principal Component Analysis (PCA)
#'
#' Extracts a per-component variance summary from a PCA fit. Accepts either an
#' existing \code{\link[stats]{prcomp}} object or raw data, in which case
#' \code{\link[stats]{prcomp}} is run first.
#'
#' @param data A `prcomp` object, or a numeric matrix/data frame to fit PCA on.
#' @param center,scale Passed to \code{\link[stats]{prcomp}} (as `center` and
#'   `scale.`) when `data` is raw data; ignored when `data` is already a
#'   `prcomp` object.
#' @param ... Further arguments passed to \code{\link[stats]{prcomp}} when
#'   `data` is raw data.
#' @return A tibble with one row per component: `component`, `eigenvalue`,
#'   `variance_pct`, `cum_variance_pct`.
#' @export
#' @examples
#' proc_pca(prcomp(mtcars[, 1:4], scale. = TRUE))
#'
#' # Or fit the PCA in one step
#' proc_pca(mtcars[, 1:4], scale = TRUE)
proc_pca <- function(data, center = TRUE, scale = TRUE, ...) {
  pca_obj <- if (inherits(data, "prcomp")) {
    data
  } else if (is.data.frame(data)) {
    num_data <- dplyr::select(data, dplyr::where(is.numeric))
    stats::prcomp(num_data, center = center, scale. = scale, ...)
  } else {
    stats::prcomp(data, center = center, scale. = scale, ...)
  }

  vars <- pca_obj$sdev^2
  prop <- vars / sum(vars)
  pct_var <- prop * 100
  cum_pct <- cumsum(pct_var)
  diffs <- c(-diff(vars), NA_real_)

  res <- tibble::tibble(
    component = paste0("PC", seq_along(vars)),
    eigenvalue = vars,
    difference = diffs,
    proportion = prop,
    variance_pct = pct_var,
    cum_variance_pct = cum_pct
  )
  attr(res, "prcomp") <- pca_obj
  class(res) <- c("cbe_pca", class(res))
  res
}
