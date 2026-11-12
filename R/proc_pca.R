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
#'   `difference`, `proportion`, `variance_pct`, `cum_variance_pct`.
#' @export
#' @examples
#' run_pca(prcomp(mtcars[, 1:4], scale. = TRUE))
#'
#' # Or fit the PCA in one step
#' run_pca(mtcars[, 1:4], scale = TRUE)
run_pca <- function(data, center = TRUE, scale = TRUE, ...) {
  dots <- list(...)
  if ("scale." %in% names(dots)) {
    scale <- dots[["scale."]]
    dots[["scale."]] <- NULL
  }

  pca_obj <- if (inherits(data, "prcomp")) {
    data
  } else if (is.data.frame(data)) {
    is_num <- vapply(data, is.numeric, logical(1))
    dropped <- names(data)[!is_num]
    if (length(dropped) > 0L) {
      message("Dropping non-numeric column(s) from PCA: ", paste(dropped, collapse = ", "))
    }
    num_data <- data[is_num]
    if (isTRUE(scale)) {
      # prcomp() errors on a constant column with no package-level message
      # naming it ("cannot rescale a constant/zero column to unit variance").
      constant <- names(num_data)[vapply(num_data, function(x) {
        x <- x[!is.na(x)]
        length(x) == 0L || length(unique(x)) < 2L
      }, logical(1))]
      if (length(constant) > 0L) {
        stop("Cannot scale constant column(s): ", paste(constant, collapse = ", "),
             ". Drop them, or use `scale = FALSE`.", call. = FALSE)
      }
    }
    do.call(stats::prcomp, c(list(x = num_data, center = center, scale. = scale), dots))
  } else {
    do.call(stats::prcomp, c(list(x = data, center = center, scale. = scale), dots))
  }

  if (!is.null(pca_obj$rotation) && is.null(rownames(pca_obj$rotation))) {
    rownames(pca_obj$rotation) <- paste0("V", seq_len(nrow(pca_obj$rotation)))
  }

  vars <- pca_obj$sdev^2
  # All-constant input (scale = FALSE) has total variance 0, so the
  # proportions used to be NaN.
  prop <- if (sum(vars) > 0) vars / sum(vars) else rep(0, length(vars))
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
