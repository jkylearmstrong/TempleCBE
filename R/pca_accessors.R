#' Scalar Accessors for PCA Results (for Inline Prose)
#'
#' Small helpers that return one loading, one variance percentage or a short ranked list from a
#' PCA fit, so a Quarto or R Markdown report can write
#' `` `r pca_loading(fit, "age", 1)` `` instead of keeping local accessor functions.
#'
#' @section Supported objects:
#' A [stats::prcomp()] object, or the result of [run_pca()] (which carries its `prcomp` fit).
#' Anything else is an error. [stats::princomp()] fits are not supported.
#'
#' @section Components:
#' `pc` is a component number (`1`, `2`, ...) or a name (`"PC1"`). A component that does not exist
#' in the fit is an error, never `NA`. For `prcomp(..., rank. = k)` the variance percentages still
#' use all components of the full fit (`sdev` is complete), so they are shares of the total variance.
#'
#' @section Scale:
#' Variance percentages are on a 0 to 100 scale (`12.3` means 12.3 percent), like `variance_pct`
#' in [run_pca()]. Loadings are the elements of `rotation`: signed, and unit-length per component.
#'
#' @param pca_mod A `prcomp` object or a [run_pca()] result.
#' @param feature Name of one original variable (a row name of the rotation matrix).
#'   An unknown name is an error that lists the available names.
#' @param pc Component number or name (`"PC2"`). `pca_loading()` takes exactly one;
#'   `pca_variance_pct()` accepts several.
#' @return `pca_loading()`: one number. `pca_variance_pct()`: a numeric vector, one value per `pc`.
#'   `pca_top_features()`: a tibble with `feature`, `loading` (signed), `abs_loading` and `rank`.
#' @seealso [run_pca()], [pca_percent_var_explained()], [pca_feature_loading_heatmap()]
#' @name pca_accessors
#' @examples
#' fit <- run_pca(mtcars[, 1:5], scale = TRUE)
#' pca_loading(fit, "mpg", 1)
#' pca_variance_pct(fit, 1)
#' pca_variance_pct(fit, 2, cumulative = TRUE)
#' pca_top_features(fit, 1, n = 3)
NULL

# component index/name -> integer position in the fit, with a clear error
.pca_pc_index <- function(pc, n_pc, arg = "pc") {
  if (length(pc) < 1L || anyNA(pc)) {
    stop("`", arg, "` must be a component number or name such as \"PC1\".", call. = FALSE)
  }
  idx <- if (is.character(pc)) {
    ok <- grepl("^PC[0-9]+$", pc)
    if (!all(ok)) stop("`", arg, "` must be a component number or a name such as \"PC1\".", call. = FALSE)
    as.integer(sub("^PC", "", pc))
  } else if (is.numeric(pc) && all(pc == round(pc))) {
    as.integer(pc)
  } else {
    stop("`", arg, "` must be a component number or a name such as \"PC1\".", call. = FALSE)
  }
  if (any(idx < 1L | idx > n_pc)) {
    stop("`", arg, "` must be between 1 and ", n_pc, " (the number of components in the fit).", call. = FALSE)
  }
  idx
}

# rotation as a matrix with usable row/column names
.pca_rotation <- function(pca_mod) {
  fit <- extract_prcomp(pca_mod, "pca_mod")
  rot <- fit$rotation
  if (is.null(rownames(rot))) rownames(rot) <- paste0("V", seq_len(nrow(rot)))
  rot
}

#' @rdname pca_accessors
#' @export
pca_loading <- function(pca_mod, feature, pc) {
  rot <- .pca_rotation(pca_mod)
  if (!is.character(feature) || length(feature) != 1L || is.na(feature)) {
    stop("`feature` must be one variable name.", call. = FALSE)
  }
  if (length(pc) != 1L) stop("`pc` must be a single component for `pca_loading()`.", call. = FALSE)
  j <- .pca_pc_index(pc, ncol(rot))
  if (!feature %in% rownames(rot)) {
    stop("`feature` \"", feature, "\" is not in the PCA. Available: ",
         paste(rownames(rot), collapse = ", "), ".", call. = FALSE)
  }
  unname(rot[feature, j])
}

#' @rdname pca_accessors
#' @param cumulative If `TRUE`, the percentage of variance explained by components 1 to `pc`
#'   together (each value of `pc` gives the cumulative share up to it).
#' @export
pca_variance_pct <- function(pca_mod, pc, cumulative = FALSE) {
  fit <- extract_prcomp(pca_mod, "pca_mod")
  pct <- 100 * fit$sdev^2 / sum(fit$sdev^2)
  idx <- .pca_pc_index(pc, length(pct))
  if (isTRUE(cumulative)) cumsum(pct)[idx] else pct[idx]
}

#' @rdname pca_accessors
#' @param n Number of features to return (default 5; fewer if the fit has fewer variables).
#' @param by `"abs"` (default) ranks by the size of the loading regardless of sign; `"positive"`
#'   and `"negative"` rank only the loadings of that sign, largest first. Ties keep the order of
#'   the variables in the fit.
#' @export
pca_top_features <- function(pca_mod, pc, n = 5L, by = c("abs", "positive", "negative")) {
  by <- match.arg(by)
  rot <- .pca_rotation(pca_mod)
  if (length(pc) != 1L) stop("`pc` must be a single component for `pca_top_features()`.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
    stop("`n` must be a positive whole number.", call. = FALSE)
  }
  j <- .pca_pc_index(pc, ncol(rot))
  loading <- rot[, j]
  keep <- switch(by, abs = rep(TRUE, length(loading)), positive = loading > 0, negative = loading < 0)
  key <- switch(by, abs = -abs(loading), positive = -loading, negative = loading)
  df <- tibble::tibble(
    feature = rownames(rot)[keep],
    loading = unname(loading[keep]),
    abs_loading = abs(unname(loading[keep]))
  )
  df <- df[order(key[keep], seq_len(nrow(df))), , drop = FALSE]
  df <- utils::head(df, as.integer(n))
  df$rank <- seq_len(nrow(df))
  df
}
