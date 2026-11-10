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
#' The sign of a component is arbitrary (a refit can flip it), so describe direction relative to
#' the other loadings of the same component.
#'
#' @param pca_mod A `prcomp` object or a [run_pca()] result.
#' @param feature Name of one original variable (a row name of the rotation matrix).
#'   An unknown name is an error that lists the available names.
#' @param pc Component number or name (`"PC2"`). `pca_loading()` takes exactly one;
#'   `pca_variance_pct()` accepts several.
#' @param digits `NULL` (default for `pca_loading()` and `pca_variance_pct()`) returns numbers. A
#'   number of decimals returns text formatted with that many decimals, ready for inline prose
#'   (`pca_variance_pct(fit, 1, digits = 1)` is `"62.8"`). `pca_loading_range()` formats with 2
#'   decimals by default; `digits = NULL` there returns the numeric minimum and maximum.
#' @return `pca_loading()`: one number. `pca_variance_pct()`: a numeric vector, one value per `pc`.
#'   Both return character when `digits` is given. `pca_loading_range()`: one string such as
#'   `"0.31 to 0.52"`, the smallest and largest loading of `features` on `pc`.
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
    ok <- grepl("^PC[0-9]+$", pc, ignore.case = TRUE)
    if (!all(ok)) stop("`", arg, "` must be a component number or a name such as \"PC1\".", call. = FALSE)
    as.integer(sub("^PC", "", toupper(pc)))
  } else if (is.numeric(pc) && all(is.finite(pc)) && all(pc == round(pc))) {
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
pca_loading <- function(pca_mod, feature, pc, digits = NULL) {
  if (!is.null(digits)) return(.pca_fmt(pca_loading(pca_mod, feature, pc), digits))
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
pca_variance_pct <- function(pca_mod, pc, cumulative = FALSE, digits = NULL) {
  if (!is.null(digits)) return(.pca_fmt(pca_variance_pct(pca_mod, pc, cumulative = cumulative), digits))
  fit <- extract_prcomp(pca_mod, "pca_mod")
  pct <- 100 * fit$sdev^2 / sum(fit$sdev^2)
  idx <- .pca_pc_index(pc, length(pct))
  if (isTRUE(cumulative)) cumsum(pct)[idx] else pct[idx]
}

#' @rdname pca_accessors
#' @param features Names of two or more original variables.
#' @param absolute If `TRUE`, the range of the absolute loadings (sizes regardless of sign).
#'   Default `FALSE` gives the signed minimum and maximum.
#' @param sep Text between the two ends of the range (default `" to "`).
#' @export
pca_loading_range <- function(pca_mod, features, pc, digits = 2, absolute = FALSE, sep = " to ") {
  rot <- .pca_rotation(pca_mod)
  if (!is.character(features) || length(features) < 1L || anyNA(features)) {
    stop("`features` must be variable names.", call. = FALSE)
  }
  if (length(pc) != 1L) stop("`pc` must be a single component for `pca_loading_range()`.", call. = FALSE)
  j <- .pca_pc_index(pc, ncol(rot))
  gone <- setdiff(features, rownames(rot))
  if (length(gone)) {
    stop("`features` not in the PCA: ", paste(gone, collapse = ", "), ". Available: ",
         paste(rownames(rot), collapse = ", "), ".", call. = FALSE)
  }
  v <- unname(rot[features, j])
  if (isTRUE(absolute)) v <- abs(v)
  if (is.null(digits)) return(range(v))
  paste(.pca_fmt(range(v), digits), collapse = sep)
}

# fixed-decimal text for inline prose; a rounded negative zero is written as zero
.pca_fmt <- function(v, digits) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a non-negative number or NULL.", call. = FALSE)
  }
  out <- formatC(round(v, digits) + 0, format = "f", digits = digits)
  sub("^-(0(\\.0+)?)$", "\\1", out)
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
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n != round(n)) {
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
