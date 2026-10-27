#' PCA Component as a LaTeX Linear Combination
#'
#' Writes one principal component as a LaTeX sum of signed loadings times feature names, for a
#' report (for example PC_1 = +0.412 x - 0.231 y). Loadings are the elements of `rotation` (signed; the sign of a component is
#' arbitrary). Underscores and other LaTeX specials in feature names are escaped.
#'
#' @param pca_mod A [stats::prcomp()] object or a [run_pca()] result.
#' @param pc Component number or name (`"PC2"`); exactly one.
#' @param feature_names Optional display names for the features, either one per rotation row
#'   (in order) or a named vector (names = original features) that may cover only some of them.
#'   `NULL` uses the row names of the rotation.
#' @param max_terms Keep only this many terms, those with the largest absolute loading (shown
#'   in that order, followed by `\dots`); `NULL` keeps all in the original order.
#' @param prefix Component label prefix.
#' @param digits Decimals of the loadings.
#' @return A single string of LaTeX math, without delimiters (wrap it in `$$ ... $$`).
#' @seealso [pca_component_cutoff()], [pca_top_features()], [pca_eqns()]
#' @export
#' @examples
#' fit <- prcomp(mtcars[, 1:4], scale. = TRUE)
#' pca_equation_latex(fit, 1, digits = 2)
#' pca_equation_latex(fit, 2, max_terms = 2)
pca_equation_latex <- function(pca_mod, pc = 1, feature_names = NULL, max_terms = NULL,
                               prefix = "PC", digits = 3) {
  rot <- .pca_rotation(pca_mod)
  idx <- .pca_pc_index(pc, ncol(rot))
  if (length(idx) != 1L) stop("`pc` must be a single component.", call. = FALSE)
  load <- rot[, idx]
  nm <- rownames(rot)
  if (!is.null(feature_names)) {
    if (!is.null(names(feature_names))) {
      hit <- match(nm, names(feature_names))
      nm[!is.na(hit)] <- as.character(feature_names[hit[!is.na(hit)]])
    } else {
      if (length(feature_names) != length(nm)) {
        stop("`feature_names` must have one entry per feature (", length(nm), ") or be named.", call. = FALSE)
      }
      nm <- as.character(feature_names)
    }
  }
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) {
    stop("`digits` must be a non-negative number.", call. = FALSE)
  }
  trimmed <- FALSE
  if (!is.null(max_terms)) {
    if (!is.numeric(max_terms) || length(max_terms) != 1L || max_terms < 1) {
      stop("`max_terms` must be a positive number.", call. = FALSE)
    }
    if (max_terms < length(load)) {
      keep <- order(abs(load), decreasing = TRUE)[seq_len(max_terms)]
      load <- load[keep]
      nm <- nm[keep]
      trimmed <- TRUE
    }
  }
  esc <- function(x) {
    x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
    gsub("([_%&#$])", "\\\\\\1", x)
  }
  rhs <- paste(sprintf("%s \\text{%s}", sprintf("%+.*f", digits, load), esc(nm)), collapse = " ")
  if (trimmed) rhs <- paste(rhs, "\\dots")
  sprintf("%s_{%d} = %s", prefix, idx, rhs)
}

#' Number of Components Needed to Reach a Cumulative Variance Threshold
#'
#' @param pca_mod A [stats::prcomp()] object or a [run_pca()] result.
#' @param threshold Cumulative share of the total variance, in (0, 1] (default 0.80).
#' @return The smallest number of components whose cumulative variance share is at least
#'   `threshold`, as an integer with the attribute `cumulative` (the shares for all components).
#'   For `prcomp(..., rank. = k)` the shares are of the full variance; a threshold that the
#'   retained components cannot reach is an error.
#' @seealso [pca_variance_pct()], [pca_equation_latex()]
#' @export
#' @examples
#' pca_component_cutoff(prcomp(mtcars, scale. = TRUE), 0.9)
pca_component_cutoff <- function(pca_mod, threshold = 0.80) {
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold <= 0 || threshold > 1) {
    stop("`threshold` must be a single number in (0, 1].", call. = FALSE)
  }
  fit <- extract_prcomp(pca_mod, "pca_mod")
  v <- fit$sdev^2
  cum <- cumsum(v) / sum(v)
  k <- which(cum >= threshold - 1e-12)[1L]
  if (is.na(k) || k > ncol(fit$rotation)) {
    stop("The retained components do not reach a cumulative variance of ", threshold, ".", call. = FALSE)
  }
  structure(as.integer(k), cumulative = cum)
}
