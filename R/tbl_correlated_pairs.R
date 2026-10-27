#' Table of Highly Correlated Feature Pairs
#'
#' Lists every pair of numeric features whose absolute correlation is at least `cutoff`, with
#' optional labels and an executive caption ("X of the Y features (Z%) are at least W%
#' correlated"). Where [drop_correlated_columns()] says which columns to remove, this table is
#' for the reader of the report.
#'
#' @param data A data frame.
#' @param features Numeric columns to check; `NULL` uses all numeric columns.
#' @param cutoff Absolute correlation threshold in (0, 1]; a pair at exactly the cutoff counts.
#' @param method `"pearson"` or `"spearman"` (pairwise complete observations). Pairs with an
#'   undefined correlation (a constant column, fewer than 3 shared rows) are not listed.
#' @param metadata Optional data frame with columns `columns` (feature names) and `labels`.
#'   Features without a label keep their name.
#' @param format `"kable"` (needs `knitr`), `"gt"` (needs `gt`) or `"tibble"`.
#' @param caption Build the caption (shown as the table caption for `"kable"` and `"gt"`, and
#'   always returned as the `caption` attribute of the tibble form).
#' @param digits Decimals of the correlation in the formatted tables.
#' @return For `"tibble"`, a tibble with `feature_1`, `feature_2`, `label_1`, `label_2` and
#'   `correlation`, sorted by decreasing absolute correlation, with attributes `caption`,
#'   `n_features` and `n_involved`. For the other formats the formatted table.
#' @seealso [drop_correlated_columns()], [plot_target_correlation_heatmap()]
#' @export
#' @examples
#' tbl_correlated_pairs(mtcars, cutoff = 0.85, format = "tibble")
tbl_correlated_pairs <- function(data, features = NULL, cutoff = 0.65,
                                 method = c("pearson", "spearman"), metadata = NULL,
                                 format = c("kable", "gt", "tibble"), caption = TRUE, digits = 3) {
  method <- match.arg(method)
  format <- match.arg(format)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.numeric(cutoff) || length(cutoff) != 1L || is.na(cutoff) || cutoff <= 0 || cutoff > 1) {
    stop("`cutoff` must be a single number in (0, 1].", call. = FALSE)
  }
  if (is.null(features)) {
    features <- names(data)[vapply(data, is.numeric, logical(1))]
  } else {
    gone <- setdiff(features, names(data))
    if (length(gone)) stop("`features` not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
    notnum <- features[!vapply(data[features], is.numeric, logical(1))]
    if (length(notnum)) stop("Non-numeric `features`: ", paste(notnum, collapse = ", "), ".", call. = FALSE)
  }
  if (length(features) < 2L) stop("Need at least two numeric features.", call. = FALSE)
  if (!is.null(metadata) && !(is.data.frame(metadata) && all(c("columns", "labels") %in% names(metadata)))) {
    stop("`metadata` must be a data frame with columns `columns` and `labels`.", call. = FALSE)
  }
  if (format == "kable") rlang::check_installed("knitr", reason = "for format = \"kable\".")
  if (format == "gt") rlang::check_installed("gt", reason = "for format = \"gt\".")

  m <- suppressWarnings(stats::cor(data[features], use = "pairwise.complete.obs", method = method))
  ix <- which(upper.tri(m), arr.ind = TRUE)
  r <- m[ix]
  hit <- !is.na(r) & abs(r) >= cutoff
  lab <- function(x) {
    if (is.null(metadata)) return(x)
    l <- as.character(metadata$labels[match(x, metadata$columns)])
    ifelse(is.na(l), x, l)
  }
  f1 <- features[ix[hit, 1]]
  f2 <- features[ix[hit, 2]]
  out <- tibble::tibble(feature_1 = f1, feature_2 = f2, label_1 = lab(f1), label_2 = lab(f2),
                        correlation = r[hit])
  out <- out[order(-abs(out$correlation)), , drop = FALSE]
  n_inv <- length(unique(c(out$feature_1, out$feature_2)))
  cap <- sprintf("%d of the %d features (%.1f%%) are at least %s%% correlated with another feature (%s).",
                 n_inv, length(features), 100 * n_inv / length(features),
                 format(round(100 * cutoff, 1), trim = TRUE), method)
  attr(out, "caption") <- cap
  attr(out, "n_features") <- length(features)
  attr(out, "n_involved") <- n_inv
  if (format == "tibble") return(out)
  shown <- out[, c("label_1", "label_2", "correlation")]
  names(shown) <- c("Feature 1", "Feature 2", "Correlation")
  cap_arg <- if (caption) cap else NULL
  if (format == "kable") {
    knitr::kable(shown, digits = digits, caption = cap_arg)
  } else {
    g <- gt::fmt_number(gt::gt(shown), columns = "Correlation", decimals = digits)
    if (caption) g <- gt::tab_header(g, title = cap)
    g
  }
}
