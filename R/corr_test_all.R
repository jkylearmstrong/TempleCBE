#' Pairwise Correlation Matrix and Significance Testing
#'
#' Computes pairwise correlations across all numeric features in a data frame,
#' returning correlation coefficients, p-values, and sample sizes.
#'
#' @param data A data frame or tibble containing numeric variables.
#' @param method Correlation method ("pearson", "kendall", "spearman").
#' @param use Strategy for handling missing values (default "pairwise.complete.obs").
#' @return A long tibble with pairs of variables, correlation coefficients (`r`), and p-values (`p_value`).
#' @importFrom corrplot corrplot
#' @export
#' @examples
#' corr_test_all(iris[, 1:4])
corr_test_all <- function(data, method = "pearson", use = "pairwise.complete.obs") {
  num_cols <- names(data)[sapply(data, is.numeric)]
  if (length(num_cols) < 2) {
    stop("Input 'data' must contain at least 2 numeric columns.")
  }
  
  df_num <- data[, num_cols, drop = FALSE]
  cor_mat <- stats::cor(df_num, use = use, method = method)
  
  pairs <- expand.grid(var1 = num_cols, var2 = num_cols, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$var1 < pairs$var2, ]
  
  res <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    v1 <- pairs$var1[i]
    v2 <- pairs$var2[i]
    ct <- tryCatch(
      stats::cor.test(df_num[[v1]], df_num[[v2]], method = method),
      error = function(e) list(estimate = NA, p.value = NA)
    )
    tibble::tibble(
      var1 = v1,
      var2 = v2,
      r = unname(ct$estimate),
      p_value = unname(ct$p.value)
    )
  })
  
  dplyr::arrange(res, .data$p_value)
}

#' Find Highly Correlated Columns
#'
#' Identifies numeric columns to drop for multicollinearity, via
#' \code{\link[caret]{findCorrelation}} on the pairwise correlation matrix of
#' \code{data}'s numeric columns. Named \code{find_correlation()} (not
#' \code{findCorrelation()}) so it doesn't shadow \pkg{caret}'s function of
#' the same name for anyone with both packages loaded.
#'
#' @param data A data frame or tibble.
#' @param use Passed to \code{\link[stats]{cor}} (default \code{"pairwise.complete.obs"}).
#' @param method Passed to \code{\link[stats]{cor}} (default \code{"pearson"}).
#' @param cutoff Absolute correlation above which a column is flagged (default 0.9).
#' @param verbose Logical; passed to \code{\link[caret]{findCorrelation}}.
#' @param names Logical; if \code{TRUE} (default) return column names instead of indices.
#' @param exact Passed to \code{\link[caret]{findCorrelation}}; defaults to
#'   \code{ncol(data) < 100} (the original had a bug here referencing an
#'   undefined \code{x} instead of \code{data} — fixed).
#' @return Character vector (or integer indices) of columns to remove.
#' @export
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   find_correlation(mtcars, cutoff = 0.8)
#' }
find_correlation <- function(data,
                              use = "pairwise.complete.obs",
                              method = "pearson",
                              cutoff = 0.9,
                              verbose = FALSE,
                              names = TRUE,
                              exact = ncol(data) < 100) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for find_correlation().")
  }
  num_data <- dplyr::select(data, dplyr::where(is.numeric))
  cor_matrix <- stats::cor(num_data, use = use, method = method)
  caret::findCorrelation(cor_matrix, cutoff = cutoff, verbose = verbose, names = names, exact = exact)
}
