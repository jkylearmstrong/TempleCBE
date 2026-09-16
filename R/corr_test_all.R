#' Pairwise Correlation Tests Across All Numeric Columns
#'
#' Runs \code{\link[stats]{cor.test}} on every pair of numeric columns in a
#' data frame and returns one row per pair.
#'
#' Pairs are enumerated in column order (via \code{\link[utils]{combn}}), so
#' \code{var1} is always the column that appears first in \code{data}. A pair
#' whose test cannot be computed (e.g. fewer than three complete observations)
#' is kept with \code{NA} results rather than aborting the whole run.
#'
#' @param data A data frame or tibble. Non-numeric columns are ignored.
#' @param method Correlation method passed to \code{\link[stats]{cor.test}}:
#'   \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}.
#' @param use How missing values are handled. \code{"pairwise.complete.obs"}
#'   (default) tests each pair on the rows where both columns are observed,
#'   which is what \code{cor.test()} does natively. \code{"complete.obs"}
#'   first drops every row with a missing value in any numeric column, so all
#'   pairs are tested on the same rows.
#' @param columns Output shape. \code{"compact"} (default) returns
#'   \code{var1}, \code{var2}, \code{r}, \code{p_value}. \code{"tidy"} returns
#'   \code{var1}, \code{var2}, then every column \code{broom::tidy()} reports
#'   for the test, with \code{estimate} renamed \code{cor}: \code{cor},
#'   \code{statistic}, \code{p.value}, \code{parameter}, \code{conf.low},
#'   \code{conf.high}, \code{method}, \code{alternative}. \code{parameter} and
#'   the confidence limits are only reported by methods that compute them
#'   (Pearson).
#' @param sort Row order. \code{"p_value"} (default) is ascending p-value;
#'   \code{"estimate"} is descending correlation; \code{"abs_estimate"} is
#'   descending absolute correlation; \code{"none"} keeps pair order. Ties keep
#'   pair order, and \code{NA}s sort last.
#' @param ... Further arguments passed to \code{\link[stats]{cor.test}}, such
#'   as \code{alternative}, \code{conf.level}, or \code{exact}.
#' @return A tibble with one row per pair of numeric columns.
#' @importFrom corrplot corrplot
#' @export
#' @examples
#' corr_test_all(iris[, 1:4])
#'
#' # Every cor.test() statistic, strongest positive correlation first
#' corr_test_all(mtcars[, c("mpg", "hp", "wt", "qsec")], columns = "tidy", sort = "estimate")
corr_test_all <- function(data,
                          method = "pearson",
                          use = c("pairwise.complete.obs", "complete.obs"),
                          columns = c("compact", "tidy"),
                          sort = c("p_value", "estimate", "abs_estimate", "none"),
                          ...) {
  use <- match.arg(use)
  columns <- match.arg(columns)
  sort <- match.arg(sort)

  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(num_cols) < 2) {
    stop("Input 'data' must contain at least 2 numeric columns.")
  }

  df_num <- as.data.frame(data)[, num_cols, drop = FALSE]
  if (use == "complete.obs") {
    df_num <- stats::na.omit(df_num)
  }

  res <- purrr::map_dfr(utils::combn(num_cols, 2, simplify = FALSE), function(pair) {
    test <- tryCatch(
      broom::tidy(stats::cor.test(df_num[[pair[1]]], df_num[[pair[2]]], method = method, ...)),
      error = function(e) tibble::tibble(estimate = NA_real_, p.value = NA_real_)
    )
    dplyr::bind_cols(tibble::tibble(var1 = pair[1], var2 = pair[2]), test)
  })

  res <- switch(sort,
    p_value = dplyr::arrange(res, .data$p.value),
    estimate = dplyr::arrange(res, dplyr::desc(.data$estimate)),
    abs_estimate = dplyr::arrange(res, dplyr::desc(abs(.data$estimate))),
    none = res
  )

  if (columns == "compact") {
    return(dplyr::select(res, "var1", "var2", r = "estimate", p_value = "p.value"))
  }
  dplyr::rename(res, cor = "estimate")
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
