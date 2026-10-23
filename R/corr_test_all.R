#' Pairwise Correlation Tests Across All Numeric Columns
#'
#' Runs \code{\link[stats]{cor.test}} on every pair of numeric columns in a
#' data frame and returns one row per pair.
#'
#' Pairs are enumerated in column order (via \code{\link[utils]{combn}}), so
#' \code{var1} is always the column that appears first in \code{data}. A pair
#' whose test cannot be computed (e.g. fewer than three complete observations)
#' is kept with \code{NA} results rather than aborting the whole run. Invalid
#' arguments (an unknown \code{method}, \code{alternative}, a bad
#' \code{conf.level}, ...) are errors, not \code{NA} rows.
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
  method <- match.arg(method, c("pearson", "kendall", "spearman"))

  # Surface invalid cor.test() arguments (a bad `alternative`, `conf.level`,
  # `exact`, ...) now, on a small clean sample: the per-pair tryCatch below is
  # only meant to keep pairs whose *data* cannot be tested, and would otherwise
  # turn a typo into a full table of NAs.
  suppressWarnings(stats::cor.test(c(1, 2, 3, 4, 5), c(2, 1, 4, 3, 5), method = method, ...))

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
#' **Note:** `find_correlation()` identifies column names or indices to *remove*
#' to mitigate multicollinearity. To inspect the pairwise correlation coefficients
#' themselves in a tidy table, use [correlated_pairs_table()] or [corr_test_all()].
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
#' @seealso [correlated_pairs_table()], [corr_test_all()], [plot_corr_scatter()]
#' @export
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   drop_correlated_columns(mtcars, cutoff = 0.8)
#' }
drop_correlated_columns <- function(data,
                                    use = "pairwise.complete.obs",
                                    method = "pearson",
                                    cutoff = 0.9,
                                    verbose = FALSE,
                                    names = TRUE,
                                    exact = ncol(data) < 100) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for drop_correlated_columns().")
  }
  if (is.matrix(data)) {
    # If already a square correlation matrix, pass directly
    if (nrow(data) == ncol(data) && !is.null(colnames(data)) && isTRUE(all.equal(unname(diag(data)), rep(1, nrow(data))))) {
      cor_matrix <- data
      if (is.null(rownames(cor_matrix))) {
        rownames(cor_matrix) <- colnames(cor_matrix)
      }
    } else {
      cor_matrix <- stats::cor(data, use = use, method = method)
    }
  } else {
    num_data <- dplyr::select(as.data.frame(data), dplyr::where(is.numeric))
    cor_matrix <- stats::cor(num_data, use = use, method = method)
  }
  if (!is.null(colnames(cor_matrix)) && is.null(rownames(cor_matrix))) {
    rownames(cor_matrix) <- colnames(cor_matrix)
  }
  caret::findCorrelation(cor_matrix, cutoff = cutoff, verbose = verbose, names = names, exact = exact)
}

#' Formatted Table of Highly Correlated Variable Pairs
#'
#' Evaluates pairwise correlation between all numeric columns in `data` and returns
#' a tidy table of variable pairs exceeding a correlation threshold.
#'
#' Unlike [drop_correlated_columns()], which returns column names or indices to *drop* for multicollinearity,
#' `correlated_pairs_table()` preserves and reports the pairwise correlation metrics, absolute
#' magnitude, and p-values for all pairs above `cutoff`.
#'
#' @param data A data frame or tibble.
#' @param cutoff Minimum absolute correlation threshold (default 0.50). Set to 0 to return all pairs.
#' @param method Correlation method: `"pearson"` (default), `"kendall"`, or `"spearman"`.
#' @param use Missing value handling passed to [stats::cor.test] (default `"pairwise.complete.obs"`).
#' @param sort Sorting order: `"abs_desc"` (default, descending absolute correlation),
#'   `"desc"` (descending raw correlation), `"p_value"` (ascending p-value), or `"none"`.
#' @return A tibble with columns `var1`, `var2`, `r`, `abs_r`, `p_value`.
#' @seealso [drop_correlated_columns()], [corr_test_all()], [plot_corr_scatter()]
#' @export
#' @examples
#' correlated_pairs_table(mtcars, cutoff = 0.7)
correlated_pairs_table <- function(data,
                                   cutoff = 0.50,
                                   method = c("pearson", "spearman", "kendall"),
                                   use = c("pairwise.complete.obs", "complete.obs"),
                                   sort = c("abs_desc", "desc", "p_value", "none")) {
  method <- match.arg(method)
  use <- match.arg(use)
  sort <- match.arg(sort)

  all_pairs <- corr_test_all(data, method = method, use = use, columns = "compact", sort = "none")
  if (nrow(all_pairs) == 0L) {
    return(tibble::tibble(var1 = character(), var2 = character(), r = numeric(), abs_r = numeric(), p_value = numeric()))
  }

  all_pairs$abs_r <- abs(all_pairs$r)

  if (!is.null(cutoff) && !is.na(cutoff)) {
    all_pairs <- all_pairs[!is.na(all_pairs$abs_r) & all_pairs$abs_r >= cutoff, , drop = FALSE]
  }

  res <- switch(sort,
    abs_desc = dplyr::arrange(all_pairs, dplyr::desc(.data$abs_r)),
    desc = dplyr::arrange(all_pairs, dplyr::desc(.data$r)),
    p_value = dplyr::arrange(all_pairs, .data$p_value),
    none = all_pairs
  )

  tibble::as_tibble(res[, c("var1", "var2", "r", "abs_r", "p_value")])
}

#' Scatter Plot of Two Correlated Variables With Regression Fit and Statistics
#'
#' Visualizes the relationship between two continuous variables with a linear regression line,
#' confidence interval band, and subtitle/caption reporting Pearson or Spearman correlation
#' coefficient (r) and p-value. Replaces inline copies across reports.
#'
#' @param data Data frame containing `x` and `y` columns.
#' @param x Character string naming the x-axis variable.
#' @param y Character string naming the y-axis variable.
#' @param method Correlation method: `"pearson"` (default) or `"spearman"`.
#' @param point_color Point fill/outline color (default Temple Cherry `"#9D2235"`).
#' @param line_color Regression line color (default charcoal `"#2B2B2B"`).
#' @param se Logical. Overlay confidence interval band (default `TRUE`).
#' @param title Optional plot title.
#' @param subtitle Optional subtitle. If `NULL` (default), dynamically formats correlation statistic and p-value.
#' @param base_size Base font size (default 12).
#' @return A `ggplot2` object.
#' @seealso [correlated_pairs_table()], [plot_rank_heatmap()]
#' @export
plot_corr_scatter <- function(data,
                              x,
                              y,
                              method = c("pearson", "spearman"),
                              point_color = "#9D2235",
                              line_color = "#2B2B2B",
                              se = TRUE,
                              title = NULL,
                              subtitle = NULL,
                              base_size = 12) {
  method <- match.arg(method)
  if (!all(c(x, y) %in% names(data))) {
    stop("Both '", x, "' and '", y, "' must be present in `data`.", call. = FALSE)
  }

  df <- data[!is.na(data[[x]]) & !is.na(data[[y]]), c(x, y), drop = FALSE]
  if (nrow(df) < 3) {
    stop("At least 3 complete observations are required to compute correlation and regression.", call. = FALSE)
  }

  if (is.null(subtitle)) {
    test <- suppressWarnings(stats::cor.test(df[[x]], df[[y]], method = method))
    r_val <- unname(test$estimate)
    p_val <- unname(test$p.value)
    p_str <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)
    metric_label <- if (method == "pearson") "r" else "rho"
    subtitle <- sprintf("%s = %.3f, %s (N = %d)", metric_label, r_val, p_str, nrow(df))
  }

  if (is.null(title)) {
    title <- sprintf("%s vs %s", y, x)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(color = point_color, alpha = 0.6, size = 2) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = line_color, se = se, fill = "grey80", linewidth = 1) +
    theme_cbe(base_size = base_size) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x,
      y = y
    )

  p
}

#' Heatmap of Variable or Model Ranks Across Resamples or Metrics
#'
#' Generates a tile heatmap displaying ranks of features or candidate models across
#' cross-validation resamples, metrics, or methods. Replaces inline copies across reports.
#'
#' @param data A data frame containing item names, resampling split/method identifiers, and ranks.
#' @param item_col Character name of feature or model column (y-axis).
#' @param group_col Character name of fold, split, or metric column (x-axis).
#' @param rank_col Character name of rank column (fill).
#' @param low_color Color for best rank (rank 1, default Temple Cherry `"#9D2235"`).
#' @param high_color Color for worst rank (default light grey `"#F4F4F4"`).
#' @param title Optional plot title.
#' @param base_size Base font size (default 12).
#' @return A `ggplot2` object.
#' @export
plot_rank_heatmap <- function(data,
                              item_col,
                              group_col,
                              rank_col,
                              low_color = "#9D2235",
                              high_color = "#F4F4F4",
                              title = "Rank Stability Across Resamples",
                              base_size = 12) {
  if (!all(c(item_col, group_col, rank_col) %in% names(data))) {
    stop("Required columns '", item_col, "', '", group_col, "', and '", rank_col, "' must be in `data`.", call. = FALSE)
  }

  df <- data[!is.na(data[[item_col]]) & !is.na(data[[group_col]]) & !is.na(data[[rank_col]]), , drop = FALSE]

  # Order items by average rank
  avg_rank <- dplyr::group_by(df, .data[[item_col]])
  avg_rank <- dplyr::summarize(avg_rank, mean_rank = mean(.data[[rank_col]]), .groups = "drop")
  avg_rank <- dplyr::arrange(avg_rank, .data$mean_rank)

  df[[item_col]] <- factor(df[[item_col]], levels = rev(avg_rank[[item_col]]))
  df[[group_col]] <- factor(df[[group_col]])

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[group_col]], y = .data[[item_col]], fill = .data[[rank_col]])) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data[[rank_col]], 0)), size = 3.5, color = "black") +
    ggplot2::scale_fill_gradient(low = low_color, high = high_color, name = "Rank") +
    theme_cbe(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = title,
      x = group_col,
      y = item_col
    )
}

