#' Compact Groupwise Numeric Summary
#'
#' Raw numeric summaries (n, missingness, mean, SD, median, quartiles, IQR, range) of numeric
#' variables, overall or by one or more grouping columns, as a long tibble for programmatic
#' filtering. No formatting or rounding: use [summary_table()] or [tbl_table_one()] for
#' presentation.
#'
#' @param data A data frame.
#' @param vars Numeric columns to summarise; default all numeric columns that are not in `by`.
#' @param by Grouping columns (character vector) or `NULL` for the whole data.
#' @param stats Statistics to return, any of `"n"`, `"n_missing"`, `"pct_missing"`, `"mean"`,
#'   `"sd"`, `"median"`, `"q1"`, `"q3"`, `"iqr"`, `"min"`, `"max"` (default all). `n` counts
#'   non-missing values; `pct_missing` is on a 0 to 100 scale.
#' @return A tibble with the `by` columns, `variable` and the requested statistics, one row per
#'   group and variable (a missing `by` value forms its own group). Statistics that cannot be
#'   computed (an all-missing group, the SD of one value) are `NA`.
#' @seealso [summary_table()], [tbl_table_one()], [describe_columns()]
#' @export
#' @examples
#' calc_grouped_summary(mtcars, vars = c("mpg", "hp"), by = "cyl")
#' calc_grouped_summary(airquality, stats = c("n", "pct_missing", "median"))
calc_grouped_summary <- function(data,
                                 vars = NULL,
                                 by = NULL,
                                 stats = c("n", "n_missing", "pct_missing", "mean", "sd", "median",
                                           "q1", "q3", "iqr", "min", "max")) {
  all_stats <- c("n", "n_missing", "pct_missing", "mean", "sd", "median", "q1", "q3", "iqr", "min", "max")
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  stats <- unique(as.character(stats))
  bad <- setdiff(stats, all_stats)
  if (length(bad) || length(stats) == 0L) {
    stop("Unknown statistic(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(by)) {
    gone <- setdiff(by, names(data))
    if (length(gone)) stop("`by` column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(vars)) {
    vars <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], by)
  } else {
    gone <- setdiff(vars, names(data))
    if (length(gone)) stop("`vars` column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
    notnum <- vars[!vapply(data[vars], is.numeric, logical(1))]
    if (length(notnum)) stop("Non-numeric `vars`: ", paste(notnum, collapse = ", "), ".", call. = FALSE)
  }
  if (length(vars) == 0L) stop("No numeric variables to summarise.", call. = FALSE)

  g <- dplyr::group_by(data, dplyr::across(dplyr::all_of(by)))
  keys <- dplyr::group_keys(g)
  idx <- dplyr::group_rows(g)

  one <- function(v) {
    ok <- v[!is.na(v)]
    q <- if (length(ok)) stats::quantile(ok, c(0.25, 0.75), names = FALSE) else c(NA_real_, NA_real_)
    all <- list(
      n = length(ok),
      n_missing = length(v) - length(ok),
      pct_missing = if (length(v)) 100 * (length(v) - length(ok)) / length(v) else NA_real_,
      mean = if (length(ok)) mean(ok) else NA_real_,
      sd = if (length(ok) > 1L) stats::sd(ok) else NA_real_,
      median = if (length(ok)) stats::median(ok) else NA_real_,
      q1 = q[1L], q3 = q[2L], iqr = q[2L] - q[1L],
      min = if (length(ok)) min(ok) else NA_real_,
      max = if (length(ok)) max(ok) else NA_real_
    )
    tibble::as_tibble(all[stats])
  }

  out <- lapply(vars, function(v) {
    s <- dplyr::bind_rows(lapply(idx, function(i) one(data[[v]][i])))
    dplyr::bind_cols(keys, tibble::tibble(variable = v), s)
  })
  tibble::as_tibble(dplyr::bind_rows(out))
}
