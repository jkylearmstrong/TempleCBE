#' Paired Assay Agreement Table (Bias, Limits of Agreement, Lin's CCC)
#'
#' For each row of `pairs_df`, compares two measurement columns of the same specimens: the
#' mean difference (bias) `method_1 - method_2`, the SD of the differences, Bland-Altman 95
#' percent limits of agreement (`bias +/- 1.96 SD`), Lin's concordance correlation coefficient
#' and the percentage of rows missing each method.
#'
#' Only rows where both methods are present are paired. Repeated measurements of the same
#' subject are treated as independent, so the limits are naive for longitudinal data; `id_col`
#' only adds the number of subjects.
#'
#' @param data A data frame.
#' @param pairs_df Data frame with columns `parameter`, `method_1` and `method_2` (names of
#'   numeric columns of `data`).
#' @param id_col Optional subject column (adds `n_subjects`).
#' @param time_col Optional time column; rows with a missing time are excluded, as they cannot be
#'   matched to a time point.
#' @param metrics Statistics to return, any of `"bias"`, `"sd_diff"`, `"loa_lower"`,
#'   `"loa_upper"`, `"ccc"`, `"pct_miss_1"`, `"pct_miss_2"`. `parameter`, `method_1`, `method_2`
#'   and `n_pairs` are always returned.
#' @param digits Decimals for rounding; `NULL` for none.
#' @return A tibble with one row per pair. Statistics that cannot be computed (fewer than two
#'   pairs, constant data for the CCC) are `NA`.
#' @seealso [tbl_two_sample_compare()]
#' @export
#' @examples
#' d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1.1, 2.3, 2.9, 4.2, 5.4))
#' tbl_paired_assay_agreement(d, data.frame(parameter = "x", method_1 = "a", method_2 = "b"))
tbl_paired_assay_agreement <- function(data, pairs_df, id_col = NULL, time_col = NULL,
                                       metrics = c("bias", "sd_diff", "loa_lower", "loa_upper",
                                                   "ccc", "pct_miss_1", "pct_miss_2"),
                                       digits = 2) {
  all_metrics <- c("bias", "sd_diff", "loa_lower", "loa_upper", "ccc", "pct_miss_1", "pct_miss_2")
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.data.frame(pairs_df) || !all(c("parameter", "method_1", "method_2") %in% names(pairs_df))) {
    stop("`pairs_df` must have columns `parameter`, `method_1` and `method_2`.", call. = FALSE)
  }
  bad <- setdiff(metrics, all_metrics)
  if (length(bad) || length(metrics) == 0L) stop("Unknown metric(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)
  for (v in c(id_col, time_col)) {
    if (!v %in% names(data)) stop("`", v, "` is not a column of `data`.", call. = FALSE)
  }
  use <- if (is.null(time_col)) rep(TRUE, nrow(data)) else !is.na(data[[time_col]])

  one <- function(m1, m2) {
    for (m in c(m1, m2)) {
      if (!m %in% names(data)) stop("`", m, "` is not a column of `data`.", call. = FALSE)
      if (!is.numeric(data[[m]])) stop("`", m, "` must be numeric.", call. = FALSE)
    }
    x <- data[[m1]][use]
    y <- data[[m2]][use]
    ok <- !is.na(x) & !is.na(y)
    n <- sum(ok)
    dd <- x[ok] - y[ok]
    bias <- if (n > 0L) mean(dd) else NA_real_
    sdd <- if (n > 1L) stats::sd(dd) else NA_real_
    ccc <- NA_real_
    if (n > 1L) {
      vx <- mean((x[ok] - mean(x[ok]))^2)
      vy <- mean((y[ok] - mean(y[ok]))^2)
      cxy <- mean((x[ok] - mean(x[ok])) * (y[ok] - mean(y[ok])))
      den <- vx + vy + (mean(x[ok]) - mean(y[ok]))^2
      if (den > 0) ccc <- 2 * cxy / den
    }
    res <- tibble::tibble(
      n_pairs = n,
      bias = bias, sd_diff = sdd,
      loa_lower = bias - 1.96 * sdd, loa_upper = bias + 1.96 * sdd,
      ccc = ccc,
      pct_miss_1 = if (length(x)) 100 * mean(is.na(x)) else NA_real_,
      pct_miss_2 = if (length(y)) 100 * mean(is.na(y)) else NA_real_
    )
    if (!is.null(id_col)) res$n_subjects <- length(unique(data[[id_col]][use][ok]))
    res
  }

  rows <- Map(one, as.character(pairs_df$method_1), as.character(pairs_df$method_2))
  out <- dplyr::bind_cols(
    tibble::tibble(parameter = as.character(pairs_df$parameter),
                   method_1 = as.character(pairs_df$method_1),
                   method_2 = as.character(pairs_df$method_2)),
    dplyr::bind_rows(rows)
  )
  keep <- c("parameter", "method_1", "method_2", "n_pairs", if (!is.null(id_col)) "n_subjects", metrics)
  out <- out[, keep]
  if (!is.null(digits)) {
    num <- intersect(metrics, names(out))
    out[num] <- lapply(out[num], round, digits)
  }
  out
}
