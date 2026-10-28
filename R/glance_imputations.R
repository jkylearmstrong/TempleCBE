#' Pool Model-Level Fit Statistics over Imputations
#'
#' Companion of [pool_imputations()] for the one-row-per-model summaries (`broom::glance()`, the
#' `glance` of [cox_single()]): concordance, Brier or integrated Brier score, AIC, degrees of
#' freedom and so on, over the same model fitted to each of `m` imputed data sets.
#'
#' @section Method:
#' Every numeric column is pooled as the mean over the imputations, reported with the
#' between-imputation standard deviation, minimum and maximum. This is the usual summary for
#' fit statistics, which have no Rubin variance of their own. If the column `std.error.<stat>`
#' exists (as `broom::glance()` gives for `concordance`), the statistic is instead pooled with
#' Rubin's rules through [pool_imputations()], giving a standard error, a t interval and the
#' fraction of missing information. Pooled concordance is a mean of C-indices, each computed on
#' its own completed data set; it is not a C-index of a pooled model. Columns named
#' `std.error.*` are not reported as statistics.
#'
#' @param x A list of one-row data frames (or tibbles), or a stacked data frame with a column
#'   naming the imputation.
#' @param imputation_col Name of the imputation column when `x` is stacked.
#' @param conf_level Confidence level for statistics that have a standard error.
#' @return A tibble with `statistic`, `m`, `estimate`, `between_sd`, `min`, `max`, and, where a
#'   standard error was available, `std.error`, `conf.low`, `conf.high` and `fmi` (otherwise `NA`).
#' @seealso [pool_imputations()]
#' @export
#' @examples
#' set.seed(1)
#' g <- lapply(1:5, function(i) data.frame(concordance = 0.7 + rnorm(1, sd = 0.01),
#'                                         std.error.concordance = 0.03, AIC = 400 + rnorm(1, sd = 2),
#'                                         df = 3))
#' glance_imputations(g)
glance_imputations <- function(x, imputation_col = "imputation", conf_level = 0.95) {
  if (is.data.frame(x)) {
    if (!imputation_col %in% names(x)) stop("`x` has no column `", imputation_col, "`.", call. = FALSE)
    x <- split(x, x[[imputation_col]])
  }
  if (!is.list(x) || length(x) < 2L) stop("At least two imputations are needed.", call. = FALSE)
  if (!all(vapply(x, function(d) is.data.frame(d) && nrow(d) == 1L, logical(1)))) {
    stop("Each imputation must be a one-row data frame.", call. = FALSE)
  }
  nm <- names(x[[1L]])
  if (!all(vapply(x, function(d) setequal(names(d), nm), logical(1)))) {
    stop("All imputations need the same columns.", call. = FALSE)
  }
  num <- setdiff(nm[vapply(x[[1L]], is.numeric, logical(1))], imputation_col)
  stat_cols <- num[!startsWith(num, "std.error.")]
  if (length(stat_cols) == 0L) stop("No numeric statistic to pool.", call. = FALSE)
  m <- length(x)
  rows <- lapply(stat_cols, function(s) {
    v <- vapply(x, function(d) as.numeric(d[[s]]), numeric(1))
    if (anyNA(v)) stop("Statistic `", s, "` is missing in at least one imputation.", call. = FALSE)
    out <- tibble::tibble(statistic = s, m = m, estimate = mean(v), between_sd = stats::sd(v),
                          min = min(v), max = max(v), std.error = NA_real_, conf.low = NA_real_,
                          conf.high = NA_real_, fmi = NA_real_)
    se_col <- paste0("std.error.", s)
    if (se_col %in% num) {
      fits <- lapply(x, function(d) data.frame(term = s, estimate = d[[s]], std.error = d[[se_col]]))
      p <- pool_imputations(fits, conf_level = conf_level)
      out$estimate <- p$estimate
      out$std.error <- p$std.error
      out$conf.low <- p$conf.low
      out$conf.high <- p$conf.high
      out$fmi <- p$fmi
    }
    out
  })
  dplyr::bind_rows(rows)
}
