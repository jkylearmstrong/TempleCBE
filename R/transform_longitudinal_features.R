#' Per-Subject Summaries of Repeated Measurements
#'
#' Collapses each subject's repeated values of a feature into one number, using one or more
#' transformations that are often tried as predictors of an outcome: change from baseline,
#' within-subject variability, or the largest swing.
#'
#' @section Definitions:
#' Within a subject and feature, missing values are dropped and the rest are ordered by `time_col`
#' (ties in time keep the data order). `n_obs` is the number of values used.
#' * `"pct_change_baseline"`: `100 * (last - first) / abs(first)`, with `first` the value at the earliest
#'   time (the baseline) and `last` the value at the latest time; the absolute value keeps the sign meaning
#'   "went up" or "went down" when the baseline is negative. `NA` if the baseline is 0 or there is only
#'   one value.
#' * `"variance"` and `"sd"`: the sample variance and standard deviation (divisor `n - 1`) of all values;
#'   `NA` with fewer than two values.
#' * `"max_signed_diff"`: `max - min` of the values, **signed by time order**: positive when the
#'   (first) maximum comes after the (first) minimum, negative when it comes before, and 0 when all values
#'   are equal. `NA` with fewer than two values.
#'
#' @param data A data frame in long format (one row per subject and time).
#' @param id_col,time_col Names of the subject and numeric time columns.
#' @param features Character vector of numeric feature columns.
#' @param methods One or more of `"pct_change_baseline"`, `"variance"`, `"sd"`, `"max_signed_diff"`.
#' @param format `"long"` (default; one row per subject, feature and method) or `"wide"` (one row per
#'   subject, columns named `<feature>__<method>`).
#' @return A tibble. Long format: the id column (named as `id_col`), `feature`, `method`, `value`,
#'   `n_obs`. Wide format: the id column and the value columns.
#' @seealso [batch_cox_single()], [plot_feature_transform_heatmap()]
#' @export
#' @examples
#' long <- data.frame(id = rep(1:3, each = 3), time = rep(c(0, 5, 15), 3),
#'                    hr = c(80, 90, 70, 60, 61, 62, 100, 90, 80))
#' transform_longitudinal_features(long, "id", "time", "hr")
transform_longitudinal_features <- function(data,
                                            id_col,
                                            time_col,
                                            features,
                                            methods = c("pct_change_baseline", "variance", "sd", "max_signed_diff"),
                                            format = c("long", "wide")) {
  format <- match.arg(format)
  all_methods <- c("pct_change_baseline", "variance", "sd", "max_signed_diff")
  methods <- unique(methods)
  if (!all(methods %in% all_methods)) {
    stop("`methods` must be among: ", paste(all_methods, collapse = ", "), ".", call. = FALSE)
  }
  if (length(methods) == 0L) stop("`methods` must name at least one method.", call. = FALSE)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (a in c("id_col", "time_col")) {
    v <- get(a)
    if (!is.character(v) || length(v) != 1L || !v %in% names(data)) {
      stop("`", a, "` must name one column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  features <- as.character(features)
  if (length(features) == 0L) stop("`features` must name at least one column.", call. = FALSE)
  gone <- setdiff(features, names(data))
  if (length(gone)) stop("Feature(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  bad <- features[!vapply(data[features], is.numeric, logical(1))]
  if (length(bad)) stop("Feature(s) must be numeric: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (anyNA(data[[id_col]])) stop("`id_col` must not contain missing values.", call. = FALSE)
  if (id_col %in% c("feature", "method", "value", "n_obs")) {
    stop("`id_col` must not be named feature, method, value or n_obs; rename the column first.", call. = FALSE)
  }

  summarise_one <- function(x, tm) {
    ok <- !is.na(x) & !is.na(tm)
    x <- x[ok]
    tm <- tm[ok]
    o <- order(tm)
    x <- x[o]
    n <- length(x)
    res <- c(pct_change_baseline = NA_real_, variance = NA_real_, sd = NA_real_, max_signed_diff = NA_real_)
    if (n >= 2L) {
      if (x[1] != 0) res[["pct_change_baseline"]] <- 100 * (x[n] - x[1]) / abs(x[1])
      res[["variance"]] <- stats::var(x)
      res[["sd"]] <- stats::sd(x)
      imax <- which.max(x)
      imin <- which.min(x)
      res[["max_signed_diff"]] <- if (imax == imin) 0 else sign(imax - imin) * (x[imax] - x[imin])
    }
    list(values = res, n = n)
  }

  ids <- unique(data[[id_col]])
  idx <- split(seq_len(nrow(data)), factor(data[[id_col]], levels = ids))
  rows <- list()
  for (f in features) {
    for (k in seq_along(idx)) {
      s <- summarise_one(data[[f]][idx[[k]]], data[[time_col]][idx[[k]]])
      rows[[length(rows) + 1L]] <- tibble::tibble(
        id = ids[k], feature = f, method = methods, value = unname(s$values[methods]), n_obs = s$n
      )
    }
  }
  out <- dplyr::bind_rows(rows)
  names(out)[1] <- id_col
  if (format == "wide") {
    out$.col <- paste(out$feature, out$method, sep = "__")
    wide <- tidyr::pivot_wider(out[, c(id_col, ".col", "value")], names_from = ".col", values_from = "value")
    return(wide)
  }
  out
}
