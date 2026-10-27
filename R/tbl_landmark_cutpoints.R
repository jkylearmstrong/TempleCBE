#' Survival Cutpoints at Several Landmark Times
#'
#' For each landmark time, keeps the subjects still at risk (follow-up beyond the landmark),
#' restarts their clock at the landmark, splits a feature into low and high groups and compares
#' the survival of the two. One call replaces a copy of the analysis per landmark.
#'
#' @section Feature values:
#' By default `feature` is one value per subject (a baseline value). With `measure_time_col`
#' (and `id_col`) the data are long: for each landmark the value used is each subject's last
#' measurement at or before the landmark, so nothing measured after the landmark is used.
#'
#' @section Cutpoints and p-values:
#' `"surv_cutpoint"` picks the cutpoint with [km_cutpoint_search()] (smallest log-rank p-value
#' among candidates with at least `min_prop` of the subjects in each group). That p-value is then
#' optimistic, because the cut was chosen on the same data; use it for description, or
#' validate the cut elsewhere. `"median"` and `"quantile"` (cut at `probs`) are not data-driven.
#' The hazard ratio is high versus low, from a Cox model; the p-value is the log-rank test.
#'
#' @param data A data frame, one row per subject, or long with `measure_time_col`.
#' @param feature Numeric feature column.
#' @param time_col,status_col Survival time (same unit as `landmarks`) and event indicator.
#' @param landmarks Landmark times.
#' @param method `"surv_cutpoint"`, `"median"` or `"quantile"`.
#' @param probs Probability of the cut for `method = "quantile"` (default 0.75, so the top quarter
#'   is "high").
#' @param min_prop Smallest share of subjects per group for `"surv_cutpoint"`.
#' @param id_col,measure_time_col See Feature values; long data only.
#' @return A tibble with one row per landmark: `landmark`, `n`, `events`, `cutpoint`, `n_low`,
#'   `n_high`, `hr`, `conf.low`, `conf.high`, `p.value`, `status` (`"ok"` or why the landmark was
#'   not analysed).
#' @seealso [km_cutpoint_search()], [batch_cutpoint_survdiff()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(200, 1 / 80), status = rbinom(200, 1, 0.7), x = rnorm(200))
#' tbl_landmark_cutpoints(d, "x", "time", "status", landmarks = c(15, 30, 60), method = "median")
tbl_landmark_cutpoints <- function(data, feature, time_col = "survival_time_min", status_col = "status",
                                   landmarks = c(15, 30, 60, 90, 120),
                                   method = c("surv_cutpoint", "median", "quantile"),
                                   probs = 0.75, min_prop = 0.10, id_col = NULL, measure_time_col = NULL) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(feature, time_col, status_col, id_col, measure_time_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[feature]])) stop("`feature` must be numeric.", call. = FALSE)
  if (!is.numeric(landmarks) || length(landmarks) == 0L || anyNA(landmarks) || any(landmarks < 0)) {
    stop("`landmarks` must be non-negative numbers.", call. = FALSE)
  }
  if (is.null(id_col) != is.null(measure_time_col)) {
    stop("`id_col` and `measure_time_col` go together.", call. = FALSE)
  }
  if (!is.numeric(probs) || length(probs) != 1L || probs <= 0 || probs >= 1) stop("`probs` must be in (0, 1).", call. = FALSE)

  rows <- lapply(landmarks, function(L) {
    empty <- function(st, n = NA_integer_, ev = NA_integer_) {
      tibble::tibble(landmark = L, n = n, events = ev, cutpoint = NA_real_, n_low = NA_integer_,
                     n_high = NA_integer_, hr = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                     p.value = NA_real_, status = st)
    }
    d <- data.frame(.time = data[[time_col]], .status = as.numeric(data[[status_col]]), .x = data[[feature]])
    if (!is.null(id_col)) {
      d$.id <- data[[id_col]]
      d$.mt <- data[[measure_time_col]]
      d <- d[!is.na(d$.mt) & d$.mt <= L & !is.na(d$.x), , drop = FALSE]
      d <- d[order(d$.id, d$.mt), , drop = FALSE]
      d <- d[!duplicated(d$.id, fromLast = TRUE), , drop = FALSE]
    }
    d <- d[stats::complete.cases(d[, c(".time", ".status", ".x")]) & d$.time > L, , drop = FALSE]
    d$.time <- d$.time - L
    n <- nrow(d)
    ev <- sum(d$.status == 1)
    if (n < 10L || ev < 2L) return(empty("too few subjects or events at risk", n, ev))
    if (length(unique(d$.x)) < 2L) return(empty("feature is constant", n, ev))
    cut <- switch(method,
      median = stats::median(d$.x),
      quantile = unname(stats::quantile(d$.x, probs)),
      surv_cutpoint = tryCatch(
        suppressWarnings(km_cutpoint_search(d, ".time", ".status", ".x", min_prop = min_prop)$optimal_cutpoint),
        error = function(e) NA_real_
      )
    )
    if (is.na(cut)) return(empty("no cutpoint found", n, ev))
    d$.high <- as.integer(d$.x > cut)
    n_high <- sum(d$.high)
    if (n_high == 0L || n_high == n) return(empty("cutpoint leaves one group empty", n, ev))
    fit <- tryCatch(suppressWarnings(survival::coxph(survival::Surv(.time, .status) ~ .high, data = d)),
                    error = function(e) NULL)
    if (is.null(fit)) return(empty("Cox model failed", n, ev))
    sd <- survival::survdiff(survival::Surv(.time, .status) ~ .high, data = d)
    ci <- summary(fit)$conf.int
    tibble::tibble(landmark = L, n = n, events = ev, cutpoint = cut, n_low = n - n_high, n_high = n_high,
                   hr = ci[1, "exp(coef)"], conf.low = ci[1, 3], conf.high = ci[1, 4],
                   p.value = stats::pchisq(sd$chisq, 1, lower.tail = FALSE), status = "ok")
  })
  dplyr::bind_rows(rows)
}
