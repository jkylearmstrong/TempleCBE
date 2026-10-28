#' Fine-Gray Subdistribution Hazard Regression for One Predictor
#'
#' Fits the Fine-Gray (1999) proportional subdistribution hazards model of one cause with the
#' other causes treated as competing events, for one predictor at a time. It is the regression
#' counterpart of [tbl_cumulative_incidence()], as [cox_single()] is for [km_single()]. Uses
#' `cmprsk::crr()`.
#'
#' @section Interpretation:
#' The estimate is a subdistribution hazard ratio (sHR). It describes the effect on the cumulative
#' incidence of the cause of interest, not on the rate among those still event-free, so it is not
#' comparable to a cause-specific hazard ratio from [cox_single()] with the competing events
#' censored. Subjects with a competing event stay in the risk set, which is what `crr()` does.
#' Rows with a missing time, event code or predictor are dropped. A factor or character predictor
#' gets one row per non-reference level (the first level is the reference). Terms are named as
#' `broom::tidy()` names them: a logical predictor gives `<feature>TRUE`, a factor `<feature><level>`.
#'
#' @param data A data frame, one row per subject.
#' @param time_col Follow-up time.
#' @param event_col Event type, as in [tbl_cumulative_incidence()]: a censoring code, then one code
#'   per cause.
#' @param feature Name of the predictor column.
#' @param failcode Code of the cause of interest (default 1).
#' @param cencode Code for censoring (default 0).
#' @param conf_level Confidence level.
#' @param exponentiate Report the subdistribution hazard ratio (default); `FALSE` gives the
#'   coefficient on the log scale.
#' @param ... Passed to `cmprsk::crr()` (for example `maxiter`).
#' @return A tibble with `feature`, `term`, `estimate`, `std.error` (of the log coefficient),
#'   `statistic`, `p.value`, `conf.low`, `conf.high`, `n` and `n_event` (events of the cause of
#'   interest among the rows used).
#' @seealso [tbl_cumulative_incidence()], [plot_cumulative_incidence()], [cox_single()]
#' @export
#' @examples
#' if (requireNamespace("cmprsk", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                   age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
#'   tbl_fine_gray_single(d, "time", "type", "age")
#'   tbl_fine_gray_single(d, "time", "type", "arm", failcode = 2)
#' }
tbl_fine_gray_single <- function(data, time_col, event_col, feature, failcode = 1, cencode = 0,
                                 conf_level = 0.95, exponentiate = TRUE, ...) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `tbl_fine_gray_single()`.", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(time_col, event_col, feature)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }
  if (length(failcode) != 1L || length(cencode) != 1L || identical(failcode, cencode)) {
    stop("`failcode` and `cencode` must be two different single codes.", call. = FALSE)
  }
  ev <- data[[event_col]]
  if (is.factor(ev)) ev <- as.character(ev)
  keep <- !is.na(data[[time_col]]) & !is.na(ev) & !is.na(data[[feature]])
  d <- data.frame(time = data[[time_col]][keep], ev = ev[keep], x = data[[feature]][keep])
  if (!failcode %in% d$ev) stop("No row has the event code `failcode` = ", failcode, ".", call. = FALSE)
  if (!cencode %in% d$ev) stop("No row has the censoring code `cencode` = ", cencode, ".", call. = FALSE)
  if (any(d$time < 0)) stop("`time_col` has negative values.", call. = FALSE)
  if (!is.numeric(d$x) && !is.logical(d$x)) d$x <- factor(d$x)
  if (is.factor(d$x)) d$x <- droplevels(d$x)
  if (is.factor(d$x) && nlevels(d$x) < 2L) {
    stop("`feature` has fewer than two levels in the rows used.", call. = FALSE)
  }
  if (!is.factor(d$x) && !isTRUE(stats::var(as.numeric(d$x)) > 0)) {
    stop("`feature` has zero variance in the rows used.", call. = FALSE)
  }

  mm <- stats::model.matrix(~ x, data = d)[, -1L, drop = FALSE]
  fit <- cmprsk::crr(ftime = d$time, fstatus = d$ev, cov1 = mm, failcode = failcode,
                     cencode = cencode, ...)
  beta <- as.numeric(fit$coef)
  se <- sqrt(diag(fit$var))
  z <- beta / se
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  tf <- if (isTRUE(exponentiate)) exp else identity
  # same labels as broom::tidy(): the feature name plus the level (`armB`, `chemoTRUE`)
  terms <- paste0(feature, sub("^x", "", colnames(mm)))
  tibble::tibble(
    feature = feature, term = terms, estimate = tf(beta), std.error = se, statistic = z,
    p.value = 2 * stats::pnorm(-abs(z)), conf.low = tf(beta - crit * se),
    conf.high = tf(beta + crit * se), n = nrow(d), n_event = sum(d$ev == failcode)
  )
}
