#' Cumulative Incidence with Competing Risks
#'
#' Aalen-Johansen estimate of the cumulative incidence of each cause at chosen times, optionally
#' by group. Unlike `1 - Kaplan-Meier` with the other causes treated as censored, this does not
#' overstate a cause's incidence when competing events remove subjects from risk.
#'
#' @section Scope:
#' This describes cumulative incidence. It does not fit a Fine-Gray subdistribution hazard model;
#' for that use `cmprsk::crr()` or `tidycmprsk`.
#'
#' @param data A data frame, one row per subject.
#' @param time_col Follow-up time.
#' @param event_col Event type: 0 (or the first level of a factor) for censored, then one code per
#'   cause.
#' @param group_col Optional grouping column.
#' @param times Times at which to report. Default: the quartiles of the observed event times.
#' @param conf_level Confidence level of the pointwise interval.
#' @return A tibble with `group` (when `group_col` is used), `cause`, `time`, `cif`, `std.error`,
#'   `conf.low`, `conf.high`.
#' @seealso [tbl_landmark_cutpoints()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(200, 0.1), type = sample(0:2, 200, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                 arm = rep(c("A", "B"), 100))
#' tbl_cumulative_incidence(d, "time", "type", group_col = "arm", times = c(5, 10))
tbl_cumulative_incidence <- function(data, time_col, event_col, group_col = NULL, times = NULL,
                                     conf_level = 0.95) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(time_col, event_col, group_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }
  ev <- data[[event_col]]
  ev <- if (is.factor(ev)) droplevels(ev) else factor(ev, levels = sort(unique(ev)))
  if (nlevels(ev) < 3L) stop("`event_col` needs a censoring code and at least two causes.", call. = FALSE)
  keep <- !is.na(data[[time_col]]) & !is.na(ev)
  if (!is.null(group_col)) keep <- keep & !is.na(data[[group_col]])
  d <- data.frame(time = data[[time_col]][keep], ev = ev[keep])
  if (!is.null(group_col)) d$group <- as.character(data[[group_col]][keep])
  if (is.null(times)) {
    times <- unname(stats::quantile(d$time[as.integer(d$ev) > 1L], c(0.25, 0.5, 0.75)))
  }
  if (!is.numeric(times) || length(times) == 0L || anyNA(times) || any(times < 0)) {
    stop("`times` must be non-negative numbers.", call. = FALSE)
  }

  one <- function(dd) {
    fit <- survival::survfit(survival::Surv(time, ev) ~ 1, data = dd, conf.int = conf_level)
    s <- summary(fit, times = times, extend = TRUE)
    states <- colnames(s$pstate)
    dplyr::bind_rows(lapply(which(states != "(s0)"), function(j) {
      tibble::tibble(cause = states[j], time = times, cif = s$pstate[, j], std.error = s$std.err[, j],
                     conf.low = s$lower[, j], conf.high = s$upper[, j])
    }))
  }
  if (is.null(group_col)) return(one(d))
  dplyr::bind_rows(lapply(sort(unique(d$group)), function(g) {
    dplyr::mutate(one(d[d$group == g, , drop = FALSE]), group = g, .before = 1)
  }))
}
