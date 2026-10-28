#' Multivariable Fine-Gray Subdistribution Hazard Regression
#'
#' Fits one Fine-Gray (1999) proportional subdistribution hazards model of a cause with several
#' predictors at once, using `cmprsk::crr()`. It is the adjusted counterpart of
#' [tbl_fine_gray_single()], as [cox_multi()] is for [cox_single()]. The estimates are subdistribution
#' hazard ratios (see [tbl_fine_gray_single()] for how to read them).
#'
#' @section Rows used:
#' Rows with a missing time, event code or any variable in `formula` are dropped (complete cases).
#' The design matrix comes from `stats::model.matrix()`, so factors, logicals, interactions and
#' transforms such as `log(x)` or `I(x^2)` work, and terms are named as `broom::tidy()` names them.
#'
#' @inheritParams tbl_fine_gray_single
#' @param formula One-sided formula of the predictors, for example `~ age + arm + sex`.
#' @return A tibble with `term`, `estimate`, `std.error` (of the log coefficient), `statistic`,
#'   `p.value`, `conf.low`, `conf.high`, `n` and `n_event`.
#' @seealso [tbl_fine_gray_single()], [tbl_fine_gray_screen()], [tbl_cumulative_incidence()]
#' @export
#' @examples
#' if (requireNamespace("cmprsk", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                   age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
#'   tbl_fine_gray_multi(d, "time", "type", ~ age + arm)
#' }
tbl_fine_gray_multi <- function(data, time_col, event_col, formula, failcode = 1, cencode = 0,
                                conf_level = 0.95, exponentiate = TRUE, ...) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `tbl_fine_gray_multi()`.", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("`formula` must be one-sided, for example `~ age + arm`.", call. = FALSE)
  }
  for (v in c(time_col, event_col)) {
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
  miss <- setdiff(all.vars(formula), names(data))
  if (length(miss)) stop("Not columns of `data`: ", paste(miss, collapse = ", "), ".", call. = FALSE)

  ev <- data[[event_col]]
  if (is.factor(ev)) ev <- as.character(ev)
  rhs <- data[, all.vars(formula), drop = FALSE]
  keep <- !is.na(data[[time_col]]) & !is.na(ev) & stats::complete.cases(rhs)
  d <- data[keep, , drop = FALSE]
  ev <- ev[keep]
  time <- d[[time_col]]
  if (!failcode %in% ev) stop("No row has the event code `failcode` = ", failcode, ".", call. = FALSE)
  if (!cencode %in% ev) stop("No row has the censoring code `cencode` = ", cencode, ".", call. = FALSE)
  if (any(time < 0)) stop("`time_col` has negative values.", call. = FALSE)
  for (v in names(rhs)) {
    if (is.character(d[[v]])) d[[v]] <- factor(d[[v]])
    if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  }

  mm <- stats::model.matrix(formula, data = d)
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  if (ncol(mm) == 0L) stop("`formula` has no predictor.", call. = FALSE)
  const <- apply(mm, 2L, function(col) !isTRUE(stats::var(col) > 0))
  if (any(const)) {
    stop("Zero-variance term(s) in the rows used: ", paste(colnames(mm)[const], collapse = ", "), ".",
         call. = FALSE)
  }
  fit <- cmprsk::crr(ftime = time, fstatus = ev, cov1 = mm, failcode = failcode, cencode = cencode, ...)
  beta <- as.numeric(fit$coef)
  se <- sqrt(diag(fit$var))
  z <- beta / se
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  tf <- if (isTRUE(exponentiate)) exp else identity
  tibble::tibble(
    term = colnames(mm), estimate = tf(beta), std.error = se, statistic = z,
    p.value = 2 * stats::pnorm(-abs(z)), conf.low = tf(beta - crit * se),
    conf.high = tf(beta + crit * se), n = nrow(d), n_event = sum(ev == failcode)
  )
}

#' Screen Many Predictors with Univariable Fine-Gray Models
#'
#' Runs [tbl_fine_gray_single()] for each feature and stacks the results, with a multiplicity
#' adjusted p-value. A feature that cannot be fitted (for example zero variance in the rows used)
#' is left out with a warning, so one bad column does not stop the screen.
#'
#' @inheritParams tbl_fine_gray_single
#' @param features Character vector of predictor columns.
#' @param p_adjust Method for `stats::p.adjust()` (default `"BH"`), applied across all rows.
#' @return The stacked [tbl_fine_gray_single()] rows plus `p.adj`.
#' @seealso [tbl_fine_gray_single()], [tbl_fine_gray_multi()]
#' @export
#' @examples
#' if (requireNamespace("cmprsk", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                   age = rnorm(150, 60, 8), bmi = rnorm(150, 27, 4), arm = rep(c("A", "B"), 75))
#'   tbl_fine_gray_screen(d, "time", "type", c("age", "bmi", "arm"))
#' }
tbl_fine_gray_screen <- function(data, time_col, event_col, features, failcode = 1, cencode = 0,
                                 conf_level = 0.95, exponentiate = TRUE, p_adjust = "BH", ...) {
  if (!is.character(features) || length(features) == 0L) {
    stop("`features` must be a character vector of column names.", call. = FALSE)
  }
  p_adjust <- match.arg(p_adjust, stats::p.adjust.methods)
  rows <- lapply(unique(features), function(f) {
    tryCatch(
      tbl_fine_gray_single(data, time_col, event_col, f, failcode = failcode, cencode = cencode,
                           conf_level = conf_level, exponentiate = exponentiate, ...),
      error = function(e) {
        warning("`", f, "` skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0L) stop("No feature could be fitted.", call. = FALSE)
  out$p.adj <- stats::p.adjust(out$p.value, method = p_adjust)
  out
}
