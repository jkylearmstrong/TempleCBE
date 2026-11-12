# Time-varying-coefficient Cox models, fit with survival::coxph() and a tt()
# term: the R counterpart of inst/sas/coxtvc.sas. survival::survfit() and
# predict() refuse coxph fits with tt() terms, so the baseline hazard and the
# survival predictions here are computed from the coefficients directly (see
# tvc_breslow()).

#' Time-Varying-Coefficient Cox Model
#'
#' Fits a Cox model in which the coefficients of `tv_vars` vary with time:
#' the log hazard ratio of a time-varying predictor `x` is
#' \eqn{\beta + \gamma f(t)}, with `f` the time function (`log` by default),
#' instead of a constant `\eqn{\beta}`. This is the usual remedy when
#' [tbl_schoenfeld_audit()] flags a proportional-hazards violation for a term:
#' fit the term with a time-varying coefficient rather than dropping it or
#' stratifying on it. The SAS counterpart is `inst/sas/coxtvc.sas`.
#'
#' The fit is [survival::coxph()] with a `tt()` term, so the model is an
#' ordinary Cox fit with one extra term per time-varying predictor: the
#' coefficient on `x` is its log hazard ratio where `f(t) = 0` (at `t = 1` for
#' `time_fn = "log"`), and the coefficient on `tt(x)` is how that log hazard
#' ratio changes per unit of `f(t)`. Use [hazard_ratio_at_time()] for the
#' hazard ratio at a given time.
#'
#' @details
#' **Limits of this draft.** Time-varying predictors must be numeric columns;
#' a factor or interaction term cannot be a `tv_var`. `strata()` terms are not
#' supported. Survival predictions extrapolate flat past the last observed
#' event time (the cumulative baseline hazard is constant there), as with a
#' Breslow estimate. The linear predictor varies with time, so
#' `predict(type = "linear_pred")` needs one `eval_time`.
#'
#' @param data A data frame with the outcome and predictors.
#' @param formula A two-sided formula with a [survival::Surv()] outcome, such
#'   as `survival::Surv(time, status) ~ age + sex`. Terms may not already use
#'   `tt()`; name time-varying predictors in `tv_vars` instead.
#' @param tv_vars Character vector of predictor names whose coefficients vary
#'   with time. Each must be a term of `formula` and a numeric column of
#'   `data`.
#' @param time_fn The time function `f(t)`: `"log"` (default, `log(t)`),
#'   `"identity"` (`t`), `"sqrt"` (`sqrt(t)`), or a function of one numeric
#'   vector. It must return finite values at the observed event times, so
#'   `"log"` needs event times above zero.
#' @param conf_level Confidence level of the hazard ratios (default `0.95`).
#' @param ... Further arguments passed to [survival::coxph()], such as `ties`
#'   or `weights`. Not `tt`, which this function sets itself.
#' @return An object of class `cox_time_varying`: a list with `fit` (the
#'   [survival::coxph()] fit), `terms` (a tibble with `term`, `estimate`,
#'   `std_error`, `hr`, `conf_low`, `conf_high`, `statistic`, `p_value` and
#'   `time_varying`), `tv_vars`, `time_fn` (the name of the time function, or
#'   `"user function"`), `n`, `n_events`, and `conf_level`. `tidy()` returns
#'   `terms`; `predict()` gives linear predictors at a time or survival
#'   probabilities.
#' @seealso [tbl_schoenfeld_audit()], [hazard_ratio_at_time()],
#'   [recurrent_cox()], [cox_single()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(1)
#'   n <- 200
#'   d <- data.frame(
#'     time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
#'     age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
#'   )
#'   fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
#'   fit
#'   hazard_ratio_at_time(fit, "age", times = c(10, 50, 100))
#' }
cox_time_varying <- function(data, formula, tv_vars, time_fn = c("log", "identity", "sqrt"),
                             conf_level = 0.95, ...) {
  rlang::check_installed("survival", reason = "to fit time-varying-coefficient Cox models.")
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must be a two-sided formula with a `Surv()` outcome, such as `survival::Surv(time, status) ~ age + sex`.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number between 0 and 1.", call. = FALSE)
  }
  dots <- list(...)
  if ("tt" %in% names(dots)) {
    stop("`tt` is set by `cox_time_varying()` itself; name time-varying predictors in `tv_vars`.", call. = FALSE)
  }
  if (!is.character(tv_vars) || !length(tv_vars) || anyNA(tv_vars) || any(!nzchar(tv_vars))) {
    stop("`tv_vars` must be a non-empty character vector of predictor names.", call. = FALSE)
  }
  tv_vars <- unique(tv_vars)

  resolved <- resolve_tvc_time_fn(time_fn)
  time_fn <- resolved$fn
  time_fn_label <- resolved$label

  terms_obj <- stats::terms(formula, data = data)
  term_labels <- attr(terms_obj, "term.labels")
  if (any(grepl("^tt\\(", term_labels))) {
    stop("`formula` must not already use `tt()`; name time-varying predictors in `tv_vars` instead.", call. = FALSE)
  }
  if (any(grepl("^strata\\(", term_labels))) {
    stop("`strata()` terms are not supported by `cox_time_varying()` in this draft.", call. = FALSE)
  }
  if (!length(term_labels)) {
    stop("`formula` has no predictor terms.", call. = FALSE)
  }
  not_terms <- setdiff(tv_vars, term_labels)
  if (length(not_terms)) {
    stop("`tv_vars` must name terms of `formula`; not terms: ", paste(not_terms, collapse = ", "), ".", call. = FALSE)
  }
  gone <- setdiff(tv_vars, names(data))
  if (length(gone)) {
    stop("`data` is missing column(s): ", paste(gone, collapse = ", "), call. = FALSE)
  }
  not_numeric <- tv_vars[!vapply(data[tv_vars], is.numeric, logical(1))]
  if (length(not_numeric)) {
    stop("Time-varying predictors must be numeric columns; not numeric: ", paste(not_numeric, collapse = ", "), ".", call. = FALSE)
  }

  # Outcome: validate the Surv columns before coxph sees them, so a bad
  # status or a missing time fails here with a named column.
  lhs <- formula[[2]]
  if (!is.call(lhs)) {
    stop("The left-hand side of `formula` must be a `Surv()` call.", call. = FALSE)
  }
  outcome_vars <- all.vars(lhs)
  gone_out <- setdiff(outcome_vars, names(data))
  if (length(gone_out)) {
    stop("`data` is missing outcome column(s): ", paste(gone_out, collapse = ", "), call. = FALSE)
  }
  y <- tvc_surv_outcome(data, outcome_vars)
  event_times <- unname(y[y[, "status"] == 1, "time"])
  if (!length(event_times)) {
    stop("`data` contains no events.", call. = FALSE)
  }
  f_at_events <- tryCatch(time_fn(event_times), error = function(e) e)
  if (inherits(f_at_events, "error") || !is.numeric(f_at_events) ||
      length(f_at_events) != length(event_times) || any(!is.finite(f_at_events))) {
    stop("`time_fn` must return finite values at the observed event times.", call. = FALSE)
  }

  lhs_str <- paste(deparse(lhs, width.cutoff = 500L), collapse = "")
  rhs <- c(term_labels, paste0("tt(", tv_vars, ")"))
  # The survival namespace is the formula's parent, so Surv(), strata() and
  # tt() resolve without attaching it.
  env <- new.env(parent = asNamespace("survival"))
  fml <- stats::as.formula(paste(lhs_str, "~", paste(rhs, collapse = " + ")), env = env)
  tt_fn <- function(x, t, ...) x * time_fn(t)
  fit <- survival::coxph(fml, data = data, tt = tt_fn, ties = "efron", model = FALSE, x = FALSE, y = TRUE, ...)

  # Fixed (non-tt) design matrix, aligned with the fixed coefficients by
  # name, so factors expand to the dummy columns coxph fitted.
  fixed_terms <- stats::terms(stats::reformulate(term_labels), data = data)
  needed_vars <- unique(c(all.vars(stats::reformulate(term_labels)), tv_vars))
  x_fixed <- stats::model.matrix(fixed_terms, data)
  x_fixed <- x_fixed[, setdiff(colnames(x_fixed), "(Intercept)"), drop = FALSE]
  beta_names <- names(stats::coef(fit))
  fixed_names <- beta_names[!grepl("^tt\\(", beta_names)]
  if (!all(fixed_names %in% colnames(x_fixed))) {
    stop("Internal error: the fixed design matrix does not match the fitted coefficients.", call. = FALSE)
  }
  x_fixed <- x_fixed[, fixed_names, drop = FALSE]
  x_tv <- as.matrix(data[, tv_vars, drop = FALSE])
  storage.mode(x_tv) <- "numeric"

  beta <- stats::coef(fit)
  vc <- stats::vcov(fit)
  se <- sqrt(diag(vc))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  stat <- beta / se
  tab <- tibble::tibble(
    term = names(beta),
    estimate = unname(beta),
    std_error = unname(se),
    hr = exp(unname(beta)),
    conf_low = exp(unname(beta) - z * unname(se)),
    conf_high = exp(unname(beta) + z * unname(se)),
    statistic = unname(stat),
    p_value = 2 * stats::pnorm(-abs(unname(stat))),
    time_varying = grepl("^tt\\(", names(beta))
  )

  structure(
    list(
      fit = fit,
      formula = formula,
      tv_vars = tv_vars,
      time_fn = time_fn,
      time_fn_label = time_fn_label,
      fixed_terms = fixed_terms,
      needed_vars = needed_vars,
      xlevels = attr(fixed_terms, "xlevels"),
      fixed_names = fixed_names,
      x_fixed = x_fixed,
      x_tv = x_tv,
      y = y,
      terms = tab,
      n = nrow(data),
      n_events = sum(y[, "status"] == 1),
      conf_level = conf_level
    ),
    class = "cox_time_varying"
  )
}

#' Resolve the Time Function of a Time-Varying-Coefficient Model
#'
#' @param time_fn A name (`"log"`, `"identity"`, `"sqrt"`) or a function.
#' @return A list with `fn` (the function) and `label` (its name, or
#'   `"user function"`).
#' @keywords internal
#' @noRd
resolve_tvc_time_fn <- function(time_fn) {
  if (is.function(time_fn)) {
    return(list(fn = time_fn, label = "user function"))
  }
  if (!is.character(time_fn) || length(time_fn) < 1L || is.na(time_fn[1])) {
    stop("`time_fn` must be \"log\", \"identity\", \"sqrt\", or a function.", call. = FALSE)
  }
  fn_name <- match.arg(time_fn, c("log", "identity", "sqrt"))
  fn <- switch(fn_name,
    log = log,
    identity = function(t) t,
    sqrt = sqrt
  )
  list(fn = fn, label = fn_name)
}

#' Build the Surv Outcome of a Time-Varying-Coefficient Fit
#'
#' @param data The data frame.
#' @param outcome_vars Outcome column names: `c(time, status)` or
#'   `c(start, stop, status)`.
#' @keywords internal
#' @noRd
tvc_surv_outcome <- function(data, outcome_vars) {
  if (length(outcome_vars) == 2L) {
    time <- as.numeric(data[[outcome_vars[1]]])
    status <- tvc_status_values(data[[outcome_vars[2]]], outcome_vars[2])
    if (anyNA(time) || anyNA(status)) {
      stop("The outcome columns must not contain missing values.", call. = FALSE)
    }
    survival::Surv(time, status)
  } else if (length(outcome_vars) == 3L) {
    start <- as.numeric(data[[outcome_vars[1]]])
    stop <- as.numeric(data[[outcome_vars[2]]])
    status <- tvc_status_values(data[[outcome_vars[3]]], outcome_vars[3])
    if (anyNA(start) || anyNA(stop) || anyNA(status)) {
      stop("The outcome columns must not contain missing values.", call. = FALSE)
    }
    survival::Surv(start, stop, status)
  } else {
    stop("The outcome of `formula` must be `Surv(time, status)` or `Surv(start, stop, status)`.", call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
tvc_status_values <- function(values, column) {
  st <- if (is.logical(values)) {
    as.integer(values)
  } else if (is.factor(values)) {
    as.numeric(as.character(values))
  } else {
    as.numeric(values)
  }
  if (any(!is.na(st) & !st %in% c(0, 1))) {
    stop("`", column, "` must be 0/1 (or logical).", call. = FALSE)
  }
  st
}

#' Breslow Baseline Hazard of a Time-Varying-Coefficient Fit
#'
#' `survival::basehaz()` and `survival::survfit()` refuse `coxph` fits with
#' `tt()` terms, so the baseline hazard is computed directly: at event time
#' `t_j` the increment is `d_j / sum(exp(lp_i(t_j)))` over the rows at risk,
#' with `lp_i(t)` the linear predictor at `t` (fixed part plus the
#' time-varying part evaluated at `t`).
#'
#' @param object A [cox_time_varying()] fit.
#' @return A list with `time` (distinct event times), `increment` (the
#'   Breslow increment at each) and `cumhaz` (their cumulative sum).
#' @keywords internal
#' @noRd
tvc_breslow <- function(object) {
  beta_f <- stats::coef(object$fit)[object$fixed_names]
  gamma <- stats::coef(object$fit)[paste0("tt(", object$tv_vars, ")")]
  parts <- surv_components(object$y)
  event_times <- sort(unique(parts$stop[parts$status == 1]))
  if (!length(event_times)) {
    return(list(time = numeric(0), increment = numeric(0), cumhaz = numeric(0)))
  }
  lp_fixed <- as.numeric(object$x_fixed %*% beta_f)
  deaths <- tabulate(match(parts$stop[parts$status == 1], event_times), nbins = length(event_times))
  incr <- numeric(length(event_times))
  for (j in seq_along(event_times)) {
    t <- event_times[j]
    atrisk <- parts$start < t & parts$stop >= t
    lp_t <- lp_fixed[atrisk] + as.numeric((object$x_tv[atrisk, , drop = FALSE] * object$time_fn(t)) %*% gamma)
    incr[j] <- deaths[j] / sum(exp(lp_t))
  }
  list(time = event_times, increment = incr, cumhaz = cumsum(incr))
}

#' Design Matrices of New Data for a Time-Varying-Coefficient Fit
#'
#' @param object A [cox_time_varying()] fit.
#' @param new_data A data frame of new predictors.
#' @return A list with `x_fixed` (fixed design matrix) and `x_tv` (numeric
#'   matrix of the time-varying predictors).
#' @keywords internal
#' @noRd
tvc_new_design <- function(object, new_data) {
  if (!is.data.frame(new_data) || nrow(new_data) == 0L) {
    stop("`new_data` must be a non-empty data frame.", call. = FALSE)
  }
  # The fixed design is built from terms, so check the underlying columns:
  # model.frame() names missing columns itself, but the check reads better.
  gone <- setdiff(object$needed_vars, names(new_data))
  if (length(gone)) {
    stop("`new_data` is missing column(s): ", paste(gone, collapse = ", "), call. = FALSE)
  }
  not_numeric <- object$tv_vars[!vapply(new_data[object$tv_vars], is.numeric, logical(1))]
  if (length(not_numeric)) {
    stop("Time-varying predictors must be numeric columns; not numeric: ", paste(not_numeric, collapse = ", "), ".", call. = FALSE)
  }
  mf <- stats::model.frame(object$fixed_terms, new_data, na.action = stats::na.pass, xlev = object$xlevels)
  x_fixed <- stats::model.matrix(object$fixed_terms, mf)
  x_fixed <- x_fixed[, setdiff(colnames(x_fixed), "(Intercept)"), drop = FALSE]
  if (!all(object$fixed_names %in% colnames(x_fixed))) {
    stop("`new_data` does not produce the fitted fixed predictors.", call. = FALSE)
  }
  x_fixed <- x_fixed[, object$fixed_names, drop = FALSE]
  if (anyNA(x_fixed)) {
    stop("`new_data` has missing values in the predictors; impute or drop those rows first.", call. = FALSE)
  }
  x_tv <- as.matrix(new_data[, object$tv_vars, drop = FALSE])
  storage.mode(x_tv) <- "numeric"
  if (anyNA(x_tv)) {
    stop("`new_data` has missing values in the time-varying predictors; impute or drop those rows first.", call. = FALSE)
  }
  list(x_fixed = x_fixed, x_tv = x_tv)
}

#' Linear Predictor of a Time-Varying-Coefficient Fit at a Time
#'
#' @param object A [cox_time_varying()] fit.
#' @param x_fixed,x_tv Design matrices, as from [tvc_new_design()].
#' @param t The time the linear predictor is evaluated at.
#' @return A numeric vector, one value per row: the fixed linear predictor
#'   plus the time-varying part evaluated at `t`. Larger means higher risk.
#' @keywords internal
#' @noRd
tvc_linear_predictor <- function(object, x_fixed, x_tv, t) {
  beta_f <- stats::coef(object$fit)[object$fixed_names]
  gamma <- stats::coef(object$fit)[paste0("tt(", object$tv_vars, ")")]
  as.numeric(x_fixed %*% beta_f) + as.numeric((x_tv * object$time_fn(t)) %*% gamma)
}

#' Predict From a `cox_time_varying` Model
#'
#' @param object A [cox_time_varying()] fit.
#' @param new_data A data frame of new predictors.
#' @param type `"linear_pred"` for the linear predictor at `eval_time`
#'   (larger means higher risk), or `"survival"` for survival probabilities
#'   at `eval_time`.
#' @param eval_time For `type = "linear_pred"`, the single time the linear
#'   predictor is evaluated at (required: it varies with time). For
#'   `type = "survival"`, the times to predict survival at (required).
#' @param ... Not used.
#' @return A tibble with one row per row of `new_data`: `.pred_linear_pred`,
#'   or `.pred`, a list-column of tibbles with `.eval_time` and
#'   `.pred_survival`.
#'
#' Survival is \eqn{S(t \mid x) = \exp(-\sum_{t_j \le t} \Delta H_0(t_j)
#' e^{lp(t_j)})}, with \eqn{\Delta H_0} the Breslow increments of
#' [tvc_breslow()] and \eqn{lp(t_j)} the linear predictor at the event time
#' `t_j`; the sum runs over the fitted event times, so survival is flat past
#' the last one.
#' @export
predict.cox_time_varying <- function(object, new_data, type = c("linear_pred", "survival"),
                                     eval_time = NULL, ...) {
  type <- match.arg(type)
  design <- tvc_new_design(object, new_data)
  if (type == "linear_pred") {
    if (is.null(eval_time) || length(eval_time) != 1L || !is.numeric(eval_time) ||
        is.na(eval_time) || !is.finite(eval_time) || eval_time < 0) {
      stop("`eval_time` must be a single non-negative time for `type = \"linear_pred\"`: the linear predictor varies with time.", call. = FALSE)
    }
    tibble::tibble(.pred_linear_pred = tvc_linear_predictor(object, design$x_fixed, design$x_tv, eval_time))
  } else {
    if (is.null(eval_time)) {
      stop("`eval_time` is required for `type = \"survival\"`.", call. = FALSE)
    }
    check_eval_time(eval_time, min_length = 1)
    bh <- tvc_breslow(object)
    surv <- matrix(1, nrow = nrow(new_data), ncol = length(eval_time))
    for (k in seq_along(eval_time)) {
      at <- bh$time <= eval_time[k]
      if (!any(at)) next
      # Cumulative hazard at eval_time[k]: sum over event times <= it of the
      # Breslow increment times exp(lp at that event time).
      h <- numeric(nrow(new_data))
      for (j in which(at)) {
        h <- h + bh$increment[j] * exp(tvc_linear_predictor(object, design$x_fixed, design$x_tv, bh$time[j]))
      }
      surv[, k] <- exp(-h)
    }
    tibble::tibble(.pred = lapply(seq_len(nrow(new_data)), function(i) {
      tibble::tibble(.eval_time = eval_time, .pred_survival = surv[i, ])
    }))
  }
}

#' Tidy a `cox_time_varying` Model
#'
#' @param x A [cox_time_varying()] fit.
#' @param ... Not used.
#' @return The `terms` tibble: `term`, `estimate` (log hazard ratio), `std_error`,
#'   `hr`, `conf_low`, `conf_high`, `statistic`, `p_value`, and `time_varying`
#'   (`TRUE` for the `tt()` rows). For a `tt()` row the hazard ratio is the
#'   multiplicative change in the hazard ratio per unit of the time function,
#'   not a hazard ratio at a fixed time; use [hazard_ratio_at_time()] for that.
#' @exportS3Method generics::tidy
tidy.cox_time_varying <- function(x, ...) {
  x$terms
}

#' @rdname templecbe_methods
#' @export
print.cox_time_varying <- function(x, ...) {
  cat("Time-varying-coefficient Cox model (time function: ", x$time_fn_label, ")\n", sep = "")
  cat(sprintf("  %d row(s), %d event(s); time-varying: %s\n",
              x$n, x$n_events, paste(x$tv_vars, collapse = ", ")))
  tab <- x$terms
  show <- data.frame(
    term = tab$term, HR = round(tab$hr, 3),
    CI = sprintf("%.3f to %.3f", tab$conf_low, tab$conf_high),
    p = signif(tab$p_value, 3),
    time_varying = tab$time_varying
  )
  print(show, row.names = FALSE)
  cat("  Hazard ratios of time-varying terms are at f(t) = 0; see hazard_ratio_at_time().\n")
  invisible(x)
}

#' Hazard Ratio of a Time-Varying Coefficient at Given Times
#'
#' For a time-varying predictor `x` with log hazard ratio
#' \eqn{\beta + \gamma f(t)}, returns \eqn{\exp(\beta + \gamma f(t))} at each
#' time in `times`, with confidence intervals from the delta method (the
#' covariance of the two coefficients comes from the fitted model).
#'
#' @param object A [cox_time_varying()] fit.
#' @param variable One time-varying predictor name (an element of
#'   `object$tv_vars`).
#' @param times Numeric vector of times to report the hazard ratio at.
#' @return A tibble with `variable`, `time`, `hazard_ratio`, `conf_low` and
#'   `conf_high`.
#' @seealso [cox_time_varying()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(1)
#'   n <- 200
#'   d <- data.frame(
#'     time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
#'     age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
#'   )
#'   fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
#'   hazard_ratio_at_time(fit, "age", times = c(10, 50, 100))
#' }
hazard_ratio_at_time <- function(object, variable, times) {
  if (!inherits(object, "cox_time_varying")) {
    stop("`object` must be a `cox_time_varying()` fit.", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1L || is.na(variable) ||
      !variable %in% object$tv_vars) {
    stop("`variable` must be one time-varying predictor of `object`: ", paste(object$tv_vars, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(times) || !length(times) || anyNA(times) || any(!is.finite(times)) || any(times < 0)) {
    stop("`times` must be finite, non-negative numbers.", call. = FALSE)
  }
  beta <- stats::coef(object$fit)[variable]
  gamma <- stats::coef(object$fit)[paste0("tt(", variable, ")")]
  vc <- stats::vcov(object$fit)[c(variable, paste0("tt(", variable, ")")), c(variable, paste0("tt(", variable, ")")), drop = FALSE]
  z <- stats::qnorm(1 - (1 - object$conf_level) / 2)
  f <- object$time_fn(times)
  log_hr <- beta + gamma * f
  var_log_hr <- vc[1, 1] + f^2 * vc[2, 2] + 2 * f * vc[1, 2]
  tibble::tibble(
    variable = variable,
    time = times,
    hazard_ratio = exp(log_hr),
    conf_low = exp(log_hr - z * sqrt(pmax(var_log_hr, 0))),
    conf_high = exp(log_hr + z * sqrt(pmax(var_log_hr, 0)))
  )
}
