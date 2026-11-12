#' Event Risk at One Horizon from Survival Predictions
#'
#' Adapters from the survival-probability output of `predict()` on a
#' [coxnet()], [cv_coxnet()] or [joint_model()] fit — or from the
#' cumulative-incidence output of `predict()` on a [fine_gray()] fit — to the
#' predicted-risk inputs of [calibration_table()] and [decision_curve()], so a
#' fitted model can be validated without hand-extracting one horizon's
#' probabilities. `predict()` returns survival probabilities; calibration and
#' decision curves take event probabilities, so the risk at a horizon is one
#' minus the survival probability at that horizon. For a Fine-Gray fit the
#' risk is the cumulative incidence (CIF) at the horizon itself: the
#' `.pred_cif` column of the predict output is returned as-is, not subtracted
#' from one.
#'
#' @param pred A tibble from `predict(..., type = "survival")`: one row per
#'   subject, with a `.pred` list-column (from [coxnet()] / [cv_coxnet()] /
#'   [cox_time_varying()]) or a `.pred_survival` list-column (from
#'   [joint_model()]) whose elements are tibbles with `.eval_time` and
#'   `.pred_survival`; or a tibble from `predict()` on a [fine_gray()] fit,
#'   with a `.pred` list-column whose elements are tibbles with `.eval_time`
#'   and `.pred_cif`.
#' @param horizon A single time: the horizon the risk applies to. It must be
#'   one of the `.eval_time` values in `pred`; otherwise an error lists the
#'   available times.
#' @return `extract_horizon_risk()`: a numeric vector of event probabilities
#'   by `horizon`, one per row of `pred` — one minus survival for survival
#'   predictions, the CIF itself for Fine-Gray predictions. A row whose
#'   predicted survival is `NA` returns `NA` risk, so the result always
#'   aligns with `pred`.
#' @seealso [predict_event_risk()], [calibration_from_model()],
#'   [decision_curve_from_model()], [calibration_table()], [decision_curve()]
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
#'                 age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
#' fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
#' pr <- predict(fit, d[1:5, ], type = "survival", eval_time = c(5, 10, 20))
#' extract_horizon_risk(pr, 10)
extract_horizon_risk <- function(pred, horizon) {
  if (!is.data.frame(pred)) {
    stop("`pred` must be a tibble from `predict(..., type = \"survival\")`.", call. = FALSE)
  }
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) || horizon < 0) {
    stop("`horizon` must be a single finite, non-negative time.", call. = FALSE)
  }
  pred_col <- if (".pred" %in% names(pred)) {
    ".pred"
  } else if (".pred_survival" %in% names(pred)) {
    ".pred_survival"
  } else if (".pred_cif" %in% names(pred)) {
    ".pred_cif"
  } else {
    NA_character_
  }
  if (is.na(pred_col)) {
    stop("`pred` has neither a `.pred`, a `.pred_survival` nor a `.pred_cif` list-column: ",
         "pass the output of `predict(..., type = \"survival\")` (or `predict()` on a `fine_gray()` fit).", call. = FALSE)
  }
  curves <- pred[[pred_col]]
  if (!is.list(curves) || length(curves) != nrow(pred)) {
    stop("`pred$`", pred_col, " must be a list-column with one curve per row.", call. = FALSE)
  }
  if (nrow(pred) == 0L) {
    stop("`pred` has no rows: pass the output of `predict(..., type = \"survival\")` for at least one subject.", call. = FALSE)
  }
  # A curve carries either survival probabilities (`.pred_survival`, risk is
  # one minus survival) or a Fine-Gray cumulative incidence (`.pred_cif`,
  # risk is the CIF itself). All curves must carry the same one.
  curve_value_col <- function(cv) {
    if (".pred_survival" %in% names(cv)) ".pred_survival" else if (".pred_cif" %in% names(cv)) ".pred_cif" else NA_character_
  }
  value_cols <- vapply(curves, curve_value_col, character(1))
  if (anyNA(value_cols)) {
    stop("Each curve in `pred` must be a tibble with `.eval_time` and `.pred_survival` (survival predictions) ",
         "or `.eval_time` and `.pred_cif` (Fine-Gray predictions).", call. = FALSE)
  }
  if (length(unique(value_cols)) != 1L) {
    stop("All curves in `pred` must carry the same value column (`.pred_survival` or `.pred_cif`).", call. = FALSE)
  }
  value_col <- value_cols[1L]
  is_cif <- identical(value_col, ".pred_cif")
  bad <- vapply(curves, function(cv) {
    !is.data.frame(cv) || !all(c(".eval_time", value_col) %in% names(cv))
  }, logical(1))
  if (any(bad)) {
    stop("Each curve in `pred` must be a tibble with `.eval_time` and `", value_col, "`.", call. = FALSE)
  }
  times <- curves[[1L]]$.eval_time
  if (!all(vapply(curves, function(cv) identical(cv$.eval_time, times), logical(1)))) {
    stop("All curves in `pred` must share the same `.eval_time` values.", call. = FALSE)
  }
  tol <- 1e-8 * max(1, abs(horizon))
  at <- which(abs(times - horizon) <= tol)
  if (length(at) == 0L) {
    nearest <- times[which.min(abs(times - horizon))]
    stop("`horizon` (", horizon, ") is not one of the predicted `.eval_time` values (",
         paste(times, collapse = ", "), "); nearest is ", nearest, ".", call. = FALSE)
  }
  vals <- vapply(curves, function(cv) cv[[value_col]][at[1L]], numeric(1))
  # A row whose prediction is NA (its predictors were missing, so predict()
  # could not score it) returns NA risk rather than failing the whole call;
  # a non-NA prediction that is non-finite or out of range still errors, and
  # names the rows, so a genuinely broken prediction is not hidden.
  na_vals <- is.na(vals)
  bad_finite <- which(!na_vals & !is.finite(vals))
  if (length(bad_finite) > 0L) {
    stop(if (is_cif) "Predicted cumulative incidences must be finite (rows: " else "Predicted survival probabilities must be finite (rows: ",
         paste(bad_finite, collapse = ", "), ").", call. = FALSE)
  }
  bad_range <- which(!na_vals & (vals < -1e-6 | vals > 1 + 1e-6))
  if (length(bad_range) > 0L) {
    stop(if (is_cif) "Predicted cumulative incidences must be between 0 and 1 (rows: " else "Predicted survival probabilities must be between 0 and 1 (rows: ",
         paste(bad_range, collapse = ", "), ").", call. = FALSE)
  }
  risk <- rep(NA_real_, length(vals))
  risk[!na_vals] <- pmin(pmax(if (is_cif) vals[!na_vals] else 1 - vals[!na_vals], 0), 1)
  risk
}

#' Predicted Event Risk at One Horizon from a Fitted Survival Model
#'
#' Predicts the event probability by `horizon` (one minus the survival
#' probability) for new data under a fitted [coxnet()], [cv_coxnet()],
#' [joint_model()], [cox_time_varying()] or [fine_gray()] model. This is the
#' model-level adapter: for the raw `predict()` output, use
#' [extract_horizon_risk()]. For a [fine_gray()] fit the risk is the
#' cumulative incidence (CIF) of the cause of interest at `horizon`, not one
#' minus a survival probability.
#'
#' (A plain function dispatching on the model class, not an S3 generic, so it
#' works as written: the S3 split is a follow-up for the roxygen run that
#' regenerates `NAMESPACE`.)
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet`, `joint_model`,
#'   `cox_time_varying` or `fine_gray_model` object.
#' @param new_data A data frame of new predictors, as for `predict()`.
#' @param horizon A single time: the horizon the risk applies to.
#' @param penalty For `coxnet_model`: the penalty to predict at; defaults to
#'   the model's `penalty`. For `cv_coxnet`: `"lambda.min"` (default),
#'   `"lambda.1se"`, or a number. Ignored for `joint_model`,
#'   `cox_time_varying` and `fine_gray_model`.
#' @param ... Not used.
#' @return A numeric vector of event probabilities by `horizon`, one per row
#'   of `new_data`.
#' @seealso [extract_horizon_risk()], [calibration_from_model()],
#'   [decision_curve_from_model()]
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
#'                 age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
#' fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
#' predict_event_risk(fit, d[1:5, ], horizon = 10)
predict_event_risk <- function(object, new_data, horizon, penalty = NULL, ...) {
  # Validated up front: the branches below that return early (rows with a
  # missing predictor) would otherwise never reach a downstream check.
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) || horizon < 0) {
    stop("`horizon` must be a single finite, non-negative time.", call. = FALSE)
  }
  if (inherits(object, "coxnet_model")) {
    pred <- stats::predict(object, new_data, type = "survival", eval_time = horizon, penalty = penalty)
  } else if (inherits(object, "cv_coxnet")) {
    if (is.null(penalty)) penalty <- "lambda.min"
    pred <- stats::predict(object, new_data, type = "survival", eval_time = horizon, penalty = penalty)
  } else if (inherits(object, "joint_model")) {
    pred <- stats::predict(object, new_data, eval_time = horizon, type = "survival")
  } else if (inherits(object, "cox_time_varying")) {
    # predict.cox_time_varying() stops on any missing predictor. As for a
    # Fine-Gray fit, a row it cannot score gets NA risk (and is dropped by
    # the validation) instead of aborting the whole call.
    vars <- intersect(union(object$needed_vars, object$tv_vars), names(new_data))
    ok <- stats::complete.cases(new_data[, vars, drop = FALSE])
    if (!all(ok)) {
      risk <- rep(NA_real_, nrow(new_data))
      if (any(ok)) {
        part <- stats::predict(object, new_data[ok, , drop = FALSE], type = "survival", eval_time = horizon)
        risk[ok] <- extract_horizon_risk(part, horizon)
      }
      return(risk)
    }
    pred <- stats::predict(object, new_data, type = "survival", eval_time = horizon)
  } else if (inherits(object, "fine_gray_model")) {
    # The CIF is a step function, so any horizon is a valid prediction time;
    # extract_horizon_risk() returns the CIF itself for `.pred_cif` curves.
    pred <- stats::predict(object, new_data, times = horizon)
  } else {
    stop("No `predict_event_risk()` method for class ", paste(class(object), collapse = ", "), ".", call. = FALSE)
  }
  extract_horizon_risk(pred, horizon)
}

# The Surv outcome and the event-risk vector for a model's validation, from
# the time/event columns of `new_data`.
#' @noRd
.model_validation_inputs <- function(object, new_data, time_col, event_col, horizon, penalty) {
  if (!is.data.frame(new_data)) stop("`new_data` must be a data frame.", call. = FALSE)
  for (v in c(time_col, event_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(new_data)) {
      stop("`", v, "` is not a column of `new_data`.", call. = FALSE)
    }
  }
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) || horizon <= 0) {
    stop("`horizon` must be a single positive time.", call. = FALSE)
  }
  if (!is.numeric(new_data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  event <- new_data[[event_col]]
  if (inherits(object, "fine_gray_model")) {
    # The Fine-Gray outcome codes several causes (and censoring) in one
    # column; the validation is for the cause of interest only, so competing
    # events are coded 0 here and treated as censoring by
    # calibration_table() / decision_curve(), as their help states.
    event <- as.integer(as.character(event) == as.character(object$failcode))
  }
  truth <- survival::Surv(new_data[[time_col]], event)
  risk <- if (is.null(penalty)) {
    predict_event_risk(object, new_data, horizon)
  } else {
    predict_event_risk(object, new_data, horizon, penalty = penalty)
  }
  list(truth = truth, risk = risk)
}

#' Calibration Table of a Fitted Survival Model
#'
#' Validates a fitted [coxnet()], [cv_coxnet()], [joint_model()],
#' [cox_time_varying()] or [fine_gray()] model at one horizon: the event risk
#' is predicted with [predict_event_risk()] and passed, with the
#' right-censored outcome from `new_data`, to [calibration_table()]. See there
#' for the bins, the observed risks and the metrics; competing events are
#' treated as censoring, as in [calibration_table()]. For a [fine_gray()] fit
#' the risk is the cumulative incidence of the cause of interest and the
#' outcome is dichotomised to that cause (`event_col == failcode`); other
#' causes count as censoring.
#'
#' @details
#' **No summary metrics for Surv outcomes.** `attr(x, "metrics")` is always
#' `NA` for `intercept`, `slope` and `brier` here: [calibration_table()]
#' computes those only for a binary outcome, and no IPCW-based summary
#' metrics for right-censored outcomes are implemented. The bins table itself
#' (mean predicted vs Kaplan-Meier observed risk per bin) is the calibration
#' assessment. To estimate a calibration slope yourself, dichotomise the
#' outcome at the horizon — 1 for an event by `horizon`, 0 for follow-up past
#' `horizon` (subjects censored before `horizon` have unknown status and are
#' dropped) — and regress that indicator on the logit of the predicted risk,
#' for example `glm(y_horizon ~ qlogis(risk), family = binomial())`: the
#' coefficient of `qlogis(risk)` is the slope, and the intercept of
#' `glm(y_horizon ~ 1 + offset(qlogis(risk)), family = binomial())` is
#' calibration-in-the-large.
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet`, `joint_model`,
#'   `cox_time_varying` or `fine_gray_model` object.
#' @param new_data A data frame with the predictors and the `time_col` /
#'   `event_col` outcome columns. For a `fine_gray_model` fit, `event_col`
#'   holds the competing-risk event codes (censoring code, then one code per
#'   cause), as in [fine_gray()].
#' @param time_col,event_col Outcome columns in `new_data`: follow-up time
#'   and the 0/1 event indicator (or, for a `fine_gray_model` fit, the event
#'   codes).
#' @param horizon A single positive time: the horizon the calibration applies
#'   to.
#' @param penalty Passed to [predict_event_risk()]; ignored for `joint_model`,
#'   `cox_time_varying` and `fine_gray_model`.
#' @param ... Passed to [calibration_table()] (for example `bins` or
#'   `conf_level`).
#' @return As [calibration_table()]: a tibble with one row per model and bin,
#'   with `attr(x, "metrics")` and `attr(x, "horizon")`. For Surv outcomes
#'   the metrics are `NA` (see Details).
#' @seealso [decision_curve_from_model()], [predict_event_risk()],
#'   [calibration_table()]
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
#'                 age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
#' fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
#' calibration_from_model(fit, d, "time", "status", horizon = 10, bins = 5)
calibration_from_model <- function(object, new_data, time_col, event_col, horizon,
                                   penalty = NULL, ...) {
  inp <- .model_validation_inputs(object, new_data, time_col, event_col, horizon, penalty)
  calibration_table(inp$truth, inp$risk, horizon = horizon, ...)
}

#' Decision Curve of a Fitted Survival Model
#'
#' Validates a fitted [coxnet()], [cv_coxnet()], [joint_model()],
#' [cox_time_varying()] or [fine_gray()] model at one horizon: the event risk
#' is predicted with [predict_event_risk()] and passed, with the
#' right-censored outcome from `new_data`, to [decision_curve()]. See there
#' for the net-benefit definition; competing events are treated as censoring,
#' as in [decision_curve()]. For a [fine_gray()] fit the risk is the
#' cumulative incidence of the cause of interest and the outcome is
#' dichotomised to that cause (`event_col == failcode`); other causes count
#' as censoring.
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet`, `joint_model`,
#'   `cox_time_varying` or `fine_gray_model` object.
#' @param new_data A data frame with the predictors and the `time_col` /
#'   `event_col` outcome columns. For a `fine_gray_model` fit, `event_col`
#'   holds the competing-risk event codes, as in [fine_gray()].
#' @param time_col,event_col Outcome columns in `new_data`: follow-up time
#'   and the 0/1 event indicator (or, for a `fine_gray_model` fit, the event
#'   codes).
#' @param horizon A single positive time: the horizon the decision curve
#'   applies to.
#' @param penalty Passed to [predict_event_risk()]; ignored for `joint_model`,
#'   `cox_time_varying` and `fine_gray_model`.
#' @param ... Passed to [decision_curve()] (for example `thresholds`).
#' @return As [decision_curve()]: a tibble of class `decision_curve` with one
#'   row per threshold.
#' @seealso [calibration_from_model()], [predict_event_risk()],
#'   [decision_curve()]
#' @export
#' @examples
#' set.seed(1)
#' n <- 120
#' d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
#'                 age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
#' fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
#' head(decision_curve_from_model(fit, d, "time", "status", horizon = 10))
decision_curve_from_model <- function(object, new_data, time_col, event_col, horizon,
                                      penalty = NULL, ...) {
  inp <- .model_validation_inputs(object, new_data, time_col, event_col, horizon, penalty)
  decision_curve(inp$truth, inp$risk, horizon = horizon, ...)
}
