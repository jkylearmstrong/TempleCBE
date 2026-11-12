#' Event Risk at One Horizon from Survival Predictions
#'
#' Adapters from the survival-probability output of `predict()` on a
#' [coxnet()], [cv_coxnet()] or [joint_model()] fit to the predicted-risk
#' inputs of [calibration_table()] and [decision_curve()], so a fitted model
#' can be validated without hand-extracting one horizon's probabilities.
#' `predict()` returns survival probabilities; calibration and decision curves
#' take event probabilities, so the risk at a horizon is one minus the
#' survival probability at that horizon.
#'
#' @param pred A tibble from `predict(..., type = "survival")`: one row per
#'   subject, with a `.pred` list-column (from [coxnet()] / [cv_coxnet()]) or a
#'   `.pred_survival` list-column (from [joint_model()]) whose elements are
#'   tibbles with `.eval_time` and `.pred_survival`.
#' @param horizon A single time: the horizon the risk applies to. It must be
#'   one of the `.eval_time` values in `pred`; otherwise an error lists the
#'   available times.
#' @return `extract_horizon_risk()`: a numeric vector of event probabilities
#'   by `horizon`, one per row of `pred`.
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
  if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) || horizon < 0) {
    stop("`horizon` must be a single non-negative time.", call. = FALSE)
  }
  col <- if (".pred" %in% names(pred)) ".pred" else if (".pred_survival" %in% names(pred)) ".pred_survival" else NA_character_
  if (is.na(col)) {
    stop("`pred` has neither a `.pred` nor a `.pred_survival` list-column: ",
         "pass the output of `predict(..., type = \"survival\")`.", call. = FALSE)
  }
  curves <- pred[[col]]
  if (!is.list(curves) || length(curves) != nrow(pred)) {
    stop("`pred$`", col, " must be a list-column with one survival curve per row.", call. = FALSE)
  }
  bad <- vapply(curves, function(cv) {
    !is.data.frame(cv) || !all(c(".eval_time", ".pred_survival") %in% names(cv))
  }, logical(1))
  if (any(bad)) {
    stop("Each survival curve in `pred` must be a tibble with `.eval_time` and `.pred_survival`.", call. = FALSE)
  }
  times <- curves[[1L]]$.eval_time
  if (!all(vapply(curves, function(cv) identical(cv$.eval_time, times), logical(1)))) {
    stop("All survival curves in `pred` must share the same `.eval_time` values.", call. = FALSE)
  }
  tol <- 1e-8 * max(1, abs(horizon))
  at <- which(abs(times - horizon) <= tol)
  if (length(at) == 0L) {
    nearest <- times[which.min(abs(times - horizon))]
    stop("`horizon` (", horizon, ") is not one of the predicted `.eval_time` values (",
         paste(times, collapse = ", "), "); nearest is ", nearest, ".", call. = FALSE)
  }
  surv <- vapply(curves, function(cv) cv$.pred_survival[at[1L]], numeric(1))
  if (any(!is.finite(surv))) stop("Predicted survival probabilities must be finite.", call. = FALSE)
  if (any(surv < -1e-6 | surv > 1 + 1e-6)) {
    stop("Predicted survival probabilities must be between 0 and 1.", call. = FALSE)
  }
  pmin(pmax(1 - surv, 0), 1)
}

#' Predicted Event Risk at One Horizon from a Fitted Survival Model
#'
#' Predicts the event probability by `horizon` (one minus the survival
#' probability) for new data under a fitted [coxnet()], [cv_coxnet()] or
#' [joint_model()] model. This is the model-level adapter: for the raw
#' `predict()` output, use [extract_horizon_risk()].
#'
#' (A plain function dispatching on the model class, not an S3 generic, so it
#' works as written: the S3 split is a follow-up for the roxygen run that
#' regenerates `NAMESPACE`.)
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet` or `joint_model` object.
#' @param new_data A data frame of new predictors, as for `predict()`.
#' @param horizon A single time: the horizon the risk applies to.
#' @param penalty For `coxnet_model`: the penalty to predict at; defaults to
#'   the model's `penalty`. For `cv_coxnet`: `"lambda.min"` (default),
#'   `"lambda.1se"`, or a number. Ignored for `joint_model`.
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
  if (inherits(object, "coxnet_model")) {
    pred <- stats::predict(object, new_data, type = "survival", eval_time = horizon, penalty = penalty)
  } else if (inherits(object, "cv_coxnet")) {
    if (is.null(penalty)) penalty <- "lambda.min"
    pred <- stats::predict(object, new_data, type = "survival", eval_time = horizon, penalty = penalty)
  } else if (inherits(object, "joint_model")) {
    pred <- stats::predict(object, new_data = new_data, eval_time = horizon, type = "survival")
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
  if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) || horizon <= 0) {
    stop("`horizon` must be a single positive time.", call. = FALSE)
  }
  if (!is.numeric(new_data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  truth <- survival::Surv(new_data[[time_col]], new_data[[event_col]])
  risk <- if (is.null(penalty)) {
    predict_event_risk(object, new_data, horizon)
  } else {
    predict_event_risk(object, new_data, horizon, penalty = penalty)
  }
  list(truth = truth, risk = risk)
}

#' Calibration Table of a Fitted Survival Model
#'
#' Validates a fitted [coxnet()], [cv_coxnet()] or [joint_model()] model at
#' one horizon: the event risk is predicted with [predict_event_risk()] and
#' passed, with the right-censored outcome from `new_data`, to
#' [calibration_table()]. See there for the bins, the observed risks and the
#' metrics; competing events are treated as censoring, as in
#' [calibration_table()].
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet` or `joint_model` object.
#' @param new_data A data frame with the predictors and the `time_col` /
#'   `event_col` outcome columns.
#' @param time_col,event_col Outcome columns in `new_data`: follow-up time
#'   and the 0/1 event indicator.
#' @param horizon A single positive time: the horizon the calibration applies
#'   to.
#' @param penalty Passed to [predict_event_risk()]; ignored for `joint_model`.
#' @param ... Passed to [calibration_table()] (for example `bins` or
#'   `conf_level`).
#' @return As [calibration_table()]: a tibble with one row per model and bin,
#'   with `attr(x, "metrics")` and `attr(x, "horizon")`.
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
#' Validates a fitted [coxnet()], [cv_coxnet()] or [joint_model()] model at
#' one horizon: the event risk is predicted with [predict_event_risk()] and
#' passed, with the right-censored outcome from `new_data`, to
#' [decision_curve()]. See there for the net-benefit definition; competing
#' events are treated as censoring, as in [decision_curve()].
#'
#' @param object A fitted `coxnet_model`, `cv_coxnet` or `joint_model` object.
#' @param new_data A data frame with the predictors and the `time_col` /
#'   `event_col` outcome columns.
#' @param time_col,event_col Outcome columns in `new_data`: follow-up time
#'   and the 0/1 event indicator.
#' @param horizon A single positive time: the horizon the decision curve
#'   applies to.
#' @param penalty Passed to [predict_event_risk()]; ignored for `joint_model`.
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
