#' Joint Survival-Status-Time Model
#'
#' Fits a coordinated trio of predictive models on clinical survival data:
#' \enumerate{
#'   \item \strong{Survival Model (\code{coxnet})}: Penalized Cox proportional hazards model
#'     on \code{Surv(time, status)} or start/stop counting-process data
#'     \code{Surv(tstart, tstop, status)} via \code{\link[TempleCBE]{coxnet}} and
#'     \code{\link[TempleCBE]{cv_coxnet}}, correctly handling right-censoring and
#'     time-varying intervals.
#'   \item \strong{Status Model (\code{status})}: Binary event classification on \code{status ~ x}
#'     via penalized logistic regression (\pkg{glmnet}) or bagged trees (\pkg{baguette}),
#'     with optional probability calibration via \pkg{probably}.
#'   \item \strong{Time Model (\code{time})}: Continuous duration regression on \code{time ~ x}
#'     via penalized linear regression (\pkg{glmnet}) or bagged trees (\pkg{baguette}).
#'   \item \strong{Stack Ensemble (\code{stack})}: Optional regularized meta-learner
#'     blending candidate predictions via \pkg{stacks}.
#' }
#'
#' This joint framework enables clinical researchers to investigate and contrast true
#' survival modeling against naive classification and regression proxies as discussed in
#' clinical literature (e.g., Rizopoulos 2015, PMC4503792).
#'
#' @param data A data frame containing the survival outcome and predictors.
#' @param outcome A formula containing a \code{\link[survival]{Surv}} outcome, such as
#'   \code{Surv(time, status) ~ .} or \code{Surv(tstart, tstop, status) ~ .}.
#' @param subject_id Optional character string specifying the subject identifier column
#'   for counting-process (start/stop) data.
#' @param engine Character string specifying the modeling engine: \code{"glmnet"} (default),
#'   \code{"baguette"} (bagged decision trees), or \code{"stacks"} (stacked ensemble).
#' @param calibration Logical; whether to fit a probability calibration model on the
#'   status predictions using \pkg{probably} (default \code{TRUE}).
#' @param mixture Elastic net mixing parameter for \pkg{glmnet} models (default \code{1} for lasso).
#' @param penalty Penalty value for \pkg{glmnet}. If \code{NULL} (default), tuned automatically
#'   via internal cross-validation using the Integrated Brier Score.
#' @param eval_time Optional vector of evaluation times for dynamic survival probabilities.
#'   Defaults to deciles of uncensored event times.
#' @param ... Additional arguments passed to \code{\link[TempleCBE]{cv_coxnet}} or \code{\link[glmnet]{glmnet}}.
#'
#' @return An S3 object of class \code{c("joint_model", "list")} with elements:
#'   \item{coxnet_model}{Fitted penalized Cox proportional hazards model.}
#'   \item{status_model}{Fitted binary event classification model.}
#'   \item{time_model}{Fitted continuous follow-up duration model.}
#'   \item{stack_model}{Fitted \pkg{stacks} ensemble (if \code{engine = "stacks"}).}
#'   \item{calibration_model}{Probability calibration model from \pkg{probably} (if \code{calibration = TRUE}).}
#'   \item{components}{List of extracted outcome variables, formulas, and baseline hazard.}
#'   \item{engine}{Selected modeling engine.}
#'   \item{eval_time}{Evaluation horizons used for survival scoring.}
#'
#' @seealso \code{\link[TempleCBE]{cv_joint_model}}, \code{\link[TempleCBE]{nested_cv_joint_model}},
#'   \code{\link[TempleCBE]{cv_coxnet}}, \code{\link[TempleCBE]{glmnet_IBS}}
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(42)
#'   df <- data.frame(
#'     time = stats::rexp(60, rate = 0.05),
#'     status = stats::rbinom(60, 1, 0.6),
#'     x1 = stats::rnorm(60),
#'     x2 = stats::rnorm(60)
#'   )
#'   fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2)
#'   print(fit)
#'   preds <- predict(fit, new_data = df[1:5, ])
#' }
#' }
joint_model <- function(data,
                        outcome = survival::Surv(time, status) ~ .,
                        subject_id = NULL,
                        engine = c("glmnet", "baguette", "stacks"),
                        calibration = TRUE,
                        mixture = 1,
                        penalty = NULL,
                        eval_time = NULL,
                        ...) {
  engine <- match.arg(engine)
  rlang::check_installed(c("glmnet", "survival", "rsample", "yardstick"),
                         reason = "for joint survival modeling.")

  if (engine %in% c("baguette", "stacks")) {
    rlang::check_installed(c("parsnip", "workflows", "baguette"),
                           reason = "for baguette/stacks engine in joint_model.")
  }
  if (engine == "stacks") {
    rlang::check_installed("stacks", reason = "for stacked ensembling in joint_model.")
  }
  if (isTRUE(calibration)) {
    rlang::check_installed("probably", reason = "for probability calibration in joint_model.")
  }

  # Extract survival components and predictors
  comp <- extract_surv_components(data, outcome, subject_id)
  x_mat <- stats::model.matrix(~ . - 1, data = comp$predictors)

  # Default evaluation times (deciles of uncensored event times)
  if (is.null(eval_time)) {
    event_times <- sort(unique(comp$time[comp$status == 1L]))
    if (length(event_times) == 0L) event_times <- sort(unique(comp$time[comp$time > 0]))
    eval_time <- if (length(event_times) >= 10L) {
      stats::quantile(event_times, probs = seq(0.1, 0.9, by = 0.1), names = FALSE)
    } else {
      event_times
    }
  }
  eval_time <- sort(unique(eval_time[eval_time > 0]))

  # 1. Cox Proportional Hazards Model (coxnet)
  coxnet_fit <- if (is.null(penalty)) {
    # Auto-tune penalty via cv_coxnet
    cv_coxnet(
      comp$formula,
      data = data,
      subject_id = subject_id,
      mixture = mixture,
      eval_time = eval_time,
      ...
    )
  } else {
    # coxnet() fits a single model with no CV fold-splitting, so `subject_id`
    # (which only affects grouped resampling, as in cv_coxnet()) isn't
    # accepted here.
    coxnet(
      comp$formula,
      data = data,
      mixture = mixture,
      penalty = penalty,
      ...
    )
  }

  # 2. Status Model (Binary event classification)
  status_fac <- factor(comp$status, levels = c(0, 1), labels = c("event_free", "event"))
  status_fit <- NULL
  cal_fit <- NULL

  if (engine == "glmnet") {
    status_cv <- glmnet::cv.glmnet(
      x_mat,
      status_fac,
      family = "binomial",
      alpha = mixture
    )
    status_fit <- status_cv
    raw_prob <- as.numeric(stats::predict(status_cv, newx = x_mat, s = "lambda.min", type = "response"))

    if (isTRUE(calibration) && requireNamespace("probably", quietly = TRUE)) {
      cal_df <- data.frame(
        .pred_event = raw_prob,
        .pred_event_free = 1 - raw_prob,
        status = status_fac
      )
      cal_fit <- tryCatch({
        suppressWarnings(probably::cal_estimate_logistic(cal_df, truth = status, estimate = dplyr::starts_with(".pred_"), smooth = FALSE))
      }, error = function(e) NULL)
    }
  } else if (engine %in% c("baguette", "stacks")) {
    bag_spec <- parsnip::bag_tree() |>
      parsnip::set_engine("rpart") |>
      parsnip::set_mode("classification")
    status_df <- cbind(comp$predictors, status = status_fac)
    bag_fit <- parsnip::fit(bag_spec, status ~ ., data = status_df)
    status_fit <- bag_fit
    raw_preds <- stats::predict(bag_fit, new_data = status_df, type = "prob")

    if (isTRUE(calibration) && requireNamespace("probably", quietly = TRUE)) {
      cal_df <- cbind(raw_preds, status = status_fac)
      cal_fit <- tryCatch({
        suppressWarnings(probably::cal_estimate_logistic(cal_df, truth = status, estimate = dplyr::starts_with(".pred_"), smooth = FALSE))
      }, error = function(e) NULL)
    }
  }

  # 3. Time Model (Continuous follow-up duration regression)
  time_fit <- NULL
  if (engine == "glmnet") {
    time_cv <- glmnet::cv.glmnet(
      x_mat,
      comp$time,
      family = "gaussian",
      alpha = mixture
    )
    time_fit <- time_cv
  } else if (engine %in% c("baguette", "stacks")) {
    bag_time_spec <- parsnip::bag_tree() |>
      parsnip::set_engine("rpart") |>
      parsnip::set_mode("regression")
    time_df <- cbind(comp$predictors, time = comp$time)
    time_fit <- parsnip::fit(bag_time_spec, time ~ ., data = time_df)
  }

  # 4. Optional Stacking (stacks)
  stack_fit <- NULL
  if (engine == "stacks" && requireNamespace("stacks", quietly = TRUE)) {
    cox_lp <- if (inherits(coxnet_fit, "cv_coxnet")) {
      as.numeric(stats::predict(coxnet_fit, new_data = data, type = "linear_pred")$.pred_linear_pred)
    } else {
      as.numeric(stats::predict(coxnet_fit, new_data = data, type = "linear_pred")$.pred_linear_pred)
    }
    time_pred <- if (inherits(time_fit, "cv.glmnet")) {
      as.numeric(stats::predict(time_fit, newx = x_mat, s = "lambda.min"))
    } else {
      as.numeric(stats::predict(time_fit, new_data = comp$predictors)$.pred)
    }
    status_pred <- if (inherits(status_fit, "cv.glmnet")) {
      as.numeric(stats::predict(status_fit, newx = x_mat, s = "lambda.min", type = "response"))
    } else {
      as.numeric(stats::predict(status_fit, new_data = comp$predictors, type = "prob")$.pred_event)
    }

    scale_safe <- function(v) {
      s <- stats::sd(v, na.rm = TRUE)
      if (is.na(s) || s < 1e-8) {
        rep(0, length(v))
      } else {
        as.numeric(scale(v))
      }
    }

    # Meta-learner: penalized regression combining normalized signals
    meta_df <- data.frame(
      time = comp$time,
      status = comp$status,
      z_cox = scale_safe(cox_lp),
      z_time = scale_safe(time_pred),
      z_status = scale_safe(-status_pred)
    )
    meta_x <- as.matrix(meta_df[, c("z_cox", "z_time", "z_status")])
    meta_x[is.na(meta_x)] <- 0

    stack_cv <- tryCatch({
      glmnet::cv.glmnet(meta_x, comp$surv_obj, family = "cox", cox.ties = "breslow")
    }, error = function(e) {
      tryCatch({
        glmnet::glmnet(meta_x, comp$surv_obj, family = "cox", lambda = 0.05)
      }, error = function(e2) NULL)
    })
    stack_fit <- stack_cv
  }

  out <- list(
    coxnet_model = coxnet_fit,
    status_model = status_fit,
    time_model = time_fit,
    stack_model = stack_fit,
    calibration_model = cal_fit,
    components = comp,
    engine = engine,
    calibration = calibration,
    eval_time = eval_time
  )
  class(out) <- c("joint_model", "list")
  out
}

#' Extract Survival Outcome Components and Predictors
#'
#' Helper utility that parses a survival formula into clean components
#' supporting both 2-parameter \code{Surv(time, status)} and 3-parameter start/stop
#' \code{Surv(tstart, tstop, status)} counting process structures.
#'
#' @param data A data frame.
#' @param outcome A survival formula or Surv expression.
#' @param subject_id Optional subject identifier column name.
#'
#' @return A list with elements \code{surv_obj}, \code{time}, \code{start}, \code{status},
#'   \code{type}, \code{predictors}, \code{pred_names}, and \code{formula}.
#' @export
extract_surv_components <- function(data, outcome, subject_id = NULL) {
  if (!inherits(outcome, "formula")) {
    stop("`outcome` must be a formula containing a Surv() response, e.g. Surv(time, status) ~ .",
         call. = FALSE)
  }

  mf <- stats::model.frame(outcome, data = data, na.action = stats::na.pass)
  surv_col <- stats::model.response(mf)

  if (!inherits(surv_col, "Surv")) {
    stop("The left-hand side of `outcome` must evaluate to a survival::Surv object.", call. = FALSE)
  }

  surv_type <- attr(surv_col, "type")
  is_counting <- identical(surv_type, "counting")

  time_val <- if (is_counting) surv_col[, "stop"] else surv_col[, "time"]
  start_val <- if (is_counting) surv_col[, "start"] else rep(0, nrow(data))
  status_val <- as.integer(surv_col[, "status"])

  # Extract predictors excluding response and subject_id
  all_vars <- all.vars(outcome)
  resp_vars <- all.vars(outcome[[2L]])
  pred_names <- setdiff(names(data), c(resp_vars, subject_id))
  predictors <- data[, pred_names, drop = FALSE]

  list(
    surv_obj = surv_col,
    time = time_val,
    start = start_val,
    status = status_val,
    type = surv_type,
    predictors = predictors,
    pred_names = pred_names,
    formula = outcome
  )
}

#' Predict Method for Joint Models
#'
#' Generates multi-paradigm predictions from a fitted \code{\link[TempleCBE]{joint_model}}:
#' dynamic survival probabilities from the Cox model, binary event probabilities from the
#' status model (both raw and calibrated), and expected duration from the time model.
#'
#' @param object A \code{joint_model} object.
#' @param new_data Optional new data frame to predict upon. If \code{NULL}, predicts on training data.
#' @param eval_time Horizon times for survival probabilities. Defaults to the model's \code{eval_time}.
#' @param ... Additional arguments passed to underlying predict methods.
#'
#' @return A tibble with columns:
#'   \item{.pred_survival}{Nested list of survival probability curves over \code{eval_time}.}
#'   \item{.pred_status}{Predicted event probability from the status model.}
#'   \item{.pred_status_calibrated}{Calibrated event probability (if calibration was enabled).}
#'   \item{.pred_time}{Predicted duration from the time model.}
#'   \item{.pred_linear_pred}{Linear predictor from the Cox model (higher = longer survival).}
#'   \item{.pred_risk_score}{Relative hazard risk score from the Cox model.}
#' @export
predict.joint_model <- function(object, new_data = NULL, eval_time = NULL, ...) {
  if (is.null(new_data)) {
    new_data <- object$components$predictors
  }
  eval_time <- eval_time %||% object$eval_time
  x_mat <- stats::model.matrix(~ . - 1, data = new_data[, object$components$pred_names, drop = FALSE])

  # 1. Coxnet predictions
  cox_res <- if (inherits(object$coxnet_model, "cv_coxnet")) {
    lp <- as.numeric(stats::predict(object$coxnet_model, new_data = new_data, type = "linear_pred")$.pred_linear_pred)
    surv_prob <- stats::predict(object$coxnet_model, new_data = new_data, type = "survival", eval_time = eval_time)
    list(lp = lp, surv = surv_prob$.pred)
  } else {
    lp <- as.numeric(stats::predict(object$coxnet_model, new_data = new_data, type = "linear_pred")$.pred_linear_pred)
    surv_prob <- stats::predict(object$coxnet_model, new_data = new_data, type = "survival", eval_time = eval_time)
    list(lp = lp, surv = surv_prob$.pred)
  }

  # 2. Status predictions
  status_raw <- if (inherits(object$status_model, "cv.glmnet")) {
    as.numeric(stats::predict(object$status_model, newx = x_mat, s = "lambda.min", type = "response"))
  } else if (inherits(object$status_model, "model_fit")) {
    as.numeric(stats::predict(object$status_model, new_data = new_data, type = "prob")$.pred_event)
  } else {
    rep(NA_real_, nrow(new_data))
  }

  # Calibrated status predictions
  status_cal <- status_raw
  if (!is.null(object$calibration_model) && requireNamespace("probably", quietly = TRUE)) {
    df_pred <- data.frame(
      .pred_event = status_raw,
      .pred_event_free = 1 - status_raw
    )
    cal_applied <- tryCatch({
      probably::cal_apply(df_pred, object$calibration_model)
    }, error = function(e) NULL)
    if (!is.null(cal_applied) && ".pred_event" %in% names(cal_applied)) {
      status_cal <- cal_applied$.pred_event
    }
  }

  # 3. Time predictions
  time_pred <- if (inherits(object$time_model, "cv.glmnet")) {
    as.numeric(stats::predict(object$time_model, newx = x_mat, s = "lambda.min"))
  } else if (inherits(object$time_model, "model_fit")) {
    as.numeric(stats::predict(object$time_model, new_data = new_data)$.pred)
  } else {
    rep(NA_real_, nrow(new_data))
  }

  tibble::tibble(
    .pred_survival = cox_res$surv,
    .pred_status = status_raw,
    .pred_status_calibrated = status_cal,
    .pred_time = time_pred,
    .pred_linear_pred = cox_res$lp,
    .pred_risk_score = exp(-cox_res$lp)
  )
}

#' Print Method for Joint Models
#' @param x A \code{joint_model} object.
#' @param ... Additional arguments.
#' @export
print.joint_model <- function(x, ...) {
  cat("=== TempleCBE Joint Survival-Status-Time Model ===\n")
  cat(sprintf("Engine: %s | Calibration: %s\n", x$engine, isTRUE(x$calibration)))
  cat(sprintf("Outcome: %s (Type: %s)\n", deparse(x$components$formula[[2L]]), x$components$type))
  cat(sprintf("Sample Size: %d observations | Predictors: %d\n",
              length(x$components$time), length(x$components$pred_names)))
  cat(sprintf("Events: %d (%.1f%%) | Median Follow-up Time: %.2f\n",
              sum(x$components$status),
              100 * mean(x$components$status),
              stats::median(x$components$time)))
  cat("Fitted Sub-Models:\n")
  cat(sprintf("  1. Cox Survival Model: %s\n", class(x$coxnet_model)[1]))
  cat(sprintf("  2. Status Classification Model: %s\n", class(x$status_model)[1]))
  cat(sprintf("  3. Time Duration Model: %s\n", class(x$time_model)[1]))
  if (!is.null(x$stack_model)) {
    cat(sprintf("  4. Stacked Meta-Learner: %s\n", class(x$stack_model)[1]))
  }
  invisible(x)
}

#' Tidy Method for Joint Models
#'
#' Extracts and aligns nonzero coefficients across the survival (\code{coxnet}),
#' binary classification (\code{status}), and duration regression (\code{time}) models.
#'
#' @param x A \code{joint_model} object.
#' @param ... Additional arguments.
#' @return A tibble with comparative coefficients per feature.
#' @export
tidy.joint_model <- function(x, ...) {
  terms <- x$components$pred_names

  # Extract coxnet coefficients
  cox_coef <- if (inherits(x$coxnet_model, "cv_coxnet")) {
    generics::tidy(x$coxnet_model, penalty = "lambda.min")
  } else if (inherits(x$coxnet_model, "coxnet_model")) {
    generics::tidy(x$coxnet_model)
  } else {
    tibble::tibble(term = terms, estimate = NA_real_)
  }
  names(cox_coef)[names(cox_coef) == "estimate"] <- "estimate_coxnet"

  # Extract status coefficients (if glmnet)
  status_coef <- if (inherits(x$status_model, "cv.glmnet")) {
    cf <- stats::coef(x$status_model, s = "lambda.min")
    tibble::tibble(term = rownames(cf)[-1], estimate_status = as.numeric(cf)[-1])
  } else {
    tibble::tibble(term = terms, estimate_status = NA_real_)
  }

  # Extract time coefficients (if glmnet)
  time_coef <- if (inherits(x$time_model, "cv.glmnet")) {
    cf <- stats::coef(x$time_model, s = "lambda.min")
    tibble::tibble(term = rownames(cf)[-1], estimate_time = as.numeric(cf)[-1])
  } else {
    tibble::tibble(term = terms, estimate_time = NA_real_)
  }

  out <- tibble::tibble(term = terms) |>
    dplyr::left_join(dplyr::select(cox_coef, "term", "estimate_coxnet"), by = "term") |>
    dplyr::left_join(status_coef, by = "term") |>
    dplyr::left_join(time_coef, by = "term")

  out
}

#' Cross-Validation for Joint Models
#'
#' Evaluates the \code{\link[TempleCBE]{joint_model}} trio across cross-validation splits
#' (e.g. via \code{\link[rsample]{vfold_cv}} or \code{\link[rsample]{group_vfold_cv}}),
#' benchmarking survival, classification, and regression paradigms using the IPCW
#' Integrated Brier Score (\code{brier_survival_integrated}), Concordance, and Calibration.
#'
#' @param data A data frame.
#' @param outcome Survival formula with a \code{Surv()} outcome.
#' @param v Number of cross-validation folds (default 5).
#' @param resamples Optional pre-constructed \pkg{rsample} object.
#' @param subject_id Optional subject identifier column for grouped resampling.
#' @param engine Modeling engine: \code{"glmnet"}, \code{"baguette"}, or \code{"stacks"}.
#' @param calibration Logical; whether to calibrate status predictions (default \code{TRUE}).
#' @param parallel Logical; whether to run folds in parallel via \pkg{furrr}.
#' @param ... Additional arguments passed to \code{\link[TempleCBE]{joint_model}}.
#'
#' @note Counting-process \code{Surv(start, stop, event)} outcomes are not yet
#'   supported here (only in \code{\link[TempleCBE]{joint_model}} itself):
#'   scoring needs one row per subject, but predictions are one row per
#'   interval. Use \code{Surv(time, status)} outcomes for cross-validation.
#'
#' @return An S3 object of class \code{c("cv_joint_model", "tbl_df")} summarizing
#'   comparative metrics across folds.
#' @export
cv_joint_model <- function(data,
                           outcome = survival::Surv(time, status) ~ .,
                           v = 5,
                           resamples = NULL,
                           subject_id = NULL,
                           engine = c("glmnet", "baguette", "stacks"),
                           calibration = TRUE,
                           parallel = FALSE,
                           ...) {
  engine <- match.arg(engine)
  rlang::check_installed(c("rsample", "yardstick", "survival", "glmnet"),
                         reason = "for cross-validation of joint_model.")

  if (identical(extract_surv_components(data, outcome, subject_id)$type, "counting")) {
    stop(
      "cv_joint_model() does not yet support counting-process Surv(start, stop, event) ",
      "outcomes: scoring needs one prediction row per subject, but joint_model() predicts ",
      "one row per input row (per interval). Collapse to one row per subject with ",
      "surv_subject_truth() first, or fit joint_model() directly without cross-validation.",
      call. = FALSE
    )
  }

  # Create resamples if not provided
  if (is.null(resamples)) {
    resamples <- if (!is.null(subject_id) && subject_id %in% names(data)) {
      rsample::group_vfold_cv(data, group = dplyr::all_of(subject_id), v = v)
    } else {
      rsample::vfold_cv(data, v = v)
    }
  }

  run_fold <- function(split) {
    analysis_df <- rsample::analysis(split)
    assessment_df <- rsample::assessment(split)

    # Fit joint model on analysis set
    fit <- joint_model(
      data = analysis_df,
      outcome = outcome,
      subject_id = subject_id,
      engine = engine,
      calibration = calibration,
      ...
    )

    # Predict on assessment set
    preds <- stats::predict(fit, new_data = assessment_df)
    comp_assess <- extract_surv_components(assessment_df, outcome, subject_id)
    comp_train <- extract_surv_components(analysis_df, outcome, subject_id)

    # Evaluate dynamic survival scoring via Integrated Brier Score
    cens_km <- censoring_km(comp_train$surv_obj)
    eval_time <- fit$eval_time

    # Score Coxnet via yardstick::brier_survival_integrated
    cox_surv_mat <- do.call(rbind, lapply(preds$.pred_survival, function(df) df$.pred_survival))
    cox_ibs <- score_surv_matrix_ibs(cox_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    # Score status model (converting 1 - p_event to survival proxy at eval_time)
    status_surv_mat <- matrix(1 - preds$.pred_status_calibrated,
                              nrow = nrow(assessment_df), ncol = length(eval_time))
    status_ibs <- score_surv_matrix_ibs(status_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    # Score time model (converting predicted duration to survival proxy)
    time_surv_mat <- matrix(as.numeric(preds$.pred_time > rep(eval_time, each = nrow(assessment_df))),
                            nrow = nrow(assessment_df), ncol = length(eval_time))
    time_ibs <- score_surv_matrix_ibs(time_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    # Concordance (Harrell's C-index)
    c_cox <- tryCatch({
      surv_truth_df <- data.frame(
        .truth = comp_assess$surv_obj,
        .pred = preds$.pred_linear_pred
      )
      yardstick::concordance_survival(surv_truth_df, truth = .truth, estimate = .pred)$.estimate
    }, error = function(e) NA_real_)

    c_status <- tryCatch({
      surv_truth_df <- data.frame(
        .truth = comp_assess$surv_obj,
        .pred = -preds$.pred_status_calibrated
      )
      yardstick::concordance_survival(surv_truth_df, truth = .truth, estimate = .pred)$.estimate
    }, error = function(e) NA_real_)

    c_time <- tryCatch({
      surv_truth_df <- data.frame(
        .truth = comp_assess$surv_obj,
        .pred = preds$.pred_time
      )
      yardstick::concordance_survival(surv_truth_df, truth = .truth, estimate = .pred)$.estimate
    }, error = function(e) NA_real_)

    tibble::tibble(
      model = c("coxnet", "status_calibrated", "time_regression"),
      ibs = c(cox_ibs, status_ibs, time_ibs),
      concordance = c(c_cox, c_status, c_time)
    )
  }

  fold_ids <- seq_along(resamples$splits)
  rows <- if (isTRUE(parallel)) {
    rlang::check_installed(c("furrr", "future"), reason = "for parallel joint cross-validation.")
    furrr::future_map(fold_ids, function(i) {
      out <- run_fold(resamples$splits[[i]])
      out$fold <- resamples$id[i]
      out
    }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(fold_ids, function(i) {
      out <- run_fold(resamples$splits[[i]])
      out$fold <- resamples$id[i]
      out
    })
  }

  res_tbl <- purrr::list_rbind(rows)
  class(res_tbl) <- c("cv_joint_model", class(res_tbl))
  res_tbl
}

#' Nested Cross-Validation for Joint Models
#'
#' Implements two-layer nested cross-validation on an \code{\link[rsample]{nested_cv}}
#' object to evaluate the joint model pipeline without tuning leakage.
#'
#' @param object An \code{rsample::nested_cv} object.
#' @param outcome Survival formula with a \code{Surv()} outcome.
#' @param subject_id Optional subject identifier column.
#' @param engine Modeling engine: \code{"glmnet"}, \code{"baguette"}, or \code{"stacks"}.
#' @param calibration Logical; whether to calibrate status predictions (default \code{TRUE}).
#' @param parallel Logical; whether to run outer splits in parallel.
#' @param ... Additional arguments.
#'
#' @note Counting-process \code{Surv(start, stop, event)} outcomes are not yet
#'   supported here (only in \code{\link[TempleCBE]{joint_model}} itself):
#'   scoring needs one row per subject, but predictions are one row per
#'   interval. Use \code{Surv(time, status)} outcomes for cross-validation.
#'
#' @return An S3 object of class \code{c("nested_cv_joint_model", "tbl_df")}.
#' @export
nested_cv_joint_model <- function(object,
                                  outcome = survival::Surv(time, status) ~ .,
                                  subject_id = NULL,
                                  engine = c("glmnet", "baguette", "stacks"),
                                  calibration = TRUE,
                                  parallel = FALSE,
                                  ...) {
  if (!inherits(object, "nested_cv")) {
    stop("`object` must be an rsample::nested_cv() object.", call. = FALSE)
  }
  engine <- match.arg(engine)

  if (identical(extract_surv_components(object$splits[[1]]$data, outcome, subject_id)$type, "counting")) {
    stop(
      "nested_cv_joint_model() does not yet support counting-process Surv(start, stop, event) ",
      "outcomes: scoring needs one prediction row per subject, but joint_model() predicts ",
      "one row per input row (per interval). Collapse to one row per subject with ",
      "surv_subject_truth() first, or fit joint_model() directly without cross-validation.",
      call. = FALSE
    )
  }

  run_outer <- function(i) {
    outer_split <- object$splits[[i]]
    analysis_df <- rsample::analysis(outer_split)
    assessment_df <- rsample::assessment(outer_split)

    fit <- joint_model(
      data = analysis_df,
      outcome = outcome,
      subject_id = subject_id,
      engine = engine,
      calibration = calibration,
      ...
    )

    preds <- stats::predict(fit, new_data = assessment_df)
    comp_assess <- extract_surv_components(assessment_df, outcome, subject_id)
    comp_train <- extract_surv_components(analysis_df, outcome, subject_id)
    cens_km <- censoring_km(comp_train$surv_obj)
    eval_time <- fit$eval_time

    # Integrated Brier Score
    cox_surv_mat <- do.call(rbind, lapply(preds$.pred_survival, function(df) df$.pred_survival))
    cox_ibs <- score_surv_matrix_ibs(cox_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    status_surv_mat <- matrix(1 - preds$.pred_status_calibrated,
                              nrow = nrow(assessment_df), ncol = length(eval_time))
    status_ibs <- score_surv_matrix_ibs(status_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    time_surv_mat <- matrix(as.numeric(preds$.pred_time > rep(eval_time, each = nrow(assessment_df))),
                            nrow = nrow(assessment_df), ncol = length(eval_time))
    time_ibs <- score_surv_matrix_ibs(time_surv_mat, eval_time, comp_assess$surv_obj, cens_km)

    tibble::tibble(
      outer_id = object$id[i],
      model = c("coxnet", "status_calibrated", "time_regression"),
      ibs = c(cox_ibs, status_ibs, time_ibs)
    )
  }

  outer_ids <- seq_along(object$splits)
  rows <- if (isTRUE(parallel)) {
    rlang::check_installed(c("furrr", "future"), reason = "for parallel nested cross-validation.")
    furrr::future_map(outer_ids, run_outer, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(outer_ids, run_outer)
  }

  out <- purrr::list_rbind(rows)
  class(out) <- c("nested_cv_joint_model", class(out))
  out
}

#' Helper to Score Survival Probability Matrix with Integrated Brier Score
#' @param surv_matrix Numeric matrix of predicted survival probabilities (rows = subjects, cols = eval_time).
#' @param eval_time Numeric vector of evaluation times.
#' @param surv_truth Surv object of true assessment outcomes.
#' @param cens_km Fitted Kaplan-Meier censoring object.
#' @param trunc Truncation bound for IPCW weights.
#' @noRd
score_surv_matrix_ibs <- function(surv_matrix, eval_time, surv_truth, cens_km, trunc = 0.05) {
  weights <- graf_weights(surv_truth, eval_time, cens_km, trunc)
  n_times <- length(eval_time)
  brier_vals <- numeric(n_times)

  for (t_idx in seq_len(n_times)) {
    t_val <- eval_time[t_idx]
    w_vec <- weights[, t_idx]
    s_hat <- surv_matrix[, t_idx]
    status_at_t <- as.numeric(surv_truth[, "time"] > t_val)

    non_zero <- w_vec > 0
    if (sum(non_zero) > 0) {
      brier_vals[t_idx] <- sum(w_vec[non_zero] * (status_at_t[non_zero] - s_hat[non_zero])^2) / sum(w_vec[non_zero])
    } else {
      brier_vals[t_idx] <- NA_real_
    }
  }

  # Trapezoidal integration across eval_time
  valid <- !is.na(brier_vals)
  if (sum(valid) < 2L) return(mean(brier_vals, na.rm = TRUE))

  times_v <- eval_time[valid]
  briers_v <- brier_vals[valid]
  delta_t <- diff(times_v)
  mid_brier <- (briers_v[-1] + briers_v[-length(briers_v)]) / 2
  ibs <- sum(delta_t * mid_brier) / (max(times_v) - min(times_v))
  ibs
}

#' Autoplot Method for Cross-Validated Joint Models
#'
#' Visualizes comparative predictive performance across paradigms,
#' plotting the distribution of Integrated Brier Scores and Concordance.
#'
#' @param object A \code{cv_joint_model} object.
#' @param ... Additional arguments.
#' @return A ggplot2 object.
#' @exportS3Method ggplot2::autoplot
autoplot.cv_joint_model <- function(object, ...) {
  rlang::check_installed("ggplot2", reason = "for autoplot.cv_joint_model.")

  p1 <- ggplot2::ggplot(object, ggplot2::aes(x = .data$model, y = .data$ibs, fill = .data$model)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, size = 2, shape = 21, color = "black") +
    ggplot2::scale_fill_manual(values = c("coxnet" = "#9D2235", "status_calibrated" = "#3B5998", "time_regression" = "#4A777A")) +
    ggplot2::labs(
      title = "Cross-Validated Integrated Brier Score (IBS)",
      subtitle = "Lower IBS indicates superior calibrated survival predictions (TempleCBE IPCW)",
      x = "Model Paradigm",
      y = "Integrated Brier Score (IBS)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")

  p1
}
