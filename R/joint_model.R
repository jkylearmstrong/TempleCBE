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
#' }
#'
#' With \code{engine = "stacks"} the status and time models are the bagged trees of
#' \code{engine = "baguette"}, and a penalized Cox meta-learner (\pkg{glmnet}) is also fit
#' on the standardized predictions of the three models and stored as \code{stack_model}.
#' \code{\link{predict.joint_model}} returns the meta-learner's linear predictor and risk score
#' as the extra columns \code{.pred_stack_linear_pred} and \code{.pred_stack_risk_score}; the
#' survival, status and time predictions are exactly those of \code{"baguette"}, and the
#' \pkg{stacks} package is not involved. A warning says so.
#'
#' This joint framework enables clinical researchers to investigate and contrast true
#' survival modeling against naive classification and regression proxies as discussed in
#' clinical literature (e.g., Rizopoulos 2015, PMC4503792).
#'
#' Predictors are processed through the \pkg{hardhat} \code{\link[TempleCBE]{coxnet}}
#' formula blueprint (one-hot dummy encoding, no intercept), the same preprocessing
#' used by \code{\link[TempleCBE]{coxnet}} and \code{\link[TempleCBE]{cv_coxnet}}, so the
#' status and time sub-models see exactly the columns the survival sub-model does, and
#' `predict()` re-applies the training encoding via \code{\link[hardhat]{forge}} rather
#' than recomputing dummy columns from scratch.
#'
#' The status calibrator is fitted on out-of-fold predictions, never on the training
#' rows' own predictions, which are overfit (a bagged tree classifies its training rows
#' almost perfectly, and a calibrator fitted on them amplifies the overfit). For
#' \code{engine = "glmnet"} these are \code{\link[glmnet]{cv.glmnet}}'s prevalidated
#' predictions at \code{lambda.min}; for the bagged trees, the predictions of an inner
#' 5-fold cross-validation. With start/stop data and a \code{subject_id}, the folds are
#' grouped by subject, so a subject's intervals are never split between the model and the
#' predictions used to calibrate it; the same folds select the penalty of the glmnet
#' status and time models. When out-of-fold predictions cannot be produced, a warning says
#' so and the status probabilities are returned uncalibrated.
#'
#' @param data A data frame containing the survival outcome and predictors.
#' @param outcome A formula containing a \code{\link[survival]{Surv}} outcome, such as
#'   \code{Surv(time, status) ~ .} or \code{Surv(tstart, tstop, status) ~ .}.
#' @param subject_id Optional character string specifying the subject identifier column
#'   for counting-process (start/stop) data.
#' @param engine Character string specifying the modeling engine: \code{"glmnet"} (default),
#'   \code{"baguette"} (bagged decision trees), or \code{"stacks"} (the \code{"baguette"}
#'   models plus a Cox meta-learner whose predictions \code{predict()} adds as extra columns;
#'   see Details).
#' @param calibration Logical; whether to fit a probability calibration model on the
#'   status predictions using \pkg{probably} (default \code{TRUE}), fitted on out-of-fold
#'   predictions; see Details.
#' @param mixture Elastic net mixing parameter for \pkg{glmnet} models (default \code{1} for lasso).
#' @param penalty Penalty value for the \code{coxnet} survival model. If \code{NULL} (default),
#'   tuned automatically via internal cross-validation using the Integrated Brier Score. The
#'   \pkg{glmnet} status and time models always choose their own penalty (\code{lambda.min}).
#' @param eval_time Optional vector of evaluation times for dynamic survival probabilities.
#'   Defaults to deciles of uncensored event times.
#' @param covariates \code{"path"} (the default) or \code{"baseline"}; as in
#'   \code{\link[TempleCBE]{cv_coxnet}}. Used only when \code{penalty} is \code{NULL}, to score
#'   the internal cross-validation that tunes the penalty of the \code{coxnet} model on start/stop data.
#' @param ... Additional arguments passed to \code{\link[TempleCBE]{cv_coxnet}} (when
#'   \code{penalty} is \code{NULL}) or \code{\link[TempleCBE]{coxnet}}, and so to \code{\link[glmnet]{glmnet}}
#'   for the survival model only.
#'
#' @return An S3 object of class \code{c("joint_model", "hardhat_model")} with elements:
#'   \item{coxnet_model}{Fitted penalized Cox proportional hazards model.}
#'   \item{status_model}{Fitted binary event classification model.}
#'   \item{time_model}{Fitted continuous follow-up duration model.}
#'   \item{stack_model}{Fitted Cox meta-learner (if \code{engine = "stacks"}); \code{predict()} returns
#'     its output as \code{.pred_stack_linear_pred} and \code{.pred_stack_risk_score}.}
#'   \item{calibration_model}{Probability calibration model from \pkg{probably} (\code{NULL} if
#'     \code{calibration = FALSE} or if it was skipped with a warning).}
#'   \item{components}{List of extracted outcome variables, formulas, the hardhat blueprint, and the
#'     raw (pre-blueprint) predictor columns, \code{raw_predictors}.}
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
                        covariates = c("path", "baseline"),
                        ...) {
  engine <- match.arg(engine)
  covariates <- match.arg(covariates)
  rlang::check_installed(c("glmnet", "survival", "rsample", "yardstick"),
                         reason = "for joint survival modeling.")

  if (engine %in% c("baguette", "stacks")) {
    rlang::check_installed(c("parsnip", "workflows", "baguette"),
                           reason = "for baguette/stacks engine in joint_model.")
  }
  if (engine == "stacks") {
    warning(
      "`engine = \"stacks\"` fits the same bagged trees as `engine = \"baguette\"`, so ",
      "`.pred_survival`, `.pred_status` and `.pred_time` are the same; it only adds the ",
      "meta-learner columns `.pred_stack_linear_pred` and `.pred_stack_risk_score`, and the ",
      "stacks package is not involved. See ?joint_model.",
      call. = FALSE
    )
  }
  if (isTRUE(calibration)) {
    rlang::check_installed("probably", reason = "for probability calibration in joint_model.")
  }

  # Extract survival components and predictors through the hardhat blueprint
  comp <- extract_surv_components(data, outcome, subject_id)
  x_mat <- predictors_matrix(comp$predictors)

  # A subject's start/stop rows must stay together in every internal fold: the
  # status and time penalties are chosen, and the calibrator is fitted, on the
  # held-out predictions of those folds.
  subject <- if (!is.null(subject_id)) data[[subject_id]]
  foldid <- if (identical(comp$type, "counting") && !is.null(subject)) joint_foldid(subject)

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
    # Auto-tune penalty via cv_coxnet; cv_coxnet() drops `subject_id` from the
    # predictors itself.
    cv_coxnet(
      comp$formula,
      data = data,
      subject_id = subject_id,
      mixture = mixture,
      eval_time = eval_time,
      covariates = covariates,
      ...
    )
  } else {
    # coxnet() has no `subject_id` argument, so drop the column first: it must
    # not leak into `...`/glmnet's arguments, nor be picked up by `~ .`.
    coxnet(
      comp$formula,
      data = data[setdiff(names(data), subject_id)],
      mixture = mixture,
      penalty = penalty,
      ...
    )
  }

  # 2. Status Model (Binary event classification)
  status_fac <- factor(comp$status, levels = c(0, 1), labels = c("event_free", "event"))
  status_fit <- NULL
  cal_fit <- NULL

  # The calibrator learns from out-of-fold probabilities only (`honest`): the
  # fitted model's own predictions for its training rows are overfit, and a
  # calibrator fitted on them amplifies the overfit.
  if (engine == "glmnet") {
    status_cv <- glmnet::cv.glmnet(
      x_mat,
      status_fac,
      family = "binomial",
      alpha = mixture,
      keep = isTRUE(calibration),
      foldid = foldid
    )
    honest <- if (isTRUE(calibration)) {
      # glmnet's prevalidated fits are on the link scale
      stats::plogis(status_cv$fit.preval[, match(status_cv$lambda.min, status_cv$lambda)])
    }
    status_cv$fit.preval <- NULL
    status_fit <- status_cv
  } else if (engine %in% c("baguette", "stacks")) {
    bag_spec <- parsnip::bag_tree() |>
      parsnip::set_engine("rpart") |>
      parsnip::set_mode("classification")
    status_df <- comp$predictors
    status_df$..joint_status.. <- status_fac
    status_fit <- parsnip::fit(bag_spec, ..joint_status.. ~ ., data = status_df)
    honest <- if (isTRUE(calibration)) {
      bagged_out_of_fold_status(bag_spec, status_df, subject %||% seq_len(nrow(status_df)))
    }
  }
  if (isTRUE(calibration)) {
    cal_fit <- fit_status_calibrator(honest, status_fac)
  }

  # 3. Time Model (Continuous follow-up duration regression)
  time_fit <- NULL
  if (engine == "glmnet") {
    time_cv <- glmnet::cv.glmnet(
      x_mat,
      comp$time,
      family = "gaussian",
      alpha = mixture,
      foldid = foldid
    )
    time_fit <- time_cv
  } else if (engine %in% c("baguette", "stacks")) {
    bag_time_spec <- parsnip::bag_tree() |>
      parsnip::set_engine("rpart") |>
      parsnip::set_mode("regression")
    time_df <- comp$predictors
    time_df$..joint_time.. <- comp$time
    time_fit <- parsnip::fit(bag_time_spec, ..joint_time.. ~ ., data = time_df)
  }

  # 4. Stack meta-learner (engine = "stacks"): penalized Cox regression combining normalized signals
  stack_fit <- NULL
  stack_scales <- NULL
  if (engine == "stacks") {
    cox_lp <- as.numeric(stats::predict(coxnet_fit, new_data = data, type = "linear_pred")$.pred_linear_pred)
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

    scale_params <- function(v) {
      s <- stats::sd(v, na.rm = TRUE)
      m <- mean(v, na.rm = TRUE)
      if (is.na(s) || s < 1e-8) {
        list(scaled = rep(0, length(v)), center = m, scale = 1)
      } else {
        list(scaled = as.numeric(scale(v)), center = m, scale = s)
      }
    }

    sp_cox <- scale_params(cox_lp)
    sp_time <- scale_params(time_pred)
    sp_status <- scale_params(-status_pred)
    stack_scales <- list(cox = sp_cox, time = sp_time, status = sp_status)

    meta_df <- data.frame(
      time = comp$time,
      status = comp$status,
      z_cox = sp_cox$scaled,
      z_time = sp_time$scaled,
      z_status = sp_status$scaled
    )
    meta_x <- as.matrix(meta_df[, c("z_cox", "z_time", "z_status")])
    meta_x[is.na(meta_x)] <- 0

    cv_args <- list(x = meta_x, y = comp$surv_obj, family = "cox")
    if ("cox.ties" %in% names(formals(glmnet::cv.glmnet))) {
      cv_args$cox.ties <- "breslow"
    }
    stack_fit <- tryCatch(
      do.call(glmnet::cv.glmnet, cv_args),
      error = function(e) {
        warning(
          "The meta-learner of `engine = \"stacks\"` could not be fitted (", conditionMessage(e),
          "), so `$stack_model` is NULL.",
          call. = FALSE
        )
        NULL
      }
    )
  }

  hardhat::new_model(
    coxnet_model = coxnet_fit,
    status_model = status_fit,
    time_model = time_fit,
    stack_model = stack_fit,
    stack_scales = stack_scales,
    calibration_model = cal_fit,
    components = comp,
    engine = engine,
    calibration = calibration,
    eval_time = eval_time,
    subject_id = subject_id,
    blueprint = comp$blueprint,
    class = "joint_model"
  )
}

#' Folds That Keep a Subject's Rows Together
#'
#' @param subject Subject identifier of every row.
#' @param nfolds Number of folds, capped at the number of subjects.
#' @return An integer fold id per row; all rows of a subject share one.
#' @keywords internal
#' @noRd
joint_foldid <- function(subject, nfolds = 10L) {
  subjects <- unique(subject)
  nfolds <- min(nfolds, length(subjects))
  fold_of_subject <- rep_len(seq_len(nfolds), length(subjects))[sample.int(length(subjects))]
  fold_of_subject[match(subject, subjects)]
}

#' Out-of-Fold Event Probabilities of the Bagged-Tree Status Model
#'
#' An inner cross-validation of `spec` on `status_df`, with folds grouped by
#' subject: every row's probability comes from a model that never saw its
#' subject.
#'
#' @param spec A parsnip classification specification.
#' @param status_df Predictors plus the `status` factor.
#' @param subject Subject identifier of every row.
#' @param v Number of folds, capped at the number of subjects.
#' @return A numeric vector with one event probability per row, `NA` where a
#'   fold's model could not be fit; `NULL` with fewer than 2 subjects.
#' @keywords internal
#' @noRd
bagged_out_of_fold_status <- function(spec, status_df, subject, v = 5L) {
  fold <- joint_foldid(subject, v)
  if (max(fold) < 2L) {
    return(NULL)
  }
  prob <- rep(NA_real_, nrow(status_df))
  for (k in seq_len(max(fold))) {
    held <- fold == k
    fit_k <- tryCatch(
      parsnip::fit(spec, ..joint_status.. ~ ., data = status_df[!held, , drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(fit_k)) next
    prob[held] <- tryCatch(
      as.numeric(stats::predict(fit_k, new_data = status_df[held, , drop = FALSE], type = "prob")$.pred_event),
      error = function(e) NA_real_
    )
  }
  prob
}

#' Fit the Status Calibrator on Out-of-Fold Probabilities
#'
#' @param honest Out-of-fold event probabilities, one per training row; `NA` or
#'   `NULL` where none could be produced.
#' @param status_fac The training status, a factor with levels `"event_free"`
#'   and `"event"`.
#' @return A \pkg{probably} calibration object, or `NULL` with a warning when
#'   there are too few out-of-fold predictions or the calibrator cannot be
#'   fitted: it is never fitted on in-sample predictions instead.
#' @keywords internal
#' @noRd
fit_status_calibrator <- function(honest, status_fac) {
  ok <- if (is.null(honest)) logical(0) else !is.na(honest)
  if (sum(ok) < 10L || nlevels(droplevels(status_fac[ok])) < 2L) {
    warning(
      "Out-of-fold status predictions could not be produced for the calibrator, so the status ",
      "probabilities are returned uncalibrated. (Calibrating on the training rows' own predictions would ",
      "amplify their overfit.)",
      call. = FALSE
    )
    return(NULL)
  }
  cal_df <- data.frame(
    .pred_event = honest[ok],
    .pred_event_free = 1 - honest[ok],
    status = status_fac[ok]
  )
  tryCatch(
    suppressWarnings(probably::cal_estimate_logistic(
      cal_df, truth = status, estimate = dplyr::starts_with(".pred_"), smooth = FALSE
    )),
    error = function(e) {
      warning(
        "The status calibrator could not be fitted (", conditionMessage(e),
        "), so the status probabilities are returned uncalibrated.",
        call. = FALSE
      )
      NULL
    }
  )
}

#' Status and Time Predictions of a Joint Model From Forged Predictors
#'
#' The one path from predictors to the status and time sub-models'
#' predictions, shared by [predict.joint_model()], the scoring in
#' [cv_joint_model()], and the explainers of [cbe_explain()].
#'
#' @param object A `joint_model`.
#' @param predictors The `$predictors` of [hardhat::forge()] on new data and the
#'   model's blueprint.
#' @return A list with `status` (raw event probability), `status_calibrated`
#'   (equal to `status` when there is no calibrator), and `time`, each with one
#'   value per row of `predictors`.
#' @keywords internal
#' @noRd
joint_status_time_predictions <- function(object, predictors) {
  x_mat <- predictors_matrix(predictors)

  status_raw <- if (inherits(object$status_model, "cv.glmnet")) {
    as.numeric(stats::predict(object$status_model, newx = x_mat, s = "lambda.min", type = "response"))
  } else if (inherits(object$status_model, "model_fit")) {
    as.numeric(stats::predict(object$status_model, new_data = predictors, type = "prob")$.pred_event)
  } else {
    rep(NA_real_, nrow(x_mat))
  }

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

  time_pred <- if (inherits(object$time_model, "cv.glmnet")) {
    as.numeric(stats::predict(object$time_model, newx = x_mat, s = "lambda.min"))
  } else if (inherits(object$time_model, "model_fit")) {
    as.numeric(stats::predict(object$time_model, new_data = predictors)$.pred)
  } else {
    rep(NA_real_, nrow(x_mat))
  }

  list(status = status_raw, status_calibrated = status_cal, time = time_pred)
}

#' The Raw (Pre-Blueprint) Predictor Columns of a Joint Model
#'
#' What the sub-models' `predict()` takes: the original columns, before the
#' one-hot encoding. Models fit before `raw_predictors` was stored fall back to
#' the columns of the stored data that the blueprint expects.
#' @keywords internal
#' @noRd
joint_raw_predictors <- function(object) {
  comp <- object$components
  comp$raw_predictors %||% comp$data[names(comp$blueprint$ptypes$predictors)]
}

#' Extract Survival Outcome Components and Predictors
#'
#' Helper utility that molds a survival formula into clean components, through the
#' same \pkg{hardhat} formula blueprint used by \code{\link[TempleCBE]{coxnet}}
#' (one-hot dummy encoding, no intercept), supporting both 2-parameter
#' \code{Surv(time, status)} and 3-parameter start/stop
#' \code{Surv(tstart, tstop, status)} counting process structures.
#'
#' @param data A data frame.
#' @param outcome A survival formula or Surv expression.
#' @param subject_id Optional subject identifier column name; dropped from `data`
#'   before molding, so it is never treated as a predictor.
#'
#' @return A list with elements \code{surv_obj}, \code{time}, \code{start}, \code{status},
#'   \code{type}, \code{predictors} (a numeric tibble, one-hot encoded), \code{pred_names},
#'   \code{raw_predictors} (the original predictor columns, before encoding, which is what
#'   the models' \code{predict()} methods and the explainers take), \code{formula},
#'   \code{data} (the molded, `subject_id`-free data frame), and
#'   \code{blueprint} (the hardhat blueprint used, for \code{\link[hardhat]{forge}}).
#' @export
extract_surv_components <- function(data, outcome, subject_id = NULL) {
  if (!inherits(outcome, "formula")) {
    stop("`outcome` must be a formula containing a Surv() response, e.g. Surv(time, status) ~ .",
         call. = FALSE)
  }
  if (!is.null(subject_id) &&
      (!is.character(subject_id) || length(subject_id) != 1 || !subject_id %in% names(data))) {
    stop("`subject_id` must be the name of a column in `data`.", call. = FALSE)
  }

  model_data <- data[setdiff(names(data), subject_id)]
  processed <- hardhat::mold(outcome, model_data, blueprint = coxnet_formula_blueprint())
  surv_col <- surv_outcome(processed$outcomes)

  surv_type <- attr(surv_col, "type")
  is_counting <- identical(surv_type, "counting")

  time_val <- if (is_counting) surv_col[, "stop"] else surv_col[, "time"]
  start_val <- if (is_counting) surv_col[, "start"] else rep(0, length(time_val))
  status_val <- as.integer(surv_col[, "status"])

  if (!all(is.finite(time_val)) || !all(is.finite(start_val))) {
    stop("Follow-up times must be finite; Inf or -Inf found in outcome.", call. = FALSE)
  }

  list(
    surv_obj = surv_col,
    time = time_val,
    start = start_val,
    status = status_val,
    type = surv_type,
    predictors = processed$predictors,
    pred_names = names(processed$predictors),
    raw_predictors = model_data[names(processed$blueprint$ptypes$predictors)],
    formula = outcome,
    data = model_data,
    blueprint = processed$blueprint,
    subject_id = subject_id
  )
}

#' Predict Method for Joint Models
#'
#' Generates multi-paradigm predictions from a fitted \code{\link[TempleCBE]{joint_model}}:
#' dynamic survival probabilities from the Cox model, binary event probabilities from the
#' status model (both raw and calibrated), and expected duration from the time model.
#'
#' Predictors are re-derived from \code{new_data} with \code{\link[hardhat]{forge}}
#' against the model's training blueprint, so factor levels and dummy columns match
#' training exactly, even if \code{new_data} does not exhibit every level.
#'
#' The result has one row per row of \code{new_data}, also for start/stop data, and each
#' row is predicted from its own covariates, taken as constant from time 0. That is not how a
#' subject's survival is scored along their start/stop covariate path; see
#' \code{\link{cv_joint_model}} (argument \code{covariates}).
#'
#' @param object A \code{joint_model} object.
#' @param new_data Optional new data frame to predict upon. If \code{NULL}, predicts on training data.
#' @param eval_time Horizon times for survival probabilities. Defaults to the model's \code{eval_time}.
#' @param type Optional single prediction type, to return only that column as a one-column tibble:
#'   \code{"survival"}, \code{"time"} (or \code{"numeric"}), \code{"prob"} (calibrated status
#'   probability), \code{"status"}, \code{"linear_pred"}, \code{"risk_score"},
#'   \code{"stack_linear_pred"} or \code{"stack_risk_score"}. \code{NULL} (the default) or
#'   \code{"all"} returns every column.
#' @param ... Not used; checked with \code{\link[rlang]{check_dots_empty}}.
#'
#' @return A tibble with columns:
#'   \item{.pred_survival}{Nested list of survival probability curves over \code{eval_time}.}
#'   \item{.pred_status}{Predicted event probability from the status model.}
#'   \item{.pred_status_calibrated}{Calibrated event probability (if calibration was enabled).}
#'   \item{.pred_time}{Predicted duration from the time model.}
#'   \item{.pred_linear_pred}{Linear predictor from the Cox model (higher = longer survival).}
#'   \item{.pred_risk_score}{Relative hazard risk score from the Cox model.}
#'   \item{.pred_stack_linear_pred}{Linear predictor from the stacked meta-learner (if \code{engine = "stacks"}).}
#'   \item{.pred_stack_risk_score}{Relative hazard risk score from the stacked meta-learner (if \code{engine = "stacks"}).}
#' @export
predict.joint_model <- function(object, new_data = NULL, eval_time = NULL, type = NULL, ...) {
  rlang::check_dots_empty()
  if (is.null(new_data)) {
    new_data <- object$components$data
  }
  eval_time <- eval_time %||% object$eval_time
  forged <- hardhat::forge(new_data, object$components$blueprint)

  # 1. Coxnet predictions
  lp <- as.numeric(stats::predict(object$coxnet_model, new_data = new_data, type = "linear_pred")$.pred_linear_pred)
  surv_prob <- stats::predict(object$coxnet_model, new_data = new_data, type = "survival", eval_time = eval_time)
  cox_res <- list(lp = lp, surv = surv_prob$.pred)

  # 2. Status predictions (raw and calibrated) and 3. time predictions
  aux <- joint_status_time_predictions(object, forged$predictors)
  status_raw <- aux$status
  status_cal <- aux$status_calibrated
  time_pred <- aux$time

  out <- tibble::tibble(
    .pred_survival = cox_res$surv,
    .pred_status = status_raw,
    .pred_status_calibrated = status_cal,
    .pred_time = time_pred,
    .pred_linear_pred = cox_res$lp,
    .pred_risk_score = exp(-cox_res$lp)
  )

  # 4. Stack meta-learner predictions (if present)
  if (!is.null(object$stack_model) && !is.null(object$stack_scales)) {
    scales <- object$stack_scales
    scale_apply <- function(v, name) {
      s <- scales[[name]]
      if (is.null(s) || s$scale < 1e-8) rep(0, length(v)) else (v - s$center) / s$scale
    }
    meta_new <- cbind(
      z_cox = scale_apply(cox_res$lp, "cox"),
      z_time = scale_apply(time_pred, "time"),
      z_status = scale_apply(-status_cal, "status")
    )
    meta_new[is.na(meta_new)] <- 0
    stack_lp <- as.numeric(stats::predict(object$stack_model, newx = meta_new, s = "lambda.min"))
    out$.pred_stack_linear_pred <- stack_lp
    out$.pred_stack_risk_score <- exp(-stack_lp)
  }

  if (!is.null(type) && type != "all") {
    return(switch(
      type,
      "survival" = tibble::tibble(.pred_survival = out$.pred_survival),
      "time" = tibble::tibble(.pred_time = out$.pred_time),
      "numeric" = tibble::tibble(.pred_time = out$.pred_time),
      "prob" = tibble::tibble(.pred_status_calibrated = out$.pred_status_calibrated),
      "status" = tibble::tibble(.pred_status = out$.pred_status),
      "linear_pred" = tibble::tibble(.pred_linear_pred = out$.pred_linear_pred),
      "risk_score" = tibble::tibble(.pred_risk_score = out$.pred_risk_score),
      "stack_linear_pred" = tibble::tibble(.pred_stack_linear_pred = out$.pred_stack_linear_pred),
      "stack_risk_score" = tibble::tibble(.pred_stack_risk_score = out$.pred_stack_risk_score),
      stop("Unknown prediction type '", type, "'.", call. = FALSE)
    ))
  }

  hardhat::validate_prediction_size(out, new_data)
  out
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
#' @exportS3Method generics::tidy
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
#' Integrated Brier Score (the \code{yardstick::brier_survival_integrated()} value, as
#' \code{\link[TempleCBE]{cv_coxnet}} and \code{\link[TempleCBE]{glmnet_IBS}} report it) and
#' Concordance. Each fold's model is fit on the analysis set and scored on the assessment
#' set, one row per subject, with Graf censoring weights from the analysis set.
#'
#' \strong{Subjects.} A counting-process (start/stop) outcome needs \code{subject_id}, so
#' that a subject's intervals stay in one fold and are scored together; without it the
#' function stops. For right-censored data each row is a subject when \code{subject_id} is
#' \code{NULL}. Unless \code{resamples} is given, folds come from
#' \code{\link[rsample]{group_vfold_cv}} grouped by \code{subject_id}.
#'
#' \strong{Supplied resamples.} With \code{resamples}, every split is checked before anything
#' is fit: if a subject is in both the analysis and the assessment set of a split, the function
#' stops, because the models would be scored on subjects they were fit on. That is what a
#' row-level \code{\link[rsample]{vfold_cv}} does to start/stop data. Group the folds by the subject
#' instead, with \code{\link[rsample]{group_vfold_cv}} or \code{\link[rsample]{group_bootstraps}};
#' the out-of-bag assessment set of a bootstrap never overlaps its analysis set.
#' \code{check_subject_overlap = FALSE} switches the check off, for overlap that is deliberate.
#'
#' \strong{Scoring start/stop data.} The three models are scored on the same subjects, but
#' not from the same rows:
#' \itemize{
#'   \item The \code{coxnet} model is scored as \code{\link[TempleCBE]{cv_coxnet}} scores it:
#'     the survival of each assessment subject comes from the Breslow baseline hazard of the
#'     analysis set and the subject's covariates. With \code{covariates = "path"} the cumulative
#'     hazard is integrated over the subject's start/stop covariate path, carrying each
#'     interval's values forward to the next interval and past the last one; with
#'     \code{"baseline"}, only the covariates of the first interval are used, which uses no
#'     information from after time 0. For the same fit, folds and \code{eval_time}, the
#'     \code{coxnet} IBS and concordance are those of \code{cv_coxnet()}. (Concordance uses the
#'     predicted survival at the last \code{eval_time}.)
#'   \item The status and time models predict one value per row, so for a subject they can only
#'     use one row. They use the \strong{baseline} (first-interval) row, whatever
#'     \code{covariates} is.
#' }
#'
#' \strong{Missing predictors.} An assessment subject with a missing predictor after
#' preprocessing, such as a factor level the analysis set never saw, cannot be predicted by any
#' of the models: it is left out of the scores of all three, with one warning per resample that
#' gives the count. A resample with no subject left is skipped.
#'
#' @param data A data frame.
#' @param outcome Survival formula with a \code{Surv()} outcome.
#' @param v Number of cross-validation folds (default 5).
#' @param resamples Optional pre-constructed \pkg{rsample} object. No subject may be in both the
#'   analysis and the assessment set of a split; see Details and \code{check_subject_overlap}.
#' @param subject_id Subject identifier column. Required for start/stop outcomes; used for
#'   grouped resampling.
#' @param engine Modeling engine: \code{"glmnet"}, \code{"baguette"}, or \code{"stacks"} (the
#'   \code{"baguette"} models plus a stored Cox meta-learner whose output \code{predict()} adds as
#'   extra columns; see \code{\link[TempleCBE]{joint_model}}).
#' @param calibration Logical; whether to calibrate status predictions (default \code{TRUE}).
#' @param parallel Logical; whether to run folds in parallel via \pkg{furrr}.
#' @param covariates \code{"path"} or \code{"baseline"}; see Details. The same as
#'   \code{\link[TempleCBE]{cv_coxnet}}'s argument, and also used when \code{penalty} is \code{NULL}
#'   to score the cross-validation that tunes each fold's \code{coxnet} penalty.
#' @param check_subject_overlap If \code{TRUE} (the default), stop when a supplied
#'   \code{resamples} puts the same subject in both the analysis and the assessment set of a
#'   split; see Details. \code{FALSE} skips the check. Folds that \code{cv_joint_model()} builds
#'   itself are never checked, as they are grouped by subject already.
#' @param ... Additional arguments passed to \code{\link[TempleCBE]{joint_model}}.
#'
#' @return An S3 object of class \code{c("cv_joint_model", "tbl_df")} summarizing
#'   comparative metrics across folds: \code{model}, \code{ibs}, \code{concordance}, and \code{fold}.
#' @export
cv_joint_model <- function(data,
                           outcome = survival::Surv(time, status) ~ .,
                           v = 5,
                           resamples = NULL,
                           subject_id = NULL,
                           engine = c("glmnet", "baguette", "stacks"),
                           calibration = TRUE,
                           parallel = FALSE,
                           covariates = c("path", "baseline"),
                           check_subject_overlap = TRUE,
                           ...) {
  engine <- match.arg(engine)
  covariates <- match.arg(covariates)
  rlang::check_installed(c("rsample", "yardstick", "survival", "glmnet"),
                         reason = "for cross-validation of joint_model.")
  check_true_false(check_subject_overlap, "check_subject_overlap")
  check_joint_subject_id(extract_surv_components(data, outcome, subject_id)$type, subject_id)

  if (is.null(resamples)) {
    # Folds made here are grouped by subject already; only supplied ones can leak.
    resamples <- if (!is.null(subject_id)) {
      rsample::group_vfold_cv(data, group = dplyr::all_of(subject_id), v = v)
    } else {
      rsample::vfold_cv(data, v = v)
    }
  } else {
    if (!inherits(resamples, "rset")) {
      stop("`resamples` must be an rsample rset, such as rsample::group_vfold_cv().", call. = FALSE)
    }
    if (check_subject_overlap) {
      check_resample_overlap(resamples, joint_subject_spec(subject_id), "`resamples`")
    }
  }

  run_fold <- function(i) {
    split <- resamples$splits[[i]]
    id <- resamples$id[i]
    scored <- joint_model_fold_scores(
      rsample::analysis(split), rsample::assessment(split), outcome, subject_id, engine, calibration,
      covariates = covariates, where = paste0("resample `", id, "`"), ...
    )
    if (is.null(scored)) {
      return(NULL)
    }
    out <- scored$ibs
    out$concordance <- scored$concordance
    out$fold <- id
    out
  }

  fold_ids <- seq_along(resamples$splits)
  rows <- if (isTRUE(parallel)) {
    rlang::check_installed(c("furrr", "future"), reason = "for parallel joint cross-validation.")
    furrr::future_map(fold_ids, run_fold, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(fold_ids, run_fold)
  }

  res_tbl <- purrr::list_rbind(rows)
  if (!nrow(res_tbl)) {
    stop("No resample could be scored; see the warnings.", call. = FALSE)
  }
  class(res_tbl) <- c("cv_joint_model", class(res_tbl))
  res_tbl
}

#' Nested Cross-Validation for Joint Models
#'
#' Scores the \code{\link[TempleCBE]{joint_model}} pipeline on the outer splits of an
#' \code{\link[rsample]{nested_cv}} object. As in \code{\link{cv_joint_model}}, each outer
#' split's model is fit on its analysis set and scored on its assessment set one row per
#' subject, and the \code{coxnet} model of a start/stop outcome is scored along each subject's
#' covariate path (\code{covariates}), as \code{\link[TempleCBE]{cv_coxnet}} does. The inner
#' resamples are not used to fit anything: the penalty of each outer split's \code{coxnet} model
#' is tuned by the internal cross-validation of \code{joint_model()} (unless \code{penalty} is
#' given), and the status and time models choose theirs by \code{cv.glmnet}.
#'
#' A counting-process (start/stop) outcome needs \code{subject_id}, and the outer splits and
#' every inner resample are checked before anything is fit: if a subject is in both the analysis
#' and the assessment set of one of them, the function stops, as \code{\link{cv_joint_model}} does
#' for supplied \code{resamples}. Build \code{object} with grouped resampling at both levels, e.g.
#' \code{nested_cv(data, outside = group_vfold_cv(group = subject_id), inside = group_vfold_cv(group = subject_id))}.
#' Out-of-bag assessment sets of bootstraps pass. \code{check_subject_overlap = FALSE} skips the check.
#'
#' @param object An \code{rsample::nested_cv} object.
#' @param outcome Survival formula with a \code{Surv()} outcome.
#' @param subject_id Subject identifier column. Required for start/stop outcomes.
#' @param engine Modeling engine: \code{"glmnet"}, \code{"baguette"}, or \code{"stacks"} (the
#'   \code{"baguette"} models plus a stored Cox meta-learner whose output \code{predict()} adds as
#'   extra columns; see \code{\link[TempleCBE]{joint_model}}).
#' @param calibration Logical; whether to calibrate status predictions (default \code{TRUE}).
#' @param parallel Logical; whether to run outer splits in parallel.
#' @param covariates \code{"path"} or \code{"baseline"}; see \code{\link{cv_joint_model}}.
#' @param check_subject_overlap If \code{TRUE} (the default), stop when an outer split, or an inner
#'   resample, puts the same subject in both its analysis and its assessment set; see Details.
#'   \code{FALSE} skips the check.
#' @param tune_method Method for tuning the Cox component penalty on the inner resamples:
#'   \code{"none"} (default, uses internal CV of \code{joint_model()}), \code{"race_anova"}
#'   (\code{\link[finetune]{tune_race_anova}} racing), or \code{"race_win_loss"}
#'   (\code{\link[finetune]{tune_race_win_loss}} racing, which needs \pkg{BradleyTerry2}).
#'   With racing, the penalty found on each outer split's inner resamples replaces any
#'   \code{penalty} given in \code{...}.
#' @param ... Additional arguments passed to \code{\link[TempleCBE]{joint_model}}.
#'
#' @return An S3 object of class \code{c("nested_cv_joint_model", "tbl_df")}: \code{outer_id},
#'   \code{model}, and \code{ibs}, one row per outer split and model.
#' @export
nested_cv_joint_model <- function(object,
                                  outcome = survival::Surv(time, status) ~ .,
                                  subject_id = NULL,
                                  engine = c("glmnet", "baguette", "stacks"),
                                  calibration = TRUE,
                                  parallel = FALSE,
                                  covariates = c("path", "baseline"),
                                  check_subject_overlap = TRUE,
                                  tune_method = c("none", "race_anova", "race_win_loss"),
                                  ...) {
  if (!inherits(object, "nested_cv")) {
    stop("`object` must be an rsample::nested_cv() object.", call. = FALSE)
  }
  engine <- match.arg(engine)
  covariates <- match.arg(covariates)
  tune_method <- match.arg(tune_method)
  check_true_false(check_subject_overlap, "check_subject_overlap")
  outcome_type <- extract_surv_components(object$splits[[1]]$data, outcome, subject_id)$type
  check_joint_subject_id(outcome_type, subject_id)
  if (tune_method != "none") check_race_outcome_type(outcome_type, tune_method)
  # The outer splits and every inner resample are supplied, so all of them are
  # checked here, before any model is fit.
  if (check_subject_overlap) {
    spec <- joint_subject_spec(subject_id)
    check_resample_overlap(object, spec, "The outer splits of `object`")
    for (i in seq_along(object$splits)) {
      check_resample_overlap(
        object$inner_resamples[[i]], spec,
        paste0("The inner resamples of outer split `", object$id[i], "`")
      )
    }
  }

  fit_args <- list(...)
  if (tune_method != "none" && "penalty" %in% names(fit_args)) {
    warning(
      "`penalty` is ignored when `tune_method = \"", tune_method, "\"`: the penalty is chosen by ",
      "racing on each outer split's inner resamples.",
      call. = FALSE
    )
  }

  run_outer <- function(i) {
    outer_split <- object$splits[[i]]
    analysis_df <- rsample::analysis(outer_split)
    assessment_df <- rsample::assessment(outer_split)

    inner_penalty <- NULL
    if (tune_method %in% c("race_anova", "race_win_loss")) {
      rlang::check_installed(c("finetune", "parsnip", "workflows"), reason = "for racing in nested_cv_joint_model.")
      spec_cox <- parsnip::set_engine(
        parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1),
        "coxnet"
      ) |> parsnip::set_mode("censored regression")
      wflow <- workflows::workflow() |>
        workflows::add_model(spec_cox) |>
        workflows::add_formula(outcome)
      race_res <- tune_race_survival(
        wflow,
        resamples = object$inner_resamples[[i]],
        fn = if (tune_method == "race_anova") "tune_race_anova" else "tune_race_win_loss",
        grid = 15,
        eval_time = fit_args$eval_time
      )
      best <- tune::select_best(race_res, metric = "brier_survival_integrated")
      inner_penalty <- best$penalty
    }

    # `penalty` can arrive in `...`; naming it a second time here would stop with
    # "formal argument "penalty" matched by multiple actual arguments". Only a penalty
    # found by racing replaces the caller's.
    fold_args <- fit_args
    if (!is.null(inner_penalty)) fold_args$penalty <- inner_penalty
    scored <- do.call(
      joint_model_fold_scores,
      c(
        list(
          analysis_df, assessment_df, outcome, subject_id, engine, calibration,
          covariates = covariates,
          where = paste0("the outer assessment set of split `", object$id[i], "`")
        ),
        fold_args
      )
    )
    if (is.null(scored)) {
      stop("Outer split `", object$id[i], "` has no assessment subject to score.", call. = FALSE)
    }
    tibble::tibble(outer_id = object$id[i], model = scored$ibs$model, ibs = scored$ibs$ibs)
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

#' The `spec` That `check_resample_overlap()` Reads, for a Joint-Model Subject
#' @keywords internal
#' @noRd
joint_subject_spec <- function(subject_id) {
  list(subject_col = subject_id, group_col = subject_id)
}

#' Stop When Start/Stop Data Comes Without a Subject
#'
#' @param type The `Surv` type of the outcome.
#' @param subject_id The subject identifier column name, or `NULL`.
#' @keywords internal
#' @noRd
check_joint_subject_id <- function(type, subject_id) {
  if (identical(type, "counting") && is.null(subject_id)) {
    stop(
      "`subject_id` is required for counting-process Surv(start, stop, event) outcomes, ",
      "so each subject's intervals stay in one fold and can be combined for scoring.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Fit a Joint Model on an Analysis Set and Score It on an Assessment Set
#'
#' Shared by \code{\link{cv_joint_model}} and \code{\link{nested_cv_joint_model}}. Scores are
#' per assessment subject. The coxnet model's survival comes from the scorer of
#' \code{\link[TempleCBE]{cv_coxnet}} (`predict_subject_survival()` on the Breslow baseline hazard
#' of the analysis set), along each subject's covariate path or from their baseline covariates;
#' the status and time models predict one value per row, so they use each subject's first
#' (baseline) row.
#'
#' @param covariates `"path"` or `"baseline"`.
#' @param where How the assessment set is named in the warning about subjects left out for
#'   missing predictors.
#' @return A list with `ibs` (a tibble of `model`, `ibs`) and `concordance` (a numeric
#'   vector aligned with `ibs$model`), or `NULL` when no assessment subject can be predicted.
#' @keywords internal
#' @noRd
joint_model_fold_scores <- function(analysis_df, assessment_df, outcome, subject_id, engine, calibration,
                                    covariates = c("path", "baseline"), where = "the assessment set", ...) {
  covariates <- match.arg(covariates)
  fit <- joint_model(
    data = analysis_df,
    outcome = outcome,
    subject_id = subject_id,
    engine = engine,
    calibration = calibration,
    covariates = covariates,
    ...
  )
  comp_train <- fit$components
  check_joint_subject_id(comp_train$type, subject_id)
  eval_time <- fit$eval_time

  # The assessment set goes through the training blueprint, as in predict(), with its outcome.
  forged <- hardhat::forge(
    assessment_df[setdiff(names(assessment_df), subject_id)], comp_train$blueprint, outcomes = TRUE
  )
  y_assess <- surv_outcome(forged$outcomes)
  # Keys, not raw identifiers: a bootstrap analysis set repeats subjects.
  subject_assess <- subject_keys(
    y_assess, if (is.null(subject_id)) seq_len(nrow(assessment_df)) else assessment_df[[subject_id]]
  )
  x_assess <- predictors_matrix(forged$predictors)

  # A subject with a missing predictor, most often a factor level the analysis set never saw
  # (hardhat turns it into NA with only a warning), cannot be predicted by any of the models:
  # it is left out of all three, with one warning, as in cv_coxnet().
  usable <- complete_assessment_rows(x_assess, subject_assess, where)
  if (!any(usable)) {
    return(NULL)
  }
  predictors <- forged$predictors[usable, , drop = FALSE]
  x_assess <- x_assess[usable, , drop = FALSE]
  y_assess <- y_assess[usable]
  subject_assess <- subject_assess[usable]
  assess <- surv_components(y_assess)

  subject_train <- subject_keys(
    comp_train$surv_obj,
    if (is.null(subject_id)) seq_len(nrow(analysis_df)) else analysis_df[[subject_id]]
  )
  truth_train <- surv_subject_truth(comp_train$surv_obj, subject_train)
  truth_test <- surv_subject_truth(y_assess, subject_assess)
  cens_km <- censoring_km(truth_train$.truth)
  n_subj <- nrow(truth_test)
  n_times <- length(eval_time)

  # Coxnet: the survival of each subject along their covariate path (or from
  # baseline), from the Breslow hazard of the analysis set, exactly as cv_coxnet() does.
  cox <- if (inherits(fit$coxnet_model, "cv_coxnet")) fit$coxnet_model$fit else fit$coxnet_model
  train <- surv_components(cox$y)
  bh <- breslow_cumhaz(
    coxnet_link(cox$fit, cox$x, cox$penalty), train$start, train$stop, train$status
  )
  pred <- predict_subject_survival(
    coxnet_link(cox$fit, x_assess[, colnames(cox$x), drop = FALSE], cox$penalty),
    assess$start, assess$stop, subject_assess, eval_time, bh, covariates
  )
  cox_surv_mat <- matrix(
    pred$surv[match(truth_test$.subject_id, pred$id), , 1], nrow = n_subj, ncol = n_times
  )
  cox_ibs <- score_surv_matrix_ibs(cox_surv_mat, eval_time, truth_test$.truth, cens_km)

  # Status and time models: one prediction per row, so the baseline (first-interval) row of each subject.
  aux <- joint_status_time_predictions(fit, predictors)
  first <- first_interval_rows(subject_assess, assess$start, assess$stop)
  at <- first[match(truth_test$.subject_id, subject_assess[first])]
  status_cal <- aux$status_calibrated[at]
  time_pred <- aux$time[at]

  # Status model (converting 1 - p_event to a survival proxy at eval_time)
  status_surv_mat <- matrix(1 - status_cal, nrow = n_subj, ncol = n_times)
  status_ibs <- score_surv_matrix_ibs(status_surv_mat, eval_time, truth_test$.truth, cens_km)

  # Time model (converting predicted duration to a survival proxy)
  time_surv_mat <- matrix(
    as.numeric(time_pred > rep(eval_time, each = n_subj)),
    nrow = n_subj, ncol = n_times
  )
  time_ibs <- score_surv_matrix_ibs(time_surv_mat, eval_time, truth_test$.truth, cens_km)

  concordance_of <- function(estimate) {
    tryCatch({
      surv_truth_df <- data.frame(.truth = truth_test$.truth, .pred = estimate)
      yardstick::concordance_survival(surv_truth_df, truth = .truth, estimate = .pred)$.estimate
    }, error = function(e) NA_real_)
  }

  list(
    ibs = tibble::tibble(
      model = c("coxnet", "status_calibrated", "time_regression"),
      ibs = c(cox_ibs, status_ibs, time_ibs)
    ),
    concordance = c(
      concordance_of(cox_surv_mat[, n_times]),
      concordance_of(-status_cal),
      concordance_of(time_pred)
    )
  )
}

#' First (Baseline) Row of Each Subject
#'
#' The row with the earliest start (ties broken by stop) of every subject, in the
#' order of [surv_subject_truth()] (by subject, then start, then stop).
#'
#' @param subject,start,stop Row-level subject identifiers and interval bounds.
#' @return Row indices, one per subject.
#' @keywords internal
#' @noRd
first_interval_rows <- function(subject, start, stop) {
  ord <- order(subject, start, stop)
  ord[!duplicated(subject[ord])]
}

#' Helper to Score Survival Probability Matrix with Integrated Brier Score
#'
#' The value of `yardstick::brier_survival_integrated()` (and of `cv_coxnet()` and `glmnet_IBS()`):
#' at every evaluation time, the Graf-weighted squared error summed over subjects and divided by
#' the number of subjects (censored-by-t subjects have weight 0), integrated by the trapezoidal
#' rule over `eval_time` and divided by the largest `eval_time`. With a single evaluation time, the
#' Brier score at that time.
#'
#' @param surv_matrix Numeric matrix of predicted survival probabilities (rows = subjects, cols = eval_time).
#' @param eval_time Numeric vector of evaluation times.
#' @param surv_truth Right-censored Surv object of true assessment outcomes, one per subject.
#' @param cens_km Fitted Kaplan-Meier censoring object.
#' @param trunc Truncation bound for IPCW weights.
#' @keywords internal
#' @noRd
score_surv_matrix_ibs <- function(surv_matrix, eval_time, surv_truth, cens_km, trunc = 0.05) {
  surv_matrix <- as.matrix(surv_matrix)
  weights <- graf_weights(surv_truth, eval_time, cens_km, trunc)
  alive <- outer(surv_components(surv_truth)$stop, eval_time, ">") * 1
  brier <- colSums(weights * (alive - surv_matrix)^2) / nrow(surv_matrix)

  if (length(eval_time) < 2L) {
    return(mean(brier))
  }
  sum(diff(eval_time) * (utils::head(brier, -1) + utils::tail(brier, -1)) / 2) / max(eval_time)
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
