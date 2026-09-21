#' The `coxnet` Engine for `parsnip::proportional_hazards()`
#'
#' Registers [coxnet()] as an engine of [parsnip::proportional_hazards()] in
#' `"censored regression"` mode, so penalized Cox models fit by TempleCBE work
#' anywhere a parsnip model does: [parsnip::fit()], \pkg{workflows},
#' \pkg{tune}, \pkg{workflowsets}, and \pkg{stacks}. Unlike \pkg{censored}'s
#' `"glmnet"` engine, it also accepts counting-process outcomes,
#' `Surv(start, stop, event)`, for time-varying covariates.
#'
#' The engine is registered when \pkg{parsnip} is loaded, whichever package is
#' loaded first; nothing needs to be called.
#'
#' Use it like any other engine:
#'
#' ```r
#' spec <- parsnip::proportional_hazards(penalty = 0.05, mixture = 0.5) |>
#'   parsnip::set_engine("coxnet")
#' fit <- parsnip::fit(spec, survival::Surv(time, status) ~ ., data = lung)
#' predict(fit, lung, type = "linear_pred")
#' predict(fit, lung, type = "survival", eval_time = c(180, 365))
#' ```
#'
#' * `penalty` is required (it is glmnet's `lambda`); `mixture` defaults to 1,
#'   the lasso. Both can be [tune::tune()]d. Each candidate is fit separately;
#'   the coxnet engine doesn't use glmnet's path to evaluate a grid of
#'   penalties from one fit, so tuning `penalty` alone costs one fit per value.
#' * Engine arguments given to [parsnip::set_engine()] are passed on to
#'   [glmnet::glmnet()], such as `nlambda` or `cox.ties`.
#' * Predictions: `"linear_pred"` (larger means longer survival, as in
#'   \pkg{censored}), `"survival"`, which needs `eval_time`, and `"time"`, the
#'   restricted mean survival time, which static metrics such as
#'   [yardstick::concordance_survival()] use.
#' * Case weights aren't supported, and neither is `strata()` in the formula.
#' * [generics::tidy()] on the parsnip fit returns the coefficients.
#'
#' @param formula A formula with a [survival::Surv()] outcome.
#' @param data A data frame containing the variables in `formula`.
#' @param penalty The penalty, glmnet's `lambda`: a single non-negative number.
#' @param mixture Elastic-net mixing parameter, glmnet's `alpha`. `NULL` means 1.
#' @param weights Not supported; must be `NULL`.
#' @param ... Further arguments passed to [glmnet::glmnet()].
#' @return For `coxnet_train()`, a [coxnet()] model. This is the function parsnip
#'   calls to fit the engine; call [coxnet()] directly outside parsnip.
#' @seealso [coxnet()], [cv_coxnet()], [parsnip::proportional_hazards()],
#'   `censored::proportional_hazards` engines
#' @export
#' @examples
#' if (requireNamespace("parsnip", quietly = TRUE) &&
#'     requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE)) {
#'   lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
#'   spec <- parsnip::set_engine(
#'     parsnip::proportional_hazards(penalty = 0.05, mixture = 0.5),
#'     "coxnet"
#'   )
#'   fit <- parsnip::fit(spec, survival::Surv(time, status) ~ ., data = lung)
#'   predict(fit, lung[1:3, ], type = "survival", eval_time = c(180, 365))
#' }
coxnet_train <- function(formula, data, penalty = NULL, mixture = NULL, weights = NULL, ...) {
  if (!is.null(weights)) {
    stop("The coxnet engine doesn't support case weights.", call. = FALSE)
  }
  if (is.null(penalty)) {
    stop(
      "The coxnet engine needs a `penalty`, e.g. `proportional_hazards(penalty = 0.05)` ",
      "or `proportional_hazards(penalty = tune())`.",
      call. = FALSE
    )
  }
  coxnet.formula(formula, data, penalty = penalty, mixture = mixture %||% 1, ...)
}

# parsnip's predict() hands `penalty` over as the spec's quosure argument; an
# already-evaluated value passes through unchanged.
coxnet_engine_penalty <- function(penalty) {
  if (rlang::is_quosure(penalty)) rlang::eval_tidy(penalty) else penalty
}

coxnet_engine_predict <- function(object, new_data, type, penalty = NULL, eval_time = NULL) {
  stats::predict(
    object, new_data, type = type, penalty = coxnet_engine_penalty(penalty), eval_time = eval_time
  )
}

#' @rdname coxnet_train
#' @param object A parsnip `model_fit` using the coxnet engine.
#' @param new_data A data frame of new predictors.
#' @param eval_time Times at which to predict survival.
#' @export
predict_coxnet_linear_pred <- function(object, new_data, penalty = NULL) {
  coxnet_engine_predict(object, new_data, "linear_pred", penalty)
}

#' @rdname coxnet_train
#' @export
predict_coxnet_time <- function(object, new_data, penalty = NULL) {
  coxnet_engine_predict(object, new_data, "time", penalty)
}

#' @rdname coxnet_train
#' @export
predict_coxnet_survival <- function(object, new_data, eval_time, penalty = NULL) {
  coxnet_engine_predict(object, new_data, "survival", penalty, eval_time)
}

# ---------------------------------------------------------------------------
# Registration
# ---------------------------------------------------------------------------

register_coxnet_engine <- function() {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  model <- "proportional_hazards"
  mode <- "censored regression"
  registered <- parsnip::get_model_env()[[paste0(model, "_fit")]]
  if (!is.null(registered) && any(registered$engine == "coxnet")) {
    return(invisible(TRUE))
  }

  parsnip::set_model_engine(model, mode, "coxnet")
  for (pkg in c("TempleCBE", "glmnet", "survival")) {
    parsnip::set_dependency(model, "coxnet", pkg, mode = mode)
  }

  parsnip::set_model_arg(
    model = model, eng = "coxnet", parsnip = "penalty", original = "penalty",
    func = list(pkg = "dials", fun = "penalty"), has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = model, eng = "coxnet", parsnip = "mixture", original = "mixture",
    func = list(pkg = "dials", fun = "mixture"), has_submodel = FALSE
  )

  parsnip::set_fit(
    model = model, eng = "coxnet", mode = mode,
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "TempleCBE", fun = "coxnet_train"),
      defaults = list()
    )
  )
  # The formula and data go to coxnet() untouched; hardhat does the encoding.
  parsnip::set_encoding(
    model = model, eng = "coxnet", mode = mode,
    options = list(
      predictor_indicators = "none", compute_intercept = FALSE,
      remove_intercept = FALSE, allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = model, eng = "coxnet", mode = mode, type = "linear_pred",
    value = list(
      pre = NULL,
      post = function(x, object) x$.pred_linear_pred,
      func = c(pkg = "TempleCBE", fun = "predict_coxnet_linear_pred"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        penalty = quote(object$spec$args$penalty)
      )
    )
  )
  parsnip::set_pred(
    model = model, eng = "coxnet", mode = mode, type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "TempleCBE", fun = "predict_coxnet_survival"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        eval_time = quote(eval_time),
        penalty = quote(object$spec$args$penalty)
      )
    )
  )
  parsnip::set_pred(
    model = model, eng = "coxnet", mode = mode, type = "time",
    value = list(
      pre = NULL,
      post = function(x, object) x$.pred_time,
      func = c(pkg = "TempleCBE", fun = "predict_coxnet_time"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        penalty = quote(object$spec$args$penalty)
      )
    )
  )
  invisible(TRUE)
}
