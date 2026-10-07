#' Explain Survival Models via survex and DALEX
#'
#' Adapts Tidymodels survival models (\code{workflows}, \code{parsnip} / \code{censored},
#' \code{coxnet_model}, and \code{joint_model}) to create a \code{survex} survival explainer.
#' Standardizes survival predictions into \eqn{N \times K} survival curves, bridging the
#' gap identified in survex issue #98.
#'
#' @param model A fitted model object: \code{joint_model}, \code{coxnet_model},
#'   \code{cv_coxnet}, \code{workflow}, or \code{model_fit}.
#' @param data A data frame containing predictor columns used to calculate explanations.
#'   If \code{NULL} and \code{model} is a \code{joint_model}, the training predictors are used,
#'   as the original columns (e.g. a factor stays one column, not its one-hot dummies), which is
#'   what the model's \code{predict()} takes.
#' @param y A survival object or response variable (e.g. \code{Surv(time, status)}). If
#'   \code{NULL} and \code{model} is a \code{joint_model}, the original survival object is used.
#' @param predict_survival_function Optional custom prediction function returning an
#'   \eqn{N \times K} data frame or matrix of survival probabilities \eqn{S(t)}.
#' @param predict_risk_function Optional custom prediction function returning a numeric
#'   vector of risk scores or cumulative hazard.
#' @param times A numeric vector of evaluation time points. If \code{NULL}, defaults to
#'   unique event times in \code{y}.
#' @param label Character string naming the model in plots and summaries.
#' @param verbose Logical; if \code{TRUE}, progress messages are shown. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{survex::explain_survival()}.
#'
#' @return A \code{survex_explainer} object compatible with \code{survex::model_parts()},
#'   \code{survex::model_performance()}, and \code{survex::predict_parts()}.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survex", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
#'   lung <- survival::lung
#'   df <- na.omit(lung[, c("time", "status", "age", "sex")])
#'   fit <- joint_model(df, survival::Surv(time, status) ~ age + sex)
#'   expl <- cbe_explain_survival(fit)
#'   mp <- survex::model_parts(expl)
#' }
#' }
cbe_explain_survival <- function(model,
                                 data = NULL,
                                 y = NULL,
                                 predict_survival_function = NULL,
                                 predict_risk_function = NULL,
                                 times = NULL,
                                 label = NULL,
                                 verbose = FALSE,
                                 ...) {
  if (!requireNamespace("survex", quietly = TRUE)) {
    stop("Package 'survex' is required for cbe_explain_survival(). Please install it.", call. = FALSE)
  }

  # 1. Resolve model object, data, and y
  target_model <- model
  target_data <- data
  target_y <- y

  if (inherits(model, "joint_model")) {
    target_model <- model$coxnet_model
    if (is.null(target_data)) {
      # The original columns, not the one-hot dummies: the coxnet sub-model
      # forges its predictors from the original ones.
      target_data <- joint_raw_predictors(model)
    }
    if (is.null(target_y)) {
      target_y <- model$components$surv_obj
    }
    if (is.null(label)) {
      label <- paste0("Joint Model (", model$engine, ")")
    }
  }

  if (is.null(target_data)) {
    stop("`data` must be supplied if `model` is not a `joint_model` containing components.", call. = FALSE)
  }

  if (is.null(target_y)) {
    stop("`y` must be supplied (e.g. Surv(time, status)) if not provided in `model`.", call. = FALSE)
  }

  if (inherits(target_y, "Surv") && identical(attr(target_y, "type"), "counting")) {
    stop(
      "survex explains one row per subject with a right-censored outcome, but `y` is a ",
      "start/stop Surv(start, stop, event) with one row per interval. Explain each subject at ",
      "baseline instead: pass `data` with one row per subject (their first interval) and ",
      "`y = surv_subject_truth(y, subject_id)$.truth`.",
      call. = FALSE
    )
  }

  if (is.null(times)) {
    if (inherits(target_y, "Surv")) {
      surv_mat <- as.matrix(target_y)
      # 2-parameter Surv has cols (time, status); 3-parameter has (start, stop, status)
      time_col <- if (ncol(surv_mat) == 3) surv_mat[, 2] else surv_mat[, 1]
      status_col <- if (ncol(surv_mat) == 3) surv_mat[, 3] else surv_mat[, 2]
      event_times <- sort(unique(time_col[status_col == 1]))
      if (length(event_times) > 0) {
        times <- event_times
      } else {
        times <- sort(unique(time_col))
      }
    } else {
      times <- seq(0, 100, length.out = 20)
    }
  }

  # 2. Build default survival prediction function for Tidymodels / Coxnet objects
  default_times <- times
  if (is.null(predict_survival_function)) {
    predict_survival_function <- function(m, new_data, times = NULL, ...) {
      eval_times <- times
      if (is.null(eval_times)) eval_times <- default_times
      eval_times <- sort(unique(eval_times))

      # Reshape a `.pred` list-column (one tibble per row, each with
      # .eval_time/.pred_survival) into a row-per-observation data frame
      # aligned to eval_times.
      surv_list_to_df <- function(surv_list) {
        mat <- lapply(surv_list, function(df) {
          df <- df[match(eval_times, df$.eval_time), ]
          df$.pred_survival
        })
        as.data.frame(do.call(rbind, mat))
      }

      # Case A: coxnet_model / cv_coxnet
      if (inherits(m, c("coxnet_model", "cv_coxnet"))) {
        preds <- stats::predict(m, new_data = new_data, type = "survival", eval_time = eval_times)
        return(surv_list_to_df(preds$.pred))
      }

      # Case B: Tidymodels workflow or parsnip model_fit
      if (inherits(m, c("workflow", "model_fit"))) {
        preds <- stats::predict(m, new_data = new_data, type = "survival", eval_time = eval_times)
        if (".pred" %in% names(preds) && is.list(preds$.pred)) {
          return(surv_list_to_df(preds$.pred))
        }
      }

      # Case C: base survival::coxph or similar
      if (inherits(m, "coxph")) {
        sf <- survival::survfit(m, newdata = new_data)
        s_summary <- summary(sf, times = eval_times, extend = TRUE)
        return(as.data.frame(t(s_summary$surv)))
      }

      # Fallback generic survival predict
      preds <- tryCatch({
        stats::predict(m, new_data = new_data, type = "survival", eval_time = eval_times)
      }, error = function(e) NULL)

      if (!is.null(preds) && ".pred" %in% names(preds)) {
        return(surv_list_to_df(preds$.pred))
      }

      stop("Could not automatically infer survival prediction for model class: ",
           paste(class(m), collapse = ", "), ". Please supply `predict_survival_function`.", call. = FALSE)
    }
  }

  # 3. Build default risk prediction function
  if (is.null(predict_risk_function)) {
    predict_risk_function <- function(m, new_data, times = NULL, ...) {
      # Try linear predictor first: `explain_predict_risk()` for the model
      # classes whose sign it knows, then the generic tidymodels linear_pred.
      lp <- tryCatch({
        p <- explain_predict_risk(m, new_data)
        if (!is.null(p)) {
          p
        } else {
          p <- stats::predict(m, new_data = new_data, type = "linear_pred")
          if (ncol(p) > 0) as.numeric(p[[1]]) else NULL
        }
      }, error = function(e) NULL)

      if (!is.null(lp)) return(lp)

      # Try predicting restricted mean survival time with negation (A2-15)
      p_time <- tryCatch({
        p <- stats::predict(m, new_data = new_data, type = "time")
        if (ncol(p) > 0) {
          warning("Risk score sign was inferred by negating `predict(type = 'time')`.", call. = FALSE)
          -as.numeric(p[[1]])
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(p_time)) return(p_time)

      # Fallback to 1 - median survival
      med_t <- stats::median(default_times, na.rm = TRUE)
      s_df <- predict_survival_function(m, new_data, times = med_t)
      warning("Risk score was inferred as 1 - S(t) at median evaluation time.", call. = FALSE)
      1 - as.numeric(s_df[[1]])
    }
  }

  if (is.null(label)) {
    label <- paste(class(target_model)[1], "Survival Explainer")
  }

  survex::explain_survival(
    model = target_model,
    data = target_data,
    y = target_y,
    predict_survival_function = predict_survival_function,
    predict_risk_function = predict_risk_function,
    times = times,
    label = label,
    verbose = verbose,
    ...
  )
}

# Risk scores in survex's convention (higher means higher risk) for the model
# classes whose linear predictor's sign is known; NULL for any other class.
# `predict.coxnet_model()` flips the sign by default (larger = longer survival,
# as in censored), so glmnet's own sign is requested explicitly. `predict.coxph()`
# has no `new_data` argument, and would silently score the training data.
# Fitted parsnip models and workflows return tidymodels' censored-regression
# linear predictor, which by default increases with survival time (the opposite
# sign of the engine's own), so a risk score is its negative. Engines may also
# differ from `predict()` by a constant (censored's "survival" engine doesn't
# center the linear predictor), which doesn't change a risk ranking.
explain_predict_risk <- function(m, new_data) {
  if (inherits(m, c("coxnet_model", "cv_coxnet"))) {
    p <- stats::predict(m, new_data = new_data, type = "linear_pred", increasing = FALSE)
    return(as.numeric(p[[1]]))
  }
  if (inherits(m, "coxph")) {
    return(as.numeric(stats::predict(m, newdata = new_data, type = "lp")))
  }
  if (inherits(m, c("model_fit", "workflow"))) {
    p <- stats::predict(m, new_data = new_data, type = "linear_pred")
    return(-as.numeric(p[[1]]))
  }
  NULL
}

#' TempleCBE Custom Loss Functions for survex
#'
#' Provides survival loss functions implementing TempleCBE's inverse-probability-of-censoring
#' weighted (IPCW) Graf Integrated Brier Score and time-dependent Brier Score for use with
#' \code{survex::model_parts()} and \code{survex::model_performance()}.
#'
#' @param normalization Optional normalization parameter for \code{survex} integral calculation.
#'   Can be \code{NULL}, \code{"t_max"}, or \code{"survival"}.
#' @param max_quantile Upper quantile cutoff for evaluation time window. Default is 1.
#' @param trunc Lower truncation threshold for censoring survival probabilities to prevent
#'   inflation by rare tails. Default is 0.05.
#'
#' @return A loss function with attributes \code{"loss_type"} (\code{"integrated"} or
#'   \code{"time-dependent"}) and \code{"loss_name"}, directly passable to \code{survex}.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survex", quietly = TRUE)) {
#'   loss_ibs <- cbe_survex_loss_ibs()
#'   # mp <- survex::model_parts(explainer, loss_function = loss_ibs)
#' }
#' }
cbe_survex_loss_ibs <- function(normalization = c("none", "t_max", "survival"), max_quantile = 1, trunc = 0.05) {
  if (is.null(normalization)) normalization <- "none"
  normalization <- match.arg(normalization)

  loss_fn <- function(y_true = NULL, risk = NULL, surv = NULL, times = NULL) {
    if (is.null(times)) {
      times <- sort(unique(y_true[, 1]))
    }

    # Ensure times and surv columns are sorted together
    if (is.data.frame(surv)) surv <- as.matrix(surv)
    if (is.unsorted(times)) {
      ord <- order(times)
      times <- times[ord]
      if (!is.null(surv) && ncol(surv) == length(times)) {
        surv <- surv[, ord, drop = FALSE]
      }
    }

    # Filter to requested quantile
    q_val <- stats::quantile(y_true[, 1], probs = max_quantile, na.rm = TRUE)
    mask <- times <= q_val
    times_sub <- times[mask]

    # Ensure surv matrix aligns with times_sub
    if (length(times_sub) < 2) {
      times_sub <- times
      surv_sub <- surv
    } else if (!is.null(surv) && ncol(surv) == length(times)) {
      surv_sub <- surv[, mask, drop = FALSE]
    } else {
      surv_sub <- surv
    }

    # Format survival response
    surv_obj <- if (inherits(y_true, "Surv")) {
      y_true
    } else if (is.matrix(y_true) && ncol(y_true) == 2) {
      survival::Surv(y_true[, 1], y_true[, 2])
    } else if (is.matrix(y_true) && ncol(y_true) == 3) {
      survival::Surv(y_true[, 1], y_true[, 2], y_true[, 3])
    } else {
      survival::Surv(y_true[, 1], y_true[, 2])
    }

    # Fit censoring KM on training truth using TempleCBE's reverse KM estimator
    cens_km <- censoring_km(surv_obj)

    # Use TempleCBE's Graf IPCW calculation
    ibs_val <- score_surv_matrix_ibs(
      surv_matrix = surv_sub,
      eval_time = times_sub,
      surv_truth = surv_obj,
      cens_km = cens_km,
      trunc = trunc
    )

    if (normalization == "t_max" && length(times_sub) >= 2) {
      ibs_val <- ibs_val * max(times_sub) / diff(range(times_sub))
    }

    ibs_val
  }

  attr(loss_fn, "loss_type") <- "integrated"
  attr(loss_fn, "loss_name") <- "TempleCBE Integrated Brier Score"
  loss_fn
}

#' @rdname cbe_survex_loss_ibs
#' @export
cbe_survex_loss_brier <- function(trunc = 0.05) {
  loss_fn <- function(y_true = NULL, risk = NULL, surv = NULL, times = NULL) {
    if (is.null(times)) {
      times <- sort(unique(y_true[, 1]))
    } else {
      times <- sort(unique(times))
    }

    if (is.data.frame(surv)) surv <- as.matrix(surv)

    surv_obj <- if (inherits(y_true, "Surv")) {
      y_true
    } else {
      survival::Surv(y_true[, 1], y_true[, 2])
    }

    # Graf IPCW weights, from the reverse Kaplan-Meier estimate of censoring.
    # (Looking G up with summary.survfit(times = ) doesn't work here: it sorts
    # `times`, so the weights came back in time order, not subject order.)
    parts <- surv_components(surv_obj)
    weights <- graf_weights(surv_obj, times, censoring_km(surv_obj), trunc)
    alive <- outer(parts$stop, times, ">") * 1

    unname(colSums(weights * (alive - surv)^2)) / length(parts$stop)
  }

  attr(loss_fn, "loss_type") <- "time-dependent"
  attr(loss_fn, "loss_name") <- "TempleCBE Brier Score"
  loss_fn
}

#' Explain Joint Model Components via DALEX and survex
#'
#' Extracts and creates explainers for all components of a \code{joint_model}:
#' the survival model (via \code{survex}), the binary status model (via \code{DALEX}),
#' and the continuous duration time model (via \code{DALEX}).
#'
#' @param model A fitted \code{joint_model} object.
#' @param data Evaluation data frame with the original predictor columns (before one-hot
#'   encoding; extra columns are ignored). If \code{NULL}, the training predictors are used, as
#'   the original columns. The status and time explainers re-apply the model's training encoding
#'   with \code{\link[hardhat]{forge}}, as \code{\link{predict.joint_model}} does, so a factor
#'   predictor is one column in the explainers too.
#' @param type Character indicating which component(s) to explain: \code{"all"},
#'   \code{"survival"}, \code{"status"}, \code{"time"}, or \code{"stack"}.
#' @param times Numeric vector of evaluation time points for survival explanations.
#' @param label Optional custom label prefix.
#' @param verbose Logical; if \code{TRUE}, progress messages are shown. Default is \code{FALSE}.
#' @param ... Additional arguments passed to the underlying explainers.
#'
#' @return If \code{type = "all"}, a list of class \code{"cbe_joint_explainer"} containing
#'   \code{$survival}, \code{$status}, \code{$time}, and (if present) \code{$stack} explainers.
#'   Otherwise, the individual requested explainer.
#' @export
cbe_explain <- function(model,
                        data = NULL,
                        type = c("all", "survival", "status", "time", "stack"),
                        times = NULL,
                        label = NULL,
                        verbose = FALSE,
                        ...) {
  type <- match.arg(type)
  if (!inherits(model, "joint_model")) {
    stop("`model` must be a `joint_model` object.", call. = FALSE)
  }

  eval_data <- if (is.null(data)) joint_raw_predictors(model) else data

  # The status and time models are fit on the one-hot predictors: raw data goes
  # through the same forge() + predictors-matrix path as predict.joint_model().
  joint_sub_predictions <- function(new_data) {
    joint_status_time_predictions(model, hardhat::forge(new_data, model$components$blueprint)$predictors)
  }

  # 1. Survival Explainer
  expl_surv <- NULL
  if (type %in% c("all", "survival")) {
    expl_surv <- cbe_explain_survival(
      model = model,
      data = eval_data,
      times = times,
      label = if (is.null(label)) "Joint Survival" else paste(label, "(Survival)"),
      verbose = verbose,
      ...
    )
    if (type == "survival") return(expl_surv)
  }

  # 2. Status Explainer (DALEX)
  expl_status <- NULL
  if (type %in% c("all", "status")) {
    if (!requireNamespace("DALEX", quietly = TRUE)) {
      stop("Package 'DALEX' is required for explaining status models.", call. = FALSE)
    }

    status_target <- model$components$status
    pred_status_fn <- function(m, new_data) {
      joint_sub_predictions(new_data)$status
    }

    expl_status <- DALEX::explain(
      model = model$status_model,
      data = eval_data,
      y = status_target,
      predict_function = pred_status_fn,
      label = if (is.null(label)) "Joint Status (Classification)" else paste(label, "(Status)"),
      verbose = verbose
    )
    if (type == "status") return(expl_status)
  }

  # 3. Time Explainer (DALEX)
  expl_time <- NULL
  if (type %in% c("all", "time")) {
    if (!requireNamespace("DALEX", quietly = TRUE)) {
      stop("Package 'DALEX' is required for explaining time models.", call. = FALSE)
    }

    time_target <- model$components$time
    pred_time_fn <- function(m, new_data) {
      joint_sub_predictions(new_data)$time
    }

    expl_time <- DALEX::explain(
      model = model$time_model,
      data = eval_data,
      y = time_target,
      predict_function = pred_time_fn,
      label = if (is.null(label)) "Joint Time (Duration)" else paste(label, "(Time)"),
      verbose = verbose
    )
    if (type == "time") return(expl_time)
  }

  # 4. Stack Explainer (Meta-Learner via survex)
  expl_stack <- NULL
  if ((type == "stack" || (type == "all" && !is.null(model$stack_model))) && !is.null(model$stack_model)) {
    pred_stack_risk <- function(m, new_data) {
      p <- stats::predict(model, new_data = new_data)
      if (!is.null(p$.pred_stack_risk_score)) p$.pred_stack_risk_score else p$.pred_risk_score
    }
    expl_stack <- survex::explain_survival(
      model = model$stack_model,
      data = eval_data,
      y = model$components$surv_obj,
      predict_risk_function = pred_stack_risk,
      label = if (is.null(label)) "Joint Stack (Meta-Learner)" else paste(label, "(Stack)"),
      verbose = verbose
    )
    if (type == "stack") return(expl_stack)
  }

  res <- list(
    survival = expl_surv,
    status = expl_status,
    time = expl_time,
    stack = expl_stack,
    model = model
  )
  class(res) <- c("cbe_joint_explainer", "list")
  res
}

#' @rdname cbe_explain
#' @export
cbe_explain_status <- function(model, data = NULL, label = NULL, verbose = FALSE, ...) {
  cbe_explain(model = model, data = data, type = "status", label = label, verbose = verbose, ...)
}

#' @rdname cbe_explain
#' @export
cbe_explain_time <- function(model, data = NULL, label = NULL, verbose = FALSE, ...) {
  cbe_explain(model = model, data = data, type = "time", label = label, verbose = verbose, ...)
}

#' @rdname cbe_explain
#' @export
cbe_explain_stack <- function(model, data = NULL, label = NULL, verbose = FALSE, ...) {
  cbe_explain(model = model, data = data, type = "stack", label = label, verbose = verbose, ...)
}

#' @rdname cbe_explain
#' @export
explain_joint <- cbe_explain

#' @rdname cbe_explain
#' @export
explain_status <- cbe_explain_status

#' @rdname cbe_explain
#' @export
explain_time <- cbe_explain_time

#' @rdname cbe_explain
#' @export
explain_stack <- cbe_explain_stack

#' @rdname cbe_explain_survival
#' @export
explain_survival <- cbe_explain_survival

#' @rdname cbe_survex_loss_ibs
#' @export
survex_loss_ibs <- cbe_survex_loss_ibs

#' @rdname cbe_survex_loss_ibs
#' @export
survex_loss_brier <- cbe_survex_loss_brier

#' SHAP Variable Attributions across Survival and Joint Models
#'
#' Computes individual observation SHAP attributions: using time-dependent
#' \code{SurvSHAP(t)} via \code{survex::predict_parts(type = "survshap")} for survival
#' explainers, and \code{DALEX::predict_parts(type = "shap")} for classification/regression
#' explainers.
#'
#' @param explainer An explainer object produced by \code{cbe_explain_survival()},
#'   \code{cbe_explain()}, or \code{DALEX::explain()}.
#' @param new_observation A 1-row data frame containing the observation to explain.
#' @param type Character specifying the attribution type: \code{"auto"} (the default),
#'   \code{"shap"}, or \code{"survshap"}.
#' @param ... Additional arguments passed to \code{survex::predict_parts()} or
#'   \code{DALEX::predict_parts()}.
#'
#' @return A \code{predict_parts} attribution object, printable and plottable with \code{plot()}.
#' @export
cbe_predict_parts_shap <- function(explainer,
                                   new_observation,
                                   type = c("auto", "shap", "survshap"),
                                   ...) {
  type <- match.arg(type)

  if (inherits(explainer, "cbe_joint_explainer")) {
    # Compute attributions across all components
    res <- list(
      survival = cbe_predict_parts_shap(explainer$survival, new_observation, type = "survshap", ...),
      status = cbe_predict_parts_shap(explainer$status, new_observation, type = "shap", ...),
      time = cbe_predict_parts_shap(explainer$time, new_observation, type = "shap", ...)
    )
    if (!is.null(explainer$stack)) {
      res$stack <- tryCatch(
        cbe_predict_parts_shap(explainer$stack, new_observation, type = "survshap", ...),
        error = function(e) NULL
      )
    }
    class(res) <- c("cbe_joint_predict_parts", "list")
    return(res)
  }

  if (inherits(explainer, c("surv_explainer", "survex_explainer"))) {
    if (!requireNamespace("survex", quietly = TRUE)) {
      stop("Package 'survex' is required for SurvSHAP attributions.", call. = FALSE)
    }
    # survex only supports "survshap" for survival explainers; "auto" and
    # "shap" both resolve to it here (plain "shap" is DALEX's classification/
    # regression attribution, handled in the `explainer` branch below).
    return(survex::predict_parts(
      explainer = explainer,
      new_observation = new_observation,
      type = "survshap",
      ...
    ))
  }

  if (inherits(explainer, "explainer")) {
    if (!requireNamespace("DALEX", quietly = TRUE)) {
      stop("Package 'DALEX' is required for SHAP attributions.", call. = FALSE)
    }
    return(DALEX::predict_parts(
      explainer = explainer,
      new_observation = new_observation,
      type = "shap",
      ...
    ))
  }

  stop("`explainer` must be a `survex_explainer`, `cbe_joint_explainer`, or `DALEX` explainer.", call. = FALSE)
}

#' @rdname cbe_predict_parts_shap
#' @export
predict_parts_shap <- cbe_predict_parts_shap
