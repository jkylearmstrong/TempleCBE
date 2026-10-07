#' Racing Methods and Controls for Survival Workflows
#'
#' Provides an interface to \pkg{finetune} racing methods (\code{\link[finetune]{tune_race_anova}},
#' \code{\link[finetune]{tune_race_win_loss}}, and \code{\link[finetune]{control_race}}) tailored
#' for Tidymodels survival workflows, workflow sets, and nested cross-validation.
#'
#' Racing evaluates candidate parameter configurations across resample folds sequentially,
#' eliminating unpromising candidates using ANOVA models or Bradley-Terry win-fraction models
#' before evaluating all folds on all grid points. This significantly accelerates hyperparameter
#' optimization for survival models such as \code{\link{coxnet}} and multi-model workflow sets.
#'
#' @name racing_workflows
NULL

#' Control Parameters for Racing Survival Workflows
#'
#' Creates a racing control specification via \code{\link[finetune]{control_race}} with
#' default settings optimized for clinical survival workflows and nested cross-validation.
#'
#' @param save_pred Logical; whether to save out-of-fold assessment predictions (default \code{TRUE}).
#' @param parallel_over How to parallelize execution: \code{"everything"} (default), \code{"resamples"},
#'   or \code{"across"}.
#' @param save_workflow Logical; whether to retain the fitted workflow object in the output (default \code{TRUE}),
#'   required by \code{\link[tune]{fit_best}}.
#' @param burn_in Minimum number of resamples evaluated before candidate configurations can be eliminated
#'   (default 3).
#' @param num_ties Number of bootstrap samples used to evaluate ties in win-fraction racing (default 10).
#' @param alpha Significance level threshold for elimination in ANOVA racing (default 0.05).
#' @param randomize Logical; whether to randomize the order of resamples (default \code{TRUE}).
#' @param ... Additional arguments forwarded to \code{\link[finetune]{control_race}}.
#'
#' @return A \code{control_race} object.
#' @seealso \code{\link{tune_race_survival}}, \code{\link[finetune]{control_race}}
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("finetune", quietly = TRUE)) {
#'   ctrl <- control_race_survival()
#' }
#' }
control_race_survival <- function(save_pred = TRUE,
                                  parallel_over = c("everything", "resamples", "across"),
                                  save_workflow = TRUE,
                                  burn_in = 3,
                                  num_ties = 10,
                                  alpha = 0.05,
                                  randomize = TRUE,
                                  ...) {
  rlang::check_installed("finetune", reason = "for control_race_survival().")
  parallel_over <- match.arg(parallel_over)
  finetune::control_race(
    save_pred = save_pred,
    parallel_over = parallel_over,
    save_workflow = save_workflow,
    burn_in = burn_in,
    num_ties = num_ties,
    alpha = alpha,
    randomize = randomize,
    ...
  )
}

#' @rdname control_race_survival
#' @export
cbe_control_race <- control_race_survival

#' Adaptive Racing Tuning for Survival Workflows and Workflow Sets
#'
#' Tunes hyperparameters of a survival \code{\link[workflows]{workflow}} or
#' \code{\link[workflowsets]{workflow_set}} using \pkg{finetune} racing algorithms
#' (\code{\link[finetune]{tune_race_anova}} or \code{\link[finetune]{tune_race_win_loss}}).
#' When given a \code{workflow_set}, execution is seamlessly mapped across all constituent
#' workflows via \code{\link[workflowsets]{workflow_map}}.
#'
#' Default evaluation metrics are \code{\link[yardstick]{brier_survival_integrated}} and
#' \code{\link[yardstick]{concordance_survival}}.
#'
#' @param object A \code{\link[workflows]{workflow}} or \code{\link[workflowsets]{workflow_set}}
#'   specifying the preprocessor and survival model.
#' @param resamples An \code{rset} resampling object (e.g. from \code{\link[rsample]{vfold_cv}}
#'   or \code{\link[rsample]{group_vfold_cv}}).
#' @param fn Character string specifying the racing function: \code{"tune_race_anova"} (default)
#'   or \code{"tune_race_win_loss"} (which needs \pkg{BradleyTerry2}).
#' @param grid Integer number of candidate tuning parameter combinations (default 20), or an
#'   explicit parameter grid data frame.
#' @param metrics A \code{\link[yardstick]{metric_set}} of survival metrics. If \code{NULL} (default),
#'   defaults to Integrated Brier Score and Concordance.
#' @param eval_time Numeric vector of evaluation time points for dynamic survival metrics. If
#'   \code{NULL} (default), the deciles of the observed event times in the first resample's data
#'   are used, as in \code{\link{cv_coxnet}} and \code{\link{nested_cv_coxnet}}; \pkg{tune}
#'   itself requires at least two evaluation times for the integrated metrics. The outcome is
#'   read from the workflow's formula or recipe (for a \code{workflow_set}, from its first
#'   workflow); pass \code{eval_time} for any other preprocessor.
#' @param control A \code{\link[finetune]{control_race}} object. Defaults to \code{\link{control_race_survival}()}.
#' @param seed Optional random seed integer for reproducible candidate generation and fold processing.
#' @param ... Additional arguments passed to \code{\link[finetune]{tune_race_anova}},
#'   \code{\link[finetune]{tune_race_win_loss}}, or \code{\link[workflowsets]{workflow_map}}.
#'
#' @return If \code{object} is a workflow, a \code{tune_results} / \code{race_results} object.
#'   If \code{object} is a workflow set, a \code{workflow_set} object containing tuning results.
#' @seealso \code{\link{control_race_survival}}, \code{\link[finetune]{tune_race_anova}},
#'   \code{\link[workflowsets]{workflow_map}}
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("finetune", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("parsnip", quietly = TRUE) &&
#'     requireNamespace("workflows", quietly = TRUE) &&
#'     requireNamespace("rsample", quietly = TRUE)) {
#'   lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
#'   spec <- parsnip::set_engine(
#'     parsnip::proportional_hazards(penalty = tune::tune(), mixture = 0.5),
#'     "coxnet"
#'   ) |> parsnip::set_mode("censored regression")
#'   wflow <- workflows::workflow() |>
#'     workflows::add_model(spec) |>
#'     workflows::add_formula(survival::Surv(time, status) ~ age + sex)
#'   folds <- rsample::vfold_cv(lung, v = 4)
#'   res <- tune_race_survival(wflow, resamples = folds, grid = 10, eval_time = c(180, 365))
#'   tune::collect_metrics(res)
#' }
#' }
tune_race_survival <- function(object,
                               resamples,
                               fn = c("tune_race_anova", "tune_race_win_loss"),
                               grid = 20,
                               metrics = NULL,
                               eval_time = NULL,
                               control = NULL,
                               seed = 1503,
                               ...) {
  rlang::check_installed(c("finetune", "tune", "yardstick"), reason = "for tune_race_survival().")
  fn <- match.arg(fn)
  if (fn == "tune_race_win_loss") {
    rlang::check_installed("BradleyTerry2", reason = "for win-loss racing (tune_race_win_loss).")
  }
  call_fn <- fn

  if (is.null(control)) {
    n_resamples <- nrow(resamples)
    burn_in_auto <- min(3, max(2, n_resamples - 1))
    control <- control_race_survival(burn_in = burn_in_auto)
  } else if (!is.null(control$burn_in) && control$burn_in >= nrow(resamples)) {
    control$burn_in <- max(2, nrow(resamples) - 1)
  }

  eval_time <- eval_time %||% race_default_eval_time(object, resamples)

  if (is.null(metrics)) {
    metrics <- yardstick::metric_set(
      yardstick::brier_survival_integrated,
      yardstick::concordance_survival
    )
  }

  if (inherits(object, "workflow_set")) {
    rlang::check_installed("workflowsets", reason = "for racing across a workflow_set.")
    res <- workflowsets::workflow_map(
      object,
      fn = call_fn,
      seed = seed,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      control = control,
      ...
    )
    return(res)
  }

  if (!inherits(object, "workflow")) {
    stop("`object` must be a workflows::workflow or workflowsets::workflow_set object.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (call_fn == "tune_race_anova") {
    finetune::tune_race_anova(
      object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      control = control,
      ...
    )
  } else {
    finetune::tune_race_win_loss(
      object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      control = control,
      ...
    )
  }
}

#' @rdname tune_race_survival
#' @export
cbe_tune_race_survival <- tune_race_survival

#' Default Evaluation Times for Racing, Read From a Workflow's Outcome
#'
#' \pkg{tune} needs at least two evaluation times for the integrated survival metrics but has
#' no default, so a workflow raced without `eval_time` stopped in `check_enough_eval_times()`.
#' The outcome is evaluated from the preprocessor (a formula's left-hand side, or the outcome
#' column of a recipe) in the data of the first resample; the times are the event-time deciles
#' ([default_eval_time()]).
#'
#' @param object A workflow or workflow_set.
#' @param resamples An rset.
#' @return Sorted evaluation times.
#' @keywords internal
#' @noRd
race_default_eval_time <- function(object, resamples) {
  wflow <- if (inherits(object, "workflow_set")) object$info[[1]]$workflow[[1]] else object
  pre <- tryCatch(workflows::extract_preprocessor(wflow), error = function(e) NULL)
  data <- resamples$splits[[1]]$data
  truth <- tryCatch(
    if (inherits(pre, "recipe")) {
      outcome <- pre$var_info$variable[pre$var_info$role == "outcome"]
      data[[outcome[1]]]
    } else if (inherits(pre, "formula")) {
      rlang::eval_tidy(rlang::f_lhs(pre), data = data, env = rlang::f_env(pre))
    },
    error = function(e) NULL
  )
  if (!inherits(truth, "Surv")) {
    stop(
      "Could not read a `Surv()` outcome from the workflow's formula or recipe to choose default ",
      "evaluation times; pass `eval_time`.",
      call. = FALSE
    )
  }
  default_eval_time(truth)
}

#' Racing Needs a Right-Censored Outcome
#'
#' tune and finetune race over parsnip workflows, and parsnip's censored-regression mode
#' accepts only right-censored outcomes, so start/stop (counting-process) data fail deep in
#' the fits ("the allowed censoring type is \"right\"", then every model failed). Stop early
#' with the reason instead.
#'
#' @param type Outcome type from `surv_components()`: `"right"` or `"counting"`.
#' @param tune_method The requested tuning method.
#' @keywords internal
#' @noRd
check_race_outcome_type <- function(type, tune_method) {
  if (identical(type, "counting")) {
    stop(
      "`tune_method = \"", tune_method, "\"` needs a right-censored outcome, Surv(time, status): ",
      "racing runs on parsnip workflows, and parsnip's censored regression cannot fit start/stop ",
      "(counting-process) data. Use the default `tune_method` for start/stop outcomes.",
      call. = FALSE
    )
  }
  invisible(type)
}
