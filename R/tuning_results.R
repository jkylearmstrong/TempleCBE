#' Tuning Parameters of a Ranked Workflow in a Workflow Set
#'
#' Ranks the workflows of tuned workflow set results by `rank_metric`, each
#' workflow represented by its best configuration, and returns the
#' preprocessor, model, and best tuning parameters of the workflow at position
#' `.rank`.
#'
#' @param workflowset_results Tuned workflow set results, the output of
#'   [workflowsets::workflow_map()].
#' @param rank_metric Name of the metric to rank by, e.g. `"rmse"` or
#'   `"brier_survival_integrated"`.
#' @param .rank Position to return; 1 (default) is the best workflow.
#' @return A one-row tibble with `wflow_id`, `preproc`, `model`, and one column
#'   per tuning parameter.
#' @seealso [fit_n_rank()] to also fit the chosen configuration.
#' @export
#' @examples
#' \dontrun{
#' results <- workflowsets::workflow_map(wset, "tune_grid", resamples = folds, grid = 5)
#' get_model_parameters(results, "rmse")
#' get_model_parameters(results, "rmse", .rank = 2)
#' }
get_model_parameters <- function(workflowset_results, rank_metric, .rank = 1) {
  require_packages(c("workflowsets", "tune"), "get_model_parameters")
  ranked <- workflowsets::rank_results(workflowset_results, rank_metric = rank_metric, select_best = TRUE)
  ranked <- dplyr::filter(ranked, .data$.metric == rank_metric, .data$rank == .rank)
  if (nrow(ranked) == 0) {
    stop("No workflow at rank ", .rank, " for metric '", rank_metric, "'.", call. = FALSE)
  }

  wflow_id <- ranked$wflow_id[1]
  tune_res <- workflowsets::extract_workflow_set_result(workflowset_results, id = wflow_id)
  best <- tune::select_best(tune_res, metric = rank_metric)
  dplyr::bind_cols(workflow_info(workflowset_results, wflow_id), dplyr::select(best, -".config"))
}

#' Fit the Configuration Ranked `.rank` in a Workflow Set
#'
#' Ranks the configurations of tuned workflow set results by `rank_metric`,
#' picks the one at position `.rank`, and fits it on the full training set
#' with [tune::fit_best()].
#'
#' @param grid_results Tuned workflow set results, the output of
#'   [workflowsets::workflow_map()] run with
#'   `control = tune::control_grid(save_workflow = TRUE)`, which
#'   [tune::fit_best()] needs.
#' @param rank_metric Name of the metric to rank by.
#' @param .rank Position to fit; 1 (default) is the best.
#' @param group_wflow If `TRUE` (default), rank workflows, each by its best
#'   configuration, so `.rank = 2` is the second-best *workflow*. If `FALSE`,
#'   rank configurations directly, so `.rank = 2` can be a second configuration
#'   of the best workflow.
#' @param select_best Passed to [workflowsets::rank_results()]: keep only each
#'   workflow's best configuration before ranking.
#' @param ... Passed to [tune::fit_best()].
#' @return A list with `model`, the fitted workflow, and `tuned_parameters`, a
#'   one-row tibble of the workflow's `wflow_id`, `preproc`, `model`, tuning
#'   parameters, and its [workflowsets::rank_results()] row (`.config`,
#'   `.metric`, `mean`, `std_err`, `n`, `rank`, and `overall_rank` when
#'   `group_wflow = TRUE`).
#' @seealso [get_model_parameters()]
#' @export
#' @examples
#' \dontrun{
#' results <- workflowsets::workflow_map(
#'   wset, "tune_grid", resamples = folds, grid = 5,
#'   control = tune::control_grid(save_workflow = TRUE)
#' )
#' best <- fit_n_rank(results, rank_metric = "rmse")
#' best$tuned_parameters
#' predict(best$model, new_data)
#' }
fit_n_rank <- function(grid_results,
                       rank_metric,
                       .rank = 1,
                       group_wflow = TRUE,
                       select_best = FALSE,
                       ...) {
  require_packages(c("workflowsets", "tune"), "fit_n_rank")
  rankings <- workflowsets::rank_results(grid_results, rank_metric = rank_metric, select_best = select_best)
  rankings <- dplyr::filter(rankings, .data$.metric == rank_metric)

  if (group_wflow) {
    rankings <- dplyr::group_by(rankings, .data$wflow_id)
    rankings <- dplyr::filter(rankings, dplyr::row_number() == 1)
    rankings <- dplyr::ungroup(rankings)
    rankings$overall_rank <- rankings$rank
    rankings$rank <- seq_len(nrow(rankings))
  }

  chosen <- dplyr::filter(rankings, .data$rank == .rank)
  if (nrow(chosen) == 0) {
    stop("No configuration at rank ", .rank, " for metric '", rank_metric, "'.", call. = FALSE)
  }
  chosen <- chosen[1, ]

  tune_res <- workflowsets::extract_workflow_set_result(grid_results, id = chosen$wflow_id)
  params <- config_parameters(tune_res, chosen$.config)

  tuned_parameters <- dplyr::bind_cols(workflow_info(grid_results, chosen$wflow_id), params)
  tuned_parameters <- dplyr::left_join(
    tuned_parameters, chosen,
    by = intersect(names(tuned_parameters), names(chosen))
  )

  model <- if (ncol(params) > 0) {
    tune::fit_best(tune_res, parameters = params, ...)
  } else {
    tune::fit_best(tune_res, metric = rank_metric, ...)
  }

  list(model = model, tuned_parameters = tuned_parameters)
}

#' The `wflow_id`, `preproc` and `model` of One Workflow in a Set
#'
#' @keywords internal
#' @noRd
workflow_info <- function(workflowset_results, wflow_id) {
  rows <- tibble::as_tibble(workflowset_results)
  rows <- rows[rows$wflow_id == wflow_id, c("wflow_id", "info")]
  rows <- tidyr::unnest(rows, "info")
  dplyr::select(rows, "wflow_id", "preproc", "model")
}

#' Tuning Parameter Values of One Configuration
#'
#' @return A one-row tibble with one column per tuning parameter (zero columns
#'   when nothing was tuned).
#' @keywords internal
#' @noRd
config_parameters <- function(tune_res, config) {
  param_names <- tune::.get_tune_parameter_names(tune_res)
  metrics <- tune::collect_metrics(tune_res)
  metrics <- metrics[metrics$.config == config, param_names, drop = FALSE]
  tibble::as_tibble(utils::head(dplyr::distinct(metrics), 1))
}
