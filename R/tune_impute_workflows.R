#' Tune Candidate Workflows to Impute One Numeric Variable
#'
#' Benchmarks every combination of candidate recipes and models for predicting one numeric
#' variable from the others, by cross-validated RMSE, and uses the best one to fill in the
#' variable's missing values. It wraps the `workflowsets::workflow_set()` plus
#' `finetune::tune_race_anova()` scaffold that was repeated for each imputed variable.
#'
#' @section What is tuned on:
#' Only rows where `target` is observed are used for resampling and for the final fit; rows where
#' it is missing are predicted at the end. Predictors may have missing values only if the recipe
#' deals with them (the default recipe imputes them with the median or mode).
#'
#' @section Null model and RSE:
#' `null_rmse` is the cross-validated RMSE of predicting every held-out value with the mean of
#' `target` in the corresponding training folds, and `rse = (rmse / null_rmse)^2` for the best
#' workflow, both from the same folds. An RSE below 1 beats the mean. This is the training-mean
#' baseline of [tune_impute_numeric()], not the oracle baseline of [compare_imputation_rse()].
#'
#' @section Cost and reproducibility:
#' Every workflow is tuned over `grid` candidates on every fold, so the run time grows with
#' `length(recipes) * length(models) * grid * folds`. Racing drops poor candidates after the first
#' three folds. Results depend on `seed` (folds, grids and models that use random numbers).
#'
#' @param data A data frame.
#' @param target Name of the numeric column to impute.
#' @param recipes Named list of `recipes::recipe()` objects with `target` as the outcome, or `NULL`
#'   for one default recipe (all other columns as predictors; median / mode imputation of the
#'   predictors, dummy variables, zero-variance filter).
#' @param models Named list of `parsnip` regression model specifications, or `NULL` for a linear
#'   model (`parsnip::linear_reg()`).
#' @param folds Number of cross-validation folds (default 5), or an `rsample` resampling object
#'   built on the rows of `data` where `target` is observed.
#' @param grid Number of tuning candidates per workflow (default 10), or a grid data frame.
#' @param race If `TRUE` (default), tune with `finetune::tune_race_anova()` (needs at least three
#'   folds); otherwise with `tune::tune_grid()`. Workflows without tuning parameters are simply
#'   resampled.
#' @param seed Random seed (default 123).
#' @param verbose Passed to `workflowsets::workflow_map()`.
#' @return A list of class `cbe_impute_tune` with
#'   \item{ranking}{one row per workflow (its best candidate): `wflow_id`, `rmse`, `std_err`, `n`,
#'     `rank`, ordered best first}
#'   \item{metrics}{RMSE of every candidate of every workflow}
#'   \item{best_workflow_id, best_params}{the winner and its tuning parameter values}
#'   \item{predictions}{out-of-fold predictions of the winner: `Fold`, `.row` (row of `data`),
#'     `observed` and `imputed`, ready for [plot_imputed_vs_observed()]}
#'   \item{rmse, null_rmse, rse}{see Null model and RSE}
#'   \item{fit}{the winner fitted to all rows with an observed `target`}
#'   \item{data}{`data` with the missing values of `target` filled in}
#'   \item{n_imputed, target}{number of filled values, and the target name}
#' @seealso [plot_tune_grid_rmse()], [tbl_tune_best_model()], [plot_imputed_vs_observed()],
#'   [tune_impute_numeric()], [compare_imputation_rse()]
#' @export
#' @examples
#' \donttest{
#' ok <- vapply(c("workflowsets", "finetune", "tune", "rsample", "parsnip", "workflows", "yardstick"),
#'              requireNamespace, logical(1), quietly = TRUE)
#' if (all(ok)) {
#'   d <- mtcars
#'   d$mpg[c(3, 11, 20)] <- NA
#'   res <- tune_impute_workflows(d, "mpg", folds = 3, race = FALSE)
#'   tbl_tune_best_model(res)
#' }
#' }
tune_impute_workflows <- function(data, target, recipes = NULL, models = NULL, folds = 5, grid = 10,
                                  race = TRUE, seed = 123, verbose = FALSE) {
  need <- c("workflowsets", "tune", "rsample", "parsnip", "workflows", "yardstick", if (isTRUE(race)) "finetune")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("tune_impute_workflows() needs package(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(target) || length(target) != 1L || !target %in% names(data)) {
    stop("`target` must name one column of `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[target]])) stop("`target` must be a numeric column.", call. = FALSE)
  named_list <- function(x, arg) {
    if (!is.list(x) || inherits(x, c("recipe", "model_spec")) || is.null(names(x)) || any(!nzchar(names(x))) ||
        anyDuplicated(names(x))) {
      stop("`", arg, "` must be a list with unique names.", call. = FALSE)
    }
    x
  }
  observed <- !is.na(data[[target]])
  train <- as.data.frame(data[observed, , drop = FALSE])
  if (nrow(train) < 10L) stop("At least 10 rows with an observed `", target, "` are needed.", call. = FALSE)

  if (is.null(recipes)) {
    rec <- recipes::recipe(stats::as.formula(paste0("`", target, "` ~ .")), data = train)
    rec <- recipes::step_impute_median(rec, recipes::all_numeric_predictors())
    rec <- recipes::step_impute_mode(rec, recipes::all_nominal_predictors())
    rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
    rec <- recipes::step_zv(rec, recipes::all_predictors())
    recipes <- list(base = rec)
  }
  recipes <- named_list(recipes, "recipes")
  if (is.null(models)) models <- list(lm = parsnip::linear_reg())
  models <- named_list(models, "models")

  if (!is.null(seed)) set.seed(seed)
  if (inherits(folds, "rset")) {
    resamples <- folds
  } else {
    if (!is.numeric(folds) || length(folds) != 1L || folds < 2) stop("`folds` must be a number of at least 2 or an rset.", call. = FALSE)
    resamples <- rsample::vfold_cv(train, v = as.integer(folds))
  }
  if (isTRUE(race) && nrow(resamples) < 3L) stop("Racing needs at least three folds; use `race = FALSE`.", call. = FALSE)

  wf_set <- workflowsets::workflow_set(preproc = recipes, models = models, cross = TRUE)
  metrics <- yardstick::metric_set(yardstick::rmse)
  control <- if (isTRUE(race)) {
    finetune::control_race(save_pred = TRUE, save_workflow = TRUE, verbose = FALSE)
  } else {
    tune::control_grid(save_pred = TRUE, save_workflow = TRUE)
  }
  res <- workflowsets::workflow_map(
    wf_set, fn = if (isTRUE(race)) "tune_race_anova" else "tune_grid", resamples = resamples, grid = grid,
    metrics = metrics, control = control, seed = if (is.null(seed)) sample.int(10^4, 1) else seed, verbose = verbose
  )
  failed <- vapply(res$result, function(r) !inherits(r, "tune_results"), logical(1))
  if (all(failed)) stop("Every workflow failed to tune; check the recipes and models.", call. = FALSE)
  if (any(failed)) {
    warning("Workflow(s) failed and were left out: ", paste(res$wflow_id[failed], collapse = ", "), ".", call. = FALSE)
    res <- res[!failed, ]
  }

  all_metrics <- tibble::as_tibble(workflowsets::collect_metrics(res))
  all_metrics <- all_metrics[all_metrics$.metric == "rmse", , drop = FALSE]
  n_folds <- nrow(resamples)
  # a raced candidate that was dropped early was scored on fewer folds; only complete ones can win
  complete <- all_metrics[all_metrics$n == max(all_metrics$n), , drop = FALSE]
  best_rows <- do.call(rbind, lapply(split(complete, complete$wflow_id), function(m) m[which.min(m$mean), , drop = FALSE]))
  best_rows <- best_rows[order(best_rows$mean), , drop = FALSE]
  ranking <- tibble::tibble(wflow_id = best_rows$wflow_id, .config = best_rows$.config, rmse = best_rows$mean,
                            std_err = best_rows$std_err, n = best_rows$n, rank = seq_len(nrow(best_rows)))
  best_id <- ranking$wflow_id[1]
  best_result <- workflowsets::extract_workflow_set_result(res, best_id)
  cfg_metrics <- tune::collect_metrics(best_result)
  best_params <- cfg_metrics[cfg_metrics$.config == ranking$.config[1] & cfg_metrics$.metric == "rmse", , drop = FALSE]
  best_params <- best_params[setdiff(names(best_params), c(".metric", ".estimator", "mean", "n", "std_err", ".iter"))]

  preds <- tune::collect_predictions(best_result)
  preds <- preds[preds$.config == ranking$.config[1], , drop = FALSE]
  fold_col <- intersect(c("id", "id2"), names(preds))
  predictions <- tibble::tibble(
    Fold = if (length(fold_col)) do.call(paste, c(as.list(preds[fold_col]), sep = " ")) else "all",
    .row = which(observed)[preds$.row],
    observed = preds[[target]],
    imputed = preds$.pred
  )

  # mean of the analysis rows of each fold, scored on its assessment rows: the same cells as `predictions`
  null_sq <- unlist(lapply(resamples$splits, function(s) {
    (rsample::assessment(s)[[target]] - mean(rsample::analysis(s)[[target]]))^2
  }))
  null_rmse <- sqrt(mean(null_sq))
  rmse <- sqrt(mean((predictions$observed - predictions$imputed)^2))

  final <- workflowsets::extract_workflow(res, best_id)
  if (ncol(best_params) > 1L) final <- tune::finalize_workflow(final, best_params)
  fit <- parsnip::fit(final, data = train)
  out_data <- data
  n_imputed <- sum(!observed)
  if (n_imputed > 0L) {
    out_data[[target]][!observed] <- stats::predict(fit, new_data = as.data.frame(data[!observed, , drop = FALSE]))$.pred
  }

  out <- list(
    ranking = ranking, metrics = all_metrics, best_workflow_id = best_id, best_params = best_params,
    predictions = predictions, rmse = rmse, null_rmse = null_rmse,
    rse = if (null_rmse > 0) (rmse / null_rmse)^2 else NA_real_,
    fit = fit, data = out_data, n_imputed = n_imputed, target = target, n_folds = n_folds
  )
  class(out) <- c("cbe_impute_tune", "list")
  out
}

.check_impute_tune <- function(x) {
  if (!inherits(x, "cbe_impute_tune")) stop("`x` must be a tune_impute_workflows() result.", call. = FALSE)
  invisible(x)
}

#' Plot and Tabulate the Workflows of an Imputation Tuning Run
#'
#' `plot_tune_grid_rmse()` shows the cross-validated RMSE (mean and one standard error) of every
#' tuning candidate, grouped by workflow, with the null-model RMSE as a dashed line.
#' `tbl_tune_best_model()` lists the best candidate of each workflow with its RMSE and its RSE
#' against the null model.
#'
#' @param x A [tune_impute_workflows()] result.
#' @param digits Decimals in the table.
#' @return `plot_tune_grid_rmse()`: a `ggplot2` object. `tbl_tune_best_model()`: a tibble with `rank`,
#'   `workflow`, `rmse`, `std_err`, `n_folds`, `null_rmse`, `rse` and `selected`.
#' @seealso [tune_impute_workflows()], [plot_imputed_vs_observed()]
#' @export
plot_tune_grid_rmse <- function(x) {
  .check_impute_tune(x)
  m <- as.data.frame(x$metrics)
  m$wflow_id <- factor(m$wflow_id, levels = rev(x$ranking$wflow_id))
  m$std_err[is.na(m$std_err)] <- 0
  m$complete <- ifelse(m$n == max(m$n), "All folds", "Dropped by racing")
  ggplot2::ggplot(m, ggplot2::aes(x = .data$mean, y = .data$wflow_id, color = .data$complete)) +
    ggplot2::geom_vline(xintercept = x$null_rmse, linetype = "dashed", color = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$mean - .data$std_err, xmax = .data$mean + .data$std_err),
                           width = 0.2, orientation = "y", position = ggplot2::position_jitter(height = 0.2, width = 0, seed = 1)) +
    ggplot2::geom_point(size = 2, position = ggplot2::position_jitter(height = 0.2, width = 0, seed = 1)) +
    ggplot2::scale_color_manual(values = c(`All folds` = "#9D2235", `Dropped by racing` = "grey65"), name = NULL) +
    theme_cbe() +
    ggplot2::labs(title = paste0("Cross-validated RMSE of imputation workflows: ", x$target),
                  x = "RMSE (mean \u00b1 SE over folds; dashed: null model)", y = "Workflow")
}

#' @rdname plot_tune_grid_rmse
#' @export
tbl_tune_best_model <- function(x, digits = 3) {
  .check_impute_tune(x)
  r <- x$ranking
  tibble::tibble(
    rank = r$rank,
    workflow = r$wflow_id,
    rmse = round(r$rmse, digits),
    std_err = round(r$std_err, digits),
    n_folds = r$n,
    null_rmse = round(x$null_rmse, digits),
    rse = round(if (x$null_rmse > 0) (r$rmse / x$null_rmse)^2 else NA_real_, digits),
    selected = r$wflow_id == x$best_workflow_id
  )
}
