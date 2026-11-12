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
  if (!is.logical(race) || length(race) != 1L || is.na(race)) {
    stop("`race` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) || seed != round(seed) || abs(seed) > .Machine$integer.max)) {
    stop("`seed` must be a single whole number or NULL.", call. = FALSE)
  }
  # `grid` is passed straight to workflow_map(): a non-integer, a data frame
  # with no rows, or anything else would fail deep inside the tuning run.
  if (is.numeric(grid)) {
    if (length(grid) != 1L || is.na(grid) || grid < 1 || grid != round(grid)) {
      stop("`grid` must be a single whole number of at least 1, or a grid data frame.", call. = FALSE)
    }
  } else if (is.data.frame(grid)) {
    if (nrow(grid) < 1L) stop("`grid` must have at least one row.", call. = FALSE)
  } else {
    stop("`grid` must be a single whole number of at least 1, or a grid data frame.", call. = FALSE)
  }
  if (!inherits(folds, "rset") &&
      (!is.numeric(folds) || length(folds) != 1L || is.na(folds) || !is.finite(folds) ||
       folds < 2 || folds != round(folds))) {
    stop("`folds` must be a whole number of at least 2 or an rset.", call. = FALSE)
  }
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
    # A user-supplied rset must be built on the observed-target rows: `.row`
    # is mapped back with `which(observed)[preds$.row]`, so folds built on the
    # full data silently mis-attribute predictions. A fold built on the full
    # data has assessment/analysis rows whose target is missing, or row
    # indices outside the observed-rows frame.
    for (s in resamples$splits) {
      in_id <- s$in_id
      if (!is.null(in_id) && (anyNA(in_id) || any(in_id < 1L) || any(in_id > nrow(train)))) {
        stop("`folds` (rset) must be built on the rows of `data` where `target` is observed: ",
             "a split has row indices outside those rows.", call. = FALSE)
      }
      # The checks above are heuristic: a split built on the full data
      # whose indices happen to fall in range and whose rows happen to
      # have observed targets would pass them. A partition-style split
      # (vfold, validation split, manual rset) covers every row exactly
      # once, so its analysis + assessment rows must total nrow(train);
      # a full-data rset totals nrow(data) instead. Bootstrap splits
      # resample rows with replacement, so their totals legitimately
      # exceed nrow(train) and are exempt (their in_id has duplicates).
      # Rolling-origin splits are time windows, not partitions: they cover
      # fewer rows than nrow(train) until the last split, so they are
      # exempt from the total (their indices were range-checked above).
      if (!inherits(resamples, "rolling_origin") && (is.null(in_id) || !anyDuplicated(in_id))) {
        n_split <- nrow(rsample::analysis(s)) + nrow(rsample::assessment(s))
        out_id <- s$out_id
        if (is.numeric(out_id) && !is.null(in_id) && !all(is.na(out_id)) &&
            length(intersect(in_id, out_id)) > 0L) {
          stop("`folds` (rset) must be built on the rows of `data` where `target` is observed: ",
               "a split's analysis and assessment rows overlap.", call. = FALSE)
        }
        if (n_split != nrow(train)) {
          stop("`folds` (rset) must be built on the rows of `data` where `target` is observed: ",
               "a split covers ", n_split, " rows, not ", nrow(train), ".", call. = FALSE)
        }
      }
      for (part in list(rsample::analysis(s), rsample::assessment(s))) {
        if (target %in% names(part) && anyNA(part[[target]])) {
          stop("`folds` (rset) must be built on the rows of `data` where `target` is observed: ",
               "a split contains rows whose `target` is missing.", call. = FALSE)
        }
      }
    }
  } else {
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
  # With no RMSE rows at all, `max(all_metrics$n)` is -Inf and the best-row /
  # ranking chain below fails cryptically (empty `best_rows`, `best_id` NA).
  if (nrow(all_metrics) == 0L) {
    stop("No RMSE metrics were collected for any workflow; the tuning run produced nothing to rank.",
         call. = FALSE)
  }
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
    ggplot2::scale_color_manual(values = c(`All folds` = "#A41E35", `Dropped by racing` = "grey65"), name = NULL) +
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

#' Tune Imputation for a Whole Data Set
#'
#' Extends [tune_impute_workflows()] — which benchmarks recipes and models
#' for predicting **one** numeric variable — to a whole data set, so the user
#' does not have to loop it per variable. With `engine = "workflows"`
#' (default), each incomplete numeric column is tuned in turn with
#' [tune_impute_workflows()] (same `recipes`, `models`, `folds`, `grid` and
#' `race`, with the seed advanced by one per variable so each variable's
#' folds are reproducible and distinct), and the tuned imputations are
#' combined into one completed data set. With `engine = "missforest"` or
#' `"missranger"`, the whole data set is instead imputed by sweeping `mtry`
#' with [missforest_sweep_mtry()] / [missranger_sweep_mtry()], which already
#' operate on full data but only tune `mtry`; extra arguments (`mtry_values`,
#' `ntree` / `num.trees`, `parallel`, `max_pct_missing`, `checkpoint_dir`,
#' `shard`, ...) are passed through to the sweep.
#'
#' @section How the per-variable fits combine:
#' Each variable is tuned against the **original** data (the other columns as
#' they stand, with the recipe's own median/mode imputation of predictors),
#' not against already-imputed values, so the result does not depend on the
#' order variables are tuned in. The completed data takes each tuned
#' variable's column from its own fit. Imputations are **not chained**: an
#' earlier target's tuned imputation is never used as a predictor for a later
#' target, and no sequential (chained-equations) imputation is intended here.
#' A variable that is complete, or that is not numeric, is never a target for
#' the `"workflows"` engine; the forest engines impute every column they are
#' given (numeric, factor and character) except `exclude`d ones.
#'
#' @section Forest engines impute the whole data set:
#' With `engine = "missforest"` or `"missranger"`, the sweep
#' ([missforest_sweep_mtry()] / [missranger_sweep_mtry()]) is run on the
#' whole data frame, not on `targets` alone, so `$data` has **every**
#' incomplete column filled in — not only the columns named in `targets` —
#' except columns in `exclude` (and columns held out by the sweep itself for
#' exceeding `max_pct_missing`). `$summary` still reports one row per target
#' only, so a non-target column that was filled is not listed there. When
#' `targets` is `NULL` (the default) this is invisible, because every
#' incomplete column is then a target; it only matters when `targets` names a
#' subset. If only the target columns should be filled, use the
#' `"workflows"` engine, or pass the non-target columns in `exclude`.
#'
#' @param data A data frame.
#' @param targets Character vector of columns to impute, or `NULL` (default)
#'   for every incomplete numeric column ( `"workflows"` engine) or every
#'   incomplete column (`"missforest"` / `"missranger"` engines). For the
#'   forest engines `targets` only selects which columns are summarised in
#'   `$summary`: the sweep imputes the whole data set, so non-target
#'   incomplete columns are filled in `$data` as well (see the Forest engines
#'   section).
#' @param engine `"workflows"`, `"missforest"` or `"missranger"`.
#' @param exclude Character vector of columns to hold out of the imputation
#'   entirely (identifiers, time columns). Passed through to the sweep for
#'   the forest engines; for the `"workflows"` engine the columns are simply
#'   never targets and are never used as predictors either (they are dropped
#'   from the tuning data).
#' @param recipes,models,folds,grid,race,seed,verbose Passed to
#'   [tune_impute_workflows()] for the `"workflows"` engine.
#' @param ... For the forest engines, passed to [missforest_sweep_mtry()] /
#'   [missranger_sweep_mtry()] (for example `mtry_values`, `ntree`,
#'   `num.trees`, `parallel`, `max_pct_missing`, `checkpoint_dir`, `shard`).
#' @return A list of class `impute_tune_dataset` with
#'   \item{per_variable}{named list of per-variable [tune_impute_workflows()]
#'     results (`"workflows"` engine), or the single sweep result
#'     (`"missforest"` / `"missranger"` engines, under `sweep`)}
#'   \item{summary}{one row per imputed variable: `variable`, and for the
#'     `"workflows"` engine `best_workflow_id`, `rmse`, `null_rmse`, `rse`
#'     and `n_imputed`; for the forest engines `best_mtry` and `best_error`}
#'   \item{data}{`data` with every target column filled in; for the forest
#'     engines, every incomplete column except `exclude`d ones (see the
#'     Forest engines section)}
#'   \item{engine, targets}{the engine used and the variables imputed}
#' @seealso [tune_impute_workflows()], [missforest_sweep_mtry()],
#'   [missranger_sweep_mtry()]
#' @param x The object to print (the result of the function).
#' @export
#' @examples
#' \donttest{
#' ok <- vapply(c("workflowsets", "finetune", "tune", "rsample", "parsnip", "workflows", "yardstick"),
#'              requireNamespace, logical(1), quietly = TRUE)
#' if (all(ok)) {
#'   d <- mtcars
#'   d$mpg[c(3, 11, 20)] <- NA
#'   d$hp[c(5, 15)] <- NA
#'   res <- tune_impute_dataset(d, folds = 3, race = FALSE)
#'   res$summary
#' }
#' }
tune_impute_dataset <- function(data, targets = NULL, engine = c("workflows", "missforest", "missranger"),
                                exclude = NULL, recipes = NULL, models = NULL, folds = 5, grid = 10,
                                race = TRUE, seed = 123, verbose = FALSE, ...) {
  engine <- match.arg(engine)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  # Validate `seed` up front, with the same check tune_impute_workflows()
  # uses, so a non-numeric seed fails here instead of cryptically at
  # `seed + i - 1L` in the per-variable loop (or deep inside a sweep).
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) || seed != round(seed) || abs(seed) > .Machine$integer.max)) {
    stop("`seed` must be a single whole number or NULL.", call. = FALSE)
  }
  if (!is.null(exclude) && (!is.character(exclude) || any(!exclude %in% names(data)))) {
    stop("`exclude` must name columns of `data`.", call. = FALSE)
  }
  if (!is.null(targets) && (!is.character(targets) || length(targets) == 0L || anyDuplicated(targets) ||
                            any(!targets %in% names(data)))) {
    stop("`targets` must name columns of `data`, without duplicates.", call. = FALSE)
  }
  if (!is.null(targets) && length(intersect(targets, exclude)) > 0L) {
    stop("`targets` and `exclude` must not overlap.", call. = FALSE)
  }

  if (identical(engine, "workflows")) {
    if (is.null(targets)) {
      targets <- names(data)[vapply(data, function(z) is.numeric(z) && anyNA(z), logical(1))]
      targets <- setdiff(targets, exclude)
    } else if (any(!vapply(data[targets], is.numeric, logical(1)))) {
      stop("The \"workflows\" engine imputes numeric columns only: non-numeric target(s): ",
           paste(targets[!vapply(data[targets], is.numeric, logical(1))], collapse = ", "), ".", call. = FALSE)
    }
    if (length(targets) == 0L) {
      stop("No incomplete numeric columns to impute.", call. = FALSE)
    }
    # Fail before fitting anything when a target cannot be tuned at all.
    too_few <- targets[vapply(data[targets], function(z) sum(!is.na(z)) < 10L, logical(1))]
    if (length(too_few) > 0L) {
      stop("Target(s) need at least 10 observed rows: ", paste(too_few, collapse = ", "), ".", call. = FALSE)
    }
    tune_data <- if (length(exclude) > 0L) data[, setdiff(names(data), exclude), drop = FALSE] else data
    per_variable <- list()
    out_data <- data
    for (i in seq_along(targets)) {
      tg <- targets[i]
      res <- tune_impute_workflows(
        tune_data, tg, recipes = recipes, models = models, folds = folds, grid = grid,
        race = race, seed = if (is.null(seed)) NULL else seed + i - 1L, verbose = verbose
      )
      per_variable[[tg]] <- res
      out_data[[tg]] <- res$data[[tg]]
    }
    summary <- tibble::tibble(
      variable = targets,
      best_workflow_id = vapply(per_variable, function(r) r$best_workflow_id, character(1)),
      rmse = vapply(per_variable, function(r) r$rmse, numeric(1)),
      null_rmse = vapply(per_variable, function(r) r$null_rmse, numeric(1)),
      rse = vapply(per_variable, function(r) r$rse, numeric(1)),
      n_imputed = vapply(per_variable, function(r) r$n_imputed, integer(1))
    )
    out <- list(per_variable = per_variable, summary = summary, data = out_data,
                engine = engine, targets = targets)
  } else {
    if (is.null(targets)) {
      targets <- names(data)[vapply(data, anyNA, logical(1))]
      targets <- setdiff(targets, exclude)
    }
    if (length(targets) == 0L) {
      stop("No incomplete columns to impute.", call. = FALSE)
    }
    sweep_fn <- if (identical(engine, "missforest")) missforest_sweep_mtry else missranger_sweep_mtry
    sweep <- sweep_fn(data, exclude = exclude, seed = seed, ...)
    best <- sweep$best
    summary <- tibble::tibble(
      variable = targets,
      best_mtry = vapply(targets, function(v) {
        hit <- best$mtry[best$column == v]
        if (length(hit) == 0L) NA_real_ else hit[1]
      }, numeric(1)),
      best_error = vapply(targets, function(v) {
        hit <- best$error[best$column == v]
        if (length(hit) == 0L) NA_real_ else hit[1]
      }, numeric(1))
    )
    out <- list(per_variable = list(sweep = sweep), summary = summary, data = sweep$imp_data,
                engine = engine, targets = targets)
  }
  class(out) <- c("impute_tune_dataset", "list")
  out
}

#' @rdname tune_impute_dataset
#' @export
print.impute_tune_dataset <- function(x, ...) {
  cat("<impute_tune_dataset> engine: ", x$engine,
      "; imputed ", length(x$targets), " variable(s): ",
      paste(x$targets, collapse = ", "), "\n", sep = "")
  print(x$summary, n = nrow(x$summary))
  invisible(x)
}
