#' Score Candidate Models on Outer Resampling Split(s)
#'
#' A tidy wrapper around [glmnet_IBS()] using [furrr::future_map()] (or sequential
#' [purrr::map()]) to evaluate a grid of candidate models across one or more outer
#' resampling splits.
#'
#' @param object An `rsplit` (e.g. from [rsample::initial_split()]), an `rset` /
#'   [rsample::nested_cv()] object containing outer splits, or a list of `rsplit`
#'   objects.
#' @param grid A data frame or tibble defining candidate models to evaluate.
#'   Must contain an `alpha` column. If a `formula` or `formulas` column is present,
#'   each row's formula is passed to [glmnet_IBS()]. An optional `model` or `model_id`
#'   column will be preserved as the model identifier; if absent, one will be created.
#' @param recipe A recipe object specifying preprocessing.
#' @param ... Additional arguments passed to [glmnet_IBS()] (such as `time_data`,
#'   `internal_folds`, `feature_names`, `id_col`, `cox.ties`, `covariates`, `rule`,
#'   `metric`, `tune_engine`).
#' @param censoring_weights Weighting scheme: `"ipcw"` (default) or `"none"`.
#' @param coefficients Option controlling which coefficients to store in the
#'   `coefficients` list column: `"all"` (default, includes predictors shrunk to 0)
#'   or `"nonzero"` (filters to non-zero estimates).
#' @param seed Optional integer seed for reproducible model fitting across future plans.
#' @param progress Show progress bar (default `FALSE`).
#' @return A tibble with columns `outer_fold`, `model`, `alpha`, `formula` (if provided),
#'   `IBS` (integrated Brier score), `lambda`, and `coefficients` (list column of
#'   coefficient estimates).
#' @seealso [glmnet_IBS()], [tidy_outer_coefficients()], [compare_censoring_weights()], [select_candidate_models()]
#' @export
score_outer <- function(object,
                        grid,
                        recipe,
                        ...,
                        censoring_weights = c("ipcw", "none"),
                        coefficients = c("all", "nonzero"),
                        seed = NULL,
                        progress = FALSE) {
  require_packages(c("glmnet", "survival", "rsample", "yardstick", "recipes"), "score_outer")
  censoring_weights <- match.arg(censoring_weights)
  coefficients <- match.arg(coefficients)
  dots <- list(...)

  # Resolve splits and fold identifiers
  splits <- list()
  fold_names <- character()

  if (inherits(object, "rsplit")) {
    splits <- list(object)
    fold_names <- "split"
  } else if (inherits(object, "nested_cv") || inherits(object, "rset")) {
    splits <- object$splits
    if ("id" %in% names(object)) {
      if ("id2" %in% names(object)) {
        fold_names <- paste(object$id, object$id2, sep = "_")
      } else {
        fold_names <- as.character(object$id)
      }
    } else {
      fold_names <- paste0("Fold", seq_along(splits))
    }
  } else if (is.list(object) && length(object) > 0 && all(vapply(object, inherits, logical(1), "rsplit"))) {
    splits <- object
    fold_names <- if (!is.null(names(object)) && all(nzchar(names(object)))) {
      names(object)
    } else {
      paste0("Fold", seq_along(splits))
    }
  } else {
    stop("`object` must be an rsplit, a nested_cv/rset object, or a list of rsplits.", call. = FALSE)
  }

  if (!is.data.frame(grid) || nrow(grid) == 0L) {
    stop("`grid` must be a non-empty data frame.", call. = FALSE)
  }
  if (!"alpha" %in% names(grid)) {
    stop("`grid` must contain an 'alpha' column.", call. = FALSE)
  }

  grid_df <- tibble::as_tibble(grid)
  if (!"model" %in% names(grid_df)) {
    if ("model_id" %in% names(grid_df)) {
      grid_df$model <- grid_df$model_id
    } else if ("candidate_id" %in% names(grid_df)) {
      grid_df$model <- grid_df$candidate_id
    } else if ("formula" %in% names(grid_df)) {
      grid_df$model <- paste0("alpha=", round(grid_df$alpha, 3), "; ", grid_df$formula)
    } else {
      grid_df$model <- paste0("model_", seq_len(nrow(grid_df)))
    }
  }

  # Build cartesian tasks: candidate models x outer splits
  tasks <- expand.grid(
    row_idx = seq_len(nrow(grid_df)),
    split_idx = seq_along(splits),
    stringsAsFactors = FALSE
  )
  n_tasks <- nrow(tasks)

  if (!is.null(seed)) {
    local_seed(seed)
    task_seeds <- as.list(sample.int(1e9, n_tasks))
  } else {
    task_seeds <- vector("list", n_tasks)
  }

  score_one_task <- function(task_idx, s_seed) {
    if (!is.null(s_seed)) {
      local_seed(s_seed)
    }
    t_row <- tasks[task_idx, ]
    r <- grid_df[t_row$row_idx, , drop = FALSE]
    curr_split <- splits[[t_row$split_idx]]
    curr_fold <- fold_names[t_row$split_idx]

    a <- r$alpha[[1]]
    call_args <- c(
      list(object = curr_split, alpha = a, recipe = recipe, censoring_weights = censoring_weights),
      dots
    )
    if ("formula" %in% names(r)) {
      call_args$formula <- r$formula[[1]]
    } else if ("formulas" %in% names(r)) {
      call_args$formula <- r$formulas[[1]]
    }
    if (!"feature_names" %in% names(call_args)) {
      if ("feature_names" %in% names(r)) {
        call_args$feature_names <- r$feature_names[[1]]
      } else if (!is.null(call_args$formula)) {
        call_args$feature_names <- trimws(strsplit(formula_terms_string(call_args$formula), "\\+")[[1]])
      } else if (inherits(recipe, "recipe") && !is.null(recipe$var_info)) {
        roles <- recipe$var_info$role
        call_args$feature_names <- recipe$var_info$variable[!roles %in% c("id variable", "outcome")]
      }
    }

    res <- do.call(glmnet_IBS, call_args)

    has_coefs <- all(c("term", "estimate") %in% names(res))
    coef_tbl <- if (has_coefs && nrow(res) > 0) {
      if (coefficients == "nonzero") {
        tibble::tibble(term = res$term[res$estimate != 0], estimate = res$estimate[res$estimate != 0])
      } else {
        tibble::tibble(term = res$term, estimate = res$estimate)
      }
    } else {
      tibble::tibble(term = character(), estimate = numeric())
    }

    tibble::tibble(
      outer_fold = curr_fold,
      model = r$model[[1]],
      alpha = a,
      formula = if ("formula" %in% names(r)) as.character(r$formula[[1]]) else NA_character_,
      IBS = if (nrow(res) > 0) res$IBS[1] else NA_real_,
      lambda = if (nrow(res) > 0) res$lambda[1] else NA_real_,
      coefficients = list(coef_tbl)
    )
  }

  can_furrr <- requireNamespace("furrr", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)

  if (can_furrr) {
    res_list <- furrr::future_map2(
      seq_len(n_tasks),
      task_seeds,
      score_one_task,
      .options = furrr::furrr_options(seed = TRUE),
      .progress = progress
    )
  } else {
    res_list <- purrr::map2(
      seq_len(n_tasks),
      task_seeds,
      score_one_task
    )
  }

  dplyr::bind_rows(res_list)
}

#' Tidy Outer Model Coefficients into Long Format
#'
#' Unnests the `coefficients` list column produced by [score_outer()] into a long
#' tibble with one row per term per model and fold. Suitable for plotting variable
#' importance, coefficient paths, and term stability across outer folds.
#'
#' @param x A tibble returned by [score_outer()].
#' @return A long tibble containing `outer_fold` (if present), `model`, `alpha`,
#'   `formula` (if present), `IBS`, `lambda`, `term`, and `estimate`.
#' @seealso [score_outer()]
#' @export
tidy_outer_coefficients <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame returned by score_outer().", call. = FALSE)
  }
  if (!"coefficients" %in% names(x)) {
    stop("`x` must contain a 'coefficients' list column.", call. = FALSE)
  }

  res_list <- lapply(seq_len(nrow(x)), function(i) {
    row_meta <- x[i, setdiff(names(x), "coefficients"), drop = FALSE]
    coefs <- x$coefficients[[i]]
    if (is.null(coefs) || !is.data.frame(coefs) || nrow(coefs) == 0L) {
      tibble::tibble(row_meta, term = character(), estimate = numeric())
    } else {
      meta_expanded <- row_meta[rep(1, nrow(coefs)), , drop = FALSE]
      meta_expanded$term <- coefs$term
      meta_expanded$estimate <- coefs$estimate
      meta_expanded
    }
  })
  dplyr::bind_rows(res_list)
}

#' @rdname tidy_outer_coefficients
#' @export
unnest_outer_coefficients <- tidy_outer_coefficients

#' Compare Model Performance Under Unweighted and IPCW Censoring Weights
#'
#' Evaluates candidate models on an outer resampling split under both `"none"`
#' (unweighted legacy scoring) and `"ipcw"` (proper inverse probability of censoring
#' weighted scoring), returning a tidy side-by-side comparison table of integrated
#' Brier scores (IBS) and model ranks, along with rank agreement statistics
#' (Spearman rank correlation and Kendall's tau).
#'
#' Alternatively, accepts two precomputed tables of model evaluations (one under
#' `"none"` and one under `"ipcw"`) and merges them into the comparison table.
#'
#' @param object An `rsplit`, `nested_cv`, list of `rsplit`s, or a
#'   precomputed data frame scored under `censoring_weights = "none"`.
#' @param grid A candidate model grid (data frame with `alpha` and optional `formula`),
#'   or a precomputed data frame scored under `censoring_weights = "ipcw"` when `object`
#'   is already a scored data frame.
#' @param recipe Preprocessing recipe passed to [score_outer()].
#' @param ... Additional arguments passed to [glmnet_IBS()] through [score_outer()].
#' @param seed Optional integer seed for reproducible draws and fitting.
#' @param progress Show progress bar (default `FALSE`).
#' @return A tibble with columns:
#'   \item{outer_fold}{Outer fold identifier (if present)}
#'   \item{model}{Model identifier}
#'   \item{alpha}{Elastic net mixing parameter}
#'   \item{formula}{Formula specification (if present)}
#'   \item{ibs_none}{Integrated Brier score under `censoring_weights = "none"`}
#'   \item{ibs_ipcw}{Integrated Brier score under `censoring_weights = "ipcw"`}
#'   \item{rank_none}{Model rank under unweighted IBS (1 = best / lowest IBS)}
#'   \item{rank_ipcw}{Model rank under IPCW IBS (1 = best / lowest IBS)}
#'   \item{rank_diff}{`rank_ipcw - rank_none`}
#'
#' The returned object has attribute `"rank_agreement"` containing a named list with
#' `spearman` (Spearman's rank correlation) and `kendall` (Kendall's rank correlation).
#' @seealso [score_outer()], [glmnet_IBS()], [select_candidate_models()]
#' @export
compare_censoring_weights <- function(object,
                                      grid,
                                      recipe = NULL,
                                      ...,
                                      seed = NULL,
                                      progress = FALSE) {
  if (is.data.frame(object) && is.data.frame(grid)) {
    # Pre-scored tables mode
    tbl_none <- tibble::as_tibble(object)
    tbl_ipcw <- tibble::as_tibble(grid)

    ibs_col_none <- if ("ibs_none" %in% names(tbl_none)) "ibs_none" else if ("IBS" %in% names(tbl_none)) "IBS" else stop("First table must contain 'IBS' or 'ibs_none'.", call. = FALSE)
    ibs_col_ipcw <- if ("ibs_ipcw" %in% names(tbl_ipcw)) "ibs_ipcw" else if ("IBS" %in% names(tbl_ipcw)) "IBS" else stop("Second table must contain 'IBS' or 'ibs_ipcw'.", call. = FALSE)

    id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), intersect(names(tbl_none), names(tbl_ipcw)))
    if (length(id_cols) == 0) {
      tbl_none$model <- paste0("model_", seq_len(nrow(tbl_none)))
      tbl_ipcw$model <- paste0("model_", seq_len(nrow(tbl_ipcw)))
      id_cols <- "model"
    }

    sub_none <- dplyr::select(tbl_none, dplyr::all_of(c(id_cols, ibs_col_none)))
    sub_none <- dplyr::rename(sub_none, ibs_none = !!ibs_col_none)

    sub_ipcw <- dplyr::select(tbl_ipcw, dplyr::all_of(c(id_cols, ibs_col_ipcw)))
    sub_ipcw <- dplyr::rename(sub_ipcw, ibs_ipcw = !!ibs_col_ipcw)

    out <- dplyr::inner_join(sub_none, sub_ipcw, by = id_cols)
  } else if (inherits(object, "rsplit") || inherits(object, "nested_cv") || inherits(object, "rset") || (is.list(object) && length(object) > 0 && all(vapply(object, inherits, logical(1), "rsplit")))) {
    if (is.null(recipe)) {
      stop("`recipe` is required when scoring on an rsplit `object`.", call. = FALSE)
    }
    # Direct evaluation mode
    scored_none <- score_outer(
      object, grid, recipe = recipe, ...,
      censoring_weights = "none", seed = seed, progress = progress
    )
    scored_ipcw <- score_outer(
      object, grid, recipe = recipe, ...,
      censoring_weights = "ipcw", seed = seed, progress = progress
    )

    id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), names(scored_none))
    out <- dplyr::inner_join(
      dplyr::select(scored_none, dplyr::all_of(c(id_cols, "IBS"))),
      dplyr::select(scored_ipcw, dplyr::all_of(c(id_cols, "IBS"))),
      by = id_cols,
      suffix = c("_none", "_ipcw")
    )
    out <- dplyr::rename(out, ibs_none = .data$IBS_none, ibs_ipcw = .data$IBS_ipcw)
  } else {
    stop("`object` must be an rsplit, nested_cv, list of rsplits, or a data frame.", call. = FALSE)
  }

  if ("outer_fold" %in% names(out)) {
    out <- dplyr::group_by(out, .data$outer_fold)
    out <- dplyr::mutate(
      out,
      rank_none = rank(.data$ibs_none, ties.method = "min", na.last = "keep"),
      rank_ipcw = rank(.data$ibs_ipcw, ties.method = "min", na.last = "keep"),
      rank_diff = .data$rank_ipcw - .data$rank_none
    )
    out <- dplyr::ungroup(out)
  } else {
    out$rank_none <- rank(out$ibs_none, ties.method = "min", na.last = "keep")
    out$rank_ipcw <- rank(out$ibs_ipcw, ties.method = "min", na.last = "keep")
    out$rank_diff <- out$rank_ipcw - out$rank_none
  }

  n_valid <- sum(!is.na(out$ibs_none) & !is.na(out$ibs_ipcw))
  spearman_rho <- if (n_valid >= 2) stats::cor(out$ibs_none, out$ibs_ipcw, method = "spearman", use = "complete.obs") else NA_real_
  kendall_tau <- if (n_valid >= 2) stats::cor(out$ibs_none, out$ibs_ipcw, method = "kendall", use = "complete.obs") else NA_real_

  attr(out, "rank_agreement") <- list(
    spearman = spearman_rho,
    kendall = kendall_tau
  )

  out
}

#' Select Candidate Models From Tuning Results
#'
#' Takes results from [summarize_tune_results()] or [tune_over_alpha()], identifies
#' the best-performing `alpha` values (e.g. within the top `ntile` of integrated Brier
#' scores), and crosses them with candidate formulas to construct an outer evaluation
#' grid.
#'
#' @param tune_results A data frame containing tuning results with at least `alpha`
#'   and `IBS` columns (e.g. from [summarize_tune_results()]).
#' @param formulas Character vector of formulas, list of formula objects, or a single
#'   formula to evaluate.
#' @param ntile Numeric quantile cutoff (between 0 and 1, default `0.25`) for
#'   selecting the best alphas with lowest IBS.
#' @param top_n Optional integer number of best alphas to keep (overrides `ntile`
#'   if provided).
#' @param by Selection granularity: `"row"` (default, selects alphas appearing in the
#'   top `ntile` of individual tune result rows, e.g. `ntile(IBS, 5) == 1`) or `"alpha"`
#'   (summarizes mean IBS across all rows per alpha before selecting).
#' @return A tibble with columns `model`, `alpha`, and `formula`.
#' @seealso [score_outer()], [compare_censoring_weights()], [summarize_tune_results()]
#' @export
select_candidate_models <- function(tune_results,
                                    formulas,
                                    ntile = 0.25,
                                    top_n = NULL,
                                    by = c("row", "alpha")) {
  by <- match.arg(by)
  if (!is.data.frame(tune_results) || nrow(tune_results) == 0L) {
    stop("`tune_results` must be a non-empty data frame.", call. = FALSE)
  }
  if (!all(c("alpha", "IBS") %in% names(tune_results))) {
    stop("`tune_results` must contain 'alpha' and 'IBS' columns.", call. = FALSE)
  }

  if (inherits(formulas, "formula")) {
    formulas <- list(formulas)
  }
  clean_formulas <- vapply(formulas, formula_terms_string, character(1), USE.NAMES = FALSE)

  if (by == "row") {
    ordered <- dplyr::arrange(tune_results, .data$IBS)
    selected_alphas <- if (!is.null(top_n)) {
      unique_alphas <- unique(ordered$alpha)
      head(unique_alphas, top_n)
    } else {
      n_bins <- if (ntile > 1) as.integer(ntile) else max(1L, as.integer(round(1 / ntile)))
      is_top_tile <- dplyr::ntile(tune_results$IBS, n_bins) == 1L
      sort(unique(tune_results$alpha[which(is_top_tile)]))
    }
  } else {
    # Summarize mean IBS per alpha
    alpha_summary <- dplyr::group_by(tune_results, .data$alpha)
    alpha_summary <- dplyr::summarize(alpha_summary, mean_ibs = mean(.data$IBS, na.rm = TRUE), .groups = "drop")
    alpha_summary <- dplyr::arrange(alpha_summary, .data$mean_ibs)

    selected_alphas <- if (!is.null(top_n)) {
      head(alpha_summary$alpha, top_n)
    } else {
      cutoff <- stats::quantile(alpha_summary$mean_ibs, probs = ntile, na.rm = TRUE)
      alpha_summary$alpha[alpha_summary$mean_ibs <= cutoff]
    }
  }

  grid <- expand.grid(
    alpha = selected_alphas,
    formula = clean_formulas,
    stringsAsFactors = FALSE
  )

  tibble::tibble(
    model = paste0("alpha=", round(grid$alpha, 3), "; ", grid$formula),
    alpha = grid$alpha,
    formula = grid$formula
  )
}
