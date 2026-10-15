#' Score Candidate Models on an Outer Split
#'
#' A tidy wrapper around [glmnet_IBS()] using [furrr::future_pmap()] (or sequential
#' [purrr::pmap()]) to evaluate a grid of candidate models on an outer resampling
#' split.
#'
#' @param object An `rsplit` representing the evaluation split (e.g. from
#'   [rsample::initial_split()] or an outer split of [rsample::nested_cv()]).
#' @param grid A data frame or tibble defining candidate models to evaluate.
#'   Must contain an `alpha` column. If a `formula` or `formulas` column is present,
#'   each row's formula is passed to [glmnet_IBS()]. An optional `model` or `model_id`
#'   column will be preserved as the model identifier; if absent, one will be created.
#' @param recipe A recipe object specifying preprocessing.
#' @param ... Additional arguments passed to [glmnet_IBS()] (such as `time_data`,
#'   `internal_folds`, `feature_names`, `id_col`, `cox.ties`, `covariates`, `rule`, `metric`).
#' @param censoring_weights Weighting scheme: `"ipcw"` (default) or `"none"`.
#' @param seed Optional integer seed for reproducible model fitting across future plans.
#' @param progress Show progress bar (default `FALSE`).
#' @return A tibble with one row per model in `grid`, containing model identifier,
#'   `alpha`, `formula` (if provided), `IBS` (integrated Brier score), `lambda`, and
#'   `coefficients` (list column of fitted non-zero coefficient estimates).
#' @seealso [glmnet_IBS()], [compare_censoring_weights()], [select_candidate_models()]
#' @export
score_outer <- function(object,
                        grid,
                        recipe,
                        ...,
                        censoring_weights = c("ipcw", "none"),
                        seed = NULL,
                        progress = FALSE) {
  require_packages(c("glmnet", "survival", "rsample", "yardstick", "recipes"), "score_outer")
  censoring_weights <- match.arg(censoring_weights)
  dots <- list(...)

  if (!inherits(object, "rsplit")) {
    stop("`object` must be an rsplit (e.g. from rsample::initial_split).", call. = FALSE)
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

  if (!is.null(seed)) {
    local_seed(seed)
    row_seeds <- as.list(sample.int(1e9, nrow(grid_df)))
  } else {
    row_seeds <- vector("list", nrow(grid_df))
  }

  score_one_row <- function(row_idx, s_seed) {
    if (!is.null(s_seed)) {
      local_seed(s_seed)
    }
    r <- grid_df[row_idx, , drop = FALSE]
    a <- r$alpha[[1]]
    call_args <- c(
      list(object = object, alpha = a, recipe = recipe, censoring_weights = censoring_weights),
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
      tibble::tibble(term = res$term, estimate = res$estimate)
    } else {
      tibble::tibble(term = character(), estimate = numeric())
    }

    tibble::tibble(
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
      seq_len(nrow(grid_df)),
      row_seeds,
      score_one_row,
      .options = furrr::furrr_options(seed = TRUE),
      .progress = progress
    )
  } else {
    res_list <- purrr::map2(
      seq_len(nrow(grid_df)),
      row_seeds,
      score_one_row
    )
  }

  dplyr::bind_rows(res_list)
}

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
#' @param object An `rsplit` object (e.g. from [rsample::initial_split()]) or a
#'   precomputed data frame scored under `censoring_weights = "none"`.
#' @param grid A candidate model grid (data frame with `alpha` and optional `formula`),
#'   or a precomputed data frame scored under `censoring_weights = "ipcw"` when `object`
#'   is already a scored data frame.
#' @param recipe Preprocessing recipe passed to [score_outer()].
#' @param ... Additional arguments passed to [glmnet_IBS()] through [score_outer()].
#' @param seed Optional integer seed for reproducible draws and fitting.
#' @param progress Show progress bar (default `FALSE`).
#' @return A tibble with columns:
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

    id_cols <- intersect(c("model", "alpha", "formula"), intersect(names(tbl_none), names(tbl_ipcw)))
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
  } else if (inherits(object, "rsplit")) {
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

    id_cols <- intersect(c("model", "alpha", "formula"), names(scored_none))
    out <- dplyr::inner_join(
      dplyr::select(scored_none, dplyr::all_of(c(id_cols, "IBS"))),
      dplyr::select(scored_ipcw, dplyr::all_of(c(id_cols, "IBS"))),
      by = id_cols,
      suffix = c("_none", "_ipcw")
    )
    out <- dplyr::rename(out, ibs_none = .data$IBS_none, ibs_ipcw = .data$IBS_ipcw)
  } else {
    stop("`object` must be an rsplit or a data frame.", call. = FALSE)
  }

  out$rank_none <- rank(out$ibs_none, ties.method = "min", na.last = "keep")
  out$rank_ipcw <- rank(out$ibs_ipcw, ties.method = "min", na.last = "keep")
  out$rank_diff <- out$rank_ipcw - out$rank_none

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
#'   selecting the best alphas with lowest mean IBS.
#' @param top_n Optional integer number of best alphas to keep (overrides `ntile`
#'   if provided).
#' @return A tibble with columns `alpha`, `formula`, and `model`.
#' @seealso [score_outer()], [compare_censoring_weights()], [summarize_tune_results()]
#' @export
select_candidate_models <- function(tune_results,
                                    formulas,
                                    ntile = 0.25,
                                    top_n = NULL) {
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
