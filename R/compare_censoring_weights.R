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
#' @param checkpoint_dir Optional directory path to save per-task checkpoint files.
#'   Finished tasks are loaded from disk on rerun when their fingerprint matches,
#'   skipping recomputation.
#' @param checkpoint_mismatch Action when a checkpoint file in `checkpoint_dir`
#'   has a mismatched fingerprint: `"recompute"` (default; warns and recomputes),
#'   `"overwrite"` (recomputes and overwrites mismatched checkpoint files in
#'   place without warning; can also be enabled via
#'   `options(templecbe.checkpoint_mismatch = "overwrite")` or
#'   `options(templecbe.checkpoint_overwrite = TRUE)`), or `"error"` (stops
#'   immediately).
#' @param shard Optional integer vector `c(i, n)` specifying that this machine
#'   should evaluate shard `i` of `n` into `checkpoint_dir`. Use
#'   [collect_checkpoints()] to merge all shards once finished.
#' @param chunk_size Optional integer number of tasks per worker chunk passed to
#'   [furrr::furrr_options()] (default `NULL`, which lets \pkg{furrr} divide
#'   tasks evenly into one chunk per worker).
#' @param on_complete Optional callback `function(result)` invoked when the run
#'   finishes or errors (defaults to `getOption("templecbe.on_complete", NULL)`).
#' @param progress Show progress bar (default `FALSE`).
#' @return A tibble with columns `outer_fold`, `model`, `alpha`, `formula` (if provided),
#'   `IBS` (integrated Brier score), `lambda`, and `coefficients` (list column of
#'   coefficient estimates).
#' @seealso [glmnet_IBS()], [tidy_outer_coefficients()], [summarize_outer_importance()],
#'   [plot_outer_importance()], [collect_checkpoints()], [clean_checkpoints()],
#'   [compare_censoring_weights()], [select_candidate_models()]
#' @export
score_outer <- function(object,
                        grid,
                        recipe,
                        ...,
                        censoring_weights = c("ipcw", "none"),
                        coefficients = c("all", "nonzero"),
                        seed = NULL,
                        checkpoint_dir = NULL,
                        checkpoint_mismatch = c("recompute", "overwrite", "error"),
                        shard = NULL,
                        chunk_size = NULL,
                        on_complete = getOption("templecbe.on_complete", NULL),
                        progress = FALSE) {
  require_packages(c("glmnet", "survival", "rsample", "yardstick", "recipes"), "score_outer")
  censoring_weights <- match.arg(censoring_weights)
  coefficients <- match.arg(coefficients)
  checkpoint_mismatch <- resolve_checkpoint_mismatch(
    checkpoint_mismatch,
    missing_arg = missing(checkpoint_mismatch)
  )
  shard <- validate_shard_spec(shard)
  if (!is.null(chunk_size)) {
    chunk_size <- as.integer(chunk_size)
    if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
      stop("`chunk_size` must be a positive integer or NULL.", call. = FALSE)
    }
  }
  dots <- list(...)

  # Resolve splits and fold identifiers
  splits <- list()
  fold_names <- character()

  if (inherits(object, "rsplit")) {
    splits <- list(object)
    fold_names <- "split"
  } else if (inherits(object, "nested_cv") || inherits(object, "rset") || (is.data.frame(object) && "splits" %in% names(object) && is.list(object$splits))) {
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

  run_with_on_complete(
    fn_name = "score_outer",
    on_complete = on_complete,
    meta = list(n_tasks = n_tasks, checkpoint_dir = checkpoint_dir, shard = shard, seed = seed),
    expr = {
      if (!is.null(seed)) {
        local_seed(seed)
        task_seeds <- as.list(sample.int(1e9, n_tasks))
      } else {
        task_seeds <- vector("list", n_tasks)
      }

      if (!is.null(checkpoint_dir)) {
        dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
        if (isTRUE(progress)) {
          existing_ckpts <- list.files(checkpoint_dir, pattern = "^score_fold_.*\\.rds$")
          if (length(existing_ckpts) > 0) {
            message(sprintf("Found %d cached checkpoint file(s) in '%s'; skipping.", length(existing_ckpts), checkpoint_dir))
          }
        }
      }

      base_dots <- c(list(recipe = recipe, censoring_weights = censoring_weights), dots)
      splits_canon <- lapply(splits, canonicalize_split_for_hash)
      run_fp <- compute_checkpoint_fingerprint(
        split = NULL,
        dots = base_dots,
        seed = seed,
        extra = list(
          fn = "score_outer",
          splits = splits_canon,
          grid = as.data.frame(grid_df),
          coefficients = coefficients
        )
      )

      resolve_task_call_args <- function(task_idx, split_obj = NULL, recipe_obj = recipe, base_dots_obj = dots) {
        t_row <- tasks[task_idx, ]
        r <- grid_df[t_row$row_idx, , drop = FALSE]
        curr_split <- split_obj %||% splits[[t_row$split_idx]]
        a <- r$alpha[[1]]
        call_args <- c(
          list(object = curr_split, alpha = a, recipe = recipe_obj, censoring_weights = censoring_weights),
          base_dots_obj
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
          } else if (inherits(recipe_obj, "recipe") && !is.null(recipe_obj$var_info)) {
            roles <- recipe_obj$var_info$role
            call_args$feature_names <- recipe_obj$var_info$variable[!roles %in% c("id variable", "outcome")]
          }
        }
        call_args
      }

      task_fps <- lapply(seq_len(n_tasks), function(task_idx) {
        t_row <- tasks[task_idx, ]
        r <- grid_df[t_row$row_idx, , drop = FALSE]
        curr_split <- splits[[t_row$split_idx]]
        curr_fold <- fold_names[t_row$split_idx]
        c_args <- resolve_task_call_args(task_idx)
        c_dots <- c_args[setdiff(names(c_args), c("object", "alpha"))]
        compute_checkpoint_fingerprint(
          split = curr_split,
          dots = c_dots,
          alpha = r$alpha[[1]],
          formula = c_args$formula,
          seed = task_seeds[[task_idx]],
          extra = list(
            fn = "score_outer_task",
            outer_fold = curr_fold,
            model = r$model[[1]],
            coefficients = coefficients
          )
        )
      })

      ckpt_files <- lapply(seq_len(n_tasks), function(task_idx) {
        if (is.null(checkpoint_dir)) return(NULL)
        t_row <- tasks[task_idx, ]
        r <- grid_df[t_row$row_idx, , drop = FALSE]
        curr_fold <- fold_names[t_row$split_idx]
        safe_fold <- gsub("[^A-Za-z0-9._-]", "_", curr_fold)
        safe_model <- gsub("[^A-Za-z0-9._-]", "_", r$model[[1]])
        file.path(checkpoint_dir, sprintf("score_fold_%s_model_%s.rds", safe_fold, safe_model))
      })

      if (!is.null(shard)) {
        active_indices <- which(((seq_len(n_tasks) - 1L) %% shard[2]) + 1L == shard[1])
      } else {
        active_indices <- seq_len(n_tasks)
      }

      if (!is.null(checkpoint_dir) && length(active_indices) > 0L) {
        mismatch_msgs <- character()
        for (k in active_indices) {
          f_k <- ckpt_files[[k]]
          if (!is.null(f_k) && file.exists(f_k)) {
            chk <- read_verified_checkpoint(f_k, task_fps[[k]], checkpoint_mismatch = checkpoint_mismatch)
            if (identical(chk$status, "mismatch")) {
              if (checkpoint_mismatch == "recompute") {
                mismatch_msgs <- c(mismatch_msgs, chk$message)
              }
              unlink(f_k)
            }
          }
        }
        if (is.null(shard)) {
          all_existing <- list.files(checkpoint_dir, pattern = "^score_fold_.*\\.rds$", full.names = TRUE)
          other_files <- setdiff(all_existing, unlist(ckpt_files))
          for (of in other_files) {
            if (file.exists(of)) {
              cached_of <- tryCatch(readRDS(of), error = function(e) NULL)
              found_run_fp <- if (!is.null(cached_of)) attr(cached_of, "checkpoint_run_fingerprint", exact = TRUE) else NULL
              found_scheme <- if (!is.null(cached_of)) {
                attr(cached_of, "seed_scheme", exact = TRUE) %||%
                  attr(cached_of, "checkpoint_meta", exact = TRUE)$seed_scheme
              } else {
                NULL
              }
              if (!identical(found_run_fp, run_fp) || !identical(found_scheme, current_seed_scheme())) {
                msg <- sprintf(
                  "Checkpoint '%s' has a mismatched fingerprint (inputs, recipe, features, censoring_weights, seed_scheme, or settings changed).",
                  basename(of)
                )
                if (checkpoint_mismatch == "error") {
                  stop(msg, call. = FALSE)
                }
                if (checkpoint_mismatch == "recompute") {
                  mismatch_msgs <- c(mismatch_msgs, msg)
                }
                unlink(of)
              }
            }
          }
        }
        if (length(mismatch_msgs) > 0L && checkpoint_mismatch == "recompute") {
          warning(sprintf(
            "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed. First mismatch: %s",
            length(mismatch_msgs), checkpoint_dir, mismatch_msgs[1]
          ), call. = FALSE)
        }
      }

      can_furrr <- requireNamespace("furrr", quietly = TRUE) &&
        requireNamespace("future", quietly = TRUE)
      is_parallel <- can_furrr && tryCatch(future::nbrOfWorkers() > 1L, error = function(e) FALSE)

      shared_bundle <- create_shared_worker_bundle(splits, base_dots, run_fp)
      bundle_file <- shared_bundle$bundle_file
      fallback_payload <- if (is_parallel && file.exists(bundle_file)) NULL else shared_bundle$payload
      on.exit({
        if (!is.null(bundle_file) && file.exists(bundle_file)) {
          unlink(bundle_file)
        }
        if (exists(run_fp, envir = .worker_cache, inherits = FALSE)) {
          rm(list = run_fp, envir = .worker_cache)
        }
      }, add = TRUE)

      furrr_pkgs <- furrr_worker_packages()

      score_one_task <- local({
        tasks <- tasks
        grid_df <- grid_df
        fold_names <- fold_names
        ckpt_files <- ckpt_files
        task_fps <- task_fps
        run_fp <- run_fp
        bundle_file <- bundle_file
        fallback_payload <- fallback_payload
        censoring_weights <- censoring_weights
        coefficients <- coefficients
        n_tasks <- n_tasks
        shard <- shard
        is_parallel <- is_parallel
        function(task_idx, s_seed) {
          if (is_parallel) {
            Sys.setenv(
              OMP_NUM_THREADS = "1",
              OPENBLAS_NUM_THREADS = "1",
              MKL_NUM_THREADS = "1",
              VECLIB_MAXIMUM_THREADS = "1"
            )
          }
          if (!is.null(s_seed)) {
            local_seed(s_seed)
          }
          t_row <- tasks[task_idx, ]
          r <- grid_df[t_row$row_idx, , drop = FALSE]
          curr_fold <- fold_names[t_row$split_idx]
          ckpt_file <- ckpt_files[[task_idx]]
          t_fp <- task_fps[[task_idx]]

          if (!is.null(ckpt_file) && file.exists(ckpt_file)) {
            chk <- read_verified_checkpoint(ckpt_file, t_fp, checkpoint_mismatch = "recompute")
            if (identical(chk$status, "hit")) {
              return(chk$value)
            }
          }

          bundle <- get_worker_bundle(run_fp, bundle_file = bundle_file, fallback_payload = fallback_payload)
          prep_split <- get_worker_prepared_split(bundle, t_row$split_idx)

          a <- r$alpha[[1]]
          w_dots <- bundle$dots[setdiff(names(bundle$dots), c("recipe", "censoring_weights"))]
          w_recipe <- bundle$dots$recipe
          call_args <- c(
            list(
              object = bundle$splits[[t_row$split_idx]],
              alpha = a,
              recipe = w_recipe,
              censoring_weights = censoring_weights,
              .prepared_split = prep_split
            ),
            w_dots
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
            } else if (inherits(w_recipe, "recipe") && !is.null(w_recipe$var_info)) {
              roles <- w_recipe$var_info$role
              call_args$feature_names <- w_recipe$var_info$variable[!roles %in% c("id variable", "outcome")]
            }
          }

          res <- tryCatch(
            withCallingHandlers(
              do.call(glmnet_IBS, call_args),
              message = function(m) {
                if (grepl("Training data contains no censored subjects", conditionMessage(m))) {
                  invokeRestart("muffleMessage")
                }
              }
            ),
            error = function(e) {
              warning("score_outer() task failed: ", conditionMessage(e), call. = FALSE)
              tibble::tibble(IBS = NA_real_, lambda = NA_real_, term = character(), estimate = numeric())
            }
          )

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

          res_row <- tibble::tibble(
            outer_fold = curr_fold,
            model = r$model[[1]],
            alpha = a,
            formula = if ("formula" %in% names(r)) as.character(r$formula[[1]]) else NA_character_,
            IBS = if (nrow(res) > 0) res$IBS[1] else NA_real_,
            lambda = if (nrow(res) > 0) res$lambda[1] else NA_real_,
            coefficients = list(coef_tbl)
          )

          res_row <- write_verified_checkpoint(
            res_row,
            ckpt_file = ckpt_file,
            fingerprint = t_fp,
            run_fingerprint = run_fp,
            meta = list(
              fn = "score_outer",
              task_idx = task_idx,
              n_tasks = n_tasks,
              outer_fold = curr_fold,
              model = r$model[[1]],
              alpha = a,
              shard = shard,
              seed_scheme = current_seed_scheme()
            )
          )

          res_row
        }
      })

      if (can_furrr) {
        f_opts <- furrr::furrr_options(
          seed = TRUE,
          packages = furrr_pkgs,
          chunk_size = chunk_size
        )
        res_list <- furrr::future_map2(
          active_indices,
          task_seeds[active_indices],
          score_one_task,
          .options = f_opts,
          .progress = progress
        )
      } else {
        res_list <- purrr::map2(
          active_indices,
          task_seeds[active_indices],
          score_one_task
        )
      }

      res_tbl <- dplyr::bind_rows(res_list)
      if (!is.data.frame(res_tbl) || nrow(res_tbl) == 0L || all(is.na(res_tbl$IBS))) {
        stop("All candidate model fits failed in score_outer().", call. = FALSE)
      }
      n_failed <- sum(is.na(res_tbl$IBS))
      if (n_failed > 0L) {
        warning(sprintf("%d candidate model fit(s) failed in score_outer().", n_failed), call. = FALSE)
      }
      attr(res_tbl, "checkpoint_run_fingerprint") <- run_fp
      attr(res_tbl, "seed_scheme") <- current_seed_scheme()
      res_tbl
    }
  )
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
#' @seealso [score_outer()], [summarize_outer_importance()], [plot_outer_importance()]
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

#' Summarize Predictor Importance and Selection Stability Across Outer Folds
#'
#' Computes selection frequency (\eqn{\hat{\beta} \neq 0}), median, range, and
#' interquartile summary statistics of model coefficients across outer folds and
#' candidate models. Accepts either the long tibble returned by
#' [tidy_outer_coefficients()] or the nested tibble returned by [score_outer()].
#'
#' @param x A tibble returned by [tidy_outer_coefficients()] (with `term` and
#'   `estimate` columns) or [score_outer()] (with a `coefficients` list column).
#' @param nonzero_only Logical (default `FALSE`). When `TRUE`, `median_estimate`,
#'   `min_estimate`, `max_estimate`, and quantiles are computed over non-zero
#'   estimates only (with `0` when never selected). When `FALSE`, they are
#'   computed across all evaluated models/folds.
#' @param label_map Optional label mapping (named character vector, data frame
#'   with `term`/`short`/`full`, or `cbe_label_map` object). Defaults to the
#'   active package label map from [cbe_labels()].
#' @param style Label style to use when applying `label_map`: `"short"` or
#'   `"full"`.
#' @return A tibble with one row per predictor and columns `term`, `label`,
#'   `n_models`, `n_selected`, `selection_freq`, `median_estimate`,
#'   `mean_estimate`, `min_estimate`, `max_estimate`, `range_estimate`,
#'   `q25_estimate`, `q75_estimate`, and `iqr_estimate`, ordered by descending
#'   `selection_freq` and `abs(median_estimate)`.
#' @seealso [tidy_outer_coefficients()], [plot_outer_importance()], [score_outer()]
#' @export
summarize_outer_importance <- function(x,
                                       nonzero_only = FALSE,
                                       label_map = NULL,
                                       style = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame from `tidy_outer_coefficients()` or `score_outer()`.", call. = FALSE)
  }
  total_models_override <- NULL
  if ("coefficients" %in% names(x) && !"term" %in% names(x)) {
    total_models_override <- nrow(x)
    x <- tidy_outer_coefficients(x)
  }
  req <- c("term", "estimate")
  missing_cols <- setdiff(req, names(x))
  if (length(missing_cols) > 0L) {
    stop("`x` is missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nrow(x) == 0L) {
    return(tibble::tibble(
      term = character(),
      label = character(),
      n_models = integer(),
      n_selected = integer(),
      selection_freq = numeric(),
      median_estimate = numeric(),
      mean_estimate = numeric(),
      min_estimate = numeric(),
      max_estimate = numeric(),
      range_estimate = numeric(),
      q25_estimate = numeric(),
      q75_estimate = numeric(),
      iqr_estimate = numeric()
    ))
  }

  # Determine total models evaluated if x came from coefficients = "nonzero"
  id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), names(x))
  n_distinct_models <- if (!is.null(total_models_override)) {
    as.integer(total_models_override)
  } else if (length(id_cols) > 0L) {
    nrow(dplyr::distinct(x[, id_cols, drop = FALSE]))
  } else {
    NA_integer_
  }

  terms_u <- unique(as.character(x$term))
  rows <- lapply(terms_u, function(tm) {
    sub_df <- x[x$term == tm & !is.na(x$estimate), , drop = FALSE]
    n_obs <- nrow(sub_df)
    n_mod <- if (!is.na(n_distinct_models) && n_distinct_models > n_obs) n_distinct_models else n_obs
    est_all <- if (n_mod > n_obs) c(sub_df$estimate, rep(0, n_mod - n_obs)) else sub_df$estimate
    nz_mask <- est_all != 0
    n_sel <- sum(nz_mask)
    freq <- if (n_mod > 0L) n_sel / n_mod else NA_real_

    vals <- if (isTRUE(nonzero_only)) {
      if (n_sel > 0L) est_all[nz_mask] else 0
    } else {
      if (length(est_all) > 0L) est_all else NA_real_
    }

    med_v <- stats::median(vals, na.rm = TRUE)
    mean_v <- mean(vals, na.rm = TRUE)
    min_v <- min(vals, na.rm = TRUE)
    max_v <- max(vals, na.rm = TRUE)
    q25_v <- unname(stats::quantile(vals, probs = 0.25, na.rm = TRUE))
    q75_v <- unname(stats::quantile(vals, probs = 0.75, na.rm = TRUE))

    tibble::tibble(
      term = tm,
      label = resolve_cbe_labels(tm, label_map = label_map, style = style),
      n_models = as.integer(n_mod),
      n_selected = as.integer(n_sel),
      selection_freq = as.numeric(freq),
      median_estimate = as.numeric(med_v),
      mean_estimate = as.numeric(mean_v),
      min_estimate = as.numeric(min_v),
      max_estimate = as.numeric(max_v),
      range_estimate = as.numeric(max_v - min_v),
      q25_estimate = as.numeric(q25_v),
      q75_estimate = as.numeric(q75_v),
      iqr_estimate = as.numeric(q75_v - q25_v)
    )
  })

  out <- dplyr::bind_rows(rows)
  ord <- order(-out$selection_freq, -abs(out$median_estimate), out$term)
  out[ord, , drop = FALSE]
}

#' Plot Outer-Fold Coefficient Importance and Selection Frequency
#'
#' Visualizes predictor coefficient distributions and selection frequencies
#' across outer resampling folds. Accepts output from [tidy_outer_coefficients()],
#' [score_outer()], or [summarize_outer_importance()].
#'
#' @param x A tibble from [tidy_outer_coefficients()], [score_outer()], or
#'   [summarize_outer_importance()].
#' @param top_n Optional integer restricting the plot to the top `top_n`
#'   predictors ranked by selection frequency and absolute median coefficient.
#' @param nonzero_only Logical (default `FALSE`). When `TRUE`, drops zero
#'   coefficient estimates before drawing boxplots.
#' @param type Plot type: `"boxplot"` (default; horizontal boxplot or range
#'   intervals of coefficients annotated with selection percentage) or
#'   `"frequency"` (bar/lollipop chart of selection frequency).
#' @param label_map Optional label mapping passed to [resolve_cbe_labels()].
#' @param style Label style (`"short"` or `"full"`) when resolving `label_map`.
#' @return A `ggplot` object.
#' @seealso [summarize_outer_importance()], [tidy_outer_coefficients()], [score_outer()]
#' @export
plot_outer_importance <- function(x,
                                  top_n = NULL,
                                  nonzero_only = FALSE,
                                  type = c("boxplot", "frequency"),
                                  label_map = NULL,
                                  style = NULL) {
  require_packages("ggplot2", "plot_outer_importance")
  type <- match.arg(type)
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  is_summary <- all(c("term", "selection_freq", "median_estimate", "min_estimate", "max_estimate") %in% names(x)) &&
    !"estimate" %in% names(x)

  if (is_summary) {
    sum_df <- x
    sum_df$label <- resolve_cbe_labels(sum_df$term, label_map = label_map, style = style)
    raw_df <- NULL
  } else {
    if ("coefficients" %in% names(x) && !"term" %in% names(x)) {
      raw_df <- tidy_outer_coefficients(x)
    } else {
      raw_df <- x
    }
    sum_df <- summarize_outer_importance(raw_df, nonzero_only = nonzero_only, label_map = label_map, style = style)
  }

  if (nrow(sum_df) == 0L) {
    stop("No terms available to plot in `plot_outer_importance()`.", call. = FALSE)
  }

  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0 && nrow(sum_df) > top_n) {
    sum_df <- sum_df[seq_len(as.integer(top_n)), , drop = FALSE]
  }

  # Create display label with selection frequency for boxplot axis
  sum_df$axis_label <- sprintf("%s (%d%%)", sum_df$label, round(100 * sum_df$selection_freq))
  # Make unique in case two terms map to the same display label
  sum_df$axis_label <- make.unique(sum_df$axis_label, sep = " ")
  level_order <- rev(sum_df$axis_label)

  if (type == "frequency") {
    sum_df$label_f <- factor(make.unique(sum_df$label, sep = " "), levels = rev(make.unique(sum_df$label, sep = " ")))
    p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = .data$label_f, y = .data$selection_freq)) +
      ggplot2::geom_col(fill = "#0072B2", alpha = 0.85, width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"), limits = c(0, 1)) +
      ggplot2::labs(
        title = "Outer-Fold Predictor Selection Frequency",
        subtitle = "Proportion of outer models with non-zero coefficient",
        x = "Predictor",
        y = "Selection Frequency"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  if (!is.null(raw_df)) {
    plot_df <- raw_df[raw_df$term %in% sum_df$term & !is.na(raw_df$estimate), , drop = FALSE]
    if (isTRUE(nonzero_only)) {
      plot_df <- plot_df[plot_df$estimate != 0, , drop = FALSE]
    }
    label_lookup <- stats::setNames(sum_df$axis_label, sum_df$term)
    plot_df$axis_label <- factor(unname(label_lookup[plot_df$term]), levels = level_order)

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$axis_label, y = .data$estimate)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_boxplot(fill = "#56B4E9", alpha = 0.6, outlier.shape = 16, outlier.size = 1.5) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Outer-Fold Coefficient Importance",
        subtitle = "Coefficient distribution across outer folds (with selection frequency %)",
        x = "Predictor (Selection %)",
        y = "Coefficient Estimate (log-hazard)"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  sum_df$axis_label <- factor(sum_df$axis_label, levels = level_order)
  ggplot2::ggplot(sum_df, ggplot2::aes(x = .data$axis_label, y = .data$median_estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$min_estimate, ymax = .data$max_estimate), color = "#0072B2", linewidth = 0.8) +
    ggplot2::geom_point(size = 2.5, color = "#D55E00") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Outer-Fold Coefficient Importance",
      subtitle = "Median and [min, max] range across outer folds (with selection frequency %)",
      x = "Predictor (Selection %)",
      y = "Coefficient Estimate (log-hazard)"
    ) +
    ggplot2::theme_minimal()
}


#' Check Whether Any Training Split Contains Censored Subjects (E3)
#'
#' @keywords internal
#' @noRd
has_censored_subjects_in_splits <- function(object, recipe = NULL, dots = list()) {
  splits <- if (inherits(object, "rsplit")) {
    list(object)
  } else if (inherits(object, "nested_cv") || inherits(object, "rset") ||
             (is.data.frame(object) && "splits" %in% names(object) && is.list(object$splits))) {
    object$splits
  } else if (is.list(object) && length(object) > 0 && all(vapply(object, inherits, logical(1), "rsplit"))) {
    object
  } else {
    return(TRUE)
  }

  id_col <- dots$id_col %||% "id"
  status_col <- dots$status_col %||% "status"

  for (sp in splits) {
    train_raw <- tryCatch(rsample::analysis(sp), error = function(e) NULL)
    if (is.null(train_raw) || !is.data.frame(train_raw)) {
      return(TRUE)
    }
    check_df <- train_raw
    if (!(status_col %in% names(check_df)) && inherits(recipe, "recipe")) {
      check_df <- tryCatch({
        rec_bound <- bind_recipes_namespace(recipe)
        prepped <- recipes::prep(rec_bound, training = train_raw)
        recipes::bake(prepped, new_data = train_raw)
      }, error = function(e) train_raw)
    }
    if (!(status_col %in% names(check_df))) {
      return(TRUE)
    }
    status_vec <- check_df[[status_col]]
    if (anyNA(status_vec)) {
      return(TRUE)
    }
    has_cens <- if (id_col %in% names(check_df)) {
      any(!tapply(status_vec != 0, check_df[[id_col]], any))
    } else {
      any(status_vec == 0)
    }
    if (isTRUE(has_cens)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Compare Model Performance Under Unweighted and IPCW Censoring Weights
#'
#' Evaluates candidate models on an outer resampling split under both `"none"`
#' (unweighted legacy scoring) and `"ipcw"` (proper inverse probability of censoring
#' weighted scoring), returning a tidy side-by-side comparison table of integrated
#' Brier scores (IBS) and model ranks, along with rank agreement statistics
#' (Spearman rank correlation and Kendall's tau).
#'
#' When the training data across splits contains no censored subjects, this is
#' detected before model fitting so [score_outer()] is run only once and both
#' `ibs_none` and `ibs_ipcw` are populated from that single fit.
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
#' @param on_complete Optional callback `function(result)` invoked when the run
#'   finishes or errors (defaults to `getOption("templecbe.on_complete", NULL)`).
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
#' The returned object has attributes `"rank_agreement"` (containing `spearman`
#' @param checkpoint_dir Optional directory to save fingerprinted checkpoints
#'   for outer scoring passes.
#' @param shard Optional integer vector `c(i, n)` for sharded execution.
#' @param checkpoint_mismatch Action when a checkpoint fingerprint mismatches:
#'   `"recompute"` (default), `"overwrite"`, or `"error"`.
#' @param progress Logical (default `FALSE`). Display progress indication.
#' @return A tibble with columns `outer_fold` (if grouped), `model`, `alpha`,
#'   `formula`, `ibs_none`, and `ibs_ipcw`, plus attributes `"rank_agreement"`
#'   (a list with rank correlation metrics across candidates, `spearman`
#'   and `kendall`), `"single_fit_no_censoring"` (`TRUE` when the duplicate fit was
#'   skipped), and `"n_score_outer_runs"`.
#' @seealso [score_outer()], [glmnet_IBS()], [select_candidate_models()]
#' @export
compare_censoring_weights <- function(object,
                                      grid,
                                      recipe = NULL,
                                      ...,
                                      checkpoint_dir = NULL,
                                      shard = NULL,
                                      checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                      seed = NULL,
                                      on_complete = getOption("templecbe.on_complete", NULL),
                                      progress = FALSE) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  shared_seed <- if (!is.null(seed)) seed else sample.int(1e9, 1L)
  dots <- list(...)

  run_with_on_complete(
    fn_name = "compare_censoring_weights",
    on_complete = on_complete,
    meta = list(seed = shared_seed),
    expr = {
      single_fit_no_censoring <- FALSE
      n_score_outer_runs <- 0L

      if (is.data.frame(object) && !("splits" %in% names(object)) && any(c("IBS", "ibs_none") %in% names(object)) && is.data.frame(grid)) {
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
      } else if (inherits(object, "rsplit") || inherits(object, "nested_cv") || inherits(object, "rset") || (is.data.frame(object) && "splits" %in% names(object) && is.list(object$splits)) || (is.list(object) && length(object) > 0 && all(vapply(object, inherits, logical(1), "rsplit")))) {
        if (is.null(recipe)) {
          stop("`recipe` is required when scoring on an rsplit `object`.", call. = FALSE)
        }
        has_cens <- has_censored_subjects_in_splits(object, recipe = recipe, dots = dots)
        ckpt_none <- if (!is.null(checkpoint_dir)) file.path(checkpoint_dir, "score_none") else NULL
        ckpt_ipcw <- if (!is.null(checkpoint_dir)) file.path(checkpoint_dir, "score_ipcw") else NULL

        if (!has_cens) {
          single_fit_no_censoring <- TRUE
          n_score_outer_runs <- 1L
          freq <- if (isTRUE(getOption("templecbe_suppress_frequency_limit", FALSE))) "always" else "once"
          rlang::inform(
            "Training data contains no censored subjects; running score_outer() once and returning both 'ibs_none' and 'ibs_ipcw' from the single fit.",
            .frequency = freq,
            .frequency_id = "TempleCBE_compare_censoring_weights_no_censoring_single_fit"
          )
          scored_once <- score_outer(
            object, grid, recipe = recipe, ...,
            censoring_weights = "none", seed = shared_seed,
            checkpoint_dir = ckpt_none,
            shard = shard,
            checkpoint_mismatch = checkpoint_mismatch,
            on_complete = NULL, progress = progress
          )
          id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), names(scored_once))
          out <- dplyr::select(scored_once, dplyr::all_of(id_cols))
          out$ibs_none <- scored_once$IBS
          out$ibs_ipcw <- scored_once$IBS
          attr(out, "scored_outer") <- scored_once
        } else {
          n_score_outer_runs <- 2L
          scored_none <- score_outer(
            object, grid, recipe = recipe, ...,
            censoring_weights = "none", seed = shared_seed,
            checkpoint_dir = ckpt_none,
            shard = shard,
            checkpoint_mismatch = checkpoint_mismatch,
            on_complete = NULL, progress = progress
          )
          scored_ipcw <- score_outer(
            object, grid, recipe = recipe, ...,
            censoring_weights = "ipcw", seed = shared_seed,
            checkpoint_dir = ckpt_ipcw,
            shard = shard,
            checkpoint_mismatch = checkpoint_mismatch,
            on_complete = NULL, progress = progress
          )

          id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), names(scored_none))
          out <- dplyr::inner_join(
            dplyr::select(scored_none, dplyr::all_of(c(id_cols, "IBS"))),
            dplyr::select(scored_ipcw, dplyr::all_of(c(id_cols, "IBS"))),
            by = id_cols,
            suffix = c("_none", "_ipcw")
          )
          out <- dplyr::rename(out, ibs_none = .data$IBS_none, ibs_ipcw = .data$IBS_ipcw)
        }
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
      if (n_valid == 0L) {
        stop("All model evaluations failed in compare_censoring_weights().", call. = FALSE)
      }

      is_identical <- n_valid > 0L && all(abs(out$ibs_none - out$ibs_ipcw) < 1e-12, na.rm = TRUE)
      if (is_identical) {
        rlang::inform(
          "Censoring weights 'none' and 'ipcw' produce identical scores across all models.",
          .frequency = "once",
          .frequency_id = "TempleCBE_compare_censoring_weights_identical"
        )
        spearman_rho <- 1.0
        kendall_tau <- 1.0
      } else {
        spearman_rho <- if (n_valid >= 2) stats::cor(out$ibs_none, out$ibs_ipcw, method = "spearman", use = "complete.obs") else NA_real_
        kendall_tau <- if (n_valid >= 2) stats::cor(out$ibs_none, out$ibs_ipcw, method = "kendall", use = "complete.obs") else NA_real_
      }

      attr(out, "seed") <- shared_seed
      attr(out, "single_fit_no_censoring") <- single_fit_no_censoring
      attr(out, "n_score_outer_runs") <- n_score_outer_runs
      attr(out, "rank_agreement") <- list(
        spearman = spearman_rho,
        kendall = kendall_tau
      )

      class(out) <- unique(c("cbe_censoring_weights_comparison", class(out)))
      out
    }
  )
}

#' Select Candidate Models From Tuning Results
#'
#' Takes results from [tune_alpha_by_split()] or [tune_over_alpha()], identifies
#' the best-performing `alpha` values (e.g. within the top `ntile` of integrated Brier
#' scores), and crosses them with candidate formulas to construct an outer evaluation
#' grid.
#'
#' @param tune_results A data frame containing tuning results with at least `alpha`
#'   and `IBS` columns (e.g. from [tune_alpha_by_split()]).
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
#' @seealso [score_outer()], [compare_censoring_weights()], [tune_alpha_by_split()]
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

#' Render Censoring Weight Sensitivity and Rank Agreement Table
#'
#' Formats the output of [compare_censoring_weights()] (or evaluates it directly on candidate models)
#' into a tidy sensitivity table suitable for inclusion in Quarto and R Markdown reports.
#' Reports integrated Brier scores under unweighted and IPCW weighting alongside model ranks,
#' rank differences, and rank agreement metrics (Spearman's \eqn{\rho} and Kendall's \eqn{\tau}).
#'
#' @param data A data frame returned by [compare_censoring_weights()], or an object
#'   passed to [compare_censoring_weights()] (such as an `rsplit`, `nested_cv`, or list).
#' @param grid Optional candidate model grid if `data` is an `rsplit`/resamples object.
#' @param recipe Optional preprocessing recipe if `data` is an `rsplit`/resamples object.
#' @param ... Additional arguments passed to [compare_censoring_weights()].
#' @param format Output format: `"tibble"` (default) or `"gt"` (requires \pkg{gt}).
#' @param digits Number of decimal places for numeric metrics (default 4).
#' @param label_map Optional label mapping passed to [resolve_cbe_labels()].
#' @param style Optional label style (`"short"` or `"full"`) when resolving `label_map`.
#' @return A tibble or \code{gt_tbl} object displaying side-by-side IBS, ranks, rank differences,
#'   and rank agreement correlation statistics.
#' @seealso [compare_censoring_weights()], [score_outer()]
#' @export
tbl_censoring_comparison <- function(data,
                                     grid = NULL,
                                     recipe = NULL,
                                     ...,
                                     format = c("tibble", "gt"),
                                     digits = 4,
                                     label_map = NULL,
                                     style = NULL) {
  format <- match.arg(format)

  comp_df <- if (is.data.frame(data) && all(c("ibs_none", "ibs_ipcw") %in% names(data))) {
    data
  } else {
    compare_censoring_weights(data, grid = grid, recipe = recipe, ...)
  }

  rank_agr <- attr(comp_df, "rank_agreement")
  rho <- if (!is.null(rank_agr$spearman)) rank_agr$spearman else NA_real_
  tau <- if (!is.null(rank_agr$kendall)) rank_agr$kendall else NA_real_

  # Select and order columns
  id_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), names(comp_df))
  metric_cols <- c("ibs_none", "ibs_ipcw", "rank_none", "rank_ipcw", "rank_diff")
  display_df <- dplyr::select(comp_df, dplyr::all_of(c(id_cols, metric_cols)))
  if ("model" %in% names(display_df)) {
    display_df$model <- resolve_cbe_labels(display_df$model, label_map = label_map, style = style)
  }
  if ("formula" %in% names(display_df)) {
    display_df$formula <- resolve_cbe_formula_labels(display_df$formula, label_map = label_map, style = style)
  }

  if (format == "gt") {
    require_packages("gt", "tbl_censoring_comparison(format = 'gt')")
    agr_note <- sprintf("Rank agreement: Spearman's \u03c1 = %.*f, Kendall's \u03c4 = %.*f",
                        min(digits, 3), rho, min(digits, 3), tau)
    gt_tbl <- gt::gt(display_df)
    gt_tbl <- gt::tab_header(
      gt_tbl,
      title = "Censoring Weight Sensitivity Analysis",
      subtitle = "Comparison of Unweighted vs. IPCW Integrated Brier Scores (IBS)"
    )
    gt_tbl <- gt::fmt_number(gt_tbl, columns = dplyr::all_of(c("ibs_none", "ibs_ipcw")), decimals = digits)
    if ("alpha" %in% names(display_df)) {
      gt_tbl <- gt::fmt_number(gt_tbl, columns = "alpha", decimals = min(digits, 3))
    }
    gt_tbl <- gt::tab_spanner(gt_tbl, label = "Integrated Brier Score", columns = c("ibs_none", "ibs_ipcw"))
    gt_tbl <- gt::tab_spanner(gt_tbl, label = "Model Rank", columns = c("rank_none", "rank_ipcw", "rank_diff"))
    gt_tbl <- gt::cols_label(
      gt_tbl,
      ibs_none = "Unweighted",
      ibs_ipcw = "IPCW",
      rank_none = "Unweighted",
      rank_ipcw = "IPCW",
      rank_diff = "\u0394 Rank"
    )
    gt_tbl <- gt::tab_source_note(gt_tbl, source_note = agr_note)
    return(gt_tbl)
  }

  attr(display_df, "rank_agreement") <- list(spearman = rho, kendall = tau)
  display_df
}

#' Compare and Diff Outer-Fold Runs Across Platforms With Tolerance
#'
#' Evaluates reproducibility between two sets of model evaluation runs (e.g. from
#' [score_outer()]), such as runs performed across different operating systems
#' (Windows, Linux, macOS) or hardware architectures (x86_64 vs ARM64). Compares
#' integrated Brier scores (IBS), selected penalties (\eqn{\lambda}), and coefficient
#' estimates within user-specified numeric tolerances.
#'
#' @param run1 A data frame returned by [score_outer()] or a character path to an `.rds` file.
#' @param run2 A second data frame or character path to compare against `run1`.
#' @param tolerance Numeric tolerance for IBS and \eqn{\lambda} differences (default `1e-6`).
#' @param coef_tolerance Numeric tolerance for coefficient differences (defaults to `tolerance`).
#' @param label1 Optional character label for `run1` (default `"run1"`).
#' @param label2 Optional character label for `run2` (default `"run2"`).
#' @return An object of class `c("templecbe_run_comparison", "tbl_df", "tbl", "data.frame")`
#'   containing per-model diffs, with overall reproducibility summary attributes.
#' @seealso [score_outer()], [tidy_outer_coefficients()]
#' @export
compare_runs <- function(run1,
                         run2,
                         tolerance = 1e-6,
                         coef_tolerance = tolerance,
                         label1 = "run1",
                         label2 = "run2") {
  if (is.character(run1) && length(run1) == 1L && file.exists(run1)) {
    run1 <- readRDS(run1)
  }
  if (is.character(run2) && length(run2) == 1L && file.exists(run2)) {
    run2 <- readRDS(run2)
  }

  if (is.data.frame(run1) && is.data.frame(run2) &&
      !"model" %in% names(run1) && !"model" %in% names(run2) &&
      all(c("alpha", "IBS") %in% names(run1)) && all(c("alpha", "IBS") %in% names(run2))) {
    return(compare_tuning_runs(run1, run2, tolerance = tolerance, label1 = label1, label2 = label2))
  }

  if (!is.data.frame(run1) || !is.data.frame(run2)) {
    stop("`run1` and `run2` must be data frames or valid paths to RDS files.", call. = FALSE)
  }
  if (!all(c("model", "IBS") %in% names(run1)) || !all(c("model", "IBS") %in% names(run2))) {
    stop("Both runs must contain 'model' and 'IBS' columns.", call. = FALSE)
  }

  join_cols <- intersect(c("outer_fold", "model", "alpha", "formula"), intersect(names(run1), names(run2)))
  if (length(join_cols) == 0L) {
    join_cols <- "model"
  }

  df1 <- tibble::as_tibble(run1)
  df2 <- tibble::as_tibble(run2)

  merged <- dplyr::inner_join(
    df1,
    df2,
    by = join_cols,
    suffix = c(paste0("_", label1), paste0("_", label2))
  )

  if (nrow(merged) == 0L) {
    stop("No matching models found between run1 and run2 using keys: ", paste(join_cols, collapse = ", "), call. = FALSE)
  }

  ibs1_col <- paste0("IBS_", label1)
  ibs2_col <- paste0("IBS_", label2)
  diff_ibs <- abs(merged[[ibs1_col]] - merged[[ibs2_col]])

  has_lambda <- paste0("lambda_", label1) %in% names(merged) && paste0("lambda_", label2) %in% names(merged)
  diff_lambda <- if (has_lambda) {
    abs(merged[[paste0("lambda_", label1)]] - merged[[paste0("lambda_", label2)]])
  } else {
    rep(0, nrow(merged))
  }

  has_coefs <- paste0("coefficients_", label1) %in% names(merged) && paste0("coefficients_", label2) %in% names(merged)

  max_coef_diffs <- numeric(nrow(merged))
  terms_matched <- logical(nrow(merged))

  for (i in seq_len(nrow(merged))) {
    if (has_coefs) {
      c1 <- merged[[paste0("coefficients_", label1)]][[i]]
      c2 <- merged[[paste0("coefficients_", label2)]][[i]]
      if (is.data.frame(c1) && is.data.frame(c2) && nrow(c1) > 0 && nrow(c2) > 0) {
        nz1 <- sort(c1$term[abs(c1$estimate) > 1e-9])
        nz2 <- sort(c2$term[abs(c2$estimate) > 1e-9])
        terms_matched[i] <- identical(nz1, nz2)

        m_c <- merge(c1, c2, by = "term", all = TRUE)
        m_c$estimate.x[is.na(m_c$estimate.x)] <- 0
        m_c$estimate.y[is.na(m_c$estimate.y)] <- 0
        max_coef_diffs[i] <- max(abs(m_c$estimate.x - m_c$estimate.y), na.rm = TRUE)
      } else {
        terms_matched[i] <- TRUE
        max_coef_diffs[i] <- 0
      }
    } else {
      terms_matched[i] <- TRUE
      max_coef_diffs[i] <- 0
    }
  }

  ibs_ok <- is.na(diff_ibs) | (diff_ibs <= tolerance)
  lambda_ok <- is.na(diff_lambda) | (diff_lambda <= tolerance)
  coef_ok <- max_coef_diffs <= coef_tolerance
  reproducible <- ibs_ok & lambda_ok & coef_ok & terms_matched

  out_df <- dplyr::select(merged, dplyr::all_of(join_cols))
  out_df[[paste0("ibs_", label1)]] <- merged[[ibs1_col]]
  out_df[[paste0("ibs_", label2)]] <- merged[[ibs2_col]]
  out_df$diff_ibs <- diff_ibs

  if (has_lambda) {
    out_df[[paste0("lambda_", label1)]] <- merged[[paste0("lambda_", label1)]]
    out_df[[paste0("lambda_", label2)]] <- merged[[paste0("lambda_", label2)]]
    out_df$diff_lambda <- diff_lambda
  }
  if (has_coefs) {
    out_df$max_coef_diff <- max_coef_diffs
    out_df$terms_match <- terms_matched
  }
  out_df$reproducible <- reproducible

  spearman_rho <- if (nrow(out_df) >= 2) stats::cor(out_df[[paste0("ibs_", label1)]], out_df[[paste0("ibs_", label2)]], method = "spearman", use = "complete.obs") else NA_real_

  summary_info <- list(
    label1 = label1,
    label2 = label2,
    tolerance = tolerance,
    coef_tolerance = coef_tolerance,
    n_models = nrow(out_df),
    all_reproducible = all(reproducible),
    max_ibs_diff = max(diff_ibs, na.rm = TRUE),
    max_lambda_diff = if (has_lambda) max(diff_lambda, na.rm = TRUE) else 0,
    max_coef_diff = if (has_coefs) max(max_coef_diffs, na.rm = TRUE) else 0,
    rank_correlation = spearman_rho
  )

  attr(out_df, "comparison_summary") <- summary_info
  class(out_df) <- c("templecbe_run_comparison", class(out_df))
  out_df
}

#' @export
print.templecbe_run_comparison <- function(x, ...) {
  s <- attr(x, "comparison_summary")
  cat("====================================================\n")
  cat("TempleCBE Cross-Platform Run Comparison\n")
  cat(sprintf("Runs compared: '%s' vs '%s'\n", s$label1, s$label2))
  cat(sprintf("Models evaluated: %d\n", s$n_models))
  cat(sprintf("Tolerance: IBS/lambda <= %g, Coefs <= %g\n", s$tolerance, s$coef_tolerance))
  cat("----------------------------------------------------\n")
  cat(sprintf("Reproducible within tolerance: %s\n", if (s$all_reproducible) "YES (\u2714)" else "NO (\u2718)"))
  cat(sprintf("Max |IBS diff|:        %g\n", s$max_ibs_diff))
  cat(sprintf("Max |lambda diff|:     %g\n", s$max_lambda_diff))
  cat(sprintf("Max |coefficient diff|: %g\n", s$max_coef_diff))
  cat(sprintf("Spearman Rank Corr:    %.4f\n", s$rank_correlation))
  cat("====================================================\n")
  if (!s$all_reproducible) {
    discrepant <- x[!x$reproducible, , drop = FALSE]
    cat(sprintf("\n%d model(s) exceeded tolerance:\n", nrow(discrepant)))
    print(utils::head(discrepant, 10))
  }
  invisible(x)
}

#' Compare and Summarize Two Tuning Runs
#'
#' Computes comparative summary statistics (mean, median, 25th and 75th percentiles)
#' of integrated Brier scores (IBS) across candidate alpha values and formulas between
#' two tuning runs (e.g. from [tune_alpha_by_split()] or [tune_over_alpha()]).
#'
#' @param tune1 A data frame of tuning results containing `alpha` and `IBS` columns,
#'   or a path to an `.rds` file.
#' @param tune2 A second data frame of tuning results to compare against `tune1`,
#'   or a path to an `.rds` file.
#' @param tolerance Numeric tolerance for differences in mean IBS (default `1e-4`).
#' @param label1 Character label for `tune1` (default `"run1"`).
#' @param label2 Character label for `tune2` (default `"run2"`).
#' @param match Matching strategy for candidate hyperparameter configurations:
#'   `"exact"` (default; matches on exact `alpha` values), `"nearest"` (matches each
#'   `alpha` in `tune1` to the closest `alpha` in `tune2`), or `"binned"` (discretizes
#'   `alpha` into `n_bins` equal-width bins).
#' @param n_bins Number of bins when `match = "binned"` (default `10`).
#' @return An object of class `c("templecbe_tuning_comparison", "tbl_df", "tbl", "data.frame")`
#'   summarizing per-alpha IBS distributions, distributional shifts, and global IBS statistics.
#' @seealso [tune_alpha_by_split()], [compare_runs()]
#' @export
compare_tuning_runs <- function(tune1,
                                tune2,
                                tolerance = 1e-4,
                                label1 = "run1",
                                label2 = "run2",
                                match = c("exact", "nearest", "binned"),
                                n_bins = 10) {
  match <- match.arg(match)
  tune1 <- as_tuning_results(tune1)
  tune2 <- as_tuning_results(tune2)

  # Overall distribution comparison across all raw fits (J1)
  calc_dist <- function(df) {
    ibs <- stats::na.omit(as.numeric(df$IBS))
    best_row <- if (length(ibs) > 0L) which.min(df$IBS) else NA_integer_
    list(
      n = length(ibs),
      best_ibs = if (!is.na(best_row)) df$IBS[best_row] else NA_real_,
      best_alpha = if (!is.na(best_row)) df$alpha[best_row] else NA_real_,
      min = if (length(ibs) > 0L) min(ibs) else NA_real_,
      q25 = if (length(ibs) > 0L) stats::quantile(ibs, 0.25, names = FALSE) else NA_real_,
      median = if (length(ibs) > 0L) stats::median(ibs) else NA_real_,
      mean = if (length(ibs) > 0L) mean(ibs) else NA_real_,
      q75 = if (length(ibs) > 0L) stats::quantile(ibs, 0.75, names = FALSE) else NA_real_,
      max = if (length(ibs) > 0L) max(ibs) else NA_real_
    )
  }
  d1 <- calc_dist(tune1)
  d2 <- calc_dist(tune2)
  has_ties <- any(duplicated(tune1$IBS[!is.na(tune1$IBS)])) || any(duplicated(tune2$IBS[!is.na(tune2$IBS)]))
  ks_p <- if (d1$n >= 2L && d2$n >= 2L) {
    suppressWarnings(tryCatch(stats::ks.test(tune1$IBS, tune2$IBS)$p.value, error = function(e) NA_real_))
  } else {
    NA_real_
  }
  dist_comparison <- tibble::tibble(
    metric = c("n_fits", "best_ibs", "best_alpha", "min_ibs", "q25_ibs", "median_ibs", "mean_ibs", "q75_ibs", "max_ibs"),
    run1 = c(d1$n, d1$best_ibs, d1$best_alpha, d1$min, d1$q25, d1$median, d1$mean, d1$q75, d1$max),
    run2 = c(d2$n, d2$best_ibs, d2$best_alpha, d2$min, d2$q25, d2$median, d2$mean, d2$q75, d2$max),
    abs_diff = c(abs(d1$n - d2$n), abs(d1$best_ibs - d2$best_ibs), abs(d1$best_alpha - d2$best_alpha),
                 abs(d1$min - d2$min), abs(d1$q25 - d2$q25), abs(d1$median - d2$median),
                 abs(d1$mean - d2$mean), abs(d1$q75 - d2$q75), abs(d1$max - d2$max))
  )
  names(dist_comparison)[2:3] <- c(label1, label2)

  has_formula <- ("formula" %in% names(tune1) && "formula" %in% names(tune2))

  if (identical(match, "exact")) {
    group_cols <- if (has_formula) c("alpha", "formula") else "alpha"

    summarize_tune <- function(df, lbl) {
      g <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols)))
      s <- dplyr::summarize(
        g,
        n_fits = dplyr::n(),
        mean_ibs = mean(.data$IBS, na.rm = TRUE),
        median_ibs = stats::median(.data$IBS, na.rm = TRUE),
        q25_ibs = stats::quantile(.data$IBS, 0.25, na.rm = TRUE),
        q75_ibs = stats::quantile(.data$IBS, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
      names(s)[names(s) %in% c("n_fits", "mean_ibs", "median_ibs", "q25_ibs", "q75_ibs")] <-
        paste0(c("n_fits", "mean_ibs", "median_ibs", "q25_ibs", "q75_ibs"), "_", lbl)
      s
    }

    s1 <- summarize_tune(tune1, label1)
    s2 <- summarize_tune(tune2, label2)
    merged <- dplyr::inner_join(s1, s2, by = group_cols)

  } else if (identical(match, "nearest")) {
    group_cols <- if (has_formula) c("alpha", "formula") else "alpha"

    summarize_tune_base <- function(df) {
      g <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols)))
      dplyr::summarize(
        g,
        n_fits = dplyr::n(),
        mean_ibs = mean(.data$IBS, na.rm = TRUE),
        median_ibs = stats::median(.data$IBS, na.rm = TRUE),
        q25_ibs = stats::quantile(.data$IBS, 0.25, na.rm = TRUE),
        q75_ibs = stats::quantile(.data$IBS, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    }

    s1 <- summarize_tune_base(tune1)
    s2 <- summarize_tune_base(tune2)

    rows <- vector("list", nrow(s1))
    for (i in seq_len(nrow(s1))) {
      a1 <- s1$alpha[i]
      cand_s2 <- if (has_formula) s2[s2$formula == s1$formula[i], , drop = FALSE] else s2
      if (nrow(cand_s2) == 0L) cand_s2 <- s2

      dist_a <- abs(cand_s2$alpha - a1)
      best_idx <- which.min(dist_a)
      match_row <- cand_s2[best_idx, , drop = FALSE]

      row_i <- tibble::tibble(
        alpha_1 = a1,
        alpha_2 = match_row$alpha,
        alpha_dist = abs(a1 - match_row$alpha)
      )
      if (has_formula) {
        row_i$formula <- s1$formula[i]
      }
      row_i[[paste0("n_fits_", label1)]] <- s1$n_fits[i]
      row_i[[paste0("mean_ibs_", label1)]] <- s1$mean_ibs[i]
      row_i[[paste0("median_ibs_", label1)]] <- s1$median_ibs[i]
      row_i[[paste0("q25_ibs_", label1)]] <- s1$q25_ibs[i]
      row_i[[paste0("q75_ibs_", label1)]] <- s1$q75_ibs[i]

      row_i[[paste0("n_fits_", label2)]] <- match_row$n_fits
      row_i[[paste0("mean_ibs_", label2)]] <- match_row$mean_ibs
      row_i[[paste0("median_ibs_", label2)]] <- match_row$median_ibs
      row_i[[paste0("q25_ibs_", label2)]] <- match_row$q25_ibs
      row_i[[paste0("q75_ibs_", label2)]] <- match_row$q75_ibs
      rows[[i]] <- row_i
    }
    merged <- dplyr::bind_rows(rows)

  } else if (identical(match, "binned")) {
    breaks <- seq(0, 1, length.out = as.integer(n_bins) + 1L)
    bin_labels <- sprintf("[%.2f, %.2f]", breaks[-length(breaks)], breaks[-1])

    b1 <- tune1
    b2 <- tune2
    b1$alpha_bin <- cut(b1$alpha, breaks = breaks, labels = bin_labels, include.lowest = TRUE)
    b2$alpha_bin <- cut(b2$alpha, breaks = breaks, labels = bin_labels, include.lowest = TRUE)

    group_cols <- if (has_formula) c("alpha_bin", "formula") else "alpha_bin"

    summarize_binned <- function(df, lbl) {
      g <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols)))
      s <- dplyr::summarize(
        g,
        n_fits = dplyr::n(),
        mean_alpha = mean(.data$alpha, na.rm = TRUE),
        mean_ibs = mean(.data$IBS, na.rm = TRUE),
        median_ibs = stats::median(.data$IBS, na.rm = TRUE),
        q25_ibs = stats::quantile(.data$IBS, 0.25, na.rm = TRUE),
        q75_ibs = stats::quantile(.data$IBS, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
      names(s)[names(s) %in% c("n_fits", "mean_alpha", "mean_ibs", "median_ibs", "q25_ibs", "q75_ibs")] <-
        paste0(c("n_fits", "mean_alpha", "mean_ibs", "median_ibs", "q25_ibs", "q75_ibs"), "_", lbl)
      s
    }

    s1 <- summarize_binned(b1, label1)
    s2 <- summarize_binned(b2, label2)
    merged <- dplyr::inner_join(s1, s2, by = group_cols)
  }

  if (nrow(merged) == 0L) {
    stop("No matching alpha/formula configurations found between tune1 and tune2.", call. = FALSE)
  }

  mean1_col <- paste0("mean_ibs_", label1)
  mean2_col <- paste0("mean_ibs_", label2)
  diff_mean <- abs(merged[[mean1_col]] - merged[[mean2_col]])
  reproducible <- is.na(diff_mean) | (diff_mean <= tolerance)

  merged$diff_mean_ibs <- diff_mean
  merged$reproducible <- reproducible

  spearman_rho <- if (nrow(merged) >= 2) stats::cor(merged[[mean1_col]], merged[[mean2_col]], method = "spearman", use = "complete.obs") else NA_real_

  summary_info <- list(
    label1 = label1,
    label2 = label2,
    match = match,
    tolerance = tolerance,
    n_configs = nrow(merged),
    all_reproducible = all(reproducible),
    max_mean_diff = max(diff_mean, na.rm = TRUE),
    rank_correlation = spearman_rho,
    ks_pvalue = ks_p,
    ks_has_ties = has_ties,
    distribution_comparison = dist_comparison
  )

  attr(merged, "comparison_summary") <- summary_info
  attr(merged, "distribution_comparison") <- dist_comparison
  class(merged) <- c("templecbe_tuning_comparison", class(merged))
  merged
}

#' @export
print.templecbe_tuning_comparison <- function(x, ...) {
  s <- attr(x, "comparison_summary")
  cat("====================================================\n")
  cat("TempleCBE Tuning Run Comparison\n")
  cat(sprintf("Runs compared: '%s' vs '%s' (match = '%s')\n", s$label1, s$label2, s$match))
  cat(sprintf("Tuning configurations matched: %d\n", s$n_configs))
  cat(sprintf("Tolerance: mean |IBS diff| <= %g\n", s$tolerance))
  cat("----------------------------------------------------\n")
  cat(sprintf("Reproducible within tolerance: %s\n", if (s$all_reproducible) "YES (\u2714)" else "NO (\u2718)"))
  cat(sprintf("Max mean |IBS diff|:   %g\n", s$max_mean_diff))
  cat(sprintf("Spearman Rank Corr:    %.4f\n", s$rank_correlation))
  ties_note <- if (isTRUE(s$ks_has_ties)) " (approximate in presence of ties)" else ""
  cat(sprintf("KS Test (IBS dist):    p = %.4g%s\n", s$ks_pvalue, ties_note))
  cat("----------------------------------------------------\n")
  cat("Overall IBS Distribution Summary:\n")
  print(as.data.frame(s$distribution_comparison), row.names = FALSE)
  cat("====================================================\n")
  if (!s$all_reproducible) {
    discrepant <- x[!x$reproducible, , drop = FALSE]
    cat(sprintf("\n%d configuration(s) exceeded tolerance:\n", nrow(discrepant)))
    print(as.data.frame(utils::head(discrepant, 10)))
  }
  invisible(x)
}
