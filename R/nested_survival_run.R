# ==============================================================================
# E1: cbe_nested_survival_run() — End-to-End Nested Survival Run
# E2: repro_appendix()          — Self-Rendering Reproducibility Appendix
# E4: cbe_domain_children()     — Per-Domain Child Expansion with Isolation
# ==============================================================================

#' Run an End-to-End Nested Cross-Validation Survival Pipeline (E1)
#'
#' Executes the complete multi-stage nested survival analysis workflow in a
#' single call:
#' \enumerate{
#'   \item Pre-flight data validation via [validate_start_stop()] (stopping or
#'     warning on structural anomalies or absence of censoring).
#'   \item Grouped nested resampling fold construction via [rsample::nested_cv()]
#'     and [rsample::group_vfold_cv()].
#'   \item Inner hyperparameter tuning across outer folds via
#'     [summarize_tune_results()] with per-task fingerprinted checkpoints.
#'   \item Candidate model selection via [select_candidate_models()].
#'   \item Outer-fold model scoring via [score_outer()] with fingerprinted
#'     checkpoints.
#'   \item Predictor importance and selection stability summarization across
#'     outer folds via [summarize_outer_importance()].
#'   \item Censoring weight sensitivity comparison via
#'     [compare_censoring_weights()] (automatically skipping the duplicate fit
#'     when the training data has no censored subjects).
#' }
#'
#' When `run_dir` is provided, every stage writes fingerprinted checkpoints and
#' a run manifest (`render_manifest.json`) into `run_dir` and resumes from disk
#' on rerun without recomputing completed tasks. When `shard = c(i, n)` is
#' supplied, only shard `i` of `n` is evaluated; once all shards finish, rerun
#' with `shard = NULL` (or `collect = TRUE`) to merge checkpoints and finalize
#' the run object.
#'
#' @param data A data frame containing counting-process `(start, stop]` or
#'   right-censored survival intervals.
#' @param recipe Optional [recipes::recipe()] object specifying preprocessing.
#'   When `NULL`, a default recipe is built on `data` assigning `id`, `start`,
#'   `stop`, and `status` the `"id variable"` role.
#' @param id,start,stop,status Column names (as strings or bare symbols) for
#'   subject identifier, interval start time, interval stop time, and event
#'   status indicator (`0` = censored, `1` = event). Defaults to `"id"`,
#'   `"tstart"`, `"tstop"`, and `"status"`.
#' @param domain Character label identifying the clinical or modeling domain
#'   (default `"default"`).
#' @param run_dir Optional directory path where fingerprinted checkpoints,
#'   stage artifacts (`nested_survival_run.rds`), and `render_manifest.json`
#'   are stored.
#' @param shard Optional integer vector `c(i, n)` specifying that this machine
#'   should evaluate shard `i` of `n` into `run_dir`.
#' @param collect Logical (default `FALSE`). When `TRUE` and `run_dir` contains
#'   completed sharded checkpoints, merges them with [collect_checkpoints()]
#'   before computing candidate selection, outer importance, and summaries.
#' @param feature_names Optional character vector of candidate predictor column
#'   names. Defaults to all predictor variables in `recipe` (or all columns of
#'   `data` other than `id`, `start`, `stop`, and `status`).
#' @param formulas Optional character vector or list of candidate right-hand-side
#'   formulas. Defaults to a single full formula combining `feature_names`.
#' @param outer_v Number of outer cross-validation folds (default `5L`).
#' @param inner_v Number of inner cross-validation folds (default `5L`).
#' @param inner_bootstraps Optional integer number of grouped inner bootstrap
#'   resamples (when non-`NULL`, uses [rsample::group_bootstraps()] instead of
#'   [rsample::group_vfold_cv()] for the inner resamples).
#' @param nested_folds Optional pre-constructed [rsample::nested_cv()] object.
#'   Overrides `outer_v`, `inner_v`, and `inner_bootstraps` when supplied.
#' @param alphas Optional numeric vector of elastic-net mixing values in
#'   \eqn{[0, 1]} for inner tuning.
#' @param num_alpha_values Number of alpha values to generate when `alphas` is
#'   `NULL` (default `6L`).
#' @param num_fixed Number of evenly spaced fixed alpha values when `alphas` is
#'   `NULL` (default `min(6L, num_alpha_values)`).
#' @param ntile Quantile cutoff in `(0, 1]` passed to [select_candidate_models()]
#'   (default `0.25`).
#' @param top_n Optional integer number of top alphas to select in
#'   [select_candidate_models()] (overrides `ntile` when non-`NULL`).
#' @param select_by Selection granularity for [select_candidate_models()]:
#'   `"row"` (default) or `"alpha"`.
#' @param internal_folds Number of internal folds passed to [glmnet_IBS()]
#'   (default `5L`).
#' @param censoring_weights Primary censoring weight scheme for tuning and
#'   outer scoring: `"ipcw"` (default) or `"none"`.
#' @param coefficients Which coefficients to store in [score_outer()]: `"all"`
#'   (default) or `"nonzero"`.
#' @param compare_censoring Logical (default `TRUE`). Run
#'   [compare_censoring_weights()] on the candidate grid across outer folds.
#' @param no_censoring_action Action when [validate_start_stop()] detects zero
#'   censored subjects (`n_censored == 0`): `"warn"` (default), `"stop"`, or
#'   `"ignore"`.
#' @param error_on_invalid Logical (default `TRUE`). Stop immediately if
#'   [validate_start_stop()] detects structural interval errors.
#' @param checkpoint_mismatch Action when an existing checkpoint in `run_dir`
#'   has a mismatched fingerprint: `"recompute"` (default), `"overwrite"`, or
#'   `"error"`.
#' @param chunk_size Optional integer number of tasks per worker chunk passed to
#'   [summarize_tune_results()] and [score_outer()] (default `NULL`).
#' @param seed Integer RNG seed for reproducible fold construction, tuning, and
#'   outer scoring (default `42L`).
#' @param on_complete Optional callback `function(result)` invoked when the run
#'   completes or errors (defaults to `getOption("templecbe.on_complete", NULL)`).
#' @param progress Logical (default `FALSE`). Display progress bars.
#' @param ... Additional arguments passed to [glmnet_IBS()] (e.g. `cox.ties`,
#'   `rule`, `metric`, `tune_engine`, `eval_time`).
#' @return An S3 object of class `cbe_nested_survival_run` containing:
#'   \describe{
#'     \item{`domain`}{Domain identifier string.}
#'     \item{`validation`}{Result of [validate_start_stop()].}
#'     \item{`nested_folds`}{The [rsample::nested_cv()] object.}
#'     \item{`tune_results`}{Tibble of inner tuning results across outer folds.}
#'     \item{`candidates`}{Tibble of candidate models from [select_candidate_models()].}
#'     \item{`outer_scores`}{Tibble of outer-fold model evaluations from [score_outer()].}
#'     \item{`outer_coefficients`}{Long tibble of outer-fold coefficients from [tidy_outer_coefficients()].}
#'     \item{`importance`}{Predictor importance summary tibble from [summarize_outer_importance()].}
#'     \item{`censoring_comparison`}{Tibble from [compare_censoring_weights()] (when `compare_censoring = TRUE`).}
#'     \item{`run_dir`}{Normalized path to `run_dir` (or `NULL`).}
#'     \item{`fingerprint`}{SHA-256 fingerprint of the run configuration and data.}
#'     \item{`seed`}{RNG seed used.}
#'     \item{`seed_scheme`}{Seed generation scheme version (`"v0.5.6"`).}
#'     \item{`shard`}{Shard specification (`NULL` or `c(i, n)`).}
#'     \item{`status`}{`"completed"` or `"shard_completed"`.}
#'   }
#' @seealso [validate_start_stop()], [summarize_tune_results()],
#'   [select_candidate_models()], [score_outer()], [summarize_outer_importance()],
#'   [compare_censoring_weights()], [collect_checkpoints()], [repro_appendix()]
#' @export
nested_survival_run <- function(data,
                                recipe = NULL,
                                id = "id",
                                start = "tstart",
                                stop = "tstop",
                                status = "status",
                                domain = "default",
                                run_dir = NULL,
                                shard = NULL,
                                collect = FALSE,
                                feature_names = NULL,
                                formulas = NULL,
                                outer_v = 5L,
                                inner_v = 5L,
                                inner_bootstraps = NULL,
                                nested_folds = NULL,
                                alphas = NULL,
                                num_alpha_values = 6L,
                                num_fixed = min(6L, as.integer(num_alpha_values)),
                                ntile = 0.25,
                                top_n = NULL,
                                select_by = c("row", "alpha"),
                                internal_folds = 5L,
                                censoring_weights = c("ipcw", "none"),
                                coefficients = c("all", "nonzero"),
                                compare_censoring = TRUE,
                                no_censoring_action = c("warn", "stop", "ignore"),
                                error_on_invalid = TRUE,
                                checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                chunk_size = NULL,
                                seed = 42L,
                                on_complete = getOption("templecbe.on_complete", NULL),
                                progress = FALSE,
                                ...) {
  require_packages(c("glmnet", "survival", "rsample", "yardstick", "recipes"), "nested_survival_run")

  id_col <- if (is.character(id)) id[1] else rlang::as_name(rlang::ensym(id))
  start_col <- if (is.character(start)) start[1] else rlang::as_name(rlang::ensym(start))
  stop_col <- if (is.character(stop)) stop[1] else rlang::as_name(rlang::ensym(stop))
  status_col <- if (is.character(status)) status[1] else rlang::as_name(rlang::ensym(status))

  select_by <- match.arg(select_by)
  censoring_weights <- match.arg(censoring_weights)
  coefficients <- match.arg(coefficients)
  no_censoring_action <- match.arg(no_censoring_action)
  checkpoint_mismatch <- resolve_checkpoint_mismatch(
    checkpoint_mismatch,
    missing_arg = missing(checkpoint_mismatch)
  )
  shard <- validate_shard_spec(shard)
  dots <- list(...)

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  run_with_on_complete(
    fn_name = "nested_survival_run",
    on_complete = on_complete,
    meta = list(domain = domain, run_dir = run_dir, shard = shard, seed = seed),
    expr = {
      t0 <- proc.time()[["elapsed"]]

      # 1. Validate start/stop survival data first
      val <- validate_start_stop(
        data = data,
        id = id_col,
        start = start_col,
        stop = stop_col,
        status = status_col,
        warn_no_censoring = (no_censoring_action == "warn"),
        error_on_invalid = error_on_invalid
      )
      if (!isTRUE(val$has_censoring)) {
        if (no_censoring_action == "stop") {
          stop(
            sprintf(
              "Domain '%s': `validate_start_stop()` detected 0 censored subjects (%d/%d subjects experienced an event) and `no_censoring_action = \"stop\"`.",
              domain, val$n_subjects_with_event, val$n_subjects
            ),
            call. = FALSE
          )
        }
        rlang::inform(
          "Training data contains no censored subjects; 'none' and 'ipcw' censoring weights produce identical scores.",
          .frequency = "once",
          .frequency_id = "TempleCBE_no_censored_subjects_notice"
        )
      }

      # Resolve recipe & feature_names
      key_cols <- unique(c(id_col, start_col, stop_col, status_col))
      if (is.null(recipe)) {
        recipe <- recipes::recipe(~ ., data = data) |>
          recipes::update_role(dplyr::all_of(key_cols), new_role = "id variable")
      }
      if (is.null(feature_names)) {
        if (inherits(recipe, "recipe") && !is.null(recipe$var_info)) {
          roles <- recipe$var_info$role
          feature_names <- recipe$var_info$variable[!roles %in% c("id variable", "outcome", "id")]
        } else {
          feature_names <- setdiff(names(data), key_cols)
        }
      }
      if (length(feature_names) == 0L) {
        stop("Could not infer `feature_names`; supply `feature_names` or a recipe with predictor variables.", call. = FALSE)
      }
      if (is.null(formulas)) {
        formulas <- paste(feature_names, collapse = " + ")
      }

      time_data <- dots$time_data %||% unique(data[, c(start_col, stop_col), drop = FALSE])
      dots_clean <- dots[setdiff(names(dots), c("time_data", "id_col", "start_col", "stop_col", "status_col"))]

      # 2. Build nested grouped folds (reproducibly with seed)
      if (is.null(nested_folds)) {
        if (!is.null(seed)) local_seed(seed)
        id_sym <- rlang::sym(id_col)
        ov <- as.integer(outer_v)
        if (!is.null(inner_bootstraps)) {
          ib <- as.integer(inner_bootstraps)
          nested_folds <- rlang::inject(
            rsample::nested_cv(
              data,
              outside = rsample::group_vfold_cv(group = !!id_sym, v = !!ov),
              inside = rsample::group_bootstraps(group = !!id_sym, times = !!ib)
            )
          )
        } else {
          iv <- as.integer(inner_v)
          nested_folds <- rlang::inject(
            rsample::nested_cv(
              data,
              outside = rsample::group_vfold_cv(group = !!id_sym, v = !!ov),
              inside = rsample::group_vfold_cv(group = !!id_sym, v = !!iv)
            )
          )
        }
      }

      # Compute overall run fingerprint
      data_hash <- paste0(as.character(openssl::sha256(serialize(list(
        dim = dim(data),
        names = names(data),
        data = data
      ), NULL))), collapse = "")

      run_fp <- compute_checkpoint_fingerprint(
        split = NULL,
        dots = c(
          list(
            recipe = recipe,
            feature_names = feature_names,
            internal_folds = internal_folds,
            censoring_weights = censoring_weights,
            id_col = id_col,
            start_col = start_col,
            stop_col = stop_col,
            status_col = status_col
          ),
          dots_clean
        ),
        seed = seed,
        extra = list(
          fn = "cbe_nested_survival_run",
          domain = as.character(domain),
          data_hash = data_hash,
          outer_v = nrow(nested_folds),
          alphas = alphas,
          num_alpha_values = as.integer(num_alpha_values),
          num_fixed = as.integer(num_fixed),
          formulas = vapply(
            if (inherits(formulas, "formula")) list(formulas) else formulas,
            formula_terms_string,
            character(1),
            USE.NAMES = FALSE
          ),
          ntile = ntile,
          top_n = top_n,
          select_by = select_by,
          coefficients = coefficients,
          compare_censoring = isTRUE(compare_censoring)
        )
      )

      norm_run_dir <- NULL
      if (!is.null(run_dir)) {
        dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
        norm_run_dir <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
        cached_run_file <- file.path(norm_run_dir, "nested_survival_run.rds")
        if (is.null(shard) && !isTRUE(collect) && file.exists(cached_run_file)) {
          cached_run <- tryCatch(readRDS(cached_run_file), error = function(e) NULL)
          if (inherits(cached_run, "cbe_nested_survival_run") &&
              identical(cached_run$fingerprint, run_fp) &&
              identical(cached_run$seed_scheme, current_seed_scheme()) &&
              identical(cached_run$status, "completed")) {
            cached_run$resumed <- TRUE
            return(cached_run)
          }
        }
      }

      # 3. Inner tuning across outer folds
      n_outer <- nrow(nested_folds)
      outer_ids <- if ("id" %in% names(nested_folds)) as.character(nested_folds$id) else paste0("Fold", seq_len(n_outer))

      tune_list <- vector("list", n_outer)
      for (i in seq_len(n_outer)) {
        fold_id <- outer_ids[i]
        safe_fold <- gsub("[^A-Za-z0-9._-]", "_", fold_id)
        outer_ckpt_file <- if (!is.null(norm_run_dir)) {
          file.path(norm_run_dir, "checkpoints", sprintf("tune_outer_%s.rds", safe_fold))
        } else {
          NULL
        }
        fold_ckpt_dir <- if (!is.null(norm_run_dir)) {
          file.path(norm_run_dir, "checkpoints", paste0("tune_", safe_fold))
        } else {
          NULL
        }
        fold_seed <- if (!is.null(seed)) as.integer(seed) + i * 1000L else NULL
        inner_rset <- nested_folds$inner_resamples[[i]]

        fold_fp <- if (!is.null(norm_run_dir)) {
          compute_checkpoint_fingerprint(
            split = NULL,
            dots = dots_clean,
            seed = fold_seed,
            extra = list(
              fn = "nested_survival_run_outer_fold",
              fold = fold_id,
              alphas = alphas,
              num_alpha_values = num_alpha_values,
              formulas = formulas,
              run_fp = run_fp
            )
          )
        } else {
          NULL
        }

        t_df <- NULL
        if (!is.null(outer_ckpt_file) && file.exists(outer_ckpt_file) && is.null(shard)) {
          chk <- read_verified_checkpoint(outer_ckpt_file, fold_fp, checkpoint_mismatch = checkpoint_mismatch)
          if (identical(chk$status, "hit")) {
            t_df <- chk$value
          }
        }

        if (is.null(t_df)) {
          if (isTRUE(collect) && is.null(shard) && !is.null(fold_ckpt_dir) && dir.exists(fold_ckpt_dir)) {
            t_df <- collect_checkpoints(fold_ckpt_dir, type = "tune")
          } else {
            t_args <- c(
              list(
                object = inner_rset,
                recipe = recipe,
                feature_names = feature_names,
                alphas = alphas,
                num_alpha_values = num_alpha_values,
                num_fixed = num_fixed,
                time_data = time_data,
                internal_folds = internal_folds,
                censoring_weights = censoring_weights,
                id_col = id_col,
                start_col = start_col,
                stop_col = stop_col,
                status_col = status_col,
                seed = fold_seed,
                checkpoint_dir = fold_ckpt_dir,
                checkpoint_mismatch = checkpoint_mismatch,
                shard = shard,
                chunk_size = chunk_size,
                on_complete = NULL,
                progress = progress
              ),
              dots_clean
            )
            t_df <- do.call(tune_alpha_by_split, t_args)
            if (is.null(shard) && isTRUE(collect) && !is.null(fold_ckpt_dir)) {
              t_df <- collect_checkpoints(fold_ckpt_dir, type = "tune")
            }
          }
          if (!is.null(outer_ckpt_file) && is.null(shard) && is.data.frame(t_df)) {
            write_verified_checkpoint(
              t_df, outer_ckpt_file, fingerprint = fold_fp, run_fingerprint = run_fp,
              meta = list(fn = "nested_survival_run_outer_fold", fold = fold_id)
            )
            if (!is.null(fold_ckpt_dir) && dir.exists(fold_ckpt_dir)) {
              unlink(fold_ckpt_dir, recursive = TRUE)
            }
          }
        }
        t_df$outer_fold <- fold_id
        cols_order <- c("outer_fold", setdiff(names(t_df), "outer_fold"))
        tune_list[[i]] <- t_df[, cols_order, drop = FALSE]
      }
      tune_results <- dplyr::bind_rows(tune_list)

      # If sharding is active, check whether all tuning checkpoints exist across all shards
      # before proceeding to candidate selection, or if we can select candidates from collected/current tuning.
      all_tune_ready <- TRUE
      if (!is.null(shard) && !is.null(norm_run_dir)) {
        # Try collecting full tuning checkpoints if all shards have finished; otherwise use current shard's tune_results
        collected_tune <- tryCatch({
          lapply(seq_len(n_outer), function(i) {
            fold_id <- outer_ids[i]
            safe_fold <- gsub("[^A-Za-z0-9._-]", "_", fold_id)
            f_dir <- file.path(norm_run_dir, "checkpoints", paste0("tune_", safe_fold))
            df_i <- collect_checkpoints(f_dir, type = "tune")
            df_i$outer_fold <- fold_id
            df_i[, c("outer_fold", setdiff(names(df_i), "outer_fold")), drop = FALSE]
          })
        }, error = function(e) NULL)
        if (!is.null(collected_tune)) {
          expected_per_fold <- length(nested_folds$inner_resamples[[1]]$splits) *
            (if (!is.null(alphas)) length(alphas) else as.integer(num_alpha_values))
          actual_counts <- vapply(collected_tune, function(d) {
            nrow(dplyr::distinct(d[, intersect(c("inner_resamples_splits", "alpha"), names(d)), drop = FALSE]))
          }, integer(1))
          if (all(actual_counts >= expected_per_fold)) {
            tune_results <- dplyr::bind_rows(collected_tune)
          } else {
            all_tune_ready <- FALSE
          }
        } else {
          all_tune_ready <- FALSE
        }
      }

      # 4. Candidate model selection
      candidates <- select_candidate_models(
        tune_results = tune_results,
        formulas = formulas,
        ntile = ntile,
        top_n = top_n,
        by = select_by
      )

      # 5. Outer scoring
      score_ckpt_dir <- if (!is.null(norm_run_dir)) {
        file.path(norm_run_dir, "checkpoints", "score_outer")
      } else {
        NULL
      }
      score_consolidated_file <- if (!is.null(norm_run_dir)) {
        file.path(norm_run_dir, "checkpoints", paste0("score_outer_", censoring_weights, ".rds"))
      } else {
        NULL
      }
      score_seed <- if (!is.null(seed)) as.integer(seed) + 50000L else NULL
      score_fp <- if (!is.null(norm_run_dir)) {
        compute_checkpoint_fingerprint(
          split = NULL,
          dots = dots_clean,
          seed = score_seed,
          extra = list(
            fn = "nested_survival_run_score_outer",
            weights = censoring_weights,
            candidates = candidates,
            run_fp = run_fp
          )
        )
      } else {
        NULL
      }

      outer_scores <- NULL
      if (!is.null(score_consolidated_file) && file.exists(score_consolidated_file) && is.null(shard)) {
        chk_score <- read_verified_checkpoint(score_consolidated_file, score_fp, checkpoint_mismatch = checkpoint_mismatch)
        if (identical(chk_score$status, "hit")) {
          outer_scores <- chk_score$value
        }
      }

      if (is.null(outer_scores)) {
        if (isTRUE(collect) && is.null(shard) && !is.null(score_ckpt_dir) && dir.exists(score_ckpt_dir)) {
          # Ensure any missing outer tasks are scored and then collect all checkpoints
          outer_scores <- tryCatch(
            collect_checkpoints(score_ckpt_dir, type = "score_outer"),
            error = function(e) NULL
          )
          expected_outer_rows <- n_outer * nrow(candidates)
          if (is.null(outer_scores) || nrow(outer_scores) < expected_outer_rows) {
            s_args <- c(
              list(
                object = nested_folds,
                grid = candidates,
                recipe = recipe,
                feature_names = feature_names,
                time_data = time_data,
                internal_folds = internal_folds,
                censoring_weights = censoring_weights,
                coefficients = coefficients,
                id_col = id_col,
                start_col = start_col,
                stop_col = stop_col,
                status_col = status_col,
                seed = score_seed,
                checkpoint_dir = score_ckpt_dir,
                checkpoint_mismatch = checkpoint_mismatch,
                shard = NULL,
                chunk_size = chunk_size,
                on_complete = NULL,
                progress = progress
              ),
              dots_clean
            )
            outer_scores <- do.call(score_outer, s_args)
          }
        } else {
          s_args <- c(
            list(
              object = nested_folds,
              grid = candidates,
              recipe = recipe,
              feature_names = feature_names,
              time_data = time_data,
              internal_folds = internal_folds,
              censoring_weights = censoring_weights,
              coefficients = coefficients,
              id_col = id_col,
              start_col = start_col,
              stop_col = stop_col,
              status_col = status_col,
              seed = score_seed,
              checkpoint_dir = score_ckpt_dir,
              checkpoint_mismatch = checkpoint_mismatch,
              shard = shard,
              chunk_size = chunk_size,
              on_complete = NULL,
              progress = progress
            ),
            dots_clean
          )
          outer_scores <- do.call(score_outer, s_args)
        }
        if (!is.null(score_consolidated_file) && is.null(shard) && is.data.frame(outer_scores)) {
          write_verified_checkpoint(
            outer_scores, score_consolidated_file,
            fingerprint = score_fp, run_fingerprint = run_fp,
            meta = list(fn = "nested_survival_run_score_outer", weights = censoring_weights)
          )
          if (!is.null(score_ckpt_dir) && dir.exists(score_ckpt_dir)) {
            unlink(score_ckpt_dir, recursive = TRUE)
          }
        }
      }

      # 6. Outer coefficients and predictor importance
      outer_coefficients <- tidy_outer_coefficients(outer_scores)
      importance <- summarize_outer_importance(outer_coefficients)

      # 7. Censoring weight comparison (E3-aware: skips duplicate fit when no censoring)
      censoring_comparison <- NULL
      if (isTRUE(compare_censoring)) {
        has_cens <- isTRUE(val$has_censoring) &&
          has_censored_subjects_in_splits(
            nested_folds,
            recipe = recipe,
            dots = list(id_col = id_col, status_col = status_col)
          )
        if (!has_cens) {
          censoring_comparison <- compare_censoring_weights(outer_scores, outer_scores, seed = score_seed, on_complete = NULL)
          attr(censoring_comparison, "single_fit_no_censoring") <- TRUE
          attr(censoring_comparison, "n_score_outer_runs") <- 1L
        } else {
          alt_weights <- if (censoring_weights == "ipcw") "none" else "ipcw"
          alt_ckpt_dir <- if (!is.null(norm_run_dir)) {
            file.path(norm_run_dir, "checkpoints", paste0("score_outer_", alt_weights))
          } else {
            NULL
          }
          alt_consolidated_file <- if (!is.null(norm_run_dir)) {
            file.path(norm_run_dir, "checkpoints", paste0("score_outer_", alt_weights, ".rds"))
          } else {
            NULL
          }
          alt_fp <- if (!is.null(norm_run_dir)) {
            compute_checkpoint_fingerprint(
              split = NULL,
              dots = dots_clean,
              seed = score_seed,
              extra = list(
                fn = "nested_survival_run_score_outer",
                weights = alt_weights,
                candidates = candidates,
                run_fp = run_fp
              )
            )
          } else {
            NULL
          }

          alt_scores <- NULL
          if (!is.null(alt_consolidated_file) && file.exists(alt_consolidated_file) && is.null(shard)) {
            chk_alt <- read_verified_checkpoint(alt_consolidated_file, alt_fp, checkpoint_mismatch = checkpoint_mismatch)
            if (identical(chk_alt$status, "hit")) {
              alt_scores <- chk_alt$value
            }
          }

          if (is.null(alt_scores)) {
            alt_args <- c(
              list(
                object = nested_folds,
                grid = candidates,
                recipe = recipe,
                feature_names = feature_names,
                time_data = time_data,
                internal_folds = internal_folds,
                censoring_weights = alt_weights,
                coefficients = coefficients,
                id_col = id_col,
                start_col = start_col,
                stop_col = stop_col,
                status_col = status_col,
                seed = score_seed,
                checkpoint_dir = alt_ckpt_dir,
                checkpoint_mismatch = checkpoint_mismatch,
                shard = shard,
                chunk_size = chunk_size,
                on_complete = NULL,
                progress = progress
              ),
              dots_clean
            )
            alt_scores <- do.call(score_outer, alt_args)
            if (isTRUE(collect) && is.null(shard) && !is.null(alt_ckpt_dir) && dir.exists(alt_ckpt_dir)) {
              alt_scores <- tryCatch(collect_checkpoints(alt_ckpt_dir, type = "score_outer"), error = function(e) alt_scores)
            }
            if (!is.null(alt_consolidated_file) && is.null(shard) && is.data.frame(alt_scores)) {
              write_verified_checkpoint(
                alt_scores, alt_consolidated_file,
                fingerprint = alt_fp, run_fingerprint = run_fp,
                meta = list(fn = "nested_survival_run_score_outer", weights = alt_weights)
              )
              if (!is.null(alt_ckpt_dir) && dir.exists(alt_ckpt_dir)) {
                unlink(alt_ckpt_dir, recursive = TRUE)
              }
            }
          }
          tbl_none <- if (censoring_weights == "none") outer_scores else alt_scores
          tbl_ipcw <- if (censoring_weights == "ipcw") outer_scores else alt_scores
          censoring_comparison <- compare_censoring_weights(tbl_none, tbl_ipcw, seed = score_seed, on_complete = NULL)
          attr(censoring_comparison, "single_fit_no_censoring") <- FALSE
          attr(censoring_comparison, "n_score_outer_runs") <- 2L
        }
      }

      elapsed_sec <- max(0, proc.time()[["elapsed"]] - t0)
      run_status <- if (is.null(shard)) "completed" else "shard_completed"

      manifest_path <- NULL
      if (!is.null(norm_run_dir)) {
        timing_df <- data.frame(
          domain = as.character(domain),
          status = run_status,
          duration = round(elapsed_sec, 3),
          shard = if (is.null(shard)) "all" else paste(shard, collapse = "/"),
          stringsAsFactors = FALSE
        )
        manifest_path <- tryCatch(
          write_run_manifest(norm_run_dir, timing_df, seed = seed, seed_scheme = current_seed_scheme()),
          error = function(e) NULL
        )
      }

      res_obj <- structure(
        list(
          domain = as.character(domain),
          validation = val,
          nested_folds = nested_folds,
          tune_results = tune_results,
          candidates = candidates,
          outer_scores = outer_scores,
          outer_coefficients = outer_coefficients,
          importance = importance,
          censoring_comparison = censoring_comparison,
          run_dir = norm_run_dir,
          manifest_path = manifest_path,
          fingerprint = run_fp,
          data_hash = data_hash,
          seed = seed,
          seed_scheme = current_seed_scheme(),
          shard = shard,
          status = run_status,
          resumed = FALSE,
          elapsed_sec = elapsed_sec,
          columns = list(
            id = id_col,
            start = start_col,
            stop = stop_col,
            status = status_col,
            feature_names = feature_names
          )
        ),
        class = c("nested_survival_run", "cbe_nested_survival_run")
      )

      if (!is.null(norm_run_dir) && is.null(shard)) {
        tryCatch(
          saveRDS(res_obj, file.path(norm_run_dir, "nested_survival_run.rds")),
          error = function(e) NULL
        )
      }

      res_obj
    }
  )
}

#' @rdname nested_survival_run
#' @export
cbe_nested_survival_run <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "cbe_nested_survival_run()", "nested_survival_run()")
  nested_survival_run(...)
}

#' @export
print.nested_survival_run <- function(x, ...) {
  print.cbe_nested_survival_run(x, ...)
}

#' @export
print.cbe_nested_survival_run <- function(x, ...) {
  cat(sprintf("<cbe_nested_survival_run> Domain: %s [%s]\n", x$domain, x$status))
  cat(sprintf("  Subjects: %d (Events: %d, Censored: %d [%.1f%%])\n",
              x$validation$n_subjects,
              x$validation$n_subjects_with_event,
              x$validation$n_censored,
              100 * x$validation$censoring_rate))
  cat(sprintf("  Outer folds: %d | Candidate models: %d | Outer fits: %d\n",
              nrow(x$nested_folds),
              nrow(x$candidates),
              nrow(x$outer_scores)))
  cat(sprintf("  Median outer IBS: %.4f (IQR: %.4f) | Seed: %s (%s)\n",
              stats::median(x$outer_scores$IBS, na.rm = TRUE),
              stats::IQR(x$outer_scores$IBS, na.rm = TRUE),
              as.character(x$seed %||% "NULL"),
              x$seed_scheme))
  if (!is.null(x$run_dir)) {
    cat(sprintf("  Run directory: %s (fingerprint: %s)\n",
                x$run_dir, substr(x$fingerprint, 1, 12)))
  }
  invisible(x)
}

#' Summarize a `cbe_nested_survival_run` Object
#'
#' @param object A [cbe_nested_survival_run()] object.
#' @param ... Unused.
#' @return A list of class `summary.cbe_nested_survival_run` with elements
#'   `overview` (1-row tibble of run metrics), `model_summary` (per-model outer
#'   IBS summary), `top_importance` (top predictors by selection frequency),
#'   and `censoring_agreement` (Spearman and Kendall rank correlations).
#' @export
summary.cbe_nested_survival_run <- function(object, ...) {
  scores <- object$outer_scores
  mod_sum <- scores |>
    dplyr::group_by(.data$model, .data$alpha, .data$formula) |>
    dplyr::summarise(
      n_folds = dplyr::n(),
      mean_ibs = mean(.data$IBS, na.rm = TRUE),
      median_ibs = stats::median(.data$IBS, na.rm = TRUE),
      sd_ibs = stats::sd(.data$IBS, na.rm = TRUE),
      iqr_ibs = stats::IQR(.data$IBS, na.rm = TRUE),
      mean_lambda = mean(.data$lambda, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$median_ibs, .data$mean_ibs)

  best_mod <- if (nrow(mod_sum) > 0L) mod_sum$model[1] else NA_character_
  best_alpha <- if (nrow(mod_sum) > 0L) mod_sum$alpha[1] else NA_real_
  rank_agr <- if (!is.null(object$censoring_comparison)) {
    attr(object$censoring_comparison, "rank_agreement")
  } else {
    list(spearman = NA_real_, kendall = NA_real_)
  }

  overview <- tibble::tibble(
    domain = object$domain,
    status = object$status,
    n_subjects = object$validation$n_subjects,
    n_events = object$validation$n_events,
    n_censored = object$validation$n_censored,
    has_censoring = object$validation$has_censoring,
    n_outer_folds = nrow(object$nested_folds),
    n_candidates = nrow(object$candidates),
    best_model = best_mod,
    best_alpha = best_alpha,
    median_outer_ibs = stats::median(scores$IBS, na.rm = TRUE),
    mean_outer_ibs = mean(scores$IBS, na.rm = TRUE),
    spearman_censoring = rank_agr$spearman %||% NA_real_,
    kendall_censoring = rank_agr$kendall %||% NA_real_,
    seed = object$seed %||% NA_integer_,
    seed_scheme = object$seed_scheme,
    fingerprint = object$fingerprint
  )

  structure(
    list(
      overview = overview,
      model_summary = mod_sum,
      top_importance = utils::head(object$importance, 10),
      censoring_agreement = rank_agr
    ),
    class = "summary.cbe_nested_survival_run"
  )
}

#' @export
print.summary.cbe_nested_survival_run <- function(x, ...) {
  ov <- x$overview
  cat(sprintf("Summary: Nested Survival Run (Domain: %s)\n", ov$domain))
  cat("--------------------------------------------------\n")
  cat(sprintf("Cohort:       %d subjects (%d events, %d censored)\n",
              ov$n_subjects, ov$n_events, ov$n_censored))
  cat(sprintf("Best model:   %s (alpha = %.3f)\n", ov$best_model, ov$best_alpha))
  cat(sprintf("Outer IBS:    median = %.4f, mean = %.4f\n",
              ov$median_outer_ibs, ov$mean_outer_ibs))
  if (!is.na(ov$spearman_censoring)) {
    cat(sprintf("IPCW vs None: Spearman = %.3f, Kendall = %.3f\n",
                ov$spearman_censoring, ov$kendall_censoring))
  }
  if (nrow(x$top_importance) > 0L) {
    cat("\nTop Predictors by Outer-Fold Selection Frequency:\n")
    print(x$top_importance[, c("term", "label", "selection_freq", "median_estimate")], n = 5)
  }
  invisible(x)
}

#' Plot a `cbe_nested_survival_run` Object
#'
#' @param x,object A [cbe_nested_survival_run()] object.
#' @param type Plot type:
#'   \describe{
#'     \item{`"importance"`}{(Default) Outer-fold coefficient importance boxplot via [plot_outer_importance()].}
#'     \item{`"frequency"`}{Outer-fold predictor selection frequency bar chart via [plot_outer_importance()].}
#'     \item{`"outer_ibs"`}{Distribution of outer-fold Integrated Brier Scores across candidate models.}
#'     \item{`"tuning"`}{Inner tuning Integrated Brier Score across `alpha`.}
#'     \item{`"censoring"`}{Scatter comparison of unweighted vs IPCW outer-fold IBS.}
#'   }
#' @param top_n Optional integer restricting importance plots to the top `top_n` predictors.
#' @param ... Additional arguments passed to [plot_outer_importance()].
#' @return A `ggplot` object.
#' @export
plot.cbe_nested_survival_run <- function(x,
                                         type = c("importance", "frequency", "outer_ibs", "tuning", "censoring"),
                                         top_n = NULL,
                                         ...) {
  require_packages("ggplot2", "plot.cbe_nested_survival_run")
  type <- match.arg(type)

  if (type == "importance") {
    return(plot_outer_importance(x$outer_coefficients, top_n = top_n, type = "boxplot", ...))
  }
  if (type == "frequency") {
    return(plot_outer_importance(x$importance, top_n = top_n, type = "frequency", ...))
  }
  if (type == "outer_ibs") {
    df <- x$outer_scores
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$model, .data$IBS, FUN = stats::median), y = .data$IBS)) +
        ggplot2::geom_boxplot(fill = "#56B4E9", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = sprintf("Outer-Fold Integrated Brier Score (%s)", x$domain),
          x = "Candidate Model",
          y = "Integrated Brier Score (IBS)"
        ) +
        ggplot2::theme_minimal()
    )
  }
  if (type == "tuning") {
    df <- x$tune_results |>
      dplyr::distinct(.data$outer_fold, .data$inner_resamples_splits, .data$alpha, .data$IBS)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$alpha, y = .data$IBS)) +
        ggplot2::geom_point(alpha = 0.5, color = "#0072B2") +
        ggplot2::stat_summary(fun = stats::median, geom = "line", color = "#D55E00", linewidth = 0.9) +
        ggplot2::labs(
          title = sprintf("Inner Tuning IBS Profile (%s)", x$domain),
          subtitle = "Points: inner splits; line: median IBS across splits",
          x = "Elastic-Net Mixing Parameter (alpha)",
          y = "Integrated Brier Score (IBS)"
        ) +
        ggplot2::theme_minimal()
    )
  }
  if (type == "censoring") {
    if (is.null(x$censoring_comparison)) {
      stop("`censoring_comparison` is NULL (run with `compare_censoring = TRUE`).", call. = FALSE)
    }
    df <- x$censoring_comparison
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$ibs_none, y = .data$ibs_ipcw)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        ggplot2::geom_point(size = 2.5, color = "#0072B2", alpha = 0.8) +
        ggplot2::labs(
          title = sprintf("Censoring Weight Sensitivity (%s)", x$domain),
          x = "Unweighted IBS ('none')",
          y = "IPCW IBS ('ipcw')"
        ) +
        ggplot2::theme_minimal()
    )
  }
}

#' @rdname plot.cbe_nested_survival_run
#' @exportS3Method ggplot2::autoplot
autoplot.cbe_nested_survival_run <- function(object,
                                             type = c("importance", "frequency", "outer_ibs", "tuning", "censoring"),
                                             top_n = NULL,
                                             ...) {
  plot.cbe_nested_survival_run(object, type = type, top_n = top_n, ...)
}

#' Tidy a `cbe_nested_survival_run` Object
#'
#' Extracts a tidy tibble from a [cbe_nested_survival_run()] object.
#'
#' @param x A [cbe_nested_survival_run()] object.
#' @param component Which component table to extract:
#'   `"importance"` (default; predictor selection frequencies and coefficient
#'   summaries), `"outer_scores"`, `"coefficients"`, `"candidates"`, `"tuning"`,
#'   `"censoring"`, `"validation"`, or `"overview"`.
#' @param ... Unused.
#' @return A tibble.
#' @exportS3Method generics::tidy
tidy.cbe_nested_survival_run <- function(x,
                                         component = c(
                                           "importance",
                                           "outer_scores",
                                           "coefficients",
                                           "candidates",
                                           "tuning",
                                           "censoring",
                                           "validation",
                                           "overview"
                                         ),
                                         ...) {
  component <- match.arg(component)
  switch(
    component,
    importance = x$importance,
    outer_scores = x$outer_scores,
    coefficients = x$outer_coefficients,
    candidates = x$candidates,
    tuning = x$tune_results,
    censoring = x$censoring_comparison %||% tibble::tibble(),
    validation = tibble::as_tibble(x$validation),
    overview = summary.cbe_nested_survival_run(x)$overview
  )
}


# ==============================================================================
# E2: Self-Rendering Reproducibility Appendix (`repro_appendix()`)
# ==============================================================================

#' Generate a Self-Rendering Reproducibility Appendix (E2)
#'
#' Collects and renders a standardized reproducibility record for Quarto and
#' R Markdown reports, including:
#' \enumerate{
#'   \item Package versions and R session platform metadata
#'   \item Git commit SHA and working tree status
#'   \item RNG seed and task `seed_scheme` version (`"v0.5.6"`)
#'   \item Active `future::plan()` and worker count
#'   \item SHA-256 content hashes of input datasets or pipeline hash manifests
#'   \item Pre-flight [validate_start_stop()] summary
#'   \item Checkpoint directory fingerprints and `seed_scheme` audit
#'   \item Render manifest (`render_manifest.json`) summary
#' }
#'
#' Can be called directly inside a report chunk (where its `knit_print` and
#' `print` methods emit ready-to-render Markdown), or referenced via the
#' packaged Quarto child template returned by [repro_appendix_template()].
#'
#' @param run Optional [cbe_nested_survival_run()] object (or list of such
#'   objects across domains). When supplied, `data`, `validation`, `run_dir`,
#'   `seed`, and `checkpoint_fingerprints` are extracted from `run` automatically.
#' @param data Optional data frame or named list of data frames to hash with
#'   SHA-256 (and validate with [validate_start_stop()] if `validation` is not
#'   supplied).
#' @param validation Optional [validate_start_stop()] result.
#' @param run_dir Optional directory path containing checkpoints and/or
#'   `render_manifest.json`.
#' @param manifest_path Optional explicit path to `render_manifest.json`.
#' @param seed Optional integer seed used for the report or run.
#' @param id,start,stop,status Column names passed to [validate_start_stop()]
#'   when `data` is a survival data frame and `validation` is `NULL`.
#' @param packages Character vector of packages whose versions should be
#'   recorded.
#' @param title Section heading for the appendix (default
#'   `"Reproducibility Appendix"`).
#' @param heading_level Integer Markdown heading level (default `2L`, i.e. `##`).
#' @param format Output representation: `"markdown"` (default; returns a
#'   character object of class `c("cbe_repro_appendix", "knit_asis")` with all
#'   structured metadata attached in attributes and `$` accessors) or `"list"`
#'   (returns the structured list directly).
#' @return An object of class `cbe_repro_appendix` containing the rendered
#'   Markdown string and structured fields `package_versions`, `git`, `seed`,
#'   `seed_scheme`, `future_plan`, `data_hashes`, `validation`,
#'   `checkpoint_fingerprints`, `render_manifest`, and `markdown`.
#' @seealso [repro_appendix_template()], [write_run_manifest()],
#'   [validate_start_stop()], [cbe_nested_survival_run()]
#' @export
repro_appendix <- function(run = NULL,
                           data = NULL,
                           validation = NULL,
                           run_dir = NULL,
                           manifest_path = NULL,
                           seed = NULL,
                           id = "id",
                           start = "tstart",
                           stop = "tstop",
                           status = "status",
                           packages = c(
                             "TempleCBE", "survival", "glmnet", "recipes",
                             "rsample", "yardstick", "furrr", "future",
                             "quarto", "rmarkdown", "renv"
                           ),
                           title = "Reproducibility Appendix",
                           heading_level = 2L,
                           format = c("markdown", "list")) {
  format <- match.arg(format)

  # Extract from `run` if provided
  if (inherits(run, "cbe_nested_survival_run")) {
    validation <- validation %||% run$validation
    run_dir <- run_dir %||% run$run_dir
    manifest_path <- manifest_path %||% run$manifest_path
    seed <- seed %||% run$seed
  } else if (is.list(run) && length(run) > 0L && all(vapply(run, inherits, logical(1), "cbe_nested_survival_run"))) {
    if (is.null(validation)) {
      validation <- dplyr::bind_rows(lapply(run, function(r) {
        v <- tibble::as_tibble(r$validation)
        v$domain <- r$domain
        v[, c("domain", setdiff(names(v), "domain")), drop = FALSE]
      }))
    }
    if (is.null(run_dir)) {
      dirs <- unique(unlist(lapply(run, function(r) r$run_dir)))
      if (length(dirs) > 0L) run_dir <- dirs
    }
    if (is.null(seed)) {
      seed <- run[[1]]$seed
    }
  }

  # 1. Package versions
  pkg_rows <- lapply(unique(packages), function(p) {
    ver <- if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      "not installed"
    }
    tibble::tibble(package = p, version = ver)
  })
  pkg_tbl <- dplyr::bind_rows(pkg_rows)

  # 2. Git commit & branch
  git_sha <- tryCatch({
    res <- suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE))
    st <- attr(res, "status")
    if ((is.null(st) || st == 0) && length(res) > 0 && nzchar(res[1])) trimws(res[1]) else NA_character_
  }, error = function(e) NA_character_)

  git_branch <- tryCatch({
    res <- suppressWarnings(system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE, stderr = FALSE))
    st <- attr(res, "status")
    if ((is.null(st) || st == 0) && length(res) > 0 && nzchar(res[1])) trimws(res[1]) else NA_character_
  }, error = function(e) NA_character_)

  git_info <- list(sha = git_sha, branch = git_branch)

  # 3. Seed and seed_scheme
  resolved_seed <- if (!is.null(seed)) {
    seed
  } else if (exists(".Random.seed", envir = .GlobalEnv)) {
    .GlobalEnv$.Random.seed[1:min(2, length(.GlobalEnv$.Random.seed))]
  } else {
    NA_integer_
  }
  seed_scheme <- if (inherits(run, "cbe_nested_survival_run") && !is.null(run$seed_scheme)) {
    run$seed_scheme
  } else {
    current_seed_scheme()
  }

  # 4. Future plan & workers
  plan_class <- tryCatch({
    if (requireNamespace("future", quietly = TRUE)) {
      paste(class(future::plan()), collapse = "/")
    } else {
      "sequential"
    }
  }, error = function(e) "sequential")
  n_workers <- tryCatch({
    if (requireNamespace("future", quietly = TRUE)) {
      as.integer(future::nbrOfWorkers())
    } else {
      1L
    }
  }, error = function(e) 1L)
  future_info <- list(plan = plan_class, workers = n_workers)

  # 5. Data hashes
  hash_rows <- list()
  if (inherits(run, "cbe_nested_survival_run") && !is.null(run$data_hash)) {
    hash_rows[[length(hash_rows) + 1L]] <- tibble::tibble(
      dataset = paste0("run:", run$domain),
      rows = run$validation$n_intervals %||% NA_integer_,
      cols = length(run$columns$feature_names) + 4L,
      sha256 = paste0(as.character(run$data_hash), collapse = "")
    )
  } else if (is.list(run) && length(run) > 0L && all(vapply(run, inherits, logical(1), "cbe_nested_survival_run"))) {
    for (r in run) {
      hash_rows[[length(hash_rows) + 1L]] <- tibble::tibble(
        dataset = paste0("run:", r$domain),
        rows = r$validation$n_intervals %||% NA_integer_,
        cols = length(r$columns$feature_names) + 4L,
        sha256 = paste0(as.character(r$data_hash %||% NA_character_), collapse = "")
      )
    }
  }
  if (!is.null(data)) {
    data_list <- if (is.data.frame(data)) {
      list(data = data)
    } else if (is.list(data)) {
      data
    } else {
      list(data = data)
    }
    nms <- names(data_list) %||% paste0("data_", seq_along(data_list))
    for (i in seq_along(data_list)) {
      d_i <- data_list[[i]]
      nm_i <- if (nzchar(nms[i])) nms[i] else paste0("data_", i)
      if (is.data.frame(d_i)) {
        h_i <- paste0(as.character(openssl::sha256(serialize(list(
          dim = dim(d_i),
          names = names(d_i),
          data = d_i
        ), NULL))), collapse = "")
        hash_rows[[length(hash_rows) + 1L]] <- tibble::tibble(
          dataset = nm_i,
          rows = nrow(d_i),
          cols = ncol(d_i),
          sha256 = h_i
        )
      } else if (is.character(d_i) && length(d_i) == 1L && file.exists(d_i)) {
        con <- file(d_i, "rb")
        h_file <- paste0(as.character(openssl::sha256(con)), collapse = "")
        close(con)
        hash_rows[[length(hash_rows) + 1L]] <- tibble::tibble(
          dataset = basename(d_i),
          rows = NA_integer_,
          cols = NA_integer_,
          sha256 = h_file
        )
      }
    }
  }
  data_hashes <- if (length(hash_rows) > 0L) {
    dplyr::bind_rows(hash_rows)
  } else {
    tibble::tibble(dataset = character(), rows = integer(), cols = integer(), sha256 = character())
  }

  # 6. validate_start_stop() result
  if (is.null(validation) && is.data.frame(data) && all(c(id, start, stop, status) %in% names(data))) {
    validation <- tryCatch(
      suppressWarnings(validate_start_stop(
        data,
        id = id,
        start = start,
        stop = stop,
        status = status,
        warn_no_censoring = FALSE,
        error_on_invalid = FALSE
      )),
      error = function(e) NULL
    )
  }
  validation_tbl <- if (!is.null(validation)) tibble::as_tibble(validation) else tibble::tibble()

  # 7. Checkpoint fingerprints
  ckpt_rows <- list()
  if (inherits(run, "cbe_nested_survival_run")) {
    ckpt_rows[[length(ckpt_rows) + 1L]] <- tibble::tibble(
      source = paste0("run:", run$domain),
      family = "cbe_nested_survival_run",
      n_files = NA_integer_,
      run_fingerprint = as.character(run$fingerprint),
      seed_scheme = as.character(run$seed_scheme)
    )
  }
  if (!is.null(run_dir)) {
    for (rd in as.character(run_dir)) {
      if (dir.exists(rd)) {
        inspected <- tryCatch(
          clean_checkpoints(rd, keep = "current", recursive = TRUE, dry_run = TRUE),
          error = function(e) NULL
        )
        if (!is.null(inspected) && nrow(inspected) > 0L) {
          grp <- inspected |>
            dplyr::group_by(.data$family, .data$run_fingerprint, .data$seed_scheme) |>
            dplyr::summarise(n_files = dplyr::n(), .groups = "drop")
          grp$source <- basename(normalizePath(rd, winslash = "/", mustWork = FALSE))
          ckpt_rows[[length(ckpt_rows) + 1L]] <- grp[, c("source", "family", "n_files", "run_fingerprint", "seed_scheme"), drop = FALSE]
        }
      }
    }
  }
  checkpoint_fingerprints <- if (length(ckpt_rows) > 0L) {
    dplyr::bind_rows(ckpt_rows)
  } else {
    tibble::tibble(
      source = character(),
      family = character(),
      n_files = integer(),
      run_fingerprint = character(),
      seed_scheme = character()
    )
  }

  # 8. Render manifest
  resolved_manifest_path <- manifest_path
  if (is.null(resolved_manifest_path) && !is.null(run_dir)) {
    for (rd in as.character(run_dir)) {
      cand <- file.path(rd, "render_manifest.json")
      if (file.exists(cand)) {
        resolved_manifest_path <- cand
        break
      }
    }
  }
  if (is.null(resolved_manifest_path) && file.exists("render_manifest.json")) {
    resolved_manifest_path <- normalizePath("render_manifest.json", winslash = "/", mustWork = FALSE)
  }

  render_manifest <- if (!is.null(resolved_manifest_path) && file.exists(resolved_manifest_path) &&
                         requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(jsonlite::read_json(resolved_manifest_path, simplifyVector = TRUE), error = function(e) NULL)
  } else {
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      templecbe_version = templecbe_version(),
      git_sha = git_sha,
      future_plan = plan_class,
      seed = resolved_seed,
      seed_scheme = seed_scheme
    )
  }

  # Build Markdown representation
  h_prefix <- paste(rep("#", max(1L, as.integer(heading_level))), collapse = "")
  h_sub <- paste(rep("#", max(2L, as.integer(heading_level) + 1L)), collapse = "")

  md_lines <- c(
    sprintf("%s %s", h_prefix, title),
    "",
    sprintf("%s Environment & Execution Metadata", h_sub),
    "",
    "| Field | Value |",
    "| :--- | :--- |",
    sprintf("| **R Version** | `%s` (%s) |", R.version.string, R.version$platform),
    sprintf("| **TempleCBE Version** | `%s` |", templecbe_version()),
    sprintf("| **Git Commit (Branch)** | `%s` (`%s`) |",
            ifelse(is.na(git_sha), "N/A", substr(git_sha, 1, 12)),
            ifelse(is.na(git_branch), "N/A", git_branch)),
    sprintf("| **RNG Seed** | `%s` |", paste(resolved_seed, collapse = ", ")),
    sprintf("| **Seed Scheme** | `%s` |", seed_scheme),
    sprintf("| **Future Plan** | `%s` (%d worker(s)) |", plan_class, n_workers),
    sprintf("| **Manifest Timestamp** | `%s` |", render_manifest$timestamp %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    ""
  )

  # Package versions table
  md_lines <- c(
    md_lines,
    sprintf("%s Package Versions", h_sub),
    "",
    "| Package | Version |",
    "| :--- | :--- |",
    vapply(seq_len(nrow(pkg_tbl)), function(i) {
      sprintf("| `%s` | `%s` |", pkg_tbl$package[i], pkg_tbl$version[i])
    }, character(1)),
    ""
  )

  # Data hashes table
  if (nrow(data_hashes) > 0L) {
    md_lines <- c(
      md_lines,
      sprintf("%s Data Content Hashes (SHA-256)", h_sub),
      "",
      "| Dataset | Rows | Cols | SHA-256 |",
      "| :--- | ---: | ---: | :--- |",
      vapply(seq_len(nrow(data_hashes)), function(i) {
        sprintf("| `%s` | %s | %s | `%s` |",
                data_hashes$dataset[i],
                ifelse(is.na(data_hashes$rows[i]), "-", as.character(data_hashes$rows[i])),
                ifelse(is.na(data_hashes$cols[i]), "-", as.character(data_hashes$cols[i])),
                data_hashes$sha256[i])
      }, character(1)),
      ""
    )
  }

  # validate_start_stop() table
  if (nrow(validation_tbl) > 0L) {
    md_lines <- c(
      md_lines,
      sprintf("%s Counting-Process Validation (`validate_start_stop()`)", h_sub),
      "",
      "| Subjects | Intervals | Events | Censored | Censoring Rate | Valid |",
      "| ---: | ---: | ---: | ---: | ---: | :--- |",
      vapply(seq_len(nrow(validation_tbl)), function(i) {
        sprintf("| %d | %d | %d | %d | %.1f%% | %s |",
                validation_tbl$n_subjects[i],
                validation_tbl$n_intervals[i],
                validation_tbl$n_events[i],
                validation_tbl$n_censored[i],
                100 * validation_tbl$censoring_rate[i],
                ifelse(isTRUE(validation_tbl$valid[i]), "PASS", "FAIL"))
      }, character(1)),
      ""
    )
  }

  # Checkpoint fingerprints table
  if (nrow(checkpoint_fingerprints) > 0L) {
    md_lines <- c(
      md_lines,
      sprintf("%s Checkpoint Fingerprints", h_sub),
      "",
      "| Source | Family | Files | Fingerprint | Seed Scheme |",
      "| :--- | :--- | ---: | :--- | :--- |",
      vapply(seq_len(nrow(checkpoint_fingerprints)), function(i) {
        sprintf("| `%s` | `%s` | %s | `%s` | `%s` |",
                checkpoint_fingerprints$source[i],
                checkpoint_fingerprints$family[i],
                ifelse(is.na(checkpoint_fingerprints$n_files[i]), "-", as.character(checkpoint_fingerprints$n_files[i])),
                checkpoint_fingerprints$run_fingerprint[i],
                checkpoint_fingerprints$seed_scheme[i])
      }, character(1)),
      ""
    )
  }

  md_text <- paste(md_lines, collapse = "\n")

  fields <- list(
    package_versions = pkg_tbl,
    git = git_info,
    seed = resolved_seed,
    seed_scheme = seed_scheme,
    future_plan = future_info,
    data_hashes = data_hashes,
    validation = validation_tbl,
    checkpoint_fingerprints = checkpoint_fingerprints,
    render_manifest = render_manifest,
    manifest_path = resolved_manifest_path,
    markdown = md_text
  )

  if (format == "list") {
    class(fields) <- c("cbe_repro_appendix_list", "cbe_repro_appendix", "list")
    return(fields)
  }

  out <- md_text
  for (nm in names(fields)) {
    attr(out, nm) <- fields[[nm]]
  }
  class(out) <- c("cbe_repro_appendix", "knit_asis", "character")
  out
}

#' Path to the Ready-Made Quarto Reproducibility Appendix Child Template
#'
#' Returns the file path to the packaged `repro_appendix.qmd` child document so
#' Quarto and R Markdown reports can include it directly via
#' `knitr::knit_child(repro_appendix_template(), quiet = TRUE)`.
#'
#' @return Character file path to `inst/templates/repro_appendix.qmd`.
#' @seealso [repro_appendix()]
#' @export
repro_appendix_template <- function() {
  p <- system.file("templates", "repro_appendix.qmd", package = "TempleCBE")
  if (!nzchar(p) || !file.exists(p)) {
    cand <- file.path("inst", "templates", "repro_appendix.qmd")
    if (file.exists(cand)) {
      p <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    }
  }
  p
}

#' @export
`$.cbe_repro_appendix` <- function(x, name) {
  if (is.list(x)) {
    return(x[[name]])
  }
  attr(x, name, exact = TRUE)
}

#' @export
print.cbe_repro_appendix <- function(x, ...) {
  if (is.character(x)) {
    cat(as.character(x), "\n", sep = "")
  } else if (is.list(x) && !is.null(x$markdown)) {
    cat(x$markdown, "\n", sep = "")
  }
  invisible(x)
}


# ==============================================================================
# E4: Per-Domain Child Template Expansion & Error Isolation (`cbe_domain_children()`)
# ==============================================================================

#' Expand and Knit Per-Domain Child Templates With Failure Isolation (E4)
#'
#' Loops over `domains`, expands a child Quarto (`.qmd`) or R Markdown (`.Rmd`)
#' template for each domain using [knitr::knit_expand()], and evaluates each
#' child in its own isolated environment via [knitr::knit_child()]. If a domain
#' fails (when `on_error = "continue"`), the error is caught, logged in the
#' returned status table, and rendered as a structured warning callout so
#' remaining domains continue to compile cleanly.
#'
#' Inside the child template, the current domain is available both as the
#' \code{\{\{domain\}\}} [knitr::knit_expand()] placeholder and as the `domain` variable
#' in the child evaluation environment (`envir`). Any additional named arguments
#' in `...` are passed to [knitr::knit_expand()] and bound in the child's
#' environment (if a named argument has the same length as `domains` or is
#' named by `domains`, each domain receives its corresponding element).
#'
#' @param domains Character vector of domain names (or a named list of
#'   per-domain parameter lists).
#' @param child Path to a child `.qmd` / `.Rmd` file, or a character string
#'   containing inline child template text.
#' @param ... Additional template variables passed to [knitr::knit_expand()] and
#'   bound in each domain's child evaluation environment.
#' @param envir Parent environment for child evaluation (defaults to
#'   [parent.frame()]). Each domain executes in `new.env(parent = envir)` so
#'   variables created in one domain never leak into or overwrite another.
#' @param on_error Behavior when a domain child errors: `"continue"` (default;
#'   records the failure, emits a warning, and continues with remaining domains)
#'   or `"stop"` (stops immediately).
#' @param quiet Logical (default `TRUE`). Passed to [knitr::knit_child()].
#' @return A character object of class `c("cbe_domain_children", "knit_asis", "character")`
#'   containing the combined knitted output across domains, with attributes and
#'   `$` accessors:
#'   \describe{
#'     \item{`status`}{Tibble with columns `domain`, `status` (`"succeeded"` or
#'       `"failed"`), `error`, `duration_sec`, and `output`.}
#'     \item{`failed_domains`}{Character vector of domain names that failed.}
#'     \item{`succeeded_domains`}{Character vector of domain names that succeeded.}
#'   }
#' @seealso [repro_appendix()], [cbe_nested_survival_run()], [render()]
#' @export
domain_children <- function(domains,
                            child,
                            ...,
                            envir = parent.frame(),
                            on_error = c("continue", "stop"),
                            quiet = TRUE) {
  require_packages("knitr", "domain_children")
  on_error <- match.arg(on_error)
  dots <- list(...)

  if (is.list(domains) && !is.null(names(domains)) && all(nzchar(names(domains)))) {
    domain_names <- names(domains)
    domain_params <- domains
  } else if (is.character(domains) && length(domains) > 0L) {
    domain_names <- as.character(domains)
    domain_params <- stats::setNames(lapply(domain_names, function(d) list()), domain_names)
  } else {
    stop("`domains` must be a non-empty character vector or named list.", call. = FALSE)
  }

  if (!is.character(child) || length(child) == 0L) {
    stop("`child` must be a file path or template character string.", call. = FALSE)
  }

  template_text <- if (length(child) == 1L && file.exists(child)) {
    paste(readLines(child, warn = FALSE), collapse = "\n")
  } else {
    paste(child, collapse = "\n")
  }

  n_dom <- length(domain_names)
  status_rows <- vector("list", n_dom)
  rendered_chunks <- character(n_dom)

  for (i in seq_len(n_dom)) {
    dom <- domain_names[i]
    t0 <- proc.time()[["elapsed"]]

    # Resolve per-domain variables from domain_params and dots
    dom_vars <- list(domain = dom)
    if (is.list(domain_params[[i]])) {
      for (k in names(domain_params[[i]])) {
        dom_vars[[k]] <- domain_params[[i]][[k]]
      }
    }
    if (length(dots) > 0L) {
      for (k in names(dots)) {
        val <- dots[[k]]
        if (is.list(val) && !is.data.frame(val) && !inherits(val, "recipe") && !inherits(val, "rset")) {
          if (!is.null(names(val)) && dom %in% names(val)) {
            dom_vars[[k]] <- val[[dom]]
          } else if (length(val) == n_dom) {
            dom_vars[[k]] <- val[[i]]
          } else {
            dom_vars[[k]] <- val
          }
        } else if (is.atomic(val) && length(val) == n_dom && n_dom > 1L &&
                   (!is.null(names(val)) && dom %in% names(val))) {
          dom_vars[[k]] <- val[[dom]]
        } else {
          dom_vars[[k]] <- val
        }
      }
    }

    # Isolate domain environment so side-effects/failures do not leak across domains
    dom_env <- new.env(parent = envir)
    for (k in names(dom_vars)) {
      assign(k, dom_vars[[k]], envir = dom_env)
    }

    # Prepare scalar/string variables for knitr::knit_expand()
    expand_vars <- lapply(dom_vars, function(v) {
      if (is.character(v) && length(v) == 1L) {
        v
      } else if ((is.numeric(v) || is.logical(v)) && length(v) == 1L) {
        v
      } else {
        deparse(v)[1]
      }
    })

    old_err_opt <- knitr::opts_chunk$get("error")
    knitr::opts_chunk$set(error = FALSE)
    on.exit(knitr::opts_chunk$set(error = old_err_opt), add = TRUE)

    dom_res <- tryCatch({
      expanded <- do.call(knitr::knit_expand, c(list(text = template_text), expand_vars))
      knitted <- knitr::knit_child(text = expanded, envir = dom_env, quiet = quiet)
      list(
        status = "succeeded",
        error = NA_character_,
        output = paste(knitted, collapse = "\n")
      )
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (on_error == "stop") {
        stop(sprintf("Domain '%s' failed in domain_children(): %s", dom, msg), call. = FALSE)
      }
      warning(sprintf("Domain '%s' failed in domain_children(): %s", dom, msg), call. = FALSE)
      err_block <- paste(
        sprintf("::: {.callout-warning title=\"Domain '%s' Failed\"}", dom),
        sprintf("Execution of domain `%s` encountered an error and was isolated:", dom),
        "",
        sprintf("`%s`", msg),
        ":::",
        sep = "\n"
      )
      list(
        status = "failed",
        error = msg,
        output = err_block
      )
    })

    dur <- max(0, proc.time()[["elapsed"]] - t0)
    rendered_chunks[i] <- dom_res$output
    status_rows[[i]] <- tibble::tibble(
      domain = dom,
      status = dom_res$status,
      error = dom_res$error,
      duration_sec = dur,
      output = dom_res$output
    )
  }

  status_tbl <- dplyr::bind_rows(status_rows)
  failed_domains <- status_tbl$domain[status_tbl$status == "failed"]
  succeeded_domains <- status_tbl$domain[status_tbl$status == "succeeded"]

  combined <- paste(rendered_chunks, collapse = "\n\n")
  attr(combined, "status") <- status_tbl
  attr(combined, "failed_domains") <- failed_domains
  attr(combined, "succeeded_domains") <- succeeded_domains
  class(combined) <- c("domain_children", "cbe_domain_children", "knit_asis", "character")
  combined
}

#' @rdname domain_children
#' @export
cbe_domain_children <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "cbe_domain_children()", "domain_children()")
  domain_children(...)
}

#' @export
`$.domain_children` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

#' @export
`$.cbe_domain_children` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

#' @export
print.domain_children <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}

#' @export
print.cbe_domain_children <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}

#' @export
summary.domain_children <- function(object, ...) {
  attr(object, "status", exact = TRUE)
}

#' @export
summary.cbe_domain_children <- function(object, ...) {
  attr(object, "status", exact = TRUE)
}

#' @exportS3Method generics::tidy
tidy.domain_children <- function(x, ...) {
  attr(x, "status", exact = TRUE)
}

#' @exportS3Method generics::tidy
tidy.cbe_domain_children <- function(x, ...) {
  attr(x, "status", exact = TRUE)
}

#' Packaged Nested Survival Report Quarto Child Template
#'
#' Locates and returns the file path to the packaged Quarto child report template
#' (`nested_survival_report.qmd`) used by [cbe_domain_children()] to render a
#' complete nested survival analysis report for a domain run object.
#'
#' @return File path character string.
#' @seealso [cbe_domain_children()], [cbe_nested_survival_run()]
#' @export
nested_survival_report_template <- function() {
  path <- system.file("templates", "nested_survival_report.qmd", package = "TempleCBE")
  if (!nzchar(path) || !file.exists(path)) {
    path <- file.path("inst", "templates", "nested_survival_report.qmd")
  }
  if (!file.exists(path)) {
    stop("`nested_survival_report.qmd` template not found.", call. = FALSE)
  }
  path
}

