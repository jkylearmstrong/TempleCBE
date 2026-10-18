# ==============================================================================
# General-Purpose Checkpointed Mapping & Tidymodels Wrappers
# ==============================================================================

#' Map Over Elements With Fingerprinted Checkpoints
#'
#' A purrr/furrr-style mapping function that caches individual task results to
#' disk under cryptographic fingerprints. Tasks that have already been evaluated
#' into `checkpoint_dir` with a matching fingerprint are loaded from disk and
#' skipped. When `shard = c(i, n)` is supplied, only shard `i` of `n` tasks is
#' evaluated; other tasks are returned from disk if available or as `NULL`.
#'
#' @param .x A list, vector, or data frame of inputs to iterate over.
#' @param .f A function, specified or formula, to apply to each element of `.x`.
#' @param ... Additional arguments passed to `.f`.
#' @param checkpoint_dir Optional directory path where per-task `.rds`
#'   checkpoint files are stored. If `NULL`, executes normally without disk caching.
#' @param key Optional vector or function producing safe, unique character keys
#'   for each element of `.x`. Defaults to names of `.x`, or `item_0001`, `item_0002`, etc.
#' @param fingerprint Optional character vector of pre-computed fingerprints
#'   per item, or a function `function(x, ...)` returning a fingerprint string.
#'   When `NULL`, fingerprints are derived from `.x[[i]]`, `...`, and package version.
#' @param shard Optional integer vector `c(i, n)` specifying that this execution
#'   evaluates shard `i` of `n` items.
#' @param checkpoint_mismatch Action when an existing checkpoint file has a
#'   mismatched fingerprint: `"recompute"` (default), `"overwrite"`, or `"error"`.
#' @param progress Logical (default `FALSE`). Display progress indication.
#' @return A list of evaluated (or loaded) results with names preserved from `.x`.
#' @seealso [collect_checkpoints()], [clean_checkpoints()], [tune_grid_checkpointed()]
#' @export
#' @examples
#' \donttest{
#' tmp <- file.path(tempdir(), "ckpt_map_demo")
#' res <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = tmp)
#' # Second run loads from disk instantaneously
#' res2 <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = tmp)
#' unlink(tmp, recursive = TRUE)
#' }
checkpoint_map <- function(.x,
                           .f,
                           ...,
                           checkpoint_dir = NULL,
                           key = NULL,
                           fingerprint = NULL,
                           shard = NULL,
                           checkpoint_mismatch = c("recompute", "overwrite", "error"),
                           progress = FALSE) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  n <- length(.x)
  if (n == 0L) {
    return(list())
  }

  .f <- rlang::as_function(.f)
  dots <- list(...)

  # Derive keys
  item_names <- names(.x)
  keys <- if (!is.null(key)) {
    if (is.function(key)) {
      vapply(seq_len(n), function(i) as.character(key(.x[[i]], ...)), character(1))
    } else if (length(key) == n) {
      as.character(key)
    } else {
      stop("`key` must be a function or a vector of the same length as `.x`.", call. = FALSE)
    }
  } else if (!is.null(item_names) && all(nzchar(item_names))) {
    item_names
  } else {
    sprintf("item_%04d", seq_len(n))
  }
  safe_keys <- gsub("[^A-Za-z0-9._-]", "_", keys)

  # Checkpoint directory
  if (!is.null(checkpoint_dir)) {
    if (!dir.exists(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Derive fingerprints
  pkg_ver <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.8")
  fps <- if (!is.null(fingerprint)) {
    if (is.function(fingerprint)) {
      vapply(seq_len(n), function(i) as.character(fingerprint(.x[[i]], ...)), character(1))
    } else if (length(fingerprint) == n) {
      as.character(fingerprint)
    } else if (length(fingerprint) == 1L) {
      rep(as.character(fingerprint), n)
    } else {
      stop("`fingerprint` must be a function or a vector of length 1 or length(.x).", call. = FALSE)
    }
  } else {
    vapply(seq_len(n), function(i) {
      payload <- list(
        version = pkg_ver,
        seed_scheme = current_seed_scheme(),
        item = .x[[i]],
        dots = dots
      )
      paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
    }, character(1))
  }

  # Run fingerprint representing the entire batch
  run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = fps, keys = safe_keys), NULL))), collapse = "")

  # Sharding mask
  shard_parsed <- validate_shard_spec(shard)
  active_mask <- if (!is.null(shard_parsed)) {
    ((seq_len(n) - 1L) %% shard_parsed[2]) + 1L == shard_parsed[1]
  } else {
    rep(TRUE, n)
  }

  # Build checkpoint file paths
  ckpt_files <- if (!is.null(checkpoint_dir)) {
    file.path(checkpoint_dir, sprintf("ckpt_%s.rds", safe_keys))
  } else {
    rep(list(NULL), n)
  }

  # Verify existing checkpoints up front
  if (!is.null(checkpoint_dir)) {
    for (i in seq_len(n)) {
      cf <- ckpt_files[[i]]
      if (!is.null(cf) && file.exists(cf)) {
        chk <- read_verified_checkpoint(cf, fps[i], checkpoint_mismatch = checkpoint_mismatch)
        if (identical(chk$status, "mismatch")) {
          unlink(cf)
        }
      }
    }
  }

  # Worker task function
  evaluate_one <- function(i) {
    cf <- ckpt_files[[i]]
    expected_fp <- fps[i]
    is_active <- active_mask[i]

    # Check cached
    if (!is.null(cf) && file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, expected_fp, checkpoint_mismatch = "recompute")
      if (identical(chk$status, "hit")) {
        return(chk$value)
      }
    }

    if (!is_active) {
      return(NULL)
    }

    val <- do.call(.f, c(list(.x[[i]]), dots))

    if (!is.null(cf)) {
      write_verified_checkpoint(
        val, cf, fingerprint = expected_fp, run_fingerprint = run_fp,
        meta = list(fn = "checkpoint_map", key = keys[i], index = i, n_items = n)
      )
    }
    val
  }

  is_parallel <- requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential") &&
    requireNamespace("furrr", quietly = TRUE)

  indices <- seq_len(n)
  results <- if (is_parallel) {
    furrr_pkgs <- furrr_worker_packages()
    f_opts <- furrr::furrr_options(seed = TRUE, packages = furrr_pkgs)
    furrr::future_map(indices, evaluate_one, .options = f_opts, .progress = progress)
  } else {
    purrr::map(indices, evaluate_one)
  }

  names(results) <- if (!is.null(item_names)) item_names else keys
  attr(results, "checkpoint_run_fingerprint") <- run_fp
  attr(results, "seed_scheme") <- current_seed_scheme()
  results
}

# ==============================================================================
# Tidymodels Checkpointed Wrappers
# ==============================================================================

#' Compute a Reproducible Checkpoint Fingerprint for Tidymodels Workflows
#'
#' @keywords internal
#' @noRd
compute_tidymodels_split_fingerprint <- function(object,
                                                 split,
                                                 grid = NULL,
                                                 metrics = NULL,
                                                 eval_time = NULL,
                                                 control = NULL) {
  pkg_ver <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.8")
  in_id <- if (inherits(split, "rsplit")) split$in_id else NULL
  out_id <- if (inherits(split, "rsplit")) {
    tryCatch(rsample::complement(split), error = function(e) split$out_id)
  } else {
    NULL
  }
  data_dim <- if (inherits(split, "rsplit") && is.data.frame(split$data)) dim(split$data) else NULL

  payload <- list(
    version = pkg_ver,
    seed_scheme = current_seed_scheme(),
    object_cls = class(object),
    split_in = in_id,
    split_out = out_id,
    data_dim = data_dim,
    grid = grid,
    metrics = if (!is.null(metrics)) names(metrics) else NULL,
    eval_time = eval_time,
    control = if (!is.null(control)) class(control) else NULL
  )
  paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
}

#' Checkpointed Tuning Over Resamples (`tune_grid_checkpointed`)
#'
#' A fingerprinted, resumable wrapper around [tune::tune_grid()] that evaluates
#' tuning on a per-resample unit of work. Completed resample results are saved
#' into `checkpoint_dir` and skipped on rerun. Returns an ordinary
#' `tune_results` object so downstream functions like [tune::select_best()],
#' [tune::collect_metrics()], [tune::finalize_workflow()], and [ggplot2::autoplot()]
#' work identically.
#'
#' @param object A `workflow` or model specification.
#' @param resamples An `rset` resampling object (e.g. from [rsample::vfold_cv()]).
#' @param grid Number of grid points or a parameter grid data frame.
#' @param metrics A `yardstick::metric_set()`.
#' @param control A `tune::control_grid()` object.
#' @param checkpoint_dir Directory to store per-resample fingerprinted checkpoints.
#' @param shard Optional integer vector `c(i, n)` for distributed evaluation.
#' @param checkpoint_mismatch Action when a checkpoint fingerprint mismatches:
#'   `"recompute"` (default), `"overwrite"`, or `"error"`.
#' @param progress Logical (default `FALSE`).
#' @param ... Additional arguments passed to [tune::tune_grid()].
#' @return A standard `tune_results` object.
#' @seealso [fit_resamples_checkpointed()], [workflow_map_checkpointed()], [checkpoint_map()]
#' @export
tune_grid_checkpointed <- function(object,
                                   resamples,
                                   grid = 10,
                                   metrics = NULL,
                                   control = tune::control_grid(),
                                   checkpoint_dir = NULL,
                                   shard = NULL,
                                   checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                   progress = FALSE,
                                   ...) {
  rlang::check_installed("tune", reason = "for tune_grid_checkpointed().")
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)

  if (is.null(checkpoint_dir)) {
    return(tune::tune_grid(
      object = object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      control = control,
      ...
    ))
  }

  if (!inherits(resamples, "rset")) {
    stop("`resamples` must be an rsample rset object.", call. = FALSE)
  }

  n_splits <- nrow(resamples)
  fold_ids <- if ("id" %in% names(resamples)) resamples$id else sprintf("Resample%02d", seq_len(n_splits))
  safe_ids <- gsub("[^A-Za-z0-9._-]", "_", fold_ids)

  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Fingerprint per resample split
  split_fps <- vapply(seq_len(n_splits), function(i) {
    compute_tidymodels_split_fingerprint(
      object = object,
      split = resamples$splits[[i]],
      grid = grid,
      metrics = metrics,
      control = control
    )
  }, character(1))

  run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, ids = safe_ids), NULL))), collapse = "")

  active_mask <- if (!is.null(shard)) {
    ((seq_len(n_splits) - 1L) %% as.integer(shard[2])) + 1L == as.integer(shard[1])
  } else {
    rep(TRUE, n_splits)
  }

  ckpt_files <- file.path(checkpoint_dir, sprintf("tune_grid_%s.rds", safe_ids))

  # Verify existing
  for (i in seq_len(n_splits)) {
    cf <- ckpt_files[i]
    if (file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = checkpoint_mismatch)
      if (identical(chk$status, "mismatch")) {
        unlink(cf)
      }
    }
  }

  eval_split <- function(i) {
    cf <- ckpt_files[i]
    if (file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = "recompute")
      if (identical(chk$status, "hit")) {
        return(chk$value)
      }
    }
    if (!active_mask[i]) {
      return(NULL)
    }

    # Construct single-split rset
    single_rset <- resamples[i, , drop = FALSE]
    class(single_rset) <- class(resamples)
    for (a in names(attributes(resamples))) {
      if (!a %in% names(attributes(single_rset))) {
        attr(single_rset, a) <- attr(resamples, a)
      }
    }

    res_single <- tune::tune_grid(
      object = object,
      resamples = single_rset,
      grid = grid,
      metrics = metrics,
      control = control,
      ...
    )

    write_verified_checkpoint(
      res_single, cf, fingerprint = split_fps[i], run_fingerprint = run_fp,
      meta = list(fn = "tune_grid_checkpointed", fold_id = fold_ids[i], index = i)
    )
    res_single
  }

  is_parallel <- requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential") &&
    requireNamespace("furrr", quietly = TRUE)

  indices <- seq_len(n_splits)
  rows <- if (is_parallel) {
    furrr_pkgs <- furrr_worker_packages()
    f_opts <- furrr::furrr_options(seed = TRUE, packages = furrr_pkgs)
    furrr::future_map(indices, eval_split, .options = f_opts, .progress = progress)
  } else {
    purrr::map(indices, eval_split)
  }

  good_rows <- Filter(Negate(is.null), rows)
  if (length(good_rows) == 0L) {
    stop("No resample could be tuned in tune_grid_checkpointed().", call. = FALSE)
  }

  # Combine rows while retaining full tune_results structure
  combined <- dplyr::bind_rows(good_rows)
  ref_obj <- good_rows[[1]]
  class(combined) <- class(ref_obj)
  for (a in names(attributes(ref_obj))) {
    if (!a %in% c("names", "row.names")) {
      attr(combined, a) <- attr(ref_obj, a)
    }
  }
  attr(combined, "checkpoint_run_fingerprint") <- run_fp
  attr(combined, "checkpoint_dir") <- checkpoint_dir
  combined
}

#' Checkpointed Model Evaluation Across Resamples (`fit_resamples_checkpointed`)
#'
#' A fingerprinted, resumable wrapper around [tune::fit_resamples()] that evaluates
#' each resample split independently and saves fingerprinted checkpoints into `checkpoint_dir`.
#'
#' @param object A `workflow` or model specification.
#' @param resamples An `rset` resampling object.
#' @param metrics A `yardstick::metric_set()`.
#' @param control A `tune::control_resamples()` object.
#' @param checkpoint_dir Directory to store per-resample fingerprinted checkpoints.
#' @param shard Optional integer vector `c(i, n)` for distributed evaluation.
#' @param checkpoint_mismatch Action when a checkpoint fingerprint mismatches:
#'   `"recompute"` (default), `"overwrite"`, or `"error"`.
#' @param progress Logical (default `FALSE`).
#' @param ... Additional arguments passed to [tune::fit_resamples()].
#' @return A standard `resample_results` object.
#' @seealso [tune_grid_checkpointed()], [checkpoint_map()]
#' @export
fit_resamples_checkpointed <- function(object,
                                       resamples,
                                       metrics = NULL,
                                       control = tune::control_resamples(),
                                       checkpoint_dir = NULL,
                                       shard = NULL,
                                       checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                       progress = FALSE,
                                       ...) {
  rlang::check_installed("tune", reason = "for fit_resamples_checkpointed().")
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)

  if (is.null(checkpoint_dir)) {
    return(tune::fit_resamples(
      object = object,
      resamples = resamples,
      metrics = metrics,
      control = control,
      ...
    ))
  }

  if (!inherits(resamples, "rset")) {
    stop("`resamples` must be an rsample rset object.", call. = FALSE)
  }

  n_splits <- nrow(resamples)
  fold_ids <- if ("id" %in% names(resamples)) resamples$id else sprintf("Resample%02d", seq_len(n_splits))
  safe_ids <- gsub("[^A-Za-z0-9._-]", "_", fold_ids)

  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }

  split_fps <- vapply(seq_len(n_splits), function(i) {
    compute_tidymodels_split_fingerprint(
      object = object,
      split = resamples$splits[[i]],
      metrics = metrics,
      control = control
    )
  }, character(1))

  run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, ids = safe_ids), NULL))), collapse = "")

  active_mask <- if (!is.null(shard)) {
    ((seq_len(n_splits) - 1L) %% as.integer(shard[2])) + 1L == as.integer(shard[1])
  } else {
    rep(TRUE, n_splits)
  }

  ckpt_files <- file.path(checkpoint_dir, sprintf("fit_resample_%s.rds", safe_ids))

  for (i in seq_len(n_splits)) {
    cf <- ckpt_files[i]
    if (file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = checkpoint_mismatch)
      if (identical(chk$status, "mismatch")) {
        unlink(cf)
      }
    }
  }

  eval_split <- function(i) {
    cf <- ckpt_files[i]
    if (file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = "recompute")
      if (identical(chk$status, "hit")) {
        return(chk$value)
      }
    }
    if (!active_mask[i]) {
      return(NULL)
    }

    single_rset <- resamples[i, , drop = FALSE]
    class(single_rset) <- class(resamples)
    for (a in names(attributes(resamples))) {
      if (!a %in% names(attributes(single_rset))) {
        attr(single_rset, a) <- attr(resamples, a)
      }
    }

    res_single <- tune::fit_resamples(
      object = object,
      resamples = single_rset,
      metrics = metrics,
      control = control,
      ...
    )

    write_verified_checkpoint(
      res_single, cf, fingerprint = split_fps[i], run_fingerprint = run_fp,
      meta = list(fn = "fit_resamples_checkpointed", fold_id = fold_ids[i], index = i)
    )
    res_single
  }

  is_parallel <- requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential") &&
    requireNamespace("furrr", quietly = TRUE)

  indices <- seq_len(n_splits)
  rows <- if (is_parallel) {
    furrr_pkgs <- furrr_worker_packages()
    f_opts <- furrr::furrr_options(seed = TRUE, packages = furrr_pkgs)
    furrr::future_map(indices, eval_split, .options = f_opts, .progress = progress)
  } else {
    purrr::map(indices, eval_split)
  }

  good_rows <- Filter(Negate(is.null), rows)
  if (length(good_rows) == 0L) {
    stop("No resample could be fit in fit_resamples_checkpointed().", call. = FALSE)
  }

  combined <- dplyr::bind_rows(good_rows)
  ref_obj <- good_rows[[1]]
  class(combined) <- class(ref_obj)
  for (a in names(attributes(ref_obj))) {
    if (!a %in% c("names", "row.names")) {
      attr(combined, a) <- attr(ref_obj, a)
    }
  }
  attr(combined, "checkpoint_run_fingerprint") <- run_fp
  attr(combined, "checkpoint_dir") <- checkpoint_dir
  combined
}

#' Checkpointed Workflow Sets Mapping (`workflow_map_checkpointed`)
#'
#' A resumable, fingerprinted wrapper around [workflowsets::workflow_map()] that
#' checkpoints each workflow's tuning results independently into subdirectories
#' of `checkpoint_dir`.
#'
#' @param object A `workflow_set` object.
#' @param fn Function to apply to each workflow (default `"tune_grid"`).
#' @param ... Arguments passed to `fn`.
#' @param checkpoint_dir Directory to store per-workflow checkpoints.
#' @param shard Optional integer vector `c(i, n)`.
#' @param checkpoint_mismatch Action on fingerprint mismatch.
#' @param progress Logical (default `FALSE`).
#' @return A `workflow_set` object with populated result columns.
#' @seealso [tune_grid_checkpointed()], [workflowsets::workflow_map()]
#' @export
workflow_map_checkpointed <- function(object,
                                      fn = "tune_grid",
                                      ...,
                                      checkpoint_dir = NULL,
                                      shard = NULL,
                                      checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                      progress = FALSE) {
  rlang::check_installed("workflowsets", reason = "for workflow_map_checkpointed().")
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)

  if (is.null(checkpoint_dir)) {
    return(workflowsets::workflow_map(object = object, fn = fn, ...))
  }

  if (!inherits(object, "workflow_set")) {
    stop("`object` must be a workflow_set object.", call. = FALSE)
  }

  n_wflow <- nrow(object)
  wflow_ids <- object$wflow_id
  safe_ids <- gsub("[^A-Za-z0-9._-]", "_", wflow_ids)

  active_mask <- if (!is.null(shard)) {
    ((seq_len(n_wflow) - 1L) %% as.integer(shard[2])) + 1L == as.integer(shard[1])
  } else {
    rep(TRUE, n_wflow)
  }

  results_list <- vector("list", n_wflow)
  dots <- list(...)

  for (i in seq_len(n_wflow)) {
    w_id <- wflow_ids[i]
    w_dir <- file.path(checkpoint_dir, safe_ids[i])
    w_spec <- object$info[[i]]$workflow[[1]]

    if (!active_mask[i]) {
      # Try loading if exists
      ckpt_file <- file.path(w_dir, "workflow_result.rds")
      if (file.exists(ckpt_file)) {
        chk <- tryCatch(readRDS(ckpt_file), error = function(e) NULL)
        results_list[[i]] <- chk
      }
      next
    }

    # Evaluate using tune_grid_checkpointed or fit_resamples_checkpointed if applicable
    res_i <- if (identical(fn, "tune_grid")) {
      do.call(tune_grid_checkpointed, c(
        list(object = w_spec, checkpoint_dir = w_dir, checkpoint_mismatch = checkpoint_mismatch, progress = progress),
        dots
      ))
    } else if (identical(fn, "fit_resamples")) {
      do.call(fit_resamples_checkpointed, c(
        list(object = w_spec, checkpoint_dir = w_dir, checkpoint_mismatch = checkpoint_mismatch, progress = progress),
        dots
      ))
    } else {
      # Fallback to standard workflow_map for single workflow with checkpoint file
      ckpt_file <- file.path(w_dir, "workflow_result.rds")
      if (file.exists(ckpt_file)) {
        chk <- tryCatch(readRDS(ckpt_file), error = function(e) NULL)
        if (!is.null(chk)) chk else {
          out_w <- workflowsets::workflow_map(object[i, , drop = FALSE], fn = fn, ...)
          res_w <- out_w$result[[1]]
          tryCatch(saveRDS(res_w, ckpt_file), error = function(e) NULL)
          res_w
        }
      } else {
        if (!dir.exists(w_dir)) dir.create(w_dir, recursive = TRUE, showWarnings = FALSE)
        out_w <- workflowsets::workflow_map(object[i, , drop = FALSE], fn = fn, ...)
        res_w <- out_w$result[[1]]
        tryCatch(saveRDS(res_w, ckpt_file), error = function(e) NULL)
        res_w
      }
    }

    results_list[[i]] <- res_i
    ckpt_file <- file.path(w_dir, "workflow_result.rds")
    tryCatch(saveRDS(res_i, ckpt_file), error = function(e) NULL)
  }

  res_set <- object
  res_set$result <- results_list
  attr(res_set, "checkpoint_dir") <- checkpoint_dir
  res_set
}

#' Checkpointed Survival Racing Optimization (`tune_race_survival_checkpointed`)
#'
#' A resumable wrapper around [tune_race_survival()] with fingerprinted disk caching.
#'
#' @param object A workflow or workflow set.
#' @param resamples An `rset` resampling object.
#' @param ... Arguments passed to [tune_race_survival()].
#' @param checkpoint_dir Directory to store checkpoints.
#' @param shard Optional shard specification.
#' @param checkpoint_mismatch Mismatch handling action.
#' @param progress Logical (default `FALSE`).
#' @return A racing tuning object.
#' @seealso [tune_race_survival()], [tune_grid_checkpointed()]
#' @export
tune_race_survival_checkpointed <- function(object,
                                            resamples,
                                            ...,
                                            checkpoint_dir = NULL,
                                            shard = NULL,
                                            checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                            progress = FALSE) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  if (is.null(checkpoint_dir)) {
    return(tune_race_survival(object, resamples = resamples, ...))
  }

  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ckpt_file <- file.path(checkpoint_dir, "race_survival_result.rds")
  fp <- compute_tidymodels_split_fingerprint(
    object = object,
    split = resamples$splits[[1]],
    control = list(...)
  )

  if (file.exists(ckpt_file)) {
    chk <- read_verified_checkpoint(ckpt_file, fp, checkpoint_mismatch = checkpoint_mismatch)
    if (identical(chk$status, "hit")) {
      return(chk$value)
    }
  }

  res <- tune_race_survival(object, resamples = resamples, ...)
  write_verified_checkpoint(
    res, ckpt_file, fingerprint = fp,
    meta = list(fn = "tune_race_survival_checkpointed")
  )
  res
}
