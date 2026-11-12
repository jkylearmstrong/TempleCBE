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
#'   Checkpoints are read back with [readRDS()], which can run code stored in a
#'   file: use a directory that only you (or a process you trust) can write to.
#'   A checkpoint that cannot be read, and a checkpoint that cannot be written
#'   (full disk, no permission), give a warning that names the file.
#' @param key Optional vector or function producing safe, unique character keys
#'   for each element of `.x`. Defaults to names of `.x`, or `item_0001`, `item_0002`, etc.
#' @param fingerprint Optional character vector of pre-computed fingerprints
#'   per item, or a function `function(x, ...)` returning a fingerprint string.
#'   When `NULL`, fingerprints are derived from `.x[[i]]`, `...`, and package version.
#' @param shard Optional integer vector `c(i, n)` or string `"i/n"` with `1 <= i <= n`:
#'   evaluate shard `i` of `n`. Anything else is an error.
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
  ckpt_schema <- checkpoint_schema_version()
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
        schema = ckpt_schema,
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
  active_mask <- shard_active_mask(n, validate_shard_spec(shard))

  # Build checkpoint file paths
  ckpt_files <- if (!is.null(checkpoint_dir)) {
    file.path(checkpoint_dir, sprintf("ckpt_%s.rds", safe_keys))
  } else {
    rep(list(NULL), n)
  }

  # Verify existing checkpoints up front
  mismatch_count <- 0L
  if (!is.null(checkpoint_dir)) {
    for (i in seq_len(n)) {
      cf <- ckpt_files[[i]]
      if (!is.null(cf) && file.exists(cf)) {
        chk <- read_verified_checkpoint(cf, fps[i], checkpoint_mismatch = checkpoint_mismatch)
        if (identical(chk$status, "mismatch")) {
          mismatch_count <- mismatch_count + 1L
          unlink(cf)
        }
      }
    }
    if (mismatch_count > 0L) {
      message(sprintf(
        "checkpoint_map(): Discarded %d cached task(s) in '%s' due to mismatched fingerprint (inputs, settings, or seed scheme changed). Recomputing...",
        mismatch_count, checkpoint_dir
      ))
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
        return(strip_checkpoint_attrs(chk$value))
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
    strip_checkpoint_attrs(val)
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
  ckpt_schema <- checkpoint_schema_version()
  in_id <- if (inherits(split, "rsplit")) split$in_id else NULL
  out_id <- if (inherits(split, "rsplit")) {
    tryCatch(rsample::complement(split), error = function(e) split$out_id)
  } else {
    NULL
  }
  data_repr <- if (inherits(split, "rsplit") && is.data.frame(split$data)) {
    paste0(as.character(openssl::sha256(serialize(split$data, NULL))), collapse = "")
  } else {
    NULL
  }

  metrics_repr <- if (!is.null(metrics)) {
    m_attr <- attr(metrics, "metrics", exact = TRUE)
    if (!is.null(m_attr)) {
      list(metric_names = sort(names(m_attr)))
    } else {
      paste0(as.character(openssl::sha256(serialize(metrics, NULL))), collapse = "")
    }
  } else {
    NULL
  }

  control_repr <- if (!is.null(control)) {
    c_list <- unclass(control)
    if (is.function(c_list$extract)) {
      c_list$extract <- paste(deparse(c_list$extract), collapse = "\n")
    }
    paste0(as.character(openssl::sha256(serialize(c_list, NULL))), collapse = "")
  } else {
    NULL
  }

  object_repr <- paste0(as.character(openssl::sha256(serialize(object, NULL))), collapse = "")
  grid_repr <- if (!is.null(grid)) {
    paste0(as.character(openssl::sha256(serialize(grid, NULL))), collapse = "")
  } else {
    NULL
  }

  payload <- list(
    schema = ckpt_schema,
    seed_scheme = current_seed_scheme(),
    object = object_repr,
    split_in = in_id,
    split_out = out_id,
    data = data_repr,
    grid = grid_repr,
    metrics = metrics_repr,
    eval_time = eval_time,
    control = control_repr
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
#' @param checkpoint_dir Directory to store per-resample fingerprinted checkpoints. They are
#'   read back with [readRDS()]: use a directory that only trusted processes write to.
#' @param shard Optional integer vector `c(i, n)` or string `"i/n"` with `1 <= i <= n`:
#'   evaluate shard `i` of `n`. Anything else is an error.
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
  shard <- validate_shard_spec(shard)
  object <- apply_workflow_cox_ties(object)

  fit_split <- function(resamples) {
    with_quiet_cox_ties(tune::tune_grid(
      object = object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      control = control,
      ...
    ))
  }
  if (is.null(checkpoint_dir)) {
    return(fit_split(resamples))
  }

  checkpointed_resamples(
    resamples = resamples,
    fit_split = fit_split,
    split_fingerprint = function(split) {
      compute_tidymodels_split_fingerprint(
        object = object, split = split, grid = grid, metrics = metrics, control = control
      )
    },
    fn = "tune_grid_checkpointed",
    file_prefix = "tune_grid",
    verb = "tuned",
    fingerprint_inputs = "workflow spec, data, resamples, grid, metrics, control, or seed scheme",
    checkpoint_dir = checkpoint_dir,
    shard = shard,
    checkpoint_mismatch = checkpoint_mismatch,
    progress = progress
  )
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
#' @param checkpoint_dir Directory to store per-resample fingerprinted checkpoints. They are
#'   read back with [readRDS()]: use a directory that only trusted processes write to.
#' @param shard Optional integer vector `c(i, n)` or string `"i/n"` with `1 <= i <= n`:
#'   evaluate shard `i` of `n`. Anything else is an error.
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
  shard <- validate_shard_spec(shard)

  fit_split <- function(resamples) {
    tune::fit_resamples(
      object = object,
      resamples = resamples,
      metrics = metrics,
      control = control,
      ...
    )
  }
  if (is.null(checkpoint_dir)) {
    return(fit_split(resamples))
  }

  checkpointed_resamples(
    resamples = resamples,
    fit_split = fit_split,
    split_fingerprint = function(split) {
      compute_tidymodels_split_fingerprint(
        object = object, split = split, metrics = metrics, control = control
      )
    },
    fn = "fit_resamples_checkpointed",
    file_prefix = "fit_resample",
    verb = "fit",
    fingerprint_inputs = "workflow spec, data, resamples, metrics, control, or seed scheme",
    checkpoint_dir = checkpoint_dir,
    shard = shard,
    checkpoint_mismatch = checkpoint_mismatch,
    progress = progress
  )
}

#' One Checkpoint per Resample: the Engine of the `*_checkpointed()` Wrappers
#'
#' [tune_grid_checkpointed()] and [fit_resamples_checkpointed()] differ only in
#' the function that is run on one resample and in what enters the
#' fingerprint. This evaluates each resample on its own, saves it with its
#' fingerprint, reuses a saved result whose fingerprint still matches and binds
#' the results into one object of the class the underlying function returns.
#'
#' @param resamples An `rset`.
#' @param fit_split Function of a one-row `rset`, returning its results.
#' @param split_fingerprint Function of one split, returning its fingerprint.
#' @param fn Name of the calling function, for messages and metadata.
#' @param file_prefix Prefix of the checkpoint files.
#' @param verb Past participle for the "no resample could be ..." error.
#' @param fingerprint_inputs What the fingerprint covers, for the message.
#' @param checkpoint_dir,checkpoint_mismatch,progress As in the wrappers.
#' @param shard A validated shard (see `validate_shard_spec()`) or `NULL`.
#' @keywords internal
#' @noRd
checkpointed_resamples <- function(resamples,
                                   fit_split,
                                   split_fingerprint,
                                   fn,
                                   file_prefix,
                                   verb,
                                   fingerprint_inputs,
                                   checkpoint_dir,
                                   shard,
                                   checkpoint_mismatch,
                                   progress) {
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
  split_fps <- vapply(seq_len(n_splits), function(i) split_fingerprint(resamples$splits[[i]]), character(1))

  run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, ids = safe_ids), NULL))), collapse = "")

  active_mask <- shard_active_mask(n_splits, shard)

  ckpt_files <- file.path(checkpoint_dir, sprintf("%s_%s.rds", file_prefix, safe_ids))

  # Verify existing
  mismatch_count <- 0L
  for (i in seq_len(n_splits)) {
    cf <- ckpt_files[i]
    if (file.exists(cf)) {
      chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = checkpoint_mismatch)
      if (identical(chk$status, "mismatch")) {
        mismatch_count <- mismatch_count + 1L
        unlink(cf)
      }
    }
  }
  if (mismatch_count > 0L) {
    message(sprintf(
      "%s(): Discarded %d cached task(s) in '%s' due to mismatched fingerprint (%s changed). Recomputing...",
      fn, mismatch_count, checkpoint_dir, fingerprint_inputs
    ))
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

    res_single <- fit_split(single_rset)

    write_verified_checkpoint(
      res_single, cf, fingerprint = split_fps[i], run_fingerprint = run_fp,
      meta = list(fn = fn, fold_id = fold_ids[i], index = i)
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
    stop(sprintf("No resample could be %s in %s().", verb, fn), call. = FALSE)
  }
  if (!is.null(shard) && length(good_rows) < n_splits) {
    warning(
      sprintf("%s(): shard %d/%d evaluated only %d of %d resample(s); the result is partial (missing: %s). ",
              fn, shard[1], shard[2], length(good_rows), n_splits,
              paste(fold_ids[!(seq_len(n_splits) %in% which(!vapply(rows, is.null, logical(1))))], collapse = ", ")),
      "Collect the other shards' checkpoints to complete it.",
      call. = FALSE
    )
  }

  # Combine rows while retaining the full structure of the results object
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
  if (!is.null(shard) && length(good_rows) < n_splits) {
    attr(combined, "shard_partial") <- list(
      shard = shard, n_splits = n_splits, n_evaluated = length(good_rows),
      evaluated_folds = fold_ids[!vapply(rows, is.null, logical(1))]
    )
  }
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
#' @param seed Optional integer seed (default `1L`) used to set a reproducible RNG seed per workflow.
#' @param verbose Logical (default `FALSE`). Display progress from [workflowsets::workflow_map()].
#' @param checkpoint_dir Directory to store per-workflow checkpoints. They are read back
#'   with [readRDS()]: use a directory that only trusted processes write to.
#'   Each workflow's result is stored with a fingerprint of that workflow, the
#'   function and its arguments, so a changed workflow or setting does not
#'   silently reuse a stale cached result (including for workflows this shard
#'   did not evaluate).
#' @param shard Optional integer vector `c(i, n)` or string `"i/n"` with `1 <= i <= n`:
#'   evaluate shard `i` of `n`. Anything else is an error.
#' @param checkpoint_mismatch Action on fingerprint mismatch.
#' @param progress Logical (default `FALSE`).
#' @return A `workflow_set` object with populated result columns.
#' @seealso [tune_grid_checkpointed()], [workflowsets::workflow_map()]
#' @export
workflow_map_checkpointed <- function(object,
                                      fn = "tune_grid",
                                      ...,
                                      seed = 1L,
                                      verbose = FALSE,
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

  active_mask <- shard_active_mask(n_wflow, validate_shard_spec(shard))

  results_list <- vector("list", n_wflow)
  dots <- list(...)
  if ("seed" %in% names(dots)) {
    seed <- dots$seed
    dots$seed <- NULL
  }
  if ("verbose" %in% names(dots)) {
    verbose <- dots$verbose
    dots$verbose <- NULL
  }

  # One fingerprint per workflow: the workflow spec, the function and its
  # arguments (including the resamples), and the seed. The run fingerprint
  # combines them all.
  ckpt_schema <- checkpoint_schema_version()
  wflow_fp <- function(i) {
    payload <- list(
      schema = ckpt_schema,
      seed_scheme = current_seed_scheme(),
      fn = fn,
      wflow_id = wflow_ids[i],
      workflow = tryCatch(object$info[[i]]$workflow[[1]], error = function(e) NULL),
      dots = dots,
      seed = seed
    )
    paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
  }
  wflow_fps <- vapply(seq_len(n_wflow), wflow_fp, character(1))
  run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = wflow_fps, ids = wflow_ids), NULL))), collapse = "")

  for (i in seq_len(n_wflow)) {
    w_id <- wflow_ids[i]
    w_dir <- file.path(checkpoint_dir, safe_ids[i])
    w_spec <- object$info[[i]]$workflow[[1]]

    if (!active_mask[i]) {
      # Try loading if exists, but only when its fingerprint still matches:
      # an unfingerprinted load would silently reuse a stale result when the
      # workflow or settings changed.
      ckpt_file <- file.path(w_dir, "workflow_result.rds")
      if (file.exists(ckpt_file)) {
        chk <- read_verified_checkpoint(ckpt_file, wflow_fps[i], checkpoint_mismatch = "recompute")
        if (identical(chk$status, "hit")) {
          results_list[[i]] <- chk$value
        } else if (identical(chk$status, "mismatch")) {
          warning(
            "Cached result for workflow '", w_id, "' has a mismatched fingerprint and is ignored; ",
            "this shard does not recompute it, so the workflow result is NULL for this run.",
            call. = FALSE
          )
        }
      }
      next
    }

    eval_expr <- function() {
      if (identical(fn, "tune_grid")) {
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
        ckpt_file <- file.path(w_dir, "workflow_result.rds")
        if (file.exists(ckpt_file)) {
          chk <- read_verified_checkpoint(ckpt_file, wflow_fps[i], checkpoint_mismatch = checkpoint_mismatch)
          if (identical(chk$status, "hit")) chk$value else {
            if (!dir.exists(w_dir)) dir.create(w_dir, recursive = TRUE, showWarnings = FALSE)
            out_w <- workflowsets::workflow_map(object[i, , drop = FALSE], fn = fn, seed = seed, verbose = verbose, ...)
            res_w <- out_w$result[[1]]
            write_verified_checkpoint(res_w, ckpt_file, fingerprint = wflow_fps[i], run_fingerprint = run_fp,
                                      meta = list(fn = "workflow_map_checkpointed", wflow_id = w_id))
            res_w
          }
        } else {
          if (!dir.exists(w_dir)) dir.create(w_dir, recursive = TRUE, showWarnings = FALSE)
          out_w <- workflowsets::workflow_map(object[i, , drop = FALSE], fn = fn, seed = seed, verbose = verbose, ...)
          res_w <- out_w$result[[1]]
          write_verified_checkpoint(res_w, ckpt_file, fingerprint = wflow_fps[i], run_fingerprint = run_fp,
                                    meta = list(fn = "workflow_map_checkpointed", wflow_id = w_id))
          res_w
        }
      }
    }

    res_i <- if (!is.null(seed)) {
      with_local_seed(as.integer(seed) + i - 1L, eval_expr())
    } else {
      eval_expr()
    }

    results_list[[i]] <- res_i
    ckpt_file <- file.path(w_dir, "workflow_result.rds")
    write_verified_checkpoint(res_i, ckpt_file, fingerprint = wflow_fps[i], run_fingerprint = run_fp,
                              meta = list(fn = "workflow_map_checkpointed", wflow_id = w_id))
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
#' @param shard Optional shard specification. It is validated, but a single
#'   race result cannot be sharded across machines: a non-`NULL` shard warns
#'   and the full race is run (and cached) as one result.
#' @param checkpoint_mismatch Mismatch handling action.
#' @param progress Logical (default `FALSE`). Accepted for interface
#'   consistency; the finetune race does not expose progress reporting, so it
#'   currently has no effect.
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
  shard <- validate_shard_spec(shard)
  if (!is.null(shard)) {
    warning(
      "`shard` is ignored by tune_race_survival_checkpointed(): a single race result is evaluated over all ",
      "resamples and cannot be split across shards. The full race is run and cached as one result.",
      call. = FALSE
    )
  }
  if (is.null(checkpoint_dir)) {
    return(tune_race_survival(object, resamples = resamples, ...))
  }

  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ckpt_file <- file.path(checkpoint_dir, "race_survival_result.rds")
  # Fingerprint every split, not just the first: changing folds 2..n must
  # not reuse the cached race result.
  race_dots <- list(...)
  fp <- tryCatch({
    split_fps <- vapply(seq_len(nrow(resamples)), function(i) {
      compute_tidymodels_split_fingerprint(
        object = object,
        split = resamples$splits[[i]],
        grid = race_dots$grid,
        metrics = race_dots$metrics,
        eval_time = race_dots$eval_time,
        control = race_dots$control %||% list(...)
      )
    }, character(1))
    paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, shard = shard), NULL))), collapse = "")
  }, error = function(e) {
    compute_tidymodels_split_fingerprint(
      object = object,
      split = resamples$splits[[1]],
      control = list(...)
    )
  })

  if (file.exists(ckpt_file)) {
    chk <- read_verified_checkpoint(ckpt_file, fp, checkpoint_mismatch = checkpoint_mismatch)
    if (identical(chk$status, "hit")) {
      return(chk$value)
    } else if (identical(chk$status, "mismatch")) {
      message(sprintf(
        "tune_race_survival_checkpointed(): Discarded cached result in '%s' due to mismatched fingerprint. Recomputing...",
        ckpt_file
      ))
      unlink(ckpt_file)
    }
  }

  res <- tune_race_survival(object, resamples = resamples, ...)
  write_verified_checkpoint(
    res, ckpt_file, fingerprint = fp,
    meta = list(fn = "tune_race_survival_checkpointed")
  )
  res
}
