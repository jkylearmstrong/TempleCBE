# ANOVA racing for cv_coxnet() and nested_cv_coxnet(), run on glmnet directly.
#
# finetune::tune_race_anova() cannot race start/stop (counting-process) data:
# the yardstick survival metrics need a right-censored truth, and tune scores
# each row as if it were a subject. cv_coxnet() already scores at the subject
# level (one right-censored truth per subject, survival integrated over the
# subject's intervals), so the race runs on that scoring instead. The rule is
# finetune's: after `burn_in` resamples, the selection metric is modelled as
# `.estimate ~ candidate + resample` (a randomized block ANOVA, the fixed-effect
# form of finetune's `(1 | id)` model), and a candidate whose difference from
# the current best has a `1 - alpha` confidence interval entirely on the worse
# side is dropped. Later resamples score only the survivors; a mixture with no
# surviving penalty is no longer fit.

#' Racing Settings for cv_coxnet()
#'
#' @param control `NULL`, a list, or a [finetune::control_race()] object. Only
#'   `burn_in`, `alpha` and `randomize` are used.
#' @return A list with `burn_in`, `alpha`, and `randomize`.
#' @keywords internal
#' @noRd
race_coxnet_control <- function(control = NULL) {
  defaults <- list(burn_in = 3L, alpha = 0.05, randomize = TRUE)
  if (is.null(control)) {
    return(defaults)
  }
  if (!is.list(control)) {
    stop("`control` must be NULL, a list, or a finetune::control_race() object.", call. = FALSE)
  }
  out <- utils::modifyList(defaults, unclass(control)[intersect(names(control), names(defaults))])
  if (!is.numeric(out$burn_in) || length(out$burn_in) != 1 || is.na(out$burn_in) || out$burn_in < 2) {
    stop("`control$burn_in` must be a single number of at least 2.", call. = FALSE)
  }
  if (!is.numeric(out$alpha) || length(out$alpha) != 1 || is.na(out$alpha) ||
      out$alpha <= 0 || out$alpha >= 1) {
    stop("`control$alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  out$burn_in <- as.integer(out$burn_in)
  out$randomize <- isTRUE(out$randomize)
  out
}

#' Effective `num_ties` of a Race Control, for Fingerprinting
#'
#' `race_coxnet_control()` drops `num_ties` — the native ANOVA race never
#' uses it — but the finetune win-loss race does, so the fingerprint needs
#' the effective value: the supplied one, or finetune's default of 10
#' (R7-01 sub-item).
#'
#' @param control `NULL`, a list, or a [finetune::control_race()] object.
#' @return A single integer.
#' @keywords internal
#' @noRd
race_control_num_ties <- function(control = NULL) {
  n <- if (is.list(control)) control$num_ties else NULL
  if (is.numeric(n) && length(n) == 1 && !is.na(n)) as.integer(n) else 10L
}

#' Race the Candidates of a cv_coxnet() Grid Over Resamples
#'
#' @param splits,ids The resamples' splits and ids.
#' @param score_split `function(split, id, active)` from cv_coxnet_impl().
#' @param map_splits `function(splits, ids)` that scores splits on every
#'   candidate, in parallel when asked; used for the burn-in resamples.
#' @param mixture,paths The mixtures and their penalty paths.
#' @param info,metric,eval_time The metric set's info, the selection metric
#'   and the evaluation times.
#' @param control From race_coxnet_control().
#' @return A list: `results`, each split's metrics in the original order, and
#'   `candidates`, one row per (`mixture`, `penalty`) with `n_resamples`
#'   (resamples it was scored on) and `eliminated`.
#' @keywords internal
#' @noRd
race_coxnet_resamples <- function(splits, ids, score_split, map_splits, mixture, paths,
                                  info, metric, eval_time, control) {
  n <- length(splits)
  burn_in <- min(control$burn_in, max(2L, n - 1L))
  order <- if (control$randomize && n > 1) sample.int(n) else seq_len(n)
  direction <- info$direction[info$metric == metric]
  dynamic <- info$class[info$metric == metric] == "dynamic_survival_metric"
  metric_time <- if (dynamic) eval_time[1] else NA_real_

  candidates <- purrr::list_rbind(Map(function(m, p) {
    tibble::tibble(mixture = m, penalty = p)
  }, mixture, paths))
  active <- rep(TRUE, nrow(candidates))
  results <- vector("list", n)

  first <- order[seq_len(min(burn_in, n))]
  results[first] <- map_splits(splits[first], ids[first])
  done <- length(first)

  tag <- function(res, id) {
    if (is.null(res) || !nrow(res)) return(NULL)
    res$id <- id
    res
  }
  while (done < n) {
    scored <- purrr::list_rbind(Map(tag, results[order[seq_len(done)]], ids[order[seq_len(done)]]))
    if (!is.null(scored) && nrow(scored)) {
      keep <- race_anova_survivors(
        scored, candidates[active, , drop = FALSE], metric, metric_time, direction, control$alpha
      )
      active[active] <- keep
    }
    i <- order[done + 1L]
    score_at <- lapply(seq_along(mixture), function(j) {
      candidates$penalty[active & candidates$mixture == mixture[j]]
    })
    results[i] <- list(score_split(splits[[i]], ids[i], score_at))
    done <- done + 1L
  }

  scored <- purrr::list_rbind(Map(tag, results, ids))
  n_res <- if (is.null(scored) || !nrow(scored)) {
    rep(0L, nrow(candidates))
  } else {
    sel <- scored[scored$.metric == metric, , drop = FALSE]
    counts <- dplyr::count(dplyr::distinct(sel, .data$mixture, .data$penalty, .data$id), .data$mixture, .data$penalty)
    hit <- match(paste(candidates$mixture, candidates$penalty), paste(counts$mixture, counts$penalty))
    ifelse(is.na(hit), 0L, counts$n[hit])
  }
  candidates$n_resamples <- as.integer(n_res)
  candidates$eliminated <- !active
  list(results = results, candidates = candidates)
}

#' Which Active Candidates Survive One ANOVA Racing Step
#'
#' @param scored Fold metrics so far (`id`, `mixture`, `penalty`, `.metric`,
#'   `.eval_time`, `.estimate`).
#' @param active The candidates still in the race (`mixture`, `penalty`).
#' @return A logical vector along `active`: `TRUE` for survivors. Candidates
#'   with no estimate yet, or when the model cannot be fit, survive.
#' @keywords internal
#' @noRd
race_anova_survivors <- function(scored, active, metric, metric_time, direction, alpha) {
  keep <- rep(TRUE, nrow(active))
  d <- scored[scored$.metric == metric & !is.na(scored$.estimate), , drop = FALSE]
  if (!is.na(metric_time)) {
    d <- d[!is.na(d$.eval_time) & d$.eval_time == metric_time, , drop = FALSE]
  }
  active_key <- paste(active$mixture, active$penalty)
  d$.config <- paste(d$mixture, d$penalty)
  d <- d[d$.config %in% active_key, , drop = FALSE]
  if (length(unique(d$.config)) < 2 || length(unique(d$id)) < 2) {
    return(keep)
  }
  means <- tapply(d$.estimate, d$.config, mean)
  best <- if (direction == "minimize") names(which.min(means)) else names(which.max(means))
  d$.config <- factor(d$.config, levels = c(best, setdiff(names(means), best)))
  d$id <- factor(d$id)
  fit <- tryCatch(stats::lm(.estimate ~ .config + id, data = d), error = function(e) NULL)
  if (is.null(fit) || fit$df.residual < 1) {
    return(keep)
  }
  ci <- suppressWarnings(tryCatch(stats::confint(fit, level = 1 - alpha), error = function(e) NULL))
  if (is.null(ci)) {
    return(keep)
  }
  terms <- paste0(".config", levels(d$.config)[-1])
  ci <- ci[intersect(terms, rownames(ci)), , drop = FALSE]
  worse <- if (direction == "minimize") ci[, 1] > 0 else ci[, 2] < 0
  worse <- worse & !is.na(worse)
  dropped <- sub("^\\.config", "", rownames(ci)[worse])
  keep[active_key %in% dropped] <- FALSE
  keep
}
