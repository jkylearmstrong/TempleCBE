#' Sensitivity Analysis Across Imputation Strategies
#'
#' Refits the same model under several imputation strategies and summarises
#' how its estimates move. The motivating case (fresh-review B4) is a result
#' that rests on one imputation choice — for example median imputation of a
#' variable that is 42% missing — with no check of whether another choice
#' would change the conclusion. This function makes that check explicit: one
#' fit per strategy, one row per term per strategy, and a per-term summary
#' (minimum, maximum, range and standard deviation of the estimate across
#' strategies), plus a Rubin's-rules pool ([pool_imputations()]) over the
#' strategies when every strategy reports the same terms.
#'
#' A strategy is either a completed data frame, a function
#' `function(data)` returning a completed data frame, or one of the built-in
#' names:
#' \describe{
#'   \item{`"complete_cases"`}{rows with any missing value dropped
#'     ([stats::na.omit()]); nothing is imputed.}
#'   \item{`"median"` / `"mean"`}{numeric columns filled with their observed
#'     median / mean; factor and character columns with their mode.}
#'   \item{`"missforest"` / `"missranger"`}{the whole data set imputed by
#'     [missforest_sweep_mtry()] / [missranger_sweep_mtry()] (needs the
#'     corresponding package); extra sweep arguments come from `...`.}
#' }
#'
#' @section Reading the summary:
#' A term whose estimate range across strategies is large relative to its
#' standard error (compare `range` with `median_se`) is sensitive to the
#' imputation choice: the reported result depends on how the missing values
#' were filled in, and the analysis should say so. The pooled estimate treats
#' the strategies as imputations for Rubin's rules; that is a summary device,
#' not a substitute for proper multiple imputation — the strategies are
#' chosen methods, not draws from a posterior predictive distribution (see
#' [pool_imputations()]).
#'
#' @param data A data frame with missing values.
#' @param fit_fn A function `function(data)` returning a fitted model. Its
#'   coefficients are extracted with [broom::tidy()] when that gives `term`,
#'   `estimate` and `std.error`, and otherwise from [stats::coef()] and the
#'   diagonal of [stats::vcov()].
#' @param strategies A named list of at least two strategies (see above).
#'   Names must be unique and non-empty.
#' @param conf_level Confidence level passed to [pool_imputations()] for the
#'   pooled table.
#' @param ... Passed to [missforest_sweep_mtry()] / [missranger_sweep_mtry()]
#'   for the `"missforest"` / `"missranger"` strategies.
#' @return A list of class `imputation_sensitivity` with
#'   \item{strategies}{the strategy names, in order}
#'   \item{n_imputed}{a tibble with `strategy`, `n_imputed` (missing cells
#'     filled; `NA` when a strategy reordered the rows and changed values,
#'     so its rows cannot be aligned with `data` without a row id) and
#'     `n_dropped` (rows dropped; non-zero only for
#'     `"complete_cases"`)}
#'   \item{estimates}{a tibble with `term`, `strategy`, `estimate` and
#'     `std.error`}
#'   \item{summary}{one row per term: `term`, `n_strategies`, `min`,
#'     `max`, `range`, `sd`, `median_se`, and, when a pooled table exists,
#'     `pooled_estimate` and `pooled_se`}
#'   \item{pooled}{the [pool_imputations()] table over the strategies, or
#'     `NULL` when the strategies do not report the same terms (with the
#'     reason in `pooled_note`)}
#'   \item{pooled_note}{character string, or `NULL` when pooling succeeded}
#'   \item{fits}{named list of the fitted model per strategy}
#' @seealso [pool_imputations()], [tune_impute_dataset()],
#'   [missforest_sweep_mtry()], [missranger_sweep_mtry()]
#' @param x The object to print (the result of the function).
#' @export
#' @examples
#' d <- mtcars
#' d$hp[c(3, 11, 20)] <- NA
#' d$wt[c(5, 15)] <- NA
#' res <- imputation_sensitivity(
#'   d,
#'   fit_fn = function(x) stats::lm(mpg ~ wt + hp, data = x),
#'   strategies = list(median = "median", mean = "mean", complete_cases = "complete_cases")
#' )
#' res$summary
imputation_sensitivity <- function(data, fit_fn, strategies, conf_level = 0.95, ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.function(fit_fn)) {
    stop("`fit_fn` must be a function of one argument, the (completed) data.", call. = FALSE)
  }
  if (!is.list(strategies) || length(strategies) < 2L || is.null(names(strategies)) ||
      any(!nzchar(names(strategies))) || anyDuplicated(names(strategies))) {
    stop("`strategies` must be a named list of at least two uniquely-named strategies.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }
  if (sum(is.na(data)) == 0L) {
    warning("`data` has no missing values; the strategies will agree trivially.", call. = FALSE)
  }

  complete_one <- function(strategy, nm) {
    if (is.data.frame(strategy)) {
      return(strategy)
    }
    if (is.function(strategy)) {
      out <- strategy(data)
      if (!is.data.frame(out)) {
        stop("Strategy '", nm, "': a function strategy must return a data frame.", call. = FALSE)
      }
      return(out)
    }
    if (is.character(strategy) && length(strategy) == 1L) {
      switch(strategy,
        complete_cases = return(stats::na.omit(data)),
        median = return(.impute_constant(data, stats::median)),
        mean = return(.impute_constant(data, function(z) mean(z, na.rm = TRUE))),
        missforest = return(missforest_sweep_mtry(data, ...)$imp_data),
        missranger = return(missranger_sweep_mtry(data, ...)$imp_data),
        stop("Strategy '", nm, "': unknown built-in strategy '", strategy,
             "'. Use 'complete_cases', 'median', 'mean', 'missforest', 'missranger', ",
             "a data frame, or a function.", call. = FALSE)
      )
    }
    stop("Strategy '", nm, "' must be a built-in name, a data frame, or a function.", call. = FALSE)
  }

  tidy_one <- function(fit, nm) {
    td <- tryCatch(
      {
        tb <- broom::tidy(fit)
        if (!all(c("term", "estimate", "std.error") %in% names(tb))) stop("no term/estimate/std.error")
        tibble::tibble(term = as.character(tb$term), estimate = tb$estimate, std.error = tb$std.error)
      },
      error = function(e) NULL
    )
    if (!is.null(td)) {
      return(td)
    }
    cf <- tryCatch(stats::coef(fit), error = function(e) NULL)
    vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    if (is.null(cf) || is.null(vc)) {
      stop("Strategy '", nm, "': could not extract coefficients from the fitted model ",
           "(broom::tidy() failed and coef()/vcov() are unavailable).", call. = FALSE)
    }
    tibble::tibble(
      term = names(cf), estimate = unname(cf),
      std.error = sqrt(diag(vc))[names(cf)]
    )
  }

  fits <- list()
  estimates <- list()
  n_imputed <- tibble::tibble(strategy = character(), n_imputed = integer(), n_dropped = integer())
  for (nm in names(strategies)) {
    completed <- complete_one(strategies[[nm]], nm)
    if (!is.data.frame(completed)) {
      stop("Strategy '", nm, "' did not return a data frame.", call. = FALSE)
    }
    if (anyNA(completed)) {
      warning("Strategy '", nm, "' left ", sum(is.na(completed)),
              " missing value(s) in the completed data.", call. = FALSE)
    }
    # Cells filled are only countable when the completed data has the same
    # rows and columns as `data` (a strategy that drops rows, such as
    # "complete_cases", reports its dropped rows instead), and only when the
    # rows are still in the same order: a positional comparison miscounts
    # when a strategy reorders rows. Row names identify the order for data
    # frames; a pure reordering fills nothing (0), while a reordering
    # combined with changed values cannot be aligned without a row id, so
    # it reports NA rather than a positional guess. (Tibbles carry no row
    # names, so only the pure-reordering case is detected for them.)
    comp <- NULL
    n_filled <- if (identical(nrow(completed), nrow(data)) &&
                   all(names(data) %in% names(completed))) {
      comp <- completed[, names(data), drop = FALSE]
      if (.same_row_multiset(comp, data) && !.same_row_sequence(comp, data)) {
        0L
      } else if (!is.null(rownames(comp)) && !is.null(rownames(data)) &&
                 !identical(rownames(comp), rownames(data))) {
        NA_integer_
      } else {
        sum(is.na(data) & !is.na(comp))
      }
    } else {
      0L
    }
    n_imputed <- dplyr::bind_rows(n_imputed, tibble::tibble(
      strategy = nm, n_imputed = as.integer(n_filled),
      n_dropped = as.integer(nrow(data) - nrow(completed))
    ))
    fit <- tryCatch(fit_fn(completed), error = function(e) e)
    if (inherits(fit, "error")) {
      stop("Strategy '", nm, "': `fit_fn()` failed: ", conditionMessage(fit), call. = FALSE)
    }
    fits[[nm]] <- fit
    td <- tidy_one(fit, nm)
    td$strategy <- nm
    estimates[[nm]] <- td[, c("term", "strategy", "estimate", "std.error")]
  }
  estimates <- dplyr::bind_rows(estimates)

  terms <- unique(estimates$term)
  summary_rows <- lapply(terms, function(tm) {
    sub <- estimates[estimates$term == tm, , drop = FALSE]
    # min()/max() on all-NA estimates return Inf/-Inf (with a warning);
    # a term with no finite estimate has no minimum, maximum or range.
    fin <- sub$estimate[is.finite(sub$estimate)]
    fin_se <- sub$std.error[is.finite(sub$std.error)]
    tibble::tibble(
      term = tm,
      n_strategies = nrow(sub),
      min = if (length(fin)) min(fin) else NA_real_,
      max = if (length(fin)) max(fin) else NA_real_,
      range = if (length(fin)) max(fin) - min(fin) else NA_real_,
      sd = if (length(fin) > 1L) stats::sd(fin) else NA_real_,
      median_se = if (length(fin_se)) stats::median(fin_se) else NA_real_
    )
  })
  summary <- dplyr::bind_rows(summary_rows)

  # Pool only when every strategy reports the same terms with finite values;
  # otherwise report why, instead of pooling a subset silently.
  pooled <- NULL
  pooled_note <- NULL
  per_strategy <- lapply(names(strategies), function(nm) {
    estimates[estimates$strategy == nm, c("term", "estimate", "std.error")]
  })
  same_terms <- all(vapply(per_strategy, function(d) identical(sort(d$term), sort(terms)), logical(1)))
  finite <- all(is.finite(estimates$estimate)) && all(is.finite(estimates$std.error))
  if (!same_terms) {
    pooled_note <- "Not pooled: the strategies do not all report the same terms."
  } else if (!finite) {
    pooled_note <- "Not pooled: some estimates or standard errors are not finite."
  } else {
    pooled <- pool_imputations(per_strategy, conf_level = conf_level)
    summary$pooled_estimate <- pooled$estimate[match(summary$term, pooled$term)]
    summary$pooled_se <- pooled$std.error[match(summary$term, pooled$term)]
  }

  out <- list(
    strategies = names(strategies), n_imputed = n_imputed, estimates = estimates,
    summary = summary, pooled = pooled, pooled_note = pooled_note, fits = fits
  )
  class(out) <- c("imputation_sensitivity", "list")
  out
}

#' @rdname imputation_sensitivity
#' @export
print.imputation_sensitivity <- function(x, ...) {
  cat("<imputation_sensitivity> strategies: ", paste(x$strategies, collapse = ", "), "\n", sep = "")
  cat("  per-term range of estimates across strategies:\n")
  print(x$summary, n = nrow(x$summary))
  if (!is.null(x$pooled_note)) cat("  pooled: ", x$pooled_note, "\n", sep = "")
  invisible(x)
}

# One string key per row, so two frames can be compared as a multiset or as
# a sequence of rows regardless of column types. Missing values get their
# own token so that a filled cell (NA -> value) changes the key. A frame
# with a column this cannot stringify (for example a list-column) yields
# NULL, and the caller falls back to the positional count.
.row_keys <- function(df) {
  if (nrow(df) == 0L) {
    return(character())
  }
  keys <- tryCatch(
    {
      parts <- lapply(df, function(z) {
        z <- as.character(z)
        z[is.na(z)] <- "\001"
        z
      })
      do.call(paste, c(parts, list(sep = "\002")))
    },
    error = function(e) NULL
  )
  keys
}

# TRUE when `a` and `b` hold the same rows in any order (a pure reordering
# fills nothing, so its `n_imputed` is 0); FALSE when the keys cannot be
# compared or the multisets differ.
.same_row_multiset <- function(a, b) {
  ka <- .row_keys(a)
  kb <- .row_keys(b)
  if (is.null(ka) || is.null(kb) || length(ka) != length(kb)) {
    return(FALSE)
  }
  identical(sort(ka), sort(kb))
}

# TRUE when `a` and `b` hold the same rows in the same order.
.same_row_sequence <- function(a, b) {
  ka <- .row_keys(a)
  kb <- .row_keys(b)
  if (is.null(ka) || is.null(kb)) {
    return(FALSE)
  }
  identical(ka, kb)
}

# Fill numeric columns with a constant summary (median/mean) and
# factor/character columns with their mode. Columns that are entirely NA are
# left alone (there is nothing to summarise) and warned about by the caller.
.impute_constant <- function(data, fn) {
  out <- data
  for (nm in names(out)) {
    z <- out[[nm]]
    if (!anyNA(z)) next
    if (is.numeric(z)) {
      obs <- z[!is.na(z)]
      if (length(obs) == 0L) next
      fill <- fn(obs)
      if (length(fill) == 1L && is.finite(fill)) out[[nm]][is.na(z)] <- fill
    } else if (is.factor(z) || is.character(z)) {
      obs <- z[!is.na(z)]
      if (length(obs) > 0L) {
        mode <- names(sort(table(obs), decreasing = TRUE))[1]
        out[[nm]][is.na(z)] <- mode
      }
    }
  }
  out
}
