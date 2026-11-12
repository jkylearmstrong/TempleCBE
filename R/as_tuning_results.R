# ==============================================================================
# G5: as_tuning_results() — Adapter for Historical & Tidymodels Tuning Outputs
# ==============================================================================

#' Coerce Historical or Tidymodels Tuning Outputs to Standardized Schema (G5)
#'
#' Adapts older legacy tuning outputs (`tuning_results_df_*.rds`), tidymodels
#' tuning results, or generic hyperparameter evaluation tables to the standardized
#' schema required by [compare_tuning_runs()], [select_candidate_models()], and
#' [plot.cbe_nested_survival_run()].
#'
#' Maps historical and tidymodels column synonyms to canonical names:
#' \itemize{
#'   \item Integrated Brier Score: `ibs`, `mean_ibs`, `integrated_brier_score`, `brier_score` -> `IBS`.
#'     The generic tidymodels columns `mean` and `.estimate` are taken as the IBS only when a
#'     `.metric` column says that every remaining row is a Brier metric
#'     (`"brier_survival_integrated"`, `"brier_survival"`, `"brier"` or `"ibs"`). Without
#'     `.metric`, a `mean` could be the mean of any metric (a concordance, for example), so it
#'     is not used and the call fails with the list of columns found.
#'   \item Elastic net mixing: `alphas`, `mixture` -> `alpha`. A table with none of these gets
#'     `alpha = 1` (lasso) with a warning, because the mixing parameter is then an assumption,
#'     not something read from the data.
#'   \item Penalty parameter: `penalty`, `lambdas` -> `lambda`.
#'   \item Model formula: `formulas`, `formula_str` -> `formula`.
#'   \item Resampling split: `split`, `splits`, `resample`, `id` -> `inner_resamples_splits`.
#'   \item Outer cross-validation fold: `fold`, `outer_id` -> `outer_fold`.
#' }
#'
#' @param x A data frame, tibble, path to an `.rds` file, or a tuning result object. An
#'   `.rds` file is read with [readRDS()], which can run code stored in the file: pass only
#'   files that you or a process you trust wrote.
#' @param ... Unused.
#' @return A standardized [tibble::tibble()] guaranteed to contain `alpha` and `IBS` columns.
#' @seealso [compare_tuning_runs()], [select_candidate_models()]
#' @export
#' @examples
#' # Historical legacy format with lowercase column names
#' old_df <- data.frame(alphas = c(0.1, 0.5), ibs = c(0.12, 0.10), penalty = c(0.01, 0.02))
#' standardized <- as_tuning_results(old_df)
#' names(standardized)
as_tuning_results <- function(x, ...) {
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    x <- tryCatch(readRDS(x), error = function(e) {
      stop(sprintf("Failed to read tuning results RDS file '%s': %s", x, conditionMessage(e)), call. = FALSE)
    })
  }

  if (inherits(x, "tune_results")) {
    rlang::check_installed("tune", reason = "to collect metrics from tune_results.")
    x <- tune::collect_metrics(x)
  }

  if (!is.data.frame(x)) {
    stop("`x` must be a data frame, tibble, tune_results object, or path to an RDS file.", call. = FALSE)
  }

  df <- tibble::as_tibble(x)
  col_names <- names(df)

  # 1. Filter tidymodels metric sets to IBS/Brier metric if multiple metrics present
  metric_is_brier <- FALSE
  ibs_source <- NULL
  if (".metric" %in% col_names) {
    brier_rows <- df$.metric %in% c("brier_survival_integrated", "brier_survival", "brier", "ibs")
    brier_rows[is.na(brier_rows)] <- FALSE
    if (any(brier_rows)) {
      dropped <- sum(!brier_rows)
      if (dropped > 0L) {
        warning("Dropping ", dropped, " row(s) whose `.metric` is not a Brier metric: ",
                paste(unique(stats::na.omit(df$.metric[!brier_rows])), collapse = ", "), ".", call. = FALSE)
      }
      df <- df[brier_rows, , drop = FALSE]
      metric_is_brier <- TRUE
    }
  }

  # 2. Map IBS
  if ("IBS" %in% names(df)) {
    ibs_source <- "IBS"
  } else {
    if ("ibs" %in% names(df)) {
      df$IBS <- df$ibs
      ibs_source <- "ibs"
    } else if ("mean_ibs" %in% names(df)) {
      df$IBS <- df$mean_ibs
      ibs_source <- "mean_ibs"
    } else if ("integrated_brier_score" %in% names(df)) {
      df$IBS <- df$integrated_brier_score
      ibs_source <- "integrated_brier_score"
    } else if ("brier_score" %in% names(df)) {
      df$IBS <- df$brier_score
      ibs_source <- "brier_score"
    } else if (metric_is_brier && "mean" %in% names(df)) {
      # `mean` and `.estimate` are whatever `.metric` names: the IBS only when
      # `.metric` says so
      df$IBS <- df$mean
      ibs_source <- "mean"
    } else if (metric_is_brier && ".estimate" %in% names(df)) {
      df$IBS <- df$.estimate
      ibs_source <- ".estimate"
    } else if (any(c("mean", ".estimate") %in% names(df))) {
      stop(
        "Could not adapt tuning results to standardized format: the column `",
        intersect(c("mean", ".estimate"), names(df))[1],
        "` is taken as the IBS only when a `.metric` column identifies it as a Brier metric ",
        "(brier_survival_integrated, brier_survival, brier or ibs). ",
        "Rename the column to `IBS` if that is what it holds. Found columns: ",
        paste(col_names, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  # 3. Map alpha
  if (!"alpha" %in% names(df)) {
    if ("mixture" %in% names(df)) {
      df$alpha <- df$mixture
    } else if ("alphas" %in% names(df)) {
      df$alpha <- df$alphas
    } else if (!"IBS" %in% names(df)) {
      # nothing usable at all: the error below lists the columns
      NULL
    } else {
      warning(
        "The tuning results have no `alpha`, `mixture` or `alphas` column: `alpha` is set to 1 (lasso). ",
        "Add an `alpha` column if the models were fitted with another mixing parameter.",
        call. = FALSE
      )
      df$alpha <- 1.0
    }
  }

  # 4. Map lambda
  if (!"lambda" %in% names(df)) {
    if ("penalty" %in% names(df)) {
      df$lambda <- df$penalty
    } else if ("lambdas" %in% names(df)) {
      df$lambda <- df$lambdas
    }
  }

  # 5. Map formula
  if (!"formula" %in% names(df)) {
    if ("formulas" %in% names(df)) {
      df$formula <- df$formulas
    } else if ("formula_str" %in% names(df)) {
      df$formula <- df$formula_str
    }
  }

  # 6. Map inner split
  if (!"inner_resamples_splits" %in% names(df)) {
    if ("split" %in% names(df)) {
      df$inner_resamples_splits <- as.character(df$split)
    } else if ("splits" %in% names(df)) {
      df$inner_resamples_splits <- as.character(df$splits)
    } else if ("resample" %in% names(df)) {
      df$inner_resamples_splits <- as.character(df$resample)
    } else if ("id" %in% names(df)) {
      df$inner_resamples_splits <- as.character(df$id)
    }
  }

  # 7. Map outer fold
  if (!"outer_fold" %in% names(df)) {
    if ("fold" %in% names(df)) {
      df$outer_fold <- as.character(df$fold)
    } else if ("outer_id" %in% names(df)) {
      df$outer_fold <- as.character(df$outer_id)
    }
  }

  # Validate essential columns
  if (!all(c("alpha", "IBS") %in% names(df))) {
    missing <- setdiff(c("alpha", "IBS"), names(df))
    stop(
      sprintf("Could not adapt tuning results to standardized format. Missing required column(s): %s. Found columns: %s.",
              paste(missing, collapse = ", "), paste(col_names, collapse = ", ")),
      call. = FALSE
    )
  }

  # Values that cannot be parsed as numbers (e.g. IBS = "abc") used to coerce
  # to NA with only a warning from as.numeric(): name them instead.
  for (col in c("alpha", "IBS")) {
    raw <- df[[col]]
    if (is.character(raw)) {
      coerced <- suppressWarnings(as.numeric(raw))
      bad <- !is.na(raw) & is.na(coerced)
      if (any(bad)) {
        stop("`", col, "` has value(s) that are not numbers: ",
             paste(unique(raw[bad]), collapse = ", "), ".", call. = FALSE)
      }
    }
  }
  df$alpha <- as.numeric(df$alpha)
  df$IBS <- as.numeric(df$IBS)
  if (any(!is.na(df$alpha) & (df$alpha < 0 | df$alpha > 1))) {
    stop("`alpha` must be between 0 and 1.", call. = FALSE)
  }
  if (any(!is.finite(df$IBS[!is.na(df$IBS)]))) {
    stop("`IBS` must be finite.", call. = FALSE)
  }
  if (any(!is.na(df$IBS) & (df$IBS < 0 | df$IBS > 1))) {
    stop("`IBS` must be between 0 and 1 (it is a Brier score).", call. = FALSE)
  }
  if ("lambda" %in% names(df)) {
    df$lambda <- as.numeric(df$lambda)
  }
  if ("formula" %in% names(df)) {
    df$formula <- as.character(df$formula)
  }

  dots <- list(...)
  if (!is.null(dots$domain)) {
    if (!is.character(dots$domain) || length(dots$domain) != 1L || is.na(dots$domain)) {
      stop("`domain` must be a single string.", call. = FALSE)
    }
    df$domain <- dots$domain
  }
  if (!".metric" %in% names(df)) {
    # Stamp the integrated-Brier name only when the IBS is known to be an
    # integrated Brier score. A `brier_score` column may be a point Brier
    # score, so it gets the generic "brier" name instead.
    df$.metric <- if (identical(ibs_source, "brier_score")) "brier" else "brier_survival_integrated"
  } else {
    # The output's `IBS` column is always an integrated Brier score, so a
    # `.metric` that says otherwise mislabels it (e.g. an input with
    # `.metric = "concordance"` and an `ibs` column used to keep
    # `.metric = "concordance"` next to the new `IBS` column). Overwrite it
    # with the Brier name and say so; a `.metric` that already names a Brier
    # metric is left untouched.
    brier_name <- if (identical(ibs_source, "brier_score")) "brier" else "brier_survival_integrated"
    brier_names <- c("brier_survival_integrated", "brier_survival", "brier", "ibs")
    non_brier <- unique(stats::na.omit(df$.metric[!df$.metric %in% brier_names]))
    if (length(non_brier) || anyNA(df$.metric)) {
      warning(
        "The `.metric` column names a non-Brier metric",
        if (length(non_brier)) paste0(" (", paste(non_brier, collapse = ", "), ")") else " (or is missing)",
        " but `IBS` holds an integrated Brier score; `.metric` is overwritten with \"",
        brier_name, "\".",
        call. = FALSE
      )
      df$.metric <- brier_name
    }
  }
  if (!".estimate" %in% names(df) && "IBS" %in% names(df)) {
    df$.estimate <- df$IBS
  }

  class(df) <- unique(c("cbe_tuning_results", class(df)))
  df
}

#' @rdname templecbe_methods
#' @export
summary.cbe_tuning_results <- function(object, ...) {
  structure(
    list(
      n_rows = nrow(object),
      mean_ibs = mean(object$IBS, na.rm = TRUE),
      median_ibs = stats::median(object$IBS, na.rm = TRUE),
      alphas = sort(unique(object$alpha)),
      data = object
    ),
    class = "summary.cbe_tuning_results"
  )
}

#' @rdname templecbe_methods
#' @export
tidy.cbe_tuning_results <- function(x, ...) {
  tibble::as_tibble(x)
}
