# ==============================================================================
# G5: as_tuning_results() — Adapter for Historical & Tidymodels Tuning Outputs
# ==============================================================================

#' Coerce Historical or Tidymodels Tuning Outputs to Standardized Schema (G5)
#'
#' Adapts older Wolfson tuning outputs (`tuning_results_df_*.rds`), tidymodels
#' tuning results, or generic hyperparameter evaluation tables to the standardized
#' schema required by [compare_tuning_runs()], [select_candidate_models()], and
#' [plot.cbe_nested_survival_run()].
#'
#' Maps historical and tidymodels column synonyms to canonical names:
#' \itemize{
#'   \item Integrated Brier Score: `ibs`, `mean_ibs`, `mean` (when `.metric` is `"brier_survival_integrated"`), `brier_score` -> `IBS`.
#'   \item Elastic net mixing: `alphas`, `mixture` -> `alpha`.
#'   \item Penalty parameter: `penalty`, `lambdas` -> `lambda`.
#'   \item Model formula: `formulas`, `formula_str` -> `formula`.
#'   \item Resampling split: `split`, `splits`, `resample`, `id` -> `inner_resamples_splits`.
#'   \item Outer cross-validation fold: `fold`, `outer_id` -> `outer_fold`.
#' }
#'
#' @param x A data frame, tibble, path to an `.rds` file, or a tuning result object.
#' @param ... Unused.
#' @return A standardized [tibble::tibble()] guaranteed to contain `alpha` and `IBS` columns.
#' @seealso [compare_tuning_runs()], [select_candidate_models()]
#' @export
#' @examples
#' # Historical Wolfson format with lowercase column names
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
  if (".metric" %in% col_names) {
    brier_rows <- df$.metric %in% c("brier_survival_integrated", "brier_survival", "brier", "ibs")
    if (any(brier_rows)) {
      df <- df[brier_rows, , drop = FALSE]
    }
  }

  # 2. Map IBS
  if (!"IBS" %in% names(df)) {
    if ("ibs" %in% names(df)) {
      df$IBS <- df$ibs
    } else if ("mean_ibs" %in% names(df)) {
      df$IBS <- df$mean_ibs
    } else if ("mean" %in% names(df)) {
      df$IBS <- df$mean
    } else if ("integrated_brier_score" %in% names(df)) {
      df$IBS <- df$integrated_brier_score
    } else if ("brier_score" %in% names(df)) {
      df$IBS <- df$brier_score
    } else if (".estimate" %in% names(df)) {
      df$IBS <- df$.estimate
    }
  }

  # 3. Map alpha
  if (!"alpha" %in% names(df)) {
    if ("mixture" %in% names(df)) {
      df$alpha <- df$mixture
    } else if ("alphas" %in% names(df)) {
      df$alpha <- df$alphas
    } else {
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

  df$alpha <- as.numeric(df$alpha)
  df$IBS <- as.numeric(df$IBS)
  if ("lambda" %in% names(df)) {
    df$lambda <- as.numeric(df$lambda)
  }
  if ("formula" %in% names(df)) {
    df$formula <- as.character(df$formula)
  }

  dots <- list(...)
  if (!is.null(dots$domain)) {
    df$domain <- as.character(dots$domain)
  }
  if (!".metric" %in% names(df)) {
    df$.metric <- "brier_survival_integrated"
  }
  if (!".estimate" %in% names(df) && "IBS" %in% names(df)) {
    df$.estimate <- df$IBS
  }

  class(df) <- unique(c("cbe_tuning_results", class(df)))
  df
}

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

#' @export
tidy.cbe_tuning_results <- function(x, ...) {
  tibble::as_tibble(x)
}
