#' Univariable Kaplan-Meier and Cox Screening Engine
#'
#' Pairs a univariable [cbe_cox_single()] Cox proportional hazards fit with the
#' matching Kaplan-Meier stratified survival curve for the same predictor, so both
#' are produced from a single call without refitting the Cox model twice.
#'
#' @name cbe_km_single
NULL

#' Analyze a Single Predictor with Kaplan-Meier and Cox Proportional Hazards
#'
#' @param data A data frame containing survival data and the candidate feature.
#' @param outcome Character string naming the Surv object or outcome column in \code{data}
#'   (e.g., \code{"outcome"} or \code{"Surv(survival_time, status)"}).
#' @param feature Character string naming the candidate predictor column in \code{data}.
#'   Factor (or coercible) predictors are used directly as Kaplan-Meier strata; numeric
#'   predictors are binned into quartiles for the Kaplan-Meier strata (matching
#'   [plot_cox_survival()]'s convention), while the Cox side treats them as continuous.
#' @param conf_level Numeric confidence level (default: 0.95).
#' @return An object of class \code{cbe_km} containing:
#'   \itemize{
#'     \item \code{cox}: The \code{cbe_cox} object from [cbe_cox_single()].
#'     \item \code{km_fit}: The \code{survival::survfit} object for the Kaplan-Meier curve.
#'     \item \code{km_tidy}: \code{broom::tidy()} table of \code{km_fit}.
#'     \item \code{direction}: \code{"increases"} or \code{"decreases"}, pulled from the Cox
#'       hazard ratio direction.
#'     \item \code{summary}: One-row-per-stratum tidy summary combining Cox hazard ratios and
#'       Kaplan-Meier median survival.
#'     \item \code{feature}: Feature name string.
#'     \item \code{var_label}: Human-readable variable label.
#'   }
#' @seealso [cbe_cox_single()], [plot_cox_survival()]
#' @export
cbe_km_single <- function(data, outcome = "outcome", feature, conf_level = 0.95) {
  cox <- cbe_cox_single(data, outcome = outcome, feature = feature, conf_level = conf_level)

  col_vals <- data[[feature]]
  if (cox$is_numeric) {
    strata_data <- data
    strata_data$.cbe_km_strata <- dplyr::ntile(col_vals, 4)
    strata_data <- strata_data |>
      dplyr::group_by(.data$.cbe_km_strata) |>
      dplyr::mutate(
        .cbe_km_strata_label = sprintf(
          "[%s \u2013 %s]",
          fmt_num(min(.data[[feature]], na.rm = TRUE), 1),
          fmt_num(max(.data[[feature]], na.rm = TRUE), 1)
        )
      ) |>
      dplyr::ungroup()
    strata_col <- ".cbe_km_strata_label"
  } else {
    strata_data <- data
    strata_col <- feature
  }

  surv_fmla <- stats::as.formula(sprintf("%s ~ %s", outcome, strata_col))
  km_fit <- survival::survfit(surv_fmla, data = strata_data)
  km_tidy <- broom::tidy(km_fit)

  hr_val <- if (cox$is_numeric) cox$table$HR[1] else cox$table$HR[cox$table$Role == "Comparison"][1]
  direction <- if (hr_val > 1) "increases" else "decreases"

  km_raw_tab <- tryCatch(summary(km_fit)$table, error = function(e) NULL)
  km_strata_labels <- if (!is.null(km_fit$strata)) sub("^.*=", "", names(km_fit$strata)) else strata_col
  km_median <- if (!is.null(km_raw_tab)) {
    if (is.matrix(km_raw_tab)) {
      if ("median" %in% colnames(km_raw_tab)) unname(km_raw_tab[, "median"]) else rep(NA_real_, nrow(km_raw_tab))
    } else if ("median" %in% names(km_raw_tab)) {
      unname(km_raw_tab["median"])
    } else {
      rep(NA_real_, length(km_strata_labels))
    }
  } else {
    rep(NA_real_, length(km_strata_labels))
  }

  km_summary <- tibble::tibble(Level = km_strata_labels, km_median_time = km_median)

  ci_col <- grep("% CI$", names(cox$table), value = TRUE)[1]
  if (is.na(ci_col)) ci_col <- "95% CI"

  summary_tbl <- if (cox$is_numeric) {
    dplyr::bind_cols(
      km_summary,
      cox$table[rep(1, nrow(km_summary)), c("Variable", "HR", ci_col, "p.value")]
    )
  } else {
    dplyr::left_join(km_summary, cox$table, by = "Level")
  }

  structure(
    list(
      cox       = cox,
      km_fit    = km_fit,
      km_tidy   = km_tidy,
      direction = direction,
      summary   = summary_tbl,
      feature   = feature,
      var_label = cox$var_label
    ),
    class = "cbe_km"
  )
}

#' Print Method for cbe_km Object
#'
#' @param x Object of class \code{cbe_km}
#' @param ... Additional arguments (unused)
#' @export
print.cbe_km <- function(x, ...) {
  cat("=======================================================\n")
  cat(sprintf("Kaplan-Meier & Cox PH Summary for: %s (%s)\n", x$var_label, x$feature))
  cat("=======================================================\n\n")
  cat(sprintf("The risk of the event %s with %s.\n\n", x$direction, x$var_label))
  cat("Cox Coefficients:\n")
  print(x$cox$table)
  cat("\nKaplan-Meier Curve:\n")
  print(x$km_fit)
  invisible(x)
}

utils::globalVariables(c(".cbe_km_strata"))
