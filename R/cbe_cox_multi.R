#' Multivariable Cox Proportional Hazards Modeling Engine
#'
#' Fits a multivariable Cox proportional hazards model, validates the proportional
#' hazards assumption for every term (including the multivariate global test),
#' formats coefficients with explicit reference levels grouped by variable, and
#' gathers model fit and convergence metrics.
#'
#' @name cbe_cox_multi
NULL

#' Analyze Multiple Predictors in a Cox Proportional Hazards Model
#'
#' @param data A data frame containing survival data and the candidate features.
#' @param formula Optional. A model formula with a \code{survival::Surv()} outcome,
#'   e.g. \code{Surv(time, status) ~ age + sex}. When supplied, \code{outcome} and
#'   \code{features} are ignored.
#' @param outcome Character string naming the Surv object or outcome column in \code{data}
#'   (e.g., \code{"outcome"} or \code{"Surv(survival_time, status)"}). Ignored if \code{formula}
#'   is supplied.
#' @param features Character vector of candidate predictor columns in \code{data}. Required
#'   if \code{formula} is not supplied.
#' @param conf_level Numeric confidence level (default: 0.95).
#' @param ... Additional arguments passed to \code{survival::coxph} (e.g., \code{id}, \code{ties}, \code{weights}).
#' @return An object of class \code{cbe_cox_multi} containing:
#'   \itemize{
#'     \item \code{model}: The fitted \code{survival::coxph} object.
#'     \item \code{table}: Tidy coefficient table (HR, log-HR, CI, p-value) with explicit
#'       reference rows per factor, grouped by \code{Variable}.
#'     \item \code{glance}: One-row data frame of model goodness-of-fit metrics.
#'     \item \code{zph}: A \code{cbe_cox_check} object (includes the multivariate global test).
#'     \item \code{converged}: Logical; whether the fit converged within the iteration limit.
#'     \item \code{n_iterations}: Number of Newton-Raphson iterations used by the fit.
#'     \item \code{features}: Character vector of feature names used.
#'     \item \code{var_labels}: Named character vector mapping feature names to labels.
#'   }
#' @seealso [cbe_cox_single()], [cbe_cox_check()], [cbe_cox_table()], [plot_cox_forest_multi()]
#' @export
cbe_cox_multi <- function(data, formula = NULL, outcome = "outcome", features = NULL, conf_level = 0.95, ...) {
  if (is.null(formula) && is.null(features)) {
    stop("Must supply either `formula` or `features`.", call. = FALSE)
  }

  if (!is.null(formula)) {
    fmla <- stats::as.formula(formula)
    features <- all.vars(fmla[[3]])
  } else {
    missing_feats <- setdiff(features, names(data))
    if (length(missing_feats) > 0) {
      stop(sprintf("Feature(s) not found in provided data: %s", paste(missing_feats, collapse = ", ")), call. = FALSE)
    }
    fmla_str <- sprintf("%s ~ %s", outcome, paste(features, collapse = " + "))
    fmla <- stats::as.formula(fmla_str)
  }

  var_labels <- stats::setNames(
    vapply(features, function(f) {
      col_vals <- data[[f]]
      if (requireNamespace("labelled", quietly = TRUE)) {
        lbl <- labelled::var_label(col_vals)
        if (is.null(lbl) || is.na(lbl) || !nzchar(lbl)) f else as.character(lbl)
      } else {
        f
      }
    }, character(1)),
    features
  )

  dots <- match.call(expand.dots = FALSE)$...
  base_args <- list(quote(survival::coxph), formula = fmla, data = quote(data))
  if (!"model" %in% names(dots)) base_args$model <- TRUE
  cph_call <- as.call(c(base_args, as.list(dots)))
  fit <- eval(cph_call, environment(), parent.frame())
  fit$call$data <- match.call()$data

  check <- cbe_cox_check(fit)

  td <- broom::tidy(fit, conf.int = TRUE, conf.level = conf_level, exponentiate = TRUE)

  ci_col <- if (!is.null(conf_level) && conf_level != 0.95) {
    sprintf("%d%% CI", round(conf_level * 100))
  } else {
    "95% CI"
  }

  coef_table <- purrr::map_dfr(features, function(feat) {
    col_vals <- data[[feat]]
    var_lbl <- var_labels[[feat]]

    if (is.numeric(col_vals)) {
      term_rows <- td[td$term == feat, , drop = FALSE]
      term_rows |>
        dplyr::transmute(
          Variable  = var_lbl,
          Level     = "1-unit increase",
          Role      = "Covariate",
          HR        = round(estimate, 2),
          `log(HR)` = round(log(estimate), 3),
          !!ci_col  := sprintf("%.2f \u2013 %.2f", conf.low, conf.high),
          p.value   = scales::pvalue(p.value)
        )
    } else {
      f_levels <- levels(as.factor(col_vals))
      ref_level <- f_levels[1]
      expected_terms <- paste0(feat, f_levels[-1])
      term_rows <- td[td$term %in% expected_terms, , drop = FALSE]

      comp_rows <- term_rows |>
        dplyr::mutate(
          clean_level = stringr::str_remove(term, stringr::fixed(feat)),
          Variable    = var_lbl,
          Level       = clean_level,
          Role        = "Comparison",
          HR          = round(estimate, 2),
          `log(HR)`   = round(log(estimate), 3),
          !!ci_col    := sprintf("%.2f \u2013 %.2f", conf.low, conf.high),
          p.value     = scales::pvalue(p.value)
        ) |>
        dplyr::select(Variable, Level, Role, HR, `log(HR)`, dplyr::all_of(ci_col), p.value)

      ref_row <- tibble::tibble(
        Variable  = var_lbl,
        Level     = ref_level,
        Role      = "Reference",
        HR        = 1.00,
        `log(HR)` = 0,
        !!ci_col  := "Reference",
        p.value   = "\u2014"
      )

      dplyr::bind_rows(ref_row, comp_rows)
    }
  })

  glance_tbl <- broom::glance(fit) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Statistic", values_to = "Value") |>
    dplyr::mutate(Value = round(Value, 3))

  n_iter <- fit$iter
  converged <- if (is.null(n_iter)) NA else n_iter < survival::coxph.control()$iter.max

  structure(
    list(
      model        = fit,
      table        = coef_table,
      glance       = glance_tbl,
      zph          = check,
      converged    = converged,
      n_iterations = n_iter,
      features     = features,
      var_labels   = var_labels,
      conf_level   = conf_level
    ),
    class = "cbe_cox_multi"
  )
}

#' Print Method for cbe_cox_multi Object
#'
#' @param x Object of class \code{cbe_cox_multi}
#' @param ... Additional arguments (unused)
#' @export
print.cbe_cox_multi <- function(x, ...) {
  cat("=======================================================\n")
  cat(sprintf("Multivariable Cox PH Model: %s\n", paste(x$features, collapse = ", ")))
  cat("=======================================================\n\n")
  cat(sprintf(
    "Converged: %s (%s iterations)\n\n",
    if (isTRUE(x$converged)) "Yes" else "No",
    if (is.null(x$n_iterations)) "unknown" else x$n_iterations
  ))
  cat("Proportional Hazards Test:\n")
  cat(paste(x$zph$zph_text, collapse = "\n"), "\n\n")
  cat("Coefficients:\n")
  print(x$table)
  invisible(x)
}

utils::globalVariables(c(
  "estimate", "conf.low", "conf.high", "p.value", "term", "clean_level",
  "Variable", "Level", "Role", "HR", "log(HR)", "95% CI"
))
