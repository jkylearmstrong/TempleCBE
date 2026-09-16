#' Univariable Cox Proportional Hazards Screening Engine
#'
#' Fits a univariable Cox proportional hazards model, validates the proportional
#' hazards assumption, formats coefficients with explicit reference levels,
#' generates plain-language clinical interpretations, and gathers model fit metrics.
#'
#' @name cbe_cox_single
NULL

#' Analyze a Single Predictor in a Cox Proportional Hazards Model
#'
#' @param data A data frame containing survival data and the candidate feature.
#' @param outcome Character string naming the Surv object or outcome column in \code{data}
#'   (e.g., \code{"outcome"} or \code{"Surv(survival_time, status)"}).
#' @param feature Character string naming the candidate predictor column in \code{data}.
#' @param conf_level Numeric confidence level (default: 0.95).
#' @return An object of class \code{cbe_cox} containing:
#'   \itemize{
#'     \item \code{model}: The fitted \code{survival::coxph} object.
#'     \item \code{zph}: The \code{survival::cox.zph} proportional hazards test object.
#'     \item \code{zph_table}: Formatted assumption test table.
#'     \item \code{zph_violated}: Logical indicating whether the assumption was violated (p < 0.05).
#'     \item \code{zph_text}: Automated sentence summarizing the assumption test.
#'     \item \code{table}: Clean coefficient table with explicit reference level rows for factors.
#'     \item \code{interpretation}: Plain-language automated interpretation of the hazard ratio(s).
#'     \item \code{glance}: One-row data frame of model goodness-of-fit metrics.
#'     \item \code{feature}: Feature name string.
#'     \item \code{var_label}: Human-readable variable label.
#'     \item \code{is_numeric}: Logical flag indicating whether predictor is continuous/numeric.
#'   }
#' @export
cbe_cox_single <- function(data, outcome = "outcome", feature, conf_level = 0.95) {
  if (!feature %in% names(data)) {
    stop(sprintf("Feature '%s' not found in provided data.", feature))
  }

  col_vals <- data[[feature]]
  var_lbl <- if (requireNamespace("labelled", quietly = TRUE)) {
    lbl <- labelled::var_label(col_vals)
    if (is.null(lbl) || is.na(lbl) || !nzchar(lbl)) feature else as.character(lbl)
  } else {
    feature
  }

  is_num <- is.numeric(col_vals)

  # 1. Fit Cox Model
  fmla_str <- sprintf("%s ~ %s", outcome, feature)
  fmla <- stats::as.formula(fmla_str)
  fit <- survival::coxph(fmla, data = data)

  # 2. Test Proportional Hazards Assumption
  zph_res <- tryCatch(
    survival::cox.zph(fit),
    error = function(e) NULL
  )

  if (!is.null(zph_res)) {
    zph_tab <- as.data.frame(zph_res$table)
    p_zph <- zph_tab[1, "p"]
    zph_violated <- p_zph < 0.05
    zph_text <- sprintf(
      "Test of the proportional hazards assumption yields p = %.3f. Therefore, the proportional hazards assumption is %sviolated.",
      p_zph, if (zph_violated) "" else "not "
    )
  } else {
    zph_tab <- data.frame()
    p_zph <- NA_real_
    zph_violated <- FALSE
    zph_text <- "Proportional hazards assumption could not be calculated."
  }

  # 3. Tidy Model Coefficients
  td <- broom::tidy(fit, conf.int = TRUE, conf.level = conf_level, exponentiate = TRUE)

  if (is_num) {
    coef_table <- td |>
      dplyr::transmute(
        Variable    = var_lbl,
        Level       = "1-unit increase",
        Role        = "Covariate",
        HR          = round(estimate, 2),
        `95% CI`    = sprintf("%.2f \u2013 %.2f", conf.low, conf.high),
        p.value     = scales::pvalue(p.value)
      )

    hr_val <- td$estimate[1]
    ci_low <- td$conf.low[1]
    ci_high <- td$conf.high[1]
    pval <- td$p.value[1]
    pct_change <- round(abs(hr_val - 1) * 100, 1)
    direction <- if (hr_val > 1) "increases" else "decreases"
    sig_text <- if (pval < 0.05) {
      "The p-value indicates that this association is statistically significant."
    } else {
      "The p-value indicates that this association is not statistically significant."
    }

    interp <- glue::glue(
      "The hazard ratio for {var_lbl} is {round(hr_val, 2)} (95% CI {round(ci_low, 2)} \u2013 {round(ci_high, 2)}). ",
      "For each one-unit increase in {var_lbl}, the risk of the event {direction} by {pct_change}%. {sig_text}"
    )
  } else {
    f_levels <- levels(as.factor(col_vals))
    ref_level <- f_levels[1]

    comp_rows <- td |>
      dplyr::mutate(
        clean_level = stringr::str_remove(term, stringr::fixed(feature)),
        Variable    = var_lbl,
        Level       = clean_level,
        Role        = "Comparison",
        HR          = round(estimate, 2),
        `95% CI`    = sprintf("%.2f \u2013 %.2f", conf.low, conf.high),
        p.value     = scales::pvalue(p.value)
      ) |>
      dplyr::select(Variable, Level, Role, HR, `95% CI`, p.value)

    ref_row <- tibble::tibble(
      Variable = var_lbl,
      Level    = ref_level,
      Role     = "Reference",
      HR       = 1.00,
      `95% CI` = "Reference",
      p.value  = "\u2014"
    )

    coef_table <- dplyr::bind_rows(ref_row, comp_rows)

    interp_parts <- td |>
      dplyr::mutate(
        lvl = stringr::str_remove(term, stringr::fixed(feature)),
        pct = round(abs(estimate - 1) * 100, 1),
        dir = dplyr::if_else(estimate > 1, "increases", "decreases"),
        sig = dplyr::if_else(p.value < 0.05, "statistically significant", "not statistically significant"),
        txt = glue::glue(
          "The hazard ratio for '{var_lbl} = {lvl}' vs. reference '{ref_level}' is {round(estimate, 2)} (95% CI {round(conf.low, 2)} \u2013 {round(conf.high, 2)}), ",
          "indicating that the risk of the event {dir} by {pct}% ({sig}; p = {scales::pvalue(p.value)})."
        )
      ) |>
      dplyr::pull(txt)

    interp <- paste(interp_parts, collapse = " ")
  }

  # 4. Glance Summary Table
  glance_tbl <- broom::glance(fit) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Statistic", values_to = "Value") |>
    dplyr::mutate(Value = round(Value, 3))

  structure(
    list(
      model        = fit,
      zph          = zph_res,
      zph_table    = zph_tab,
      zph_violated = zph_violated,
      zph_text     = zph_text,
      table        = coef_table,
      interpretation = interp,
      glance       = glance_tbl,
      feature      = feature,
      var_label    = var_lbl,
      is_numeric   = is_num
    ),
    class = "cbe_cox"
  )
}

#' Print Method for cbe_cox Object
#'
#' @param x Object of class \code{cbe_cox}
#' @param ... Additional arguments (unused)
#' @export
print.cbe_cox <- function(x, ...) {
  cat("=======================================================\n")
  cat(sprintf("Cox PH Model for: %s (%s)\n", x$var_label, x$feature))
  cat("=======================================================\n\n")
  cat("Proportional Hazards Test:\n")
  cat(x$zph_text, "\n\n")
  cat("Coefficients:\n")
  print(x$table)
  cat("\nClinical Interpretation:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}

utils::globalVariables(c(
  "estimate", "conf.low", "conf.high", "p.value", "term",
  "clean_level", "Variable", "Level", "Role", "HR", "95% CI",
  "txt", "Value"
))

