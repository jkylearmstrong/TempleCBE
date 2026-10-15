#' Fit a Linear Mixed-Effects Model With Safe Fallback
#'
#' Fits a linear mixed model via [lme4::lmer()], catching non-convergence,
#' singular fit warnings, or missing package conditions and gracefully falling
#' back to a fixed-effects linear model ([stats::lm()]).
#'
#' Replaces inline tryCatch / fallback copies across longitudinal reports.
#'
#' @param formula Model formula, typically containing random effects (e.g. `y ~ x + (1 | id)`).
#' @param data Data frame containing variables in `formula`.
#' @param warn_on_fallback Logical. Emit a warning when falling back to `lm`. Default `TRUE`.
#' @param ... Additional arguments passed to [lme4::lmer()] or [stats::lm()].
#' @return A list of class `"cbe_lmer_safely"` containing:
#'   \item{model}{The fitted `merMod` or `lm` object}
#'   \item{method}{Character indicating `"lmer"` or `"lm"`}
#'   \item{is_singular}{Logical indicating whether the mixed model had a singular fit}
#'   \item{formula}{The formula used for the final fit}
#' @export
#' @examples
#' df <- data.frame(
#'   id = rep(1:5, each = 4),
#'   time = rep(1:4, times = 5),
#'   y = rnorm(20)
#' )
#' fit <- fit_lmer_safely(y ~ time + (1 | id), data = df)
#' fit$method
fit_lmer_safely <- function(formula, data, warn_on_fallback = TRUE, ...) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  form_char <- deparse(formula)
  has_random <- grepl("\\|", form_char)

  fit <- NULL
  is_singular <- FALSE
  has_lme4 <- requireNamespace("lme4", quietly = TRUE)

  if (has_lme4 && has_random) {
    fit <- tryCatch({
      m <- lme4::lmer(formula, data = data, ...)
      if (lme4::isSingular(m)) {
        is_singular <- TRUE
        if (isTRUE(warn_on_fallback)) {
          warning("`lme4::lmer()` produced a singular fit; falling back to `stats::lm()`.", call. = FALSE)
        }
        NULL
      } else {
        m
      }
    }, error = function(e) {
      if (isTRUE(warn_on_fallback)) {
        warning("`lme4::lmer()` failed with: ", conditionMessage(e), "; falling back to `stats::lm()`.", call. = FALSE)
      }
      NULL
    })
  }

  if (is.null(fit)) {
    # Strip random effect term: remove `+ ( ... | ... )` or `( ... | ... )`
    fixed_form_str <- gsub("\\+\\s*\\([^)]*\\|[^)]*\\)", "", form_char)
    fixed_form_str <- gsub("\\([^)]*\\|[^)]*\\)", "1", fixed_form_str)
    fixed_form_str <- trimws(fixed_form_str)
    if (endsWith(fixed_form_str, "+")) {
      fixed_form_str <- substr(fixed_form_str, 1, nchar(fixed_form_str) - 1)
    }
    fixed_form <- stats::as.formula(fixed_form_str)

    fit <- stats::lm(fixed_form, data = data, ...)
    method <- "lm"
    final_form <- fixed_form
  } else {
    method <- "lmer"
    final_form <- formula
  }

  out <- list(
    model = fit,
    method = method,
    is_singular = is_singular,
    formula = final_form
  )
  class(out) <- c("cbe_lmer_safely", "list")
  out
}
