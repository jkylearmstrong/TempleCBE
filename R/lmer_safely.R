#' Fit a Linear Mixed-Effects Model With Safe Fallback
#'
#' Fits a linear mixed model via [lme4::lmer()], catching non-convergence,
#' singular fit warnings, or missing package conditions and gracefully falling
#' back to a fixed-effects linear model ([stats::lm()]).
#'
#' Replaces inline tryCatch / fallback copies across longitudinal reports.
#'
#' @section What is tried, in order:
#' 1. `lmer(formula)` with the default optimizer.
#' 2. If that stops with an error (for example "Downdated VtV is not positive definite") or gives a
#'    convergence warning, the same model with each of `optimizers` in turn. The first fit without
#'    an error or convergence warning is kept.
#' 3. If no fit of `formula` is usable (every attempt failed, or the fit is singular and
#'    `singular_ok = FALSE`), `fallback_formula` when given: with [lme4::lmer()] (same optimizer
#'    retries) if it has a random-effects term, otherwise with [stats::lm()].
#' 4. Otherwise [stats::lm()] on `formula` with its random-effects terms removed.
#'
#' A fit of `formula` that only has a convergence warning under every optimizer is returned as it is,
#' with `converged = FALSE` and a warning, as before. Optimizer retries are skipped when you pass
#' your own `control` in `...`.
#'
#' @param formula Model formula, typically containing random effects (e.g. `y ~ x + (1 | id)`).
#' @param data Data frame containing variables in `formula`.
#' @param warn_on_fallback Logical. Emit a warning when falling back to `lm`. Default `TRUE`.
#' @param ... Additional arguments passed to [lme4::lmer()] or [stats::lm()].
#' @param fallback_formula Optional simpler formula to try before the fixed-effects fallback, for
#'   example a random intercept in place of random slopes (`y ~ x + (1 | id)`).
#' @param optimizers Optimizers to retry with, names accepted by [lme4::lmerControl()]
#'   (default `c("bobyqa", "Nelder_Mead")`). `NULL` or `character()` for no retries.
#' @param singular_ok If `TRUE`, a singular mixed model (a variance estimated as zero, or a
#'   correlation of plus or minus 1) is kept instead of falling back. Default `FALSE`.
#' @return A list of class `"cbe_lmer_safely"` containing:
#'   \item{model}{The fitted `merMod` or `lm` object}
#'   \item{method}{Character indicating `"lmer"` or `"lm"`}
#'   \item{is_singular}{Logical indicating whether the mixed model of `formula` had a singular fit}
#'   \item{formula}{The formula used for the final fit}
#'   \item{converged}{`TRUE` when the returned model is the mixed model of `formula` itself, fitted
#'     without a convergence warning; `FALSE` after any fallback or with a convergence warning}
#'   \item{used_fallback}{`"none"`, `"fallback_formula"` or `"lm"`}
#'   \item{optimizer}{The optimizer of the returned mixed model (`"default"` for the first attempt),
#'     `NA` for an `lm` fit}
#'   \item{messages}{Error and convergence messages collected on the way (character vector)}
#' @export
#' @examples
#' df <- data.frame(
#'   id = rep(1:5, each = 4),
#'   time = rep(1:4, times = 5),
#'   y = rnorm(20)
#' )
#' fit <- fit_lmer_safely(y ~ time + (1 | id), data = df)
#' fit$method
#' fit$converged
fit_lmer_safely <- function(formula, data, warn_on_fallback = TRUE, ...,
                            fallback_formula = NULL, optimizers = c("bobyqa", "Nelder_Mead"),
                            singular_ok = FALSE) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.null(fallback_formula) && !inherits(fallback_formula, "formula")) {
    stop("`fallback_formula` must be a formula or NULL.", call. = FALSE)
  }
  if (!is.null(optimizers) && !is.character(optimizers)) {
    stop("`optimizers` must be a character vector or NULL.", call. = FALSE)
  }

  has_random <- function(f) grepl("\\|", paste(deparse(f), collapse = " "))
  has_lme4 <- requireNamespace("lme4", quietly = TRUE)
  dots <- list(...)
  if ("control" %in% names(dots)) optimizers <- character()
  messages <- character()
  note <- function(...) messages <<- c(messages, paste0(...))

  # one lmer call: the fit (or NULL), whether it warned about convergence, other warnings re-raised
  attempt <- function(f, optimizer) {
    conv <- FALSE
    args <- c(list(formula = f, data = data), dots)
    if (!identical(optimizer, "default")) args$control <- lme4::lmerControl(optimizer = optimizer)
    fit <- tryCatch(
      withCallingHandlers(
        do.call(lme4::lmer, args),
        warning = function(w) {
          if (grepl("converge|Hessian|eigenvalue|unable to evaluate scaled gradient", conditionMessage(w), ignore.case = TRUE)) {
            conv <<- TRUE
            note("lmer (", optimizer, "): ", conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        },
        message = function(m) {
          if (grepl("singular", conditionMessage(m), ignore.case = TRUE)) invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        note("lmer (", optimizer, ") failed: ", conditionMessage(e))
        NULL
      }
    )
    list(fit = fit, conv_warning = conv, optimizer = optimizer)
  }

  # best available fit of one mixed formula: a clean one if any optimizer gives it, else the first
  # fit that at least ran
  fit_mixed <- function(f) {
    first <- NULL
    for (opt in c("default", optimizers)) {
      a <- attempt(f, opt)
      if (is.null(a$fit)) next
      if (!a$conv_warning) return(a)
      if (is.null(first)) first <- a
    }
    first
  }

  strip_random <- function(f) {
    s <- paste(deparse(f), collapse = " ")
    s <- gsub("\\+\\s*\\([^)]*\\|[^)]*\\)", "", s)
    s <- gsub("\\([^)]*\\|[^)]*\\)", "1", s)
    s <- trimws(s)
    if (endsWith(s, "+")) s <- substr(s, 1, nchar(s) - 1)
    stats::as.formula(s, env = environment(f))
  }

  result <- function(model, method, f, converged, used_fallback, optimizer, is_singular) {
    out <- list(model = model, method = method, is_singular = is_singular, formula = f, converged = converged,
                used_fallback = used_fallback, optimizer = optimizer, messages = messages)
    class(out) <- c("cbe_lmer_safely", "list")
    out
  }

  is_singular <- FALSE
  if (has_lme4 && has_random(formula)) {
    a <- fit_mixed(formula)
    if (!is.null(a)) {
      is_singular <- lme4::isSingular(a$fit)
      if (!is_singular || isTRUE(singular_ok)) {
        if (a$conv_warning && isTRUE(warn_on_fallback)) {
          warning("`lme4::lmer()` did not converge cleanly with any optimizer; the model is returned with ",
                  "`converged = FALSE`.", call. = FALSE)
        }
        return(result(a$fit, "lmer", formula, !a$conv_warning, "none", a$optimizer, is_singular))
      }
      note("lmer: singular fit")
    }
    why <- if (is_singular) "produced a singular fit" else paste0("failed with: ", utils::tail(messages, 1L))
    if (!is.null(fallback_formula) && has_random(fallback_formula)) {
      b <- fit_mixed(fallback_formula)
      if (!is.null(b) && (!lme4::isSingular(b$fit) || isTRUE(singular_ok))) {
        if (isTRUE(warn_on_fallback)) {
          warning("`lme4::lmer()` ", why, "; using `fallback_formula`.", call. = FALSE)
        }
        return(result(b$fit, "lmer", fallback_formula, FALSE, "fallback_formula", b$optimizer, is_singular))
      }
      note("lmer on `fallback_formula` was not usable")
    }
    if (isTRUE(warn_on_fallback)) {
      warning("`lme4::lmer()` ", why, "; falling back to `stats::lm()`.", call. = FALSE)
    }
  }

  fixed_form <- if (!is.null(fallback_formula) && !has_random(fallback_formula)) {
    fallback_formula
  } else if (has_random(formula)) {
    strip_random(formula)
  } else {
    formula
  }
  fit <- stats::lm(fixed_form, data = data, ...)
  used <- if (has_random(formula)) {
    if (identical(fixed_form, fallback_formula)) "fallback_formula" else "lm"
  } else {
    "none"
  }
  result(fit, "lm", fixed_form, FALSE, used, NA_character_, is_singular)
}
