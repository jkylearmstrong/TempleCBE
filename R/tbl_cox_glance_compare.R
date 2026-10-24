#' Compare the Fit of Several Cox Models in One Table
#'
#' One row per model with the number of subjects and events, log-likelihood, AIC, BIC and
#' concordance (with its standard error), taken from [broom::glance()]. Use it to line up
#' candidate survival specifications.
#'
#' @section Comparability:
#' AIC and BIC are only comparable between models fitted to the **same rows**. The function warns when
#' the models were fitted to different numbers of subjects or events (usually because a predictor had
#' missing values in one model and not in another).
#'
#' @param models A list of [survival::coxph()] fits, or of [cox_single()] / [cox_multi()] results (their
#'   `model` is used). A single model is accepted.
#' @param names Optional model names (one per model). By default the names of `models`, otherwise
#'   `"Model 1"`, `"Model 2"`, ...
#' @param sort_by `"AIC"` (default) or `"BIC"` (smallest first), `"concordance"` (largest first) or
#'   `NULL` to keep the order of `models`.
#' @param digits Decimals for the log-likelihood, AIC, BIC and concordance.
#' @return A tibble of class `cbe_cox_glance` with text columns `Model`, `N`, `Events`, `Log-Likelihood`,
#'   `AIC`, `BIC` and `Concordance (SE)`, and the attribute `"numeric"` holding the unformatted values
#'   (`model`, `n`, `events`, `loglik`, `AIC`, `BIC`, `concordance`, `se_concordance`).
#' @seealso [cox_table()], [cox_single()], [cox_multi()]
#' @export
#' @examples
#' lung <- survival::lung
#' m1 <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
#' m2 <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
#' tbl_cox_glance_compare(list(age = m1, `age + sex` = m2))
tbl_cox_glance_compare <- function(models,
                                   names = NULL,
                                   sort_by = c("AIC", "BIC", "concordance"),
                                   digits = 2) {
  if (is.null(sort_by)) {
    sort_by <- NULL
  } else {
    sort_by <- match.arg(sort_by)
  }
  if (inherits(models, "coxph") || inherits(models, c("cbe_cox", "cbe_cox_multi"))) models <- list(models)
  if (!is.list(models) || length(models) == 0L) {
    stop("`models` must be a model or a non-empty list of Cox models.", call. = FALSE)
  }
  fits <- lapply(models, function(m) {
    if (inherits(m, "coxph")) return(m)
    if (is.list(m) && inherits(m$model, "coxph")) return(m$model)
    stop("Every element of `models` must be a coxph fit or a cox_single()/cox_multi() result.", call. = FALSE)
  })
  nm <- if (!is.null(names)) {
    if (!is.character(names) || length(names) != length(fits) || anyNA(names)) {
      stop("`names` must be a character vector with one name per model.", call. = FALSE)
    }
    names
  } else if (!is.null(base::names(models)) && all(nzchar(base::names(models)))) {
    base::names(models)
  } else {
    paste("Model", seq_along(fits))
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a non-negative number.", call. = FALSE)
  }

  g <- lapply(unname(fits), function(f) {
    gl <- broom::glance(f)
    list(
      n = as.integer(gl$n), events = as.integer(gl$nevent), loglik = as.numeric(gl$logLik),
      AIC = as.numeric(gl$AIC), BIC = as.numeric(gl$BIC),
      concordance = as.numeric(gl$concordance), se = as.numeric(gl$std.error.concordance)
    ) |> (\(r) {
      # a model without covariates has no concordance; broom reports 0
      if (length(stats::coef(f)) == 0L) {
        r$concordance <- NA_real_
        r$se <- NA_real_
      }
      r
    })()
  })
  num <- tibble::tibble(
    model = nm,
    n = vapply(g, `[[`, 1L, "n"), events = vapply(g, `[[`, 1L, "events"),
    loglik = vapply(g, `[[`, 1, "loglik"), AIC = vapply(g, `[[`, 1, "AIC"), BIC = vapply(g, `[[`, 1, "BIC"),
    concordance = vapply(g, `[[`, 1, "concordance"), se_concordance = vapply(g, `[[`, 1, "se")
  )
  if (length(unique(num$n)) > 1L || length(unique(num$events)) > 1L) {
    warning("The models were fitted to different numbers of subjects or events, so their AIC and BIC are not comparable.",
            call. = FALSE)
  }
  if (!is.null(sort_by)) {
    ord <- switch(sort_by,
      AIC = order(num$AIC), BIC = order(num$BIC), concordance = order(-num$concordance)
    )
    num <- num[ord, , drop = FALSE]
  }

  f <- function(x) formatC(x, format = "f", digits = digits)
  out <- tibble::tibble(
    Model = num$model,
    N = as.character(num$n),
    Events = as.character(num$events),
    `Log-Likelihood` = f(num$loglik),
    AIC = f(num$AIC),
    BIC = f(num$BIC),
    `Concordance (SE)` = ifelse(is.na(num$concordance), "",
      ifelse(is.na(num$se_concordance), f(num$concordance),
             sprintf("%s (%s)", f(num$concordance), f(num$se_concordance))))
  )
  attr(out, "numeric") <- num
  class(out) <- c("cbe_cox_glance", class(out))
  out
}
