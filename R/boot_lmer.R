#' Cluster Bootstrap of a Linear Mixed-Effects Model
#'
#' Refits a linear mixed model ([lme4::lmer()]) on bootstrap resamples of whole
#' groups (for example subjects), drops outlying bootstrap estimates of each fixed
#' effect with Tukey's fences ([calculate_fences()]) and summarises what is left:
#' mean, standard error, median, percentile interval and a two-tailed bootstrap
#' p-value ([boot_p_value()]). It is the "resample, fit with a fallback, tidy, drop
#' outlying coefficients, summarise" block that report templates otherwise repeat.
#'
#' @details
#' Each replicate draws as many groups as there are, with replacement, and uses all
#' rows of every drawn group. A group drawn twice keeps its identifier, so it is
#' still one level of the random effect (as with `rsample::group_bootstraps()`).
#'
#' `normalize` names numeric columns that are rescaled to \[0, 1\] by their minimum
#' and maximum \emph{within each resample} ([min_max_norm()]) before the model is
#' fitted, so the estimates are per full range of the predictor.
#'
#' A replicate whose model cannot be fitted is refitted with `fallback_formula`, if
#' one is given (for example the intercept-only `y ~ -1 + (1 | id)`); a fallback
#' model contributes only the fixed effects it has, and those estimates come from
#' another model than the rest (they are flagged `fallback` in `draws`). A replicate
#' that cannot be fitted at all is counted in `n_failed` and contributes nothing.
#' Warnings and messages from the fits are not shown; the number of singular and of
#' non-converged fits is returned in `n_singular` and `n_nonconverged`.
#'
#' \strong{Rank-deficient resamples.} When a resample lacks a level of a factor (or a
#' predictor is constant in it), `lme4` drops the columns it cannot estimate, and the
#' remaining contrasts then mean something else (with level A absent, `fC` estimates
#' C - B and not C - A). The columns of the model fitted to the whole data are the
#' reference: a replicate whose columns differ is not comparable, contributes to no
#' term and is counted in `n_incomparable`. A predictor named in `normalize` that is
#' constant in a resample is mapped to 0 and dropped by `lme4` in the same way.
#'
#' The fences are applied to the bootstrap estimates of each term separately: an
#' estimate is kept when it lies strictly between the lower and upper fence (the
#' inner fences at 1.5 x IQR, the outer at 3 x IQR); when the interquartile range is 0
#' the fences coincide with the bulk value and the bulk is kept, so that only the
#' values off it are dropped. `fence = "none"` keeps every estimate.
#'
#' \strong{`std.error`} is the standard deviation of the kept estimates divided by the
#' square root of their number. That is the Monte Carlo error of the bootstrap mean,
#' not the standard error of the estimate (which is `sd`), and it shrinks as
#' `n_boot` grows. It is kept because the report templates used it, and because the
#' p-value uses it as the tolerance `epsilon` around the null value. Percentile limits
#' use all kept estimates. Terms are returned in model order. A p-value of 0 means no
#' kept estimate lay on the other side of the null; it is not exact (see [boot_p_value()]).
#'
#' A group drawn twice keeps its identifier and so counts as one level of the random
#' effect, which slightly inflates the random-effect variance; fixed effects are not
#' affected in practice.
#'
#' @param formula A mixed-model formula with a random-effect term, for example
#'   `y ~ x1 + x2 + (1 | id)`.
#' @param data A data frame.
#' @param group Name of the column that identifies the groups to resample (the
#'   grouping factor of the random effect; a warning is given when it is not one of
#'   the formula's grouping factors). Missing values are an error; unused factor levels
#'   are ignored.
#' @param n_boot Number of bootstrap replicates (default 1000).
#' @param normalize `NULL` or the names of numeric columns to rescale to
#'   \[0, 1\] within each resample.
#' @param fence `"outer"` (default), `"inner"` or `"none"`; see Details.
#' @param fallback_formula `NULL` or a formula used when the model fails.
#' @param conf_level Confidence level of the percentile interval (default 0.95).
#' @param include_intercept If `TRUE`, the intercept is summarised too (default
#'   `FALSE`).
#' @param seed Optional integer. With it the result is reproducible, and the random
#'   number generator state of the session is restored afterwards.
#' @param ... Passed to [lme4::lmer()].
#' @return A tibble of class `boot_lmer`, one row per fixed effect: `term`, `n` (kept
#'   estimates), `mean`, `sd`, `std.error`, `median`, `lower`, `upper`, `p.value`.
#'   Attributes: `draws` (every bootstrap estimate: `boot`, `term`, `estimate`,
#'   `fallback`, `comparable`, `kept`), `n_boot`, `n_failed`, `n_fallback`,
#'   `n_incomparable`, `n_singular`, `n_nonconverged`, `fence`, `conf_level`.
#' @seealso [fit_lmer_safely()], [calculate_fences()], [boot_p_value()]
#' @export
#' @examples
#' \dontrun{
#' boot_lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy, group = "Subject",
#'           n_boot = 200, normalize = "Days", seed = 1)
#' }
boot_lmer <- function(formula, data, group, n_boot = 1000L, normalize = NULL,
                      fence = c("outer", "inner", "none"), fallback_formula = NULL,
                      conf_level = 0.95, include_intercept = FALSE, seed = NULL, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for boot_lmer(). Please install it.", call. = FALSE)
  }
  fence <- match.arg(fence)
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
  if (!is.null(fallback_formula) && !inherits(fallback_formula, "formula")) {
    stop("`fallback_formula` must be NULL or a formula.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) == 0L) stop("`data` must be a non-empty data frame.", call. = FALSE)
  if (!grepl("\\|", paste(deparse(formula), collapse = ""))) {
    stop("`formula` needs a random-effect term such as (1 | id); use stats::lm() without one.", call. = FALSE)
  }
  if (!is.character(group) || length(group) != 1L || is.na(group) || !group %in% names(data)) {
    stop("`group` must be the name of a column of `data`.", call. = FALSE)
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) || n_boot < 1) {
    stop("`n_boot` must be a single number of at least 1.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(normalize)) {
    bad <- normalize[!normalize %in% names(data) | !vapply(normalize, function(v) v %in% names(data) && is.numeric(data[[v]]), logical(1))]
    if (length(bad)) stop("`normalize` must name numeric columns of `data`: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (anyNA(data[[group]])) stop("`group` has missing values; remove those rows first.", call. = FALSE)
  # grouping factors: the variables after the "|" of each random-effect term
  ftxt <- paste(deparse(formula), collapse = "")
  bars <- regmatches(ftxt, gregexpr("[|]+[^)]+[)]", ftxt))[[1]]
  grouping <- unique(unlist(lapply(sub("[)]$", "", sub("^[|]+", "", bars)), function(b) all.vars(stats::as.formula(paste("~", b))))))
  if (!group %in% grouping) {
    warning("`group` ('", group, "') is not a grouping factor of the random effects in `formula` (",
            paste(grouping, collapse = ", "), "); resampling by it may not mimic the clustering.", call. = FALSE)
  }

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else suppressWarnings(rm(".Random.seed", envir = globalenv()))
    }, add = TRUE)
    set.seed(seed)
  }

  rows_of <- split(seq_len(nrow(data)), droplevels(factor(data[[group]])))
  n_groups <- length(rows_of)
  reference <- tryCatch(
    names(lme4::fixef(suppressMessages(suppressWarnings(lme4::lmer(formula, data = data, ...))))),
    error = function(e) NULL
  )
  fit_one <- function(f, df) {
    tryCatch(suppressMessages(suppressWarnings(lme4::lmer(f, data = df, ...))), error = function(e) NULL)
  }

  draws <- vector("list", n_boot)
  n_failed <- 0L
  n_fallback <- 0L
  n_incomparable <- 0L
  n_singular <- 0L
  n_nonconverged <- 0L
  for (b in seq_len(n_boot)) {
    df <- .boot_resample(data, rows_of, normalize)
    fit <- fit_one(formula, df)
    used_fallback <- FALSE
    if (is.null(fit) && !is.null(fallback_formula)) {
      fit <- fit_one(fallback_formula, df)
      used_fallback <- !is.null(fit)
    }
    if (is.null(fit)) {
      n_failed <- n_failed + 1L
      next
    }
    if (used_fallback) n_fallback <- n_fallback + 1L
    if (isTRUE(tryCatch(lme4::isSingular(fit), error = function(e) FALSE))) n_singular <- n_singular + 1L
    if (length(fit@optinfo$conv$lme4$messages) > 0L) n_nonconverged <- n_nonconverged + 1L
    est <- lme4::fixef(fit)
    # the columns lme4 kept must be those of the model fitted to the whole data
    comparable <- used_fallback || is.null(reference) || identical(names(est), reference)
    if (!comparable) n_incomparable <- n_incomparable + 1L
    if (length(est)) {
      draws[[b]] <- tibble::tibble(
        boot = b, term = names(est), estimate = unname(est), fallback = used_fallback, comparable = comparable
      )
    }
  }
  draws <- dplyr::bind_rows(draws)
  if (nrow(draws) == 0L) {
    draws <- tibble::tibble(boot = integer(), term = character(), estimate = double(), fallback = logical(), comparable = logical())
  }
  if (n_failed == n_boot) {
    warning("No bootstrap replicate could be fitted; the result is empty.", call. = FALSE)
  } else if (n_failed > 0.1 * n_boot) {
    warning(n_failed, " of ", n_boot, " bootstrap replicates could not be fitted (too few groups?).", call. = FALSE)
  }

  draws$kept <- draws$comparable
  if (fence != "none" && nrow(draws) > 0L) {
    for (tm in unique(draws$term)) {
      i <- which(draws$term == tm & draws$comparable)
      if (length(i) == 0L) next
      draws$kept[i] <- .boot_fence_keep(draws$estimate[i], fence)
    }
  }

  terms <- unique(draws$term)
  if (!include_intercept) terms <- setdiff(terms, "(Intercept)")
  alpha <- (1 - conf_level) / 2
  rows <- lapply(terms, function(tm) {
    x <- draws$estimate[draws$term == tm & draws$kept]
    n <- length(x)
    se <- if (n > 1L) stats::sd(x) / sqrt(n) else NA_real_
    tibble::tibble(
      term = tm, n = n,
      mean = if (n) mean(x) else NA_real_,
      sd = if (n > 1L) stats::sd(x) else NA_real_,
      std.error = se,
      median = if (n) stats::median(x) else NA_real_,
      lower = if (n) unname(stats::quantile(x, alpha)) else NA_real_,
      upper = if (n) unname(stats::quantile(x, 1 - alpha)) else NA_real_,
      p.value = if (n > 1L) boot_p_value(x, 0, epsilon = se) else NA_real_
    )
  })
  out <- if (length(rows)) {
    dplyr::bind_rows(rows)
  } else {
    tibble::tibble(term = character(), n = integer(), mean = double(), sd = double(), std.error = double(),
                   median = double(), lower = double(), upper = double(), p.value = double())
  }
  structure(
    out,
    class = c("boot_lmer", class(out)),
    draws = draws, n_boot = n_boot, n_failed = n_failed, n_fallback = n_fallback,
    n_incomparable = n_incomparable, n_singular = n_singular, n_nonconverged = n_nonconverged,
    fence = fence, conf_level = conf_level
  )
}

#' Two-Tailed Bootstrap P-Value
#'
#' The share of bootstrap estimates on the far side of a null value, doubled: with
#' `lower` the proportion of estimates at or below `null_value - epsilon` and
#' `upper` the proportion at or above `null_value + epsilon`, the p-value is
#' `2 * min(lower, upper)`. Estimates within `epsilon` of the null value count on
#' neither side. Missing estimates are ignored.
#'
#' @param estimates Numeric vector of bootstrap estimates.
#' @param null_value The value under the null hypothesis (default 0).
#' @param epsilon Tolerance around the null value (default `1e-5`).
#' @return A number between 0 and 1; `NA` when there are no estimates. Estimates that equal the
#'   null value exactly count on both sides when `epsilon` is 0, and the result is capped at 1.
#'   A p-value of 0 means no estimate lies on the other side of the null: it says the p-value
#'   is below about 2 / (n + 1), not that it is zero.
#' @export
#' @examples
#' boot_p_value(c(0.5, 0.7, 0.9, 1.1, -0.2))
boot_p_value <- function(estimates, null_value = 0, epsilon = 1e-5) {
  if (!is.numeric(estimates)) stop("`estimates` must be numeric.", call. = FALSE)
  if (!is.numeric(null_value) || length(null_value) != 1L || is.na(null_value)) stop("`null_value` must be a single number.", call. = FALSE)
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon < 0) stop("`epsilon` must be a single non-negative number.", call. = FALSE)
  estimates <- estimates[!is.na(estimates)]
  n <- length(estimates)
  if (n == 0L) return(NA_real_)
  lower <- sum(estimates <= (null_value - epsilon)) / n
  upper <- sum(estimates >= (null_value + epsilon)) / n
  min(1, 2 * min(lower, upper))
}

#' @export
print.boot_lmer <- function(x, ...) {
  cat(sprintf(
    "Cluster bootstrap of a mixed model: %d replicate(s), %d failed, %d refitted with the fallback; '%s' fence kept %d of %d estimate(s).\n",
    attr(x, "n_boot"), attr(x, "n_failed"), attr(x, "n_fallback"), attr(x, "fence"),
    sum(attr(x, "draws")$kept), nrow(attr(x, "draws"))
  ))
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

# One bootstrap resample: whole groups drawn with replacement (a group keeps its identifier),
# then the named numeric columns rescaled to [0, 1] within the resample
.boot_resample <- function(data, rows_of, normalize = NULL) {
  drawn <- sample.int(length(rows_of), length(rows_of), replace = TRUE)
  df <- data[unlist(rows_of[drawn], use.names = FALSE), , drop = FALSE]
  for (v in normalize) df[[v]] <- as.numeric(min_max_norm(df[[v]]))
  df
}
# Which estimates lie strictly inside the fences; with no spread (IQR 0) the fences sit on the
# bulk, which is kept, and what is off it is dropped
.boot_fence_keep <- function(estimate, fence = "outer") {
  if (identical(fence, "none")) return(rep(TRUE, length(estimate)))
  f <- calculate_fences(estimate)
  lo <- f[[paste0("lower_", fence, "_fence")]]
  hi <- f[[paste0("upper_", fence, "_fence")]]
  if (is.na(lo)) return(rep(TRUE, length(estimate)))
  if (lo == hi) return(estimate == lo)
  lo < estimate & estimate < hi
}
