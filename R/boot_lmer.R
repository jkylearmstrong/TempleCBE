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
#' model contributes only the fixed effects it has. A replicate that cannot be
#' fitted at all is counted in `n_failed` and contributes nothing. Warnings and
#' messages from the fits (singular fits, convergence notes) are not shown.
#'
#' The fences are applied to the bootstrap estimates of each term separately: an
#' estimate is kept when it lies strictly between the lower and upper fence (the
#' inner fences at 1.5 x IQR, the outer at 3 x IQR). `std.error` is the standard
#' deviation of the kept estimates divided by the square root of their number, and
#' the p-value uses it as the tolerance `epsilon` around the null value, which is
#' what the report templates did. Percentile limits use all kept estimates.
#'
#' @param formula A mixed-model formula with a random-effect term, for example
#'   `y ~ x1 + x2 + (1 | id)`.
#' @param data A data frame.
#' @param group Name of the column that identifies the groups to resample (usually
#'   the grouping factor of the random effect).
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
#'   `kept`, `fallback`), `n_boot`, `n_failed`, `n_fallback`, `fence`, `conf_level`.
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

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else suppressWarnings(rm(".Random.seed", envir = globalenv()))
    }, add = TRUE)
    set.seed(seed)
  }

  rows_of <- split(seq_len(nrow(data)), data[[group]])
  n_groups <- length(rows_of)
  fit_one <- function(f, df) {
    tryCatch(suppressMessages(suppressWarnings(lme4::lmer(f, data = df, ...))), error = function(e) NULL)
  }

  draws <- vector("list", n_boot)
  n_failed <- 0L
  n_fallback <- 0L
  for (b in seq_len(n_boot)) {
    drawn <- sample.int(n_groups, n_groups, replace = TRUE)
    df <- data[unlist(rows_of[drawn], use.names = FALSE), , drop = FALSE]
    for (v in normalize) df[[v]] <- as.numeric(min_max_norm(df[[v]]))
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
    est <- lme4::fixef(fit)
    if (length(est)) {
      draws[[b]] <- tibble::tibble(boot = b, term = names(est), estimate = unname(est), fallback = used_fallback)
    }
  }
  draws <- dplyr::bind_rows(draws)
  if (nrow(draws) == 0L) {
    draws <- tibble::tibble(boot = integer(), term = character(), estimate = double(), fallback = logical())
  }
  if (n_failed == n_boot) {
    warning("No bootstrap replicate could be fitted; the result is empty.", call. = FALSE)
  }

  draws$kept <- TRUE
  if (fence != "none" && nrow(draws) > 0L) {
    for (tm in unique(draws$term)) {
      i <- which(draws$term == tm)
      f <- calculate_fences(draws$estimate[i])
      lo <- f[[paste0("lower_", fence, "_fence")]]
      hi <- f[[paste0("upper_", fence, "_fence")]]
      if (!is.na(lo)) draws$kept[i] <- lo < draws$estimate[i] & draws$estimate[i] < hi
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
#' @return A number between 0 and 1; `NA` when there are no estimates.
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
  2 * min(lower, upper)
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
