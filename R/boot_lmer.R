#' Bootstrap the Fixed Effects of a Linear Mixed-Effects Model
#'
#' Fits a linear mixed model with [lme4::lmer()] and returns the fixed effects
#' with parametric-bootstrap standard errors and percentile confidence
#' intervals ([lme4::bootMer()]). Optionally removes outliers of the response
#' first, by Tukey's fences ([calculate_fences()]), and says how many rows that
#' removed. It replaces the long copied blocks of "filter outliers, fit, bootstrap,
#' tidy" in report templates.
#'
#' @details
#' The outlier filter looks at the response only, on the whole data set (not
#' within a group), and keeps rows whose response is missing (the model drops
#' them as usual). `fence = "inner"` removes values beyond 1.5 x IQR from the
#' quartiles, `"outer"` beyond 3 x IQR, `"none"` removes nothing.
#'
#' Each bootstrap replicate simulates a new response from the fitted model
#' (new random effects are not drawn: `use.u = FALSE`, so the clusters of the
#' data are the clusters of the replicates) and refits it. A replicate that fails
#' to fit contributes nothing and is counted in `n_failed`; the interval and
#' standard error use the replicates that fitted. With fewer than 2 fitted
#' replicates the interval and standard error are `NA`.
#'
#' @param formula A mixed-model formula with at least one random-effect term,
#'   for example `y ~ time + (1 | id)`.
#' @param data A data frame.
#' @param n_boot Number of bootstrap replicates (default 1000).
#' @param fence `"none"` (default), `"inner"` or `"outer"`; see Details.
#' @param conf_level Confidence level of the percentile interval (default 0.95).
#' @param seed Optional integer; the bootstrap is reproducible with it, and the
#'   random number generator state of the session is restored afterwards.
#' @param ... Passed to [lme4::lmer()] (for example `REML = FALSE`).
#' @return A tibble of class `boot_lmer` with one row per fixed effect: `term`,
#'   `estimate` (from the fit), `std.error` (standard deviation of the bootstrap
#'   estimates), `conf.low`, `conf.high`, `n_boot` (replicates that fitted).
#'   Attributes: `fit` (the `merMod`), `n_removed` (rows dropped by the fence),
#'   `n_failed` (replicates that failed), `fence`, `conf_level`.
#' @seealso [fit_lmer_safely()], [calculate_fences()]
#' @export
#' @examples
#' \dontrun{
#' sleep <- lme4::sleepstudy
#' boot_lmer(Reaction ~ Days + (1 | Subject), sleep, n_boot = 200, seed = 1)
#' }
boot_lmer <- function(formula, data, n_boot = 1000L, fence = c("none", "inner", "outer"),
                      conf_level = 0.95, seed = NULL, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for boot_lmer(). Please install it.", call. = FALSE)
  }
  fence <- match.arg(fence)
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
  if (!is.data.frame(data) || nrow(data) == 0L) stop("`data` must be a non-empty data frame.", call. = FALSE)
  if (!grepl("\\|", paste(deparse(formula), collapse = ""))) {
    stop("`formula` needs a random-effect term such as (1 | id); use stats::lm() without one.", call. = FALSE)
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) || n_boot < 1) {
    stop("`n_boot` must be a single number of at least 1.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)

  n_removed <- 0L
  if (fence != "none") {
    response <- all.vars(formula[[2L]])
    if (length(response) != 1L || !response %in% names(data) || !is.numeric(data[[response]])) {
      stop("The outlier fence needs a single numeric response variable in `data`.", call. = FALSE)
    }
    f <- calculate_fences(data[[response]])
    lo <- f[[paste0("lower_", fence, "_fence")]]
    hi <- f[[paste0("upper_", fence, "_fence")]]
    y <- data[[response]]
    drop <- !is.na(y) & !is.na(lo) & (y < lo | y > hi)
    n_removed <- sum(drop)
    data <- data[!drop, , drop = FALSE]
  }

  fit <- lme4::lmer(formula, data = data, ...)

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else suppressWarnings(rm(".Random.seed", envir = globalenv()))
    }, add = TRUE)
    set.seed(seed)
  }
  boot <- suppressWarnings(lme4::bootMer(fit, FUN = lme4::fixef, nsim = n_boot, use.u = FALSE, type = "parametric"))

  est <- lme4::fixef(fit)
  draws <- boot$t
  colnames(draws) <- names(est)
  fitted_ok <- rowSums(is.na(draws)) == 0L
  draws <- draws[fitted_ok, , drop = FALSE]
  n_ok <- nrow(draws)
  alpha <- (1 - conf_level) / 2
  summarise_term <- function(j) {
    if (n_ok < 2L) return(c(NA_real_, NA_real_, NA_real_))
    c(stats::sd(draws[, j]), unname(stats::quantile(draws[, j], alpha)), unname(stats::quantile(draws[, j], 1 - alpha)))
  }
  stats_mat <- vapply(seq_along(est), summarise_term, numeric(3))
  out <- tibble::tibble(
    term = names(est),
    estimate = unname(est),
    std.error = stats_mat[1, ],
    conf.low = stats_mat[2, ],
    conf.high = stats_mat[3, ],
    n_boot = n_ok
  )
  structure(
    out,
    class = c("boot_lmer", class(out)),
    fit = fit, n_removed = as.integer(n_removed), n_failed = as.integer(n_boot - n_ok),
    fence = fence, conf_level = conf_level
  )
}

#' @export
print.boot_lmer <- function(x, ...) {
  cat(sprintf(
    "Bootstrap of a mixed model: %d fixed effect(s), %d replicate(s) fitted (%d failed), %d row(s) removed by the '%s' fence.\n",
    nrow(x), if (nrow(x)) x$n_boot[1] else 0L, attr(x, "n_failed"), attr(x, "n_removed"), attr(x, "fence")
  ))
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
