#' Pool Estimates from Several Imputations with Rubin's Rules
#'
#' Combines the same model fitted to each of `m` imputed data sets into one estimate per term:
#' the mean of the estimates, the within-imputation variance, the between-imputation variance and
#' the total variance \eqn{T = \bar{U} + (1 + 1/m) B}, with Rubin's degrees of freedom and a
#' t-based interval and p-value.
#'
#' @section Input:
#' Either a list of `m` data frames (for example `lapply(imputed, function(d) broom::tidy(fit(d)))`)
#' or one stacked data frame with a column naming the imputation. Each needs `term`, `estimate`
#' and `std.error`. Pool on the scale the estimates are approximately normal (log hazard ratio, not
#' hazard ratio); use `exponentiate = TRUE` to report on the ratio scale.
#'
#' Rubin's rules assume proper multiple imputation (draws from a posterior predictive
#' distribution). A single imputation, such as one run of [impute_piecewise_lmm()], gives no
#' between-imputation variance, so impute several times with different seeds first.
#'
#' @param x A list of data frames, or a stacked data frame.
#' @param imputation_col Name of the imputation column when `x` is stacked.
#' @param conf_level Confidence level.
#' @param exponentiate Return `estimate`, `conf.low` and `conf.high` exponentiated.
#' @param df_complete Optional complete-data degrees of freedom (for example `N - k`). When given,
#'   the Barnard-Rubin (1999) small-sample adjustment is used,
#'   \eqn{\nu_{adj} = (1/\nu + 1/\nu_{obs})^{-1}} with
#'   \eqn{\nu_{obs} = \frac{df+1}{df+3} df (1 - \lambda)}, instead of the large-sample
#'   \eqn{\nu}. Terms are pooled over the union of terms across imputations; a term missing in
#'   any imputation is an error.
#' @return A tibble with `term`, `m`, `estimate`, `std.error` (of the pooled estimate),
#'   `within_var`, `between_var`, `total_var`, `df`, `statistic`, `p.value`, `conf.low`,
#'   `conf.high` and `fmi` (fraction of missing information).
#' @export
#' @examples
#' set.seed(1)
#' fits <- lapply(1:5, function(i) {
#'   data.frame(term = c("x", "z"), estimate = c(0.5, -0.2) + rnorm(2, sd = 0.05),
#'              std.error = c(0.1, 0.08))
#' })
#' pool_imputations(fits)
pool_imputations <- function(x, imputation_col = "imputation", conf_level = 0.95, exponentiate = FALSE,
                             df_complete = NULL) {
  if (!is.null(df_complete) &&
      (!is.numeric(df_complete) || length(df_complete) != 1L || is.na(df_complete) || df_complete <= 0)) {
    stop("`df_complete` must be a single positive number.", call. = FALSE)
  }
  if (is.data.frame(x)) {
    if (!imputation_col %in% names(x)) stop("`x` has no column `", imputation_col, "`.", call. = FALSE)
    x <- split(x, x[[imputation_col]])
  }
  if (!is.list(x) || length(x) < 2L) stop("At least two imputations are needed.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }
  for (d in x) {
    if (!is.data.frame(d) || !all(c("term", "estimate", "std.error") %in% names(d))) {
      stop("Each imputation needs columns `term`, `estimate` and `std.error`.", call. = FALSE)
    }
  }
  terms <- unique(unlist(lapply(x, function(d) as.character(d$term))))
  m <- length(x)
  rows <- lapply(terms, function(tm) {
    est <- vapply(x, function(d) d$estimate[as.character(d$term) == tm][1], numeric(1))
    se <- vapply(x, function(d) d$std.error[as.character(d$term) == tm][1], numeric(1))
    if (anyNA(est) || anyNA(se)) {
      stop("Term `", tm, "` is missing or NA in at least one imputation.", call. = FALSE)
    }
    qbar <- mean(est)
    ubar <- mean(se^2)
    b <- stats::var(est)
    tot <- ubar + (1 + 1 / m) * b
    r <- (1 + 1 / m) * b / ubar
    df <- if (b == 0) Inf else (m - 1) * (1 + 1 / r)^2
    if (!is.null(df_complete) && b > 0) {
      lambda <- (1 + 1 / m) * b / tot
      df_obs <- (df_complete + 1) / (df_complete + 3) * df_complete * (1 - lambda)
      df <- 1 / (1 / df + 1 / df_obs)
    }
    se_pool <- sqrt(tot)
    stat <- qbar / se_pool
    crit <- stats::qt(1 - (1 - conf_level) / 2, df)
    tibble::tibble(
      term = tm, m = m, estimate = qbar, std.error = se_pool, within_var = ubar,
      between_var = b, total_var = tot, df = df, statistic = stat,
      p.value = 2 * stats::pt(-abs(stat), df),
      conf.low = qbar - crit * se_pool, conf.high = qbar + crit * se_pool,
      fmi = (r + 2 / (df + 3)) / (r + 1)
    )
  })
  out <- dplyr::bind_rows(rows)
  if (isTRUE(exponentiate)) {
    out$estimate <- exp(out$estimate)
    out$conf.low <- exp(out$conf.low)
    out$conf.high <- exp(out$conf.high)
  }
  out
}
