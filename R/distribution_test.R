#' P-value Significance Stars
#'
#' @param p_value A numeric vector of p-values.
#' @return A character vector of significance stars.
#' @export
#' @examples
#' significance_stars(c(0.0001, 0.02, 0.2, 0.8))
significance_stars <- function(p_value) {
  dplyr::case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.1   ~ ".",
    TRUE            ~ ""
  )
}

#' Is a Vector Composed of Integer-Valued Numbers
#'
#' @param col A numeric vector.
#' @return A single logical.
#' @export
#' @examples
#' is.int(sample(-100:100, size = 500, replace = TRUE))
#' is.int(runif(500))
is.int <- function(col) {
  temp_col <- stats::na.omit(col)
  all(temp_col == floor(temp_col))
}

#' Test Whether a Vector Looks Normally Distributed
#'
#' Runs two normality tests on the finite values of `col`:
#'
#' * **Shapiro-Wilk** ([stats::shapiro.test()]), for 3 to 5000 values. It is
#'   left out above 5000 rather than run on a random subsample, so results
#'   never depend on the random number generator.
#' * **Lilliefors** ([nortest::lillie.test()]), for 5 or more values: the
#'   Kolmogorov-Smirnov test corrected for estimating the mean and standard
#'   deviation from the same data. A plain KS test against
#'   `pnorm(mean(x), sd(x))` gives p-values that are far too large.
#'
#' Both tests are deterministic. In large samples they reject normality for
#' departures too small to matter, so read them alongside a plot such as
#' [distribution_plot()] or a normal Q-Q plot.
#'
#' @param col A numeric vector. `NA`, `NaN`, and infinite values are dropped.
#' @param alpha Significance level for `distribution.test`.
#' @return A tibble with one row per test run: `statistic`, `p.value`,
#'   `method`, `distribution.test` (`TRUE` when `p.value >= alpha`, i.e.
#'   normality is not rejected), `p_value_sig`, and `distribution`. It has no
#'   rows when neither test applies (fewer than 3 values, or all values equal).
#' @references Lilliefors HW (1967). On the Kolmogorov-Smirnov test for
#'   normality with mean and variance unknown. *Journal of the American
#'   Statistical Association*, 62(318), 399-402.
#' @export
#' @examples
#' set.seed(1)
#' is_normal(rnorm(1000, mean = 5, sd = 3))
#' is_normal(runif(1000, min = 2, max = 4))
is_normal <- function(col, alpha = 0.1) {
  if (!is.numeric(col)) {
    stop("`col` must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !(alpha > 0 && alpha < 1)) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  x <- col[is.finite(col)]
  n <- length(x)

  tests <- list()
  if (n >= 3 && length(unique(x)) > 1) {
    if (n <= 5000) tests$shapiro <- broom::tidy(stats::shapiro.test(x))
    if (n >= 5) tests$lilliefors <- broom::tidy(nortest::lillie.test(x))
  }

  if (!length(tests)) {
    return(tibble::tibble(
      statistic = double(), p.value = double(), method = character(),
      distribution.test = logical(), p_value_sig = character(), distribution = character()
    ))
  }

  dplyr::bind_rows(tests) |>
    dplyr::select("statistic", "p.value", "method") |>
    dplyr::mutate(
      distribution.test = .data$p.value >= alpha,
      p_value_sig = significance_stars(.data$p.value),
      distribution = "normal"
    )
}

#' @keywords internal
#' @noRd
poisson_gof_chisq <- function(x, mu) {
  x_int <- round(x)
  if (any(x_int < 0)) return(NULL)
  n <- length(x_int)

  # Bin edges are placed at (roughly) equal-probability quantiles of the
  # null Poisson(mu) distribution, not at raw integer values — this keeps
  # expected counts adequate regardless of where the *observed* data sits
  # (unlike tail-only pooling, which fails when the data is concentrated
  # somewhere the null distribution considers rare, e.g. overdispersed data).
  n_bins <- max(2L, min(10L, floor(n / 5)))
  interior_probs <- seq(0, 1, length.out = n_bins + 1)[-c(1, n_bins + 1)]
  edges <- unique(stats::qpois(interior_probs, lambda = mu))
  breaks <- c(-Inf, edges, Inf)
  if (length(breaks) < 4) return(NULL) # need >= 3 bins for >= 1 residual df

  obs <- as.integer(table(cut(x_int, breaks = breaks, include.lowest = TRUE)))
  exp_probs <- diff(stats::ppois(breaks, lambda = mu))
  exp_probs <- exp_probs / sum(exp_probs)

  chi_raw <- suppressWarnings(stats::chisq.test(obs, p = exp_probs, rescale.p = TRUE))
  # one degree of freedom subtracted for the estimated rate parameter (mu)
  df <- length(obs) - 1L - 1L
  if (df < 1) return(NULL)
  p_value <- stats::pchisq(unname(chi_raw$statistic), df = df, lower.tail = FALSE)

  tibble::tibble(
    statistic = unname(chi_raw$statistic),
    parameter = df,
    p.value = p_value,
    method = "Chi-squared test for Poisson goodness-of-fit (rate estimated)",
    distribution.test = p_value >= 0.1
  )
}

#' Test Whether a Vector Looks Poisson-Distributed
#'
#' Runs a chi-squared goodness-of-fit test: observed counts (binned at
#' roughly equal-probability quantiles of the fitted Poisson distribution,
#' so expected counts stay adequate regardless of where the data sits) vs.
#' Poisson-expected counts, with one degree of freedom subtracted for the
#' estimated rate. The test is deterministic — no simulated comparison
#' sample is drawn.
#'
#' A Kolmogorov-Smirnov test is deliberately *not* used here: the KS
#' statistic's null distribution assumes a continuous CDF, and the Poisson
#' distribution is discrete with real point masses, which inflates the KS
#' statistic (and deflates its p-value) regardless of true fit — the
#' chi-squared test is the standard, correctly-calibrated tool for
#' discrete/count goodness-of-fit. \code{\link{is_normal}} uses the
#' Lilliefors variant of the Kolmogorov-Smirnov test, which suits the
#' continuous normal distribution.
#'
#' Since the Poisson distribution's support is the non-negative integers,
#' this returns an empty tibble for vectors containing negative values or
#' fewer than 2 observations.
#'
#' @param col A numeric vector.
#' @return A one-row tibble of test results (empty if \code{col} isn't
#'   valid count data, e.g. it has negative values).
#' @export
#' @examples
#' is_poisson(rpois(n = 1000, lambda = 2))
#' is_poisson(runif(1000, min = 2, max = 4))
is_poisson <- function(col) {
  is_int_col <- is.int(col)
  temp_col <- stats::na.omit(col)
  n <- length(temp_col)

  if (n < 2 || min(temp_col) < 0) {
    return(data.frame())
  }

  mu <- mean(temp_col)

  chi_result <- tryCatch(poisson_gof_chisq(temp_col, mu), error = function(e) NULL)
  if (is.null(chi_result)) return(data.frame())

  chi_result |>
    dplyr::mutate(p_value_sig = significance_stars(.data$p.value)) |>
    dplyr::mutate(distribution = "poisson", is_int = is_int_col)
}

#' Check a Vector or Data Frame's Distribution
#'
#' Runs both \code{\link{is_normal}} and \code{\link{is_poisson}} against a
#' numeric vector, or against every numeric column of a data frame.
#'
#' @param x A numeric vector, matrix, or data frame/tibble.
#' @return A tibble of test results (with a \code{feature} column when
#'   \code{x} has multiple columns).
#' @export
#' @examples
#' distribution_test(rpois(n = 1000, lambda = 2))
#' distribution_test(mtcars)
distribution_test <- function(x) {
  if (is.vector(x) && is.numeric(x)) {
    return(dplyr::bind_rows(is_poisson(x), is_normal(x)))
  }

  if (!(is.matrix(x) || is.data.frame(x) || tibble::is_tibble(x))) {
    x <- try(tibble::as_tibble(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop("Error: Must be an object that can be converted to a vector or data frame.")
    }
  }
  if (is.matrix(x)) x <- tibble::as_tibble(x, .name_repair = "universal")

  numeric_cols <- names(dplyr::select(x, dplyr::where(is.numeric)))
  names(numeric_cols) <- numeric_cols

  purrr::map(numeric_cols, \(col) dplyr::bind_rows(is_poisson(x[[col]]), is_normal(x[[col]]))) |>
    purrr::list_rbind(names_to = "feature")
}
