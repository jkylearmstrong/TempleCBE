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
#' Runs a Shapiro-Wilk test (for n <= 5000, on a random subsample above that)
#' and a Kolmogorov-Smirnov test against a normal distribution matched on
#' mean/sd.
#'
#' @param col A numeric vector.
#' @return A tibble of test results.
#' @export
#' @examples
#' is_normal(rnorm(1000, mean = 5, sd = 3))
#' is_normal(runif(1000, min = 2, max = 4))
is_normal <- function(col) {
  temp_col <- stats::na.omit(col)
  mu <- mean(temp_col, na.rm = TRUE)
  sd_ <- stats::sd(temp_col, na.rm = TRUE)
  n <- length(temp_col)
  y <- stats::rnorm(n, mu, sd_)

  shapiro_result <- data.frame()
  if (n > 3 && n <= 5000) {
    shapiro_result <- broom::tidy(stats::shapiro.test(temp_col)) |>
      dplyr::mutate(distribution.test = .data$p.value >= 0.1)
  } else if (n > 5000) {
    shapiro_result <- broom::tidy(stats::shapiro.test(sample(temp_col, 5000))) |>
      dplyr::mutate(distribution.test = .data$p.value >= 0.1)
  }

  ks_result <- suppressWarnings(
    broom::tidy(stats::ks.test(temp_col, y)) |>
      dplyr::mutate(distribution.test = .data$p.value >= 0.1)
  )

  dplyr::bind_rows(ks_result, shapiro_result) |>
    dplyr::mutate(p_value_sig = significance_stars(.data$p.value)) |>
    dplyr::mutate(distribution = "normal")
}

#' Test Whether a Vector Looks Poisson-Distributed
#'
#' Runs a Kolmogorov-Smirnov and a chi-squared test against a Poisson
#' distribution matched on the mean.
#'
#' @param col A numeric vector.
#' @return A tibble of test results.
#' @export
#' @examples
#' is_poisson(rpois(n = 1000, lambda = 2))
#' is_poisson(runif(1000, min = 2, max = 4))
is_poisson <- function(col) {
  is_int_col <- is.int(col)
  temp_col <- stats::na.omit(col)
  mu <- mean(temp_col, na.rm = TRUE)
  n <- length(temp_col)
  y <- if (mu >= 0) stats::rpois(n, mu) else -stats::rpois(n, abs(mu))

  ks_result <- tryCatch(
    suppressWarnings(
      broom::tidy(stats::ks.test(temp_col, y)) |>
        dplyr::mutate(distribution.test = .data$p.value >= 0.1)
    ),
    error = function(e) NULL
  )
  if (is.null(ks_result)) return(data.frame())

  chi_result <- suppressWarnings(
    broom::tidy(stats::chisq.test(table(sort(temp_col), sort(y)))) |>
      dplyr::mutate(distribution.test = .data$p.value < 0.1)
  )

  dplyr::bind_rows(ks_result, chi_result) |>
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
