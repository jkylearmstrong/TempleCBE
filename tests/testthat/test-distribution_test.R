test_that("significance_stars maps p-values to the right stars", {
  expect_equal(
    significance_stars(c(0.0001, 0.005, 0.02, 0.08, 0.5)),
    c("***", "**", "*", ".", "")
  )
})

test_that("is.int detects integer-valued vectors", {
  expect_true(is.int(c(1, 2, 3, NA)))
  expect_false(is.int(c(1, 2.5, 3)))
})

test_that("is_normal flags normal data as normal and uniform data as not", {
  set.seed(2026)
  norm_res <- is_normal(rnorm(2000, mean = 5, sd = 3))
  expect_true(all(c("p.value", "distribution.test") %in% names(norm_res)))
  expect_true(all(norm_res$distribution.test))

  unif_res <- is_normal(runif(2000, min = 0, max = 10))
  expect_false(any(unif_res$distribution.test))
})

test_that("is_normal is deterministic (no simulated comparison sample)", {
  x <- rnorm(500, mean = 1, sd = 2)
  expect_identical(is_normal(x), is_normal(x))
})

test_that("is_poisson flags Poisson-shaped data as Poisson-shaped", {
  set.seed(2026)
  res <- is_poisson(rpois(3000, lambda = 4))
  expect_true(all(c("p.value", "distribution.test", "distribution", "is_int") %in% names(res)))
  expect_true(all(res$distribution.test))
  expect_true(all(res$is_int))
  expect_true(all(res$distribution == "poisson"))
})

test_that("is_poisson's chi-squared test correctly flags non-Poisson count data as non-Poisson", {
  # Wildly overdispersed bimodal counts: variance >> mean, nothing like a
  # Poisson(mean). Regression test for a prior bug where the chi-squared
  # branch's distribution.test flag was inverted relative to every other
  # test in this file (p < 0.1 was mislabeled as "looks Poisson").
  overdispersed <- c(rep(0, 800), rep(80, 800))
  res <- is_poisson(overdispersed)
  chi_row <- res[grepl("goodness-of-fit", res$method), ]
  expect_equal(nrow(chi_row), 1)
  expect_lt(chi_row$p.value, 0.001)
  expect_false(chi_row$distribution.test)
})

test_that("is_poisson is deterministic and returns empty for non-count data", {
  x <- rpois(500, lambda = 3)
  expect_identical(is_poisson(x), is_poisson(x))

  expect_equal(nrow(is_poisson(rnorm(100, mean = -5, sd = 1))), 0)
  expect_equal(nrow(is_poisson(numeric(0))), 0)
  expect_equal(nrow(is_poisson(c(NA_real_, NA_real_))), 0)
})

test_that("distribution_test runs both tests on a vector and on every numeric column of a data frame", {
  set.seed(2026)
  vec_res <- distribution_test(rpois(500, lambda = 2))
  expect_true(all(c("normal", "poisson") %in% vec_res$distribution))

  df <- data.frame(counts = rpois(500, lambda = 2), continuous = rnorm(500))
  df_res <- distribution_test(df)
  expect_true(all(c("counts", "continuous") %in% df_res$feature))
})
