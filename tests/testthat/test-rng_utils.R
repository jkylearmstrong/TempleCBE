test_that("local_seed() is reproducible and restores the caller's RNG state", {
  draw <- function(seed) {
    local_seed(seed)
    stats::runif(3)
  }

  expect_identical(draw(42), draw(42))
  expect_false(identical(draw(42), draw(43)))

  # the caller's stream carries on exactly as if the seeded call never happened
  set.seed(10)
  expected <- stats::runif(2)
  set.seed(10)
  invisible(draw(42))
  expect_identical(stats::runif(2), expected)
})

test_that("local_seed() leaves no .Random.seed behind when there was none", {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  withr::defer({
    if (had) assign(".Random.seed", old, envir = globalenv())
  })

  rm(".Random.seed", envir = globalenv())
  f <- function() {
    local_seed(1)
    stats::runif(1)
  }
  invisible(f())
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("cbe_loco_mp_coxnet(seed =) does not disturb the caller's RNG stream", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")

  set.seed(21)
  n <- 80
  x <- matrix(stats::rnorm(n * 3), n, dimnames = list(NULL, paste0("x", 1:3)))
  time <- stats::rexp(n, exp(x[, "x1"]) / 10)
  status <- stats::rbinom(n, 1, 0.85)

  set.seed(7)
  expected <- stats::runif(2)
  set.seed(7)
  fit <- cbe_loco_mp_coxnet(x = x, y = survival::Surv(time, status), B = 10, seed = 1, penalty = 0.02)
  expect_s3_class(fit, "cbe_loco_mp_coxnet")
  expect_identical(stats::runif(2), expected)
})

test_that("missforest_sweep_mtry(seed =) sequential path does not disturb the caller's RNG stream", {
  skip_if_not_installed("missForest")

  df <- data.frame(
    id = 1:30,
    a = c(stats::rnorm(28), NA, NA),
    b = c(NA, stats::rnorm(29)),
    c = stats::rnorm(30),
    d = stats::rnorm(30)
  )

  set.seed(7)
  expected <- stats::runif(2)
  set.seed(7)
  invisible(missforest_sweep_mtry(df, exclude = "id", ntree = 10, maxiter = 1, seed = 3, parallel = FALSE))
  expect_identical(stats::runif(2), expected)
})

# Two incomplete columns (a, b) and two complete ones (c, d), so missRanger's
# admissible mtry is 1:2.
missranger_rng_df <- function() {
  data.frame(
    id = 1:16,
    a = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, NA, 13, 14, 15, 16),
    b = c(2, NA, 6, 8, 10, 12, 14, NA, 18, 20, NA, 24, 26, 28, 30, 32),
    c = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3),
    d = c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9, 0, 4, 5)
  )
}

test_that("missranger_oob_by_mtry(seed =) restores the caller's RNG state", {
  skip_if_not_installed("missRanger")
  df <- missranger_rng_df()[, c("a", "b", "c", "d")]

  set.seed(7)
  before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  fit <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 10, maxiter = 2, seed = 3)
  expect_s3_class(fit$ximp, "tbl_df")
  expect_identical(get(".Random.seed", envir = globalenv(), inherits = FALSE), before)

  set.seed(7)
  expected <- stats::runif(2)
  set.seed(7)
  invisible(missranger_oob_by_mtry(df, mtry = 1, num.trees = 10, maxiter = 2, seed = 3))
  expect_identical(stats::runif(2), expected)
})

test_that("missranger_oob_by_mtry(seed =) leaves no .Random.seed behind when there was none", {
  skip_if_not_installed("missRanger")
  df <- missranger_rng_df()[, c("a", "b", "c", "d")]

  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  withr::defer({
    if (had) assign(".Random.seed", old, envir = globalenv())
  })

  rm(".Random.seed", envir = globalenv())
  invisible(missranger_oob_by_mtry(df, mtry = 1, num.trees = 10, maxiter = 2, seed = 3))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("missranger_oob_by_mtry() without a seed still draws from the caller's stream", {
  skip_if_not_installed("missRanger")
  df <- missranger_rng_df()[, c("a", "b", "c", "d")]

  # An unseeded fit is documented to use the ambient generator, so the
  # generator advances; only a seeded fit is isolated.
  set.seed(7)
  before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  invisible(missranger_oob_by_mtry(df, mtry = 1, num.trees = 10, maxiter = 2))
  expect_false(identical(get(".Random.seed", envir = globalenv(), inherits = FALSE), before))
})

test_that("missranger_sweep_mtry(seed =) sequential path does not disturb the caller's RNG stream", {
  skip_if_not_installed("missRanger")
  df <- missranger_rng_df()

  set.seed(7)
  expected <- stats::runif(2)
  set.seed(7)
  before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  invisible(missranger_sweep_mtry(
    df, exclude = "id", num.trees = 10, maxiter = 2, seed = 3, parallel = FALSE
  ))
  # .Random.seed is exactly as before the call, not just equivalent.
  expect_identical(get(".Random.seed", envir = globalenv(), inherits = FALSE), before)
  expect_identical(stats::runif(2), expected)
})

test_that("a bootstrap-style loop around a seeded missranger sweep draws fresh indices each replicate", {
  skip_if_not_installed("missRanger")
  df <- missranger_rng_df()

  # Every replicate draws its own sample and then runs a seeded imputation.
  # If the imputation re-seeds the caller's generator, replicates 2, 3 and 4
  # all draw the same indices: they depend on the imputation seed alone.
  replicates <- function(impute) {
    set.seed(2026)
    do.call(rbind, lapply(1:4, function(b) {
      idx <- sample(10, 4)
      impute()
      idx
    }))
  }
  reference <- replicates(function() NULL)
  seeded <- replicates(function() {
    invisible(missranger_sweep_mtry(
      df, exclude = "id", num.trees = 10, maxiter = 2, seed = 1, parallel = FALSE
    ))
  })

  expect_gt(nrow(unique(reference)), 1L)
  expect_identical(seeded, reference)
})
