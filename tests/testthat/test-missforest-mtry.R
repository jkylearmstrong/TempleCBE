toy_df <- function() {
  data.frame(
    id   = 1:12,
    time = rep(c(0, 5, 10), each = 4),
    a    = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, NA),
    b    = c(2, 4, 6, NA, 10, 12, 14, NA, 18, 20, NA, 24),
    c    = c(5, 4, 3, NA, 1, 2, 3, 4, 5, NA, 3, 2)
  )
}

# --- missforest_oob_by_mtry -------------------------------------------------

test_that("missforest_oob_by_mtry returns ximp and a tidy per-column OOB table", {
  skip_if_not_installed("missForest")
  set.seed(1)

  df <- toy_df()[, c("a", "b", "c")]
  res <- missforest_oob_by_mtry(df, mtry = 1, ntree = 20, maxiter = 2)

  expect_named(res, c("ximp", "oob_error"))
  expect_s3_class(res$ximp, "tbl_df")
  expect_equal(nrow(res$ximp), nrow(df))
  expect_false(anyNA(res$ximp))

  expect_equal(res$oob_error$column, c("a", "b", "c"))
  expect_equal(unique(res$oob_error$error_type), "MSE")
  expect_equal(unique(res$oob_error$mtry), 1L)
})

test_that("character columns are converted to factors idempotently", {
  skip_if_not_installed("missForest")

  chr <- data.frame(
    g = c("x", "y", "x", "y", NA, "x", "y", "x"),
    v = c(1, 2, 3, NA, 5, 6, 7, 8),
    stringsAsFactors = FALSE
  )
  fct <- transform(chr, g = as.factor(g))

  set.seed(99)
  from_chr <- missforest_oob_by_mtry(chr, mtry = 1, ntree = 20, maxiter = 2)
  set.seed(99)
  from_fct <- missforest_oob_by_mtry(fct, mtry = 1, ntree = 20, maxiter = 2)

  # Pre-converting at the call site must not change the result -- this is what
  # lets one function replace call sites that converted at different points.
  expect_equal(from_chr$oob_error, from_fct$oob_error)
  expect_equal(from_chr$ximp, from_fct$ximp)
  expect_equal(from_chr$oob_error$error_type, c("PFC", "MSE"))
})

test_that("missforest_oob_by_mtry validates its inputs", {
  skip_if_not_installed("missForest")
  expect_error(missforest_oob_by_mtry(1:10, mtry = 1), "must be a data frame")
  expect_error(missforest_oob_by_mtry(toy_df(), mtry = 0), "positive integer")
  expect_error(missforest_oob_by_mtry(toy_df(), mtry = c(1, 2)), "positive integer")
})

# --- all-NA columns ---------------------------------------------------------

test_that("an entirely-NA column is refused by name, not silently dropped", {
  skip_if_not_installed("missForest")

  df <- toy_df()[, c("a", "b", "c")]
  df$b <- NA_real_

  expect_error(
    missforest_oob_by_mtry(df, mtry = 1, ntree = 20, maxiter = 2),
    "entirely NA: b"
  )
})

test_that("the sweep refuses all-NA columns before dispatching any work", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  df$a <- NA_real_
  df$c <- NA_real_

  expect_error(
    missforest_sweep_mtry(
      df,
      exclude = c("id", "time"),
      ntree = 20, maxiter = 2, seed = 1, parallel = FALSE
    ),
    "entirely NA: a, c"
  )
})

test_that("an all-NA column that is excluded does not trip the guard", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  df$time <- NA_real_

  res <- missforest_sweep_mtry(
    df,
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, seed = 3, parallel = FALSE
  )

  # Excluded columns are re-attached untouched, still all NA.
  expect_true(all(is.na(res$imp_data$time)))
  expect_setequal(res$best$column, c("a", "b", "c"))
})

test_that("a logical all-NA column is caught the same as any other", {
  # The `else NA` fill in a mapping-driven reader produces a full-length
  # logical column: no information, but it still counts toward ncol() and so
  # toward both the mtry range and the predictor pool at every split.
  df <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(2, NA, 6, 8),
    mapped_but_absent = NA
  )
  expect_true(is.logical(df$mapped_but_absent))
  expect_error(
    missforest_sweep_mtry(df, parallel = FALSE),
    "entirely NA: mapped_but_absent"
  )
})

# --- missforest_impute_by_mtry ----------------------------------------------

test_that("runs are looked up by name, not by position", {
  # Regression guard: `sweep[[cur_mtry]]` with an integer indexes by position
  # and silently returns the wrong run whenever the swept mtry values are not
  # exactly 1:n. Here mtry 2 and 3 sit at positions 1 and 2.
  sweep <- list(
    "2" = list(ximp = tibble::tibble(a = c(1, 1), b = c(2, 2))),
    "3" = list(ximp = tibble::tibble(a = c(9, 9), b = c(8, 8)))
  )
  best <- tibble::tibble(column = c("a", "b"), mtry = c(3L, 2L))

  out <- missforest_impute_by_mtry(sweep, best)

  expect_equal(out$a, c(9, 9)) # from run "3"
  expect_equal(out$b, c(2, 2)) # from run "2"
})

test_that("missforest_impute_by_mtry errors on a missing run", {
  sweep <- list("1" = list(ximp = tibble::tibble(a = 1)))
  best <- tibble::tibble(column = "a", mtry = 7L)
  expect_error(missforest_impute_by_mtry(sweep, best), "No sweep result named '7'")
})

test_that("missforest_impute_by_mtry validates `best`", {
  expect_error(
    missforest_impute_by_mtry(list(), tibble::tibble(x = 1)),
    "`column` and `mtry`"
  )
})

# --- missforest_sweep_mtry --------------------------------------------------

test_that("sweep imputes everything, preserves excluded columns and column order", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  res <- missforest_sweep_mtry(
    df,
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, seed = 42, parallel = FALSE
  )

  expect_named(res, c("imp_data", "oob_error", "best"))

  # Original column order restored, not grouped by winning mtry.
  expect_equal(names(res$imp_data), names(df))

  # Excluded columns come back untouched.
  expect_equal(res$imp_data$id, df$id)
  expect_equal(res$imp_data$time, df$time)

  # Nothing missing afterwards.
  expect_false(anyNA(res$imp_data))

  # One winning row per imputed column; excluded columns never swept.
  expect_setequal(res$best$column, c("a", "b", "c"))
  expect_equal(nrow(res$best), 3L)
})

test_that("the default mtry sweep covers 1:(p - 1) of the non-excluded columns", {
  skip_if_not_installed("missForest")

  res <- missforest_sweep_mtry(
    toy_df(),
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, seed = 1, parallel = FALSE
  )
  # 3 columns remain after exclude -> mtry 1:2
  expect_equal(sort(unique(res$oob_error$mtry)), 1:2)
})

test_that("each column independently keeps its lowest-error mtry", {
  skip_if_not_installed("missForest")

  res <- missforest_sweep_mtry(
    toy_df(),
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, seed = 7, parallel = FALSE
  )

  for (col in res$best$column) {
    all_errors <- res$oob_error$error[res$oob_error$column == col]
    winning <- res$best$error[res$best$column == col]
    expect_equal(winning, min(all_errors))
  }
})

test_that("an integer seed makes the sequential sweep reproducible", {
  skip_if_not_installed("missForest")

  args <- list(
    toy_df(),
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, parallel = FALSE
  )

  one <- do.call(missforest_sweep_mtry, c(args, list(seed = 123)))
  two <- do.call(missforest_sweep_mtry, c(args, list(seed = 123)))

  expect_equal(one$imp_data, two$imp_data)
  expect_equal(one$oob_error, two$oob_error)
})

sweep_args <- function() {
  list(
    toy_df(),
    exclude = c("id", "time"),
    ntree = 20, maxiter = 2, parallel = TRUE, seed = 2024L
  )
}

test_that("the parallel path is reproducible", {
  skip_if_not_installed("missForest")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  # multisession workers load TempleCBE from .libPaths(). Under
  # devtools::load_all() the namespace is not installed, so a worker cannot
  # find it and this test fails for reasons unrelated to the code under test.
  # find.package() then points at the source tree, which has no Meta/.
  pkg_dir <- find.package("TempleCBE", quiet = TRUE)
  skip_if_not(
    length(pkg_dir) == 1L &&
      file.exists(file.path(pkg_dir, "Meta", "package.rds")),
    "TempleCBE is not installed; multisession workers cannot load a dev namespace."
  )

  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)

  one <- do.call(missforest_sweep_mtry, sweep_args())
  two <- do.call(missforest_sweep_mtry, sweep_args())

  expect_equal(one$imp_data, two$imp_data)
  expect_equal(one$oob_error, two$oob_error)
})

test_that("the same seed gives the same answer under a different plan", {
  # This is the property that lets a Windows multisession run and a Linux
  # multicore run agree: L'Ecuyer-CMRG streams do not depend on the plan or
  # the worker count.
  skip_if_not_installed("missForest")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  skip_if_not_installed("pkgload")
  # multisession workers are separate R processes that can only `library()` an
  # *installed* package, so this cannot run against a load_all() shim. It does
  # run under R CMD check, where the package is installed.
  skip_if(
    !is.null(pkgload::dev_meta("TempleCBE")),
    "multisession workers cannot see a load_all()-ed package"
  )

  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  sequential_result <- do.call(missforest_sweep_mtry, sweep_args())

  future::plan(future::multisession, workers = 2)
  multisession_result <- do.call(missforest_sweep_mtry, sweep_args())

  expect_equal(sequential_result$imp_data, multisession_result$imp_data)
  expect_equal(sequential_result$oob_error, multisession_result$oob_error)
})

test_that("missforest_sweep_mtry validates its inputs", {
  expect_error(missforest_sweep_mtry(1:10), "must be a data frame")
  expect_error(
    missforest_sweep_mtry(data.frame(a = 1:3), exclude = "a"),
    "at least 2 columns"
  )
  expect_error(
    missforest_sweep_mtry(toy_df(), exclude = c("id", "time"), mtry_values = 99),
    "must be integers in"
  )
})

test_that("unknown `exclude` names are ignored rather than erroring", {
  skip_if_not_installed("missForest")

  res <- missforest_sweep_mtry(
    toy_df(),
    exclude = c("id", "time", "not_a_column"),
    ntree = 20, maxiter = 2, seed = 5, parallel = FALSE
  )
  expect_equal(names(res$imp_data), names(toy_df()))
})
