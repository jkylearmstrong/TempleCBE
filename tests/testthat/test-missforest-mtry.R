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
  expect_error(missforest_oob_by_mtry(toy_df(), mtry = 2.7), "positive integer")
  expect_error(missforest_oob_by_mtry(toy_df(), mtry = ncol(toy_df())), "must be integers in 1:")
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

  expect_named(res, c("imp_data", "oob_error", "best", "excluded_high_missing"))

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

test_that("`exclude` names that are not columns of `data` are an error, not silently dropped", {
  # Audit A3-07. A misspelt identifier ("idd") used to be dropped by
  # intersect(), so `id` stayed in the predictor set, was imputed and was
  # used as a predictor, with no message. The check runs before any fit, so
  # it needs no missForest.
  expect_error(
    missforest_sweep_mtry(
      toy_df(), exclude = c("id", "time", "not_a_column"), parallel = FALSE
    ),
    "`exclude` names column\\(s\\) that are not in `data`: not_a_column"
  )
  expect_error(
    missforest_sweep_mtry(toy_df(), exclude = "idd", parallel = FALSE),
    "not in `data`: idd"
  )
  expect_error(
    missforest_sweep_mtry(toy_df(), exclude = c("idd", "tyme", "id"), parallel = FALSE),
    "not in `data`: idd, tyme"
  )
  # A position is not a name either.
  expect_error(
    missforest_sweep_mtry(toy_df(), exclude = 1:2, parallel = FALSE),
    "not in `data`: 1, 2"
  )
})

test_that("valid `exclude` names are held out unchanged, as before", {
  skip_if_not_installed("missForest")

  res <- missforest_sweep_mtry(
    toy_df(), exclude = c("time", "id", "time"),
    ntree = 20, maxiter = 2, seed = 5, parallel = FALSE
  )
  expect_equal(names(res$imp_data), names(toy_df()))
  expect_equal(res$imp_data$id, toy_df()$id)
  expect_equal(res$imp_data$time, toy_df()$time)
  expect_setequal(res$best$column, c("a", "b", "c"))
})

# --- best_mtry_per_column (shared by both engines) ----------------------------

test_that("best_mtry_per_column keeps one row for a column whose error is missing at every mtry", {
  # Audit A3-03. The column used to be dropped without a word, so
  # missforest_sweep_mtry() failed on a missing column far from the cause and
  # missranger_sweep_mtry() would return it unimputed.
  oe <- tibble::tibble(
    column = c("a", "a", "b", "b"), error_type = "MSE",
    error = c(NaN, NaN, 0.5, 0.4), mtry = c(1L, 2L, 1L, 2L)
  )
  expect_warning(
    best <- best_mtry_per_column(oe),
    "missing at every `mtry` for column\\(s\\): a"
  )
  expect_setequal(best$column, c("a", "b"))
  expect_equal(nrow(best), 2L)
  # First swept mtry, error left missing; the other column is ranked as usual.
  expect_equal(best$mtry[best$column == "a"], 1L)
  expect_true(is.na(best$error[best$column == "a"]))
  expect_equal(best$mtry[best$column == "b"], 2L)
  expect_equal(best$error[best$column == "b"], 0.4)

  # Several such columns are all named, once.
  oe$column[1:2] <- "z"
  oe2 <- rbind(oe, transform(oe[1:2, ], column = "y"))
  expect_warning(best2 <- best_mtry_per_column(oe2), "column\\(s\\): z, y")
  expect_equal(nrow(best2), 3L)
})

test_that("best_mtry_per_column ranks as before when only some errors are missing", {
  oe <- tibble::tibble(
    column = c("a", "a", "b", "b", "c", "c"), error_type = "MSE",
    error = c(NaN, 0.3, 0.5, 0.4, 0.2, 0.2), mtry = c(1L, 2L, 1L, 2L, 1L, 2L)
  )
  best <- expect_no_warning(best_mtry_per_column(oe))
  # Same row order as the input table; ties go to the first occurrence ("c").
  expect_equal(best$column, c("a", "b", "c"))
  expect_equal(best$mtry, c(2L, 2L, 1L))
  expect_equal(best$error, c(0.3, 0.4, 0.2))
})

test_that("best_mtry_per_column returns an empty table, quietly, when nothing was scored", {
  oe <- tibble::tibble(
    column = character(), error_type = character(), error = numeric(), mtry = integer()
  )
  best <- expect_no_warning(best_mtry_per_column(oe))
  expect_equal(nrow(best), 0L)
  expect_named(best, names(oe))
})

test_that("a column observed in a single row no longer breaks the missForest sweep", {
  skip_if_not_installed("missForest")

  # missForest reports a NaN OOB error for such a column at every mtry.
  df <- toy_df()
  df$one <- c(3, rep(NA, nrow(df) - 1L))

  res <- NULL
  expect_warning(
    res <- missforest_sweep_mtry(
      df, exclude = c("id", "time"), ntree = 20, maxiter = 2,
      seed = 1, parallel = FALSE
    ),
    "missing at every `mtry` for column\\(s\\): one"
  )
  expect_equal(names(res$imp_data), names(df))
  expect_true("one" %in% res$best$column)
  expect_true(is.na(res$best$error[res$best$column == "one"]))
  # The other columns are untouched by it.
  expect_setequal(res$best$column, c("a", "b", "c", "one"))
  expect_false(anyNA(res$imp_data[, c("a", "b", "c")]))
})

test_that("fully observed data is a supported, empty-table case", {
  skip_if_not_installed("missForest")

  full <- toy_df()[, c("id", "time")]
  full$x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8)
  full$y <- c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5)

  res <- missforest_sweep_mtry(
    full, exclude = c("id", "time"), ntree = 5, maxiter = 2,
    seed = 1, parallel = FALSE
  )
  expect_equal(res$imp_data, tibble::as_tibble(full))
})

# --- max_pct_missing --------------------------------------------------------

test_that("max_pct_missing holds out sparse columns and carries them through", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  # Observed in 2 of 12 rows: imputing it would manufacture 10 values.
  df$sparse <- c(1, 2, rep(NA, 10))

  res <- NULL
  expect_message(
    res <- missforest_sweep_mtry(
      df, exclude = c("id", "time"), max_pct_missing = 0.8,
      ntree = 20, maxiter = 2, seed = 1, parallel = FALSE
    ),
    "sparse"
  )

  expect_equal(res$excluded_high_missing, "sparse")
  # Held out, not dropped: still present, still unimputed.
  expect_equal(names(res$imp_data), names(df))
  expect_equal(sum(is.na(res$imp_data$sparse)), 10L)
  expect_false("sparse" %in% res$best$column)
  # Everything else is still imputed.
  expect_false(anyNA(res$imp_data[, c("a", "b", "c")]))
})

test_that("holding out by threshold matches naming the column in exclude", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  df$sparse <- c(1, 2, rep(NA, 10))
  args <- list(df, ntree = 20, maxiter = 2, seed = 99, parallel = FALSE)

  by_name <- do.call(missforest_sweep_mtry,
    c(args, list(exclude = c("id", "time", "sparse"))))
  by_rule <- suppressMessages(do.call(missforest_sweep_mtry,
    c(args, list(exclude = c("id", "time"), max_pct_missing = 0.8))))

  # The rule reproduces the hardcoded decision, but derives it from the data.
  expect_equal(by_name$imp_data, by_rule$imp_data)
  expect_equal(by_name$best, by_rule$best)
  expect_equal(by_name$excluded_high_missing, character(0))
  expect_equal(by_rule$excluded_high_missing, "sparse")
})

test_that("no threshold means every column is imputed however sparse", {
  skip_if_not_installed("missForest")

  df <- toy_df()
  df$sparse <- c(1, 2, rep(NA, 10))
  res <- missforest_sweep_mtry(
    df, exclude = c("id", "time"), ntree = 20, maxiter = 2,
    seed = 1, parallel = FALSE
  )
  expect_equal(res$excluded_high_missing, character(0))
  expect_false(anyNA(res$imp_data))   # sparse got imputed
})

test_that("max_pct_missing is validated", {
  df <- toy_df()
  for (bad in list(0, -1, 2, "a", c(0.5, 0.6), NA_real_)) {
    expect_error(
      missforest_sweep_mtry(df, exclude = c("id", "time"), max_pct_missing = bad,
                            parallel = FALSE),
      "must be a single number in"
    )
  }
})

test_that("normalize_mtry_values deduplicates, validates integer-ness, and sorts (A3-08, A3-09)", {
  expect_equal(unname(normalize_mtry_values(c(2, 2), max_mtry = 4)), 2L)
  expect_equal(unname(normalize_mtry_values(c(3, 1, 2), max_mtry = 4)), c(1L, 2L, 3L))
  expect_error(normalize_mtry_values(2.7, max_mtry = 4), "must be integers in 1:4")
  expect_error(normalize_mtry_values(c(1, 2.5), max_mtry = 4), "must be integers in 1:4")
})

test_that("missforest_oob_by_mtry handles logical columns and filters oob_error to incomplete columns (A3-13, A3-14)", {
  skip_if_not_installed("missForest")
  df <- data.frame(
    x1 = c(1, 2, NA, 4, 5, 6, 7, 8),
    x2 = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, FALSE, TRUE),
    x3 = c(1, 2, 3, 4, 5, 6, 7, 8) # complete
  )
  res <- missforest_oob_by_mtry(df, mtry = 1, ntree = 20, maxiter = 2)
  expect_s3_class(res$ximp, "tbl_df")
  expect_false(anyNA(res$ximp))
  # x3 is complete so it should not appear in oob_error
  expect_equal(res$oob_error$column, c("x1", "x2"))
})

test_that("missforest_sweep_mtry validates seed and restores caller RNG state (A3-11, A3-22)", {
  skip_if_not_installed("missForest")
  df <- toy_df()
  expect_error(
    missforest_sweep_mtry(df, exclude = c("id", "time"), seed = "42", parallel = FALSE),
    "`seed` must be NULL, a single logical, or a single whole number"
  )

  set.seed(77)
  old_draw <- runif(1)
  set.seed(77)
  res <- missforest_sweep_mtry(
    df, exclude = c("id", "time"), seed = 123, ntree = 20, maxiter = 2, parallel = FALSE
  )
  new_draw <- runif(1)
  expect_equal(new_draw, old_draw)
})

test_that("missforest_sweep_mtry restores character and logical column types (A3-13)", {
  skip_if_not_installed("missForest")
  df <- data.frame(
    id = 1:8,
    a = c("cat", "dog", NA, "cat", "dog", "cat", "dog", "cat"),
    b = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, FALSE, TRUE),
    c = c(1, 2, 3, NA, 5, 6, 7, 8),
    stringsAsFactors = FALSE
  )
  res <- missforest_sweep_mtry(
    df, exclude = "id", ntree = 20, maxiter = 2, seed = 42, parallel = FALSE
  )
  expect_type(res$imp_data$a, "character")
  expect_type(res$imp_data$b, "logical")
})

