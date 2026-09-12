# Shaped like the intended use: incomplete "output" columns imputed against
# fully-observed "input" columns. That shape matters for missRanger, whose
# admissible mtry is the number of complete columns -- see
# missranger_max_mtry(). A frame in which every column has gaps admits mtry=1
# alone and has nothing to sweep.
toy_df <- function() {
  data.frame(
    id   = 1:12,
    time = rep(c(0, 5, 10), each = 4),
    in1  = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8),
    in2  = c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5),
    in3  = c(1, 4, 1, 4, 2, 1, 3, 5, 6, 2, 3, 7),
    a    = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, NA),
    b    = c(2, 4, 6, NA, 10, 12, 14, NA, 18, 20, NA, 24),
    c    = c(5, 4, 3, NA, 1, 2, 3, 4, 5, NA, 3, 2)
  )
}

# Every column has gaps: the degenerate case for missRanger.
all_gappy_df <- function() {
  data.frame(
    a = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, NA),
    b = c(2, 4, 6, NA, 10, 12, 14, NA, 18, 20, NA, 24),
    c = c(5, 4, 3, NA, 1, 2, 3, 4, 5, NA, 3, 2)
  )
}

# --- missranger_oob_by_mtry -------------------------------------------------

test_that("missranger_oob_by_mtry returns ximp and a tidy per-column OOB table", {
  skip_if_not_installed("missRanger")

  df <- all_gappy_df()
  res <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 50, maxiter = 2, seed = 1)

  expect_named(res, c("ximp", "oob_error"))
  expect_s3_class(res$ximp, "tbl_df")
  expect_equal(nrow(res$ximp), nrow(df))
  expect_false(anyNA(res$ximp))

  expect_setequal(res$oob_error$column, c("a", "b", "c"))
  expect_equal(unique(res$oob_error$error_type), "1-R2")
  expect_equal(unique(res$oob_error$mtry), 1L)
})

test_that("only imputed columns are scored; complete columns get no row", {
  skip_if_not_installed("missRanger")

  # missForest run variablewise scores every column. missRanger fits forests
  # only for columns that actually have missing values, so the error table is
  # narrower -- the sweep must not assume one row per column.
  df <- data.frame(
    a = c(1, 2, NA, 4, 5, 6, 7, 8),
    b = c(2, NA, 6, 8, 10, 12, 14, 16),
    complete = c(3, 1, 4, 1, 5, 9, 2, 6)
  )
  res <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 50, maxiter = 2, seed = 1)

  expect_setequal(res$oob_error$column, c("a", "b"))
  expect_false("complete" %in% res$oob_error$column)
})

test_that("a categorical column is scored as classification error", {
  skip_if_not_installed("missRanger")

  df <- data.frame(
    g = factor(c("x", "y", "x", "y", NA, "x", "y", "x")),
    v = c(1, 2, 3, NA, 5, 6, 7, 8)
  )
  res <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 50, maxiter = 2, seed = 1)

  expect_equal(res$oob_error$error_type[res$oob_error$column == "g"], "class_error")
  expect_equal(res$oob_error$error_type[res$oob_error$column == "v"], "1-R2")
})

test_that("keep_forests returns the fit, which is the seam for new data", {
  skip_if_not_installed("missRanger")

  df <- all_gappy_df()
  res <- missranger_oob_by_mtry(
    df, mtry = 1, num.trees = 50, maxiter = 2, seed = 1, keep_forests = TRUE
  )

  expect_true("fit" %in% names(res))
  expect_s3_class(res$fit, "missRanger")
  # This is what missForest structurally cannot do: apply to a later batch.
  expect_true(length(res$fit$forests) > 0)
})

test_that("missranger_oob_by_mtry validates its inputs", {
  skip_if_not_installed("missRanger")
  expect_error(missranger_oob_by_mtry(1:10, mtry = 1), "must be a data frame")
  expect_error(missranger_oob_by_mtry(toy_df(), mtry = 0), "positive integer")
  expect_error(missranger_oob_by_mtry(toy_df(), mtry = c(1, 2)), "positive integer")
})

# --- all-NA columns ---------------------------------------------------------

test_that("an all-NA column is refused rather than silently left NA", {
  skip_if_not_installed("missRanger")

  # Unlike missForest, missRanger does not drop or error on an all-NA column:
  # it returns it still entirely NA, so the frame looks imputed but is not.
  df <- data.frame(
    a = c(1, 2, NA, 4, 5, 6),
    b = c(2, NA, 6, 8, 10, 12),
    dead = rep(NA, 6)
  )

  expect_error(
    missranger_oob_by_mtry(df, mtry = 1, num.trees = 20, maxiter = 2),
    "entirely NA: dead"
  )
  expect_error(
    missranger_sweep_mtry(df, num.trees = 20, maxiter = 2, parallel = FALSE),
    "entirely NA: dead"
  )
})

test_that("a mapped column absent from the uploaded file is refused", {
  skip_if_not_installed("missRanger")

  # A mapping-driven reader fills a mapped-but-absent column with a bare
  # NA, producing a full-length logical column. It passes name-based schema
  # checks and counts toward ncol(), so it would otherwise be swept over as a
  # predictor that is pure absence.
  uploaded_file <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(2, NA, 6, 8),
    assay_value = NA
  )
  expect_true(is.logical(uploaded_file$assay_value))

  expect_error(
    missranger_sweep_mtry(uploaded_file, parallel = FALSE),
    "entirely NA: assay_value"
  )
})

# --- missranger_sweep_mtry --------------------------------------------------

test_that("sweep imputes everything, preserves excluded columns and column order", {
  skip_if_not_installed("missRanger")

  df <- toy_df()
  res <- missranger_sweep_mtry(
    df,
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, seed = 42, parallel = FALSE
  )

  expect_named(res, c("imp_data", "oob_error", "best"))
  expect_equal(names(res$imp_data), names(df))
  expect_equal(res$imp_data$id, df$id)
  expect_equal(res$imp_data$time, df$time)
  expect_false(anyNA(res$imp_data))

  expect_setequal(res$best$column, c("a", "b", "c"))
})

test_that("the default mtry sweep covers the admissible range, not 1:(p - 1)", {
  skip_if_not_installed("missRanger")

  res <- missranger_sweep_mtry(
    toy_df(),
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, seed = 1, parallel = FALSE
  )
  # in1/in2/in3 are complete -> three admissible mtry values, NOT ncol - 1.
  expect_equal(missranger_max_mtry(toy_df()[, -(1:2)]), 3L)
  expect_equal(sort(unique(res$oob_error$mtry)), 1:3)
})

test_that("each column independently keeps its lowest-error mtry", {
  skip_if_not_installed("missRanger")

  res <- missranger_sweep_mtry(
    toy_df(),
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, seed = 7, parallel = FALSE
  )

  for (col in res$best$column) {
    all_errors <- res$oob_error$error[res$oob_error$column == col]
    expect_equal(res$best$error[res$best$column == col], min(all_errors))
  }
})

test_that("complete columns are carried through unchanged, not dropped", {
  skip_if_not_installed("missRanger")

  df <- data.frame(
    id = 1:10,
    a = c(1, 2, NA, 4, 5, 6, 7, 8, 9, NA),
    b = c(2, NA, 6, 8, 10, 12, 14, 16, 18, 20),
    complete = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  )
  res <- missranger_sweep_mtry(
    df, exclude = "id", num.trees = 50, maxiter = 2, seed = 5, parallel = FALSE
  )

  expect_equal(names(res$imp_data), names(df))
  expect_equal(res$imp_data$complete, df$complete)
  expect_false("complete" %in% res$best$column)
})

test_that("an integer seed makes the sweep reproducible", {
  skip_if_not_installed("missRanger")

  args <- list(
    toy_df(),
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, parallel = FALSE
  )
  one <- do.call(missranger_sweep_mtry, c(args, list(seed = 123)))
  two <- do.call(missranger_sweep_mtry, c(args, list(seed = 123)))

  expect_equal(one$imp_data, two$imp_data)
  expect_equal(one$oob_error, two$oob_error)
})

test_that("a seeded sweep does not depend on ambient RNG state", {
  skip_if_not_installed("missRanger")

  # missRanger seeds itself, so unlike missForest the result is fixed by
  # `seed` alone -- no L'Ecuyer stream management required.
  args <- list(
    toy_df(),
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, seed = 99, parallel = FALSE
  )
  set.seed(1)
  one <- do.call(missranger_sweep_mtry, args)
  set.seed(123456)
  two <- do.call(missranger_sweep_mtry, args)

  expect_equal(one$imp_data, two$imp_data)
})

test_that("the same seed gives the same answer under a different plan", {
  skip_if_not_installed("missRanger")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  pkg_dir <- find.package("TempleCBE", quiet = TRUE)
  skip_if_not(
    length(pkg_dir) == 1L &&
      file.exists(file.path(pkg_dir, "Meta", "package.rds")),
    "multisession workers cannot see a load_all()-ed package"
  )

  args <- list(
    toy_df(),
    exclude = c("id", "time"),
    num.trees = 50, maxiter = 2, seed = 2024L, parallel = TRUE
  )

  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  one <- do.call(missranger_sweep_mtry, args)

  future::plan(future::multisession, workers = 2)
  two <- do.call(missranger_sweep_mtry, args)

  expect_equal(one$imp_data, two$imp_data)
})

test_that("missranger_sweep_mtry validates its inputs", {
  expect_error(missranger_sweep_mtry(1:10), "must be a data frame")
  expect_error(
    missranger_sweep_mtry(data.frame(a = 1:3), exclude = "a"),
    "at least 2 columns"
  )
  expect_error(
    missranger_sweep_mtry(toy_df(), exclude = c("id", "time"), mtry_values = 99),
    "must be integers in"
  )
})

# --- engine parity ----------------------------------------------------------

test_that("both engines expose the same contract on the same data", {
  skip_if_not_installed("missRanger")
  skip_if_not_installed("missForest")

  df <- toy_df()
  ranger_res <- missranger_sweep_mtry(
    df, exclude = c("id", "time"), num.trees = 50, maxiter = 2,
    seed = 11, parallel = FALSE
  )
  forest_res <- missforest_sweep_mtry(
    df, exclude = c("id", "time"), ntree = 50, maxiter = 2,
    seed = 11, parallel = FALSE
  )

  # Same return shape, so the two are drop-in comparable on one data set.
  expect_named(ranger_res, names(forest_res))
  expect_equal(names(ranger_res$imp_data), names(forest_res$imp_data))
  expect_equal(
    sort(names(ranger_res$oob_error)),
    sort(names(forest_res$oob_error))
  )
  expect_false(anyNA(ranger_res$imp_data))
  expect_false(anyNA(forest_res$imp_data))

  # Error columns must NOT be compared across engines: missForest reports raw
  # MSE, missRanger reports scaled 1 - R^2. Guard the documented difference.
  expect_equal(unique(forest_res$oob_error$error_type), "MSE")
  expect_equal(unique(ranger_res$oob_error$error_type), "1-R2")
})

# --- the mtry bound ---------------------------------------------------------

test_that("missranger_max_mtry counts usable complete columns", {
  # Two complete columns -> bound 2.
  expect_equal(
    missranger_max_mtry(data.frame(a = c(1, NA, 3), b = 1:3, c = 4:6)),
    2L
  )
  # Every column has a gap: the first target falls back to univariate and the
  # second sees one predictor, so only mtry = 1 is admissible however wide
  # the frame is.
  expect_equal(missranger_max_mtry(all_gappy_df()), 1L)
  # Constant columns are not usable features and are not counted.
  expect_equal(
    missranger_max_mtry(data.frame(a = c(1, NA, 3), k = c(7, 7, 7), b = 1:3)),
    1L
  )
  expect_error(missranger_max_mtry(1:5), "must be a data frame")
})

test_that("the bound is lower than missForest's on the same data", {
  dt <- toy_df()[, -(1:2)]
  # missForest predicts each column from every other one.
  expect_equal(ncol(dt) - 1L, 5L)
  # missRanger builds its pool up over iteration 1, so it admits fewer.
  expect_equal(missranger_max_mtry(dt), 3L)
  expect_lt(missranger_max_mtry(dt), ncol(dt) - 1L)
})

test_that("an mtry above the bound is rejected with an explanation, not ranger's catch-all", {
  skip_if_not_installed("missRanger")

  # Exceeding the bound otherwise surfaces from ranger as
  # "User interrupt or internal error.", which names neither mtry nor why.
  expect_error(
    missranger_sweep_mtry(
      toy_df(),
      exclude = c("id", "time"),
      mtry_values = 5,
      num.trees = 20, maxiter = 2, parallel = FALSE
    ),
    "must be integers in 1:3"
  )
  expect_error(
    missranger_sweep_mtry(
      toy_df(),
      exclude = c("id", "time"),
      mtry_values = 5,
      num.trees = 20, maxiter = 2, parallel = FALSE
    ),
    "missranger_max_mtry"
  )
})

test_that("a frame where every column has gaps sweeps only mtry = 1", {
  skip_if_not_installed("missRanger")

  res <- missranger_sweep_mtry(
    all_gappy_df(), num.trees = 50, maxiter = 2, seed = 1, parallel = FALSE
  )
  expect_equal(unique(res$oob_error$mtry), 1L)
  expect_false(anyNA(res$imp_data))
})
