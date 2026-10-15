test_that("select_candidate_models creates correct outer grid", {
  tune_res <- tibble::tibble(
    alpha = rep(c(0, 0.2, 0.5, 0.8, 1.0), each = 4),
    IBS = c(
      0.30, 0.31, 0.29, 0.32,  # alpha 0: mean 0.305
      0.20, 0.21, 0.19, 0.22,  # alpha 0.2: mean 0.205 (best)
      0.24, 0.25, 0.23, 0.26,  # alpha 0.5: mean 0.245 (second)
      0.35, 0.36, 0.34, 0.37,  # alpha 0.8: mean 0.355
      0.40, 0.41, 0.39, 0.42   # alpha 1: mean 0.405
    )
  )

  forms <- c("x1 + x2", "x1 + x3")
  grid <- select_candidate_models(tune_res, forms, top_n = 2)

  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 4) # 2 alphas * 2 formulas
  expect_named(grid, c("model", "alpha", "formula"))
  expect_setequal(unique(grid$alpha), c(0.2, 0.5))
  expect_setequal(unique(grid$formula), forms)
})

test_that("compare_censoring_weights works on precomputed tables", {
  tbl_none <- tibble::tibble(
    model = c("M1", "M2", "M3", "M4"),
    alpha = c(0.1, 0.2, 0.5, 0.8),
    formula = "x1 + x2",
    IBS = c(0.20, 0.15, 0.25, 0.10)
  )
  tbl_ipcw <- tibble::tibble(
    model = c("M1", "M2", "M3", "M4"),
    alpha = c(0.1, 0.2, 0.5, 0.8),
    formula = "x1 + x2",
    IBS = c(0.22, 0.16, 0.24, 0.11)
  )

  comp <- compare_censoring_weights(tbl_none, tbl_ipcw)
  expect_s3_class(comp, "tbl_df")
  expect_named(comp, c("model", "alpha", "formula", "ibs_none", "ibs_ipcw", "rank_none", "rank_ipcw", "rank_diff"))
  expect_equal(comp$ibs_none, tbl_none$IBS)
  expect_equal(comp$ibs_ipcw, tbl_ipcw$IBS)
  expect_equal(comp$rank_none, c(3, 2, 4, 1))
  expect_equal(comp$rank_ipcw, c(3, 2, 4, 1))
  expect_equal(comp$rank_diff, c(0, 0, 0, 0))

  agree <- attr(comp, "rank_agreement")
  expect_type(agree, "list")
  expect_equal(agree$spearman, 1)
  expect_equal(agree$kendall, 1)
})

test_that("score_outer and compare_censoring_weights run on rsplit", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  set.seed(1)
  long <- do.call(rbind, lapply(seq_len(30), function(i) {
    data.frame(
      id = i,
      tstart = 0,
      tstop = stats::runif(1, 5, 40),
      status = stats::rbinom(1, 1, 0.5),
      x1 = stats::rnorm(1),
      x2 = stats::rnorm(1),
      x3 = stats::rnorm(1)
    )
  }))
  rec <- recipes::recipe(~ ., data = long)
  rec <- recipes::update_role(rec, "id", "tstart", "tstop", "status", new_role = "id variable")
  split <- rsample::initial_split(long, prop = 0.7)

  grid <- data.frame(
    alpha = c(0.2, 0.8),
    formula = c("x1 + x2", "x1 + x3"),
    stringsAsFactors = FALSE
  )

  comp <- suppressWarnings(
    compare_censoring_weights(
      split,
      grid,
      recipe = rec,
      time_data = long[, c("tstart", "tstop")],
      internal_folds = 3,
      cox.ties = "breslow",
      seed = 42
    )
  )

  expect_s3_class(comp, "tbl_df")
  expect_equal(nrow(comp), 2)
  expect_true(all(!is.na(comp$ibs_none)))
  expect_true(all(!is.na(comp$ibs_ipcw)))
  expect_true(all(!is.na(comp$rank_none)))
  expect_true(all(!is.na(comp$rank_ipcw)))

  agree <- attr(comp, "rank_agreement")
  expect_true(!is.null(agree$spearman))
  expect_true(!is.null(agree$kendall))
})
