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

test_that("select_candidate_models supports by = 'row' with ntile and top_n", {
  tune_res <- tibble::tibble(
    alpha = c(0.1, 0.2, 0.5, 0.8, 1.0, 0.2, 0.5, 0.8, 1.0, 0.1),
    IBS = c(0.50, 0.12, 0.35, 0.40, 0.45, 0.10, 0.30, 0.38, 0.42, 0.55)
  )

  # by = "row" with ntile = 0.2 (top 20% lowest IBS rows: 0.10, 0.12, both alpha 0.2)
  sel_tile <- select_candidate_models(tune_res, "x1 + x2", ntile = 0.2, by = "row")
  expect_equal(sel_tile$alpha, 0.2)

  # by = "row" with top_n = 2 (top 2 distinct alphas: 0.2 and 0.5)
  sel_top <- select_candidate_models(tune_res, c("x1 + x2", "x1 + x3"), top_n = 2, by = "row")
  expect_setequal(unique(sel_top$alpha), c(0.2, 0.5))
  expect_equal(nrow(sel_top), 4)

  # by = "alpha" summarizes mean IBS per alpha
  sel_alpha <- select_candidate_models(tune_res, "x1 + x2", top_n = 2, by = "alpha")
  expect_setequal(unique(sel_alpha$alpha), c(0.2, 0.5))
})

test_that("score_outer handles multiple splits, coefficients options, tidy_outer_coefficients, and cv.glmnet", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  set.seed(42)
  long <- do.call(rbind, lapply(seq_len(40), function(i) {
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

  splits_list <- list(
    Fold1 = rsample::initial_split(long[1:30, ], prop = 0.7),
    Fold2 = rsample::initial_split(long[11:40, ], prop = 0.7)
  )

  grid <- data.frame(
    alpha = c(0.5),
    formula = c("x1 + x2 + x3"),
    stringsAsFactors = FALSE
  )

  # 1. Score multiple outer splits
  scored_all <- suppressWarnings(
    score_outer(
      splits_list,
      grid,
      recipe = rec,
      time_data = long[, c("tstart", "tstop")],
      internal_folds = 3,
      cox.ties = "breslow",
      coefficients = "all",
      seed = 101
    )
  )

  expect_s3_class(scored_all, "tbl_df")
  expect_equal(nrow(scored_all), 2)
  expect_true("outer_fold" %in% names(scored_all))
  expect_equal(scored_all$outer_fold, c("Fold1", "Fold2"))
  expect_true(is.list(scored_all$coefficients))
  # With coefficients = "all", each row's coef table contains all 3 terms
  expect_equal(nrow(scored_all$coefficients[[1]]), 3)
  expect_setequal(scored_all$coefficients[[1]]$term, c("x1", "x2", "x3"))

  # 2. Test tidy_outer_coefficients unnesting
  tidied <- tidy_outer_coefficients(scored_all)
  expect_s3_class(tidied, "tbl_df")
  expect_equal(nrow(tidied), 6) # 2 folds * 3 terms
  expect_named(tidied, c("outer_fold", "model", "alpha", "formula", "IBS", "lambda", "term", "estimate"))
  expect_equal(tidy_outer_coefficients(scored_all), unnest_outer_coefficients(scored_all))

  # 3. Test coefficients = "nonzero"
  scored_nz <- suppressWarnings(
    score_outer(
      splits_list[[1]],
      grid,
      recipe = rec,
      time_data = long[, c("tstart", "tstop")],
      internal_folds = 3,
      cox.ties = "breslow",
      coefficients = "nonzero",
      seed = 101
    )
  )
  expect_true("outer_fold" %in% names(scored_nz))
  expect_true(all(scored_nz$coefficients[[1]]$estimate != 0))

  # 4. Test cv.glmnet legacy tuning engine
  scored_legacy <- suppressWarnings(
    glmnet_IBS(
      splits_list[[1]],
      alpha = 0.5,
      recipe = rec,
      feature_names = c("x1", "x2", "x3"),
      time_data = long[, c("tstart", "tstop")],
      internal_folds = 3,
      cox.ties = "breslow",
      tune_engine = "cv.glmnet"
    )
  )
  expect_s3_class(scored_legacy, "tbl_df")
  expect_true(!is.na(scored_legacy$IBS[1]))
  expect_true(!is.na(scored_legacy$lambda[1]))
  expect_equal(nrow(scored_legacy), 3)
})
