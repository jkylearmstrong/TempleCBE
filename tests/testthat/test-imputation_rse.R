cells <- function() {
  set.seed(1)
  truth <- rnorm(50, 10, 2)
  rbind(
    data.frame(variable = "x", method = "mean", truth = truth, imputed = mean(truth)),
    data.frame(variable = "x", method = "good", truth = truth, imputed = truth + rnorm(50, 0, 0.5)),
    data.frame(variable = "y", method = "mean", truth = truth, imputed = mean(truth)),
    data.frame(variable = "y", method = "bad", truth = truth, imputed = truth + 5)
  )
}

test_that("compare_imputation_rse() computes RMSE, null RMSE and RSE = (RMSE / null RMSE)^2", {
  d <- cells()
  r <- compare_imputation_rse(d)
  expect_s3_class(r, "cbe_rse")
  expect_named(r, c("variable", "method", "n", "n_dropped", "rmse", "null_rmse", "rse"))
  tr <- d$truth[d$variable == "x" & d$method == "mean"]
  null <- sqrt(mean((tr - mean(tr))^2))
  row <- r[r$variable == "x" & r$method == "mean", ]
  expect_equal(row$null_rmse, null)
  expect_equal(row$rse, 1) # the mean method is the null baseline
  good <- r[r$variable == "x" & r$method == "good", ]
  expect_equal(good$rse, (good$rmse / good$null_rmse)^2)
  expect_lt(good$rse, 0.2)
  expect_gt(r$rse[r$method == "bad"], 1)
  expect_equal(r$method[r$variable == "x"], c("good", "mean"))
})

test_that("null_method = 'median' and an explicit baseline change the null RMSE", {
  d <- cells()
  m <- compare_imputation_rse(d, "mean")
  md <- compare_imputation_rse(d, "median")
  expect_false(isTRUE(all.equal(m$null_rmse, md$null_rmse)))
  b <- compare_imputation_rse(d, baseline = c(x = 0, y = 0))
  tr <- d$truth[d$variable == "x" & d$method == "mean"]
  expect_equal(b$null_rmse[b$variable == "x"][1], sqrt(mean(tr^2)))
  expect_error(compare_imputation_rse(d, baseline = c(x = 0)), "no value for: y")
  expect_error(compare_imputation_rse(d, baseline = 5), "named numeric")
})

test_that("missing cells are dropped and counted; zero null RMSE gives NA", {
  d <- cells()
  d$imputed[1:3] <- NA
  r <- compare_imputation_rse(d)
  expect_equal(r$n_dropped[r$variable == "x" & r$method == "mean"], 3L)
  const <- data.frame(variable = "z", method = "m", truth = rep(2, 5), imputed = 3)
  expect_true(is.na(compare_imputation_rse(const)$rse))
})

test_that("pre-aggregated rmse / null_rmse tables are accepted; bad input errors", {
  agg <- data.frame(variable = "x", method = "a", rmse = 1, null_rmse = 2)
  expect_equal(compare_imputation_rse(agg)$rse, 0.25)
  expect_error(compare_imputation_rse(1), "data frame")
  expect_error(compare_imputation_rse(data.frame(variable = "x", method = "a")), "lacks column")
  expect_error(compare_imputation_rse(data.frame(variable = "x", method = "a", truth = "u", imputed = "v")), "numeric")
})

test_that("tbl_best_imputation_method() picks the lowest RSE per variable", {
  rse <- data.frame(variable = c("x", "x", "y", "y", "w"), method = c("mean", "knn", "mean", "knn", "mean"),
                    rse = c(1, 0.4, 0.9, 1.2, NA))
  b <- tbl_best_imputation_method(rse)
  expect_equal(b$variable, c("x", "y", "w"))
  expect_equal(b$best_method, c("knn", "mean", NA))
  expect_equal(b$rse[1:2], c(0.4, 0.9))
  expect_equal(b$runner_up[1:2], c("mean", "knn"))
  expect_equal(b$beats_null[1:2], c(TRUE, TRUE))
  expect_equal(b$n_methods, c(2L, 2L, 0L))
  tie <- data.frame(variable = c("x", "x"), method = c("b", "a"), rse = c(0.5, 0.5))
  expect_equal(tbl_best_imputation_method(tie)$best_method, "a")
  expect_error(tbl_best_imputation_method(data.frame(a = 1)), "must have columns")
})

test_that("build_best_imputed_data() takes each column from its winning method", {
  orig <- data.frame(x = c(1, NA, 3), y = c(NA, 2, 3), z = c(1, 2, 3))
  imps <- list(mean = data.frame(x = c(1, 2, 3), y = c(2.5, 2, 3), z = 1:3),
               knn = data.frame(x = c(1, 9, 3), y = c(7, 2, 3), z = 1:3))
  best <- data.frame(variable = c("x", "y"), best_method = c("mean", "knn"), beats_null = c(TRUE, FALSE))
  out <- build_best_imputed_data(orig, imps, best)
  expect_equal(out$x, c(1, 2, 3))
  expect_equal(out$y, c(7, 2, 3))
  expect_equal(out$z, c(1, 2, 3))
  expect_equal(attr(out, "imputation_source")$method, c("mean", "knn"))
  out2 <- build_best_imputed_data(orig, imps, best, require_beats_null = TRUE)
  expect_true(is.na(out2$y[1]))
  out3 <- build_best_imputed_data(orig, imps, best, require_beats_null = TRUE, fallback = "mean")
  expect_equal(out3$y[1], 2.5)
  expect_error(build_best_imputed_data(orig, list(data.frame(x = 1)), best), "named list")
  expect_error(build_best_imputed_data(orig, imps, best, fallback = "nope"), "not in `imputations`")
  expect_error(build_best_imputed_data(orig, list(mean = imps$mean[1:2, ], knn = imps$knn), best), "same number of rows")
})

test_that("RSE plots build", {
  rse <- compare_imputation_rse(cells())
  p <- plot_rse_by_column(rse)
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_s3_class(plot_rse_by_column(rse, "RMSE"), "ggplot")
  expect_s3_class(plot_rse_by_column(rse, log_scale = TRUE), "ggplot")
  expect_error(plot_rse_by_column(rse[, c("variable", "method", "rse")], "RMSE"), "no `rmse`")
  dat <- data.frame(x = c(1, NA, 3, NA), y = c(1, 2, 3, 4))
  pm <- plot_rse_vs_missingness(dat, rse)
  expect_s3_class(pm, "ggplot")
  expect_setequal(unique(pm$data$pct_miss), c(50, 0))
  expect_warning(plot_rse_vs_missingness(dat["x"], rse), "not in `data`")
})

test_that("all methods of a variable share one baseline, so their RSEs are comparable", {
  set.seed(2)
  truth <- rnorm(40, 10, 2)
  a <- data.frame(variable = "x", method = "a", truth = truth, imputed = mean(truth))
  b <- data.frame(variable = "x", method = "b", truth = truth[1:30], imputed = mean(truth))
  expect_warning(r <- compare_imputation_rse(rbind(a, b)), "different numbers of cells")
  # same constant for both methods
  base <- mean(c(truth, truth[1:30])) # every row of x counts once
  expect_equal(r$null_rmse[r$method == "a"], sqrt(mean((truth - base)^2)))
  expect_equal(r$null_rmse[r$method == "b"], sqrt(mean((truth[1:30] - base)^2)))
  expect_warning(compare_imputation_rse(rbind(a, transform(a, method = "c"))), NA)
})

test_that("require_beats_null needs a beats_null column", {
  orig <- data.frame(x = c(1, NA, 3))
  best <- data.frame(variable = "x", best_method = "m")
  expect_error(build_best_imputed_data(orig, list(m = data.frame(x = 1:3)), best, require_beats_null = TRUE), "beats_null")
})
