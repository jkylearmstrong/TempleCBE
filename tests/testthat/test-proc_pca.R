test_that("proc_pca extracts variance summary statistics from prcomp", {
  pca_res <- stats::prcomp(mtcars[, 1:4], scale. = TRUE)
  res <- proc_pca(pca_res)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(c("component", "eigenvalue", "variance_pct", "cum_variance_pct") %in% names(res)))
  expect_equal(res$component, paste0("PC", 1:4))
  expect_equal(max(res$cum_variance_pct), 100, tolerance = 1e-4)
})

test_that("proc_pca fits PCA itself when given raw data", {
  res_raw <- proc_pca(mtcars[, 1:4], scale = TRUE)
  res_fitted <- proc_pca(stats::prcomp(mtcars[, 1:4], center = TRUE, scale. = TRUE))

  expect_equal(res_raw, res_fitted)
})

test_that("proc_pca passes center/scale and ... through to prcomp", {
  res <- proc_pca(mtcars[, 1:4], center = FALSE, scale = FALSE)
  expected <- proc_pca(stats::prcomp(mtcars[, 1:4], center = FALSE, scale. = FALSE))

  expect_equal(res, expected)

  expect_error(proc_pca(list(a = 1)))
})
