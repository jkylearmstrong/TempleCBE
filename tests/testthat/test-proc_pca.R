test_that("proc_pca extracts variance summary statistics from prcomp", {
  pca_res <- stats::prcomp(mtcars[, 1:4], scale. = TRUE)
  res <- proc_pca(pca_res)
  
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(c("component", "eigenvalue", "variance_pct", "cum_variance_pct") %in% names(res)))
  expect_equal(res$component, paste0("PC", 1:4))
  expect_equal(max(res$cum_variance_pct), 100, tolerance = 1e-4)

  # Test error handling
  expect_error(proc_pca(list(a = 1)), "must be a 'prcomp' object")
})
