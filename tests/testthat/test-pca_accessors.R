fit <- stats::prcomp(mtcars[, 1:5], scale. = TRUE)
rp <- run_pca(mtcars[, 1:5], scale = TRUE)

test_that("pca_loading() returns the signed rotation element for prcomp and run_pca objects", {
  expect_equal(pca_loading(fit, "mpg", 1), unname(fit$rotation["mpg", 1]))
  expect_equal(pca_loading(rp, "disp", "PC2"), unname(fit$rotation["disp", 2]))
  expect_length(pca_loading(fit, "mpg", 1), 1L)
})

test_that("pca_loading() errors clearly on bad input", {
  expect_error(pca_loading(fit, "nope", 1), "not in the PCA.*mpg")
  expect_error(pca_loading(fit, "mpg", 9), "between 1 and 5")
  expect_error(pca_loading(fit, "mpg", 0), "between 1 and 5")
  expect_error(pca_loading(fit, "mpg", "second"), "component number")
  expect_error(pca_loading(fit, "mpg", c(1, 2)), "single component")
  expect_error(pca_loading(fit, c("mpg", "cyl"), 1), "one variable name")
  expect_error(pca_loading(lm(mpg ~ cyl, mtcars), "cyl", 1), "prcomp")
})

test_that("pca_variance_pct() is on a 0-100 scale and matches run_pca()", {
  expect_equal(pca_variance_pct(rp, 1:5), rp$variance_pct)
  expect_equal(sum(pca_variance_pct(fit, 1:5)), 100)
  expect_equal(pca_variance_pct(fit, "PC2"), rp$variance_pct[2])
  expect_equal(pca_variance_pct(rp, 3, cumulative = TRUE), rp$cum_variance_pct[3])
  expect_equal(pca_variance_pct(rp, c(1, 3), cumulative = TRUE), rp$cum_variance_pct[c(1, 3)])
  expect_error(pca_variance_pct(fit, 6), "between 1 and 5")
})

test_that("pca_top_features() orders by absolute loading and keeps the sign", {
  top <- pca_top_features(fit, 1, n = 3)
  expect_equal(nrow(top), 3L)
  expect_named(top, c("feature", "loading", "abs_loading", "rank"))
  expect_true(all(diff(top$abs_loading) <= 0))
  expect_equal(top$loading, unname(fit$rotation[top$feature, 1]))
  expect_equal(top$rank, 1:3)
  expect_equal(nrow(pca_top_features(fit, 1, n = 50)), 5L)
})

test_that("pca_top_features(by =) ranks one sign only", {
  pos <- pca_top_features(fit, 1, n = 5, by = "positive")
  neg <- pca_top_features(fit, 1, n = 5, by = "negative")
  expect_true(all(pos$loading > 0))
  expect_true(all(neg$loading < 0))
  expect_true(all(diff(pos$loading) <= 0))
  expect_true(all(diff(neg$loading) >= 0))
  expect_equal(nrow(pos) + nrow(neg), 5L)
  expect_error(pca_top_features(fit, 1, n = 0), "positive whole number")
  expect_error(pca_top_features(fit, 1, by = "up"))
})

test_that("unnamed variables get the V1.. names run_pca() gives them", {
  m <- stats::prcomp(unname(as.matrix(mtcars[, 1:3])), scale. = TRUE)
  expect_equal(pca_loading(m, "V2", 1), unname(m$rotation[2, 1]))
})
