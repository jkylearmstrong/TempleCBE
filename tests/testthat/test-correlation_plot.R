test_that("correlation_plot returns the correlation matrix invisibly and draws a plot", {
  # corrplot draws to the active graphics device; route it to a throwaway
  # file so the test doesn't pop up a device or write into the working dir.
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  res <- correlation_plot(mtcars, tl.cex = 0.7)

  expect_true(is.matrix(res))
  expect_equal(dim(res), c(ncol(mtcars), ncol(mtcars)))
  expect_equal(diag(res), rep(1, ncol(mtcars)), ignore_attr = TRUE)
})

test_that("correlation_plot only uses numeric columns", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  df <- data.frame(a = 1:10, b = 10:1, label = letters[1:10])
  res <- correlation_plot(df)

  expect_equal(rownames(res), c("a", "b"))
})

test_that("correlation_plot defaults still add coefficients and don't error", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  expect_no_error(res <- correlation_plot(mtcars))
  expect_true(is.matrix(res))
})

test_that("correlation_plot(show_coef = FALSE) doesn't error and still returns the matrix", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  expect_no_error(res <- correlation_plot(mtcars, show_coef = FALSE))
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(ncol(mtcars), ncol(mtcars)))
})

test_that("correlation_plot respects a custom mar without erroring", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  expect_no_error(correlation_plot(mtcars, mar = c(1, 1, 4, 1)))
})

test_that("correlation_plot_split groups variables via hierarchical clustering and returns one matrix per group", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  wide_data <- cbind(mtcars, iris[seq_len(nrow(mtcars)), sapply(iris, is.numeric)])
  n_vars <- sum(sapply(wide_data, is.numeric))

  res <- correlation_plot_split(wide_data, group_size = 6)

  expect_type(res, "list")
  expect_equal(length(res), ceiling(n_vars / 6))

  # Every variable appears in exactly one group, and each group's matrix is
  # square with matching row/col names (a within-group correlation matrix).
  all_vars <- unname(unlist(lapply(res, colnames)))
  expect_equal(sort(all_vars), sort(names(wide_data)[sapply(wide_data, is.numeric)]))
  expect_false(any(duplicated(all_vars)))

  for (mat in res) {
    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), ncol(mat))
    expect_equal(rownames(mat), colnames(mat))
  }
})

test_that("correlation_plot_split with a small data frame still produces a single group without erroring", {
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  res <- correlation_plot_split(mtcars, group_size = 12)

  expect_equal(length(res), 1)
  expect_equal(sort(colnames(res[[1]])), sort(names(mtcars)))
})

test_that("correlation_plot_split never produces a singleton group (regression test)", {
  # cutree(k = n_groups) can leave a lone variable in its own cluster -- a
  # 1x1 correlation matrix crashes corrplot's default order = "FPC" ordering
  # (eigen() has no second eigenvector to index). Reproduces the exact shape
  # that triggered it: 7 variables, group_size = 6 (ceiling(7/6) = 2 groups,
  # which cutree can split unevenly enough to leave a singleton).
  tmp_png <- tempfile(fileext = ".png")
  grDevices::png(tmp_png)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_png)
  }, add = TRUE)

  set.seed(42)
  df7 <- as.data.frame(matrix(stats::rnorm(7 * 30), ncol = 7))
  names(df7) <- paste0("v", 1:7)

  expect_no_error(res <- correlation_plot_split(df7, group_size = 6))
  expect_true(all(sapply(res, ncol) > 1))
})

test_that("correlation_plot() errors clearly on a single-column input instead of crashing in corrplot", {
  df1 <- data.frame(a = 1:10)
  expect_error(correlation_plot(df1), "at least 2 numeric columns")
})

test_that("merge_singleton_groups folds a singleton into its most-correlated group", {
  set.seed(1)
  cm <- stats::cor(matrix(stats::rnorm(7 * 20), ncol = 7,
                           dimnames = list(NULL, c("a", "b", "c", "d", "e", "f", "g"))))
  # Force "g" to be strongly correlated with "a" specifically, so the merge
  # target is unambiguous and verifiable.
  cm["g", "a"] <- cm["a", "g"] <- 0.99

  groups <- list("1" = c("a", "b", "c", "d", "e", "f"), "2" = "g")
  merged <- TempleCBE:::merge_singleton_groups(groups, cm)

  expect_equal(length(merged), 1)
  expect_true("g" %in% merged[[1]])
  expect_true(all(sapply(merged, length) > 1))
})

test_that("correlation_diff against itself is all zeros", {
  diff_df <- correlation_diff(mtcars, mtcars)

  expect_s3_class(diff_df, "tbl_df")
  expect_named(diff_df, c("var1", "var2", "diff"))
  expect_equal(nrow(diff_df), choose(ncol(mtcars), 2))
  expect_true(all(abs(diff_df$diff) < 1e-8))
})

test_that("correlation_diff only returns one triangle (no duplicate or diagonal rows)", {
  diff_df <- correlation_diff(mtcars, mtcars)

  pairs <- paste(diff_df$var1, diff_df$var2)
  reverse_pairs <- paste(diff_df$var2, diff_df$var1)
  expect_false(any(reverse_pairs %in% pairs))
  expect_false(any(diff_df$var1 == diff_df$var2))
})

test_that("correlation_diff matches variables by name and falls back to the intersection", {
  baseline <- mtcars
  comparison <- mtcars[, setdiff(names(mtcars), "carb")]

  diff_df <- correlation_diff(baseline, comparison)

  expect_false(any(diff_df$var1 == "carb" | diff_df$var2 == "carb"))
  expect_equal(nrow(diff_df), choose(ncol(mtcars) - 1, 2))
})

test_that("correlation_diff detects a real difference between datasets", {
  set.seed(1)
  baseline <- mtcars
  comparison <- mtcars
  comparison$hp <- rev(comparison$hp)

  diff_df <- correlation_diff(baseline, comparison)

  expect_true(any(abs(diff_df$diff) > 1e-8))
})

test_that("correlation_diff_heatmap returns a ggplot object", {
  p <- correlation_diff_heatmap(mtcars, mtcars)
  expect_s3_class(p, "ggplot")
})
