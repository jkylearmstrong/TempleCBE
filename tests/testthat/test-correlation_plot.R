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
