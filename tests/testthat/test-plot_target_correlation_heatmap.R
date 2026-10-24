test_that("plot_target_correlation_heatmap() shows one correlation per predictor", {
  p <- plot_target_correlation_heatmap(mtcars, "mpg", c("cyl", "disp", "hp", "wt", "qsec"), cutoff = 0.7)
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  d <- p$data
  expect_equal(nrow(d), 5L)
  expect_equal(unname(d$r), unname(vapply(as.character(d$predictor), function(v) cor(mtcars$mpg, mtcars[[v]]), 1)))
  # strongest first (top of the plot = last factor level)
  expect_equal(rev(levels(d$predictor))[1], "wt")
  expect_equal(d$faded, abs(d$r) < 0.7)
  expect_true(all(d$n == 32L))
})

test_that("sort_by_abs = FALSE keeps the given order; the target is ignored in predictors", {
  p <- plot_target_correlation_heatmap(mtcars, "mpg", c("qsec", "mpg", "wt"), sort_by_abs = FALSE)
  expect_equal(rev(levels(p$data$predictor)), c("qsec", "wt"))
})

test_that("pairwise missing data are handled and a constant predictor is blank", {
  d <- mtcars
  d$wt[1:10] <- NA
  d$k <- 1
  p <- plot_target_correlation_heatmap(d, "mpg", c("wt", "k"))
  expect_equal(p$data$n[p$data$predictor == "wt"], 22L)
  expect_true(is.na(p$data$r[p$data$predictor == "k"]))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("spearman and palette are accepted; bad input errors", {
  expect_s3_class(plot_target_correlation_heatmap(mtcars, "mpg", c("wt", "hp"), method = "spearman", palette = "PuOr"), "ggplot")
  expect_error(plot_target_correlation_heatmap(mtcars, "nope", "wt"), "one column")
  expect_error(plot_target_correlation_heatmap(mtcars, "mpg", "nope"), "not in `data`")
  expect_error(plot_target_correlation_heatmap(mtcars, "mpg", "mpg"), "at least one column other")
  expect_error(plot_target_correlation_heatmap(mtcars, "mpg", "wt", cutoff = 2), "between 0 and 1")
  expect_error(plot_target_correlation_heatmap(data.frame(a = 1:5, b = letters[1:5]), "a", "b"), "numeric")
})

test_that("predictors without a correlation sort last, not first", {
  d <- mtcars
  d$k <- 1
  p <- plot_target_correlation_heatmap(d, "mpg", c("k", "wt", "hp"))
  expect_equal(rev(levels(p$data$predictor)), c("wt", "hp", "k"))
})
