# plot_feature_transform_heatmap, plot_scatter_glm, plot_normalization_comparison

heat_df <- function() {
  d <- expand.grid(feature = c("a", "b", "c"), transform_method = c("Raw", "SD"), stringsAsFactors = FALSE)
  d$p.value <- c(0.0004, 0.03, 0.4, 0.2, 0.008, NA)
  d
}

test_that("the heatmap buckets p-values with strict breaks and ranks within each transformation", {
  p <- plot_feature_transform_heatmap(heat_df())
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  d <- p$data
  b <- function(f, t) as.character(d$.bucket[d$feature == f & d$transform_method == t])
  expect_match(b("a", "Raw"), "p < 0.001")
  expect_match(b("b", "Raw"), "p < 0.05")
  expect_match(b("c", "Raw"), "NS")
  expect_match(b("a", "SD"), "NS") # p = 0.2 is not below the last break
  expect_match(b("b", "SD"), "p < 0.01")
  expect_match(b("c", "SD"), "NS") # missing
  expect_equal(d$.rank[d$feature == "a" & d$transform_method == "Raw"], 1L)
  expect_equal(d$.rank[d$feature == "c" & d$transform_method == "Raw"], 3L)
  expect_equal(d$.rank[d$feature == "b" & d$transform_method == "SD"], 1L)
  expect_true(is.na(d$.rank[d$feature == "c" & d$transform_method == "SD"]))
})

test_that("a rank column is used when given; ordering, palette and input are validated", {
  d <- heat_df()
  d$rank <- 3:1
  p <- plot_feature_transform_heatmap(d, rank_col = "rank")
  expect_equal(p$data$.rank[1:3], 3:1)
  expect_s3_class(plot_feature_transform_heatmap(heat_df(), rank_col = NULL, order_features = "best_p"), "ggplot")
  expect_error(plot_feature_transform_heatmap(heat_df(), palette = c("red", "blue")), "one more colour")
  expect_error(plot_feature_transform_heatmap(heat_df(), p_breaks = c(0.5, 0.1)), "increasing")
  expect_error(plot_feature_transform_heatmap(heat_df()[, c("feature", "p.value")]), "lacks column")
  expect_error(plot_feature_transform_heatmap(rbind(heat_df(), heat_df()[1, ])), "more than one row")
})

test_that("plot_scatter_glm() reports Pearson's r, p and the line", {
  p <- plot_scatter_glm(mtcars, "wt", "mpg")
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  s <- attr(p, "stats")
  ct <- cor.test(mtcars$wt, mtcars$mpg)
  expect_equal(s$r, unname(ct$estimate))
  expect_equal(s$slope, unname(coef(lm(mpg ~ wt, mtcars))["wt"]))
  expect_equal(s$intercept, unname(coef(lm(mpg ~ wt, mtcars))["(Intercept)"]))
  expect_match(s$sentence, "^Pearson's r of -0\\.868, with p-value of <0\\.001\\. The regression line is given by y = -5\\.34\\*x \\+ 37\\.3\\.$")
  expect_equal(p$labels$caption, s$sentence)
})

test_that("plot_scatter_glm() handles facets, missing values, labels and degenerate data", {
  d <- mtcars
  d$wt[1:3] <- NA
  p <- plot_scatter_glm(d, "wt", "mpg", facet_by = "am", x_label = "Weight", annotate_stats = TRUE)
  s <- attr(p, "stats")
  expect_equal(s$panel, c("0", "1"))
  expect_equal(sum(s$n), 29L)
  expect_match(p$labels$caption, "0: Pearson")
  expect_equal(p$labels$x, "Weight")
  nocap <- plot_scatter_glm(mtcars, "wt", "mpg", annotate_stats = FALSE)
  expect_null(nocap$labels$caption)
  flat <- plot_scatter_glm(data.frame(a = 1:5, b = 3), "a", "b")
  expect_match(attr(flat, "stats")$sentence, "Not enough variation")
  expect_error(plot_scatter_glm(mtcars, "nope", "mpg"), "`x`")
  expect_error(plot_scatter_glm(data.frame(a = "x", b = 1), "a", "b"), "numeric")
  expect_error(plot_scatter_glm(mtcars, "wt", "mpg", facet_by = "zz"), "facet_by")
  expect_error(plot_scatter_glm(data.frame(a = NA_real_, b = 1), "a", "b"), "No complete pairs")
})

test_that("plot_normalization_comparison() draws two panels and validates input", {
  skip_if_not_installed("patchwork")
  x <- mtcars$hp
  p <- plot_normalization_comparison(x, min_max_norm(x), "Horsepower", "hp")
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1L) # one plot plus the first of the wrap
  expect_s3_class(plot_normalization_comparison(x, min_max_norm(x), "Horsepower", type = "histogram", bins = 10), "patchwork")
  expect_error(plot_normalization_comparison(x, x[-1], "v"), "same length")
  expect_error(plot_normalization_comparison("a", "b", "v"), "numeric")
  expect_error(plot_normalization_comparison(c(1, NA), c(NA, 1), "v"), "two complete pairs")
  expect_error(plot_normalization_comparison(x, x, c("a", "b")), "single string")
})

test_that("the heatmap legend labels are not padded and use the real break values", {
  p <- plot_feature_transform_heatmap(heat_df())
  expect_equal(levels(p$data$.bucket), c("p < 0.001", "p < 0.01", "p < 0.05", "p < 0.2", "NS (p \u2265 0.2)"))
})

test_that("plot_scatter_glm() draws a line only in panels that have statistics", {
  d <- data.frame(x = c(1, 2, 3, 4, 5, 1, 2), y = c(2, 4, 5, 8, 10, 3, 7), g = c(rep("big", 5), rep("tiny", 2)))
  p <- plot_scatter_glm(d, "x", "y", facet_by = "g")
  smooth <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1)))]]
  expect_equal(unique(smooth$data$g), "big")
  expect_no_warning(ggplot2::ggplot_build(p))
})
