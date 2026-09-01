test_that("pca_feature_loading_heatmap returns a ggplot object", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  p <- pca_feature_loading_heatmap(pca_model)
  expect_s3_class(p, "ggplot")
})

test_that("plot_pca_bi returns a ggplot object for a multi-component model", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  mtcars2 <- tibble::rownames_to_column(mtcars, "model")
  p <- plot_pca_bi(pca_model, mtcars2, column = "model")
  expect_s3_class(p, "ggplot")
})

test_that("plot_pca_bi errors clearly on a single-component model instead of plotting PC1 vs PC1", {
  # Regression test: the x == y collision fix (`y <- if (x == n_components) 1
  # else x + 1`) doesn't actually resolve anything when n_components == 1 --
  # y still ends up equal to x, silently producing a degenerate PC1-vs-PC1
  # biplot. A model with only 1 component should error instead.
  pca_model <- stats::prcomp(data.frame(v1 = 1:10))
  df <- data.frame(v1 = 1:10, id = letters[1:10])

  expect_error(plot_pca_bi(pca_model, df, column = "id"), "at least 2 principal components")
})

test_that("plot_pca_bi resolves an x == y request by picking a different component", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  mtcars2 <- tibble::rownames_to_column(mtcars, "model")
  expect_message(plot_pca_bi(pca_model, mtcars2, column = "model", x = 1, y = 1), "y = 2")
})

test_that("rotation_matrix / pca_loadings return a tibble of feature loadings with feature_num labels", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  rot <- rotation_matrix(pca_model)

  expect_s3_class(rot, "tbl_df")
  expect_true(all(c("feature", "feature_num") %in% names(rot)))
  expect_equal(nrow(rot), ncol(mtcars))
  expect_identical(pca_loadings(pca_model), rot)
})

test_that("pca_eqns returns component equations and a feature-number label key", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  res <- pca_eqns(pca_model)

  expect_named(res, c("eqns", "labels"))
  expect_equal(nrow(res$eqns), ncol(mtcars))
  expect_equal(nrow(res$labels), ncol(mtcars))
  expect_true(all(grepl("^PC\\d+= ", res$eqns$PC)))
})

test_that("pca_percent_var_explained returns a ggplot object", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  p <- pca_percent_var_explained(pca_model)
  expect_s3_class(p, "ggplot")
})

test_that("pca_percent_var_explained's y scale keeps 10% breaks and a tightened top expansion", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  p <- pca_percent_var_explained(pca_model)

  y_scale <- p$scales$get_scales("y")
  expect_equal(y_scale$breaks, seq(0, 1, 0.1))
  expect_equal(y_scale$expand, ggplot2::expansion(mult = c(0, 0.01)))
})

test_that("plot.prcomp dispatches to the right underlying plot for each type", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  mtcars2 <- tibble::rownames_to_column(mtcars, "model")

  expect_s3_class(plot(pca_model, type = "variance"), "ggplot")
  expect_s3_class(plot(pca_model, type = "heatmap"), "ggplot")
  expect_s3_class(plot(pca_model, type = "bi", newdata = mtcars2, column = "model"), "ggplot")
  expect_s3_class(plot(pca_model, type = "biplot"), "ggplot")
})

test_that("pca_biplot returns a ggplot object with PC axis labels", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  p <- pca_biplot(pca_model, x = 1, y = 2)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "PC1")
  expect_equal(p$labels$y, "PC2")
})

test_that("pca_biplot respects the requested (x, y) components in axis labels", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  p <- pca_biplot(pca_model, x = 2, y = 3)

  expect_equal(p$labels$x, "PC2")
  expect_equal(p$labels$y, "PC3")
})

test_that("pca_biplot errors clearly when a requested component doesn't exist", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  expect_error(pca_biplot(pca_model, x = 1, y = 99), "pca_biplot\\(\\) requires components up to")
})

test_that("pca_loading_diff sign-aligns components before differencing", {
  pca_baseline <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)

  # Build a synthetic "comparison" fit that is identical to the baseline
  # except PC1's loadings (and matching scores) are negated -- the same
  # sign-flip that PCA leaves arbitrary between independent fits. If sign
  # alignment works, PC1's diff should be ~0 (not ~2x the loading).
  pca_comparison <- pca_baseline
  pca_comparison$rotation[, 1] <- -pca_comparison$rotation[, 1]
  pca_comparison$x[, 1] <- -pca_comparison$x[, 1]

  diff <- pca_loading_diff(pca_baseline, pca_comparison)

  expect_s3_class(diff, "tbl_df")
  expect_true("feature" %in% names(diff))
  expect_equal(nrow(diff), ncol(mtcars))
  expect_true(all(abs(diff$PC1) < 1e-8))

  # Other components were untouched, so their diffs should also be ~0.
  other_pc_cols <- setdiff(names(diff), c("feature", "PC1"))
  for (col in other_pc_cols) {
    expect_true(all(abs(diff[[col]]) < 1e-8))
  }
})

test_that("pca_loading_diff matches variables by name and doesn't error on differing variable sets", {
  pca_baseline <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  pca_comparison <- stats::prcomp(mtcars[, setdiff(names(mtcars), "carb")], center = TRUE, scale. = TRUE)

  diff <- pca_loading_diff(pca_baseline, pca_comparison)

  expect_false("carb" %in% diff$feature)
  expect_equal(nrow(diff), ncol(mtcars) - 1)
})

test_that("pca_loading_diff respects n_components", {
  pca_baseline <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  pca_comparison <- stats::prcomp(mtcars[sample(nrow(mtcars)), ], center = TRUE, scale. = TRUE)

  diff <- pca_loading_diff(pca_baseline, pca_comparison, n_components = 2)

  expect_equal(sort(setdiff(names(diff), "feature")), c("PC1", "PC2"))
})

test_that("pca_loading_diff_heatmap returns a ggplot object", {
  pca_baseline <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  pca_comparison <- stats::prcomp(mtcars[sample(nrow(mtcars)), ], center = TRUE, scale. = TRUE)

  p <- pca_loading_diff_heatmap(pca_baseline, pca_comparison)
  expect_s3_class(p, "ggplot")
})
