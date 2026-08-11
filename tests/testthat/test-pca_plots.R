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

test_that("plot.prcomp dispatches to the right underlying plot for each type", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  mtcars2 <- tibble::rownames_to_column(mtcars, "model")

  expect_s3_class(plot(pca_model, type = "variance"), "ggplot")
  expect_s3_class(plot(pca_model, type = "heatmap"), "ggplot")
  expect_s3_class(plot(pca_model, type = "bi", newdata = mtcars2, column = "model"), "ggplot")
})
