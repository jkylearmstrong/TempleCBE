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

test_that("pca_plot dispatches to the right underlying plot for each type", {
  pca_model <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  mtcars2 <- tibble::rownames_to_column(mtcars, "model")

  expect_s3_class(pca_plot(pca_model, type = "variance"), "ggplot")
  expect_s3_class(pca_plot(pca_model, type = "heatmap"), "ggplot")
  expect_s3_class(pca_plot(pca_model, type = "bi", newdata = mtcars2, column = "model"), "ggplot")
  expect_equal(pca_plot(pca_model, type = "biplot", x = 2, y = 3)$labels$x, "PC2")
  expect_error(pca_plot(mtcars), "prcomp object")
})

test_that("TempleCBE does not replace stats' plot() method for prcomp", {
  expect_false("plot.prcomp" %in% getNamespaceExports("TempleCBE"))
  method <- utils::getS3method("plot", "prcomp")
  expect_identical(environmentName(environment(method)), "stats")
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

# --- pca_variables_plot: scaling (audit A3-04) and truncated fits (A3-05) -------

# The arrows of a pca_variables_plot(): coordinates (PC1, PC2, ...) by feature.
variables_arrows <- function(p) {
  segments <- Filter(function(layer) inherits(layer$geom, "GeomSegment"), p$layers)
  segments[[1]]$data
}

test_that("pca_variables_plot warns, naming the remedy, when the PCA was not scaled", {
  # loading x sdev is a correlation only for standardised variables; for
  # prcomp()'s default scale. = FALSE the arrows are covariances (up to 1.76 on
  # iris) drawn inside a unit circle.
  unscaled <- stats::prcomp(iris[, 1:4])

  expect_warning(pca_variables_plot(unscaled), "not scaled")
  expect_warning(pca_variables_plot(unscaled), "prcomp\\(scale\\. = TRUE\\)")
  expect_warning(
    pca_variables_plot(proc_pca(iris[, 1:4], scale = FALSE)),
    "prcomp\\(scale\\. = TRUE\\)"
  )
  expect_warning(pca_plot(unscaled, type = "circle"), "prcomp\\(scale\\. = TRUE\\)")

  # A standardised fit is what the plot is for: no warning.
  expect_no_warning(pca_variables_plot(stats::prcomp(iris[, 1:4], scale. = TRUE)))
  expect_no_warning(pca_variables_plot(proc_pca(iris[, 1:4])))
})

test_that("pca_variables_plot(data =) plots correlations whatever the scaling of the fit", {
  X <- iris[, 1:4]
  unscaled <- stats::prcomp(X)

  p <- expect_no_warning(pca_variables_plot(unscaled, data = X))
  arrows <- variables_arrows(p)
  expect_equal(
    unname(as.matrix(arrows[c("PC1", "PC2")])),
    unname(stats::cor(X, unscaled$x)[, 1:2])
  )
  expect_true(all(abs(as.matrix(arrows[c("PC1", "PC2")])) <= 1))
  expect_equal(arrows$feature, names(X))

  # For a standardised fit loading x sdev already is the correlation, so the
  # two ways of drawing the plot agree; a matrix works as well as a data frame.
  scaled <- stats::prcomp(X, scale. = TRUE)
  expect_equal(
    variables_arrows(pca_variables_plot(scaled, data = X)),
    variables_arrows(pca_variables_plot(scaled))
  )
  expect_equal(
    variables_arrows(pca_variables_plot(scaled, data = as.matrix(X))),
    variables_arrows(pca_variables_plot(scaled))
  )

  expect_error(pca_variables_plot(scaled, data = X[, 1:3]), "missing: Petal.Width")
  expect_error(
    pca_variables_plot(scaled, data = data.frame(lapply(X, as.character))),
    "must be numeric"
  )
})

test_that("pca_variables_plot works on rank- and tol-truncated prcomp fits", {
  X <- iris[, 1:4]
  fits <- list(
    rank = stats::prcomp(X, scale. = TRUE, rank. = 2),
    tol = stats::prcomp(X, scale. = TRUE, tol = 0.3)
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    # the set-up: every sdev is kept, but only two components are
    expect_equal(c(length(fit$sdev), ncol(fit$rotation)), c(4L, 2L), label = nm)

    p <- pca_variables_plot(fit)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
    expect_equal(
      unname(as.matrix(variables_arrows(p)[c("PC1", "PC2")])),
      unname(stats::cor(X, fit$x)),
      label = nm
    )
    # the axis percentages are shares of the total variance
    expect_equal(
      p$labels$x,
      sprintf("PC1 (%s%%)", round(fit$sdev[1]^2 / sum(fit$sdev^2) * 100, 1)),
      label = nm
    )
  }
  expect_s3_class(
    pca_variables_plot(fits$rank, data = X),
    "ggplot"
  )
})

test_that("pca_percent_var_explained works on rank- and tol-truncated prcomp fits", {
  X <- iris[, 1:4]
  full <- stats::prcomp(X, scale. = TRUE)
  fits <- list(
    rank = stats::prcomp(X, scale. = TRUE, rank. = 2),
    tol = stats::prcomp(X, scale. = TRUE, tol = 0.3)
  )
  for (nm in names(fits)) {
    p <- pca_percent_var_explained(fits[[nm]])
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))

    # shares of the total variance, as for the untruncated fit
    d <- p$data
    expect_equal(d$percent[d$variance == "percent"], full$sdev^2 / sum(full$sdev^2), label = nm)
    expect_equal(d$percent[d$variance == "cumulative"], cumsum(full$sdev^2) / sum(full$sdev^2), label = nm)
  }
})

test_that("pca_percent_var_explained reports what broom's eigenvalue table does for a full fit", {
  # Guards the switch from broom::tidy() to the sdev-based table: results for
  # a fit that already worked do not change. broom rounds the shares to five
  # decimals, so the plotted values agree to within 1e-5 rather than exactly.
  fit <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  eig <- broom::tidy(fit, matrix = "eigenvalues")

  d <- pca_percent_var_explained(fit)$data
  expect_equal(d$PC[d$variance == "percent"], eig$PC)
  expect_lt(max(abs(d$percent[d$variance == "percent"] - eig$percent)), 1e-5)
  expect_lt(max(abs(d$percent[d$variance == "cumulative"] - eig$cumulative)), 1e-5)
})

test_that("pca_scree_plot suppresses Kaiser line on unscaled PCA by default (A3-16)", {
  unscaled <- stats::prcomp(iris[, 1:4], scale. = FALSE)
  p <- pca_scree_plot(unscaled)
  # Check that no geom_hline with yintercept = 1 is added
  hlines <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_false(any(hlines))

  # Scaled PCA still draws Kaiser line
  scaled <- stats::prcomp(iris[, 1:4], scale. = TRUE)
  p_scaled <- pca_scree_plot(scaled)
  hlines_scaled <- vapply(p_scaled$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_true(any(hlines_scaled))
})

test_that("pca_variables_plot and rotation_matrix work when rotation has NULL rownames (A3-18)", {
  mat <- unname(as.matrix(iris[, 1:4]))
  fit <- stats::prcomp(mat, scale. = TRUE)
  expect_null(rownames(fit$rotation))

  p <- pca_variables_plot(fit)
  expect_s3_class(p, "ggplot")

  rot <- rotation_matrix(fit)
  expect_equal(rot$feature, paste0("V", 1:4))
})

test_that("pca_biplot validates x and y and requires retx = TRUE (A3-19)", {
  fit <- stats::prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
  expect_error(pca_biplot(fit, x = 1, y = 1), "distinct principal components")
  expect_error(pca_biplot(fit, x = 0, y = 2), "single positive integers")
  expect_error(pca_biplot(fit, x = 1.5, y = 2), "single positive integers")

  fit_no_retx <- stats::prcomp(iris[, 1:4], retx = FALSE)
  expect_error(pca_biplot(fit_no_retx), "has no component scores")
})

test_that("pca_loading_diff handles fits sharing exactly 1 feature (A3-20)", {
  m1 <- stats::prcomp(iris[, 1:2], center = TRUE, scale. = TRUE)
  m2 <- stats::prcomp(iris[, 2:4], center = TRUE, scale. = TRUE)
  diff_res <- pca_loading_diff(m1, m2)
  expect_s3_class(diff_res, "tbl_df")
  expect_equal(nrow(diff_res), 1)
  expect_equal(diff_res$feature, "Sepal.Width")
})

