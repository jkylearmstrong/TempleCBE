test_helper_ttest_results <- function() {
  mtcars |>
    dplyr::mutate(am = factor(am)) |>
    multiple_t_test(.var_list = c("mpg", "hp", "wt"), .class = "am")
}

test_that("manhattan_plot returns a ggplot object with the expected labels", {
  res <- test_helper_ttest_results()
  p <- manhattan_plot(res, var = var, log_p = log_p)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Variable")
  expect_equal(p$labels$y, "-log10(p-value)")
})

test_that("volcano_plot returns a ggplot object with the expected labels", {
  res <- test_helper_ttest_results()
  p <- volcano_plot(res, log2_fold_change = log2_fold_change, log_p = log_p, var = var)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "log2(Fold-Change)")
  expect_equal(p$labels$y, "-log10(p-value)")
})

test_that("manhattan_plot/volcano_plot honor a custom `alpha` threshold instead of a hardcoded 0.05", {
  # Regression test: the significance threshold used to be hardcoded to
  # -log10(0.05) in four places, so callers couldn't use any other alpha.
  res <- test_helper_ttest_results()

  p_default <- manhattan_plot(res, var = var, log_p = log_p)
  p_custom <- manhattan_plot(res, var = var, log_p = log_p, alpha = 0.1)

  hline_default <- p_default$layers[[length(p_default$layers)]]$data$yintercept
  hline_custom <- p_custom$layers[[length(p_custom$layers)]]$data$yintercept

  expect_equal(hline_default, -log10(0.05))
  expect_equal(hline_custom, -log10(0.1))
  expect_false(isTRUE(all.equal(hline_default, hline_custom)))

  p_volcano_custom <- volcano_plot(res, log2_fold_change = log2_fold_change, log_p = log_p, var = var, alpha = 0.1)
  hline_volcano <- p_volcano_custom$layers[[length(p_volcano_custom$layers)]]$data$yintercept
  expect_equal(hline_volcano, -log10(0.1))
})

test_that("manhattan_plot/volcano_plot can omit the significance shading", {
  res <- test_helper_ttest_results()
  p <- manhattan_plot(res, var = var, log_p = log_p, highlight_significant = FALSE)
  # only the geom_point layer and the hline layer remain (no annotate rect)
  expect_equal(length(p$layers), 2)
})
