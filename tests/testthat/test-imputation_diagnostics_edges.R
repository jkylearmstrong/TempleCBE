orig_df <- function() data.frame(x = c(1, 2, NA, 4, 5, NA), y = c(10, NA, 30, 40, 50, 60), z = c(1, 2, 3, 4, 5, 6))
imp_df <- function() data.frame(x = c(1, 2, 3, 4, 5, 3.5), y = c(10, 20, 30, 40, 50, 60), z = c(1, 2, 3, 4, 5, 6))

test_that("test_imputed_distributions() reports one row per variable and zero-missingness rows", {
  r <- test_imputed_distributions(orig_df(), imp_df(), c("x", "z"))
  expect_equal(r$variable, c("x", "z"))
  expect_equal(r$n_observed, c(4L, 6L))
  expect_equal(r$n_imputed, c(2L, 0L))
  expect_equal(r$mean_imputed[1], mean(c(3, 3.5)))
  expect_true(is.na(r$mean_imputed[2]) && is.na(r$ks_statistic[2]) && is.na(r$t_p_value[2]))
  expect_false(is.na(r$ks_p_value[1]))
})

test_that("test_imputed_distributions() gives NA tests, not errors, for factor or constant data", {
  o <- data.frame(f = factor(c("a", "b", NA, "a")), k = c(1, 1, NA, 1))
  i <- data.frame(f = factor(c("a", "b", "c", "a")), k = c(1, 1, 1, 1))
  r <- test_imputed_distributions(o, i, c("f", "k"))
  expect_equal(r$n_imputed, c(1L, 1L))
  expect_true(is.na(r$t_p_value[r$variable == "f"]))
  expect_true(is.na(r$t_p_value[r$variable == "k"])) # constant data: t.test fails, caught
})

test_that("test_imputed_distributions() names a column that is missing", {
  expect_error(test_imputed_distributions(orig_df(), imp_df(), c("x", "nope")), "missing from")
  expect_error(test_imputed_distributions(orig_df(), imp_df()["z"], "x"), "missing from")
})

test_that("compare_imputed_ecdf() handles a variable without missing values and errors cleanly", {
  p <- compare_imputed_ecdf(orig_df(), imp_df(), c("x", "z"))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_equal(unique(p$data$variable), c("x", "z")[c("x", "z") %in% unique(p$data$variable)])
  expect_equal(sum(p$data$dataset == "Imputed"), 2L) # z has nothing imputed
  expect_error(compare_imputed_ecdf(orig_df(), imp_df(), "nope"), "missing from")
  expect_error(compare_imputed_ecdf(orig_df()["z"], imp_df()["z"], "z", colors = c(Original = "black", Imputed = "red")) |>
                 ggplot2::ggplot_build(), NA)
  none <- data.frame(a = numeric(0))
  expect_error(compare_imputed_ecdf(none, none, "a"), "No data available")
})

test_that("plot_imputed_ridgeline() builds, and says which package or column is missing", {
  skip_if_not_installed("ggridges")
  p <- suppressMessages(plot_imputed_ridgeline(orig_df(), imp_df(), c("x", "y")))
  expect_s3_class(p, "ggplot")
  expect_error(plot_imputed_ridgeline(orig_df(), imp_df(), "nope"), "missing from")
})
