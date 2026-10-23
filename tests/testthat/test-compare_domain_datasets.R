domain_fixture <- function(n = 25, times = 1:3, shift = 0) {
  set.seed(42)
  d <- expand.grid(id = seq_len(n), time = times)
  d$hr <- stats::rnorm(nrow(d), 80, 10)
  d$map <- 0.5 * d$hr + stats::rnorm(nrow(d), 50, 5)
  d$spo2 <- stats::rnorm(nrow(d), 96, 2)
  orig <- d
  orig$hr[seq(1, nrow(d), by = 4)] <- NA
  imp <- orig
  imp$hr[is.na(imp$hr)] <- mean(imp$hr, na.rm = TRUE) + shift
  list(orig = orig, imp = imp)
}

test_that("returns the documented pieces for a longitudinal domain", {
  fx <- domain_fixture()
  out <- compare_domain_datasets(fx$orig, fx$imp, id_col = "id", time_col = "time")
  expect_named(out, c("ecdf_plots", "ks_tests", "corr_diff_table", "corr_diff_heatmap",
                      "pca_loading_diff", "trajectory_plot", "trajectory_deltas"))
  expect_s3_class(out$ecdf_plots, "ggplot")
  expect_s3_class(out$corr_diff_heatmap, "ggplot")
  expect_s3_class(out$pca_loading_diff, "ggplot")
  expect_s3_class(out$trajectory_plot, "ggplot")
  expect_identical(out$ks_tests$variable, c("hr", "map", "spo2"))
  expect_identical(
    names(out$trajectory_deltas),
    c("variable", "time", "mean_orig", "mean_imp", "delta", "n_paired", "p_value", "p_adj")
  )
  # 3 variables x 3 time points
  expect_identical(nrow(out$trajectory_deltas), 9L)
  # id and time are not compared as variables
  expect_false(any(c("id", "time") %in% out$ks_tests$variable))
})

test_that("only hr is imputed: its delta is the shift in the mean, the others are zero", {
  fx <- domain_fixture(shift = 5)
  out <- compare_domain_datasets(fx$orig, fx$imp, id_col = "id", time_col = "time")
  td <- out$trajectory_deltas
  expect_true(all(td$delta[td$variable != "hr"] == 0))
  expect_true(all(td$delta[td$variable == "hr"] > 0))
  # delta is imputed mean minus original mean at each time
  row <- td[td$variable == "hr" & td$time == 2, ]
  i <- fx$orig$time == 2
  expect_equal(row$mean_orig, mean(fx$orig$hr[i], na.rm = TRUE))
  expect_equal(row$mean_imp, mean(fx$imp$hr[i]))
  expect_equal(row$delta, row$mean_imp - row$mean_orig)
})

test_that("an imputation that leaves observed values alone has NA p-values; one that changes them has Holm-adjusted ones", {
  fx <- domain_fixture()
  out <- compare_domain_datasets(fx$orig, fx$imp, id_col = "id", time_col = "time")
  expect_true(all(is.na(out$trajectory_deltas$p_value)))
  expect_true(all(is.na(out$trajectory_deltas$p_adj)))

  changed <- fx$imp
  changed$map <- changed$map + stats::rnorm(nrow(changed), 1, 0.5)
  out2 <- compare_domain_datasets(fx$orig, changed, id_col = "id", time_col = "time")
  td <- out2$trajectory_deltas
  p <- td$p_value[td$variable == "map"]
  expect_false(anyNA(p))
  expect_equal(td$p_adj, stats::p.adjust(td$p_value, method = "holm"))
  expect_true(all(td$p_adj >= td$p_value | is.na(td$p_adj)))

  out3 <- compare_domain_datasets(fx$orig, changed, id_col = "id", time_col = "time", test = "wilcoxon")
  expect_false(anyNA(out3$trajectory_deltas$p_value[out3$trajectory_deltas$variable == "map"]))
})

test_that("without id_col and time_col there are no trajectory pieces", {
  fx <- domain_fixture()
  out <- compare_domain_datasets(fx$orig, fx$imp, vars = c("hr", "map"))
  expect_null(out$trajectory_plot)
  expect_null(out$trajectory_deltas)
  expect_identical(out$ks_tests$variable, c("hr", "map"))
  expect_s3_class(out$corr_diff_heatmap, "ggplot")
})

test_that("a single variable gives no correlation or PCA pieces; PCA can be switched off", {
  fx <- domain_fixture()
  expect_warning(
    one <- compare_domain_datasets(fx$orig, fx$imp, vars = "hr"),
    "PCA loading comparison skipped"
  )
  expect_null(one$pca_loading_diff)
  expect_null(one$corr_diff_heatmap)
  expect_identical(nrow(one$corr_diff_table), 0L)

  expect_no_warning(off <- compare_domain_datasets(fx$orig, fx$imp, vars = c("hr", "map"), pca = FALSE))
  expect_null(off$pca_loading_diff)
})

test_that("a constant variable skips the PCA comparison with a warning", {
  fx <- domain_fixture()
  fx$orig$spo2 <- 97
  fx$imp$spo2 <- 97
  # cor() also warns about the constant column; that one is not what is tested here
  expect_warning(
    out <- withCallingHandlers(
      compare_domain_datasets(fx$orig, fx$imp, vars = c("hr", "map", "spo2")),
      warning = function(w) if (grepl("standard deviation is zero", conditionMessage(w))) invokeRestart("muffleWarning")
    ),
    "no constant variable"
  )
  expect_null(out$pca_loading_diff)
})

test_that("with nothing missing there is no ECDF plot, and a message says so", {
  fx <- domain_fixture()
  expect_message(
    out <- compare_domain_datasets(fx$imp, fx$imp, vars = c("hr", "map")),
    "nothing was imputed"
  )
  expect_null(out$ecdf_plots)
  expect_s3_class(out$corr_diff_heatmap, "ggplot")
})

test_that("inputs are checked", {
  fx <- domain_fixture()
  expect_error(compare_domain_datasets(1, fx$imp), "must be data frames")
  expect_error(compare_domain_datasets(fx$orig, fx$imp[-1, ]), "same rows in the same order")
  expect_error(compare_domain_datasets(fx$orig, fx$imp, vars = "nope"), "not in both data frames")
  expect_error(
    compare_domain_datasets(transform(fx$orig, hr = "a"), fx$imp, vars = "hr"),
    "must be numeric"
  )
  expect_error(compare_domain_datasets(fx$orig, fx$imp, id_col = c("id", "time")), "single column name")
  expect_error(compare_domain_datasets(fx$orig, fx$imp, pca = NA), "TRUE or FALSE")
  expect_error(compare_domain_datasets(fx$orig, fx$imp, test = "z"), "should be one of")
  expect_error(compare_domain_datasets(fx$orig[c("id", "time")], fx$imp[c("id", "time")], id_col = "id", time_col = "time"),
               "No numeric columns")

  shuffled <- fx$imp[rev(seq_len(nrow(fx$imp))), ]
  expect_error(compare_domain_datasets(fx$orig, shuffled, id_col = "id", time_col = "time"),
               "rows must correspond")
  # id_col or time_col named but absent from one data frame
  expect_error(
    compare_domain_datasets(fx$orig, fx$imp[setdiff(names(fx$imp), "time")], id_col = "id", time_col = "time"),
    "must be columns of both"
  )
})

test_that("compare_imputed_ecdf() accepts a variable that has no missing value", {
  orig <- data.frame(a = c(1, NA, 3, 4), b = c(5, 6, 7, 8))
  imp <- data.frame(a = c(1, 2, 3, 4), b = c(5, 6, 7, 8))
  expect_s3_class(compare_imputed_ecdf(orig, imp, cols = c("a", "b")), "ggplot")
})
