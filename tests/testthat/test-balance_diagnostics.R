test_that("balance_smd reproduces a hand-computed standardized mean difference", {
  d <- data.frame(g = rep(c("a", "b"), each = 3), x = c(1, 2, 3, 3, 4, 5))
  out <- balance_smd(d, "g")
  expect_s3_class(out, "balance_smd")
  expect_equal(out$comparison, "b vs a")
  expect_equal(out$mean_ref, 2)
  expect_equal(out$mean_cmp, 4)
  # both variances are 1, so the pooled SD is 1
  expect_equal(out$smd, 2)
  expect_equal(out$var_ratio, 1)
  expect_false(out$balanced)
  expect_equal(attr(out, "ref"), "a")
  expect_output(print(out), "Covariate balance")
})

test_that("balance_smd handles binary, factor and logical covariates", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 4),
    male = c(1, 0, 0, 0, 1, 1, 1, 0),
    site = factor(c("X", "X", "Y", "Y", "X", "Y", "Y", "Y")),
    flag = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  out <- balance_smd(d, "g")
  expect_setequal(out$variable, c("male", "site = X", "site = Y", "flag"))
  male <- out[out$variable == "male", ]
  # p_ref = .25, p_cmp = .75; sample variances 0.25 and 0.25 (n/(n-1) * p(1-p))
  v <- 4 / 3 * 0.25 * 0.75
  expect_equal(male$smd, 0.5 / sqrt(v))
  # indicators of one factor are mirror images
  expect_equal(out$smd[out$variable == "site = X"], -out$smd[out$variable == "site = Y"])
  expect_true(is.na(out$var_ratio[out$variable == "flag"]) || is.finite(out$var_ratio[out$variable == "flag"]))
})

test_that("balance_smd with weights keeps the unweighted denominator and adds an Adjusted row", {
  d <- data.frame(g = rep(c("a", "b"), each = 4), x = c(1, 2, 3, 4, 3, 4, 5, 6), w = c(1, 1, 1, 1, 2, 2, 0.5, 0.5))
  out <- balance_smd(d, "g", weights = "w")
  expect_equal(out$sample, c("Unadjusted", "Adjusted"))
  expect_equal(out$smd[1], (4.5 - 2.5) / sd(c(1:4)))
  mb <- sum(d$w[5:8] * d$x[5:8]) / sum(d$w[5:8])
  expect_equal(out$mean_cmp[2], mb)
  expect_equal(out$smd[2], (mb - 2.5) / sd(1:4))
  # a weights vector and the column give the same table
  expect_equal(balance_smd(d[c("g", "x")], "g", weights = d$w)$smd, out$smd)
  # the weights column is not a covariate
  expect_false("w" %in% out$variable)
})

test_that("balance_smd handles several groups, a chosen reference and bad input", {
  d <- data.frame(g = rep(c("a", "b", "c"), each = 3), x = c(1, 2, 3, 2, 3, 4, 3, 4, 5))
  out <- balance_smd(d, "g", ref = "b")
  expect_setequal(out$comparison, c("a vs b", "c vs b"))
  expect_equal(out$smd[out$comparison == "a vs b"], -1)
  expect_error(balance_smd(d, "nope"), "group")
  expect_error(balance_smd(d, "g", ref = "z"), "ref")
  expect_error(balance_smd(d, "g", vars = "nope"), "not found")
  expect_error(balance_smd(d, "g", weights = 1:3), "weights")
  expect_error(balance_smd(data.frame(g = 1, x = 1), "g"), "two levels")
  # a constant covariate has no defined SMD
  expect_true(is.na(balance_smd(data.frame(g = c("a", "a", "b", "b"), x = 1), "g")$smd))
})

test_that("plot_balance_love draws one point per covariate and sample", {
  d <- data.frame(g = rep(c("a", "b"), each = 4), x = c(1, 2, 3, 4, 3, 4, 5, 6), y = c(1, 3, 2, 4, 2, 4, 3, 5), w = rep(1, 8))
  b <- balance_smd(d, "g", weights = "w")
  p <- plot_balance_love(b)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[2]]), 4L) # 2 covariates x 2 samples
  expect_s3_class(plot_balance_love(b, absolute = FALSE), "ggplot")
  expect_s3_class(plot_balance_love(d[c("g", "x", "y")], group = "g"), "ggplot")
  expect_error(plot_balance_love(b, comparison = "z vs a"), "comparison")
})
