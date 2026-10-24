test_that("glm_single() matches lm and gives raw and normalized rows", {
  r <- glm_single(mtcars, "mpg", "wt")
  expect_s3_class(r, "cbe_coef_screen")
  expect_named(r, c("feature", "var_label", "scale", "term", "estimate", "std.error", "statistic",
                    "p.value", "conf.low", "conf.high", "n", "status", "warning"))
  expect_equal(r$scale, c("raw", "normalized"))
  expect_equal(r$term, c("wt", "wt"))
  fit <- lm(mpg ~ wt, mtcars)
  expect_equal(r$estimate[1], unname(coef(fit)["wt"]))
  expect_equal(r$p.value[1], summary(fit)$coefficients["wt", 4])
  tcrit <- qt(0.975, fit_df <- lm(mpg ~ wt, mtcars)$df.residual)
  expect_equal(r$conf.low[1], r$estimate[1] - tcrit * r$std.error[1])
  expect_equal(c(r$conf.low[1], r$conf.high[1]), unname(confint(lm(mpg ~ wt, mtcars))["wt", ]))
  # the normalized effect is the raw effect times the observed range
  expect_equal(r$estimate[2], r$estimate[1] * diff(range(mtcars$wt)))
  expect_equal(r$p.value[2], r$p.value[1])
  expect_equal(unique(r$status), "ok")
  expect_equal(unique(r$n), 32L)
})

test_that("glm_single() supports other families, factors, labels and normalize = FALSE", {
  b <- glm_single(mtcars, "am", "wt", family = binomial())
  fit <- glm(am ~ wt, binomial(), mtcars)
  expect_equal(b$estimate[1], unname(coef(fit)["wt"]))
  expect_equal(nrow(glm_single(mtcars, "am", "wt", family = "binomial")), 2L)
  expect_equal(nrow(glm_single(mtcars, "mpg", "wt", normalize = FALSE)), 1L)
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  f <- glm_single(d, "mpg", "cyl_f")
  expect_equal(f$scale, c("raw", "raw")) # no normalized scale for a factor
  expect_equal(f$term, c("cyl_f6", "cyl_f8"))
  attr(d$wt, "label") <- "Weight (1000 lb)"
  expect_equal(unique(glm_single(d, "mpg", "wt")$var_label), "Weight (1000 lb)")
  expect_error(glm_single(mtcars, "mpg", "wt", family = "nope"))
  expect_error(glm_single(mtcars, "mpg", "wt", family = 3), "family")
})

test_that("guards return a status row instead of failing", {
  d <- mtcars
  d$k <- 1
  d$allna <- NA_real_
  d$yconst <- 5
  expect_equal(glm_single(d, "mpg", "k")$status, "predictor has fewer than two distinct values")
  expect_equal(glm_single(d, "mpg", "allna")$status, "no complete cases")
  expect_equal(glm_single(d, "yconst", "wt")$status, "outcome has fewer than two distinct values")
  expect_true(is.na(glm_single(d, "mpg", "k")$estimate))
  expect_error(glm_single(d, "nope", "wt"), "outcome")
  expect_error(glm_single(d, "mpg", "nope"), "feature")
  expect_error(glm_single(1, "a", "b"), "data frame")
  expect_error(glm_single(d, "mpg", "wt", normalize = NA), "normalize")
  expect_error(glm_single(d, "mpg", "wt", conf_level = 2), "conf_level")
})

test_that("lmm_single() fits a random-intercept model and uses Wald intervals", {
  skip_if_not_installed("lme4")
  r <- suppressWarnings(lmm_single(lme4::sleepstudy, "Reaction", "Days", id_col = "Subject"))
  fit <- suppressWarnings(lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy))
  expect_equal(r$scale, c("raw", "normalized"))
  expect_equal(r$estimate[1], unname(lme4::fixef(fit)["Days"]), tolerance = 1e-6)
  tv <- coef(summary(fit))["Days", "t value"]
  expect_equal(r$p.value[1], 2 * pnorm(-abs(tv)), tolerance = 1e-6)
  expect_equal(r$estimate[2], r$estimate[1] * diff(range(lme4::sleepstudy$Days)), tolerance = 1e-6)
  expect_error(lmm_single(lme4::sleepstudy, "Reaction", "Days", id_col = "nope"), "id_col")
  expect_error(lmm_single(lme4::sleepstudy, "Reaction", "Days"), "id_col")
})

test_that("stars follow strict less-than breaks", {
  expect_equal(
    TempleCBE:::.coef_stars(c(0.0005, 0.001, 0.009, 0.01, 0.04, 0.05, 0.09, 0.15, 0.2, 0.9, NA),
                            c(0.001, 0.01, 0.05, 0.1, 0.2), c("****", "***", "**", "*", ".", "ns")),
    c("****", "***", "***", "**", "**", "*", "*", ".", "ns", "ns", "")
  )
  expect_error(plot_coef_forest(glm_single(mtcars, "mpg", "wt"), p_labels = c("a", "b")), "one more")
  expect_error(plot_coef_forest(glm_single(mtcars, "mpg", "wt"), p_breaks = c(0.5, 0.1), p_labels = c("a", "b", "c")), "increasing")
})

test_that("plot_coef_forest() and tbl_coef_forest() display stacked screens", {
  res <- dplyr::bind_rows(
    glm_single(mtcars, "mpg", "wt"),
    glm_single(mtcars, "mpg", "hp"),
    glm_single(mtcars, "mpg", "qsec")
  )
  p <- plot_coef_forest(res, facet_by = "scale")
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_s3_class(plot_coef_forest(res, order_by = "estimate"), "ggplot")
  expect_s3_class(plot_coef_forest(res, order_by = "p"), "ggplot")
  tab <- tbl_coef_forest(res)
  expect_named(tab, c("Term", "Estimate [95% CI]", "p-value", "Significance", "Scale"))
  expect_equal(nrow(tab), nrow(res))
  wt <- tab[tab$Term == "wt" & tab$Scale == "raw", ]
  expect_match(wt$`Estimate [95% CI]`, "^-5\\.34 \\[")
  expect_equal(wt$`p-value`, "<0.001")
  expect_equal(wt$Significance, "****")
  expect_error(plot_coef_forest(res, term_col = "nope"), "lacks column")
  expect_error(plot_coef_forest(res, facet_by = "nope"), "facet_by")
  failed <- glm_single(transform(mtcars, k = 1), "mpg", "k")
  expect_error(plot_coef_forest(failed), "No estimable")
  expect_equal(tbl_coef_forest(failed)$`Estimate [95% CI]`, "")
})

test_that("gaussian screens use t limits so the interval agrees with the p-value", {
  set.seed(3)
  d <- data.frame(x = rnorm(6), y = rnorm(6))
  r <- glm_single(d, "y", "x", normalize = FALSE)
  fit <- lm(y ~ x, d)
  ci <- confint(fit)["x", ]
  expect_equal(c(r$conf.low, r$conf.high), unname(ci), tolerance = 1e-8)
  # the interval excludes 0 exactly when p < 0.05
  expect_equal(r$conf.low > 0 || r$conf.high < 0, r$p.value < 0.05)
  # binomial tables are z-based
  b <- glm_single(mtcars, "am", "wt", family = binomial(), normalize = FALSE)
  expect_equal(b$conf.low, b$estimate - qnorm(0.975) * b$std.error)
})

test_that("plot_coef_forest() separates factor contrasts and the two scales of one predictor", {
  d <- mtcars
  res <- glm_single(d, "mpg", "cyl")
  p <- plot_coef_forest(res)
  expect_equal(length(unique(p$data$.label)), nrow(p$data))
  stk <- dplyr::bind_rows(glm_single(d, "mpg", "wt"), glm_single(d, "mpg", "hp"))
  p2 <- plot_coef_forest(stk)
  expect_equal(length(unique(p2$data$.label)), nrow(stk))
  expect_match(as.character(p2$data$.label[1]), "raw|normalized")
})

test_that("star positions are per facet and the axis label can be set", {
  stk <- dplyr::bind_rows(glm_single(mtcars, "mpg", "wt"), glm_single(mtcars, "mpg", "hp"))
  p <- plot_coef_forest(stk, facet_by = "scale", x_label = "Effect (90% CI)")
  d <- p$data
  for (sc in c("raw", "normalized")) {
    hi <- max(d$.hi[d$scale == sc])
    span <- diff(range(c(d$.lo[d$scale == sc], d$.hi[d$scale == sc], d$.est[d$scale == sc])))
    expect_true(all(d$.xstar[d$scale == sc] > hi))
    expect_true(all(d$.xstar[d$scale == sc] <= hi + 0.1 * span))
  }
  expect_equal(p$labels$x, "Effect (90% CI)")
})
