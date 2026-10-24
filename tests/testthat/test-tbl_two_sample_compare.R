test_that("tbl_two_sample_compare() reports N, mean (SD), difference with CI and p-value", {
  tab <- tbl_two_sample_compare(mtcars, c("mpg", "hp"), "am")
  expect_equal(tab$Variable, c("mpg", "hp"))
  expect_equal(names(tab), c("Variable", "N (0)", "Mean (SD) 0", "N (1)", "Mean (SD) 1",
                             "Difference [95% CI]", "p-value"))
  x0 <- mtcars$mpg[mtcars$am == 0]
  x1 <- mtcars$mpg[mtcars$am == 1]
  tt <- t.test(x0, x1)
  expect_equal(tab[["N (0)"]][1], "19")
  expect_equal(tab[["Mean (SD) 0"]][1], sprintf("%.2f (%.2f)", mean(x0), sd(x0)))
  expect_equal(tab[["Difference [95% CI]"]][1],
               sprintf("%.2f [%.2f, %.2f]", mean(x0) - mean(x1), tt$conf.int[1], tt$conf.int[2]))
  expect_equal(tab[["p-value"]][1], sprintf("%.3f", tt$p.value))
  num <- attr(tab, "numeric")
  expect_equal(num$p.value[1], tt$p.value)
  expect_equal(attr(tab, "groups"), c("0", "1"))
})

test_that("student and wilcoxon tests are used when asked", {
  s <- tbl_two_sample_compare(mtcars, "mpg", "am", test = "student")
  expect_equal(attr(s, "numeric")$p.value, t.test(mpg ~ am, mtcars, var.equal = TRUE)$p.value)
  w <- tbl_two_sample_compare(mtcars, "hp", "am", test = "wilcoxon")
  expect_equal(attr(w, "numeric")$p.value, suppressWarnings(wilcox.test(hp ~ am, mtcars))$p.value)
  expect_equal(attr(w, "test"), "wilcoxon")
})

test_that("digits, small p-values, factor level order and missing values", {
  d <- mtcars
  d$grp <- factor(ifelse(d$am == 1, "Manual", "Auto"), levels = c("Manual", "Auto"))
  d$mpg[1:3] <- NA
  tab <- tbl_two_sample_compare(d, "mpg", "grp", digits = 1, p_digits = 2)
  expect_true("N (Manual)" %in% names(tab))
  expect_equal(tab[["N (Manual)"]], "10")
  expect_match(tab[["Difference [95% CI]"]], "^[0-9.-]+ \\[")
  set.seed(1)
  big <- data.frame(g = rep(c("a", "b"), each = 30), v = c(rnorm(30), rnorm(30, 10)))
  expect_equal(tbl_two_sample_compare(big, "v", "g")[["p-value"]], "<0.001")
})

test_that("degenerate variables give blank test results and bad input errors", {
  d <- data.frame(g = c("a", "a", "b", "b"), v = c(1, 1, 1, 1), s = c(1, NA, 2, 3))
  tab <- tbl_two_sample_compare(d, c("v", "s"), "g")
  expect_equal(tab[["p-value"]][1], "")
  expect_equal(tab[["Difference [95% CI]"]][1], "")
  expect_equal(tab[["N (a)"]][2], "1")
  expect_error(tbl_two_sample_compare(mtcars, "mpg", "cyl"), "exactly two groups")
  expect_error(tbl_two_sample_compare(mtcars, "mpg", "nope"), "one column")
  expect_error(tbl_two_sample_compare(mtcars, "nope", "am"), "not in `data`")
  expect_error(tbl_two_sample_compare(data.frame(g = c("a", "b"), v = c("x", "y")), "v", "g"), "numeric")
  expect_error(tbl_two_sample_compare(mtcars, "mpg", "am", test = "anova"))
})
