test_that("add_missing sets the requested share of each selected column to NA", {
  set.seed(1)
  df <- tibble::tibble(
    a = 1:20,
    b = letters[1:20],
    c = factor(rep(c("x", "y"), 10)),
    d = 20:1
  )
  out <- add_missing(df, c(a, b, c), pct_na = 0.25)

  pct <- features_percent_miss(out)
  pct_na <- stats::setNames(pct$PctNa, pct$feature)
  expect_equal(unname(pct_na[c("a", "b", "c")]), rep(0.25, 3))
  expect_equal(unname(pct_na["d"]), 0)
  expect_equal(pct$PctNa + pct$PctComp, rep(1, 4))
  expect_s3_class(out$c, "factor")
})

test_that("missing_cells records exactly the masked cells and nothing else changes", {
  set.seed(2)
  out <- add_missing(mtcars, pct_na = 0.1)
  cells <- attr(out, "missing_cells")

  expect_equal(nrow(cells), ncol(mtcars) * round(0.1 * nrow(mtcars)))
  masked <- is.na(as.matrix(out))
  expect_equal(sum(masked), nrow(cells))
  expect_true(all(masked[cbind(cells$row, match(cells$feature, names(mtcars)))]))
  expect_identical(as.matrix(out)[!masked], as.matrix(mtcars)[!masked])
})

test_that("pct_na of 0 and 1 give no and all missing values", {
  none <- add_missing(mtcars, pct_na = 0)
  expect_false(anyNA(none))
  expect_equal(nrow(attr(none, "missing_cells")), 0)

  expect_true(all(is.na(add_missing(mtcars, mpg, pct_na = 1)$mpg)))
})

test_that("add_missing accepts a vector of column-specific pct_na values", {
  set.seed(42)
  df <- tibble::tibble(
    a = 1:100,
    b = 101:200,
    c = 201:300
  )
  p_vec <- c(0.10, 0.25, 0.40)
  out <- add_missing(df, c(a, b, c), pct_na = p_vec)
  pct <- features_percent_miss(out)
  expect_equal(pct$PctNa[pct$feature == "a"], 0.10)
  expect_equal(pct$PctNa[pct$feature == "b"], 0.25)
  expect_equal(pct$PctNa[pct$feature == "c"], 0.40)

  cells <- attr(out, "missing_cells")
  expect_equal(nrow(cells), 75)
  expect_equal(as.integer(table(cells$feature)[c("a", "b", "c")]), c(10, 25, 40))

  # Named vector matching
  p_named <- c(c = 0.30, a = 0.05, b = 0.15)
  out_named <- add_missing(df, c(a, b, c), pct_na = p_named)
  pct_named <- features_percent_miss(out_named)
  expect_equal(pct_named$PctNa[pct_named$feature == "a"], 0.05)
  expect_equal(pct_named$PctNa[pct_named$feature == "b"], 0.15)
  expect_equal(pct_named$PctNa[pct_named$feature == "c"], 0.30)
})

test_that("add_missing validates its inputs", {
  expect_error(add_missing(1:5), "data frame")
  expect_error(add_missing(mtcars, pct_na = 1.5), "between 0 and 1")
  expect_error(add_missing(mtcars, pct_na = c(0.1, 0.2)), "length matching selected columns")
  expect_error(add_missing(airquality, Ozone), "found NA in: Ozone")
  expect_no_error(add_missing(airquality, c(Wind, Temp)))
})
