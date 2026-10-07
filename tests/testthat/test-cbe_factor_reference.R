test_that("cbe_factor_reference defaults to the first level", {
  f <- factor(c("B", "A", "C", "A"))
  releveled <- cbe_factor_reference(f)

  expect_equal(levels(releveled)[1], "A")
  expect_equal(attr(releveled, "cbe_reference_level"), "A")
})

test_that("cbe_factor_reference sets an explicit reference level", {
  f <- factor(c("Male", "Female", "Female", "Male"))
  releveled <- cbe_factor_reference(f, ref_level = "Female")

  expect_equal(levels(releveled)[1], "Female")
  expect_equal(attr(releveled, "cbe_reference_level"), "Female")
  expect_message(cbe_factor_reference(f, ref_level = "Female"), "Female")
})

test_that("cbe_factor_reference coerces non-factor vectors", {
  x <- c("high", "low", "medium", "low")
  releveled <- cbe_factor_reference(x, ref_level = "low")

  expect_true(is.factor(releveled))
  expect_equal(levels(releveled)[1], "low")
})

test_that("cbe_factor_reference errors on a ref_level not present in the data", {
  f <- factor(c("A", "B"))
  expect_error(cbe_factor_reference(f, ref_level = "Z"), "not a level")
})

test_that("cbe_factor_reference output feeds cleanly into cbe_cox_single", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))
  lung$sex_f <- cbe_factor_reference(lung$sex_f, ref_level = "Female")

  res <- cbe_cox_single(lung, outcome = "outcome", feature = "sex_f")
  expect_equal(res$table$Level[1], "Female")
  expect_equal(res$table$Role[1], "Reference")
})
