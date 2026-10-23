dict_fixture <- function() {
  data.frame(
    source = c("Bruise_Area", "CCS_CB_caud_ceph", "Age", "Sex"),
    target = c("bruise_area", "ccs_cb_caud_ceph", "age", "sex"),
    stringsAsFactors = FALSE
  )
}

test_that("a complete one-to-one mapping is valid and renames the data", {
  dat <- data.frame(Bruise_Area = 1:2, CCS_CB_caud_ceph = 3:4, Age = 5:6, Sex = c("f", "m"))
  res <- reconcile_column_dictionary(dat, dict_fixture())
  expect_s3_class(res, "cbe_column_reconciliation")
  expect_true(res$is_valid)
  expect_identical(names(res$data), c("bruise_area", "ccs_cb_caud_ceph", "age", "sex"))
  expect_identical(res$matched$match_tier, rep("exact", 4L))
  expect_identical(res$matched$raw_name, names(dat))
  expect_identical(res$unmapped_data, character())
  expect_identical(res$unmapped_dict, character())
  expect_identical(nrow(res$conflicts), 0L)
  # the values are untouched
  expect_identical(res$data$age, dat$Age)
})

test_that("the three tiers are tried in order and recorded", {
  dat <- data.frame(Bruise_Area = 1, ccs_cb_caud_ceph = 2, `Age (yrs)` = 3, check.names = FALSE)
  dict <- data.frame(
    source = c("Bruise_Area", "CCS_CB_caud_ceph", "age yrs"),
    target = c("bruise_area", "ccs", "age")
  )
  res <- reconcile_column_dictionary(dat, dict)
  expect_identical(res$matched$match_tier, c("exact", "case_insensitive", "normalized"))
  expect_identical(res$matched$clean_name, c("bruise_area", "ccs_cb_caud_ceph", "age_yrs"))
  expect_true(res$is_valid)
  expect_identical(names(res$data), c("bruise_area", "ccs", "age"))
})

test_that("unmapped columns of the data and unused dictionary entries are reported", {
  dat <- data.frame(Age = 1, Extra = 2, Other = 3)
  res <- reconcile_column_dictionary(dat, dict_fixture())
  expect_identical(res$unmapped_data, c("Extra", "Other"))
  expect_identical(res$unmapped_dict, c("Bruise_Area", "CCS_CB_caud_ceph", "Sex"))
  # every target is required by default
  expect_false(res$is_valid)
  expect_identical(res$missing_required, c("bruise_area", "ccs_cb_caud_ceph", "sex"))
  expect_null(res$data)

  # only some are required
  ok <- reconcile_column_dictionary(dat, dict_fixture(), required = "age")
  expect_true(ok$is_valid)
  expect_identical(names(ok$data), c("age", "Extra", "Other"))
})

test_that("two columns for one dictionary entry are a conflict and neither is renamed", {
  dat <- data.frame(Bruise_Area = 1, bruise_area = 2, Age = 3)
  res <- reconcile_column_dictionary(dat, dict_fixture(), required = "age")
  expect_false(res$is_valid)
  expect_identical(res$conflicts$type, "shared_source")
  expect_identical(res$conflicts$source_name, "Bruise_Area")
  expect_identical(res$matched$raw_name, "Age")
  expect_identical(res$unmapped_data, character())
  expect_identical(res$unmapped_dict, c("CCS_CB_caud_ceph", "Sex"))
  # "always" renames what is unambiguous and leaves the conflicting pair alone
  always <- reconcile_column_dictionary(dat, dict_fixture(), required = "age", rename = "always")
  expect_identical(names(always$data), c("Bruise_Area", "bruise_area", "age"))
})

test_that("a column that matches two entries in its tier is ambiguous", {
  dat <- data.frame(`Age-Yrs` = 1, check.names = FALSE)
  dict <- data.frame(source = c("age yrs", "AGE_YRS"), target = c("age_a", "age_b"))
  res <- reconcile_column_dictionary(dat, dict, required = character())
  expect_identical(res$conflicts$type, "ambiguous_column")
  expect_identical(res$conflicts$raw_name, "Age-Yrs")
  expect_identical(nrow(res$matched), 0L)
  expect_false(res$is_valid)
  # names that clean to the same string are seen as the collision they are, not renamed apart
  expect_identical(res$unmapped_dict, character())
})

test_that("two entries with one target, and a target that is already a column, are conflicts", {
  dat <- data.frame(A = 1, a_ = 2)
  dict <- data.frame(source = c("A", "a_"), target = c("same", "same"))
  res <- reconcile_column_dictionary(dat, dict)
  expect_identical(res$conflicts$type, "shared_target")
  expect_false(res$is_valid)

  dat2 <- data.frame(Age = 1, age_years = 2)
  dict2 <- data.frame(source = "Age", target = "age_years")
  res2 <- reconcile_column_dictionary(dat2, dict2)
  expect_identical(res2$conflicts$type, "target_in_data")
  expect_identical(nrow(res2$matched), 0L)
  expect_false(res2$is_valid)

  # a column that already has its target name is simply matched
  dat3 <- data.frame(age = 1)
  res3 <- reconcile_column_dictionary(dat3, data.frame(source = "Age", target = "age"))
  expect_true(res3$is_valid)
  expect_identical(res3$matched$match_tier, "case_insensitive")
})

test_that("the dictionary may repeat a source with one target, but not with two", {
  dat <- data.frame(Age = 1)
  same <- data.frame(source = c("Age", "Age"), target = c("age", "age"))
  expect_true(reconcile_column_dictionary(dat, same)$is_valid)

  clash <- data.frame(source = c("Age", "Age"), target = c("age", "years"))
  res <- reconcile_column_dictionary(dat, clash)
  expect_false(res$is_valid)
  expect_identical(res$conflicts$type, "duplicate_source")
  expect_identical(nrow(res$matched), 0L)
})

test_that("rename = 'never' leaves data empty", {
  dat <- data.frame(Age = 1)
  res <- reconcile_column_dictionary(dat, data.frame(source = "Age", target = "age"), rename = "never")
  expect_true(res$is_valid)
  expect_null(res$data)
})

test_that("dictionary columns can be named", {
  dat <- data.frame(Age = 1)
  dict <- data.frame(sas = "AGE", r = "age")
  res <- reconcile_column_dictionary(dat, dict, source_col = "sas", target_col = "r")
  expect_identical(names(res$data), "age")
})

test_that("inputs are checked", {
  dat <- data.frame(Age = 1)
  expect_error(reconcile_column_dictionary(1, dict_fixture()), "`data` must be a data frame")
  expect_error(reconcile_column_dictionary(dat, "x"), "`dictionary` must be a data frame")
  expect_error(reconcile_column_dictionary(dat, dict_fixture(), source_col = "nope"), "must name a column")
  expect_error(reconcile_column_dictionary(dat, data.frame(source = c("a", NA), target = c("a", "b"))), "missing or empty")
  expect_error(reconcile_column_dictionary(dat, dict_fixture(), required = "zzz"), "not in the dictionary")
  expect_error(reconcile_column_dictionary(dat, dict_fixture(), rename = "x"), "should be one of")
})

test_that("printing summarises the result", {
  dat <- data.frame(Age = 1, Extra = 2)
  res <- reconcile_column_dictionary(dat, dict_fixture())
  out <- utils::capture.output(print(res))
  expect_match(out[1], "NOT valid")
  expect_true(any(grepl("unmapped data: 1 \\(Extra\\)", out)))
  expect_true(any(grepl("required but not mapped", out)))
})
