km_fixture <- function(n_risk_a = 3) {
  data.frame(
    time = c(0, 1, 2, 3, 0, 2),
    strata = c("A", "A", "A", "A", "B", "B"),
    n.risk = c(n_risk_a, 3, 2, 1, 2, 2),
    n.event = c(0, 1, 0, 1, 0, 1),
    n.censor = c(0, 0, 1, 0, 0, 1)
  )
}

test_that("km_summary_to_prism expands counts to one row per subject", {
  out <- expect_silent(km_summary_to_prism(km_fixture()))
  expect_named(out, c("X", "A", "B"))
  expect_equal(out$X, c(1, 2, 2, 2, 3))
  expect_equal(out$A, c(1L, 0L, NA, NA, 1L))
  expect_equal(out$B, c(NA, NA, 1L, 0L, NA))
  expect_true(all(rowSums(!is.na(out[c("A", "B")])) == 1))
  expect_length(attr(out, "notes"), 2)
})

test_that("km_summary_to_prism orders group columns by strata_levels", {
  out <- km_summary_to_prism(km_fixture(), strata_levels = c("B", "A"))
  expect_named(out, c("X", "B", "A"))
  expect_error(km_summary_to_prism(km_fixture(), strata_levels = "A"), "not in `strata_levels`: B")
})

test_that("km_summary_to_prism checks columns and initial totals", {
  expect_error(km_summary_to_prism(km_fixture()[, -4]), "Missing required column\\(s\\): n.event")
  expect_warning(km_summary_to_prism(km_fixture(n_risk_a = 4)), "A \\(n.risk = 4, counts = 3\\)")
  expect_silent(km_summary_to_prism(km_fixture(n_risk_a = 4), validate_totals = FALSE))
})

test_that("km_summary_to_prism reads CSV input and writes Excel output", {
  skip_if_not_installed("writexl")
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(km_fixture(), csv, row.names = FALSE)
  xlsx <- withr::local_tempfile(fileext = ".xlsx")
  paste_xlsx <- withr::local_tempfile(fileext = ".xlsx")

  out <- km_summary_to_prism(csv, out_xlsx = xlsx, out_xlsx_paste = paste_xlsx)
  expect_equal(nrow(out), 5)
  expect_true(file.exists(xlsx))
  expect_true(file.exists(paste_xlsx))
  expect_equal(readxl::read_excel(xlsx)$A, c(1, 0, NA, NA, 1))
})
