test_that("make_excel_names turns snake_case names into readable column headings", {
  excel_names <- make_excel_names(c("patient_id", "body_mass_index"))
  expect_equal(excel_names, c("Patient Id", "Body Mass Index"))
})

test_that("clean_names_keep_labels cleans character strings and data frames and keeps the original names as labels", {
  raw_names <- c("First Name", "ZIP Code", "Total ($)")
  expect_equal(clean_names_keep_labels(raw_names), c("first_name", "zip_code", "total"))

  df <- data.frame(`First Name` = 1, `ZIP Code` = 2, check.names = FALSE)
  df_cleaned <- clean_names_keep_labels(df)
  expect_true(all(c("first_name", "zip_code") %in% names(df_cleaned)))
  expect_equal(as.character(labelled::var_label(df_cleaned$first_name)), "First Name")
})
