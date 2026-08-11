test_that("clean_names and make_excel_names sanitize character strings and data frames", {
  # Character vector test
  raw_names <- c("First Name", "ZIP Code", "Total ($)")
  cleaned <- clean_names(raw_names)
  expect_equal(cleaned, c("first_name", "zip_code", "total"))

  # Data frame test
  df <- data.frame(`First Name` = 1, `ZIP Code` = 2, check.names = FALSE)
  df_cleaned <- clean_names(df)
  expect_true(all(c("first_name", "zip_code") %in% names(df_cleaned)))

  # Excel names test
  excel_names <- make_excel_names(c("patient_id", "body_mass_index"))
  expect_equal(excel_names, c("Patient Id", "Body Mass Index"))

  # Error handling test
  expect_error(clean_names(123), "Input must be a character vector or data frame")
})
