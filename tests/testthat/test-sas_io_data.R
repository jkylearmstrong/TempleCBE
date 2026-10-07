test_that("rossi_data loads the Rossi recidivism dataset in CSV and SAS formats", {
  # Test CSV format
  df_csv <- rossi_data(format = "csv")
  expect_s3_class(df_csv, "tbl_df")
  expect_equal(nrow(df_csv), 432)
  expect_true(all(c("week", "arrest", "fin", "age", "prio") %in% names(df_csv)))

  # Test SAS format if haven is installed
  if (requireNamespace("haven", quietly = TRUE)) {
    df_sas <- rossi_data(format = "sas")
    expect_s3_class(df_sas, "tbl_df")
    expect_equal(nrow(df_sas), 432)
    expect_equal(df_csv$week, as.numeric(df_sas$week))
    expect_equal(df_csv$arrest, as.numeric(df_sas$arrest))
  }
})
