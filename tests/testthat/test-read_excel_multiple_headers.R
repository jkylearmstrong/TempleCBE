test_that("read_excel_multiple_headers validates input arguments defensively", {
  expect_error(
    read_excel_multiple_headers(123),
    "`path` must be a single non-empty character string"
  )
  expect_error(
    read_excel_multiple_headers(""),
    "`path` must be a single non-empty character string"
  )
  expect_error(
    read_excel_multiple_headers("non_existent_file.xlsx"),
    "does not exist"
  )

  # Create a dummy excel file for argument validation
  tmp_file <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(a = 1), tmp_file)

  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = 0),
    "single positive integer"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = -2),
    "single positive integer"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = 1.5),
    "single positive integer"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = "two"),
    "single positive integer"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = 1, sep = 123),
    "`sep` must be a single character string"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = 1, fill_merged = "yes"),
    "`fill_merged` must be a single logical"
  )
  expect_error(
    read_excel_multiple_headers(tmp_file, n_header_rows = 1, fill_merged = NA),
    "`fill_merged` must be a single logical"
  )
})

test_that("read_excel_multiple_headers concatenates multiple header rows", {
  tmp_file <- withr::local_tempfile(fileext = ".xlsx")
  
  # Raw sheet with 2 header rows and 2 data rows
  # Row 1: Group A, Group B
  # Row 2: Sub 1, Sub 2
  # Row 3: 10, 20
  # Row 4: 30, 40
  sheet_df <- data.frame(
    X1 = c("Group A", "Sub 1", "10", "30"),
    X2 = c("Group B", "Sub 2", "20", "40"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(sheet_df, tmp_file, col_names = FALSE)

  res <- read_excel_multiple_headers(tmp_file, n_header_rows = 2)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("Group A | Sub 1", "Group B | Sub 2"))
  expect_equal(nrow(res), 2)
  expect_equal(res[["Group A | Sub 1"]], c("10", "30"))
})

test_that("read_excel_multiple_headers supports hierarchical forward-fill for merged headers as opt-in", {
  tmp_file <- withr::local_tempfile(fileext = ".xlsx")
  
  # Row 1: Demographics, NA, Clinical
  # Row 2: Age, Sex, Stage
  # Row 3: 55, M, II
  sheet_df <- data.frame(
    X1 = c("Demographics", "Age", "55"),
    X2 = c(NA, "Sex", "M"),
    X3 = c("Clinical", "Stage", "II"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(sheet_df, tmp_file, col_names = FALSE)

  # Default is fill_merged = FALSE (backward-compatible: upper-tier NA cells not forward-filled)
  res_default <- read_excel_multiple_headers(tmp_file, n_header_rows = 2)
  expect_named(res_default, c("Demographics | Age", "Sex", "Clinical | Stage"))

  # Explicit fill_merged = FALSE matches default
  res_unfilled <- read_excel_multiple_headers(tmp_file, n_header_rows = 2, fill_merged = FALSE)
  expect_named(res_unfilled, c("Demographics | Age", "Sex", "Clinical | Stage"))

  # Opt-in with fill_merged = TRUE forward-fills spanning header categories
  res_filled <- read_excel_multiple_headers(tmp_file, n_header_rows = 2, fill_merged = TRUE)
  expect_named(res_filled, c("Demographics | Age", "Demographics | Sex", "Clinical | Stage"))
})

test_that("read_excel_multiple_headers respects custom sep and clean_names", {
  tmp_file <- withr::local_tempfile(fileext = ".xlsx")
  
  sheet_df <- data.frame(
    X1 = c("Group A", "Param 1", "100"),
    X2 = c("Group B", "Param 2", "200"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(sheet_df, tmp_file, col_names = FALSE)

  res_sep <- read_excel_multiple_headers(tmp_file, n_header_rows = 2, sep = "___")
  expect_named(res_sep, c("Group A___Param 1", "Group B___Param 2"))

  res_clean <- read_excel_multiple_headers(tmp_file, n_header_rows = 2, clean_names = TRUE)
  expect_named(res_clean, c("group_a_param_1", "group_b_param_2"))
})

test_that("read_excel_multiple_headers isolates header reading from data col_types", {
  tmp_file <- withr::local_tempfile(fileext = ".xlsx")
  
  # Row 1: Header 1, Header 2
  # Row 2: 12.5, 99.1
  # Use list to hold mixed types for sheet writing
  sheet_df <- data.frame(
    X1 = c("Header 1", "12.5"),
    X2 = c("Header 2", "99.1"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(sheet_df, tmp_file, col_names = FALSE)

  # Caller passes numeric col_types for data
  res <- suppressWarnings(read_excel_multiple_headers(
    tmp_file,
    n_header_rows = 1,
    col_types = c("numeric", "numeric")
  ))
  expect_named(res, c("Header 1", "Header 2"))
  expect_type(res[["Header 1"]], "double")
  expect_equal(res[["Header 1"]], 12.5)
})
