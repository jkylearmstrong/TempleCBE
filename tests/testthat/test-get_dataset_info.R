test_that("get_dataset_info returns expected results", {
  # Create a sample dataframe for testing
  df <- data.frame(
    numeric_col = c(1, 2, NA, 4, 5),
    factor_col = factor(c("a", "b", "a", NA, "b")),
    character_col = c("apple", "banana", NA, "apple", "banana"),
    logical_col = c(TRUE, FALSE, NA, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  # Run the function on the sample dataframe
  result <- get_dataset_info(df)

  # Check that the result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check that the result has the expected number of rows
  expect_equal(nrow(result), ncol(df))

  # Check that the result has the expected columns
  expected_cols <- c("dataset_name", "labels", "columns", "class", "mean", "sd", "most_freq", "n_distinct", "SumNa", "PctNa")
  expect_equal(colnames(result), expected_cols)

  # Check that the dataset_name is correct
  expect_equal(result$dataset_name[1], "df")
})

test_that("proc_contents works without labels on dataframe", {
  x <- 1:5
  y <- 2:6

  df2 <- data.frame(x = x, y = y)

  result <- proc_contents(df2)

  expect_equal(result$labels, c("x", "y"))
})

test_that("proc_contents handles edge cases (no numeric, date only, logical only, empty)", {
  # 1. No numeric columns (only Character + Date)
  df_no_numeric <- tibble::tibble(
    ID = c("A01", "A02", "A03"),
    Status = c("Consent", "No Consent", "Pending"),
    Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  )

  res_no_num <- proc_contents(df_no_numeric)
  expect_s3_class(res_no_num, "data.frame")
  expect_equal(nrow(res_no_num), 3)
  expect_equal(sort(unique(res_no_num$class)), sort(c("character", "Date")))

  # 2. Only Date columns (No numeric, No character/factor -> no most_freq candidates)
  df_date_only <- tibble::tibble(
    Date1 = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    Date2 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )
  res_date <- proc_contents(df_date_only)
  expect_s3_class(res_date, "data.frame")
  expect_equal(nrow(res_date), 2)
  expect_true(all(is.na(res_date$mean)))

  # 3. Only Logical (No numeric -> no mean/sd)
  df_logical <- tibble::tibble(
    Flag1 = c(TRUE, FALSE, TRUE),
    Flag2 = c(FALSE, FALSE, TRUE)
  )
  res_logical <- proc_contents(df_logical)
  expect_s3_class(res_logical, "data.frame")
  expect_equal(nrow(res_logical), 2)
  expect_true(all(is.na(res_logical$mean)))
  # Logical is picked up by most_freq
  expect_false(all(is.na(res_logical$most_freq)))

  # 4. Empty dataframe (0 columns)
  df_empty <- tibble::tibble(.rows = 3)
  res_empty <- proc_contents(df_empty)
  expect_s3_class(res_empty, "data.frame")
  expect_equal(nrow(res_empty), 0)
})

test_that("get_dataset_info handles all-NA columns without crashing", {
  # Regression test: `names(which.max(table(x)))` returns a zero-length
  # character vector when x is entirely NA (table() drops NAs by default),
  # which crashed dplyr::summarise(across()) since it requires every column
  # in one across() call to return a length-1 result.
  df <- data.frame(
    all_na_num = c(NA_real_, NA_real_, NA_real_),
    all_na_chr = c(NA_character_, NA_character_, NA_character_),
    ok = c(1, 2, 3)
  )

  res <- get_dataset_info(df)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)

  na_rows <- res |> dplyr::filter(columns %in% c("all_na_num", "all_na_chr"))
  expect_true(all(is.na(na_rows$most_freq)))
})

test_that("proc_contents handles incompatible numeric S3 classes (e.g. chron::times)", {
  # Simulate a 'times' class that is numeric but incompatible with double in pivot_longer
  # chron::times is numeric but has class "times"

  # Create a dataframe with a normal double and a 'times' like object
  x <- 1:5
  y <- 1:5
  class(y) <- "times" # mocking the class

  df_incompat <- data.frame(
    dbl = x,
    time = y
  )

  # This should not error with the fix (forced as.numeric)
  res <- proc_contents(df_incompat)

  expect_s3_class(res, "data.frame")
  expect_true("mean" %in% colnames(res))
  expect_equal(res |> dplyr::filter(columns == "time") |> dplyr::pull(mean), 3)
})
