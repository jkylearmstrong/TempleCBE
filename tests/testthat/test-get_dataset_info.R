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

test_that("get_dataset_info summarizes Surv columns via time/status, not flattened numerics", {
  skip_if_not_installed("survival")

  time <- c(5, 10, 15, 20, NA)
  status <- c(1, 0, 1, 1, 0)
  df <- data.frame(
    id = 1:5,
    surv = survival::Surv(time, status)
  )

  res <- get_dataset_info(df)
  surv_row <- res |> dplyr::filter(columns == "surv")

  expect_equal(surv_row$class, "Surv")
  expect_equal(surv_row$mean, mean(time, na.rm = TRUE))
  expect_equal(surv_row$sd, stats::sd(time, na.rm = TRUE))
  expect_equal(surv_row$most_freq, "Events: 3 (60%)")
  # all 5 (time, status) rows are distinct, including the row with NA time
  expect_equal(surv_row$n_distinct, 5)
})

test_that("get_dataset_info does not error or infinitely recurse on Surv n_distinct", {
  skip_if_not_installed("survival")

  df <- data.frame(surv = survival::Surv(c(1, 2, 3), c(1, 1, 0)))

  expect_no_error(res <- get_dataset_info(df))
  expect_false(is.na(res$n_distinct[1]))
})

test_that("get_dataset_info falls back to attr(x, 'label') when labelled::var_label is unset", {
  x <- 1:5
  attr(x, "label") <- "Base label"
  df <- data.frame(x = x, y = 1:5)

  res <- get_dataset_info(df)

  expect_equal(res |> dplyr::filter(columns == "x") |> dplyr::pull(labels), "Base label")
  expect_equal(res |> dplyr::filter(columns == "y") |> dplyr::pull(labels), "y")
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

test_that("get_dataset_info handles counting-process Surv(start, stop, status) correctly", {
  skip_if_not_installed("survival")

  tstart <- c(0, 5, 0, 10, 0)
  tstop  <- c(5, 12, 10, 25, 8)
  status <- c(0, 1, 0, 1, 0)
  duration <- tstop - tstart

  df <- data.frame(
    id = c(1, 1, 2, 2, 3),
    surv = survival::Surv(tstart, tstop, status)
  )

  res <- get_dataset_info(df)
  surv_row <- res |> dplyr::filter(columns == "surv")

  expect_equal(surv_row$class, "Surv")
  # Duration mean should be mean(tstop - tstart), not mean(tstart)
  expect_equal(surv_row$mean, mean(duration))
  expect_equal(surv_row$sd, stats::sd(duration))
  expect_true(grepl("Counting", surv_row$most_freq))
  expect_true(grepl("Events: 2", surv_row$most_freq))
})

test_that("get_dataset_info audits longitudinal repeated-measures when subject_id is provided", {
  df <- data.frame(
    patient = c(1, 1, 1, 2, 2),
    visit = c(1, 2, 3, 1, 2),
    cd4 = c(450, 420, 390, 510, 530),
    drug = factor(c("ddI", "ddI", "ddI", "ddC", "ddC"))
  )

  res <- get_dataset_info(df, subject_id = "patient")

  expect_true("variable_type" %in% names(res))
  expect_equal(res |> dplyr::filter(columns == "patient") |> dplyr::pull(variable_type), "Subject ID")
  expect_equal(res |> dplyr::filter(columns == "cd4") |> dplyr::pull(variable_type), "Longitudinal (Time-Varying)")
  expect_equal(res |> dplyr::filter(columns == "visit") |> dplyr::pull(variable_type), "Longitudinal (Time-Varying)")
  expect_equal(res |> dplyr::filter(columns == "drug") |> dplyr::pull(variable_type), "Baseline (Time-Invariant)")
})

test_that("get_dataset_info works on fitted joint_model objects", {
  skip_if_not_installed("survival")
  skip_if_not_installed("glmnet")

  set.seed(42)
  df <- data.frame(
    time = stats::rexp(40, rate = 0.05),
    status = stats::rbinom(40, 1, 0.5),
    x1 = stats::rnorm(40),
    x2 = stats::rnorm(40)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  res <- get_dataset_info(fit)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("x1", "x2") %in% res$columns))
  expect_true(any(grepl("Surv", res$columns) | grepl("survival", res$columns)))

  meta <- attr(res, "joint_model_summary")
  expect_false(is.null(meta))
  expect_equal(meta$engine, "glmnet")
  expect_equal(meta$n_obs, 40)
})

test_that("get_dataset_info.list handles multi-table clinical databases", {
  inputs <- data.frame(id = 1:5, age = c(45, 52, 61, 39, 48), sex = c("M", "F", "M", "F", "M"))
  abg <- data.frame(id = c(1, 1, 2, 2, 3), ph = c(7.35, 7.40, 7.28, 7.32, 7.45), pco2 = c(40, 38, 48, 44, 35))
  survival_data <- data.frame(id = 1:5, surv = survival::Surv(c(10, 20, 30, 40, 50), c(1, 0, 1, 0, 1)))

  db <- list(
    inputs = inputs,
    ABG = abg,
    survival_data = survival_data
  )

  cbe_database_name(db) <- "ClinicalTrialDB"
  cbe_dataset_label(db$inputs) <- "Demographics and Baseline Characteristics"
  cbe_dataset_label(db$ABG) <- "Arterial Blood Gas Longitudinal Measurements"

  res <- get_dataset_info(db, subject_id = list(inputs = "id", ABG = "id"))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), ncol(inputs) + ncol(abg) + ncol(survival_data))
  expect_true(all(c("database_name", "dataset_name", "columns", "labels", "class") %in% names(res)))
  expect_equal(unique(res$database_name), "ClinicalTrialDB")
  expect_equal(unique(res$dataset_name), c("inputs", "ABG", "survival_data"))

  abg_ph_row <- res |> dplyr::filter(dataset_name == "ABG", columns == "ph")
  expect_equal(abg_ph_row$variable_type, "Longitudinal (Time-Varying)")

  # Test helper getters and setters
  labels_map <- cbe_get_dataset_labels(db)
  expect_equal(unname(labels_map["inputs"]), "Demographics and Baseline Characteristics")
  expect_true(is.na(labels_map["survival_data"]))

  # Test cbe_set_dataset_labels
  db <- cbe_set_dataset_labels(db, list(survival_data = "Time-to-Event Clinical Followup"))
  expect_equal(cbe_dataset_label(db$survival_data), "Time-to-Event Clinical Followup")
})

