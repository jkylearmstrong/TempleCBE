test_that("write_database_to_excel and read_database_from_excel work with metadata", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  db <- list(
    patients = data.frame(id = 1:3, age = c(40, 50, 60), sex = c("M", "F", "M")),
    labs = data.frame(id = c(1, 1, 2), wbc = c(4.5, 5.0, 7.2))
  )

  # Write workbook with auto metadata
  write_database_to_excel(db, path = tmp_file, include_metadata = TRUE)
  expect_true(file.exists(tmp_file))

  # Read back
  read_db <- read_database_from_excel(tmp_file, include_metadata = TRUE)
  expect_equal(names(read_db), c("patients", "labs"))
  expect_equal(nrow(read_db$patients), 3)
  expect_equal(nrow(read_db$labs), 3)

  meta <- attr(read_db, "metadata")
  expect_false(is.null(meta))
  expect_true(all(c("dataset_name", "columns", "class") %in% names(meta)))
  expect_equal(sort(unique(meta$dataset_name)), c("labs", "patients"))
})

test_that("write_database_metadata and read_database_metadata work for CSV and XLSX", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  meta_sample <- tibble::tibble(
    dataset_name = c("demo", "demo"),
    columns = c("id", "sbp"),
    labels = c("Subject ID", "Systolic Blood Pressure"),
    role = c("id", "predictor")
  )

  tmp_csv <- tempfile(fileext = ".csv")
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(tmp_csv, tmp_xlsx)), add = TRUE)

  # CSV roundtrip
  write_database_metadata(meta_sample, tmp_csv)
  read_csv_meta <- read_database_metadata(tmp_csv)
  expect_equal(nrow(read_csv_meta), 2)
  expect_equal(read_csv_meta$labels, meta_sample$labels)

  # XLSX roundtrip
  write_database_metadata(meta_sample, tmp_xlsx)
  read_xlsx_meta <- read_database_metadata(tmp_xlsx)
  expect_equal(nrow(read_xlsx_meta), 2)
  expect_equal(read_xlsx_meta$labels, meta_sample$labels)
})

test_that("apply_database_metadata updates dataset labels and variable attributes", {
  db <- list(
    patients = data.frame(id = 1:2, sbp = c(120, 130))
  )

  meta <- tibble::tibble(
    dataset_name = c("patients", "patients"),
    columns = c("id", "sbp"),
    labels = c("Patient Identifier", "Systolic Blood Pressure (mmHg)"),
    dataset_label = c("Cohort Enrollment", "Cohort Enrollment"),
    database_name = c("ClinicalDB", "ClinicalDB"),
    role = c("id", "predictor")
  )

  updated_db <- apply_database_metadata(db, meta)

  expect_equal(cbe_database_name(updated_db), "ClinicalDB")
  expect_equal(cbe_dataset_label(updated_db$patients), "Cohort Enrollment")
  expect_equal(as.character(labelled::var_label(updated_db$patients$sbp)), "Systolic Blood Pressure (mmHg)")
  expect_equal(as.character(labelled::var_label(updated_db$patients$id)), "Patient Identifier")
})
