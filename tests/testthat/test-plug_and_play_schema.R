test_that("validate_column_mapping accepts valid mapping and rejects malformed mappings", {
  map <- demo_cbe_mapping()
  expect_true(validate_column_mapping(map))

  # Missing required column
  bad_map <- map[, -1]
  expect_error(validate_column_mapping(bad_map), "missing required column")

  # Non-logical flag
  bad_map2 <- map
  bad_map2$X_var <- "TRUE"
  expect_error(validate_column_mapping(bad_map2), "must be logical")

  # Invalid duplicate_action
  bad_map3 <- map
  bad_map3$duplicate_action[1] <- "invalid_action"
  expect_error(validate_column_mapping(bad_map3), "duplicate_action must be 'drop', 'prefer', or NA")
})

test_that("read_raw_table and read_mapped_section_data work with CSV files", {
  tmp <- withr::local_tempdir()

  raw_df <- data.frame(
    Subject  = c("S1", "S2", "S3"),
    Visit    = c("V1", "V1", "V1"),
    MAP_mmHg = c(95, 102, 88),
    Outcome  = c(0, 1, 0),
    stringsAsFactors = FALSE
  )
  csv_file <- file.path(tmp, "demo_data.csv")
  write.csv(raw_df, csv_file, row.names = FALSE)

  raw_table <- read_raw_table(csv_file)
  expect_s3_class(raw_table, "tbl_df")
  expect_equal(nrow(raw_table), 3)

  found_file <- find_section_file(tmp, "demo")
  expect_equal(normalizePath(found_file), normalizePath(csv_file))

  mapping <- demo_cbe_mapping()
  mapped_data <- read_mapped_section_data(mapping, index = "Demo", file = csv_file)
  expect_s3_class(mapped_data, "tbl_df")
  expect_equal(names(mapped_data), c("subject_id", "time_point", "map_mean", "status"))
  expect_equal(mapped_data$subject_id, c("S1", "S2", "S3"))

  # Test summarize_section_by_time with default gtsummary engine
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    tbl_res <- summarize_section_by_time(mapped_data, mapping, index = "Demo", engine = "gtsummary")
    expect_s3_class(tbl_res, "tbl_summary")
  }
})

