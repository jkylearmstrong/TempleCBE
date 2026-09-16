test_that("data manifest workflows work end-to-end", {
  tmp <- withr::local_tempdir()
  src_dir <- file.path(tmp, "source_data")
  dst_dir <- file.path(tmp, "frozen_data")
  dir.create(src_dir)
  dir.create(dst_dir)

  # Create source files
  csv_src <- file.path(src_dir, "test.csv")
  rds_src <- file.path(src_dir, "test.rds")
  write.csv(data.frame(x = 1:5, y = letters[1:5]), csv_src, row.names = FALSE)
  saveRDS(data.frame(a = 10:12, b = c(TRUE, FALSE, TRUE)), rds_src)

  # Create manifest
  manifest <- data.frame(
    file   = c("test.csv", "test.rds"),
    source = c("source_data/test.csv", "source_data/test.rds"),
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(dst_dir, "manifest.csv")
  write.csv(manifest, manifest_path, row.names = FALSE)

  # Test read_data_manifest
  read_man <- read_data_manifest(dst_dir, "manifest.csv")
  expect_equal(nrow(read_man), 2)
  expect_equal(read_man$file, c("test.csv", "test.rds"))

  # Test copy_data_manifest
  val <- copy_data_manifest(dir = dst_dir, manifest = manifest, project_root = tmp)
  expect_s3_class(val, "tbl_df")
  expect_true(all(val$valid))
  expect_true(file.exists(file.path(dst_dir, "test.csv")))
  expect_true(file.exists(file.path(dst_dir, "test.rds")))

  # Test validate_data_manifest
  val2 <- validate_data_manifest(dir = dst_dir, manifest = manifest, project_root = tmp)
  expect_true(all(val2$valid))
  expect_true(all(val2$same_content))

  # Test stop_if_invalid_manifest
  expect_silent(stop_if_invalid_manifest(val2, dir = dst_dir))

  # Modify source file to cause desynchronization
  write.csv(data.frame(x = 100:105), csv_src, row.names = FALSE)
  val_stale <- validate_data_manifest(dir = dst_dir, manifest = manifest, project_root = tmp)
  expect_false(all(val_stale$valid))
  expect_error(stop_if_invalid_manifest(val_stale, dir = dst_dir), "do not match their upstream sources")
})
