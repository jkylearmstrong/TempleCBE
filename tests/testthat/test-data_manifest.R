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

test_that("validate_data_manifest handles RDS with closures/environments (A4-08)", {
  tmp <- withr::local_tempdir()
  src_dir <- file.path(tmp, "src")
  dst_dir <- file.path(tmp, "frozen")
  dir.create(src_dir)
  dir.create(dst_dir)

  # Model fit or object containing environment / closure
  fit_obj <- list(
    fit = stats::lm(mpg ~ wt, data = mtcars),
    helper = local(function() 42),
    env = new.env()
  )
  rds_src <- file.path(src_dir, "model.rds")
  saveRDS(fit_obj, rds_src)

  manifest <- data.frame(file = "model.rds", source = "src/model.rds", stringsAsFactors = FALSE)
  val <- copy_data_manifest(dir = dst_dir, manifest = manifest, project_root = tmp)

  expect_true(val$md5_match)
  expect_true(val$same_content)
  expect_true(val$valid)
  expect_silent(stop_if_invalid_manifest(val, dir = dst_dir))
})

test_that("data_manifest rejects path traversal and escapes (A4-20)", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "proj")
  frozen <- file.path(proj, "frozen")
  src <- file.path(proj, "src")
  outside <- file.path(tmp, "outside")
  dir.create(src, recursive = TRUE)
  dir.create(frozen, recursive = TRUE)
  dir.create(outside, recursive = TRUE)

  writeLines("secret", file.path(outside, "secret.txt"))
  writeLines("payload", file.path(src, "payload.txt"))

  # Escaped destination
  bad_dst <- data.frame(file = "../../escaped.txt", source = "src/payload.txt", stringsAsFactors = FALSE)
  expect_error(
    copy_data_manifest(dir = frozen, manifest = bad_dst, project_root = proj),
    "must not contain '..' path segments"
  )
  expect_error(
    validate_data_manifest(dir = frozen, manifest = bad_dst, project_root = proj),
    "must not contain '..' path segments"
  )

  # Escaped source
  bad_src <- data.frame(file = "secret.txt", source = "../outside/secret.txt", stringsAsFactors = FALSE)
  expect_error(
    copy_data_manifest(dir = frozen, manifest = bad_src, project_root = proj),
    "must not contain '..' path segments"
  )

  # Absolute paths
  abs_src <- data.frame(file = "test.txt", source = file.path(proj, "src/payload.txt"), stringsAsFactors = FALSE)
  expect_error(
    copy_data_manifest(dir = frozen, manifest = abs_src, project_root = proj),
    "must be a relative path"
  )
})

test_that("copy_data_manifest overwrite = FALSE protects existing targets (A4-20)", {
  tmp <- withr::local_tempdir()
  src_dir <- file.path(tmp, "src")
  dst_dir <- file.path(tmp, "frozen")
  dir.create(src_dir)
  dir.create(dst_dir)

  writeLines("source_v1", file.path(src_dir, "data.txt"))
  writeLines("precious_local", file.path(dst_dir, "data.txt"))

  man <- data.frame(file = "data.txt", source = "src/data.txt", stringsAsFactors = FALSE)

  # Default overwrite = FALSE must fail
  expect_error(
    copy_data_manifest(dir = dst_dir, manifest = man, project_root = tmp),
    "Destination file\\(s\\) already exist and overwrite is FALSE"
  )
  expect_equal(readLines(file.path(dst_dir, "data.txt")), "precious_local")

  # Explicit overwrite = TRUE succeeds
  val <- copy_data_manifest(dir = dst_dir, manifest = man, project_root = tmp, overwrite = TRUE)
  expect_true(val$valid)
  expect_equal(readLines(file.path(dst_dir, "data.txt")), "source_v1")
})

test_that("read_data_manifest handles UTF-8 BOM headers cleanly", {
  tmp <- withr::local_tempdir()
  mp <- file.path(tmp, "manifest.csv")
  # Excel UTF-8 BOM: 0xEF, 0xBB, 0xBF
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("file,source\ntest.txt,src/test.txt\n")), mp)

  df <- read_data_manifest(tmp)
  expect_equal(names(df), c("file", "source"))
  expect_equal(df$file, "test.txt")
})

test_that("manifest functions handle empty manifests and malformed inputs", {
  tmp <- withr::local_tempdir()
  empty_man <- data.frame(file = character(0), source = character(0), stringsAsFactors = FALSE)

  val_empty <- validate_data_manifest(dir = tmp, manifest = empty_man, project_root = tmp)
  expect_equal(nrow(val_empty), 0)
  expect_true(all(c("file", "source", "valid") %in% names(val_empty)))
  expect_silent(stop_if_invalid_manifest(val_empty, dir = tmp))

  # Non-data frame or missing columns
  expect_error(stop_if_invalid_manifest(list()), "validation must be a data frame")
  expect_error(stop_if_invalid_manifest(data.frame(a = 1)), "containing 'valid' and 'file' columns")
})

test_that(".assert_within_root accepts a file that is not there yet under a root spelled through a link or a short name", {
  d <- withr::local_tempdir()
  real <- file.path(d, "real")
  dir.create(real)
  roots <- character()
  if (.Platform$OS.type == "windows") {
    roots <- c(roots, utils::shortPathName(real))
  } else if (isTRUE(suppressWarnings(file.symlink(real, file.path(d, "link"))))) {
    roots <- c(roots, file.path(d, "link"))
  }
  skip_if(length(roots) == 0L, "no symbolic link or 8.3 short name available")
  # normalizePath() resolves the root (it exists) but not the new file in it,
  # so the two used to be spelled differently and the file counted as an escape
  for (root in roots) {
    expect_true(.assert_within_root("new.csv", root, "file"))
  }
  # an escape is still refused
  expect_error(.assert_within_root("../x.csv", real, "file"), "must not contain")
})
