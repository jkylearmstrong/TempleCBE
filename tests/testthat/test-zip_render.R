test_that("zip_render works correctly for HTML output", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  # Create a temporary directory for the test
  tmp_dir <- file.path(tempdir(), "test_zip_render")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Write a minimal QMD file
  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c(
    "---",
    "title: \"Minimal Test\"",
    "format: html",
    "---",
    "",
    "## Hello World",
    "This is a minimal test for zip_render."
  ), qmd_file)

  # Run zip_render
  res <- zip_render(
    input = qmd_file,
    formats = "html",
    build_dir = file.path(tmp_dir, "build"),
    copy_back_dir = tmp_dir,
    verbose = FALSE
  )

  # Verify output list fields
  expect_type(res, "list")
  expect_true(file.exists(res$zip))
  expect_equal(basename(res$zip), "minimal.zip")

  # Verify that the outputs include the html file
  expect_length(res$outputs, 1)
  expect_match(res$outputs[1], "minimal\\.html$")
  expect_true(file.exists(res$outputs[1]))

  # Verify files inside the zip
  zip_files <- zip::zip_list(res$zip)$filename
  expect_true(any(grepl("minimal\\.html$", zip_files)))
  expect_true(any(grepl("minimal\\.qmd$", zip_files)))
})
