test_that("output_format_extensions maps format names to their real file extensions", {
  expect_equal(output_format_extensions("html"), "html")
  expect_equal(output_format_extensions(c("html", "pdf", "docx")), "html|pdf|docx")
  # Regression test: formats whose extension doesn't match their name.
  expect_equal(output_format_extensions("revealjs"), "html")
  expect_equal(output_format_extensions("beamer"), "pdf")
  # Unknown format names pass through as their own extension.
  expect_equal(output_format_extensions("pptx"), "pptx")
  expect_match(output_format_extensions("all"), "pptx")
})

test_that("zip_render includes outputs for formats beyond html/pdf/docx", {
  # Regression test: a prior bug hardcoded the output glob to
  # html|pdf|docx regardless of the `formats` argument, so any other
  # requested format's output silently never made it into the zip.
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_zip_render_gfm")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c(
    "---",
    "title: \"Minimal Test\"",
    "format: gfm",
    "---",
    "",
    "## Hello World",
    "This is a minimal test for zip_render."
  ), qmd_file)

  res <- zip_render(
    input = qmd_file,
    formats = "gfm",
    build_dir = file.path(tmp_dir, "build"),
    copy_back_dir = tmp_dir,
    verbose = FALSE
  )

  expect_length(res$outputs, 1)
  expect_match(res$outputs[1], "minimal\\.md$")
  expect_true(file.exists(res$outputs[1]))
})

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
