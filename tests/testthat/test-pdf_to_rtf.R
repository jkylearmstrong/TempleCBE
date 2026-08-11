test_that("pdf_to_rtf works on template PDF", {
  pdf_path <- system.file("templates", "example.pdf", package = "TempleCBE")
  skip_if(pdf_path == "", "Template PDF not found")

  rtf_path <- tempfile(fileext = ".rtf")
  on.exit(if (file.exists(rtf_path)) file.remove(rtf_path), add = TRUE)

  # Run function
  res <- pdf_to_rtf(pdf_path, rtf_path)

  # Check result
  expect_true(file.exists(rtf_path))
  expect_gt(file.info(rtf_path)$size, 0)

  # Check that it contains standard RTF structure
  rtf_lines <- readLines(rtf_path, warn = FALSE)
  expect_match(rtf_lines[1], "^\\{\\\\rtf1")
})
