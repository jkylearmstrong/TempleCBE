test_that("create_toc_from_sas_pdf works on template PDF", {
  pdf_path <- system.file("templates", "t_test_example.pdf", package = "TempleCBE")
  skip_if(pdf_path == "", "Template PDF not found")

  expected_rtf <- file.path(dirname(pdf_path), "TOC_t_test_example.rtf")
  if (file.exists(expected_rtf)) {
    file.remove(expected_rtf)
  }
  on.exit(if (file.exists(expected_rtf)) file.remove(expected_rtf), add = TRUE)

  # Run function
  res <- create_toc_from_sas_pdf(pdf_path, top_margin_height = 150)

  # Check result
  expect_type(res, "character")
  expect_match(res, "TOC saved as RTF:")
  expect_true(file.exists(expected_rtf))
  expect_gt(file.info(expected_rtf)$size, 0)
})
