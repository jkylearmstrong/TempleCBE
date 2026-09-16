test_that("pdf_to_rtf writes one RTF page per PDF page with real page breaks", {
  pdf_path <- system.file("templates", "example.pdf", package = "TempleCBE")
  skip_if(pdf_path == "", "Template PDF not found")

  rtf_path <- withr::local_tempfile(fileext = ".rtf")
  expect_identical(pdf_to_rtf(pdf_path, rtf_path), rtf_path)

  rtf <- paste(readLines(rtf_path, warn = FALSE), collapse = "\n")
  expect_match(rtf, "^\\{\\\\rtf1")
  expect_match(rtf, "\\}$")

  n_pages <- pdftools::pdf_info(pdf_path)$pages
  page_breaks <- lengths(regmatches(rtf, gregexpr("\n\\page\n", rtf, fixed = TRUE)))
  expect_equal(page_breaks, n_pages - 1)
  # A doubled backslash would print "\page" as text instead of breaking the page.
  expect_false(grepl("\\\\page", rtf, fixed = TRUE))
  # Every character is ASCII.
  expect_true(all(utf8ToInt(rtf) <= 127))
})

test_that("pdf_to_rtf defaults the output path and respects overwrite", {
  pdf_path <- system.file("templates", "example.pdf", package = "TempleCBE")
  skip_if(pdf_path == "", "Template PDF not found")

  dir <- withr::local_tempdir()
  pdf_copy <- file.path(dir, "report.PDF")
  file.copy(pdf_path, pdf_copy)

  expect_identical(pdf_to_rtf(pdf_copy), file.path(dir, "report.rtf"))
  expect_error(pdf_to_rtf(pdf_copy, overwrite = FALSE), "already exists")
})

test_that("pdf_to_rtf validates its inputs", {
  expect_error(pdf_to_rtf("does-not-exist.pdf"), "existing file")
  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(pdf_to_rtf(txt, tempfile()), "must be a .pdf")
})

test_that("rtf_escape_text escapes control characters, whitespace, and Unicode", {
  expect_identical(rtf_escape_text("a{b}\\c"), "a\\{b\\}\\\\c")
  expect_identical(rtf_escape_text("x\ty\r\nz\n\n"), "x\\tab y\\par\nz")
  expect_identical(rtf_escape_text("café"), "caf\\u233?")
  # Above U+7FFF the value is written as a signed 16-bit integer.
  expect_identical(rtf_escape_text("ﬁ"), "\\u-1279?")
  # Above U+FFFF: a UTF-16 surrogate pair.
  expect_identical(rtf_escape_text(intToUtf8(0x1F600)), "\\u-10179?\\u-8704?")
})
