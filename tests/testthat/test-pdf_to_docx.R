test_that("convert_pdf_to_docx validates inputs", {
  expect_error(convert_pdf_to_docx(character(0)), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx(c("a.pdf", "b.pdf")), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx(""), "'src' must be a single non-empty file path.")
})

test_that("convert_pdfs_to_docx returns an empty result for no conversions", {
  res <- convert_pdfs_to_docx(data.frame(src = character(0), dest = character(0)))
  expect_equal(nrow(res), 0)
  expect_true("converted" %in% names(res))
})

test_that("check_docx_toolchain reports every backend without converting", {
  state <- check_docx_toolchain(quiet = TRUE)
  expect_named(state, c("python_with_pdf2docx", "python_any", "soffice", "word_com", "backend"))
  expect_type(state$word_com, "logical")
  if (.Platform$OS.type != "windows") expect_false(state$word_com)
})

test_that("find_soffice honours the templecbe.soffice option", {
  soffice <- withr::local_tempfile()
  file.create(soffice)
  withr::local_options(templecbe.soffice = soffice)
  expect_equal(find_soffice(), soffice)
})
