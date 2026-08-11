test_that("toc_titles_with_pages keeps titles aligned to their true PDF page number", {
  # Regression test: a prior bug used the position within the *filtered*
  # vector as the page number, which drifted from the true page as soon as
  # any page (e.g. a blank continuation page) had no top-margin text.
  page <- function(y, text) data.frame(x = seq_along(text), y = y, text = text)

  pdf_data_info <- list(
    page(50, c("Table", "1")),  # page 1: has a title
    page(500, c("some", "body", "text")),  # page 2: blank at the top -> no title
    page(50, c("Table", "2")),  # page 3: has a title
    data.frame(x = integer(0), y = numeric(0), text = character(0)), # page 4: no text at all
    page(50, c("Table", "3"))  # page 5: has a title
  )

  res <- toc_titles_with_pages(pdf_data_info, top_margin_height = 100)

  expect_equal(res$titles, c("Table 1", "Table 2", "Table 3"))
  # true page numbers, NOT 1:3 (the old, buggy behavior)
  expect_equal(res$pages, c(1L, 3L, 5L))
})

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
