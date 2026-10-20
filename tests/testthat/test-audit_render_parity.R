# audit_render_parity(): PDF fixtures are drawn with base graphics at known positions (inches from
# the bottom-left of a US-letter page); Word fixtures are built from XML. All invented.

rp_pdf <- function(path, items) {
  grDevices::pdf(path, width = 8.5, height = 11, family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 8.5), ylim = c(0, 11))
  for (it in items) {
    if (!is.null(it$page) && it$page > 1) next
    graphics::text(it$x %||% 1.2, it$y, it$text, adj = c(0, 0.5), cex = 0.9)
  }
  invisible(path)
}

# A minimal .docx whose body is the XML string `body`
rp_docx <- function(path, body) {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "word"))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ), file.path(td, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    "<w:body>", body, "</w:body></w:document>"
  ), file.path(td, "word", "document.xml"), useBytes = TRUE)
  zip::zipr(path, files = list.files(td, all.files = TRUE, no.. = TRUE), root = td)
  invisible(path)
}

rp_p <- function(text, style = NULL) {
  ppr <- if (is.null(style)) "" else sprintf('<w:pPr><w:pStyle w:val="%s"/></w:pPr>', style)
  sprintf("<w:p>%s<w:r><w:t>%s</w:t></w:r></w:p>", ppr, text)
}

rp_tbl <- function(...) {
  rows <- list(...)
  tr <- vapply(rows, function(cells) {
    paste0("<w:tr>", paste0(sprintf("<w:tc><w:p><w:r><w:t>%s</w:t></w:r></w:p></w:tc>", cells), collapse = ""), "</w:tr>")
  }, "")
  paste0("<w:tbl>", paste(tr, collapse = ""), "</w:tbl>")
}

# the reference report: Table 1 and Table 2 with their captions above, Figure 1, a sentence with a percentage
rp_items <- function(t2_caption_y = 8, order_swapped = FALSE, pct = "98.9%", extra = list()) {
  t1 <- list(
    list(y = 10, text = "Table 1: Demographics"), list(y = 9.6, text = "Characteristic Value"), list(y = 9.3, text = "Age 45")
  )
  t2 <- list(
    list(y = t2_caption_y, text = "Table 2: Outcomes"), list(y = 7.6, text = "Outcome Rate"), list(y = 7.3, text = "Response 12")
  )
  if (order_swapped) {
    t1 <- lapply(t1, function(i) { i$y <- i$y - 3; i })
    t2 <- lapply(t2, function(i) { i$y <- i$y + 3; i })
  }
  c(
    t1, t2,
    list(list(y = 5, text = "Figure 1: Survival curve"), list(y = 4, text = paste("Response was", pct, "overall"))),
    extra
  )
}

rp_docx_body <- function(skip_table2_caption = FALSE, pct = "98.9%", swap = FALSE) {
  t1 <- paste0(rp_p("Table 1: Demographics", "Caption"), rp_tbl(c("Characteristic", "Value"), c("Age", "45")))
  t2 <- paste0(
    if (skip_table2_caption) "" else rp_p("Table 2: Outcomes", "Caption"),
    rp_tbl(c("Outcome", "Rate"), c("Response", "12"))
  )
  paste0(
    if (swap) paste0(t2, t1) else paste0(t1, t2),
    rp_p("Figure 1: Survival curve", "Caption"),
    rp_p(paste("Response was", pct, "overall"))
  )
}

rp_files <- function(items = rp_items(), body = rp_docx_body(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  pdf <- file.path(d, "report.pdf")
  docx <- file.path(d, "report.docx")
  rp_pdf(pdf, items)
  rp_docx(docx, body)
  list(pdf = pdf, docx = docx)
}

test_that("matching PDF and Word files give no findings", {
  skip_if_not_installed("pdftools")
  f <- rp_files()
  res <- audit_render_parity(f$pdf, f$docx)
  expect_s3_class(res, "render_parity")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("check", "severity", "location", "detail"))
  expect_identical(nrow(res), 0L)
  expect_identical(attr(res, "n_pages"), 1L)
  expect_setequal(attr(res, "captions")$pdf$label, c("Table 1", "Table 2", "Figure 1"))
  expect_setequal(attr(res, "captions")$docx$label, c("Table 1", "Table 2", "Figure 1"))
  expect_output(print(res), "No differences found")
})

test_that("text too close to the page edge is flagged with its page and text", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(extra = list(
    list(x = 6.4, y = 2, text = "E = mc^2 + a_very_long_formula_that_runs_out_of_the_text_area")
  )))
  res <- audit_render_parity(f$pdf, f$docx)
  hit <- res[res$check == "margin_overflow", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$location, "page 1")
  expect_match(hit$detail, "a_very") # the PDF engine clips what runs off the page
  expect_identical(hit$severity, "warning")
  # a wider allowance finds nothing more here, a stricter one finds the ordinary text too
  expect_identical(nrow(audit_render_parity(f$pdf, f$docx, margin_in = 0.2, check = "margins")), 1L)
  expect_gt(nrow(audit_render_parity(f$pdf, f$docx, margin_in = 1.5, check = "margins")), 1L)
})

test_that("a table caption above the table in Word and below it in the PDF is flagged", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(t2_caption_y = 6.9))
  res <- audit_render_parity(f$pdf, f$docx)
  hit <- res[res$check == "caption_placement", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$location, "Table 2")
  expect_match(hit$detail, "above the table in the Word file and below it in the PDF")
  expect_identical(nrow(res[res$check != "caption_placement", ]), 0L)
})

test_that("a table that cannot be located in the PDF is reported as info, not as a difference", {
  skip_if_not_installed("pdftools")
  items <- Filter(function(i) !grepl("Outcome Rate", i$text), rp_items())
  f <- rp_files(items = items)
  res <- audit_render_parity(f$pdf, f$docx)
  hit <- res[res$check == "caption_placement", ]
  expect_identical(hit$severity, "info")
  expect_match(hit$detail, "not found in the PDF")
})

test_that("captions in another order are flagged", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(order_swapped = TRUE))
  res <- audit_render_parity(f$pdf, f$docx, check = "order")
  expect_identical(nrow(res), 1L)
  expect_identical(res$location, "Tables")
  expect_match(res$detail, "Word has Table 1, Table 2; PDF has Table 2, Table 1")
})

test_that("a caption missing from one file, and numbers that skip, are flagged", {
  skip_if_not_installed("pdftools")
  f <- rp_files(body = rp_docx_body(skip_table2_caption = TRUE))
  res <- audit_render_parity(f$pdf, f$docx, check = "numbering")
  expect_identical(nrow(res), 1L)
  expect_identical(res$location, "Table 2")
  expect_match(res$detail, "caption in the PDF but none was found in the Word file")

  items <- Filter(function(i) !grepl("^Table 2", i$text), rp_items())
  items <- c(items, list(list(y = 6, text = "Table 4: Extra")))
  f2 <- rp_files(items = items, body = rp_docx_body(skip_table2_caption = TRUE))
  res2 <- audit_render_parity(f2$pdf, f2$docx, check = "numbering")
  expect_true(any(grepl("numbering skips 2, 3", res2$detail)))
})

test_that("a number that differs between the files is flagged both ways", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(pct = "97%"))
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_setequal(res$location, c("97%", "98.9%"))
  expect_match(res$detail[res$location == "98.9%"], "1 time\\(s\\) in the Word file and 0 time\\(s\\) in the PDF")
  expect_match(res$detail[res$location == "97%"], "0 time\\(s\\) in the Word file and 1 time\\(s\\) in the PDF")
})

test_that("math in Word is read as text and a sentence that starts with Table is not a caption", {
  skip_if_not_installed("pdftools")
  prose <- rp_p("Table 3 shows the outcome in detail.")
  f <- rp_files(body = paste0(rp_docx_body(), prose))
  res <- audit_render_parity(f$pdf, f$docx)
  expect_false("Table 3" %in% attr(res, "captions")$docx$label)
  expect_identical(nrow(res), 0L)
})

test_that("audit_render_parity() checks its arguments and the files", {
  skip_if_not_installed("pdftools")
  f <- rp_files()
  expect_error(audit_render_parity(file.path(dirname(f$pdf), "none.pdf"), f$docx), "File not found")
  expect_error(audit_render_parity(f$docx, f$docx), "must be a .pdf file")
  expect_error(audit_render_parity(f$pdf, f$pdf), "must be a .docx file")
  expect_error(audit_render_parity(f$pdf, f$docx, margin_in = -1), "margin_in")
  expect_error(audit_render_parity(f$pdf, f$docx, check = "colours"), "should be one of")
  bad <- file.path(dirname(f$pdf), "bad.docx")
  writeLines("not a zip", bad)
  expect_error(audit_render_parity(f$pdf, bad), "could not be read as a .docx archive")
  expect_error(
    audit_render_parity(f$pdf, f$docx, docx_limits = list(max_entries = 1)),
    "limit is 1"
  )
})
