# audit_render_parity(): PDF fixtures are drawn with base graphics at known positions (inches from
# the bottom-left of a US-letter page); Word fixtures are built from XML, flat and in the shape
# Quarto/pandoc writes (a one-cell layout table around each caption and table, a no-break space
# after "Table", the ImageCaption style). All text is invented.

rp_pdf <- function(path, items) {
  grDevices::pdf(path, width = 8.5, height = 11, family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  pages <- sort(unique(vapply(items, function(it) it$page %||% 1, numeric(1))))
  for (pg in pages) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 8.5), ylim = c(0, 11))
    for (it in items) {
      if ((it$page %||% 1L) != pg) next
      graphics::text(it$x %||% 1.2, it$y, it$text, adj = c(0, 0.5), cex = 0.9)
    }
  }
  invisible(path)
}

# A minimal .docx whose body is the XML string `body`; `parts` are further parts by archive path
rp_docx <- function(path, body, parts = list()) {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "word"))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ), file.path(td, "[Content_Types].xml"), useBytes = TRUE)
  ns <- paste0(
    'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
    'xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" ',
    'xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"'
  )
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    "<w:document ", ns, "><w:body>", body, "</w:body></w:document>"
  ), file.path(td, "word", "document.xml"), useBytes = TRUE)
  for (nm in names(parts)) {
    writeLines(sub("%NS%", ns, parts[[nm]], fixed = TRUE), file.path(td, nm), useBytes = TRUE)
  }
  zip::zipr(path, files = list.files(td, all.files = TRUE, no.. = TRUE), root = td)
  invisible(path)
}

NBSP <- "&#160;"

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

# What Quarto writes for a cross-referenced table: a one-cell layout table holding the caption
# paragraph and the real table
rp_qtable <- function(caption, ..., above = TRUE) {
  cap <- rp_p(caption, "ImageCaption")
  inner <- rp_tbl(...)
  paste0("<w:tbl><w:tr><w:tc>", if (above) paste0(cap, inner) else paste0(inner, cap), "</w:tc></w:tr></w:tbl>")
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

rp_docx_body <- function(skip_table2_caption = FALSE, pct = "98.9%", swap = FALSE, extra = "") {
  t1 <- rp_qtable(paste0("Table", NBSP, "1: Demographics"), c("Characteristic", "Value"), c("Age", "45"))
  t2 <- if (skip_table2_caption) {
    rp_tbl(c("Outcome", "Rate"), c("Response", "12"))
  } else {
    rp_qtable(paste0("Table", NBSP, "2: Outcomes"), c("Outcome", "Rate"), c("Response", "12"))
  }
  paste0(
    if (swap) paste0(t2, t1) else paste0(t1, t2),
    rp_p(paste0("Figure", NBSP, "1: Survival curve"), "ImageCaption"),
    rp_p(paste("Response was", pct, "overall")),
    extra
  )
}

rp_files <- function(items = rp_items(), body = rp_docx_body(), parts = list(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  pdf <- file.path(d, "report.pdf")
  docx <- file.path(d, "report.docx")
  rp_pdf(pdf, items)
  rp_docx(docx, body, parts)
  list(pdf = pdf, docx = docx, dir = d)
}

# ---- the clean case -------------------------------------------------------------------------------

test_that("matching PDF and Word files (Quarto-style Word body) give no findings", {
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
  # captions through the layout wrapper, with a no-break space, and their side of the table
  cap <- attr(res, "captions")$docx
  expect_identical(cap$placement[cap$label == "Table 1"], "above")
  expect_output(print(res), "No differences found")
})

test_that("table text is counted once, not once for the layout table and once for the table", {
  skip_if_not_installed("pdftools")
  body <- rp_docx_body(extra = rp_qtable(paste0("Table", NBSP, "3: Rates"), c("Id", "Rate"), c("S001", "26.6%"), c("S002", "37.2%")))
  items <- c(rp_items(), list(
    list(y = 3, text = "Table 3: Rates"), list(y = 2.6, text = "Id Rate"), list(y = 2.3, text = "S001 26.6%"), list(y = 2.0, text = "S002 37.2%")
  ))
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_identical(nrow(res), 0L)
})

# ---- caption parsing ------------------------------------------------------------------------------

test_that("captions are told from prose, whatever the case, dash or kind of space", {
  yes <- c("Table 1: Demographics", "Table 2. Outcomes", "TABLE 3. Outcomes", "table 4: x", "Figure 5: Curve", "Fig. 6: Curve",
           "Table 7: Demographics", "Table 8 – Outcomes", "Table 9—Outcomes", "Table 10.1: Sub", "Table 11:")
  no <- c("Table 1 shows the outcome", "Table 1.5 mg were given", "Table 2 - the results", "Tables 1: x", "In Table 1: we", "Figure1: x", "Table: x")
  for (s in yes) expect_false(is.null(.rp_parse_caption(s)), info = s)
  for (s in no) expect_null(.rp_parse_caption(s), info = s)
  expect_identical(.rp_parse_caption("Table 7: Demographics")$label, "Table 7")
  expect_identical(.rp_parse_caption("Fig. 6: Curve")$label, "Figure 6")
  expect_identical(.rp_parse_caption("Table 10.1: Sub")$number, "10.1")
  # a paragraph in a caption style needs no separator
  expect_null(.rp_parse_caption("Table 3 and more"))
  expect_identical(.rp_parse_caption("Table 3 and more", style_caption = TRUE)$label, "Table 3")
})

# ---- margins ------------------------------------------------------------------------------------

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
  expect_identical(nrow(audit_render_parity(f$pdf, f$docx, margin_in = 0.2, check = "margins")), 1L)
  expect_gt(nrow(audit_render_parity(f$pdf, f$docx, margin_in = 1.5, check = "margins")), 1L)
})

# ---- placement ------------------------------------------------------------------------------------

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

test_that("two tables with the same header row do not confuse each other", {
  skip_if_not_installed("pdftools")
  # captions BELOW their tables in both files, both tables start with "Characteristic Value"
  body <- paste0(
    rp_qtable(paste0("Table", NBSP, "1: First"), c("Characteristic", "Value"), c("Age", "45"), above = FALSE),
    rp_qtable(paste0("Table", NBSP, "2: Second"), c("Characteristic", "Value"), c("Sex", "F"), above = FALSE)
  )
  items <- list(
    list(y = 10, text = "Characteristic Value"), list(y = 9.7, text = "Age 45"), list(y = 9.3, text = "Table 1: First"),
    list(y = 7.6, text = "Characteristic Value"), list(y = 7.3, text = "Sex F"), list(y = 6.9, text = "Table 2: Second")
  )
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "placement")
  expect_identical(nrow(res[res$severity == "warning", ]), 0L)
})

test_that("a prose line before the caption that starts like the header is not taken for the table", {
  skip_if_not_installed("pdftools")
  body <- paste0(rp_p("Characteristic ages are summarised below."), rp_qtable(paste0("Table", NBSP, "1: First"), c("Characteristic", "Value"), c("Age", "45")))
  items <- list(
    list(y = 10.4, text = "Characteristic ages are summarised below."), list(y = 10, text = "Table 1: First"),
    list(y = 9.6, text = "Characteristic Value"), list(y = 9.3, text = "Age 45")
  )
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "placement")
  expect_identical(nrow(res[res$severity == "warning", ]), 0L)
})

test_that("a table that cannot be located, or is ambiguous, in the PDF is reported as info", {
  skip_if_not_installed("pdftools")
  items <- Filter(function(i) !grepl("Outcome Rate", i$text), rp_items())
  f <- rp_files(items = items)
  res <- audit_render_parity(f$pdf, f$docx, check = "placement")
  expect_identical(res$severity, "info")
  expect_match(res$detail, "no line starting with")
  # the same header twice right under one caption: ambiguous
  items2 <- c(rp_items(), list(list(y = 7.72, text = "Outcome Rate"), list(y = 8.28, text = "Outcome Rate")))
  f2 <- rp_files(items = items2)
  res2 <- audit_render_parity(f2$pdf, f2$docx, check = "placement")
  expect_identical(nrow(res2), 1L)
  expect_identical(res2$severity, "info")
  expect_match(paste(res2$detail, collapse = " "), "more than one place")
})

test_that("an empty paragraph between a caption and its table does not hide the table", {
  skip_if_not_installed("pdftools")
  body <- paste0(
    rp_p(paste0("Table", NBSP, "1: Demographics"), "Caption"), rp_p(""), rp_tbl(c("Characteristic", "Value"), c("Age", "45")),
    rp_p(paste0("Table", NBSP, "2: Outcomes"), "Caption"), rp_tbl(c("Outcome", "Rate"), c("Response", "12"))
  )
  f <- rp_files(items = rp_items(t2_caption_y = 6.9), body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "placement")
  expect_identical(res$location, "Table 2")
})

test_that("content controls and layout tables are looked through", {
  skip_if_not_installed("pdftools")
  inner <- rp_qtable(paste0("Table", NBSP, "1: Demographics"), c("Characteristic", "Value"), c("Age", "45"))
  body <- sub(inner, paste0("<w:sdt><w:sdtContent>", inner, "</w:sdtContent></w:sdt>"), rp_docx_body(), fixed = TRUE)
  f <- rp_files(body = body)
  res <- audit_render_parity(f$pdf, f$docx)
  expect_identical(nrow(res), 0L)
  expect_true("Table 1" %in% attr(res, "captions")$docx$label)
})

# ---- order and numbering --------------------------------------------------------------------------

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

test_that("the repeated caption of a table that runs over several pages counts once", {
  skip_if_not_installed("pdftools")
  items <- list(
    list(page = 1, y = 10, text = "Table 1: A long table"), list(page = 1, y = 9.6, text = "Id Rate"),
    list(page = 2, y = 10, text = "Table 1: A long table"), list(page = 2, y = 9.6, text = "S100 5.5%"),
    list(page = 3, y = 10, text = "Table 1: A long table"), list(page = 3, y = 9.6, text = "S200 6.5%")
  )
  body <- rp_qtable(paste0("Table", NBSP, "1: A long table"), c("Id", "Rate"), c("S100", "5.5%"), c("S200", "6.5%"))
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = c("numbering", "order"))
  expect_identical(nrow(res), 0L)
  expect_identical(sum(attr(res, "captions")$pdf$label == "Table 1"), 1L)
})

# ---- numbers --------------------------------------------------------------------------------------

test_that("a number that differs between the files is flagged both ways", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(pct = "97%"))
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_setequal(res$location, c("97%", "98.9%"))
  expect_match(res$detail[res$location == "98.9%"], "1 time\\(s\\) in the Word file and 0 time\\(s\\) in the PDF")
  expect_match(res$detail[res$location == "97%"], "0 time\\(s\\) in the Word file and 1 time\\(s\\) in the PDF")
})

test_that("footnotes, running headers, contents lines and section numbers do not make false differences", {
  skip_if_not_installed("pdftools")
  notes <- '<?xml version="1.0" encoding="UTF-8"?><w:footnotes %NS%><w:footnote w:type="separator" w:id="-1"><w:p><w:r><w:t>9.9%</w:t></w:r></w:p></w:footnote><w:footnote w:id="1"><w:p><w:r><w:t>Footnote with 3.7% in it</w:t></w:r></w:p></w:footnote></w:footnotes>'
  items <- list(
    list(page = 1, y = 10.5, text = "Running header 5.5%"), list(page = 2, y = 10.5, text = "Running header 5.5%"),
    list(page = 3, y = 10.5, text = "Running header 5.5%"),
    list(page = 1, y = 9, text = "1.1 Methods"),
    list(page = 1, y = 8.5, text = "Response was 98.9% overall"),
    list(page = 1, y = 8, text = "Table 1: Demographics ......... 3"),
    list(page = 1, y = 7.5, text = "Footnote with 3.7% in it"),
    list(page = 2, y = 9, text = "Plain page two"), list(page = 3, y = 9, text = "Plain page three")
  )
  body <- paste0(
    rp_p("1.1 Methods", "Heading2"), rp_p("Response was 98.9% overall"),
    rp_p(paste0("Table", NBSP, "1: Demographics 12.5%\t3"), "TableofFigures")
  )
  f <- rp_files(items = items, body = body, parts = list("word/footnotes.xml" = notes))
  res <- audit_render_parity(f$pdf, f$docx, check = c("numbers", "numbering"))
  expect_identical(nrow(res), 0L)
})

# ---- hostile and broken files -------------------------------------------------------------------

test_that("audit_render_parity() checks its arguments and the files", {
  skip_if_not_installed("pdftools")
  f <- rp_files()
  expect_error(audit_render_parity(file.path(f$dir, "none.pdf"), f$docx), "File not found")
  expect_error(audit_render_parity(f$docx, f$docx), "must be a .pdf file")
  expect_error(audit_render_parity(f$pdf, f$pdf), "must be a .docx file")
  expect_error(audit_render_parity(f$pdf, f$docx, margin_in = -1), "margin_in")
  expect_error(audit_render_parity(f$pdf, f$docx, check = "colours"), "should be one of")
  bad <- file.path(f$dir, "bad.docx")
  writeLines("not a zip", bad)
  expect_error(audit_render_parity(f$pdf, bad), "could not be read as a .docx archive")
  expect_error(audit_render_parity(f$pdf, f$docx, docx_limits = list(max_entries = 1)), "limit is 1")
  # a PDF that is not a PDF
  junk <- file.path(f$dir, "junk.pdf")
  writeLines("this is not a pdf", junk)
  expect_error(audit_render_parity(junk, f$docx), "The PDF could not be read")
})

test_that("a Word file without a body, with broken XML or with an external entity is refused or ignored safely", {
  skip_if_not_installed("pdftools")
  f <- rp_files()
  mk <- function(xml) {
    td <- withr::local_tempdir()
    dir.create(file.path(td, "word"))
    writeLines('<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"/>', file.path(td, "[Content_Types].xml"))
    writeLines(xml, file.path(td, "word", "document.xml"), useBytes = TRUE)
    out <- file.path(f$dir, paste0("x", sample.int(1e6, 1), ".docx"))
    zip::zipr(out, files = list.files(td, all.files = TRUE, no.. = TRUE), root = td)
    out
  }
  w <- 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"'
  expect_error(audit_render_parity(f$pdf, mk(paste0("<w:document ", w, "><w:p/></w:document>"))), "no document body")
  expect_error(audit_render_parity(f$pdf, mk(paste0("<w:document ", w, "><w:body><w:p>"))), "could not be parsed")
  secret <- file.path(f$dir, "secret.txt")
  writeLines("TOPSECRET-12.5%", secret)
  xxe <- paste0('<?xml version="1.0"?><!DOCTYPE d [<!ENTITY x SYSTEM "file://', gsub("\\\\", "/", secret), '">]><w:document ', w,
                '><w:body><w:p><w:r><w:t>&x;</w:t></w:r></w:p></w:body></w:document>')
  res <- suppressWarnings(audit_render_parity(f$pdf, mk(xxe), check = "numbers"))
  expect_false(any(grepl("TOPSECRET|12\\.5", paste(res$location, res$detail))))
})

test_that("print.render_parity() accepts its own arguments", {
  skip_if_not_installed("pdftools")
  f <- rp_files(items = rp_items(pct = "97%"))
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_output(print(res, n = 1), "difference\\(s\\) to look at")
})

# ---- second review ---------------------------------------------------------------------------------

test_that("rows of a long table in the page strips are not taken for running headers", {
  skip_if_not_installed("pdftools")
  # every page has a row of the same shape at its top and bottom edge, as a long table does
  items <- list()
  for (pg in 1:6) {
    items <- c(items, list(
      list(page = pg, y = 10.6, text = sprintf("S%03d %.1f%%", pg * 10, 10 + pg + 0.7)),
      list(page = pg, y = 5, text = "Body text of the page"),
      list(page = pg, y = 0.8, text = sprintf("S%03d %.1f%%", pg * 10 + 5, 20 + pg + 0.3))
    ))
  }
  rows <- lapply(1:6, function(pg) list(c(sprintf("S%03d", pg * 10), sprintf("%.1f%%", 10 + pg + 0.7)),
                                         c(sprintf("S%03d", pg * 10 + 5), sprintf("%.1f%%", 20 + pg + 0.3))))
  body <- do.call(rp_tbl, c(list(c("Id", "Rate")), unlist(rows, recursive = FALSE)))
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_identical(nrow(res), 0L)
})

test_that("a page number in a running footer is still recognised as one", {
  skip_if_not_installed("pdftools")
  items <- list()
  for (pg in 1:4) items <- c(items, list(list(page = pg, y = 0.6, text = sprintf("Page %d of 4", pg)), list(page = pg, y = 6, text = "Body")))
  items <- c(items, list(list(page = 1, y = 8, text = "Rate was 12.5%")))
  f <- rp_files(items = items, body = rp_p("Rate was 12.5%"))
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_identical(nrow(res), 0L)
})

test_that("decimals in table cells, and a wrapped line that starts with one, are not mistaken for section numbers", {
  skip_if_not_installed("pdftools")
  body <- paste0(rp_p("The daily dose was 2.5 mg daily as planned."), rp_tbl(c("Drug", "Low", "High"), c("Drug A", "0.5 mg", "1.2 mg")))
  items <- list(
    list(y = 10, text = "The daily dose was"), list(y = 9.7, text = "2.5 mg daily as planned."),
    list(y = 9, text = "Drug Low High"), list(y = 8.7, text = "Drug A 0.5 mg 1.2 mg")
  )
  f <- rp_files(items = items, body = body)
  res <- audit_render_parity(f$pdf, f$docx, check = "numbers")
  expect_identical(nrow(res), 0L)
})

test_that("a caption inside a two-cell layout table is found, and its text is counted once", {
  skip_if_not_installed("pdftools")
  panel <- paste0("<w:tbl><w:tr><w:tc>", rp_p(paste0("Figure", NBSP, "1: Left panel 12.5%"), "ImageCaption"),
                  "</w:tc><w:tc>", rp_p("Right"), "</w:tc></w:tr></w:tbl>")
  items <- list(list(y = 10, text = "Figure 1: Left panel 12.5%"), list(y = 9.5, text = "Right"))
  f <- rp_files(items = items, body = panel)
  res <- audit_render_parity(f$pdf, f$docx, check = c("numbering", "numbers", "placement"))
  expect_identical(nrow(res), 0L)
  expect_true("Figure 1" %in% attr(res, "captions")$docx$label)
})

test_that("a big Word table is read in linear time", {
  skip_if_not_installed("pdftools")
  skip_on_cran()
  rows <- lapply(seq_len(1500), function(i) c(sprintf("S%04d", i), "1.5%"))
  body <- do.call(rp_tbl, c(list(c("Id", "Rate")), rows))
  d <- withr::local_tempdir()
  docx <- file.path(d, "big.docx")
  rp_docx(docx, body)
  elapsed <- system.time(parsed <- .rp_read_docx(docx, NULL))[["elapsed"]]
  expect_lt(elapsed, 10) # the per-paragraph search this replaced took ~3 s for 1500 rows and grew with the square
  expect_match(parsed$text, "S1500")
})
