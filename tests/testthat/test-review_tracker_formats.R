tracker_df <- function() {
  data.frame(
    comment = c("Define the cohort", "Report the | pipe & <tag>", "ok"),
    author = c("Reviewer 1", "Reviewer 2", "Reviewer 1"),
    status = c("adopted", "not_adopted", "adopted"),
    location = c("Methods, page 4", "", "Table 2"),
    rationale = c("Added the inclusion criteria", "Outside the scope of the study", "Done in text"),
    stringsAsFactors = FALSE
  )
}

test_that("format_review_tracker returns a Markdown table with escaped pipes", {
  tr <- review_tracker(tracker_df())
  md <- format_review_tracker(tr, "markdown")
  expect_equal(md[1], "| item_id | file | author | comment | status | location | rationale |")
  expect_equal(md[2], "|---|---|---|---|---|---|---|")
  expect_length(md, 5L)
  expect_match(md[4], "Report the \\\\| pipe & <tag>", fixed = FALSE)
  expect_false(any(grepl("selected_text", md)))
  # an explicit column choice is honoured, in order
  two <- format_review_tracker(tr, "markdown", columns = c("status", "comment"))
  expect_equal(two[1], "| status | comment |")
  expect_error(format_review_tracker(tr, "markdown", columns = "nope"), "not in")
  expect_error(format_review_tracker(tr[c("comment")], "markdown"), "required column")
})

test_that("review_tracker writes Markdown, Word, Excel and CSV by extension or `format`", {
  tmp <- withr::local_tempdir()
  d <- tracker_df()

  md <- file.path(tmp, "t.md")
  review_tracker(d, file = md)
  expect_true(any(grepl("^\\| 1 \\|", readLines(md))))

  xl <- file.path(tmp, "t.xlsx")
  review_tracker(d, file = xl)
  skip_if_not_installed("readxl")
  back <- readxl::read_excel(xl)
  expect_equal(nrow(back), 3L)
  expect_true(all(c("selected_text", "trivial") %in% names(back))) # Excel keeps every column
  expect_true(validate_review_tracker(xl, error_on_invalid = FALSE)$n_rows == 3L)

  csv <- file.path(tmp, "t.dat")
  review_tracker(d, file = csv)
  expect_equal(nrow(utils::read.csv(csv)), 3L)

  # `format` overrides the extension
  other <- file.path(tmp, "table.txt")
  review_tracker(d, file = other, format = "markdown")
  expect_match(readLines(other)[1], "^\\| item_id")
  review_tracker(d, file = file.path(tmp, "e.dat"), format = "excel")
  expect_equal(nrow(readxl::read_excel(file.path(tmp, "e.dat"), col_types = "text")), 3L)

  expect_error(review_tracker(d, format = "docx"), "needs `file`")
  expect_error(review_tracker(d, file = md, overwrite = FALSE), "already exists")
})

test_that("the Word tracker is a readable table with escaped text", {
  skip_if_not_installed("zip")
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "tracker.docx")
  tr <- review_tracker(tracker_df())
  out <- format_review_tracker(tr, "docx", file = f)
  expect_equal(out, f)
  files <- zip::zip_list(f)$filename
  expect_true(all(c("[Content_Types].xml", "_rels/.rels", "word/document.xml") %in% files))
  zip::unzip(f, files = "word/document.xml", exdir = tmp)
  xml <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
  ns <- xml2::xml_ns(xml)
  rows <- xml2::xml_find_all(xml, "//w:tbl/w:tr", ns)
  expect_length(rows, 4L) # header + 3 comments
  txt <- xml2::xml_text(xml2::xml_find_all(xml, "//w:t", ns))
  expect_true("Report the | pipe & <tag>" %in% txt)
  expect_true("Methods, page 4" %in% txt)
  expect_error(format_review_tracker(tr, "docx"), "file")
  # the same through review_tracker()
  g <- file.path(tmp, "second.docx")
  review_tracker(tracker_df(), file = g)
  expect_true(file.exists(g))
})

test_that("format_review_tracker returns CSV text without a file", {
  csv <- format_review_tracker(review_tracker(tracker_df()), "csv")
  expect_match(csv[1], "item_id")
  expect_length(csv, 4L)
})
