# a .docx whose body is the given XML; everything here is invented
qa_docx <- function(body_xml, env = parent.frame()) {
  testthat::skip_if_not_installed("zip")
  td <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(td, "word"))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ), file.path(td, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>',
    body_xml, "</w:body></w:document>"
  ), file.path(td, "word", "document.xml"), useBytes = TRUE)
  target <- file.path(withr::local_tempdir(.local_envir = env), "questions.docx")
  zip::zip(target, files = list.files(td, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = td)
  target
}

qa_para <- function(text) paste0("<w:p><w:r><w:t>", text, "</w:t></w:r></w:p>")

test_that("questions, answers and cited files are extracted in order", {
  path <- qa_docx(paste0(
    qa_para("Study questions"),
    qa_para("What is the effect of treatment?"),
    qa_para("The effect is small; see model_report.pdf."),
    qa_para("Details are in tables.docx and model_report.pdf."),
    qa_para("Summarize the missing data."),
    qa_para("Eleven percent are missing (missing_data.html)."),
    qa_para("Was the model adjusted for age?"),
    qa_para("Yes.")
  ))
  out <- extract_docx_qa(path)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$question_id, 1:3)
  expect_identical(out$question, c("What is the effect of treatment?", "Summarize the missing data.", "Was the model adjusted for age?"))
  expect_identical(out$n_answer_paragraphs, c(2L, 1L, 1L))
  expect_identical(out$answer[1], "The effect is small; see model_report.pdf.\nDetails are in tables.docx and model_report.pdf.")
  expect_identical(out$citations, list(c("model_report.pdf", "tables.docx"), "missing_data.html", character()))
  expect_identical(out$citation_files, c("model_report.pdf; tables.docx", "missing_data.html", ""))
  # the heading before the first question is ignored; no index columns without index_data
  expect_false("Study questions" %in% out$question)
  expect_false("links" %in% names(out))
})

test_that("the default question pattern wants a whole question word, not a word that starts with one", {
  path <- qa_docx(paste0(qa_para("Isolated results are in a.pdf."), qa_para("Is it adjusted?"), qa_para("Yes, b.pdf.")))
  out <- extract_docx_qa(path)
  expect_identical(out$question, "Is it adjusted?")
  expect_identical(out$citations, list("b.pdf"))
})

test_that("patterns can be changed", {
  path <- qa_docx(paste0(qa_para("Q1: effect"), qa_para("A: see r1.rds and a.pdf"), qa_para("Q2: safety"), qa_para("A: none")))
  out <- extract_docx_qa(path, question_pattern = "^Q[0-9]+:", citation_pattern = "[[:alnum:]]+\\.rds")
  expect_identical(out$question, c("Q1: effect", "Q2: safety"))
  expect_identical(out$citations, list("r1.rds", character()))
})

test_that("tracked insertions count, tracked deletions and empty paragraphs do not, tables are read in order", {
  body <- paste0(
    "<w:p><w:r><w:t>Why was it excluded?</w:t></w:r></w:p>",
    "<w:p><w:r><w:t>Kept </w:t></w:r><w:del><w:r><w:delText>deleted words </w:delText></w:r></w:del>",
    '<w:ins w:id="1" w:author="a"><w:r><w:t>inserted words</w:t></w:r></w:ins></w:p>',
    "<w:p></w:p>",
    "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>in a table (t.pdf)</w:t></w:r></w:p></w:tc></w:tr></w:tbl>",
    "<w:p><w:r><w:t>Col</w:t></w:r><w:r><w:tab/></w:r><w:r><w:t>umn</w:t></w:r></w:p>"
  )
  out <- extract_docx_qa(qa_docx(body))
  expect_identical(out$answer, "Kept inserted words\nin a table (t.pdf)\nCol umn")
  expect_identical(out$citations, list("t.pdf"))
})

test_that("a document without a question gives an empty table", {
  out <- extract_docx_qa(qa_docx(qa_para("Just a note.")))
  expect_identical(nrow(out), 0L)
  expect_named(out, c("question_id", "question", "answer", "n_answer_paragraphs", "citations", "citation_files"))
})

test_that("an index turns citations into HYPERLINK formulas and lists the unknown ones", {
  path <- qa_docx(paste0(
    qa_para("What changed?"), qa_para("See Model_Report.PDF and ghost.pdf."),
    qa_para("Why?"), qa_para("Because.")
  ))
  index <- data.frame(
    file = c("model_report.pdf", "other.pdf"),
    path = c("reports/model \"v2\".pdf", "reports/other.pdf")
  )
  out <- extract_docx_qa(path, index_data = index)
  expect_identical(out$links[[1]], "=HYPERLINK(\"reports/model \"\"v2\"\".pdf\",\"model_report.pdf\")")
  expect_identical(out$links[[2]], character())
  expect_identical(out$unresolved_citations, c("ghost.pdf", ""))

  # a folder in the index file name is ignored, and the columns can be named
  index2 <- data.frame(name = "reports/model_report.pdf", where = "https://example.org/m.pdf")
  out2 <- extract_docx_qa(path, index_data = index2, index_file_col = "name", index_path_col = "where")
  expect_identical(out2$links[[1]], "=HYPERLINK(\"https://example.org/m.pdf\",\"model_report.pdf\")")

  none <- extract_docx_qa(qa_docx(qa_para("Just a note.")), index_data = index)
  expect_identical(nrow(none), 0L)
  expect_true(all(c("links", "unresolved_citations") %in% names(none)))
})

test_that("inputs are checked", {
  path <- qa_docx(qa_para("What?"))
  expect_error(extract_docx_qa("missing.docx"), "existing .docx")
  expect_error(extract_docx_qa(tempdir()), "existing .docx")
  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(extract_docx_qa(txt), "must be a .docx file")
  expect_error(extract_docx_qa(path, question_pattern = "("), "not a valid regular expression")
  expect_error(extract_docx_qa(path, citation_pattern = c("a", "b")), "single regular expression")
  expect_error(extract_docx_qa(path, index_data = "x"), "NULL or a data frame")
  expect_error(extract_docx_qa(path, index_data = data.frame(file = "a")), "must name a column")

  # a .docx that is not a Word document
  skip_if_not_installed("zip")
  fake <- withr::local_tempfile(fileext = ".docx")
  td <- withr::local_tempdir()
  writeLines("x", file.path(td, "a.txt"))
  zip::zip(fake, "a.txt", root = td)
  expect_error(extract_docx_qa(fake), "no word/document.xml")
})

test_that("file names with dots are cited whole", {
  path <- qa_docx(paste0(qa_para("What changed?"), qa_para("See model_v1.2.pdf and https://example.org/p/page.html.")))
  out <- extract_docx_qa(path)
  expect_identical(out$citations, list(c("model_v1.2.pdf", "page.html")))
})

test_that("a text box is read once, and its fallback copy not at all", {
  body <- paste0(
    qa_para("Why was it excluded?"),
    '<w:p><w:r><w:t>Outer text </w:t></w:r>',
    '<mc:AlternateContent xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">',
    "<mc:Choice><w:r><w:txbxContent>", qa_para("boxed.pdf"), "</w:txbxContent></w:r></mc:Choice>",
    "<mc:Fallback><w:r><w:txbxContent>", qa_para("boxed.pdf"), "</w:txbxContent></w:r></mc:Fallback>",
    "</mc:AlternateContent></w:p>"
  )
  out <- extract_docx_qa(qa_docx(body))
  expect_identical(out$answer, "Outer text
boxed.pdf")
  expect_identical(out$citations, list("boxed.pdf"))
})

test_that("an answer that begins with a question word is cut short: documented, and visible in n_answer_paragraphs", {
  path <- qa_docx(paste0(qa_para("What is the effect?"), qa_para("Can't say yet."), qa_para("See a.pdf.")))
  out <- extract_docx_qa(path)
  expect_identical(out$question, c("What is the effect?", "Can't say yet."))
  expect_identical(out$n_answer_paragraphs, c(0L, 1L))
})

test_that("index problems are reported, not hidden", {
  path <- qa_docx(paste0(qa_para("What changed?"), qa_para("See a.pdf, b.pdf and c.pdf.")))
  index <- data.frame(
    file = c("a.pdf", "A.PDF", "b.pdf", "c.pdf"),
    path = c("one/a.pdf", "two/a.pdf", strrep("x", 300L), NA)
  )
  expect_warning(
    expect_warning(out <- extract_docx_qa(path, index_data = index), "more than once"),
    "255 characters"
  )
  expect_identical(out$links[[1]], "=HYPERLINK(\"one/a.pdf\",\"a.pdf\")")
  expect_identical(out$unresolved_citations, "b.pdf; c.pdf")
})
