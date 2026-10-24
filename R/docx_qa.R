#' Extract Questions, Answers and Cited Reports From a Word Document
#'
#' Reads the body of a `.docx` that lists investigator questions, each followed by the
#' answer paragraphs written for it, and returns one row per question with its answer and the
#' report files the answer cites. This is for tracking reviewer or PI questions across a
#' project (a Q&A sheet in Word turned into a deliverables table); it does not read Word's
#' comments or who made a tracked change, for which see [docx_review_extract()].
#'
#' A paragraph is a **question** if it matches `question_pattern`; every following paragraph
#' up to the next question is part of its **answer** (joined with a line break). Paragraphs
#' before the first question are ignored. The text is what the document currently reads: tracked
#' insertions count, tracked deletions do not. A table's paragraphs count, in document order;
#' the text of a text box is read once, as its own paragraph. Empty paragraphs are dropped.
#'
#' **The default `question_pattern` is simple and can misfire**: an answer paragraph that begins
#' with one of its words ("Can't say.", "How we handled it: see a.pdf") or ends with `?` is taken
#' for a new question, and the answer it interrupts is cut short without a warning. Check
#' `n_answer_paragraphs`, or give a stricter pattern (for example `"^Q[0-9]+[.:]"` when the
#' questions are numbered).
#'
#' **Citations** are the matches of `citation_pattern` in the answer. With `index_data`, each is
#' looked up (by file name, ignoring case and folders) in the index; the column `links` then holds,
#' per question, the Excel formulas `=HYPERLINK("<path>", "<file>")` of the citations the index
#' knows, as character strings (write them to a workbook cell with `openxlsx::writeFormula()`; Excel
#' limits a string in a formula to 255 characters, so longer paths are reported with a warning and
#' are not linked). A citation the index does not know is listed in `unresolved_citations`; an index
#' with the same file name twice uses the first and warns.
#'
#' The question and answer text is returned as it is in the document. If you write it to a CSV or
#' workbook, text that begins with `=`, `+`, `-` or `@` is read by spreadsheet programs as a formula.
#'
#' @param docx_path Path to the `.docx` file.
#' @param question_pattern Regular expression (PCRE) that marks a question paragraph.
#'   The default matches paragraphs that begin with a question word (as a whole word, so
#'   "Isolated" is not "Is") or end with `?`.
#' @param citation_pattern Regular expression (PCRE) for a cited file name. The default matches
#'   names ending in `.pdf`, `.docx` or `.html`, in any letter case.
#' @param index_data Optional data frame of the deliverables, with one row per file.
#' @param index_file_col,index_path_col Columns of `index_data` with the file name and with the
#'   path or URL the link should point to (default `"file"` and `"path"`).
#' @return A tibble with one row per question: `question_id`, `question`, `answer`,
#'   `n_answer_paragraphs`, `citations` (list of character vectors), `citation_files` (the
#'   citations joined with `"; "`) and, when `index_data` is given, `links` (list of HYPERLINK
#'   formulas, one per resolved citation) and `unresolved_citations` (joined with `"; "`).
#' @seealso [docx_review_extract()], [package_deliverables()]
#' @export
#' @examples
#' \dontrun{
#' qa <- extract_docx_qa("investigator_questions.docx")
#' qa$citations
#'
#' index <- data.frame(file = c("model_report.pdf"), path = c("reports/model_report.pdf"))
#' extract_docx_qa("investigator_questions.docx", index_data = index)$links
#' }
extract_docx_qa <- function(docx_path,
                            question_pattern = "^(What|Why|How|Summarize|Explain|Can|Is|Are)\\b|\\?$",
                            citation_pattern = "(?i)\\b[[:alnum:]_.\\-]+\\.(pdf|docx|html)\\b",
                            index_data = NULL,
                            index_file_col = "file",
                            index_path_col = "path") {
  if (!is.character(docx_path) || length(docx_path) != 1L || is.na(docx_path) ||
      !file.exists(docx_path) || dir.exists(docx_path)) {
    stop("`docx_path` must be the path of an existing .docx file.", call. = FALSE)
  }
  if (!identical(tolower(tools::file_ext(docx_path)), "docx")) {
    stop("`docx_path` must be a .docx file.", call. = FALSE)
  }
  for (arg in c("question_pattern", "citation_pattern")) {
    val <- get(arg)
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      stop("`", arg, "` must be a single regular expression.", call. = FALSE)
    }
    ok <- tryCatch({ grepl(val, "", perl = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) stop("`", arg, "` is not a valid regular expression.", call. = FALSE)
  }
  if (!is.null(index_data)) {
    if (!is.data.frame(index_data)) stop("`index_data` must be NULL or a data frame.", call. = FALSE)
    for (arg in c("index_file_col", "index_path_col")) {
      val <- get(arg)
      if (!is.character(val) || length(val) != 1L || is.na(val) || !val %in% names(index_data)) {
        stop("`", arg, "` must name a column of `index_data`.", call. = FALSE)
      }
    }
  }

  paragraphs <- .docx_body_paragraphs(docx_path)
  is_q <- grepl(question_pattern, paragraphs, perl = TRUE)
  first_q <- which(is_q)[1L]

  empty <- tibble::tibble(
    question_id = integer(), question = character(), answer = character(),
    n_answer_paragraphs = integer(), citations = list(), citation_files = character()
  )
  if (is.na(first_q)) {
    if (!is.null(index_data)) {
      empty$links <- list()
      empty$unresolved_citations <- character()
    }
    return(empty)
  }

  paragraphs <- paragraphs[first_q:length(paragraphs)]
  is_q <- is_q[first_q:length(is_q)]
  group <- cumsum(is_q)
  rows <- lapply(unique(group), function(g) {
    p <- paragraphs[group == g]
    answer <- p[-1L]
    answer_text <- paste(answer, collapse = "\n")
    cites <- unique(unlist(regmatches(answer_text, gregexpr(citation_pattern, answer_text, perl = TRUE))))
    if (is.null(cites)) cites <- character()
    list(question_id = g, question = p[1L], answer = answer_text,
         n_answer_paragraphs = length(answer), citations = cites)
  })

  out <- tibble::tibble(
    question_id = vapply(rows, `[[`, integer(1), "question_id"),
    question = vapply(rows, `[[`, character(1), "question"),
    answer = vapply(rows, `[[`, character(1), "answer"),
    n_answer_paragraphs = vapply(rows, `[[`, integer(1), "n_answer_paragraphs"),
    citations = lapply(rows, `[[`, "citations")
  )
  out$citation_files <- vapply(out$citations, paste, character(1), collapse = "; ")

  if (!is.null(index_data)) {
    idx_file <- tolower(basename(as.character(index_data[[index_file_col]])))
    idx_path <- as.character(index_data[[index_path_col]])
    idx_name <- basename(as.character(index_data[[index_file_col]]))
    dup <- unique(idx_name[duplicated(idx_file)])
    if (length(dup)) {
      warning("`index_data` lists the file name(s) ", paste(dup, collapse = ", "),
              " more than once; the first entry is used.", call. = FALSE)
    }
    # a citation counts as resolved only if the index gives it a path of a length Excel accepts
    usable <- !is.na(idx_path) & nzchar(idx_path) & nchar(idx_path) <= 255L
    too_long <- unique(idx_name[!is.na(idx_path) & nchar(idx_path) > 255L])
    if (length(too_long)) {
      warning("Path(s) of more than 255 characters cannot go into an Excel HYPERLINK formula; not linked: ",
              paste(too_long, collapse = ", "), call. = FALSE)
    }
    resolve <- function(cites) {
      pos <- match(tolower(basename(cites)), idx_file)
      ok <- !is.na(pos) & usable[pos]
      ok[is.na(ok)] <- FALSE
      list(
        links = vapply(which(ok), function(i) {
          .hyperlink_formula(idx_path[pos[i]], idx_name[pos[i]])
        }, character(1)),
        unresolved = cites[!ok]
      )
    }
    res <- lapply(out$citations, resolve)
    out$links <- lapply(res, `[[`, "links")
    out$unresolved_citations <- vapply(res, function(r) paste(r$unresolved, collapse = "; "), character(1))
  }
  out
}

# `=HYPERLINK("path","text")`; a double quote inside a string is doubled
.hyperlink_formula <- function(path, text) {
  q <- function(x) paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
  paste0("=HYPERLINK(", q(path), ",", q(text), ")")
}

# the paragraphs of word/document.xml, in order, as the document currently reads
.docx_body_paragraphs <- function(docx_path) {
  parts <- suppressWarnings(utils::unzip(docx_path, list = TRUE)$Name)
  if (!"word/document.xml" %in% parts) {
    stop("`docx_path` is not a Word document: it has no word/document.xml.", call. = FALSE)
  }
  exdir <- tempfile("docx_qa_")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(docx_path, files = "word/document.xml", exdir = exdir)
  doc <- tryCatch(
    xml2::read_xml(file.path(exdir, "word", "document.xml")),
    error = function(e) stop("The Word document could not be read: ", conditionMessage(e), call. = FALSE)
  )
  ns <- c(
    w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    mc = "http://schemas.openxmlformats.org/markup-compatibility/2006"
  )
  # `mc:Fallback` repeats the content of `mc:Choice` for readers that lack the feature (text boxes)
  paras <- xml2::xml_find_all(doc, "//w:body//w:p[not(ancestor::mc:Fallback)]", ns)
  text <- vapply(paras, function(p) {
    # text runs and tabs / line breaks of this paragraph only (a text box inside it is a paragraph of
    # its own); w:delText (deleted text) is a different element and is left out
    depth <- xml2::xml_find_num(p, "count(ancestor-or-self::w:p)", ns)
    leaves <- xml2::xml_find_all(
      p,
      sprintf(".//*[(self::w:t or self::w:tab or self::w:br) and count(ancestor::w:p) = %d]", as.integer(depth)),
      ns
    )
    paste0(vapply(leaves, function(el) {
      switch(xml2::xml_name(el), t = xml2::xml_text(el), tab = " ", br = " ", "")
    }, character(1)), collapse = "")
  }, character(1))
  text <- trimws(gsub("[ \t]+", " ", text))
  text[nzchar(text)]
}
