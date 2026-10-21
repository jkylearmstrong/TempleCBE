#' Audit a Rendered PDF Against the Word Version of the Same Report
#'
#' A lightweight check, run before deliverables are sent out, for the differences
#' that reviewers keep finding between the \code{.pdf} and the \code{.docx} that
#' Quarto renders from one source. It reads both files and reports, as a table of
#' findings:
#' \describe{
#'   \item{\code{margin_overflow}}{words in the PDF that come closer than
#'     \code{margin_in} inches to the left or right edge of the page (a formula
#'     or a table that runs out of the text area).}
#'   \item{\code{numbering}}{table or figure numbers that are missing from one of
#'     the two files or that are not consecutive in one of them.}
#'   \item{\code{order}}{table and figure captions that come in another order in
#'     the PDF than in the Word file.}
#'   \item{\code{caption_placement}}{a table caption that is above the table in
#'     one file and below it in the other.}
#'   \item{\code{numbers}}{percentages and decimal numbers that occur in one file
#'     and not in the other (for example \code{98.9\%} in the PDF and \code{97\%}
#'     in Word), counted, so a number that occurs twice in one file and once in the
#'     other is reported.}
#' }
#' A check that could not be made (a table that could not be found, or found in
#' more than one place, in the PDF) is listed as \code{info}.
#'
#' @details
#' This reads text and positions only; it does not look at the pages. It is
#' meant to point at places to look at, not to certify a document. It was checked
#' against reports rendered by Quarto (PDF through LaTeX, Word through pandoc);
#' Word files written by hand in Word, other PDF engines and two-column layouts are
#' less tested.
#' \itemize{
#'   \item A caption is a line (PDF) or paragraph (Word) that begins with
#'     \code{Table}, \code{Figure} or \code{Fig.} (any case), a number, and a
#'     \code{.}, \code{:}, en dash or em dash followed by a space. A sentence that
#'     begins "Table 1 shows" or "Table 1.5 mg were given" is not a caption.
#'     Quarto's non-breaking space after the word is read as a space. Supplementary
#'     numbers (\code{Table S1}) and panels (\code{Figure 1A}) are not recognised. A
#'     caption repeated on the continuation pages of a long table counts once.
#'   \item Quarto wraps each cross-referenced table and figure in a one-cell layout
#'     table; the wrapper is looked through. A table is a table of the Word file when it
#'     has at least two cells and no table inside it. Content controls are read.
#'   \item The place of a table in the PDF is found from the first row of its header,
#'     within a page of its caption, and not from a line that another caption is
#'     nearer to. A header that occurs in two places near the caption, or a table whose
#'     first cell is empty, gives an \code{info} row.
#'   \item Not read: text boxes, headers and footers of the Word file. Footnotes and
#'     endnotes of the Word file are read, because the PDF holds them in its text. Lines
#'     of the PDF that repeat on at least half of its pages (running headers and
#'     footers), the lines of its contents lists (a line that ends in dot leaders and a
#'     page number) and section numbers at the start of a line are left out of the
#'     comparison of numbers; so are the contents paragraphs of the Word file.
#'   \item Pages that the PDF stores rotated (landscape pages made by \code{pdflscape}
#'     and similar) are measured in their displayed width. Two-column layouts are not
#'     understood: lines are read across the columns, so captions in the second column
#'     can be missed.
#'   \item PDF text is as extracted by \pkg{pdftools}: a number that the PDF engine
#'     splits across lines can appear as a difference. Positions are whole points, so
#'     \code{margin_in} has a tolerance of 2 points.
#' }
#' The Word file is an untrusted archive and is read with the same size limits as
#' \code{\link{docx_review_extract}()} (\code{docx_limits}); no entity is expanded and no
#' network is used.
#'
#' @param pdf_path Path to the rendered \code{.pdf}.
#' @param docx_path Path to the rendered \code{.docx}.
#' @param margin_in Minimum distance, in inches, between text and the left and
#'   right edge of a PDF page (default 0.5).
#' @param check Which checks to run: any of \code{"margins"}, \code{"numbering"},
#'   \code{"order"}, \code{"placement"}, \code{"numbers"}. Default all.
#' @param docx_limits \code{NULL} or a named list of limits on the Word archive; see
#'   \code{\link{docx_review_extract}()}.
#' @return A tibble of class \code{render_parity} with the columns \code{check},
#'   \code{severity} (\code{"warning"} or \code{"info"}), \code{location} (for
#'   example \code{"page 3"} or \code{"Table 2"}) and \code{detail}. No rows means
#'   nothing was found. Attributes: \code{n_pages}, \code{captions} (the captions
#'   found in each file).
#' @export
#' @examples
#' \dontrun{
#' res <- audit_render_parity("report.pdf", "report.docx")
#' res
#' }
audit_render_parity <- function(pdf_path,
                                docx_path,
                                margin_in = 0.5,
                                check = c("margins", "numbering", "order", "placement", "numbers"),
                                docx_limits = NULL) {
  check <- match.arg(check, several.ok = TRUE)
  for (p in c(pdf_path, docx_path)) {
    if (!is.character(p) || length(p) != 1L || is.na(p) || !file.exists(p) || dir.exists(p)) {
      stop("File not found: ", paste(p, collapse = ", "), call. = FALSE)
    }
  }
  if (!is.numeric(margin_in) || length(margin_in) != 1L || is.na(margin_in) || margin_in < 0) {
    stop("`margin_in` must be a single non-negative number.", call. = FALSE)
  }
  if (!tolower(tools::file_ext(pdf_path)) %in% "pdf") stop("`pdf_path` must be a .pdf file.", call. = FALSE)
  if (!tolower(tools::file_ext(docx_path)) %in% "docx") stop("`docx_path` must be a .docx file.", call. = FALSE)

  pdf <- .rp_read_pdf(pdf_path)
  docx <- .rp_read_docx(docx_path, docx_limits)

  findings <- list()
  add <- function(check, severity, location, detail) {
    findings[[length(findings) + 1L]] <<- tibble::tibble(
      check = check, severity = severity, location = location, detail = detail
    )
  }

  if ("margins" %in% check) {
    for (f in .rp_margin_findings(pdf, margin_in)) add("margin_overflow", "warning", f$location, f$detail)
  }
  if ("numbering" %in% check) {
    for (f in .rp_numbering_findings(docx$captions, pdf$captions)) add("numbering", "warning", f$location, f$detail)
  }
  if ("order" %in% check) {
    for (f in .rp_order_findings(docx$captions, pdf$captions)) add("order", "warning", f$location, f$detail)
  }
  if ("placement" %in% check) {
    for (f in .rp_placement_findings(docx, pdf)) add("caption_placement", f$severity, f$location, f$detail)
  }
  if ("numbers" %in% check) {
    for (f in .rp_number_findings(docx$text, pdf$number_text)) add("numbers", "warning", f$location, f$detail)
  }

  out <- if (length(findings)) {
    dplyr::bind_rows(findings)
  } else {
    tibble::tibble(check = character(), severity = character(), location = character(), detail = character())
  }
  structure(
    out,
    class = c("render_parity", class(out)),
    n_pages = pdf$n_pages,
    captions = list(pdf = pdf$captions, docx = docx$captions)
  )
}

#' @export
print.render_parity <- function(x, ...) {
  n_warn <- sum(x$severity == "warning")
  if (nrow(x) == 0L) {
    cat("No differences found between the PDF and the Word file.\n")
  } else {
    cat(sprintf("%d difference(s) to look at (%d warning, %d info):\n", nrow(x), n_warn, nrow(x) - n_warn))
    args <- list(tibble::as_tibble(x), ...)
    if (is.null(args$n)) args$n <- Inf
    if (is.null(args$width)) args$width <- Inf
    do.call(print, args)
  }
  invisible(x)
}

# ---- captions -----------------------------------------------------------------------------------

# Table / Figure / Fig. (any case), a number, "." ":" or a dash, then white space (the no-break
# space Quarto writes included). "Table 1.5 mg" and "Table 1 shows" do not match.
.rp_caption_pattern <- "^[\\s\u00a0]*(?i:(Table|Figure|Fig\\.?))[\\s\u00a0]+([0-9]+(?:\\.[0-9]+)?)[\\s\u00a0]*(?:[.:](?=[\\s\u00a0]|$)|[\u2013\u2014])"
.rp_caption_pattern_loose <- "^[\\s\u00a0]*(?i:(Table|Figure|Fig\\.?))[\\s\u00a0]+([0-9]+(?:\\.[0-9]+)?)(?![0-9.])"

# list(kind, number, label) or NULL
.rp_parse_caption <- function(text, style_caption = FALSE) {
  m <- regmatches(text, regexec(.rp_caption_pattern, text, perl = TRUE))[[1]]
  if (length(m) == 0L && isTRUE(style_caption)) {
    # a paragraph in a caption style needs no separator after the number
    m <- regmatches(text, regexec(.rp_caption_pattern_loose, text, perl = TRUE))[[1]]
  }
  if (length(m) == 0L) return(NULL)
  kind <- if (grepl("^fig", m[2], ignore.case = TRUE)) "Figure" else "Table"
  list(kind = kind, number = m[3], label = paste(kind, m[3]))
}

.rp_empty_captions <- function(source) {
  if (identical(source, "pdf")) {
    tibble::tibble(kind = character(), number = character(), label = character(),
                   page = integer(), y = integer(), position = numeric())
  } else {
    tibble::tibble(kind = character(), number = character(), label = character(), block = integer(),
                   placement = character(), table_anchor = character(), table_anchor2 = character())
  }
}

# ---- PDF ----------------------------------------------------------------------------------------

.rp_leader_pattern <- "(\\.[\\s.]{2,}|\u2026)[\\s]*[0-9ivxlcIVXLC]+[\\s]*$" # dot leaders and a page number
# "1.1 Methods": a section number and a capitalised title ("2.5 mg daily" is not one)
.rp_section_number_pattern <- "^\\s*[0-9]+(\\.[0-9]+)+\\s+(?=[A-Z])"

.rp_read_pdf <- function(path) {
  pages <- tryCatch(pdftools::pdf_data(path), error = function(e) {
    stop("The PDF could not be read: ", conditionMessage(e), call. = FALSE)
  })
  sizes <- pdftools::pdf_pagesize(path)
  n_pages <- length(pages)
  word_list <- vector("list", n_pages)
  line_list <- vector("list", n_pages)
  for (i in seq_len(n_pages)) {
    d <- pages[[i]]
    if (nrow(d) == 0L) next
    # a page stored rotated has coordinates in its displayed frame: use the width that fits
    pw <- sizes$width[i]
    ph <- sizes$height[i]
    if (max(d$x + d$width) > pw * 1.02 && max(d$x + d$width) <= ph * 1.02) {
      tmp <- pw
      pw <- ph
      ph <- tmp
    }
    d$page <- i
    d$page_width <- pw
    word_list[[i]] <- d
    o <- order(d$y, d$x)
    d <- d[o, , drop = FALSE]
    lid <- cumsum(c(TRUE, diff(d$y) > 3))
    d <- d[order(lid, d$x), , drop = FALSE]
    lid <- sort(lid)
    line_list[[i]] <- data.frame(
      page = i,
      page_height = ph,
      y = as.integer(tapply(d$y, lid, min)),
      text = as.character(tapply(d$text, lid, paste, collapse = " ")),
      stringsAsFactors = FALSE
    )
  }
  words <- dplyr::bind_rows(word_list)
  if (nrow(words) == 0L) {
    words <- tibble::tibble(
      width = integer(), height = integer(), x = integer(), y = integer(), space = logical(),
      text = character(), page = integer(), page_width = numeric()
    )
  }
  lines <- dplyr::bind_rows(line_list)
  if (is.null(lines) || nrow(lines) == 0L) {
    lines <- data.frame(page = integer(), page_height = numeric(), y = integer(), text = character(), stringsAsFactors = FALSE)
  }
  lines$position <- lines$page * 10000 + lines$y

  # lines that are not body text: running headers and footers (the same line, digits aside, on at
  # least half of the pages) and the lines of contents lists (dot leaders and a page number)
  # only the top and bottom strip of a page can hold a running header or footer: the rows of a long
  # table repeat in shape, and the caption of a table that runs over several pages repeats, too
  edge <- lines$y < 0.07 * lines$page_height | lines$y > 0.9 * lines$page_height
  is_caption_line <- grepl(.rp_caption_pattern, lines$text, perl = TRUE)
  # A running line repeats as it is ("Draft report 5.5%") or differs only in a page number ("Page 3 of 9",
  # at most two runs of digits). A table row such as "S017 11.7%" has three, so a long table that
  # puts a row in the strip of every page is not mistaken for a header.
  n_digit_runs <- vapply(gregexpr("[0-9]+", lines$text), function(m) sum(m > 0), integer(1))
  threshold <- max(3L, ceiling(n_pages / 2))
  repeated <- function(key) {
    key[!edge | is_caption_line] <- NA_character_
    pages_with <- tapply(lines$page, key, function(p) length(unique(p)))
    r <- n_pages >= 3L & !is.na(key) & nzchar(key) & pages_with[key] >= threshold
    r[is.na(r)] <- FALSE
    r
  }
  running <- repeated(trimws(lines$text)) | (n_digit_runs <= 2L & repeated(gsub("[0-9]+", "#", trimws(lines$text))))
  contents <- grepl(.rp_leader_pattern, lines$text, perl = TRUE)
  lines$body <- !running & !contents

  caps <- list()
  for (i in which(lines$body)) {
    cap <- .rp_parse_caption(lines$text[i])
    if (!is.null(cap)) {
      caps[[length(caps) + 1L]] <- tibble::tibble(
        kind = cap$kind, number = cap$number, label = cap$label,
        page = lines$page[i], y = lines$y[i], position = lines$position[i]
      )
    }
  }
  caps <- if (length(caps)) dplyr::bind_rows(caps) else .rp_empty_captions("pdf")
  # the caption of a table that runs over several pages is repeated on the next page: count it once
  if (nrow(caps) > 1L) {
    drop <- rep(FALSE, nrow(caps))
    for (i in seq_len(nrow(caps))[-1L]) {
      earlier <- which(caps$label[seq_len(i - 1L)] == caps$label[i] & caps$page[seq_len(i - 1L)] >= caps$page[i] - 1L)
      if (length(earlier)) drop[i] <- TRUE
    }
    caps <- caps[!drop, , drop = FALSE]
  }

  body_text <- lines$text[lines$body]
  body_text <- sub(.rp_section_number_pattern, "", body_text, perl = TRUE)
  list(
    words = words, lines = lines, n_pages = n_pages, captions = caps,
    number_text = paste(body_text, collapse = "\n")
  )
}

.rp_margin_findings <- function(pdf, margin_in) {
  w <- pdf$words
  if (nrow(w) == 0L) return(list())
  margin_pt <- margin_in * 72
  tol <- 2 # positions are whole points
  left_gap <- w$x
  right_gap <- w$page_width - (w$x + w$width)
  bad <- left_gap < margin_pt - tol | right_gap < margin_pt - tol
  if (!any(bad)) return(list())
  bw <- w[bad, , drop = FALSE]
  bw$gap <- pmin(left_gap[bad], right_gap[bad])
  res <- list()
  for (pg in sort(unique(bw$page))) {
    pw <- bw[bw$page == pg, , drop = FALSE]
    pw <- pw[order(pw$y, pw$x), , drop = FALSE]
    line_id <- cumsum(c(TRUE, diff(pw$y) > 3))
    for (id in unique(line_id)) {
      lw <- pw[line_id == id, , drop = FALSE]
      worst <- min(lw$gap)
      snippet <- paste(lw$text, collapse = " ")
      if (nchar(snippet) > 80) snippet <- paste0(substr(snippet, 1, 77), "...")
      res[[length(res) + 1L]] <- list(
        location = sprintf("page %d", pg),
        detail = sprintf(
          "text %s the page edge (%.2f in from it, minimum %.2f in): \"%s\"",
          if (worst < 0) "runs past" else "comes close to", worst / 72, margin_in, snippet
        )
      )
    }
  }
  res
}

# ---- Word ---------------------------------------------------------------------------------------

.rp_ns <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  m = "http://schemas.openxmlformats.org/officeDocument/2006/math",
  mc = "http://schemas.openxmlformats.org/markup-compatibility/2006"
)

.rp_parse_xml <- function(path, part) {
  tryCatch(
    xml2::read_xml(path, options = c("NOBLANKS", "NONET")),
    error = function(e) stop(sprintf("%s could not be parsed: %s", part, conditionMessage(e)), call. = FALSE)
  )
}

# text of a paragraph or table node: runs in order, tabs and breaks as spaces, no-break hyphen as "-",
# no-break space as a space; text boxes and the fallback copy of a drawing are left out
.rp_node_text <- function(node, sep = "") {
  parts <- xml2::xml_find_all(
    node,
    ".//w:t[not(ancestor::w:txbxContent) and not(ancestor::mc:Fallback)] | .//m:t | .//w:tab | .//w:br | .//w:noBreakHyphen",
    .rp_ns
  )
  if (length(parts) == 0L) return("")
  nm <- xml2::xml_name(parts)
  txt <- ifelse(nm %in% c("t"), xml2::xml_text(parts), ifelse(nm == "noBreakHyphen", "-", " "))
  out <- paste(txt, collapse = sep)
  trimws(gsub("[\u00a0]", " ", out))
}

.rp_read_docx <- function(path, docx_limits) {
  limits <- resolve_docx_limits(docx_limits)
  listing <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) {
    stop("The Word file could not be read as a .docx archive: ", conditionMessage(e), call. = FALSE)
  })
  check_docx_listing(listing, limits)
  if (!"word/document.xml" %in% listing$Name) stop("The Word file has no word/document.xml.", call. = FALSE)
  notes_parts <- intersect(c("word/footnotes.xml", "word/endnotes.xml"), listing$Name)
  big <- notes_parts[listing$Length[match(notes_parts, listing$Name)] > limits$max_xml_bytes]
  if (length(big)) stop(big[1], " is larger than the limit for one XML part (docx_limits$max_xml_bytes).", call. = FALSE)
  td <- tempfile("parity_docx_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  unzip_docx_parts(path, c("word/document.xml", notes_parts), td)
  doc <- .rp_parse_xml(file.path(td, "word", "document.xml"), "word/document.xml")
  body <- xml2::xml_find_first(doc, "//w:body", .rp_ns)
  if (inherits(body, "xml_missing")) stop("The Word file has no document body (w:body).", call. = FALSE)

  # paragraphs and tables in document order, wherever they sit (layout tables, content controls)
  nodes <- xml2::xml_find_all(
    body,
    ".//w:p[not(ancestor::mc:Fallback) and not(ancestor::w:txbxContent)] | .//w:tbl[not(ancestor::mc:Fallback)]",
    .rp_ns
  )
  is_tbl <- xml2::xml_name(nodes) == "tbl"
  # a data table has no table inside it and at least two cells; the rest are layout wrappers
  is_data_tbl <- is_tbl
  for (i in which(is_tbl)) {
    nd <- nodes[[i]]
    nested <- length(xml2::xml_find_all(nd, ".//w:tbl", .rp_ns)) > 0L
    cells <- length(xml2::xml_find_all(nd, ".//w:tc", .rp_ns))
    is_data_tbl[i] <- !nested && cells >= 2L
  }
  # the paragraphs of a data table follow it in document order: mark them (one pass, no per-paragraph search)
  in_data_tbl <- rep(FALSE, length(nodes))
  for (i in which(is_data_tbl)) {
    k <- length(xml2::xml_find_all(
      nodes[[i]], ".//w:p[not(ancestor::mc:Fallback) and not(ancestor::w:txbxContent)]", .rp_ns
    ))
    if (k > 0L) in_data_tbl[seq(i + 1L, min(i + k, length(nodes)))] <- TRUE
  }
  # layout wrappers (a table with a table in it, or a single cell) are looked through
  keep <- !is_tbl | is_data_tbl
  nodes <- nodes[keep]
  is_tbl <- is_tbl[keep]
  in_tbl <- in_data_tbl[keep]

  style_of <- function(nd) {
    s <- xml2::xml_attr(xml2::xml_find_first(nd, "./w:pPr/w:pStyle", .rp_ns), "w:val", ns = .rp_ns)
    if (is.na(s)) "" else s
  }
  blocks <- lapply(seq_along(nodes), function(i) {
    nd <- nodes[[i]]
    if (is_tbl[i]) {
      paras <- xml2::xml_find_all(nd, ".//w:p", .rp_ns)
      ptxt <- vapply(paras, .rp_node_text, character(1))
      first_row <- xml2::xml_find_first(nd, ".//w:tr", .rp_ns)
      row_cells <- if (inherits(first_row, "xml_missing")) character() else {
        vapply(xml2::xml_find_all(first_row, "./w:tc", .rp_ns), .rp_node_text, character(1))
      }
      row_cells <- trimws(row_cells[nzchar(trimws(row_cells))])
      list(type = "table", text = paste(ptxt[nzchar(ptxt)], collapse = "\n"), contents = FALSE, in_tbl = FALSE, heading = FALSE,
           anchor = if (length(row_cells)) row_cells[1] else NA_character_,
           anchor2 = if (length(row_cells) > 1L) row_cells[2] else NA_character_, style_caption = FALSE)
    } else {
      st <- style_of(nd)
      list(type = "paragraph", text = .rp_node_text(nd), contents = grepl("^(TOC|TableofFigures|TableofAuthorities)", st, ignore.case = TRUE),
           in_tbl = in_tbl[i], heading = grepl("^(Heading|Title)", st, ignore.case = TRUE),
           anchor = NA_character_, anchor2 = NA_character_,
           style_caption = grepl("caption", st, ignore.case = TRUE))
    }
  })
  types <- vapply(blocks, function(b) b$type, character(1))
  texts <- vapply(blocks, function(b) b$text, character(1))
  contents <- vapply(blocks, function(b) b$contents, logical(1))
  in_tbl_block <- vapply(blocks, function(b) b$in_tbl, logical(1))
  heading <- vapply(blocks, function(b) b$heading, logical(1))
  anchors <- vapply(blocks, function(b) b$anchor, character(1))
  anchors2 <- vapply(blocks, function(b) b$anchor2, character(1))

  caps <- list()
  for (i in which(types == "paragraph" & !contents)) {
    cap <- .rp_parse_caption(texts[i], blocks[[i]]$style_caption)
    if (!is.null(cap) && !grepl(.rp_leader_pattern, texts[i], perl = TRUE)) {
      caps[[length(caps) + 1L]] <- tibble::tibble(kind = cap$kind, number = cap$number, label = cap$label, block = i)
    }
  }
  caps <- if (length(caps)) dplyr::bind_rows(caps) else .rp_empty_captions("docx")[, c("kind", "number", "label", "block")]

  # which side of its table each table caption is on, ignoring empty paragraphs between them
  nonempty <- which(types == "table" | (nzchar(texts) & !in_tbl_block))
  placement <- rep(NA_character_, nrow(caps))
  table_of <- rep(NA_integer_, nrow(caps))
  for (k in seq_len(nrow(caps))) {
    if (caps$kind[k] != "Table") next
    pos <- match(caps$block[k], nonempty)
    if (is.na(pos)) next # a caption inside a table: no side to compare
    next_tbl <- pos < length(nonempty) && types[nonempty[pos + 1L]] == "table"
    prev_tbl <- pos > 1L && types[nonempty[pos - 1L]] == "table"
    if (next_tbl && !prev_tbl) { placement[k] <- "above"; table_of[k] <- nonempty[pos + 1L] }
    else if (prev_tbl && !next_tbl) { placement[k] <- "below"; table_of[k] <- nonempty[pos - 1L] }
    else if (next_tbl && prev_tbl) { placement[k] <- "ambiguous" }
  }
  if (any(placement == "ambiguous", na.rm = TRUE)) {
    # a caption between two tables: it follows the side the unambiguous captions of this document use
    known <- placement[placement %in% c("above", "below")]
    if (length(known) && length(unique(known)) == 1L) {
      choose <- known[1]
      for (k in which(placement == "ambiguous")) {
        pos <- match(caps$block[k], nonempty)
        placement[k] <- choose
        table_of[k] <- if (choose == "above") nonempty[pos + 1L] else nonempty[pos - 1L]
      }
    } else {
      placement[placement == "ambiguous"] <- NA_character_ # not decided: no comparison is made
    }
  }
  caps$placement <- placement
  caps$table_anchor <- ifelse(is.na(table_of), NA_character_, anchors[table_of])
  caps$table_anchor2 <- ifelse(is.na(table_of), NA_character_, anchors2[table_of])

  # footnotes and endnotes: the PDF carries them in its text
  notes <- character()
  for (part in notes_parts) {
    nx <- .rp_parse_xml(file.path(td, part), part)
    for (pn in xml2::xml_find_all(nx, "//w:footnote[not(@w:type)]//w:p | //w:endnote[not(@w:type)]//w:p", .rp_ns)) {
      notes <- c(notes, .rp_node_text(pn))
    }
  }
  # pandoc writes the number of a numbered section into its heading as text; the PDF does too (and
  # its section numbers are removed from the comparison), so they are removed here as well
  num_text <- texts
  num_text[heading] <- sub(.rp_section_number_pattern, "", num_text[heading], perl = TRUE)
  all_text <- paste(c(num_text[!contents & !in_tbl_block], notes), collapse = "\n")
  list(captions = caps, text = all_text)
}

# ---- comparisons --------------------------------------------------------------------------------

.rp_numbering_findings <- function(docx_caps, pdf_caps) {
  res <- list()
  for (kind in c("Table", "Figure")) {
    d <- docx_caps$label[docx_caps$kind == kind]
    p <- pdf_caps$label[pdf_caps$kind == kind]
    only_d <- setdiff(d, p)
    only_p <- setdiff(p, d)
    if (length(only_d)) {
      res[[length(res) + 1L]] <- list(
        location = paste(only_d, collapse = ", "),
        detail = "has a caption in the Word file but none was found in the PDF"
      )
    }
    if (length(only_p)) {
      res[[length(res) + 1L]] <- list(
        location = paste(only_p, collapse = ", "),
        detail = "has a caption in the PDF but none was found in the Word file"
      )
    }
    for (src in c("Word", "PDF")) {
      labs <- if (src == "Word") d else p
      nums <- suppressWarnings(as.numeric(sub("^\\S+ ", "", labs)))
      nums <- nums[!is.na(nums) & nums == floor(nums)]
      if (length(nums) > 1L) {
        gaps <- setdiff(seq(min(nums), max(nums)), nums)
        dups <- unique(nums[duplicated(nums)])
        if (length(gaps)) {
          res[[length(res) + 1L]] <- list(
            location = paste0(kind, "s in the ", src, if (src == "Word") " file" else ""),
            detail = sprintf("numbering skips %s", paste(gaps, collapse = ", "))
          )
        }
        if (length(dups)) {
          res[[length(res) + 1L]] <- list(
            location = paste0(kind, "s in the ", src, if (src == "Word") " file" else ""),
            detail = sprintf("%s used more than once: %s", tolower(kind), paste(dups, collapse = ", "))
          )
        }
      }
    }
  }
  res
}

.rp_order_findings <- function(docx_caps, pdf_caps) {
  res <- list()
  for (kind in c("Table", "Figure")) {
    d <- unique(docx_caps$label[docx_caps$kind == kind])
    p <- unique(pdf_caps$label[pdf_caps$kind == kind][order(pdf_caps$position[pdf_caps$kind == kind])])
    shared <- intersect(d, p)
    if (length(shared) > 1L) {
      d_order <- shared[order(match(shared, d))]
      p_order <- shared[order(match(shared, p))]
      if (!identical(d_order, p_order)) {
        res[[length(res) + 1L]] <- list(
          location = paste0(kind, "s"),
          detail = sprintf(
            "caption order differs: Word has %s; PDF has %s",
            paste(d_order, collapse = ", "), paste(p_order, collapse = ", ")
          )
        )
      }
    }
  }
  res
}

.rp_squish <- function(x) gsub("[[:space:]\u00a0]+", " ", trimws(x))

# TRUE for the lines that start with `anchor` as a whole word (and, if given, contain `anchor2`)
.rp_header_hits <- function(squished, anchor, anchor2) {
  starts <- startsWith(squished, anchor) &
    (nchar(squished) == nchar(anchor) | substr(squished, nchar(anchor) + 1L, nchar(anchor) + 1L) == " ")
  if (!is.na(anchor2) && nzchar(anchor2)) {
    both <- starts & grepl(anchor2, squished, fixed = TRUE)
    if (any(both)) return(both)
  }
  starts
}

.rp_placement_findings <- function(docx, pdf) {
  res <- list()
  caps <- docx$captions[docx$captions$kind == "Table" & !is.na(docx$captions$placement), , drop = FALSE]
  if (nrow(caps) == 0L || nrow(pdf$captions) == 0L) return(res)
  lines <- pdf$lines
  squished <- .rp_squish(lines$text)
  is_caption_line <- grepl(.rp_caption_pattern, lines$text, perl = TRUE)
  usable <- lines$body & !is_caption_line
  claimed <- rep(FALSE, nrow(lines))
  for (k in seq_len(nrow(caps))) {
    label <- caps$label[k]
    pdf_cap <- pdf$captions[pdf$captions$label == label, , drop = FALSE]
    if (nrow(pdf_cap) == 0L) next
    anchor <- .rp_squish(caps$table_anchor[k])
    if (is.na(anchor) || !nzchar(anchor)) {
      res[[length(res) + 1L]] <- list(severity = "info", location = label,
                                       detail = "its table has no first-row text to find in the PDF, so the caption placement was not compared")
      next
    }
    cap_pos <- pdf_cap$position[1]
    cap_page <- pdf_cap$page[1]
    near <- usable & !claimed & abs(lines$page - cap_page) <= 1L
    hits <- which(near & .rp_header_hits(squished, anchor, .rp_squish(caps$table_anchor2[k])))
    # a line that another caption is nearer to belongs to that caption's table
    others <- pdf$captions$position[pdf$captions$label != label]
    if (length(others) && length(hits)) {
      hits <- hits[vapply(hits, function(h) {
        d_own <- abs(lines$position[h] - cap_pos)
        d_other <- min(abs(lines$position[h] - others))
        d_own <= d_other
      }, logical(1))]
    }
    if (length(hits) == 0L) {
      res[[length(res) + 1L]] <- list(severity = "info", location = label,
                                       detail = sprintf("no line starting with \"%s\" was found near its caption in the PDF, so the caption placement was not compared", substr(anchor, 1, 40)))
      next
    }
    dist <- abs(lines$position[hits] - cap_pos)
    ord <- order(dist)
    if (length(hits) > 1L && dist[ord[2]] < 1.5 * max(dist[ord[1]], 1)) {
      res[[length(res) + 1L]] <- list(severity = "info", location = label,
                                       detail = sprintf("\"%s\" occurs in more than one place near the caption in the PDF, so the caption placement was not compared", substr(anchor, 1, 40)))
      next
    }
    nearest <- hits[ord[1]]
    claimed[nearest] <- TRUE
    pdf_placement <- if (cap_pos < lines$position[nearest]) "above" else "below"
    if (!identical(pdf_placement, caps$placement[k])) {
      res[[length(res) + 1L]] <- list(
        severity = "warning", location = label,
        detail = sprintf("the caption is %s the table in the Word file and %s it in the PDF", caps$placement[k], pdf_placement)
      )
    }
  }
  res
}

.rp_number_tokens <- function(text) {
  text <- gsub("(\\d)\\s+%", "\\1%", text)
  regmatches(text, gregexpr("[0-9]+\\.[0-9]+%?|[0-9]+%", text))[[1]]
}

.rp_number_findings <- function(docx_text, pdf_text) {
  d <- table(.rp_number_tokens(docx_text))
  p <- table(.rp_number_tokens(pdf_text))
  all_tokens <- union(names(d), names(p))
  res <- list()
  for (tok in all_tokens) {
    nd <- if (tok %in% names(d)) as.integer(d[[tok]]) else 0L
    np <- if (tok %in% names(p)) as.integer(p[[tok]]) else 0L
    if (nd != np) {
      res[[length(res) + 1L]] <- list(
        location = tok,
        detail = sprintf("occurs %d time(s) in the Word file and %d time(s) in the PDF", nd, np)
      )
    }
  }
  res[order(vapply(res, function(r) r$location, character(1)))]
}
