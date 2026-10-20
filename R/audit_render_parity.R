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
#' A finding that could not be checked (a table that could not be found in the PDF)
#' is listed as \code{info}.
#'
#' @details
#' This reads text and positions only; it does not look at the pages. It is
#' meant to point at places to look at, not to certify a document:
#' \itemize{
#'   \item A caption is a line (PDF) or paragraph (Word) that begins with
#'     \code{Table} or \code{Figure} (\code{Fig.}), a number and a \code{.} or
#'     \code{:}, or a Word paragraph in the \code{Caption} style that begins with
#'     them. A sentence that begins "Table 1 shows" is not a caption.
#'   \item The place of a table in the PDF is found from the first text of its
#'     first row; a table whose first cell is empty or repeated elsewhere may not
#'     be found.
#'   \item Text in text boxes, headers, footers and footnotes of the Word file is
#'     not read. Math in Word is read as text (\code{m:t}).
#'   \item PDF text is as extracted by \pkg{pdftools}: a number that the PDF engine
#'     splits across lines or breaks with a space can appear as a difference.
#' }
#' The Word file is an untrusted archive and is read with the same size limits as
#' \code{\link{docx_review_extract}()} (\code{docx_limits}).
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

  pdf_caps <- pdf$captions
  docx_caps <- docx$captions

  if ("numbering" %in% check) {
    for (f in .rp_numbering_findings(docx_caps, pdf_caps)) add("numbering", "warning", f$location, f$detail)
  }
  if ("order" %in% check) {
    for (f in .rp_order_findings(docx_caps, pdf_caps)) add("order", "warning", f$location, f$detail)
  }
  if ("placement" %in% check) {
    for (f in .rp_placement_findings(docx, pdf)) add("caption_placement", f$severity, f$location, f$detail)
  }
  if ("numbers" %in% check) {
    for (f in .rp_number_findings(docx$text, pdf$text)) add("numbers", "warning", f$location, f$detail)
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
    captions = list(pdf = pdf_caps, docx = docx_caps)
  )
}

#' @export
print.render_parity <- function(x, ...) {
  n_warn <- sum(x$severity == "warning")
  if (nrow(x) == 0L) {
    cat("No differences found between the PDF and the Word file.\n")
  } else {
    cat(sprintf("%d difference(s) to look at (%d warning, %d info):\n", nrow(x), n_warn, nrow(x) - n_warn))
    print(tibble::as_tibble(x), n = Inf, width = Inf, ...)
  }
  invisible(x)
}

# ---- captions -----------------------------------------------------------------------------------

.rp_caption_pattern <- "^\\s*(Table|Figure|Fig\\.?)\\s+([0-9]+(?:\\.[0-9]+)?)\\s*([.:–—-])"

# list(kind = "Table"/"Figure", number = "2", label = "Table 2") or NULL
.rp_parse_caption <- function(text, style_caption = FALSE) {
  m <- regmatches(text, regexec(.rp_caption_pattern, text, perl = TRUE))[[1]]
  if (length(m) == 0L && isTRUE(style_caption)) {
    # a paragraph in the Caption style needs no separator after the number
    m <- regmatches(text, regexec("^\\s*(Table|Figure|Fig\\.?)\\s+([0-9]+(?:\\.[0-9]+)?)(?![0-9.])", text, perl = TRUE))[[1]]
  }
  if (length(m) == 0L) return(NULL)
  kind <- if (grepl("^Fig", m[2])) "Figure" else "Table"
  list(kind = kind, number = m[3], label = paste(kind, m[3]))
}

# ---- PDF ----------------------------------------------------------------------------------------

.rp_read_pdf <- function(path) {
  pages <- tryCatch(pdftools::pdf_data(path), error = function(e) {
    stop("The PDF could not be read: ", conditionMessage(e), call. = FALSE)
  })
  sizes <- pdftools::pdf_pagesize(path)
  text <- paste(pdftools::pdf_text(path), collapse = "\n")
  words <- dplyr::bind_rows(lapply(seq_along(pages), function(i) {
    d <- pages[[i]]
    if (nrow(d) == 0L) return(NULL)
    d$page <- i
    d$page_width <- sizes$width[i]
    d
  }))
  if (is.null(words) || nrow(words) == 0L) {
    words <- tibble::tibble(
      width = integer(), height = integer(), x = integer(), y = integer(), space = logical(),
      text = character(), page = integer(), page_width = numeric()
    )
  }
  lines <- .rp_pdf_lines(words)
  caps <- list()
  for (i in seq_len(nrow(lines))) {
    cap <- .rp_parse_caption(lines$text[i])
    if (!is.null(cap)) {
      caps[[length(caps) + 1L]] <- tibble::tibble(
        kind = cap$kind, number = cap$number, label = cap$label,
        page = lines$page[i], y = lines$y[i], position = lines$position[i]
      )
    }
  }
  list(
    words = words, lines = lines, text = text, n_pages = length(pages),
    captions = if (length(caps)) dplyr::bind_rows(caps) else .rp_empty_captions("pdf")
  )
}

.rp_empty_captions <- function(source) {
  if (identical(source, "pdf")) {
    tibble::tibble(kind = character(), number = character(), label = character(),
                   page = integer(), y = integer(), position = numeric())
  } else {
    tibble::tibble(kind = character(), number = character(), label = character(), block = integer())
  }
}

# Words of a page grouped into lines (words whose tops are within 3 points) in reading order
.rp_pdf_lines <- function(words) {
  if (nrow(words) == 0L) {
    return(tibble::tibble(page = integer(), y = integer(), text = character(), position = numeric()))
  }
  out <- list()
  for (pg in sort(unique(words$page))) {
    w <- words[words$page == pg, , drop = FALSE]
    w <- w[order(w$y, w$x), , drop = FALSE]
    line_id <- cumsum(c(TRUE, diff(w$y) > 3))
    for (id in unique(line_id)) {
      lw <- w[line_id == id, , drop = FALSE]
      lw <- lw[order(lw$x), , drop = FALSE]
      out[[length(out) + 1L]] <- tibble::tibble(
        page = pg, y = min(lw$y), text = paste(lw$text, collapse = " "), position = pg * 10000 + min(lw$y)
      )
    }
  }
  dplyr::bind_rows(out)
}

.rp_margin_findings <- function(pdf, margin_in) {
  w <- pdf$words
  if (nrow(w) == 0L) return(list())
  margin_pt <- margin_in * 72
  left_gap <- w$x
  right_gap <- w$page_width - (w$x + w$width)
  bad <- left_gap < margin_pt | right_gap < margin_pt
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

.rp_math_ns <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  m = "http://schemas.openxmlformats.org/officeDocument/2006/math"
)

.rp_read_docx <- function(path, docx_limits) {
  limits <- resolve_docx_limits(docx_limits)
  listing <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) {
    stop("The Word file could not be read as a .docx archive: ", conditionMessage(e), call. = FALSE)
  })
  check_docx_listing(listing, limits)
  if (!"word/document.xml" %in% listing$Name) stop("The Word file has no word/document.xml.", call. = FALSE)
  td <- tempfile("parity_docx_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  unzip_docx_parts(path, "word/document.xml", td)
  doc <- read_docx_xml(file.path(td, "word", "document.xml"), "word/document.xml")
  body <- xml2::xml_find_first(doc, "//w:body", .rp_math_ns)
  nodes <- xml2::xml_find_all(body, "./w:p | ./w:tbl", .rp_math_ns)

  text_of <- function(node) paste(xml2::xml_text(xml2::xml_find_all(node, ".//w:t | .//m:t", .rp_math_ns)), collapse = "")
  blocks <- lapply(seq_along(nodes), function(i) {
    nd <- nodes[[i]]
    if (xml2::xml_name(nd) == "tbl") {
      cells <- vapply(xml2::xml_find_all(nd, ".//w:tc", .rp_math_ns), text_of, character(1))
      cells <- trimws(cells[nzchar(trimws(cells))])
      first_row <- xml2::xml_find_first(nd, ".//w:tr", .rp_math_ns)
      row_cells <- if (inherits(first_row, "xml_missing")) character() else {
        vapply(xml2::xml_find_all(first_row, ".//w:tc", .rp_math_ns), text_of, character(1))
      }
      row_cells <- trimws(row_cells[nzchar(trimws(row_cells))])
      list(type = "table", text = paste(cells, collapse = " "), anchor = if (length(row_cells)) row_cells[1] else NA_character_,
           style_caption = FALSE)
    } else {
      style <- xml2::xml_attr(xml2::xml_find_first(nd, "./w:pPr/w:pStyle", .rp_math_ns), "w:val", ns = .rp_math_ns)
      list(type = "paragraph", text = trimws(text_of(nd)), anchor = NA_character_,
           style_caption = identical(tolower(style), "caption"))
    }
  })
  types <- vapply(blocks, function(b) b$type, character(1))
  texts <- vapply(blocks, function(b) b$text, character(1))
  anchors <- vapply(blocks, function(b) b$anchor, character(1))

  caps <- list()
  for (i in which(types == "paragraph")) {
    cap <- .rp_parse_caption(texts[i], blocks[[i]]$style_caption)
    if (!is.null(cap)) {
      caps[[length(caps) + 1L]] <- tibble::tibble(kind = cap$kind, number = cap$number, label = cap$label, block = i)
    }
  }
  caps <- if (length(caps)) dplyr::bind_rows(caps) else .rp_empty_captions("docx")

  # which side of its table each table caption is on
  n <- length(blocks)
  placement <- rep(NA_character_, nrow(caps))
  table_of <- rep(NA_integer_, nrow(caps))
  for (k in seq_len(nrow(caps))) {
    if (caps$kind[k] != "Table") next
    b <- caps$block[k]
    next_tbl <- b < n && types[b + 1L] == "table"
    prev_tbl <- b > 1L && types[b - 1L] == "table"
    if (next_tbl && !prev_tbl) { placement[k] <- "above"; table_of[k] <- b + 1L }
    else if (prev_tbl && !next_tbl) { placement[k] <- "below"; table_of[k] <- b - 1L }
    else if (next_tbl && prev_tbl) { placement[k] <- "ambiguous"; table_of[k] <- NA_integer_ }
  }
  if (any(placement == "ambiguous", na.rm = TRUE)) {
    known <- placement[placement %in% c("above", "below")]
    choose <- if (length(known)) names(sort(table(known), decreasing = TRUE))[1] else "above"
    for (k in which(placement == "ambiguous")) {
      b <- caps$block[k]
      placement[k] <- choose
      table_of[k] <- if (choose == "above") b + 1L else b - 1L
    }
  }
  caps$placement <- placement
  caps$table_anchor <- ifelse(is.na(table_of), NA_character_, anchors[table_of])

  list(captions = caps, text = paste(texts, collapse = "\n"))
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

.rp_squish <- function(x) gsub("[[:space:]]+", " ", trimws(x))

.rp_placement_findings <- function(docx, pdf) {
  res <- list()
  caps <- docx$captions[docx$captions$kind == "Table" & !is.na(docx$captions$placement), , drop = FALSE]
  for (k in seq_len(nrow(caps))) {
    label <- caps$label[k]
    pdf_cap <- pdf$captions[pdf$captions$label == label, , drop = FALSE]
    anchor <- .rp_squish(caps$table_anchor[k])
    if (nrow(pdf_cap) == 0L) next
    if (is.na(anchor) || !nzchar(anchor)) {
      res[[length(res) + 1L]] <- list(severity = "info", location = label,
                                       detail = "its table has no first-row text to find in the PDF, so the caption placement was not compared")
      next
    }
    squished <- .rp_squish(pdf$lines$text)
    hits <- which(grepl(anchor, squished, fixed = TRUE) & !grepl(.rp_caption_pattern, pdf$lines$text, perl = TRUE))
    if (length(hits) == 0L) {
      res[[length(res) + 1L]] <- list(severity = "info", location = label,
                                       detail = sprintf("the table was not found in the PDF (looked for \"%s\"), so the caption placement was not compared", substr(anchor, 1, 40)))
      next
    }
    cap_pos <- pdf_cap$position[1]
    nearest <- hits[which.min(abs(pdf$lines$position[hits] - cap_pos))]
    pdf_placement <- if (cap_pos < pdf$lines$position[nearest]) "above" else "below"
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
  text <- gsub("−", "-", text, fixed = TRUE)
  text <- gsub("(\\d)\\s+%", "\\1%", text)
  tokens <- regmatches(text, gregexpr("[0-9]+\\.[0-9]+%?|[0-9]+%", text))[[1]]
  tokens
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
