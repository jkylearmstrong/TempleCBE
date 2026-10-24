#' Read an Excel Sheet Whose Header Spans Several Rows
#'
#' Instrument and lab exports often have two or three header rows (a group title over several columns,
#' then the column names, sometimes units). This reads the sheet by position, combines the header rows
#' into one name per column and returns the data below them.
#'
#' @section How names are built:
#' For each column the non-blank cells of `header_rows` are joined with `"_"`, from the top row down.
#' A blank cell in a row **above the last header row** takes the value of the nearest non-blank cell to its
#' left in that row (a merged title that spans columns); a blank in the last header row does not.
#' A column with no header text at all is named by its Excel column letters (`"C"`, `"AB"`). With
#' `clean_names = TRUE` the names go through [janitor::make_clean_names()]; otherwise they are made unique
#' with a numeric suffix. All header cells are read as text.
#'
#' @param path Path to an `.xlsx` or `.xls` file.
#' @param sheet Sheet name or number (default 1).
#' @param header_rows Integer rows (of the sheet, counting from 1) that form the header; consecutive,
#'   for example `1:2`. The data start on the row after the last one.
#' @param clean_names If `TRUE` (default) tidy the combined names with [janitor::make_clean_names()].
#' @return A tibble of the data rows with the combined names. The attribute `"header"` has the raw header
#'   cells (one row per header row).
#' @seealso [excel_colnames()]
#' @export
read_positional_excel_headers <- function(path, sheet = 1, header_rows = 1:2, clean_names = TRUE) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("`path` must be an existing Excel file.", call. = FALSE)
  }
  if (!is.numeric(header_rows) || length(header_rows) == 0L || anyNA(header_rows) ||
      any(header_rows < 1) || any(header_rows != round(header_rows)) || any(diff(sort(header_rows)) != 1L)) {
    stop("`header_rows` must be consecutive positive whole numbers such as 1:2.", call. = FALSE)
  }
  header_rows <- sort(as.integer(header_rows))
  last <- max(header_rows)

  raw_all <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  if (nrow(raw_all) < last) stop("The sheet has only ", nrow(raw_all), " row(s); `header_rows` reaches row ", last, ".", call. = FALSE)
  hdr <- as.data.frame(raw_all[header_rows, , drop = FALSE], stringsAsFactors = FALSE)
  hdr[] <- lapply(hdr, function(x) { x <- trimws(as.character(x)); x[!nzchar(x)] <- NA_character_; x })
  hm <- as.matrix(hdr)
  # carry merged titles to the right, in the rows above the last header row
  if (nrow(hm) > 1L) {
    for (i in seq_len(nrow(hm) - 1L)) {
      for (j in seq_len(ncol(hm))[-1L]) {
        if (is.na(hm[i, j]) && !is.na(hm[i, j - 1L])) hm[i, j] <- hm[i, j - 1L]
      }
    }
  }
  nm <- apply(hm, 2, function(col) {
    col <- col[!is.na(col)]
    if (length(col) == 0L) NA_character_ else paste(col, collapse = "_")
  })
  letters_for <- function(i) {
    out <- character()
    while (i > 0L) {
      r <- (i - 1L) %% 26L
      out <- c(LETTERS[r + 1L], out)
      i <- (i - 1L) %/% 26L
    }
    paste(out, collapse = "")
  }
  blank <- is.na(nm)
  nm[blank] <- vapply(which(blank), letters_for, "")
  nm <- if (isTRUE(clean_names)) janitor::make_clean_names(nm) else make.unique(nm, sep = "_")

  data_rows <- raw_all[-seq_len(last), , drop = FALSE]
  out <- as.data.frame(data_rows, stringsAsFactors = FALSE)
  names(out) <- nm
  out <- utils::type.convert(out, as.is = TRUE)
  out <- tibble::as_tibble(out)
  attr(out, "header") <- tibble::as_tibble(stats::setNames(as.data.frame(hm, stringsAsFactors = FALSE), nm))
  out
}
