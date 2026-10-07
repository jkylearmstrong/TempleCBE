#' Read Excel Data With Multi-Row Column Headers
#'
#' Variant of \code{\link[readxl]{read_excel}} for sheets where column names
#' are split across multiple header rows. The rows are concatenated in order,
#' joined with \code{sep}. Supports hierarchical/merged headers via horizontal
#' forward-filling across upper header tiers, isolates text parsing for headers
#' so data \code{col_types} do not collide, and performs robust name repair.
#'
#' @param path Path to the \code{.xls}/\code{.xlsx} file.
#' @param n_header_rows Number of header rows in the sheet (positive integer >= 1).
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an
#'   integer (the position of the sheet). Defaults to \code{1}.
#' @param fill_merged Logical (default \code{FALSE}); if \code{TRUE}, handles
#'   merged cells in upper header rows by forward-filling non-empty values
#'   left-to-right across columns before vertical concatenation.
#' @param sep Character string used to separate header levels. Default \code{" | "}.
#' @param clean_names Logical (default \code{FALSE}); if \code{TRUE}, applies
#'   \code{\link{clean_names}} to sanitize the final column names.
#' @param trim Logical (default \code{TRUE}); whether to trim leading/trailing
#'   whitespace from header cells.
#' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}
#'   for reading the data body (e.g. \code{col_types}, \code{na}, \code{guess_max}).
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2)
#' read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2, fill_merged = TRUE)
#' read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2, clean_names = TRUE)
#' }
read_excel_multiple_headers <- function(path,
                                        n_header_rows = 2L,
                                        sheet = 1,
                                        fill_merged = FALSE,
                                        sep = " | ",
                                        clean_names = FALSE,
                                        trim = TRUE,
                                        ...) {
  # 1. Defensive parameter validation
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a single non-empty character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File '%s' does not exist.", path), call. = FALSE)
  }
  if (!is.numeric(n_header_rows) || length(n_header_rows) != 1L || is.na(n_header_rows) ||
      n_header_rows < 1 || n_header_rows != as.integer(n_header_rows)) {
    stop("`n_header_rows` must be a single positive integer (>= 1).", call. = FALSE)
  }
  n_header_rows <- as.integer(n_header_rows)

  if (!is.logical(fill_merged) || length(fill_merged) != 1L || is.na(fill_merged)) {
    stop("`fill_merged` must be a single logical (TRUE or FALSE).", call. = FALSE)
  }

  if (!is.character(sep) || length(sep) != 1L) {
    stop("`sep` must be a single character string.", call. = FALSE)
  }

  # 2. Extract header rows strictly as text (isolates header parsing from data col_types)
  header_raw <- suppressMessages(readxl::read_excel(
    path = path,
    sheet = sheet,
    n_max = n_header_rows,
    col_names = FALSE,
    col_types = "text"
  ))

  if (nrow(header_raw) == 0L || ncol(header_raw) == 0L) {
    return(tibble::tibble())
  }

  if (nrow(header_raw) < n_header_rows) {
    warning(
      sprintf(
        "Requested %d header rows, but sheet '%s' only contains %d rows total.",
        n_header_rows, as.character(sheet), nrow(header_raw)
      ),
      call. = FALSE
    )
    n_header_rows <- nrow(header_raw)
  }

  header_mat <- as.matrix(header_raw)

  # 3. Handle hierarchical merged cells across columns in upper header tiers
  if (isTRUE(fill_merged) && n_header_rows > 1L) {
    for (r in seq_len(n_header_rows - 1L)) {
      row_vals <- header_mat[r, ]
      last_val <- NA_character_
      for (c in seq_along(row_vals)) {
        v <- row_vals[c]
        if (!is.na(v) && nzchar(trimws(v))) {
          last_val <- v
        } else if (!is.na(last_val)) {
          row_vals[c] <- last_val
        }
      }
      header_mat[r, ] <- row_vals
    }
  }

  # 4. Construct column names by collapsing vertical levels
  my_colnames <- character(ncol(header_mat))
  for (c in seq_len(ncol(header_mat))) {
    pieces <- header_mat[, c]
    if (isTRUE(trim)) {
      pieces <- trimws(pieces)
    }
    pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
    if (length(pieces) == 0L) {
      my_colnames[c] <- paste0("col_", c)
    } else {
      my_colnames[c] <- paste(pieces, collapse = sep)
    }
  }

  # Ensure names are valid, non-empty, and unique
  my_colnames <- vctrs::vec_as_names(my_colnames, repair = "unique", quiet = TRUE)

  if (isTRUE(clean_names)) {
    my_colnames <- clean_names(my_colnames)
  }

  # 5. Read data body with caller options (...)
  data <- readxl::read_excel(
    path = path,
    sheet = sheet,
    col_names = FALSE,
    skip = n_header_rows,
    ...
  )

  # Assign names matching data column count
  if (ncol(data) > 0L) {
    if (ncol(data) <= length(my_colnames)) {
      colnames(data) <- my_colnames[seq_len(ncol(data))]
    } else {
      extra_names <- paste0("col_", seq(length(my_colnames) + 1L, ncol(data)))
      colnames(data) <- c(my_colnames, extra_names)
    }
  }

  data
}
