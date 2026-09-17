#' Search a Directory Tree for File-Read Calls
#'
#' Convenience wrapper around \code{\link{find_code}} that searches for common
#' data-import function calls (\code{readRDS}, \code{read.csv},
#' \code{read_excel}, \code{\link{read_workbook}},
#' \code{\link{read_excel_multiple_headers}}, etc.) across a directory tree.
#'
#' @param path Character. Root directory to search.
#' @param include_comments Logical. Passed through to \code{\link{find_code}}.
#'   Defaults to \code{TRUE}.
#' @param workers Integer number of parallel workers to use when
#'   \pkg{future}/\pkg{furrr} are installed. Defaults to \code{NULL}, which
#'   caps at available cores minus one. Falls back to sequential search when
#'   \pkg{future}/\pkg{furrr} are not installed.
#'
#' @return A tibble of matches (see \code{\link{find_code}}) with an
#'   additional \code{pattern} column recording which read function matched.
#' @export
#'
#' @examples
#' \dontrun{
#' read_search(here::here("analysis"))
#' }
read_search <- function(path, include_comments = TRUE, workers = NULL) {
  string_codes <- c(
    "readRDS", "read.csv",
    "read_csv", "readr::read_csv",
    "read_excel", "readxl::read_excel",
    "read_rds",
    "read_workbook",
    "read_excel_multiple_headers"
  )

  .search_codes(
    string_codes, path,
    include_comments = include_comments, lines_after = 0,
    workers = workers
  )
}

#' Search a Directory Tree for File-Write Calls
#'
#' Convenience wrapper around \code{\link{find_code}} that searches for common
#' data-export function calls (\code{write_xlsx}, \code{saveRDS},
#' \code{write.csv}, etc.) across a directory tree.
#'
#' @param path Character. Root directory to search.
#' @param include_comments Logical. Passed through to \code{\link{find_code}}.
#'   Defaults to \code{FALSE}.
#' @param workers Integer number of parallel workers to use when
#'   \pkg{future}/\pkg{furrr} are installed. Defaults to \code{NULL}, which
#'   caps at available cores minus one. Falls back to sequential search when
#'   \pkg{future}/\pkg{furrr} are not installed.
#'
#' @return A tibble of matches (see \code{\link{find_code}}) with an
#'   additional \code{pattern} column recording which write function matched.
#' @export
#'
#' @examples
#' \dontrun{
#' write_search(here::here("analysis"))
#' }
write_search <- function(path, include_comments = FALSE, workers = NULL) {
  string_codes <- c(
    "write_xlsx",
    "openxlsx::write.xlsx",
    "writexl::write_xlsx",
    "TempleCBE::write_xlsx",
    "saveRDS", "write_rds",
    "write.csv", "write_csv"
  )

  .search_codes(
    string_codes, path,
    include_comments = include_comments, lines_after = 2,
    workers = workers
  )
}

#' @keywords internal
#' @noRd
.search_codes <- function(string_codes, path, include_comments, lines_after, workers) {
  names(string_codes) <- string_codes

  can_parallelize <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr", quietly = TRUE)

  search_one <- function(s) {
    find_code(
      directory = path,
      pattern = s,
      regex = FALSE,
      ignore_case = FALSE,
      include_comments = include_comments,
      lines_before = 0, lines_after = lines_after
    ) |>
      dplyr::mutate(pattern = s)
  }

  if (can_parallelize) {
    if (is.null(workers)) workers <- max(1L, future::availableCores() - 1L)
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)

    string_codes |> furrr::future_map_dfr(search_one)
  } else {
    string_codes |> purrr::map_dfr(search_one)
  }
}
