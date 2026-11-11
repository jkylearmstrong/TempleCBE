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
#' @param workers Integer number of parallel workers. With a number greater than
#'   1 (and \pkg{future}/\pkg{furrr} installed) the search runs on that many
#'   \code{multisession} workers, and the previous \code{future::plan()} is put
#'   back afterwards. The default \code{NULL} does not touch the plan: the search
#'   uses the plan you have set, which is sequential unless you chose otherwise.
#'
#' @return A tibble of matches (see \code{\link{find_code}}) with an
#'   additional \code{pattern} column recording which read function matched.
#' @export
#'
#' @examples
#' \dontrun{
#' find_read_calls(here::here("analysis"))
#' }
find_read_calls <- function(path, include_comments = TRUE, workers = NULL) {
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
#' @param workers Integer number of parallel workers. With a number greater than
#'   1 (and \pkg{future}/\pkg{furrr} installed) the search runs on that many
#'   \code{multisession} workers, and the previous \code{future::plan()} is put
#'   back afterwards. The default \code{NULL} does not touch the plan: the search
#'   uses the plan you have set, which is sequential unless you chose otherwise.
#'
#' @return A tibble of matches (see \code{\link{find_code}}) with an
#'   additional \code{pattern} column recording which write function matched.
#' @export
#'
#' @examples
#' \dontrun{
#' find_write_calls(here::here("analysis"))
#' }
find_write_calls <- function(path, include_comments = FALSE, workers = NULL) {
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

  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr", quietly = TRUE)
  # Parallel only on request. With `workers = NULL` the plan the caller has set is used as it is
  # (sequential unless they chose otherwise): starting a multisession cluster just because future
  # is installed changed the session's plan behind the caller's back and, inside tests or another
  # parallel job, nested worker clusters (which deadlock on Windows).
  set_plan <- has_future && !is.null(workers) && workers > 1L
  can_parallelize <- has_future &&
    (set_plan || (is.null(workers) && !inherits(future::plan(), "sequential")))

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
    if (set_plan) {
      oplan <- future::plan(future::multisession, workers = workers)
      on.exit(future::plan(oplan), add = TRUE)
    }

    string_codes |> furrr::future_map_dfr(search_one)
  } else {
    string_codes |> purrr::map_dfr(search_one)
  }
}
