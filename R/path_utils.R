#' Normalize File Paths Without Failing
#'
#' A vectorized [normalizePath()] that never errors or warns: blank and `NA`
#' inputs become `NA`, and a path that cannot be normalized comes back as it
#' was. Paths need not exist. On Windows the separator is a backslash.
#'
#' @param x Character vector of paths.
#' @return A character vector of normalized paths, named by the inputs.
#' @seealso [scan_data_io()]
#' @export
#' @examples
#' normalize_safely(c("data/../data/file.csv", "", NA))
normalize_safely <- function(x) {
  x <- ifelse(is.na(x) | trimws(x) == "", NA_character_, x)
  suppressWarnings(
    vapply(x, function(xx) {
      if (is.na(xx)) return(NA_character_)
      tryCatch(normalizePath(xx, winslash = "\\", mustWork = FALSE),
               error = function(e) xx)
    }, FUN.VALUE = character(1))
  )
}

#' Resolve `here::here()` Calls Found in Code Text
#'
#' For each string, finds the first `here::here(...)` call and evaluates it
#' with its quoted string arguments, giving the path the call builds in the
#' current project. Calls whose arguments are not all string literals resolve
#' from the literals alone.
#'
#' @param x Character vector of code lines.
#' @return A character vector the length of `x`: the resolved path, or `NA`
#'   where a line has no `here::here()` call with quoted arguments.
#' @seealso [scan_data_io()], [find_code()]
#' @export
#' @examples
#' parse_here_call_vec(c("x <- readRDS(here::here('data', 'x.rds'))", "no call"))
parse_here_call_vec <- function(x) {
  m <- stringr::str_match(x, "here::here\\s*\\((.*?)\\)")
  inner <- m[, 2]
  out <- rep(NA_character_, length(x))
  idx <- which(!is.na(inner))
  if (!length(idx)) return(out)

  out[idx] <- vapply(idx, function(i) {
    parts <- stringr::str_match_all(inner[i], "(['\"])(.*?)\\1")[[1]]
    if (is.null(parts) || nrow(parts) == 0) return(NA_character_)
    args <- parts[, 3]
    do.call(here::here, as.list(args))
  }, FUN.VALUE = character(1))

  out
}

#' File Metadata as a Tibble
#'
#' Vectorized file information from [fs::file_info()], with names and parent
#' folders split out for joining.
#'
#' @param paths Character vector of file paths.
#' @return A tibble with one row per path: `path`, `file_name`, `dir_name`
#'   (parent folder name), `dir_path`, `m_time` (modified), `c_time` (created,
#'   falling back to changed, then modified), `size` in bytes, and `uname`
#'   (owner, where the platform reports one).
#' @seealso [scan_data_io()]
#' @export
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines("a,b", f)
#' file_meta_fs(f)
file_meta_fs <- function(paths) {
  if (!length(paths)) {
    return(tibble::tibble(
      path = character(0), file_name = character(0),
      dir_name = character(0), dir_path = character(0),
      m_time = as.POSIXct(character(0)), c_time = as.POSIXct(character(0)),
      size = numeric(0), uname = character(0)
    ))
  }
  finfo <- fs::file_info(paths)
  tibble::tibble(
    path = as.character(finfo$path),
    file_name = basename(paths),
    dir_name = basename(dirname(paths)),
    dir_path = dirname(paths),
    m_time = finfo$modification_time,
    c_time = dplyr::coalesce(finfo$birth_time, finfo$change_time, finfo$modification_time),
    size   = as.numeric(finfo$size),
    uname  = if ("user" %in% names(finfo)) finfo$user else NA_character_
  )
}

# An .xlsx file name: dots and spaces allowed; no path separators, line
# breaks, quotes, backticks, parentheses, commas, or `=`, so a match stops at
# the surrounding code instead of running back over it.
xlsx_name_re <- "[^/\\\\\\n'\"`(),=]+\\.xlsx\\b"

#' Extract Full `.xlsx` Paths From Text
#'
#' Finds the first full Windows or POSIX path ending in an `.xlsx` file name
#' in each string, and splits it into folder and file name.
#'
#' @param x Character vector, e.g. code lines.
#' @param xlsx_token_re Regular expression for the file-name part. The default
#'   matches a name ending in `.xlsx` that may contain dots and spaces but not
#'   path separators, quotes, backticks, parentheses, commas, or `=`.
#' @return A tibble with one row per element of `x`: `dir`, `file`, and
#'   `full_path` (`NA` where no path was found).
#' @seealso [extract_all_xlsx_tokens()]
#' @export
#' @examples
#' extract_win_posix_paths(c("C:\\data\\table_1.xlsx", "/home/me/out/table-2.xlsx", "none"))
extract_win_posix_paths <- function(x, xlsx_token_re = xlsx_name_re) {
  # The file name follows a separator in the full-path pattern, so drop any
  # "not after a separator" lookbehind a caller's token pattern carries.
  token_re <- stringr::str_remove(xlsx_token_re, "^\\(\\?<!\\[/\\\\\\\\\\]\\)")
  # Group 1: directory (optional Windows drive + one or more segments)
  # Group 2: file name ending in .xlsx
  re <- paste0("((?:[A-Za-z]:)?(?:[/\\\\][^/\\\\\\n]+)+)[/\\\\](", token_re, ")")
  m <- stringr::str_match(x, re)
  dir <- m[, 2]
  file <- m[, 3]
  full <- dplyr::if_else(!is.na(dir) & !is.na(file), as.character(fs::path(dir, file)), NA_character_)
  tibble::tibble(dir = dir, file = file, full_path = full)
}

#' Extract Every `.xlsx` File Name From Text
#'
#' @inheritParams extract_win_posix_paths
#' @return A list the length of `x`, each element a character vector of the
#'   `.xlsx` file names found in that string (empty when none).
#' @seealso [extract_win_posix_paths()]
#' @export
#' @examples
#' extract_all_xlsx_tokens(c("read_excel('out/a.xlsx'); write_xlsx(df, 'b c.xlsx')", "none"))
extract_all_xlsx_tokens <- function(x, xlsx_token_re = xlsx_name_re) {
  stringr::str_extract_all(x, xlsx_token_re)
}
