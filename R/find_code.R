#' Search for Code Patterns Across a Directory Tree
#'
#' Recursively searches R-related files (\code{.R}, \code{.Rmd}, \code{.qmd},
#' etc.) under \code{directory} for a string or regex pattern.
#'
#' @param directory Character. Root directory to search.
#' @param pattern Character. String or regex to search for.
#' @param lines_before,lines_after Integers. Context lines around each hit.
#' @param regex Logical. If \code{TRUE}, treat \code{pattern} as a Perl regex.
#' @param ignore_case Logical. Case-insensitive when \code{TRUE} (works
#'   correctly even in fixed-string mode, where base \code{\link{grep}}'s own
#'   \code{ignore.case} argument is silently ignored).
#' @param include_comments Logical. If \code{FALSE}, skip comment-only lines.
#' @param exts Character vector of file extensions to include.
#' @param exclude_dirs Character vector of directory names to exclude.
#' @param return_all_matches Logical. If \code{TRUE}, returns one row per
#'   extracted match token (via \code{match_extractor}) instead of one row
#'   per matching line.
#' @param match_extractor Optional \code{function(content_subset)} returning
#'   a list of tokens per line; only used when \code{return_all_matches = TRUE}.
#' @return A tibble: \code{file}, \code{path}, \code{line_number}, \code{line},
#'   and (when \code{return_all_matches = TRUE}) \code{match}.
#' @export
#' @examples
#' find_code(system.file("R", package = "TempleCBE"), "roxygen2", ignore_case = TRUE)
find_code <- function(
    directory,
    pattern,
    lines_before = 0,
    lines_after = 0,
    regex = FALSE,
    ignore_case = FALSE,
    include_comments = TRUE,
    exts = c("R", "r", "Rmd", "rmd", "qmd"),
    exclude_dirs = c(".git", "renv", "_freeze", "_book", "_site", ".quarto",
                      "_extensions", "cache", ".Rproj.user"),
    return_all_matches = FALSE,
    match_extractor = NULL
) {
  stopifnot(dir.exists(directory))

  ext_re <- paste0("\\.(", paste(unique(exts), collapse = "|"), ")$")
  sep <- .Platform$file.sep

  empty_result <- tibble::tibble(file = character(0), path = character(0),
                                  line_number = integer(0), line = character(0))

  all_files <- list.files(directory, pattern = ext_re, full.names = TRUE,
                           ignore.case = TRUE, recursive = TRUE)
  if (!length(all_files)) return(empty_result)

  excluded_mask <- Reduce(`|`, lapply(exclude_dirs, function(d) {
    grepl(paste0("(^|", sep, ")", d, "(", sep, "|$)"), all_files)
  }), init = FALSE)
  files <- all_files[!excluded_mask]
  if (!length(files)) return(empty_result)

  fixed_flag <- !regex
  perl_flag  <- isTRUE(regex) && !fixed_flag

  out_list <- lapply(files, function(f) {
    content <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(content)) return(NULL)

    idx_pool <- seq_along(content)
    if (!include_comments) {
      idx_pool <- which(!grepl("^\\s*#", content))
      if (!length(idx_pool)) return(NULL)
    }

    search_content <- content[idx_pool]
    search_pattern <- pattern
    if (ignore_case && fixed_flag) {
      # base grep() ignores `ignore.case` when `fixed = TRUE`; lowercase both
      # sides ourselves so ignore_case actually works in fixed-string mode.
      search_content <- tolower(search_content)
      search_pattern <- tolower(search_pattern)
    }

    hits_local <- grep(search_pattern, search_content,
                        ignore.case = if (fixed_flag) FALSE else ignore_case,
                        fixed = fixed_flag, perl = perl_flag)
    if (!length(hits_local)) return(NULL)

    line_numbers <- idx_pool[hits_local]
    lb <- max(0, as.integer(lines_before))
    la <- max(0, as.integer(lines_after))

    if (isTRUE(return_all_matches) && is.function(match_extractor)) {
      pieces <- lapply(seq_along(line_numbers), function(j) {
        ln <- line_numbers[j]
        s <- max(1, ln - lb); e <- min(length(content), ln + la)
        block <- content[s:e]
        tokens <- match_extractor(block)
        tokens <- if (!is.null(tokens[[1]])) tokens[[1]] else character(0)
        if (!length(tokens)) return(NULL)
        tibble::tibble(file = basename(f), path = f, line_number = ln,
                        line = paste(block, collapse = "\n"), match = tokens)
      })
      pieces <- pieces[lengths(pieces) > 0]
      if (!length(pieces)) return(NULL)
      dplyr::bind_rows(pieces)
    } else {
      extracted_lines <- vapply(line_numbers, function(ln) {
        s <- max(1, ln - lb); e <- min(length(content), ln + la)
        paste(content[s:e], collapse = "\n")
      }, character(1))
      tibble::tibble(file = basename(f), path = f, line_number = line_numbers, line = extracted_lines)
    }
  })

  out <- dplyr::bind_rows(out_list)
  if (nrow(out) == 0) return(empty_result)
  out
}
