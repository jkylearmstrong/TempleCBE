#' Normalize a Path Safely (Robust Across Relative/Absolute & Slash Styles)
#' @keywords internal
#' @noRd
.normalize_safely <- function(x) {
  x <- ifelse(is.na(x) | trimws(x) == "", NA_character_, x)
  suppressWarnings(
    vapply(x, function(xx) {
      if (is.na(xx)) return(NA_character_)
      tryCatch(normalizePath(xx, winslash = "\\", mustWork = FALSE),
               error = function(e) xx)
    }, FUN.VALUE = character(1))
  )
}

#' Vectorized Parser for `here::here('a', 'b', 'file.ext')` Calls
#' @keywords internal
#' @noRd
.parse_here_call_vec <- function(x) {
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

#' Vectorized File Metadata Lookup
#' @keywords internal
#' @noRd
.file_meta_fs <- function(paths) {
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

#' Audit Data File Read/Write Calls Against a Project's Files on Disk
#'
#' Scans \code{code_path} for read and write calls (via
#' \code{\link{read_search}}/\code{\link{write_search}}), resolves the file
#' paths those calls reference — including \code{here::here()} calls, and a
#' heuristic fallback for indirect references like
#' \code{excel_file_paths[["ABG"]]} matched against filenames actually
#' present on disk — and cross-references them against every matching file
#' under \code{project_root}. The result classifies each such file as a
#' write output the code produces, a read input the code consumes, both, or
#' an orphan with no code reference found (\code{"unknown"}) — useful for
#' finding stale, missing, or undocumented data deliverables in a project.
#'
#' @param code_path Directory of scripts (\code{.R}/\code{.Rmd}/\code{.qmd})
#'   to search for read/write calls.
#' @param project_root Root directory to inventory files under, and to
#'   resolve relative/\code{here::here()} paths against (useful when
#'   auditing a checkout at a different location than the one the scripts
#'   were written against). Defaults to \code{code_path}.
#' @param ext File extension to audit, without the leading dot (e.g.
#'   \code{"xlsx"}, \code{"csv"}, \code{"rds"}). Defaults to \code{"xlsx"}.
#' @param strong_read_patterns Character vector of \code{\link{read_search}}
#'   pattern names that should always count as a read of a file with this
#'   extension, even on a line that doesn't otherwise mention \code{ext}
#'   (e.g. a call to a project-specific reader function). Defaults to
#'   \code{c("read_workbook", "read_excel_multiple_headers")}.
#' @param include_comments_write Logical, passed through to
#'   \code{\link{write_search}}. Defaults to \code{FALSE}.
#'
#' @return A list with:
#'   \item{writes}{Resolved file-write call sites.}
#'   \item{inputs}{Resolved file-read call sites.}
#'   \item{missing_write_dirs}{Directories containing a write target that
#'     doesn't exist on disk yet, and any matching-extension files present
#'     there now — a likely spot for a not-yet-(re)generated deliverable.}
#'   \item{files}{Every file with extension \code{ext} under
#'     \code{project_root}, tagged \code{write_output}/\code{workflow_input}/
#'     \code{unknown} and by which script(s) reference it.}
#' @export
#'
#' @examples
#' \dontrun{
#' scan_data_io(here::here("analysis"), ext = "xlsx")
#' }
scan_data_io <- function(code_path,
                          project_root = code_path,
                          ext = "xlsx",
                          strong_read_patterns = c("read_workbook", "read_excel_multiple_headers"),
                          include_comments_write = FALSE) {
  stopifnot(dir.exists(code_path), dir.exists(project_root))

  ws <- write_search(code_path, include_comments = include_comments_write)
  rs <- read_search(code_path, include_comments = TRUE)

  actual_root <- .normalize_safely(here::here())
  custom_root <- .normalize_safely(project_root)

  adjust_here_path <- function(p) {
    if (is.null(p) || length(p) == 0) return(p)
    p_norm <- .normalize_safely(p)
    actual_root_lower <- tolower(actual_root)
    actual_root_lower_slash <- if (grepl("[/\\\\]$", actual_root_lower)) actual_root_lower else paste0(actual_root_lower, .Platform$file.sep)

    out <- p
    for (i in seq_along(p_norm)) {
      pn <- p_norm[i]
      if (!is.na(pn) && !is.na(actual_root)) {
        pn_lower <- tolower(pn)
        if (startsWith(pn_lower, actual_root_lower_slash)) {
          rel <- substring(pn, nchar(actual_root_lower_slash) + 1)
          out[i] <- file.path(custom_root, rel)
        } else if (pn_lower == actual_root_lower) {
          out[i] <- custom_root
        }
      }
    }
    out
  }

  # `token_core` matches a bare filename with this extension. `bare_token_re`
  # additionally requires it NOT be immediately preceded by a path separator,
  # so it only catches free-standing mentions (used for `str_extract_all`
  # token scanning) rather than re-matching a filename that's actually part
  # of a `full_path_re` match, which glues the filename directly onto a
  # captured separator and so must use the unrestricted `token_core`.
  token_core <- paste0("[^/\\\\\\n]+\\.", ext, "\\b")
  bare_token_re <- paste0("(?<![/\\\\])", token_core)
  # Group 1: directory, group 2: separator (dropped), group 3: filename token
  full_path_re <- paste0(
    "((?:[A-Za-z]:)?(?:[/\\\\][^/\\\\\\n]+)+)",
    "([/\\\\])",
    "(", token_core, ")"
  )

  writes_multi <- ws |>
    dplyr::mutate(
      here_path = adjust_here_path(.parse_here_call_vec(line)),
      m = stringr::str_match(line, full_path_re),
      output_dir = m[, 2],
      output_file_from_path = m[, 4],
      output_file_tokens = stringr::str_extract_all(line, bare_token_re)
    ) |>
    dplyr::select(file, path, line_number, line, pattern, here_path, output_dir, output_file_from_path, output_file_tokens) |>
    tidyr::unnest_longer(output_file_tokens, values_to = "output_file", keep_empty = TRUE)

  writes_resolved <- writes_multi |>
    dplyr::mutate(
      outfile_path = dplyr::case_when(
        !is.na(here_path) ~ here_path,
        !is.na(output_dir) & !is.na(output_file_from_path) ~ file.path(output_dir, output_file_from_path),
        !is.na(output_dir) & !is.na(output_file) ~ file.path(output_dir, output_file),
        !is.na(output_file) ~ output_file,
        TRUE ~ NA_character_
      ),
      outfile_path = .normalize_safely(outfile_path),
      output_file = dplyr::coalesce(output_file_from_path, output_file)
    ) |>
    dplyr::distinct(file, path, line_number, output_file, outfile_path, .keep_all = TRUE)

  existing_write_tbl <- writes_resolved |>
    dplyr::filter(!is.na(outfile_path)) |>
    dplyr::mutate(path = .normalize_safely(outfile_path)) |>
    dplyr::group_by(path) |>
    dplyr::summarise(
      write_pattern = paste(unique(pattern), collapse = ", "),
      generating_script = paste(unique(file), collapse = ", "),
      write_from_code = TRUE,
      .groups = "drop"
    )

  inputs <- rs |>
    dplyr::mutate(
      has_token_str = stringr::str_detect(line, ext),
      is_strong_read = pattern %in% strong_read_patterns,
      workflow_input = has_token_str | is_strong_read
    ) |>
    dplyr::filter(workflow_input) |>
    dplyr::mutate(
      here_path = adjust_here_path(.parse_here_call_vec(line)),
      m = stringr::str_match(line, full_path_re),
      input_dir = m[, 2],
      input_file_from_path = m[, 4],
      input_file_tokens = stringr::str_extract_all(line, bare_token_re),
      key_matches = stringr::str_extract_all(line, "(\\[\\[['\"]?[a-zA-Z0-9_]+['\"]?\\]\\]|\\$[a-zA-Z0-9_]+)")
    ) |>
    dplyr::select(file, path, line_number, line, pattern, here_path, inputs_dir = input_dir, input_file_from_path, input_file_tokens, key_matches) |>
    tidyr::unnest_longer(input_file_tokens, values_to = "input_file", keep_empty = TRUE) |>
    tidyr::unnest_longer(key_matches, values_to = "key_match", keep_empty = TRUE) |>
    dplyr::mutate(
      key_match = stringr::str_remove_all(key_match, "^\\[\\[['\"]?|['\"]?\\]\\]$|^\\$")
    ) |>
    dplyr::mutate(
      infile_path = dplyr::case_when(
        !is.na(here_path) ~ here_path,
        !is.na(inputs_dir) & !is.na(input_file_from_path) ~ file.path(inputs_dir, input_file_from_path),
        !is.na(inputs_dir) & !is.na(input_file) ~ file.path(inputs_dir, input_file),
        !is.na(input_file) ~ as.character(input_file),
        TRUE ~ NA_character_
      ),
      infile_path = .normalize_safely(infile_path),
      input_file = dplyr::coalesce(input_file_from_path, as.character(input_file)),
      workflow_input = TRUE
    ) |>
    dplyr::distinct(file, path, line_number, input_file, infile_path, .keep_all = TRUE)

  missing_paths <- writes_resolved$outfile_path[
    !is.na(writes_resolved$outfile_path) & !file.exists(writes_resolved$outfile_path)
  ]
  dirs_deliverables <- unique(dirname(missing_paths))
  dir_names <- make.unique(basename(dirs_deliverables))
  names(dirs_deliverables) <- dir_names

  if (length(dirs_deliverables)) {
    present_by_dir <- dirs_deliverables |>
      lapply(\(d) if (!dir.exists(d)) character(0) else list.files(d, recursive = TRUE, full.names = TRUE, pattern = paste0("\\.", ext, "$"))) |>
      purrr::list_c() |>
      unique() |>
      as.character()

    dirs_tbl <- tibble::tibble(
      dir_name = names(dirs_deliverables),
      dir_path = dirs_deliverables
    ) |>
      dplyr::left_join(.file_meta_fs(present_by_dir), by = c("dir_name", "dir_path")) |>
      dplyr::mutate(
        path = .normalize_safely(path),
        file_name = dplyr::if_else(is.na(path), NA_character_, basename(path)),
        deliverable = TRUE
      )
  } else {
    dirs_tbl <- tibble::tibble(
      dir_name = character(0), dir_path = character(0),
      path = character(0), file_name = character(0), deliverable = logical(0)
    )
  }

  all_files <- list.files(project_root, pattern = paste0("\\.", ext, "$"), full.names = TRUE, recursive = TRUE)
  all_files_tbl <- .file_meta_fs(all_files) |>
    dplyr::mutate(path = .normalize_safely(path))

  inputs_heur <- inputs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      heuristic_path = if (isTRUE(is.na(infile_path)) && !is.na(key_match)[1]) {
        matches <- all_files_tbl$path[stringr::str_detect(all_files_tbl$file_name, stringr::fixed(key_match[1]))]
        if (length(matches) > 0) matches[1] else NA_character_
      } else {
        NA_character_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(infile_path = dplyr::coalesce(infile_path, heuristic_path)) |>
    dplyr::filter(!is.na(infile_path)) |>
    dplyr::distinct(file, path, line_number, input_file, infile_path, .keep_all = TRUE)

  files_joined <- all_files_tbl |>
    dplyr::left_join(dirs_tbl |> dplyr::select(path, deliverable), by = "path") |>
    dplyr::left_join(existing_write_tbl, by = "path") |>
    dplyr::left_join(
      inputs_heur |>
        dplyr::group_by(infile_path) |>
        dplyr::summarise(
          read_pattern = paste(unique(pattern), collapse = ", "),
          calling_script = paste(unique(file), collapse = ", "),
          workflow_input = TRUE,
          .groups = "drop"
        ) |>
        dplyr::transmute(path = infile_path, workflow_input, read_pattern, calling_script),
      by = "path"
    ) |>
    dplyr::mutate(
      deliverable = dplyr::coalesce(deliverable, FALSE),
      write_from_code = dplyr::coalesce(write_from_code, FALSE),
      workflow_input = dplyr::coalesce(workflow_input, FALSE),
      write_flag = deliverable | write_from_code
    )

  classified <- files_joined |>
    dplyr::mutate(
      file_class = dplyr::case_when(
        write_flag ~ "write_output",
        workflow_input ~ "workflow_input",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::select(
      file_class, file_name, dir_name, calling_script, generating_script,
      write_pattern, read_pattern, path, m_time, c_time, uname
    )

  list(
    writes = writes_resolved,
    inputs = inputs,
    missing_write_dirs = dirs_tbl,
    files = classified
  )
}
