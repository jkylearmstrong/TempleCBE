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
#' @param max_depth Maximum folder depth below \code{project_root} to inventory
#'   files in (default \code{Inf}, no limit).
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
                          include_comments_write = FALSE,
                          max_depth = Inf) {
  stopifnot(dir.exists(code_path), dir.exists(project_root))

  if (!is.character(ext) || length(ext) != 1 || is.na(ext)) {
    stop("ext must be a single non-NA character string", call. = FALSE)
  }
  ext <- sub("^\\.+", "", ext)
  if (!grepl("^[A-Za-z0-9]+$", ext)) {
    stop("ext must be a single alphanumeric file extension without special characters or alternation", call. = FALSE)
  }
  ext_escaped <- stringr::str_escape(ext)

  ws <- write_search(code_path, include_comments = include_comments_write)
  rs <- read_search(code_path, include_comments = TRUE)

  actual_root <- normalize_safely(here::here())
  custom_root <- normalize_safely(project_root)

  is_absolute_path <- function(p) {
    grepl("^[~/]|^[A-Za-z]:", p)
  }

  is_subpath <- function(p, root) {
    if (is.na(p) || is.na(root)) return(FALSE)
    # .canonical_path() resolves links and 8.3 names for the part of `p` that
    # exists, so a file that is not there yet is spelled like its folder
    p_norm <- tolower(.canonical_path(p))
    r_norm <- tolower(.canonical_path(root))
    r_slash <- if (grepl("[/\\\\]$", r_norm)) r_norm else paste0(r_norm, "/")
    startsWith(p_norm, r_slash) || p_norm == r_norm
  }

  adjust_here_path <- function(p) {
    if (is.null(p) || length(p) == 0) return(p)
    p_norm <- normalize_safely(p)
    actual_root_lower <- tolower(actual_root)
    actual_root_lower_slash <- if (grepl("[/\\\\]$", actual_root_lower)) actual_root_lower else paste0(actual_root_lower, "/")

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

  extract_call_text <- function(line, pattern) {
    purrr::map2_chr(line, pattern, function(l, pat) {
      if (is.na(l) || !nzchar(l)) return("")
      pos <- regexpr(pat, l, fixed = TRUE)
      if (pos < 0) return(l)
      sub_l <- substring(l, pos)
      open_paren <- regexpr("\\(", sub_l)
      if (open_paren < 0) return(sub_l)
      chars <- strsplit(substring(sub_l, open_paren), "")[[1]]
      depth <- 0
      end_idx <- length(chars)
      for (i in seq_along(chars)) {
        ch <- chars[i]
        if (ch == "(") {
          depth <- depth + 1
        } else if (ch == ")") {
          depth <- depth - 1
          if (depth == 0) {
            end_idx <- i
            break
          }
        }
      }
      substring(sub_l, 1, open_paren + end_idx - 1)
    })
  }

  extract_file_tokens <- function(lines, ext_pat) {
    quoted_re <- paste0("['\"`]([^'\"`\\n]+\\.", ext_pat, ")['\"`]")
    bare_re <- paste0("(?<![/\\\\A-Za-z0-9_.-])([A-Za-z0-9_.-]+\\.", ext_pat, ")\\b(?!\\s*\\()")

    purrr::map(lines, function(l) {
      if (is.na(l) || !nzchar(l)) return(character(0))
      m_q <- stringr::str_match_all(l, quoted_re)[[1]]
      q_toks <- if (nrow(m_q) > 0) m_q[, 2] else character(0)

      # Mask quoted strings so bare token matching does not pick up suffixes of quoted paths
      l_no_quotes <- stringr::str_replace_all(l, "['\"`][^'\"`\\n]*['\"`]", " ")
      m_b <- stringr::str_match_all(l_no_quotes, bare_re)[[1]]
      b_toks <- if (nrow(m_b) > 0) m_b[, 2] else character(0)

      unique(c(q_toks, b_toks))
    })
  }

  writes_multi <- ws |>
    dplyr::mutate(
      call_text = extract_call_text(line, pattern),
      here_path = as.character(adjust_here_path(parse_here_call_vec(line))),
      file_tokens = extract_file_tokens(.data$call_text, ext_escaped)
    ) |>
    tidyr::unnest_longer(file_tokens, values_to = "raw_token", keep_empty = TRUE) |>
    dplyr::mutate(
      raw_token = as.character(raw_token),
      output_file = dplyr::case_when(
        !is.na(here_path) ~ basename(here_path),
        !is.na(raw_token) ~ basename(raw_token),
        TRUE ~ NA_character_
      ),
      outfile_path = dplyr::case_when(
        !is.na(here_path) ~ here_path,
        !is.na(raw_token) & is_absolute_path(raw_token) ~ normalize_safely(raw_token),
        !is.na(raw_token) ~ normalize_safely(file.path(custom_root, raw_token)),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(file, path, line_number, line, pattern, here_path, output_file, outfile_path) |>
    dplyr::distinct(file, path, line_number, output_file, outfile_path, .keep_all = TRUE)

  writes_resolved <- writes_multi

  existing_write_tbl <- writes_resolved |>
    dplyr::filter(!is.na(outfile_path)) |>
    dplyr::mutate(path = normalize_safely(outfile_path)) |>
    dplyr::group_by(path) |>
    dplyr::summarise(
      write_pattern = paste(unique(pattern), collapse = ", "),
      generating_script = paste(unique(file), collapse = ", "),
      write_from_code = TRUE,
      .groups = "drop"
    )

  inputs <- rs |>
    dplyr::mutate(
      has_token_str = stringr::str_detect(line, ext_escaped),
      is_strong_read = pattern %in% strong_read_patterns,
      workflow_input = has_token_str | is_strong_read
    ) |>
    dplyr::filter(workflow_input) |>
    dplyr::mutate(
      call_text = extract_call_text(line, pattern),
      here_path = as.character(adjust_here_path(parse_here_call_vec(line))),
      file_tokens = extract_file_tokens(.data$call_text, ext_escaped),
      key_matches = stringr::str_extract_all(line, "(\\[\\[['\"]?[a-zA-Z0-9_]+['\"]?\\]\\]|\\$[a-zA-Z0-9_]+)")
    ) |>
    tidyr::unnest_longer(file_tokens, values_to = "raw_token", keep_empty = TRUE) |>
    tidyr::unnest_longer(key_matches, values_to = "key_match", keep_empty = TRUE) |>
    dplyr::mutate(
      raw_token = as.character(raw_token),
      here_path = as.character(here_path),
      key_match = as.character(key_match),
      key_match = stringr::str_remove_all(key_match, "^\\[\\[['\"]?|['\"]?\\]\\]$|^\\$"),
      input_file = dplyr::case_when(
        !is.na(here_path) ~ basename(here_path),
        !is.na(raw_token) ~ basename(raw_token),
        TRUE ~ NA_character_
      ),
      infile_path = dplyr::case_when(
        !is.na(here_path) ~ here_path,
        !is.na(raw_token) & is_absolute_path(raw_token) ~ normalize_safely(raw_token),
        !is.na(raw_token) ~ normalize_safely(file.path(custom_root, raw_token)),
        TRUE ~ NA_character_
      ),
      workflow_input = TRUE
    ) |>
    dplyr::select(file, path, line_number, line, pattern, here_path, input_file, infile_path, key_match, workflow_input) |>
    dplyr::distinct(file, path, line_number, input_file, infile_path, key_match, .keep_all = TRUE)

  missing_paths <- writes_resolved$outfile_path[
    !is.na(writes_resolved$outfile_path) & !file.exists(writes_resolved$outfile_path)
  ]
  raw_dirs <- unique(dirname(missing_paths))
  dirs_deliverables <- raw_dirs[vapply(raw_dirs, is_subpath, logical(1), root = custom_root)]
  dir_names <- make.unique(basename(dirs_deliverables))
  names(dirs_deliverables) <- dir_names

  if (length(dirs_deliverables) > 0) {
    present_by_dir <- dirs_deliverables |>
      lapply(function(d) {
        if (!dir.exists(d)) return(character(0))
        fls <- fs::dir_ls(
          path = d,
          recurse = if (is.finite(max_depth)) max_depth else TRUE,
          regexp = paste0("\\.", ext_escaped, "$"),
          fail = FALSE
        )
        fls <- fls[!grepl("(?:^|[/\\\\])(?:renv|node_modules|\\.git)(?:[/\\\\]|$)", fls)]
        as.character(head(fls, 1000))
      }) |>
      purrr::list_c() |>
      unique() |>
      as.character()

    dirs_tbl <- tibble::tibble(
      dir_name = names(dirs_deliverables),
      dir_path = dirs_deliverables
    ) |>
      dplyr::left_join(file_meta_fs(present_by_dir), by = c("dir_name", "dir_path")) |>
      dplyr::mutate(
        path = normalize_safely(path),
        file_name = dplyr::if_else(is.na(path), NA_character_, basename(path)),
        deliverable = TRUE
      )
  } else {
    dirs_tbl <- tibble::tibble(
      dir_name = character(0), dir_path = character(0),
      path = character(0), file_name = character(0), deliverable = logical(0)
    )
  }

  all_files <- fs::dir_ls(path = project_root, recurse = TRUE, regexp = paste0("\\.", ext_escaped, "$"), fail = FALSE)
  all_files <- all_files[!grepl("(?:^|[/\\\\])(?:renv|node_modules|\\.git)(?:[/\\\\]|$)", all_files)]
  if (is.finite(max_depth)) {
    rel <- fs::path_rel(all_files, start = project_root)
    all_files <- all_files[stringr::str_count(rel, "/") + 1 <= max_depth]
  }
  all_files_tbl <- file_meta_fs(as.character(all_files)) |>
    dplyr::mutate(path = normalize_safely(path))

  inputs_heur <- inputs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      heuristic_path = if (isTRUE(is.na(infile_path)) && !is.na(key_match)[1]) {
        km <- key_match[1]
        if (nchar(km) >= 3) {
          stem_re <- paste0("(^|[^A-Za-z0-9_])", stringr::str_escape(km), "($|[^A-Za-z0-9_])")
          matches <- all_files_tbl$path[stringr::str_detect(all_files_tbl$file_name, stem_re)]
          if (length(matches) > 0) matches[1] else NA_character_
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      is_heuristic = !is.na(heuristic_path) & is.na(infile_path),
      infile_path = dplyr::coalesce(infile_path, heuristic_path)
    ) |>
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

# dplyr NSE column names, not real globals (some, like `m`, are created and
# read back within the same `mutate()` call, which static analysis can't see).
utils::globalVariables(c(
  "c_time", "calling_script", "deliverable", "dir_name", "file_class",
  "file_name", "file_tokens", "generating_script", "has_token_str", "here_path",
  "heuristic_path", "infile_path", "input_file", "is_heuristic", "is_strong_read",
  "key_match", "key_matches", "line", "line_number", "m_time", "outfile_path",
  "output_file", "path", "pattern", "raw_token", "read_pattern", "uname",
  "workflow_input", "write_flag", "write_from_code", "write_pattern"
))
