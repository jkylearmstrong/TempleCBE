#' Deliverable Packaging and Pipeline Artifact Auditing
#'
#' Functions to collect, organize, and package analytical deliverables
#' (rendered PDF/DOCX/HTML reports, data tables, and spreadsheets) into structured
#' client delivery archives based on pipeline stage hierarchies.
#'
#' @keywords internal
#' @name bundle_deliverables
NULL

#' Package Pipeline Deliverables into a Structured Zip Archive
#'
#' Traverses computational pipeline objects (or explicit file lists), gathers
#' all rendered reports and data deliverables, verifies existence on disk,
#' organizes them into stage-prefixed folders, and builds a distribution ZIP file.
#'
#' @param pipeline_objects A list of \code{FilePath} / \code{FileOutputs} objects (e.g. from the compute graph).
#'   The \code{stage} of each \code{FileOutputs} becomes a folder name in the archive: path separators and
#'   the characters a file name cannot hold on Windows become \code{-}, and leading or trailing dots, dashes
#'   and spaces are dropped (\code{"../x"} gives \code{"x"}).
#' @param output_formats Character vector of report formats to include (\code{"pdf"}, \code{"docx"}, \code{"html"}, or \code{"all"}).
#' @param data_deliverables Optional list or vector of \code{FilePath} objects or file paths for data spreadsheets/RDS files.
#' @param zip_path Destination path for the generated zip archive. If NULL, defaults to a timestamped archive.
#'   Deliverables that share a file name within a folder are kept, the later ones with a numeric
#'   suffix (and a warning), rather than overwriting each other.
#' @param include_pipeline_graph Logical; whether to include static/interactive pipeline DAG graphs (default: TRUE).
#' @param stamp Logical, `Date`, or character string (default `FALSE`). When
#'   `TRUE`, appends the current date (`YYYY-MM-DD`) to the archive name (and
#'   records `attr(path, "stamp")`). A custom date or version string (e.g.
#'   `"2026-10-17"`) may also be supplied.
#' @return Absolute path to the generated zip file.
#' @seealso [diff_deliverables()], [audit_report_deliverables()]
#' @export
package_deliverables <- function(pipeline_objects = list(),
                                 output_formats = c("pdf", "docx"),
                                 data_deliverables = list(),
                                 zip_path = NULL,
                                 include_pipeline_graph = TRUE,
                                 stamp = FALSE) {
  if ("all" %in% output_formats) {
    output_formats <- c("pdf", "docx", "html")
  }

  stamp_str <- NULL
  if (isTRUE(stamp)) {
    stamp_str <- format(Sys.Date(), "%Y-%m-%d")
  } else if (inherits(stamp, c("Date", "POSIXt"))) {
    stamp_str <- format(stamp[1], "%Y-%m-%d")
  } else if (is.character(stamp) && length(stamp) == 1L && nzchar(stamp)) {
    stamp_str <- gsub("[^A-Za-z0-9._-]", "_", stamp)
  }

  if (is.null(zip_path)) {
    suffix <- if (!is.null(stamp_str)) stamp_str else format(Sys.time(), "%Y%m%d_%H%M%S")
    zip_path <- file.path(
      tempdir(),
      sprintf("CBE_Deliverables_%s.zip", suffix)
    )
  } else if (!is.null(stamp_str)) {
    stem <- tools::file_path_sans_ext(basename(zip_path))
    ext <- tools::file_ext(zip_path)
    if (!nzchar(ext)) ext <- "zip"
    if (!endsWith(stem, paste0("_", stamp_str))) {
      stem <- paste0(stem, "_", stamp_str)
    }
    zip_path <- file.path(dirname(zip_path), paste0(stem, ".", ext))
  }
  # zip::zip(root = ) resolves a relative `zipfile` against `root` -- the staging
  # folder, which is deleted below -- so the archive would be silently destroyed.
  # Resolve against the caller's working directory up front.
  zip_path <- file.path(normalizePath(dirname(zip_path), winslash = "/", mustWork = FALSE), basename(zip_path))

  temp_staging <- file.path(tempdir(), paste0("stage_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (dir.exists(temp_staging)) unlink(temp_staging, recursive = TRUE)
  dir.create(temp_staging, recursive = TRUE)
  # Removed on every way out, including a zip::zip() that fails.
  on.exit(unlink(temp_staging, recursive = TRUE), add = TRUE)

  copied_files <- character(0)
  # Copies one deliverable into the staging folder and records it; a copy that
  # fails is said so and does not count as packaged.
  stage_deliverable <- function(from, dest) {
    if (!dir.exists(from) && isTRUE(suppressWarnings(file.copy(from, dest, overwrite = TRUE)))) {
      copied_files <<- c(copied_files, dest)
    } else {
      warning("Could not copy '", from, "' into the archive; it is left out.", call. = FALSE)
    }
  }

  # 1. Gather Reports from Pipeline
  for (obj in pipeline_objects) {
    if (inherits(obj, "FileOutputs") && obj@renders) {
      dest_stage <- file.path(temp_staging, safe_stage_dir(obj@stage, "Reports"))
      if (!dir.exists(dest_stage)) dir.create(dest_stage, recursive = TRUE)

      for (fmt in output_formats) {
        expected_path <- sub("\\.[^.]+$", paste0(".", fmt), obj@path)
        if (file.exists(expected_path)) {
          dest_file <- unique_staging_path(dest_stage, basename(expected_path), copied_files)
          stage_deliverable(expected_path, dest_file)
        }
      }
    }
  }

  # 2. Gather Data Deliverables
  if (length(data_deliverables) > 0) {
    data_dest <- file.path(temp_staging, "00_Data_Deliverables")
    if (!dir.exists(data_dest)) dir.create(data_dest, recursive = TRUE)

    for (item in data_deliverables) {
      p <- if (inherits(item, "FilePath")) item@path else as.character(item)
      if (file.exists(p)) {
        dest_file <- unique_staging_path(data_dest, basename(p), copied_files)
        stage_deliverable(p, dest_file)
      }
    }
  }

  if (length(copied_files) == 0) {
    warning("No deliverable files found on disk to package.")
    return(invisible(NULL))
  }

  # 3. Create Zip Archive
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Package 'zip' is required for package_deliverables().", call. = FALSE)
  }

  zip::zip(
    zipfile = zip_path,
    files = list.files(temp_staging, full.names = FALSE, recursive = FALSE),
    root = temp_staging
  )

  if (!is.null(stamp_str)) {
    attr(zip_path, "stamp") <- stamp_str
  }

  message(sprintf("Successfully packaged %d files into: %s", length(copied_files), zip_path))
  invisible(zip_path)
}

# A staging folder holds one file per name, so two deliverables sharing a file
# name (data/a/results.xlsx and data/b/results.xlsx) would silently overwrite
# each other -- while still being counted as two packaged files. The later one
# gets a numeric suffix instead (compared case-insensitively, as on Windows and
# macOS file systems).
#' @keywords internal
#' @noRd
unique_staging_path <- function(dir, name, taken) {
  dest <- file.path(dir, name)
  if (!(tolower(dest) %in% tolower(taken))) {
    return(dest)
  }
  stem <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  k <- 2L
  repeat {
    candidate <- file.path(dir, paste0(stem, "_", k, if (nzchar(ext)) paste0(".", ext)))
    if (!(tolower(candidate) %in% tolower(taken))) break
    k <- k + 1L
  }
  warning("Two deliverables share the file name '", name, "'; the later one is packaged as '",
          basename(candidate), "'.", call. = FALSE)
  candidate
}

# A stage name becomes a folder inside the staging directory, so it must not be
# able to leave it: "../../x" would write outside the build folder while the zip
# lacked the file. Path separators and the characters Windows forbids in a file
# name (: * ? " < > |), and control characters, become "-"; leading and trailing
# dots, dashes and spaces go (no "." or ".." folder, no hidden folder). Spaces
# and non-ASCII letters are kept, so ordinary names such as "01 Results" stay as
# they are. An empty result falls back to `default`.
#' @keywords internal
#' @noRd
safe_stage_dir <- function(stage, default) {
  stage <- if (length(stage) != 1L || is.na(stage)) "" else as.character(stage)
  stage <- gsub("[\\\\/:*?\"<>|[:cntrl:]]+", "-", stage)
  stage <- gsub("^[-. ]+|[-. ]+$", "", stage)
  if (nzchar(stage)) stage else default
}

#' Audit Deliverable File Tokens in Analysis Scripts
#'
#' Scans source Quarto and R scripts to extract read/write file references and
#' verify whether declared data and report deliverables exist on disk.
#'
#' @param analysis_path Directory containing Quarto (.qmd) and R (.R) scripts.
#' @param file_pattern Regular expression matching deliverable extensions (default: \code{"\\.(xlsx|rds|pdf|docx|html)$"}).
#' @return A tibble summarizing referenced deliverable files, source lines, and on-disk existence.
#' @export
audit_report_deliverables <- function(analysis_path = ".", file_pattern = "\\.(xlsx|rds|pdf|docx|html)$") {
  script_files <- list.files(
    analysis_path,
    pattern = "\\.(qmd|R|Rmd)$",
    full.names = TRUE,
    recursive = TRUE
  )

  results <- list()

  for (f in script_files) {
    lines <- readLines(f, warn = FALSE)
    line_pattern <- sub("\\$$", "", file_pattern)
    match_idx <- grep(line_pattern, lines, ignore.case = TRUE)

    if (length(match_idx) > 0) {
      for (idx in match_idx) {
        line_txt <- lines[idx]
        tokens <- unlist(stringr::str_extract_all(line_txt, "['\"][^'\"]+\\.[a-zA-Z0-9]+['\"]"))
        clean_tokens <- gsub("['\"]", "", tokens)
        clean_tokens <- clean_tokens[grepl(file_pattern, clean_tokens, ignore.case = TRUE)]

        for (tok in clean_tokens) {
          results[[length(results) + 1]] <- tibble::tibble(
            script = f,
            line_number = idx,
            token = tok,
            exists_on_disk = file.exists(tok) || file.exists(file.path(analysis_path, tok))
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    return(tibble::tibble(script = character(), line_number = integer(), token = character(), exists_on_disk = logical()))
  }

  dplyr::bind_rows(results)
}

#' Extract Paragraph Text Vector From a DOCX File
#'
#' @keywords internal
#' @noRd
.extract_docx_paragraphs <- function(docx_path) {
  if (!file.exists(docx_path) || dir.exists(docx_path)) {
    return(character())
  }
  tmp_dir <- tempfile("docx_diff_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  ok <- tryCatch({
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::unzip(docx_path, files = "word/document.xml", exdir = tmp_dir)
    } else {
      utils::unzip(docx_path, files = "word/document.xml", exdir = tmp_dir)
    }
    TRUE
  }, error = function(e) FALSE)

  xml_file <- file.path(tmp_dir, "word", "document.xml")
  if (!isTRUE(ok) || !file.exists(xml_file)) {
    # Fallback if the file is plain text in tests
    lines <- tryCatch(readLines(docx_path, warn = FALSE), error = function(e) character())
    return(trimws(lines[nzchar(trimws(lines))]))
  }

  doc <- tryCatch(xml2::read_xml(xml_file), error = function(e) NULL)
  if (is.null(doc)) {
    return(character())
  }
  ns <- xml2::xml_ns(doc)
  p_nodes <- tryCatch(
    if ("w" %in% names(ns)) xml2::xml_find_all(doc, ".//w:p", ns = ns) else xml2::xml_find_all(doc, ".//*[local-name()='p']"),
    error = function(e) list()
  )
  if (length(p_nodes) == 0L) {
    return(character())
  }
  texts <- vapply(p_nodes, function(nd) {
    t_nodes <- if ("w" %in% names(ns)) {
      xml2::xml_find_all(nd, ".//w:t", ns = ns)
    } else {
      xml2::xml_find_all(nd, ".//*[local-name()='t']")
    }
    if (length(t_nodes) == 0L) return("")
    trimws(paste(xml2::xml_text(t_nodes), collapse = ""))
  }, character(1))
  texts[nzchar(texts)]
}

#' Compare Two Deliverable Bundles and Diff DOCX Reports
#'
#' Compares two deliverable `.zip` archives (or directories) produced by
#' [package_deliverables()], identifying added, removed, unchanged, and modified
#' files. For Word (`.docx`) and text/HTML/Markdown reports, performs a
#' paragraph/line-level text diff so review notes can state which reports moved
#' and what text changed.
#'
#' @param old_zip Path to the baseline `.zip` deliverable archive (or directory).
#' @param new_zip Path to the updated `.zip` deliverable archive (or directory).
#' @return A tibble of class `cbe_deliverable_diff` with columns:
#'   `file` (relative path in the bundle), `status` (`"unchanged"`, `"modified"`,
#'   `"added"`, or `"removed"`), `hash_changed` (logical), `text_changed`
#'   (logical), `n_added` (integer count of added paragraphs/lines), `n_removed`
#'   (integer count of removed paragraphs/lines), `diff_summary` (concise text
#'   summary of changes), and `diff_lines` (list column of `-`/`+` diff lines).
#' @seealso [package_deliverables()]
#' @export
diff_deliverables <- function(old_zip, new_zip) {
  if (!is.character(old_zip) || length(old_zip) != 1L || !file.exists(old_zip)) {
    stop("`old_zip` must be an existing `.zip` file or directory path.", call. = FALSE)
  }
  if (!is.character(new_zip) || length(new_zip) != 1L || !file.exists(new_zip)) {
    stop("`new_zip` must be an existing `.zip` file or directory path.", call. = FALSE)
  }

  unpack_source <- function(src, prefix) {
    if (dir.exists(src)) {
      return(list(dir = normalizePath(src, winslash = "/"), cleanup = FALSE))
    }
    td <- tempfile(prefix)
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::unzip(src, exdir = td)
    } else {
      utils::unzip(src, exdir = td)
    }
    list(dir = normalizePath(td, winslash = "/"), cleanup = TRUE)
  }

  old_u <- unpack_source(old_zip, "diff_old_")
  if (isTRUE(old_u$cleanup)) on.exit(unlink(old_u$dir, recursive = TRUE), add = TRUE)
  new_u <- unpack_source(new_zip, "diff_new_")
  if (isTRUE(new_u$cleanup)) on.exit(unlink(new_u$dir, recursive = TRUE), add = TRUE)

  list_rel <- function(root_dir) {
    f <- list.files(root_dir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
    sort(gsub("\\\\", "/", f))
  }

  old_files <- list_rel(old_u$dir)
  new_files <- list_rel(new_u$dir)
  all_files <- sort(unique(c(old_files, new_files)))

  if (length(all_files) == 0L) {
    out <- tibble::tibble(
      file = character(),
      status = character(),
      hash_changed = logical(),
      text_changed = logical(),
      n_added = integer(),
      n_removed = integer(),
      diff_summary = character(),
      diff_lines = list()
    )
    class(out) <- c("cbe_deliverable_diff", class(out))
    return(out)
  }

  extract_text_units <- function(fpath) {
    ext <- tolower(tools::file_ext(fpath))
    if (ext == "docx") {
      return(.extract_docx_paragraphs(fpath))
    }
    if (ext %in% c("txt", "md", "qmd", "rmd", "csv", "html")) {
      lines <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) character())
      return(trimws(lines[nzchar(trimws(lines))]))
    }
    NULL
  }

  rows <- lapply(all_files, function(rel_f) {
    in_old <- rel_f %in% old_files
    in_new <- rel_f %in% new_files
    old_p <- file.path(old_u$dir, rel_f)
    new_p <- file.path(new_u$dir, rel_f)

    if (!in_old && in_new) {
      tu <- extract_text_units(new_p)
      n_add <- if (!is.null(tu)) length(tu) else 0L
      return(tibble::tibble(
        file = rel_f,
        status = "added",
        hash_changed = TRUE,
        text_changed = TRUE,
        n_added = as.integer(n_add),
        n_removed = 0L,
        diff_summary = "New deliverable added",
        diff_lines = list(if (!is.null(tu) && length(tu) > 0L) paste0("+ ", utils::head(tu, 20)) else character())
      ))
    }
    if (in_old && !in_new) {
      tu <- extract_text_units(old_p)
      n_rem <- if (!is.null(tu)) length(tu) else 0L
      return(tibble::tibble(
        file = rel_f,
        status = "removed",
        hash_changed = TRUE,
        text_changed = TRUE,
        n_added = 0L,
        n_removed = as.integer(n_rem),
        diff_summary = "Deliverable removed",
        diff_lines = list(if (!is.null(tu) && length(tu) > 0L) paste0("- ", utils::head(tu, 20)) else character())
      ))
    }

    h_old <- .file_content_hash(old_p)
    h_new <- .file_content_hash(new_p)
    h_changed <- !identical(h_old, h_new)

    old_tu <- extract_text_units(old_p)
    new_tu <- extract_text_units(new_p)

    if (!is.null(old_tu) && !is.null(new_tu)) {
      t_changed <- !identical(old_tu, new_tu)
      if (!t_changed) {
        return(tibble::tibble(
          file = rel_f,
          status = "unchanged",
          hash_changed = h_changed,
          text_changed = FALSE,
          n_added = 0L,
          n_removed = 0L,
          diff_summary = "No text changes",
          diff_lines = list(character())
        ))
      }
      # Compute added/removed paragraph/line multiset difference
      added_lines <- setdiff(new_tu, old_tu)
      removed_lines <- setdiff(old_tu, new_tu)
      # If setdiff is empty (e.g. reordered or duplicate count changed), compare index by index
      if (length(added_lines) == 0L && length(removed_lines) == 0L) {
        max_len <- max(length(old_tu), length(new_tu))
        for (idx in seq_len(max_len)) {
          o_v <- if (idx <= length(old_tu)) old_tu[idx] else NA_character_
          n_v <- if (idx <= length(new_tu)) new_tu[idx] else NA_character_
          if (!identical(o_v, n_v)) {
            if (!is.na(o_v)) removed_lines <- c(removed_lines, o_v)
            if (!is.na(n_v)) added_lines <- c(added_lines, n_v)
          }
        }
      }
      d_lines <- c(
        if (length(removed_lines) > 0L) paste0("- ", removed_lines) else character(),
        if (length(added_lines) > 0L) paste0("+ ", added_lines) else character()
      )
      preview <- if (length(added_lines) > 0L) {
        sprintf(" (+ \"%s\")", substr(added_lines[1], 1L, 60L))
      } else if (length(removed_lines) > 0L) {
        sprintf(" (- \"%s\")", substr(removed_lines[1], 1L, 60L))
      } else {
        ""
      }
      sum_txt <- sprintf("+%d / -%d paragraph(s)%s", length(added_lines), length(removed_lines), preview)
      return(tibble::tibble(
        file = rel_f,
        status = "modified",
        hash_changed = h_changed,
        text_changed = TRUE,
        n_added = as.integer(length(added_lines)),
        n_removed = as.integer(length(removed_lines)),
        diff_summary = sum_txt,
        diff_lines = list(d_lines)
      ))
    }

    # Binary file (e.g. PDF, XLSX, RDS)
    tibble::tibble(
      file = rel_f,
      status = if (h_changed) "modified" else "unchanged",
      hash_changed = h_changed,
      text_changed = h_changed,
      n_added = 0L,
      n_removed = 0L,
      diff_summary = if (h_changed) "Binary file modified" else "Unchanged",
      diff_lines = list(character())
    )
  })

  out <- dplyr::bind_rows(rows)
  attr(out, "old_zip") <- old_zip
  attr(out, "new_zip") <- new_zip
  class(out) <- c("cbe_deliverable_diff", class(out))
  out
}

#' @export
print.cbe_deliverable_diff <- function(x, ...) {
  cat("TempleCBE Deliverable Bundle Comparison\n")
  cat("---------------------------------------\n")
  n_mod <- sum(x$status == "modified")
  n_add <- sum(x$status == "added")
  n_rem <- sum(x$status == "removed")
  n_unc <- sum(x$status == "unchanged")
  cat(sprintf("Summary: %d modified, %d added, %d removed, %d unchanged\n",
              n_mod, n_add, n_rem, n_unc))
  changed <- x[x$status != "unchanged", , drop = FALSE]
  if (nrow(changed) > 0L) {
    cat("\nChanged Deliverables:\n")
    for (i in seq_len(nrow(changed))) {
      cat(sprintf("  * [%s] %s: %s\n",
                  toupper(changed$status[i]), changed$file[i], changed$diff_summary[i]))
    }
  }
  invisible(x)
}

