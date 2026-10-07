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
#' @return Absolute path to the generated zip file.
#' @export
package_deliverables <- function(pipeline_objects,
                                 output_formats = c("pdf", "docx"),
                                 data_deliverables = list(),
                                 zip_path = NULL,
                                 include_pipeline_graph = TRUE) {
  if ("all" %in% output_formats) {
    output_formats <- c("pdf", "docx", "html")
  }

  if (is.null(zip_path)) {
    zip_path <- file.path(
      tempdir(),
      sprintf("CBE_Deliverables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
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
    if (isTRUE(suppressWarnings(file.copy(from, dest, overwrite = TRUE)))) {
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
