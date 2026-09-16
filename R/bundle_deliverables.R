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
#' @param output_formats Character vector of report formats to include (\code{"pdf"}, \code{"docx"}, \code{"html"}, or \code{"all"}).
#' @param data_deliverables Optional list or vector of \code{FilePath} objects or file paths for data spreadsheets/RDS files.
#' @param zip_path Destination path for the generated zip archive. If NULL, defaults to a timestamped archive.
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

  temp_staging <- file.path(tempdir(), paste0("stage_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (dir.exists(temp_staging)) unlink(temp_staging, recursive = TRUE)
  dir.create(temp_staging, recursive = TRUE)

  copied_files <- character(0)

  # 1. Gather Reports from Pipeline
  for (obj in pipeline_objects) {
    if (inherits(obj, "FileOutputs") && obj@renders) {
      stage_dir <- if (!is.na(obj@stage) && nzchar(obj@stage)) obj@stage else "Reports"
      dest_stage <- file.path(temp_staging, stage_dir)
      if (!dir.exists(dest_stage)) dir.create(dest_stage, recursive = TRUE)

      for (fmt in output_formats) {
        expected_path <- sub("\\.[^.]+$", paste0(".", fmt), obj@path)
        if (file.exists(expected_path)) {
          dest_file <- file.path(dest_stage, basename(expected_path))
          file.copy(expected_path, dest_file, overwrite = TRUE)
          copied_files <- c(copied_files, dest_file)
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
        dst <- file.path(data_dest, basename(p))
        file.copy(p, dst, overwrite = TRUE)
        copied_files <- c(copied_files, dst)
      }
    }
  }

  if (length(copied_files) == 0) {
    warning("No deliverable files found on disk to package.")
    return(invisible(NULL))
  }

  # 3. Create Zip Archive
  if (!requireNamespace("zip", quietly = TRUE)) {
    unlink(temp_staging, recursive = TRUE)
    stop("Package 'zip' is required for package_deliverables().", call. = FALSE)
  }

  zip::zip(
    zipfile = zip_path,
    files = list.files(temp_staging, full.names = FALSE, recursive = FALSE),
    root = temp_staging
  )

  unlink(temp_staging, recursive = TRUE)
  message(sprintf("Successfully packaged %d files into: %s", length(copied_files), zip_path))
  invisible(zip_path)
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
