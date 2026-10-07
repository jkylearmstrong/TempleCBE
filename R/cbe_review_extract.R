#' DOCX Review Extractor & Multi-Reviewer Collaborative Tracker
#'
#' @description
#' Extracts comments, tracked changes, and reconstructed paragraph redlines
#' directly from Microsoft Word (`.docx`) files and maintains a collaborative
#' review workflow in a formatted multi-sheet Excel workbook and UTF-8 CSV files.
#'
#' Supports multi-reviewer workflows with per-reviewer fork files
#' (`review_tracker_<reviewer>.xlsx`), live Excel formulas, dropdown data validations,
#' conditional formatting, non-destructive merging across document revisions, and
#' analytical compute graph ordering.
#'
#' @details
#' \strong{What is read.} Comments (\code{word/comments.xml}, with reply threads
#' and the resolved flag from \code{word/commentsExtended.xml}) and the tracked
#' changes and comment ranges of the main document body
#' (\code{word/document.xml}). A text box is read once and counts as part of the
#' paragraph that holds it, so paragraph numbers are Word's.
#'
#' \strong{What is not read.} Tracked changes and comment ranges in footnotes,
#' endnotes, headers and footers are not extracted, and a comment anchored in
#' one of them is listed with no paragraph number and no selected text. They are
#' not dropped silently: when one of those parts holds either, a single warning
#' names the files, the parts and how many there are, so that they can be
#' reviewed in Word.
#'
#' @param input_dir Character path to a directory containing `.docx` files, or to
#'   a single `.docx` file. Defaults to the first folder of
#'   `review_config()$input_dirs` that holds `.docx` files (none is configured
#'   by default), else the current working directory.
#' @param output_dir Character path to the directory where review tracker workbooks
#'   and CSV files should be saved. If `NULL` (default), uses `<input_dir>/review_extract`
#'   (or `<input_dir>` if it is already named `review_extract`).
#' @param tracker Character filename for the master Excel workbook. Defaults to
#'   `"review_tracker.xlsx"`.
#' @param min_revision_length Integer minimum character count required to record a
#'   tracked change. Defaults to `2`.
#' @param fork_reviewers Character vector of reviewer identifiers who receive
#'   their own single-reviewer fork workbooks and whose per-comment sign-offs
#'   roll up into each document's `resolved` status. Defaults to
#'   `review_config()$fork_reviewers` (generic placeholders unless a project
#'   has called \code{\link{review_config}} to configure real reviewer ids).
#' @param signoff_reviewers Character vector of document-level sign-off
#'   reviewer identifiers (e.g. a final PI read), tracked on the master
#'   workbook's "Documents" sheet and not counted toward `resolved`; they
#'   appear in no fork workbook. Defaults to
#'   `review_config()$signoff_reviewers`.
#' @param documents_sheet_mode One of `"columns"` or `"per_reviewer"`; see
#'   \code{\link{review_config}}. Defaults to
#'   `review_config()$documents_sheet_mode`.
#' @param manifest_path Optional character path to `reports_to_render.xlsx`. If `NULL`,
#'   the function looks for it in the folder set with
#'   `review_config(analysis_dir = )` (nothing is searched when none is set).
#'   Pipeline stages come from that manifest and from
#'   `review_config()$pipeline_catalog`; with neither, documents are not
#'   assigned to a stage (stage `"Unknown / Extra"`) and are listed by file name.
#' @param verbose Logical indicating whether to print detailed progress messages.
#'   Defaults to `FALSE`.
#' @param docx_limits \code{NULL} (the defaults) or a named list limiting what is
#'   done with each \code{.docx}, which is an untrusted zip archive:
#'   \code{max_entries} (entries in the archive, default 10,000),
#'   \code{max_total_bytes} (summed uncompressed size, default 2 GiB) and
#'   \code{max_xml_bytes} (uncompressed size of any one XML part that is read,
#'   default 100 MiB). The listing is checked before anything is extracted, only
#'   the parts that are read are extracted, and a file over a limit is reported
#'   in \code{$errors} instead of being read. \code{Inf} switches a limit off.
#'   The session-wide options \code{review.docx_max_entries},
#'   \code{review.docx_max_bytes} and \code{review.docx_max_xml_bytes} set the
#'   defaults.
#'
#' @details
#' \strong{Merging a new round into the tracker.} A comment is identified by what
#' it says, not by its Word comment id (Word renumbers the ids when it saves). A
#' comment continues an existing row of the same file only when its text is the
#' same (compared without regard to whitespace); among rows with the same text
#' the one with the same author and date is taken first, then one with the same
#' author (or an author missing on one side), preferring the same comment id and
#' then the nearest paragraph. A comment whose text changed is a new comment: the
#' row it replaces is kept, marked \code{Prior Round / Not in docx}, when it holds
#' a sign-off or a note. Rows of documents that left the folder, or that could not
#' be read in this run, are kept the same way.
#'
#' \strong{Reviewer forks.} A reviewer's edits in their fork workbook
#' (\code{review_tracker_<id>.xlsx}) are merged back by file and comment (or
#' suggested change). Only cells the reviewer changed since the fork was written
#' are taken (each fork records what it was written with on a hidden sheet), so a
#' sign-off typed into the master workbook is not undone by a fork that left the
#' cell alone. If both were changed to different values the fork's value is kept
#' and a warning lists the cells; the previous master is in the \code{backups}
#' folder.
#'
#' \strong{Files that are not readable Word documents} (empty, not a ZIP archive,
#' no \code{word/document.xml}, or an XML error) are listed in \code{$errors} and
#' on the Errors sheet and reported in one warning; Word's \code{~$} lock files
#' are ignored. A run that finds no readable document creates and backs up
#' nothing.
#'
#' @return An object of class `cbe_review_extract`, which is a named list containing:
#'   \describe{
#'     \item{comments}{A tibble of merged comment records.}
#'     \item{revisions}{A tibble of merged raw tracked changes.}
#'     \item{redlines}{A tibble of reconstructed paragraph before/after redlines.}
#'     \item{documents}{A tibble of document-level summary metrics and review statuses.}
#'     \item{docxwalk}{A tibble crosswalking reviewed DOCX files to source `.qmd` and rendered outputs.}
#'     \item{summary}{A tibble summarizing recurring comments across documents.}
#'     \item{errors}{A tibble of any processing errors encountered.}
#'     \item{paths}{A named list of generated file paths.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Configure real reviewer identities once per project (kept out of source
#' # control), then run extraction with no further reviewer arguments needed:
#' review_config(fork_reviewers = c("alice", "bob", "carol"), signoff_reviewers = "lead_pi")
#' # Optionally describe your own pipeline stages, so documents are ordered and
#' # labelled by stage (the package ships none):
#' review_config(pipeline_catalog = list(
#'   list(stem = "overview", heading = 1, name = "Overview"),
#'   list(stem = "model_fit", heading = 2, name = "Model fitting")
#' ))
#' res <- cbe_docx_review_extract("path/to/reviewed_docx", verbose = TRUE)
#' print(res)
#' }
cbe_docx_review_extract <- function(input_dir = NULL,
                                   output_dir = NULL,
                                   tracker = "review_tracker.xlsx",
                                   min_revision_length = 2,
                                   fork_reviewers = NULL,
                                   signoff_reviewers = NULL,
                                   documents_sheet_mode = NULL,
                                   manifest_path = NULL,
                                   verbose = FALSE,
                                   docx_limits = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for cbe_docx_review_extract(). Please install it.", call. = FALSE)
  }
  docx_limits <- resolve_docx_limits(docx_limits)

  cfg <- review_config()
  if (is.null(fork_reviewers)) fork_reviewers <- cfg$fork_reviewers
  if (is.null(signoff_reviewers)) signoff_reviewers <- cfg$signoff_reviewers
  if (is.null(documents_sheet_mode)) documents_sheet_mode <- cfg$documents_sheet_mode
  documents_sheet_mode <- match.arg(documents_sheet_mode, c("columns", "per_reviewer"))

  # Reviewer ids end up in column, sheet and file names: reject unusable ones
  # before anything is read or written
  ids <- validate_reviewer_ids(fork_reviewers, signoff_reviewers)
  fork_reviewers <- ids$fork_reviewers
  signoff_reviewers <- ids$signoff_reviewers

  # 1. Resolve directories
  if (is.null(input_dir)) {
    input_dir <- find_default_review_input_dir()
  }
  input_dir <- normalizePath(input_dir, mustWork = FALSE)
  if (!file.exists(input_dir)) {
    stop(sprintf("Input path does not exist: %s", input_dir), call. = FALSE)
  }

  is_single_file <- !dir.exists(input_dir) && grepl("\\.docx$", input_dir, ignore.case = TRUE)
  scan_dir <- if (is_single_file) dirname(input_dir) else input_dir

  if (is.null(output_dir)) {
    if (basename(scan_dir) == "review_extract") {
      output_dir <- scan_dir
    } else {
      output_dir <- file.path(scan_dir, "review_extract")
    }
  }
  # Nothing is created, copied or backed up until the run has a readable
  # document to work on: a run that finds none has no side effects.
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  master_tracker_path <- file.path(output_dir, tracker)

  # 2. Discover DOCX files. A Word owner-lock file (~$name.docx) is not a document.
  if (is_single_file) {
    all_docx <- input_dir
  } else {
    all_docx <- sort(list.files(scan_dir, pattern = "\\.docx$", full.names = TRUE, ignore.case = TRUE), method = "radix")
  }
  all_docx <- all_docx[!startsWith(basename(all_docx), "~$")]

  if (length(all_docx) == 0) {
    if (verbose) message(sprintf("No .docx files found in %s", scan_dir))
    return(structure(
      list(
        comments = tibble::tibble(),
        revisions = tibble::tibble(),
        redlines = tibble::tibble(),
        documents = tibble::tibble(),
        docxwalk = tibble::tibble(),
        summary = tibble::tibble(),
        errors = tibble::tibble(),
        paths = list(master = master_tracker_path)
      ),
      class = "cbe_review_extract"
    ))
  }

  # A file that is not a readable Word document is reported, never skipped silently
  docx_problems <- vapply(all_docx, docx_problem, character(1), USE.NAMES = FALSE)
  invalid_mask <- !is.na(docx_problems)
  valid_docx <- all_docx[!invalid_mask]
  invalid_docx <- all_docx[invalid_mask]
  invalid_errors <- tibble::tibble(
    file = basename(invalid_docx),
    error = docx_problems[invalid_mask],
    traceback = rep("", length(invalid_docx))
  )

  if (length(invalid_docx) > 0 && verbose) {
    message(sprintf("Skipping %d corrupted or non-docx file(s): %s",
                    length(invalid_docx), paste(basename(invalid_docx), collapse = ", ")))
  }

  if (length(valid_docx) == 0) {
    warning(
      sprintf(
        "No valid DOCX files found to process: %d file(s) could not be read (%s).",
        length(invalid_docx), paste(utils::head(basename(invalid_docx), 5), collapse = ", ")
      ),
      call. = FALSE
    )
    return(structure(
      list(
        comments = tibble::tibble(),
        revisions = tibble::tibble(),
        redlines = tibble::tibble(),
        documents = tibble::tibble(),
        docxwalk = tibble::tibble(),
        summary = tibble::tibble(),
        errors = invalid_errors,
        paths = list(master = master_tracker_path)
      ),
      class = "cbe_review_extract"
    ))
  }

  # From here on the run writes: make the output folder
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  master_tracker_path <- file.path(output_dir, tracker)

  # 3. Pipeline catalog and compute graph order
  if (is.null(manifest_path)) {
    manifest_path <- find_pipeline_manifest(scan_dir)
  }
  catalog <- load_pipeline_catalog(manifest_path)
  if (verbose) {
    if (!is.null(manifest_path) && file.exists(manifest_path)) {
      message(sprintf("Using pipeline manifest: %s", manifest_path))
    } else if (length(catalog) > 0) {
      message(sprintf("Using the pipeline catalog from review_config() (%d stages).", length(catalog)))
    } else {
      message("No pipeline catalog is configured: documents are not assigned to pipeline stages (see review_config(pipeline_catalog = )).")
    }
  }

  # Sort docx files by pipeline rank
  docx_ranks <- vapply(basename(valid_docx), function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  valid_docx <- valid_docx[order(docx_ranks, basename(valid_docx), method = "radix")]

  # 4. Load existing tracker data (if any)
  existing_data <- load_existing_tracker(master_tracker_path)
  existing_headers <- existing_data$headers
  existing_comments <- existing_data$comments
  existing_revisions <- existing_data$revisions
  existing_redlines <- existing_data$redlines
  existing_redline_headers <- existing_data$redline_headers
  existing_docs <- existing_data$documents

  unconfigured <- find_unconfigured_reviewers(
    existing_comments, existing_redlines, existing_docs,
    output_dir, tracker, c(fork_reviewers, signoff_reviewers)
  )
  if (length(unconfigured) > 0) {
    warning(
      sprintf(
        paste0(
          "The existing review tracker has entries from reviewer(s) that are not configured: %s. ",
          "Their fork workbooks are not merged, their document-level sign-offs are not ",
          "written to the Documents sheet, and their columns survive only as extra ",
          "Comments/SuggestedChanges columns. Add their ids with review_config() or the ",
          "fork_reviewers / signoff_reviewers arguments to keep tracking them."
        ),
        paste(unconfigured, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (verbose) {
    message(sprintf("Loaded %d existing comment(s) from tracker.", nrow(existing_comments)))
  }

  # 4b. Apply reviewer fork overlays
  fork_overlays <- apply_reviewer_fork_overlays(
    output_dir = output_dir,
    tracker_basename = tracker,
    existing_comments = existing_comments,
    existing_redlines = existing_redlines,
    fork_reviewers = fork_reviewers,
    verbose = verbose
  )
  existing_comments <- fork_overlays$comments
  existing_redlines <- fork_overlays$redlines

  # 5. Extract comments, revisions, and redlines from valid DOCX files
  incoming_comments_list <- list()
  incoming_revisions_list <- list()
  incoming_redlines_list <- list()
  errors_list <- list()
  unread_list <- list()

  for (doc in valid_docx) {
    doc_name <- basename(doc)
    res <- tryCatch({
      extract_from_docx(doc, min_revision_length = min_revision_length, docx_limits = docx_limits)
    }, error = function(e) {
      # The full message is for the console (verbose) only. What is stored
      # goes into $errors and the Errors sheet of the master and of every fork,
      # so absolute paths (the temp folder, the input folder, the home
      # folder with the account name) are replaced by placeholders.
      if (verbose) message(sprintf("Failed processing %s: %s", doc_name, conditionMessage(e)))
      errors_list[[length(errors_list) + 1]] <<- tibble::tibble(
        file = doc_name,
        error = scrub_local_paths(conditionMessage(e), dirname(doc)),
        traceback = scrub_local_paths(paste(conditionCall(e), collapse = "\n"), dirname(doc))
      )
      NULL
    })

    if (!is.null(res)) {
      if (nrow(res$comments) > 0) incoming_comments_list[[length(incoming_comments_list) + 1]] <- res$comments
      if (nrow(res$revisions) > 0) incoming_revisions_list[[length(incoming_revisions_list) + 1]] <- res$revisions
      if (nrow(res$redlines) > 0) incoming_redlines_list[[length(incoming_redlines_list) + 1]] <- res$redlines
      if (nrow(res$unread_parts) > 0) unread_list[[length(unread_list) + 1]] <- res$unread_parts

      if (verbose) {
        message(sprintf("Processed %s: %d comments, %d revisions, %d redlined paragraphs",
                        doc_name, nrow(res$comments), nrow(res$revisions), nrow(res$redlines)))
      }
    }
  }

  if (length(unread_list) > 0) {
    warning(format_unread_parts_warning(dplyr::bind_rows(unread_list)), call. = FALSE)
  }

  incoming_comments <- if (length(incoming_comments_list) > 0) dplyr::bind_rows(incoming_comments_list) else tibble::tibble()
  incoming_revisions <- if (length(incoming_revisions_list) > 0) dplyr::bind_rows(incoming_revisions_list) else tibble::tibble()
  incoming_redlines <- if (length(incoming_redlines_list) > 0) dplyr::bind_rows(incoming_redlines_list) else tibble::tibble()
  # Files that could not be read at all, and files that opened but failed to
  # extract, are errors of this run
  errors_df <- dplyr::bind_rows(invalid_errors, errors_list)
  if (nrow(errors_df) > 0) {
    errors_df <- errors_df[order(errors_df$file, method = "radix"), ]
    warning(
      sprintf(
        paste0(
          "%d Word file(s) could not be read and were left out of this run: %s. ",
          "Whatever the tracker already holds for them is kept; the reasons are in `$errors` ",
          "and on the Errors sheet."
        ),
        nrow(errors_df), paste(utils::head(errors_df$file, 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Only documents that extracted count as scanned: the rows the tracker holds
  # for a document that failed to extract are kept (as prior-round rows) and
  # not dropped because the document yielded nothing this time.
  scanned_files <- setdiff(unique(basename(valid_docx)), errors_df$file)
  processed_docs <- valid_docx[basename(valid_docx) %in% scanned_files]

  # 6. Non-destructively merge incoming with existing records
  merged_c <- merge_comments(
    incoming_comments = incoming_comments,
    existing_rows = existing_comments,
    existing_headers = existing_headers,
    catalog = catalog,
    scanned_files = scanned_files,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers
  )
  comments_columns <- merged_c$columns
  merged_comments <- merged_c$rows

  merged_revisions <- merge_revisions(
    incoming_revisions = incoming_revisions,
    existing_revisions = existing_revisions,
    catalog = catalog,
    scanned_files = scanned_files
  )

  merged_rl <- merge_redlines(
    incoming_redlines = incoming_redlines,
    existing_rows = existing_redlines,
    existing_headers = existing_redline_headers,
    catalog = catalog,
    scanned_files = scanned_files,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers
  )
  redline_columns <- merged_rl$columns
  merged_redlines <- merged_rl$rows

  # 7. Generate Excel Master Workbook (backed up first, now that it will be written).
  # Cell values, per-document figures, the crosswalk and the comment summary are the
  # same in every workbook of the run, so they are prepared once and the master and
  # the forks are written from them.
  backup_review_file(master_tracker_path)
  prepared <- prepare_review_workbook_data(
    columns = comments_columns,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    processed_docs = processed_docs,
    errors = errors_df,
    catalog = catalog,
    redline_columns = redline_columns,
    redlines_rows = merged_redlines,
    existing_docs = existing_docs,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers
  )

  write_review_tracker_excel(
    columns = comments_columns,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    processed_docs = processed_docs,
    errors = errors_df,
    output_path = master_tracker_path,
    catalog = catalog,
    redline_columns = redline_columns,
    redlines_rows = merged_redlines,
    existing_docs = existing_docs,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers,
    documents_sheet_mode = documents_sheet_mode,
    reviewer_view = NULL,
    prepared = prepared
  )

  # 7b. Generate Reviewer Fork Workbooks
  fork_paths <- character(0)
  tracker_base <- tools::file_path_sans_ext(tracker)
  tracker_ext <- tools::file_ext(tracker)
  if (nzchar(tracker_ext)) tracker_ext <- paste0(".", tracker_ext) else tracker_ext <- ".xlsx"

  for (rev in fork_reviewers) {
    fork_filename <- sprintf("%s_%s%s", tracker_base, rev, tracker_ext)
    fork_path <- file.path(output_dir, fork_filename)
    backup_review_file(fork_path)

    write_review_tracker_excel(
      columns = comments_columns,
      comments_rows = merged_comments,
      revisions_rows = merged_revisions,
      processed_docs = processed_docs,
      errors = errors_df,
      output_path = fork_path,
      catalog = catalog,
      redline_columns = redline_columns,
      redlines_rows = merged_redlines,
      existing_docs = existing_docs,
      fork_reviewers = fork_reviewers,
      signoff_reviewers = signoff_reviewers,
      documents_sheet_mode = documents_sheet_mode,
      reviewer_view = rev,
      prepared = prepared
    )
    fork_paths[[rev]] <- fork_path
  }

  # 8. Export CSVs
  csv_paths <- export_review_csvs(
    output_dir = output_dir,
    columns = comments_columns,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    redline_columns = redline_columns,
    redlines_rows = merged_redlines,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers,
    reviewer_columns = cfg$csv_reviewer_columns
  )

  # 9. Build return tables
  doc_summary <- build_documents_summary_df(
    processed_docs = processed_docs,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    redlines_rows = merged_redlines,
    catalog = catalog,
    existing_docs = existing_docs,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers
  )

  docxwalk_df <- build_docxwalk_df(
    files = sort(unique(c(basename(processed_docs), merged_comments$file)), method = "radix"),
    catalog = catalog,
    doc_paths = valid_docx
  )

  comment_summary_df <- build_comment_summary_df(merged_comments)

  out <- structure(
    list(
      comments = merged_comments,
      revisions = merged_revisions,
      redlines = merged_redlines,
      documents = doc_summary,
      docxwalk = docxwalk_df,
      summary = comment_summary_df,
      errors = errors_df,
      paths = c(
        list(
          master = master_tracker_path,
          forks = fork_paths
        ),
        csv_paths
      )
    ),
    class = "cbe_review_extract"
  )

  if (verbose) {
    print(out)
  }

  invisible(out)
}

#' @rdname cbe_docx_review_extract
#' @export
docx_review_extract <- cbe_docx_review_extract

#' @export
print.cbe_review_extract <- function(x, ...) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("REVIEW EXTRACTION & WORKFLOW TRACKER UPDATE COMPLETE\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf("Master Tracker:      %s\n", x$paths$master))
  if (length(x$paths$forks) > 0) {
    for (i in seq_along(x$paths$forks)) {
      cat(sprintf("Reviewer Fork (%s): %s\n", names(x$paths$forks)[i], x$paths$forks[[i]]))
    }
  }
  if (!is.null(x$paths$comments_csv)) {
    cat(sprintf("Comments CSV:        %s\n", x$paths$comments_csv))
  }
  if (!is.null(x$paths$tracked_changes_csv)) {
    cat(sprintf("Tracked Changes CSV: %s\n", x$paths$tracked_changes_csv))
  }
  if (!is.null(x$paths$suggested_changes_csv)) {
    cat(sprintf("Suggested Changes CSV: %s\n", x$paths$suggested_changes_csv))
  }

  n_comments <- nrow(x$comments)
  n_revisions <- nrow(x$revisions)
  n_redlines <- nrow(x$redlines)
  n_toc_lof <- if ("is_toc_or_lof" %in% names(x$redlines)) {
    sum(is_review_true(x$redlines$is_toc_or_lof), na.rm = TRUE)
  } else 0L

  cat(sprintf("\nTotal Comments in Tracker:  %s\n", format(n_comments, big.mark = ",")))
  cat(sprintf("Total Tracked Changes:      %s\n", format(n_revisions, big.mark = ",")))
  cat(sprintf("Total Suggested Changes:    %s\n", format(n_redlines, big.mark = ",")))
  cat(sprintf("  - Flagged TOC/LOF entries: %s\n", format(n_toc_lof, big.mark = ",")))

  if (nrow(x$errors) > 0) {
    cat(sprintf("Errors Encountered:         %s\n", format(nrow(x$errors), big.mark = ",")))
  }
  cat(strrep("=", 70), "\n\n", sep = "")
  invisible(x)
}

# =============================================================================
# REVIEWER CONFIGURATION
# =============================================================================

#' Get or set global multi-reviewer review-tracking configuration
#'
#' Decouples project-specific reviewer identities from the review-tracking
#' mechanics in \code{\link{cbe_docx_review_extract}}, mirroring how
#' \code{\link{pipeline_config}} decouples study name and stage labels from
#' the core compute-graph classes: the number and names of reviewers are
#' data supplied by the calling project, not hardcoded here.
#'
#' Two reviewer categories are supported:
#' \itemize{
#'   \item \code{fork_reviewers}: reviewers who sign off on individual
#'     comments and suggested changes. Each gets a personal fork workbook
#'     (\code{review_tracker_<id>.xlsx}) that holds only that reviewer's own
#'     columns: no other reviewer's columns, no sign-off reviewer's columns
#'     and no Word author names appear on any sheet, and the "Documents"
#'     sheet carries just the counts and that reviewer's own rolled-up
#'     column (there is no "Documents_<id>" sheet in a fork). In a fork the
#'     \code{resolved} columns reflect that reviewer's own sign-offs only; in
#'     the master workbook a document's \code{resolved} status is TRUE only
#'     when every fork reviewer has signed off on every comment and
#'     suggested change in that document.
#'   \item \code{signoff_reviewers}: reviewers who sign off once per
#'     document (e.g. a final PI read), tracked on the master workbook's
#'     "Documents" sheet (a status column plus a free-text comment column).
#'     They are not counted toward \code{resolved}, and they never appear
#'     in a fork workbook or, by default, in the CSV exports (see
#'     \code{csv_reviewer_columns}). The master's Comments and
#'     SuggestedChanges sheets also carry their column pair, so that
#'     anything typed there is kept.
#' }
#'
#' Existing \code{review_tracker_*.xlsx} files continue to work: column
#' data is preserved by header union regardless of configuration, but for
#' the app's reviewer-aware behavior
#' (rollups, resolved status, fork filtering) to line up with a file's
#' existing columns, keep configuring the same reviewer identifiers used
#' when that file was created.
#'
#' @param fork_reviewers Character vector of fork-reviewer identifiers. An id
#'   is used as a column name, as a fork-workbook file suffix and (with
#'   \code{documents_sheet_mode = "per_reviewer"}) in a sheet name, so it is
#'   validated: ids must be unique (ignoring case) and consist of 1-20 letters,
#'   digits or underscores; they must not be the name of a built-in tracker
#'   column (for example \code{file}, \code{author} or \code{resolved}) and
#'   must not end in \code{_comment}. A violation is an error, and nothing is
#'   changed.
#' @param signoff_reviewers Character vector of document-level sign-off
#'   reviewer identifiers (same rules as \code{fork_reviewers}; an id cannot
#'   be both a fork and a sign-off reviewer). Defaults to none.
#' @param documents_sheet_mode One of \code{"columns"} (default; a single
#'   "Documents" sheet with one column, or column pair, per reviewer) or
#'   \code{"per_reviewer"} (a shared "Documents" sheet with counts and
#'   \code{resolved} only, plus one additional "Documents_<id>" sheet per
#'   reviewer carrying just that reviewer's status/comment columns). This
#'   applies to the master workbook; a fork workbook always has one plain
#'   "Documents" sheet.
#' @param csv_reviewer_columns Which reviewer columns the CSV exports
#'   (\code{comments.csv}, \code{tracked_changes.csv},
#'   \code{suggested_changes.csv}) contain. \code{"fork"} (default) keeps the
#'   fork reviewers' sign-off and comment columns but leaves out the sign-off
#'   reviewers' columns and the Word \code{author} column; \code{"none"}
#'   leaves out every reviewer column and the author; \code{"all"} writes
#'   every column, including the sign-off reviewers' and the author, which is
#'   what the CSVs held before this option existed.
#' @param pipeline_catalog The analysis stages the reviewed documents belong to,
#'   used to label (\code{pipeline_stage}) and order documents. A list of stages,
#'   each a list with a \code{stem} (the document's file name without extension,
#'   date and reviewer initials, compared in lower case) and optionally
#'   \code{heading}, \code{name} and \code{stage} (the label shown; default
#'   \code{"Heading <heading>: <name>"}, else the name, else the stem); a data
#'   frame with those columns also works. The order of the list is the order of
#'   the documents. The package ships \emph{no} stages: unless a catalog (or a
#'   \code{reports_to_render.xlsx} manifest in \code{analysis_dir}) is supplied,
#'   every document gets the stage \code{"Unknown / Extra"}. A file whose stem
#'   is not in the catalog is matched to the most similar stem when they are at
#'   least 60 percent alike. Use \code{list()} to clear.
#' @param stem_aliases Named character vector of file-name stems (as typed, for
#'   example a recurring typo) and the catalog stems they stand for. Defaults to none.
#' @param analysis_dir Folder, relative to the working directory (or to the
#'   \code{TEMPLECBE_ANALYSIS_ROOT} environment variable) or absolute, that
#'   holds \code{reports_to_render.xlsx} and the \code{.qmd} sources shown on the
#'   docXwalk sheet. Defaults to none: no manifest or source index is looked up.
#'   Use \code{character(0)} to clear.
#' @param input_dirs Character vector of folders (relative to the working
#'   directory or \code{TEMPLECBE_ANALYSIS_ROOT}, or absolute) in which
#'   \code{\link{cbe_docx_review_extract}} looks, in order, for \code{.docx}
#'   files when \code{input_dir} is not given. Defaults to none, so the working
#'   directory is used. Use \code{character(0)} to clear.
#' @param reset If \code{TRUE}, forget everything set earlier (every argument
#'   above) and return to the defaults before applying any other argument given
#'   in the same call; the arguments are validated first, so a rejected call
#'   changes nothing. \code{review_config(reset = TRUE)} alone just restores the
#'   defaults. Defaults to \code{FALSE}.
#' @return A list containing the current configuration options.
#' @export
review_config <- function(
  fork_reviewers = NULL,
  signoff_reviewers = NULL,
  documents_sheet_mode = NULL,
  csv_reviewer_columns = NULL,
  pipeline_catalog = NULL,
  stem_aliases = NULL,
  analysis_dir = NULL,
  input_dirs = NULL,
  reset = FALSE
) {
  # Validate everything before setting anything, so a rejected call changes nothing
  ids <- validate_reviewer_ids(fork_reviewers, signoff_reviewers)
  if (!is.null(documents_sheet_mode)) {
    documents_sheet_mode <- match.arg(documents_sheet_mode, c("columns", "per_reviewer"))
  }
  if (!is.null(csv_reviewer_columns)) {
    csv_reviewer_columns <- match.arg(csv_reviewer_columns, c("fork", "all", "none"))
  }
  if (!is.null(pipeline_catalog)) pipeline_catalog <- validate_pipeline_catalog(pipeline_catalog)
  if (!is.null(stem_aliases)) stem_aliases <- validate_stem_aliases(stem_aliases)
  if (!is.null(analysis_dir)) analysis_dir <- validate_config_dirs(analysis_dir, "analysis_dir", max_length = 1L)
  if (!is.null(input_dirs)) input_dirs <- validate_config_dirs(input_dirs, "input_dirs")

  # reset: back to the defaults first, then whatever else was given in this call
  if (isTRUE(reset)) {
    options(
      review.fork_reviewers = NULL,
      review.signoff_reviewers = NULL,
      review.documents_sheet_mode = NULL,
      review.csv_reviewer_columns = NULL,
      review.pipeline_catalog = NULL,
      review.stem_aliases = NULL,
      review.analysis_dir = NULL,
      review.input_dirs = NULL
    )
  }

  if (!is.null(ids$fork_reviewers)) {
    options(review.fork_reviewers = ids$fork_reviewers)
  }
  if (!is.null(ids$signoff_reviewers)) {
    options(review.signoff_reviewers = ids$signoff_reviewers)
  }
  if (!is.null(documents_sheet_mode)) {
    options(review.documents_sheet_mode = documents_sheet_mode)
  }
  if (!is.null(csv_reviewer_columns)) {
    options(review.csv_reviewer_columns = csv_reviewer_columns)
  }
  if (!is.null(pipeline_catalog)) {
    options(review.pipeline_catalog = pipeline_catalog)
  }
  if (!is.null(stem_aliases)) {
    options(review.stem_aliases = stem_aliases)
  }
  if (!is.null(analysis_dir)) {
    options(review.analysis_dir = if (length(analysis_dir) == 0) NULL else analysis_dir)
  }
  if (!is.null(input_dirs)) {
    options(review.input_dirs = input_dirs)
  }

  list(
    fork_reviewers = getOption("review.fork_reviewers", c("reviewer_1", "reviewer_2", "reviewer_3")),
    signoff_reviewers = getOption("review.signoff_reviewers", character(0)),
    documents_sheet_mode = getOption("review.documents_sheet_mode", "columns"),
    csv_reviewer_columns = getOption("review.csv_reviewer_columns", "fork"),
    pipeline_catalog = getOption("review.pipeline_catalog", list()),
    stem_aliases = getOption("review.stem_aliases", character(0)),
    analysis_dir = getOption("review.analysis_dir", NULL),
    input_dirs = getOption("review.input_dirs", character(0))
  )
}

#' Validate and Normalise a Pipeline Catalog
#' @param x A list of stages (each a list with \code{stem} and optionally
#'   \code{heading}, \code{name}, \code{stage}), or a data frame with such columns.
#' @return A list of stages with a lowercase \code{stem}; \code{list()} for none.
#' @noRd
validate_pipeline_catalog <- function(x) {
  if (is.data.frame(x)) x <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  if (!is.list(x)) {
    stop("`pipeline_catalog` must be a list of stages, each a list with a `stem` (and optionally `heading`, `name`, `stage`).", call. = FALSE)
  }
  one_string <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(v))
  entries <- lapply(seq_along(x), function(i) {
    e <- x[[i]]
    if (!is.list(e) || !one_string(e$stem)) {
      stop(sprintf("`pipeline_catalog` entry %d needs a `stem`: one non-empty string (the document file name without date, initials and extension).", i), call. = FALSE)
    }
    for (field in c("name", "stage")) {
      v <- e[[field]]
      if (!is.null(v) && !is.na(v[1]) && !one_string(as.character(v))) {
        stop(sprintf("`pipeline_catalog` entry %d: `%s` must be a single string.", i, field), call. = FALSE)
      }
    }
    h <- e$heading
    list(
      stem = tolower(trimws(e$stem)),
      heading = if (is.null(h) || length(h) != 1L || is.na(h)) NULL else h,
      name = if (is.null(e$name) || is.na(e$name[1])) NULL else trimws(as.character(e$name)),
      stage = if (is.null(e$stage) || is.na(e$stage[1])) NULL else trimws(as.character(e$stage))
    )
  })
  stems <- vapply(entries, function(e) e$stem, character(1))
  if (anyDuplicated(stems) > 0) {
    stop(sprintf(
      "`pipeline_catalog` lists the same stem more than once (ignoring case): %s.",
      paste0("'", unique(stems[duplicated(stems)]), "'", collapse = ", ")
    ), call. = FALSE)
  }
  entries
}

#' Validate and Normalise Stem Aliases
#' @param x Named character vector (or list of strings): file-name stem as typed -> catalog stem.
#' @return Named character vector, names and values lowercase.
#' @noRd
validate_stem_aliases <- function(x) {
  if (is.list(x)) x <- unlist(x)
  if (length(x) == 0) return(character(0))
  if (!is.character(x) || anyNA(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!nzchar(x))) {
    stop("`stem_aliases` must be a named character vector: names are file-name stems as typed, values the catalog stems they stand for.", call. = FALSE)
  }
  out <- stats::setNames(tolower(trimws(x)), tolower(trimws(names(x))))
  if (anyDuplicated(names(out)) > 0) {
    stop("`stem_aliases` names the same stem more than once (ignoring case).", call. = FALSE)
  }
  out
}

#' Validate Configured Folder Names
#' @param x Character vector of folders (relative to the working directory, or absolute).
#' @param arg Argument name, for messages.
#' @param max_length Largest number of folders allowed.
#' @return \code{character(0)} for "none" (an empty vector or a single \code{""}), else \code{x}.
#' @noRd
validate_config_dirs <- function(x, arg, max_length = Inf) {
  if (length(x) == 1L && identical(x, "")) return(character(0))
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) || length(x) > max_length) {
    stop(sprintf(
      "`%s` must be %s (relative to the working directory, or absolute); use character(0) to clear it.",
      arg, if (is.finite(max_length) && max_length == 1L) "a single folder name" else "a character vector of folder names"
    ), call. = FALSE)
  }
  x
}

MAX_REVIEWER_ID_CHARS <- 20L

#' Names a reviewer id may not take: the tracker's built-in columns
#' @return Lowercase character vector.
#' @noRd
reserved_reviewer_ids <- function() {
  tolower(unique(c(
    METADATA_COLUMNS, SYSTEM_COLUMNS, REDLINE_METADATA_COLUMNS, REDLINE_SYSTEM_COLUMNS,
    # Documents-sheet and TrackedChanges-sheet headers, plus the shared workflow columns
    "pipeline_stage", "comment_count", "resolved_comment_count", "reply_count",
    "tracked_change_count", "revision_type", "changed_text",
    "resolved", "is_comment"
  )))
}

#' Validate Reviewer Identifiers
#'
#' A reviewer id is pasted into column names (\code{<id>}, \code{<id>_comment}),
#' into a fork workbook file name, into a "Documents_<id>" sheet name (31
#' characters at most in Excel) and into a regular expression, so it has to be
#' a short, plain token: unique (ignoring case, since sheet names and file names
#' are case-insensitive on common systems), made of letters, digits and
#' underscores only, at most \code{MAX_REVIEWER_ID_CHARS} characters, not the name of
#' a built-in column and not ending in \code{_comment}.
#' @param fork_reviewers,signoff_reviewers Character vectors of ids, or NULL to skip.
#' @return Invisibly, a list with the (character) \code{fork_reviewers} and
#'   \code{signoff_reviewers}; entries that were NULL stay NULL.
#' @noRd
validate_reviewer_ids <- function(fork_reviewers = NULL, signoff_reviewers = NULL) {
  sets <- list(fork_reviewers = fork_reviewers, signoff_reviewers = signoff_reviewers)
  reserved <- reserved_reviewer_ids()
  problems <- character(0)

  for (arg in names(sets)) {
    ids <- sets[[arg]]
    if (is.null(ids)) next
    if (is.factor(ids)) ids <- as.character(ids)
    if (!is.character(ids) || anyNA(ids)) {
      stop(sprintf("`%s` must be a character vector of reviewer ids without missing values.", arg), call. = FALSE)
    }
    sets[arg] <- list(ids)

    for (id in unique(ids)) {
      why <- character(0)
      if (!nzchar(id)) {
        why <- "is empty"
      } else {
        if (!grepl("^[A-Za-z0-9_]+$", id, perl = TRUE)) {
          why <- c(why, "may only contain letters, digits and underscores")
        }
        if (nchar(id) > MAX_REVIEWER_ID_CHARS) {
          why <- c(why, sprintf("is longer than %d characters", MAX_REVIEWER_ID_CHARS))
        }
        if (tolower(id) %in% reserved) {
          why <- c(why, "is the name of a built-in tracker column")
        }
        if (grepl("_comment$", id, ignore.case = TRUE)) {
          why <- c(why, "must not end in '_comment' (that suffix names a reviewer's comment column)")
        }
      }
      if (length(why) > 0) {
        problems <- c(problems, sprintf("%s: '%s' %s", arg, id, paste(why, collapse = "; ")))
      }
    }
    repeated <- unique(ids[duplicated(tolower(ids))])
    if (length(repeated) > 0) {
      problems <- c(problems, sprintf(
        "%s: %s listed more than once (ids are compared ignoring case)",
        arg, paste0("'", repeated, "'", collapse = ", ")
      ))
    }
  }

  shared <- intersect(tolower(sets$fork_reviewers), tolower(sets$signoff_reviewers))
  if (length(shared) > 0) {
    problems <- c(problems, sprintf(
      "%s used as both a fork and a sign-off reviewer",
      paste0("'", shared, "'", collapse = ", ")
    ))
  }

  if (length(problems) > 0) {
    stop(
      "Invalid reviewer id(s). Ids become column names, fork workbook file names and sheet names, ",
      "so each must be unique, 1-", MAX_REVIEWER_ID_CHARS, " characters of letters, digits and ",
      "underscores, not a built-in column name and not end in '_comment':\n",
      paste0("  - ", problems, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(sets)
}

# =============================================================================
# CONSTANTS & METADATA
# =============================================================================

W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
MC_NS <- "http://schemas.openxmlformats.org/markup-compatibility/2006"
W15_NS <- "http://schemas.microsoft.com/office/word/2012/wordml"
XML_NAMESPACES <- c(w = W_NS, mc = MC_NS)
COMMENTS_EXT_NAMESPACES <- c(w15 = W15_NS)

METADATA_COLUMNS <- c(
  "file",
  "comment_id",
  "author",
  "date",
  "comment_text",
  "selected_text",
  "paragraph_number",
  "end_paragraph_number",
  "paragraph_context"
)

#' Build the reviewer sign-off columns (value + comment pairs) for a reviewer set
#' @param fork_reviewers Character vector of fork-reviewer identifiers.
#' @param signoff_reviewers Character vector of sign-off reviewer identifiers.
#' @return Character vector: "resolved" followed by "<id>"/"<id>_comment" pairs.
#' @noRd
workflow_columns <- function(fork_reviewers = NULL, signoff_reviewers = NULL) {
  if (is.null(fork_reviewers)) fork_reviewers <- review_config()$fork_reviewers
  if (is.null(signoff_reviewers)) signoff_reviewers <- review_config()$signoff_reviewers
  c(
    "resolved",
    unlist(lapply(fork_reviewers, function(r) c(r, paste0(r, "_comment")))),
    unlist(lapply(signoff_reviewers, function(r) c(r, paste0(r, "_comment"))))
  )
}

#' Build the full canonical Comments-sheet column set for a reviewer set
#' @inheritParams workflow_columns
#' @return Character vector of column names.
#' @noRd
canonical_columns <- function(fork_reviewers = NULL, signoff_reviewers = NULL) {
  c(METADATA_COLUMNS, workflow_columns(fork_reviewers, signoff_reviewers), SYSTEM_COLUMNS)
}

SYSTEM_COLUMNS <- c(
  "doc_status",
  "pipeline_stage",
  "is_reply",
  "reply_to_id",
  "resolved_in_docx",
  "duplicate_count"
)

REDLINE_METADATA_COLUMNS <- c(
  "file",
  "paragraph_number",
  "author",
  "date",
  "original_text",
  "accepted_text",
  "is_toc_or_lof"
)

#' Build the redline workflow columns for a reviewer set
#' @inheritParams workflow_columns
#' @return Character vector: "is_comment" followed by \code{workflow_columns()}.
#' @noRd
redline_workflow_columns <- function(fork_reviewers = NULL, signoff_reviewers = NULL) {
  c("is_comment", workflow_columns(fork_reviewers, signoff_reviewers))
}

#' Build the full canonical SuggestedChanges-sheet column set for a reviewer set
#' @inheritParams workflow_columns
#' @return Character vector of column names.
#' @noRd
redline_canonical_columns <- function(fork_reviewers = NULL, signoff_reviewers = NULL) {
  c(REDLINE_METADATA_COLUMNS, redline_workflow_columns(fork_reviewers, signoff_reviewers), REDLINE_SYSTEM_COLUMNS)
}

REDLINE_SYSTEM_COLUMNS <- c("doc_status", "pipeline_stage")

COMMENTS_DEFAULT_HIDDEN_COLUMNS <- c(
  "comment_id", "author", "date", "selected_text", "paragraph_number",
  "end_paragraph_number", "paragraph_context",
  "doc_status", "pipeline_stage", "is_reply", "reply_to_id", "resolved_in_docx", "duplicate_count"
)

REDLINE_DEFAULT_HIDDEN_COLUMNS <- c(
  "paragraph_number", "author", "date", "doc_status", "pipeline_stage"
)

COLOR_METADATA <- "#1F4E79"   # Navy
COLOR_WORKFLOW <- "#2E75B6"   # Steel Blue
COLOR_RESOLVED <- "#006666"   # Dark Teal
COLOR_SYSTEM   <- "#595959"   # Slate Gray
COLOR_TRACKED  <- "#385723"   # Forest Green
COLOR_REDLINE  <- "#833C0C"   # Burnt Orange
COLOR_DOCS     <- "#C65911"   # Rust Orange
COLOR_SUMMARY  <- "#7030A0"   # Purple
COLOR_ERRORS   <- "#C00000"   # Red

# =============================================================================
# TEXT NORMALIZATION & PIPELINE HELPERS
# =============================================================================

#' Clean and Normalize Text
#' @param text Input text string or vector
#' @return Normalized character string or vector
#' @noRd
clean_review_text <- function(text) {
  if (is.null(text) || length(text) == 0) return("")
  vapply(text, function(t) {
    if (is.na(t)) return("")
    t <- as.character(t)

    # Mojibake multi-byte sequences first
    t <- gsub("\u00e2\u20ac\u00a6", "...", t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u2013", "-", t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u2014", "-", t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u02dc", "'", t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u2122", "'", t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u0153", '"', t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac\u009d", '"', t, fixed = TRUE)
    t <- gsub("\u00e2\u20ac", '"', t, fixed = TRUE)

    # Unicode smart quotes and dashes
    t <- gsub("\u00a0", " ", t, fixed = TRUE)       # Non-breaking space
    t <- gsub("\u2013", "-", t, fixed = TRUE)       # En-dash
    t <- gsub("\u2014", "-", t, fixed = TRUE)       # Em-dash
    t <- gsub("\u2018", "'", t, fixed = TRUE)       # Left single quote
    t <- gsub("\u2019", "'", t, fixed = TRUE)       # Right single quote
    t <- gsub("\u201c", '"', t, fixed = TRUE)       # Left double quote
    t <- gsub("\u201d", '"', t, fixed = TRUE)       # Right double quote
    t <- gsub("\u2026", "...", t, fixed = TRUE)     # Ellipsis
    t <- gsub("\ufffd", "'", t, fixed = TRUE)       # Replacement char

    # Collapse whitespace
    t <- gsub("\\s+", " ", t)
    trimws(t)
  }, character(1), USE.NAMES = FALSE)
}

#' Extract Clean Stem from DOCX Filename
#' @param filename DOCX filename or path
#' @param reviewer_ids Character vector of configured reviewer identifiers to
#'   also recognize as a trailing filename suffix (e.g. a fork whose id is
#'   longer than the generic 2-4 upper / 2-3 lower initials pattern already
#'   covers). Defaults to the currently configured \code{\link{review_config}}
#'   fork and sign-off reviewers.
#' @param known_stems Optional character vector of stems that exist (the pipeline
#'   catalog's, and the known typo aliases). A trailing \code{_xx} / \code{_xxx}
#'   token is only reviewer initials when the name without it is not already one
#'   of these: a stem that legitimately ends in two or three lowercase letters is
#'   returned whole.
#' @return Clean lowercased stem string
#' @noRd
extract_docx_stem <- function(filename, reviewer_ids = NULL, known_stems = NULL) {
  if (is.null(reviewer_ids)) {
    cfg <- review_config()
    reviewer_ids <- c(cfg$fork_reviewers, cfg$signoff_reviewers)
  }
  stem <- tools::file_path_sans_ext(basename(filename))
  stem <- sub("_?\\d{1,2}_\\d{1,2}_\\d{2,4}$", "", stem)
  if (length(known_stems) > 0) {
    whole <- tolower(sub("_+$", "", stem))
    if (whole %in% known_stems) return(whole)
  }
  # Ids are literal text, never regular expressions ("c++" must not break the
  # pattern and "j.doe" must not also match "jXdoe")
  reviewer_alt <- if (length(reviewer_ids) > 0) paste0("|", paste(regex_escape(reviewer_ids), collapse = "|")) else ""
  stem <- sub(paste0("_([A-Z]{2,4}|[a-z]{2,3}", reviewer_alt, ")$"), "", stem)
  stem <- sub("_+$", "", stem)
  tolower(stem)
}

#' Escape Regular-Expression Metacharacters
#' @param x Character vector of literal text
#' @return \code{x} with every metacharacter backslash-escaped, so that it
#'   matches itself inside a (TRE or PCRE) regular expression.
#' @noRd
regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

#' Replace Local Folder Paths in a Message with Placeholders
#'
#' Error text from \code{unzip()} and \code{xml2} quotes the absolute paths it
#' worked on: the temporary extraction folder, the input folder and, through a
#' hostile archive's entry names, whatever else. The stored errors end up in
#' every workbook sent to reviewers, so the temporary folder becomes
#' \code{<tempdir>}, the input folder \code{<input_dir>} and the home folder
#' (\code{path.expand("~")}, \code{HOME}, \code{USERPROFILE}; it carries the
#' account name) \code{<home>}, written with either slash style and matched
#' ignoring case.
#' @param msg Character vector of messages.
#' @param input_dir Optional folder the documents were read from.
#' @return \code{msg} with the folders replaced.
#' @noRd
scrub_local_paths <- function(msg, input_dir = NULL) {
  dirs <- c("<tempdir>" = tempdir(),
            "<home>" = path.expand("~"),
            "<home>" = Sys.getenv("USERPROFILE"),
            "<home>" = Sys.getenv("HOME"))
  if (length(input_dir) > 0) {
    dirs <- c(stats::setNames(input_dir, rep("<input_dir>", length(input_dir))), dirs)
  }
  dirs <- dirs[!is.na(dirs) & nzchar(dirs)]

  # One regular expression per folder: its components joined by "either slash"
  # (so "C:\Users/me\x" matches however the message spells it), for the folder as
  # given and as normalised (long names instead of 8.3 short names)
  rows <- lapply(seq_along(dirs), function(i) {
    d <- gsub("\\", "/", dirs[[i]], fixed = TRUE)
    spellings <- unique(sub("/+$", "", c(
      d, tryCatch(normalizePath(d, winslash = "/", mustWork = FALSE), error = function(e) d)
    )))
    # A file-system root ("/", "C:/") is not a folder worth hiding
    spellings <- spellings[!grepl("^([A-Za-z]:)?/*$", spellings)]
    patterns <- vapply(spellings, function(s) {
      paste(vapply(strsplit(s, "/", fixed = TRUE)[[1]], regex_escape, character(1)), collapse = "[/\\\\]")
    }, character(1))
    data.frame(placeholder = rep(names(dirs)[i], length(patterns)), pattern = unname(patterns),
               size = nchar(spellings), stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || nrow(rows) == 0) return(msg)

  # Longest folder first: the temp folder usually sits inside the home folder
  rows <- rows[order(-rows$size), , drop = FALSE]
  for (i in seq_len(nrow(rows))) {
    msg <- gsub(rows$pattern[i], rows$placeholder[i], msg, ignore.case = TRUE)
  }
  msg
}

#' Check if Value Represents Boolean TRUE
#' @param val Any value
#' @return Logical TRUE/FALSE
#' @noRd
is_review_true <- function(val) {
  if (is.null(val) || length(val) == 0) return(FALSE)
  if (is.logical(val)) return(isTRUE(val))
  toupper(trimws(as.character(val))) == "TRUE"
}

#' Vectorised \code{is_review_true()}
#'
#' \code{is_review_true()} answers for one value (a logical vector longer than
#' one is FALSE), so it cannot filter a column. This returns one answer per
#' element, with a missing value counting as FALSE.
#' @param val Logical or character vector
#' @return Logical vector, same length as \code{val}
#' @noRd
is_review_true_vec <- function(val) {
  if (is.null(val) || length(val) == 0) return(logical(0))
  out <- if (is.logical(val)) val else toupper(trimws(as.character(val))) == "TRUE"
  out[is.na(out)] <- FALSE
  out
}

#' Is a Folder Name Absolute?
#' @param path Character vector of folder names.
#' @return Logical vector: a drive letter or root slash, a UNC share, or a home (\code{~}) prefix.
#' @noRd
is_absolute_dir <- function(path) grepl("^([A-Za-z]:)?[/\\\\]|^~", path)

#' Base Folders a Configured Relative Folder Is Looked Up Under
#'
#' The working directory, then the \code{TEMPLECBE_ANALYSIS_ROOT} environment
#' variable when it is set.
#' @return Character vector of folders.
#' @noRd
review_dir_bases <- function() {
  env_root <- Sys.getenv("TEMPLECBE_ANALYSIS_ROOT", unset = NA)
  c(getwd(), if (!is.na(env_root) && nzchar(env_root)) env_root)
}

#' Find Pipeline Manifest File
#'
#' Looks for \code{reports_to_render.xlsx} in the configured
#' \code{review_config()$analysis_dir}. Nothing is searched when no folder is
#' configured.
#' @param start_dir Search directory; the folder itself and its two parents are tried as bases first
#' @param analysis_dir Folder that holds the manifest, relative to a base folder or absolute
#' @return File path or NULL
#' @noRd
find_pipeline_manifest <- function(start_dir = NULL, analysis_dir = review_config()$analysis_dir) {
  if (length(analysis_dir) == 0 || !nzchar(analysis_dir)) return(NULL)
  bases <- character(0)
  if (!is.null(start_dir) && nzchar(start_dir)) {
    bases <- c(start_dir, dirname(start_dir), dirname(dirname(start_dir)))
  }
  bases <- c(bases, review_dir_bases())
  candidates <- if (is_absolute_dir(analysis_dir)) analysis_dir else file.path(bases, analysis_dir)
  for (cand in file.path(candidates, "reports_to_render.xlsx")) {
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
  }
  NULL
}

#' Load Pipeline Compute Graph Catalog
#'
#' The stages come from a \code{reports_to_render.xlsx} manifest when one is
#' found, then from \code{review_config()$pipeline_catalog}. The package ships no
#' stages of its own: with neither, the catalog is empty and every document is
#' labelled "Unknown / Extra".
#' @param manifest_path Optional path to manifest file
#' @param stages Stages to add after the manifest's; defaults to \code{review_config()$pipeline_catalog}
#' @return Named list of stages
#' @noRd
load_pipeline_catalog <- function(manifest_path = NULL, stages = review_config()$pipeline_catalog) {
  catalog <- list()
  rank <- 0L

  if (!is.null(manifest_path) && file.exists(manifest_path)) {
    tryCatch({
      df <- readxl::read_excel(manifest_path)
      if ("file" %in% names(df)) {
        for (i in seq_len(nrow(df))) {
          f_path <- as.character(df$file[i])
          if (!is.na(f_path) && grepl("\\.qmd$", f_path, ignore.case = TRUE)) {
            stem <- tolower(tools::file_path_sans_ext(basename(f_path)))
            h <- if ("Heading" %in% names(df)) df$Heading[i] else NA
            name <- if ("name" %in% names(df) && !is.na(df$name[i])) as.character(df$name[i]) else stem
            name <- trimws(name)
            stage_label <- if (!is.na(h) && nzchar(as.character(h)) && as.character(h) != "NA") {
              sprintf("Heading %s: %s", h, name)
            } else {
              name
            }
            catalog[[stem]] <- list(
              rank = rank,
              heading = h,
              name = name,
              stage = stage_label
            )
            rank <- rank + 1L
          }
        }
      }
    }, error = function(e) {
      warning(sprintf("Could not read pipeline manifest %s: %s", manifest_path, conditionMessage(e)))
    })
  }

  # Augment with the stages configured through review_config()
  for (item in stages) {
    stem <- tolower(item$stem)
    if (!stem %in% names(catalog)) {
      h <- item$heading
      name <- if (!is.null(item$name)) item$name else stem
      stage_label <- if (!is.null(item$stage)) item$stage else if (!is.null(h)) sprintf("Heading %s: %s", h, name) else name
      catalog[[stem]] <- list(
        rank = rank,
        heading = h,
        name = name,
        stage = stage_label
      )
      rank <- rank + 1L
    }
  }

  catalog
}

#' Match DOCX Filename to Pipeline Stage
#' @param filename DOCX filename
#' @param catalog Pipeline catalog list
#' @param aliases Named character vector of file-name stems and the catalog stems
#'   they stand for (typos, old names); defaults to \code{review_config()$stem_aliases}
#' @return List with rank, matched_stem, and stage
#' @noRd
match_docx_to_pipeline <- function(filename, catalog, aliases = review_config()$stem_aliases) {
  raw_stem <- extract_docx_stem(filename, known_stems = c(names(catalog), names(aliases)))
  stem <- if (raw_stem %in% names(aliases)) aliases[[raw_stem]] else raw_stem

  if (stem %in% names(catalog)) {
    entry <- catalog[[stem]]
    return(list(rank = entry$rank, matched_stem = stem, stage = entry$stage))
  }

  # Fuzzy match fallback using edit distance
  cat_stems <- names(catalog)
  if (length(cat_stems) > 0) {
    dists <- utils::adist(stem, cat_stems, ignore.case = TRUE)[1, ]
    min_idx <- which.min(dists)
    max_len <- max(nchar(stem), nchar(cat_stems[min_idx]))
    similarity <- if (max_len > 0) 1 - (dists[min_idx] / max_len) else 0
    if (similarity >= 0.6) {
      best_stem <- cat_stems[min_idx]
      entry <- catalog[[best_stem]]
      return(list(rank = entry$rank, matched_stem = best_stem, stage = entry$stage))
    }
  }

  list(rank = 999L, matched_stem = raw_stem, stage = "Unknown / Extra")
}

#' Build QMD Source File Index
#' @param search_root Directory to search for .qmd files; defaults to the
#'   configured \code{review_config()$analysis_dir} under the working directory
#'   (no index when none is configured)
#' @return Named character vector mapping lowercased stems to file paths
#' @noRd
build_qmd_index <- function(search_root = NULL) {
  index <- character(0)
  if (is.null(search_root)) {
    analysis_dir <- review_config()$analysis_dir
    if (length(analysis_dir) == 0 || !nzchar(analysis_dir)) return(index)
    search_root <- if (is_absolute_dir(analysis_dir)) analysis_dir else file.path(getwd(), analysis_dir)
  }
  if (!dir.exists(search_root)) return(index)

  qmds <- list.files(search_root, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  # Exclude archive, _freeze, and cache
  excl_pattern <- "(/|\\\\)(archive|_freeze|.*_cache)(/|\\\\)"
  qmds <- qmds[!grepl(excl_pattern, qmds, ignore.case = TRUE)]

  for (q in qmds) {
    stem <- tolower(tools::file_path_sans_ext(basename(q)))
    index[[stem]] <- q
  }
  index
}

#' Format Path Relative to Repository
#'
#' A path inside the project root is shown as \code{<root folder>\\<relative path>}
#' (backslashes). A path that is not inside the root is reduced to its file
#' name: an absolute path would carry the user profile (the account name) into
#' every workbook. "Inside" is decided on whole path components, so a sibling
#' folder such as \code{proj_old} is not inside \code{proj}.
#' @param path File path
#' @param root Project root; defaults to \code{here::here()} (else the working directory)
#' @return Formatted character path with backslashes, or the file name
#' @noRd
format_repo_path <- function(path, root = NULL) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (is.null(root)) root <- tryCatch(here::here(), error = function(e) getwd())
  as_slashes <- function(p) {
    sub("/+$", "", gsub("\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE), fixed = TRUE))
  }
  norm_path <- as_slashes(path)
  norm_root <- as_slashes(root)

  fold <- if (.Platform$OS.type == "windows") tolower else identity
  if (startsWith(fold(norm_path), fold(paste0(norm_root, "/")))) {
    rel <- substring(norm_path, nchar(norm_root) + 2L)
    paste0(basename(norm_root), "\\", gsub("/", "\\\\", rel))
  } else {
    basename(norm_path)
  }
}

#' Crosswalk DOCX to Pipeline Source & Rendered Outputs
#' @param files Vector of DOCX filenames
#' @param catalog Pipeline catalog
#' @param qmd_index Optional QMD index
#' @param doc_paths Paths of the DOCX files that were read (any folder); the
#'   \code{file} column is built from the folder a document really came from. A
#'   file with no path here (a prior-round document no longer in the folder) is
#'   listed by name.
#' @param basename_only If TRUE (fork workbooks), every path column holds just a
#'   file name, so nothing of the project's folder layout leaves the master.
#' @param repo_root Project root used to make paths relative; defaults to
#'   \code{here::here()}.
#' @return Tibble of crosswalk rows
#' @noRd
build_docxwalk_df <- function(files, catalog, qmd_index = NULL, doc_paths = NULL,
                              basename_only = FALSE, repo_root = NULL) {
  if (is.null(qmd_index)) {
    qmd_index <- build_qmd_index()
  }
  doc_dirs <- if (length(doc_paths) > 0) stats::setNames(dirname(doc_paths), basename(doc_paths)) else character(0)
  show_path <- function(p) if (basename_only) basename(p) else format_repo_path(p, root = repo_root)

  rows <- lapply(files, function(fname) {
    matched <- match_docx_to_pipeline(fname, catalog)
    matched_stem <- matched$matched_stem
    qmd_path <- if (!is.null(matched_stem) && matched_stem %in% names(qmd_index)) qmd_index[[matched_stem]] else NULL

    source_qmd <- if (!is.null(qmd_path)) show_path(qmd_path) else NA_character_
    rendered_pdf <- NA_character_
    rendered_docx <- NA_character_

    if (!is.null(qmd_path) && file.exists(qmd_path)) {
      pdf_cand <- sub("\\.qmd$", ".pdf", qmd_path, ignore.case = TRUE)
      if (file.exists(pdf_cand)) rendered_pdf <- show_path(pdf_cand)
      docx_cand <- sub("\\.qmd$", ".docx", qmd_path, ignore.case = TRUE)
      if (file.exists(docx_cand)) rendered_docx <- show_path(docx_cand)
    }

    tibble::tibble(
      file = if (fname %in% names(doc_dirs)) show_path(file.path(doc_dirs[[fname]], fname)) else fname,
      source_qmd = source_qmd,
      rendered_pdf = rendered_pdf,
      rendered_docx = rendered_docx
    )
  })

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(file = character(0), source_qmd = character(0), rendered_pdf = character(0), rendered_docx = character(0))
}

#' Find Default Review Directory
#'
#' The first of the configured \code{review_config()$input_dirs} (looked up under
#' the working directory, then \code{TEMPLECBE_ANALYSIS_ROOT}) that holds
#' \code{.docx} files, else the working directory itself. No folder name is
#' built into the package.
#' @param input_dirs Candidate folders, relative to a base folder or absolute
#' @return Path to input directory
#' @noRd
find_default_review_input_dir <- function(input_dirs = review_config()$input_dirs) {
  candidates <- if (length(input_dirs) == 0) {
    character(0)
  } else if (all(is_absolute_dir(input_dirs))) {
    input_dirs
  } else {
    # relative folders: every one under the working directory first, then under the environment root
    unlist(lapply(review_dir_bases(), function(base) {
      ifelse(is_absolute_dir(input_dirs), input_dirs, file.path(base, input_dirs))
    }))
  }
  candidates <- unique(c(candidates, getwd()))
  for (cand in candidates) {
    if (dir.exists(cand) && length(list.files(cand, pattern = "\\.docx$", ignore.case = TRUE)) > 0) {
      return(cand)
    }
  }
  getwd()
}

#' Backup File Before Modification
#'
#' The backup name carries the time to the millisecond, and a counter when that
#' name is taken, and an existing backup is never overwritten: two runs in the
#' same second used to share a name, so the second erased the first backup and
#' the state before the first run was lost.
#' @param filepath Path to file
#' @return Backup path or NULL
#' @noRd
backup_review_file <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  bdir <- file.path(dirname(filepath), "backups")
  if (!dir.exists(bdir)) dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
  now <- Sys.time()
  ts <- sprintf("%s_%03d", format(now, "%Y%m%d_%H%M%S"), as.integer((as.numeric(now) %% 1) * 1000))
  stem <- tools::file_path_sans_ext(basename(filepath))
  ext <- tools::file_ext(filepath)
  if (nzchar(ext)) ext <- paste0(".", ext)
  bak_path <- file.path(bdir, sprintf("%s_%s%s.bak", stem, ts, ext))
  counter <- 1L
  while (file.exists(bak_path)) {
    counter <- counter + 1L
    bak_path <- file.path(bdir, sprintf("%s_%s_%d%s.bak", stem, ts, counter, ext))
  }
  file.copy(filepath, bak_path, overwrite = FALSE)
  bak_path
}

# =============================================================================
# DOCX XML EXTRACTION
# =============================================================================

#' Say Why a File Is Not a Readable Word Document
#'
#' A readable document is a ZIP archive with an Office Open XML content-types
#' part and the main document part, \code{word/document.xml}, which is the only
#' part the extractor needs (a renamed workbook has the first but not the second).
#' The reason never names a path.
#' @param filepath Path to DOCX file
#' @return \code{NA_character_} for a readable document, else a short reason.
#' @noRd
docx_problem <- function(filepath) {
  if (!file.exists(filepath)) return("File not found")
  size <- file.size(filepath)
  if (is.na(size) || size == 0) return("Empty file")
  files <- tryCatch(
    suppressWarnings(utils::unzip(filepath, list = TRUE)$Name),
    error = function(e) NULL
  )
  if (is.null(files)) return("Not a readable ZIP archive (corrupt, or not a Word document)")
  if (!"[Content_Types].xml" %in% files) return("Not an Office Open XML package ([Content_Types].xml is missing)")
  if (!"word/document.xml" %in% files) return("Not a Word document (word/document.xml is missing)")
  NA_character_
}

#' Validate DOCX ZIP Archive
#' @param filepath Path to DOCX file
#' @return Logical TRUE/FALSE
#' @noRd
is_valid_docx <- function(filepath) {
  is.na(docx_problem(filepath))
}

# The elements of a Word paragraph that carry text, in document order: text and
# deleted text, plus the run content that is only whitespace or a hyphen (a tab
# or line break is not stored as text). `w:tab` also appears as a tab stop
# inside paragraph properties, which is why only the one in a run counts.
#
# A drawing stored as mc:AlternateContent is written twice: an mc:Choice for
# current readers and an mc:Fallback copy for older ones. Everything that reads
# the document reads the Choice only, so each leaf is restricted to content that
# is not under an mc:Fallback; a text box's paragraphs (w:txbxContent) are part
# of the paragraph that holds the box, as Word numbers them.
NOT_IN_FALLBACK <- "not(ancestor::mc:Fallback)"
TEXT_LEAF_XPATH <- paste(
  sprintf(".//%s[%s]", c("w:t", "w:delText", "w:r/w:tab", "w:r/w:ptab", "w:r/w:br", "w:r/w:cr", "w:r/w:noBreakHyphen"), NOT_IN_FALLBACK),
  collapse = " | "
)

#' Text of a Run-Content Element
#'
#' Text and deleted text give their text; a tab, positional tab, line or page
#' break and carriage return give a space, so the words around them do not run
#' together; a non-breaking hyphen gives "-" (it is a hyphen, not a space, so
#' "e-mail" typed with one stays one word).
#' @param name Local element name (\code{xml_name()}).
#' @param text The element's text (\code{xml_text()}), used for t and delText.
#' @return Character string.
#' @noRd
leaf_element_text <- function(name, text) {
  if (name %in% c("t", "delText")) return(text)
  if (identical(name, "noBreakHyphen")) return("-")
  " "
}

#' Text of the Run Content under a Node
#' @param node xml2 node (a paragraph, a revision, a comment paragraph).
#' @return The text, not yet cleaned; "" when there is none.
#' @noRd
node_text <- function(node) {
  leaves <- xml2::xml_find_all(node, TEXT_LEAF_XPATH, XML_NAMESPACES)
  if (length(leaves) == 0) return("")
  paste(
    vapply(leaves, function(el) leaf_element_text(xml2::xml_name(el), xml2::xml_text(el)), character(1)),
    collapse = ""
  )
}

#' Parse One XML Part of a DOCX, Naming the Part in the Error
#'
#' A part that cannot be parsed is an error, not an empty result: the caller
#' (\code{cbe_docx_review_extract()}) lists the file in \code{$errors} instead
#' of writing rows with a missing author and empty text.
#' @param path Path of the extracted part on disk
#' @param part Name of the part inside the archive, e.g. \code{"word/comments.xml"}
#' @return An xml2 document
#' @noRd
read_docx_xml <- function(path, part) {
  tryCatch(
    xml2::read_xml(path),
    error = function(e) {
      stop(sprintf("%s could not be parsed: %s", part, conditionMessage(e)), call. = FALSE)
    }
  )
}

#' Read Thread Links and Resolved Flags from word/commentsExtended.xml
#'
#' Word does not nest a reply inside its parent \code{w:comment}: replies are
#' sibling comments, and the thread structure lives in this part as one
#' \code{w15:commentEx} per comment, keyed by the \code{w14:paraId} of the
#' comment's LAST paragraph (\code{paraId}), with the parent's last-paragraph id
#' (\code{paraIdParent}) and the resolved flag (\code{done}).
#' @param path Path of the extracted part, or \code{NULL} / a missing file
#' @return Named list keyed by \code{paraId}; each element holds \code{parent}
#'   (the parent's paraId or \code{NA}) and \code{done} (logical). Empty when the
#'   part is absent.
#' @noRd
read_comments_extended <- function(path) {
  out <- list()
  if (is.null(path) || !file.exists(path)) return(out)
  root <- read_docx_xml(path, "word/commentsExtended.xml")
  for (node in xml2::xml_find_all(root, "//w15:commentEx", COMMENTS_EXT_NAMESPACES)) {
    pid <- xml2::xml_attr(node, "paraId")
    if (is.na(pid) || !nzchar(pid)) next
    out[[pid]] <- list(
      parent = xml2::xml_attr(node, "paraIdParent"),
      done = tolower(xml2::xml_attr(node, "done")) %in% c("1", "true")
    )
  }
  out
}

#' Read Comment Metadata from word/comments.xml
#'
#' Replies and resolved comments are read from two layouts. Word's own is flat
#' (every \code{w:comment} a sibling; the parent link and the \code{done} flag in
#' \code{word/commentsExtended.xml}). A reply nested inside its parent
#' \code{w:comment}, and a \code{w:done} attribute on the \code{w:comment}, are
#' also understood. A comment is resolved when its own flag is set or when any
#' comment above it in its thread is, because Word resolves a thread as a unit.
#' @param comments_xml_path Path to comments.xml
#' @param comments_extended_path Optional path to commentsExtended.xml
#' @return Named list of comment metadata
#' @noRd
read_comments_metadata <- function(comments_xml_path, comments_extended_path = NULL) {
  meta <- list()
  if (!file.exists(comments_xml_path)) return(meta)

  root <- read_docx_xml(comments_xml_path, "word/comments.xml")
  extended <- read_comments_extended(comments_extended_path)

  comment_nodes <- xml2::xml_find_all(root, "//w:comment", XML_NAMESPACES)
  for (c_node in comment_nodes) {
    cid <- xml2::xml_attr(c_node, "id")
    if (is.na(cid) || !nzchar(cid)) next

    parent <- xml2::xml_parent(c_node)
    p_name <- xml2::xml_name(parent)
    is_reply <- FALSE
    reply_to_id <- NA_character_
    if (!is.na(p_name) && p_name == "comment") {
      reply_to_id <- xml2::xml_attr(parent, "id")
      is_reply <- !is.na(reply_to_id)
    }

    # The paragraphs of a multi-paragraph comment are separated by a space
    body_paragraphs <- xml2::xml_find_all(c_node, "./w:p", XML_NAMESPACES)
    raw_text <- paste(vapply(body_paragraphs, node_text, character(1)), collapse = " ")

    # commentsExtended.xml refers to a comment by the paraId of its last paragraph
    last_para <- xml2::xml_find_first(c_node, "./w:p[last()]", XML_NAMESPACES)
    para_id <- xml2::xml_attr(last_para, "paraId")

    meta[[cid]] <- list(
      comment_id = cid,
      author = xml2::xml_attr(c_node, "author"),
      date = xml2::xml_attr(c_node, "date"),
      comment_text = clean_review_text(raw_text),
      resolved_in_docx = identical(xml2::xml_attr(c_node, "done"), "1"),
      is_reply = is_reply,
      reply_to_id = reply_to_id,
      para_id = para_id
    )
  }

  if (length(extended) > 0 && length(meta) > 0) {
    para_ids <- vapply(meta, function(m) m$para_id, character(1))
    cid_by_para <- stats::setNames(names(meta), para_ids)
    cid_by_para <- cid_by_para[!is.na(names(cid_by_para)) & nzchar(names(cid_by_para))]
    for (cid in names(meta)) {
      pid <- meta[[cid]]$para_id
      if (is.na(pid) || is.null(extended[[pid]])) next
      ex <- extended[[pid]]
      if (!meta[[cid]]$is_reply && !is.na(ex$parent) && nzchar(ex$parent)) {
        meta[[cid]]$is_reply <- TRUE
        # a parent that is not in comments.xml leaves the link unknown but the reply flagged
        meta[[cid]]$reply_to_id <- if (ex$parent %in% names(cid_by_para)) cid_by_para[[ex$parent]] else NA_character_
      }
      if (isTRUE(ex$done)) meta[[cid]]$resolved_in_docx <- TRUE
    }
  }

  # A thread is resolved as a unit: a reply inherits a resolved ancestor's flag
  own_done <- vapply(meta, function(m) isTRUE(m$resolved_in_docx), logical(1))
  for (cid in names(meta)) {
    seen <- cid
    up <- meta[[cid]]$reply_to_id
    while (!own_done[[cid]] && !is.na(up) && up %in% names(meta) && !up %in% seen) {
      if (own_done[[up]]) meta[[cid]]$resolved_in_docx <- TRUE
      seen <- c(seen, up)
      up <- meta[[up]]$reply_to_id
    }
  }
  meta
}

# Main-story paragraphs exclude the mc:Fallback copy of a drawing and the
# paragraphs of a text box (see NOT_IN_FALLBACK and TEXT_LEAF_XPATH above).
MAIN_PARAGRAPH_PREDICATE <- "[not(ancestor::mc:Fallback) and not(ancestor::w:txbxContent)]"
MAIN_PARAGRAPH_XPATH <- paste0("//w:p", MAIN_PARAGRAPH_PREDICATE)
REVISION_NODES_XPATH <- paste0(
  ".//w:ins[", NOT_IN_FALLBACK, "] | .//w:del[", NOT_IN_FALLBACK, "] | ",
  ".//w:moveFrom[", NOT_IN_FALLBACK, "] | .//w:moveTo[", NOT_IN_FALLBACK, "]"
)

#' Paragraphs of the Main Story, Numbered as Word Numbers Them
#' @param doc_xml xml2 document
#' @return xml_nodeset of \code{w:p}, excluding text-box paragraphs and
#'   \code{mc:Fallback} copies
#' @noRd
docx_paragraphs <- function(doc_xml) {
  xml2::xml_find_all(doc_xml, MAIN_PARAGRAPH_XPATH, XML_NAMESPACES)
}

#' Extract Comment Locations and Spans from word/document.xml
#' @param doc_xml xml2 document
#' @return List of comment location records
#' @noRd
extract_comment_locations <- function(doc_xml) {
  paragraphs <- docx_paragraphs(doc_xml)
  if (length(paragraphs) == 0) return(list())

  starts <- list()
  ends <- list()

  # Identify paragraph indices for commentRangeStart and commentRangeEnd
  for (para_idx in seq_along(paragraphs)) {
    p <- paragraphs[[para_idx]]
    start_nodes <- xml2::xml_find_all(p, paste0(".//w:commentRangeStart[", NOT_IN_FALLBACK, "]"), XML_NAMESPACES)
    for (sn in start_nodes) {
      cid <- xml2::xml_attr(sn, "id")
      if (!is.na(cid) && nzchar(cid)) starts[[cid]] <- para_idx
    }
    end_nodes <- xml2::xml_find_all(p, paste0(".//w:commentRangeEnd[", NOT_IN_FALLBACK, "]"), XML_NAMESPACES)
    for (en in end_nodes) {
      cid <- xml2::xml_attr(en, "id")
      if (!is.na(cid) && nzchar(cid)) ends[[cid]] <- para_idx
    }
  }

  results <- list()
  for (cid in names(starts)) {
    if (!cid %in% names(ends)) next
    start_para <- starts[[cid]]
    end_para <- ends[[cid]]

    selected_pieces <- character(0)
    context_pieces <- character(0)
    collecting <- FALSE

    for (p_idx in start_para:end_para) {
      p <- paragraphs[[p_idx]]
      context_pieces <- c(context_pieces, node_text(p))

      # A selection that runs on into the next paragraph is separated from it
      if (collecting) selected_pieces <- c(selected_pieces, " ")

      # Extract elements between commentRangeStart and commentRangeEnd
      descendants <- xml2::xml_find_all(p, paste0(".//*[", NOT_IN_FALLBACK, "]"), XML_NAMESPACES)
      for (el in descendants) {
        tag <- xml2::xml_name(el)
        el_id <- xml2::xml_attr(el, "id")
        if (tag == "commentRangeStart" && !is.na(el_id) && el_id == cid) {
          collecting <- TRUE
          next
        }
        if (tag == "commentRangeEnd" && !is.na(el_id) && el_id == cid) {
          collecting <- FALSE
          break
        }
        if (collecting && tag %in% c("t", "delText", "tab", "ptab", "br", "cr", "noBreakHyphen")) {
          # a tab stop in the paragraph properties is not a tab in the text
          if (tag %in% c("tab", "ptab", "br", "cr", "noBreakHyphen") &&
              !identical(xml2::xml_name(xml2::xml_parent(el)), "r")) next
          txt <- leaf_element_text(tag, xml2::xml_text(el))
          if (nzchar(txt)) selected_pieces <- c(selected_pieces, txt)
        }
      }
    }

    results[[length(results) + 1]] <- list(
      comment_id = cid,
      paragraph_number = as.integer(start_para),
      end_paragraph_number = if (end_para != start_para) as.integer(end_para) else NA_integer_,
      selected_text = clean_review_text(paste(selected_pieces, collapse = "")),
      paragraph_context = clean_review_text(paste(context_pieces, collapse = " "))
    )
  }
  results
}

#' Extract Tracked Changes (Revisions) from word/document.xml
#' @param doc_xml xml2 document
#' @param filename DOCX filename
#' @param min_length Minimum length of changed text
#' @return Tibble of revisions
#' @noRd
extract_docx_revisions <- function(doc_xml, filename, min_length = 2) {
  rev_xpaths <- c(
    insertion = paste0("//w:ins[", NOT_IN_FALLBACK, "]"),
    deletion = paste0("//w:del[", NOT_IN_FALLBACK, "]"),
    move_from = paste0("//w:moveFrom[", NOT_IN_FALLBACK, "]"),
    move_to = paste0("//w:moveTo[", NOT_IN_FALLBACK, "]")
  )
  # A paragraph is identified by its position among the main-story paragraphs, not
  # by format(p): format() of an xml2 node is the constant "<p>" for every
  # paragraph, which gave every tracked change paragraph number 1 (audit A6-01).
  host_xpath <- paste0("ancestor::w:p", MAIN_PARAGRAPH_PREDICATE, "[1]")
  number_xpath <- paste0("count(preceding::w:p", MAIN_PARAGRAPH_PREDICATE, ") + 1")

  rows <- list()
  context_cache <- list()
  for (rev_type in names(rev_xpaths)) {
    xpath <- rev_xpaths[[rev_type]]
    rev_nodes <- xml2::xml_find_all(doc_xml, xpath, XML_NAMESPACES)
    for (node in rev_nodes) {
      txt <- clean_review_text(node_text(node))
      if (nchar(txt) < min_length) next

      # The paragraph that holds the change, numbered as docx_paragraphs() numbers it:
      # the main paragraph (a change inside a text box belongs to the paragraph that
      # holds the box), counted by the main paragraphs before it
      host <- xml2::xml_find_first(node, host_xpath, XML_NAMESPACES)
      p_num <- NA_integer_
      p_ctx <- ""
      if (!is.na(xml2::xml_name(host))) {
        p_num <- as.integer(xml2::xml_find_num(host, number_xpath, XML_NAMESPACES))
        key <- as.character(p_num)
        if (is.null(context_cache[[key]])) context_cache[[key]] <- clean_review_text(node_text(host))
        p_ctx <- context_cache[[key]]
      }

      rows[[length(rows) + 1]] <- tibble::tibble(
        file = filename,
        revision_type = rev_type,
        author = xml2::xml_attr(node, "author"),
        date = xml2::xml_attr(node, "date"),
        changed_text = txt,
        paragraph_number = p_num,
        paragraph_context = p_ctx
      )
    }
  }

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(
    file = character(0),
    revision_type = character(0),
    author = character(0),
    date = character(0),
    changed_text = character(0),
    paragraph_number = integer(0),
    paragraph_context = character(0)
  )
}

#' Reconstruct Original and Accepted Texts from Paragraph Run Elements
#' @param paragraph_node xml2 paragraph node
#' @return List of original_text and accepted_text
#' @noRd
build_redline_texts <- function(paragraph_node) {
  orig_parts <- character(0)
  acc_parts <- character(0)

  visit <- function(el, in_ins, in_del, in_moveto, in_movefrom) {
    tag <- xml2::xml_name(el)
    # the mc:Fallback copy of a drawing repeats the mc:Choice content
    if (tag == "Fallback") return(NULL)
    if (tag == "ins") in_ins <- TRUE
    else if (tag == "del") in_del <- TRUE
    else if (tag == "moveTo") in_moveto <- TRUE
    else if (tag == "moveFrom") in_movefrom <- TRUE

    # a tab, break or non-breaking hyphen is run content too; a tab stop in the
    # paragraph properties is not (its parent is not a run)
    is_text <- tag %in% c("t", "delText")
    is_run_content <- tag %in% c("tab", "ptab", "br", "cr", "noBreakHyphen") &&
      identical(xml2::xml_name(xml2::xml_parent(el)), "r")
    if (is_text || is_run_content) {
      txt <- leaf_element_text(tag, xml2::xml_text(el))
      if (!is.na(txt) && nzchar(txt)) {
        if (!in_ins && !in_moveto) orig_parts <<- c(orig_parts, txt)
        if (!in_del && !in_movefrom) acc_parts <<- c(acc_parts, txt)
      }
      return(NULL)
    }

    children <- xml2::xml_children(el)
    for (ch in children) {
      visit(ch, in_ins, in_del, in_moveto, in_movefrom)
    }
  }

  visit(paragraph_node, FALSE, FALSE, FALSE, FALSE)
  list(
    original_text = clean_review_text(paste(orig_parts, collapse = "")),
    accepted_text = clean_review_text(paste(acc_parts, collapse = ""))
  )
}

#' Detect Table of Contents / Figures / Tables Paragraphs
#' @param doc_xml xml2 document
#' @return Integer vector of paragraph numbers
#' @noRd
compute_toc_lof_paragraph_numbers <- function(doc_xml) {
  paragraphs <- docx_paragraphs(doc_xml)
  if (length(paragraphs) == 0) return(integer(0))

  toc_para_nums <- integer(0)
  field_stack <- list()

  for (para_idx in seq_along(paragraphs)) {
    p <- paragraphs[[para_idx]]
    involved <- any(vapply(field_stack, function(f) isTRUE(f$is_toc), logical(1)))

    pstyle_node <- xml2::xml_find_first(p, "./w:pPr/w:pStyle/@w:val", XML_NAMESPACES)
    pstyle <- if (!is.na(xml2::xml_text(pstyle_node))) toupper(xml2::xml_text(pstyle_node)) else ""
    if (grepl("^(TOC|TOF|TOT)", pstyle)) {
      involved <- TRUE
    }

    descendants <- xml2::xml_find_all(p, paste0(".//*[", NOT_IN_FALLBACK, "]"), XML_NAMESPACES)
    for (el in descendants) {
      tag <- xml2::xml_name(el)
      if (tag == "fldChar") {
        ftype <- xml2::xml_attr(el, "fldCharType")
        if (identical(ftype, "begin")) {
          field_stack[[length(field_stack) + 1]] <- list(instr = "", is_toc = FALSE, separated = FALSE)
        } else if (identical(ftype, "separate") && length(field_stack) > 0) {
          field_stack[[length(field_stack)]]$separated <- TRUE
          instr <- field_stack[[length(field_stack)]]$instr
          if (grepl("\\bTOC\\b", instr, ignore.case = TRUE)) {
            field_stack[[length(field_stack)]]$is_toc <- TRUE
            involved <- TRUE
          } else if (pstyle == "LISTPARAGRAPH" && grepl("HYPERLINK", toupper(instr), fixed = TRUE) && grepl('\\l "_', instr, fixed = TRUE)) {
            involved <- TRUE
          }
        } else if (identical(ftype, "end") && length(field_stack) > 0) {
          popped <- field_stack[[length(field_stack)]]
          field_stack[[length(field_stack)]] <- NULL
          if (isTRUE(popped$is_toc)) involved <- TRUE
        }
      } else if (tag == "instrText") {
        if (length(field_stack) > 0 && !isTRUE(field_stack[[length(field_stack)]]$separated)) {
          t_val <- xml2::xml_text(el)
          if (!is.na(t_val)) {
            field_stack[[length(field_stack)]]$instr <- paste0(field_stack[[length(field_stack)]]$instr, t_val)
          }
        }
      }
    }

    if (involved) {
      toc_para_nums <- c(toc_para_nums, para_idx)
    }
  }

  toc_para_nums
}

#' Attribute a Redlined Paragraph to Everyone Who Changed It
#'
#' A paragraph row combines every tracked change in the paragraph, so it names
#' every distinct author, in the order of their first change (a paragraph one
#' reviewer inserted into and another deleted from is not credited to the last
#' editor alone). \code{date} lists, in the same order, the date of that author's
#' last change; with one author it is the date of the paragraph's last change, as
#' before.
#' @param authors,dates Character vectors, one entry per revision node in the
#'   paragraph, in document order (\code{NA} where the attribute is absent)
#' @return List with character scalars \code{author} and \code{date}
#' @noRd
redline_attribution <- function(authors, dates) {
  last_date <- if (length(dates) > 0) dates[[length(dates)]] else NA_character_
  named <- !is.na(authors) & nzchar(authors)
  if (!any(named)) return(list(author = NA_character_, date = last_date))

  who <- unique(authors[named])
  when <- vapply(who, function(a) {
    d <- dates[named & authors == a]
    d[[length(d)]]
  }, character(1))
  if (length(who) == 1) return(list(author = who, date = unname(when)))

  when[is.na(when)] <- ""
  list(author = paste(who, collapse = "; "), date = if (all(!nzchar(when))) NA_character_ else paste(when, collapse = "; "))
}

#' Extract Paragraph Redlines from word/document.xml
#' @param doc_xml xml2 document
#' @param filename DOCX filename
#' @return Tibble of paragraph redlines
#' @noRd
extract_docx_redlines <- function(doc_xml, filename) {
  paragraphs <- docx_paragraphs(doc_xml)
  if (length(paragraphs) == 0) return(tibble::tibble())

  toc_lof_para_nums <- compute_toc_lof_paragraph_numbers(doc_xml)
  rows <- list()

  for (para_idx in seq_along(paragraphs)) {
    p <- paragraphs[[para_idx]]
    rev_nodes <- xml2::xml_find_all(p, REVISION_NODES_XPATH, XML_NAMESPACES)
    if (length(rev_nodes) == 0) next

    texts <- build_redline_texts(p)
    if (identical(texts$original_text, texts$accepted_text)) next

    who <- redline_attribution(xml2::xml_attr(rev_nodes, "author"), xml2::xml_attr(rev_nodes, "date"))

    rows[[length(rows) + 1]] <- tibble::tibble(
      file = filename,
      paragraph_number = as.integer(para_idx),
      author = who$author,
      date = who$date,
      original_text = texts$original_text,
      accepted_text = texts$accepted_text,
      is_toc_or_lof = para_idx %in% toc_lof_para_nums
    )
  }

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(
    file = character(0),
    paragraph_number = integer(0),
    author = character(0),
    date = character(0),
    original_text = character(0),
    accepted_text = character(0),
    is_toc_or_lof = logical(0)
  )
}

# The parts of a .docx the extractor reads
DOCX_READ_PARTS <- c("word/document.xml", "word/comments.xml", "word/commentsExtended.xml")

# Parts that hold document text the extractor does NOT read. They are only
# scanned, to warn that changes or comments in them are missing from the tracker.
DOCX_UNREAD_PART_PATTERN <- "^word/(footnotes|endnotes|header[0-9]*|footer[0-9]*)\\.xml$"

#' Count the Tracked Changes and Comment Ranges in Parts the Extractor Does Not Read
#'
#' Footnotes, endnotes, headers and footers are separate parts with their own
#' paragraphs; their tracked changes and comment ranges are not extracted.
#' This only counts them, so the caller can say so instead of leaving them out
#' silently. Only changes that carry text are counted (a bare paragraph-mark
#' marker is not), and a part that cannot be read is reported with \code{NA}
#' counts.
#' @param td Folder the parts were extracted to
#' @param parts Entry names of the parts to scan (those extracted into \code{td})
#' @param filename DOCX filename
#' @param skipped Entry names of parts too large to scan (reported with \code{NA})
#' @return Tibble with \code{file}, \code{part}, \code{tracked_changes},
#'   \code{comment_ranges}; one row per part that holds any, or cannot be read
#' @noRd
scan_unread_docx_parts <- function(td, parts, filename, skipped = character(0)) {
  one_row <- function(part, tracked, ranges) {
    tibble::tibble(file = filename, part = part, tracked_changes = tracked, comment_ranges = ranges)
  }
  with_text <- paste0("[", NOT_IN_FALLBACK, " and (.//w:t or .//w:delText)]")
  tracked_xpath <- sprintf(
    "count(//w:ins%s | //w:del%s | //w:moveFrom%s | //w:moveTo%s)",
    with_text, with_text, with_text, with_text
  )
  rows <- lapply(parts, function(part) {
    root <- tryCatch(xml2::read_xml(file.path(td, part)), error = function(e) NULL)
    if (is.null(root)) return(one_row(part, NA_integer_, NA_integer_))
    one_row(
      part,
      as.integer(xml2::xml_find_num(root, tracked_xpath, XML_NAMESPACES)),
      as.integer(xml2::xml_find_num(root, "count(//w:commentRangeStart)", XML_NAMESPACES))
    )
  })
  rows <- c(rows, lapply(skipped, function(part) one_row(part, NA_integer_, NA_integer_)))
  out <- if (length(rows) > 0) dplyr::bind_rows(rows) else one_row(character(0), integer(0), integer(0))
  out[is.na(out$tracked_changes) | out$tracked_changes > 0 | out$comment_ranges > 0, ]
}

#' Warning Text for Changes the Extractor Could Not Read
#' @param unread Rows of \code{scan_unread_docx_parts()} for several files
#' @return Character scalar
#' @noRd
format_unread_parts_warning <- function(unread) {
  describe <- function(tracked, ranges) {
    if (is.na(tracked) || is.na(ranges)) return("could not be read to check")
    paste(c(
      if (tracked > 0) sprintf("%d tracked change(s)", tracked),
      if (ranges > 0) sprintf("%d comment range(s)", ranges)
    ), collapse = " and ")
  }
  files <- unique(unread$file)
  shown <- utils::head(files, 10)
  per_file <- vapply(shown, function(f) {
    u <- unread[unread$file == f, ]
    parts <- vapply(seq_len(nrow(u)), function(i) {
      sprintf("%s: %s", sub("^word/", "", u$part[i]), describe(u$tracked_changes[i], u$comment_ranges[i]))
    }, character(1))
    sprintf("%s (%s)", f, paste(parts, collapse = "; "))
  }, character(1))
  sprintf(
    paste0(
      "Parts of a document that the extractor does not read (footnotes, endnotes, headers and footers) ",
      "hold tracked changes or comment ranges that are NOT in the tracker: %s%s. ",
      "Check those parts in Word."
    ),
    paste(per_file, collapse = "; "),
    if (length(files) > length(shown)) sprintf("; and %d more file(s)", length(files) - length(shown)) else ""
  )
}

#' Resolve the Limits Applied to an Untrusted .docx Archive
#'
#' A .docx is a zip archive from outside the project. Before anything is
#' extracted its listing is checked against three limits, so that an archive
#' with a huge number of entries or a multi-gigabyte entry is reported in
#' \code{$errors} instead of filling the temporary folder or stalling the run.
#' Defaults are generous (far above any Word report) and can be changed per call
#' (\code{docx_limits} of \code{cbe_docx_review_extract()}) or session-wide with
#' the options \code{review.docx_max_entries}, \code{review.docx_max_bytes} and
#' \code{review.docx_max_xml_bytes}; \code{Inf} switches a limit off.
#' @param limits \code{NULL} (the defaults) or a named list with any of
#'   \code{max_entries} (entries in the archive), \code{max_total_bytes} (summed
#'   uncompressed size) and \code{max_xml_bytes} (uncompressed size of any one XML
#'   part that is read).
#' @return Named list with all three limits
#' @noRd
resolve_docx_limits <- function(limits = NULL) {
  resolved <- list(
    max_entries = getOption("review.docx_max_entries", 10000),
    max_total_bytes = getOption("review.docx_max_bytes", 2 * 1024^3),
    max_xml_bytes = getOption("review.docx_max_xml_bytes", 100 * 1024^2)
  )
  if (!is.null(limits)) {
    if (!is.list(limits) || length(limits) == 0 || is.null(names(limits)) || any(!nzchar(names(limits)))) {
      stop("`docx_limits` must be NULL or a named list with entries from: ",
           paste(names(resolved), collapse = ", "), ".", call. = FALSE)
    }
    unknown <- setdiff(names(limits), names(resolved))
    if (length(unknown) > 0) {
      stop(sprintf("Unknown `docx_limits` entr%s: %s. Use: %s.",
                   if (length(unknown) == 1) "y" else "ies", paste(unknown, collapse = ", "),
                   paste(names(resolved), collapse = ", ")), call. = FALSE)
    }
    resolved[names(limits)] <- limits
  }
  valid <- vapply(resolved, function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0, logical(1))
  if (!all(valid)) {
    stop(sprintf("`docx_limits$%s` must be a single positive number (Inf for no limit).",
                 names(resolved)[!valid][1]), call. = FALSE)
  }
  resolved
}

#' Check a .docx Archive Listing Against the Limits Before Extracting Anything
#' @param listing Result of \code{utils::unzip(list = TRUE)}
#' @param limits Result of \code{resolve_docx_limits()}
#' @return \code{NULL}, invisibly; stops with a message when a limit is exceeded
#' @noRd
check_docx_listing <- function(listing, limits) {
  mb <- function(x) sprintf("%.1f MB", x / 1024^2)
  n_entries <- nrow(listing)
  if (n_entries > limits$max_entries) {
    stop(sprintf("The archive has %d entries; the limit is %s (docx_limits$max_entries).",
                 n_entries, format(limits$max_entries, scientific = FALSE)), call. = FALSE)
  }
  total <- sum(listing$Length)
  if (total > limits$max_total_bytes) {
    stop(sprintf("The archive is %s uncompressed; the limit is %s (docx_limits$max_total_bytes).",
                 mb(total), mb(limits$max_total_bytes)), call. = FALSE)
  }
  read_parts <- intersect(DOCX_READ_PARTS, listing$Name)
  sizes <- listing$Length[match(read_parts, listing$Name)]
  too_big <- sizes > limits$max_xml_bytes
  if (any(too_big)) {
    stop(sprintf("%s is %s uncompressed; the limit for one XML part is %s (docx_limits$max_xml_bytes).",
                 read_parts[too_big][1], mb(sizes[too_big][1]), mb(limits$max_xml_bytes)), call. = FALSE)
  }
  invisible(NULL)
}

#' Extract Only the Named Parts of a .docx into a Folder
#' @param docx_path Path to the .docx
#' @param parts Entry names to extract
#' @param exdir Destination folder
#' @return Invisibly, the extracted paths
#' @noRd
unzip_docx_parts <- function(docx_path, parts, exdir) {
  if (length(parts) == 0) return(invisible(character(0)))
  invisible(utils::unzip(docx_path, files = parts, exdir = exdir))
}

#' Extract All Elements from a Single DOCX File
#' @param docx_path Path to DOCX file
#' @param min_revision_length Minimum revision length
#' @param docx_limits Limits on the archive, see \code{resolve_docx_limits()}
#' @return List of comments, revisions, and redlines tibbles
#' @noRd
extract_from_docx <- function(docx_path, min_revision_length = 2, docx_limits = NULL) {
  filename <- basename(docx_path)
  limits <- resolve_docx_limits(docx_limits)
  listing <- utils::unzip(docx_path, list = TRUE)
  check_docx_listing(listing, limits)

  td <- tempfile(pattern = "docx_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # only the parts that are read, plus the parts that are scanned for the warning,
  # never the whole (untrusted) archive
  read_parts <- intersect(DOCX_READ_PARTS, listing$Name)
  unread_parts <- listing$Name[grepl(DOCX_UNREAD_PART_PATTERN, listing$Name)]
  too_big <- listing$Length[match(unread_parts, listing$Name)] > limits$max_xml_bytes
  unzip_docx_parts(docx_path, c(read_parts, unread_parts[!too_big]), td)

  comments_xml_path <- file.path(td, "word", "comments.xml")
  comments_ext_path <- file.path(td, "word", "commentsExtended.xml")
  doc_xml_path <- file.path(td, "word", "document.xml")

  metadata <- if (file.exists(comments_xml_path)) read_comments_metadata(comments_xml_path, comments_ext_path) else list()
  doc_xml <- if (file.exists(doc_xml_path)) read_docx_xml(doc_xml_path, "word/document.xml") else NULL

  comments_list <- list()
  located_ids <- character(0)

  if (!is.null(doc_xml)) {
    locations <- extract_comment_locations(doc_xml)
    for (loc in locations) {
      cid <- loc$comment_id
      located_ids <- c(located_ids, cid)
      meta <- if (cid %in% names(metadata)) metadata[[cid]] else list()

      comments_list[[length(comments_list) + 1]] <- tibble::tibble(
        file = filename,
        comment_id = cid,
        author = if (!is.null(meta$author)) meta$author else NA_character_,
        date = if (!is.null(meta$date)) meta$date else NA_character_,
        comment_text = if (!is.null(meta$comment_text)) meta$comment_text else "",
        paragraph_number = loc$paragraph_number,
        end_paragraph_number = loc$end_paragraph_number,
        selected_text = loc$selected_text,
        paragraph_context = loc$paragraph_context,
        resolved_in_docx = if (!is.null(meta$resolved_in_docx)) meta$resolved_in_docx else FALSE,
        is_reply = if (!is.null(meta$is_reply)) meta$is_reply else FALSE,
        reply_to_id = if (!is.null(meta$reply_to_id)) meta$reply_to_id else NA_character_
      )
    }

    # Metadata comments not positioned in document body
    for (cid in names(metadata)) {
      if (!cid %in% located_ids) {
        meta <- metadata[[cid]]
        comments_list[[length(comments_list) + 1]] <- tibble::tibble(
          file = filename,
          comment_id = cid,
          author = if (!is.null(meta$author)) meta$author else NA_character_,
          date = if (!is.null(meta$date)) meta$date else NA_character_,
          comment_text = if (!is.null(meta$comment_text)) meta$comment_text else "",
          paragraph_number = NA_integer_,
          end_paragraph_number = NA_integer_,
          selected_text = "",
          paragraph_context = "",
          resolved_in_docx = if (!is.null(meta$resolved_in_docx)) meta$resolved_in_docx else FALSE,
          is_reply = if (!is.null(meta$is_reply)) meta$is_reply else FALSE,
          reply_to_id = if (!is.null(meta$reply_to_id)) meta$reply_to_id else NA_character_
        )
      }
    }

    revisions <- extract_docx_revisions(doc_xml, filename, min_revision_length)
    redlines <- extract_docx_redlines(doc_xml, filename)
  } else {
    for (cid in names(metadata)) {
      meta <- metadata[[cid]]
      comments_list[[length(comments_list) + 1]] <- tibble::tibble(
        file = filename,
        comment_id = cid,
        author = if (!is.null(meta$author)) meta$author else NA_character_,
        date = if (!is.null(meta$date)) meta$date else NA_character_,
        comment_text = if (!is.null(meta$comment_text)) meta$comment_text else "",
        paragraph_number = NA_integer_,
        end_paragraph_number = NA_integer_,
        selected_text = "",
        paragraph_context = "",
        resolved_in_docx = if (!is.null(meta$resolved_in_docx)) meta$resolved_in_docx else FALSE,
        is_reply = if (!is.null(meta$is_reply)) meta$is_reply else FALSE,
        reply_to_id = if (!is.null(meta$reply_to_id)) meta$reply_to_id else NA_character_
      )
    }
    revisions <- tibble::tibble()
    redlines <- tibble::tibble()
  }

  comments <- if (length(comments_list) > 0) dplyr::bind_rows(comments_list) else tibble::tibble()
  unread <- scan_unread_docx_parts(td, unread_parts[!too_big], filename, skipped = unread_parts[too_big])
  list(comments = comments, revisions = revisions, redlines = redlines, unread_parts = unread)
}

# =============================================================================
# NON-DESTRUCTIVE MERGE ENGINE & FORK OVERLAYS
# =============================================================================

#' Load Existing Review Tracker Data
#' @param tracker_path Path to existing Excel tracker
#' @return List of existing data frames and headers
#' @noRd
load_existing_tracker <- function(tracker_path) {
  res <- list(
    headers = character(0),
    comments = tibble::tibble(),
    revisions = tibble::tibble(),
    redlines = tibble::tibble(),
    redline_headers = character(0),
    documents = list(),
    fork_baseline = NULL
  )
  if (!file.exists(tracker_path)) return(res)

  sheets <- tryCatch(readxl::excel_sheets(tracker_path), error = function(e) character(0))
  if (length(sheets) == 0) return(res)

  # A fork workbook carries the reviewer's cells as they were generated (see
  # build_fork_baseline()); a master or an older fork does not.
  if (FORK_BASELINE_SHEET %in% sheets) {
    res$fork_baseline <- tryCatch(
      readxl::read_excel(tracker_path, sheet = FORK_BASELINE_SHEET, col_types = "text"),
      error = function(e) NULL
    )
  }

  if ("Comments" %in% sheets) {
    df_c <- tryCatch(readxl::read_excel(tracker_path, sheet = "Comments", col_types = "text"), error = function(e) tibble::tibble())
    if (nrow(df_c) > 0 || ncol(df_c) > 0) {
      clean_h <- names(df_c)
      clean_h <- clean_h[!clean_h %in% c("duplicate_count_x", "duplicate_count_y")]
      res$headers <- clean_h
      res$comments <- df_c
    }
  }

  if ("TrackedChanges" %in% sheets) {
    df_tc <- tryCatch(readxl::read_excel(tracker_path, sheet = "TrackedChanges", col_types = "text"), error = function(e) tibble::tibble())
    res$revisions <- df_tc
  }

  rl_sheet <- intersect(c("SuggestedChanges", "ParagraphRedlines"), sheets)
  if (length(rl_sheet) > 0) {
    df_rl <- tryCatch(readxl::read_excel(tracker_path, sheet = rl_sheet[1], col_types = "text"), error = function(e) tibble::tibble())
    res$redlines <- df_rl
    res$redline_headers <- names(df_rl)
  }

  # Document-level sign-offs live on the "Documents" sheet, or, with
  # documents_sheet_mode = "per_reviewer", on the "Documents_<id>" sheets (the
  # "Documents" sheet then holds only counts and `resolved`). Every one of those
  # sheets is read and the rows are merged by file; for a column that is blank
  # on one sheet the value of a later sheet is used.
  doc_sheets <- c(intersect("Documents", sheets), grep("^Documents_", sheets, value = TRUE))
  doc_map <- list()
  for (sheet in doc_sheets) {
    df_d <- tryCatch(readxl::read_excel(tracker_path, sheet = sheet, col_types = "text"), error = function(e) tibble::tibble())
    if (nrow(df_d) == 0 || !"file" %in% names(df_d)) next
    for (i in seq_len(nrow(df_d))) {
      fn <- as.character(df_d$file[i])
      if (is.na(fn) || !nzchar(fn)) next
      row <- as.list(df_d[i, ])
      if (is.null(doc_map[[fn]])) {
        doc_map[[fn]] <- row
        next
      }
      for (nm in names(row)) {
        current <- doc_map[[fn]][[nm]]
        if (is.null(current) || is.na(current) || !nzchar(trimws(current))) doc_map[[fn]][[nm]] <- row[[nm]]
      }
    }
  }
  res$documents <- doc_map

  res
}

#' Find Reviewers in an Existing Tracker That Are Not Configured
#'
#' Reviewer ids are project configuration (see \code{\link{review_config}}), so a
#' tracker written under other ids holds reviewers the current run does not know:
#' their fork workbooks are not merged and their document-level sign-offs are not
#' rewritten. Finding them lets the caller warn instead of dropping them silently.
#' Only reviewers holding at least one entry (a sign-off value or a comment) count,
#' so blank leftover columns and fork workbooks do not trigger the warning.
#' @param existing_comments Comments read from the existing master tracker
#' @param existing_redlines Suggested changes read from the existing master tracker
#' @param existing_docs Per-document rows read from the existing Documents sheet
#' @param output_dir Directory holding the master tracker and its fork workbooks
#' @param tracker Master tracker file name
#' @param reviewer_ids Configured fork and sign-off reviewer identifiers
#' @return Character vector of unconfigured reviewer ids
#' @noRd
find_unconfigured_reviewers <- function(existing_comments, existing_redlines, existing_docs,
                                        output_dir, tracker, reviewer_ids) {
  has_entries <- function(x) any(!is.na(x) & nzchar(trimws(as.character(x))))
  entries_for <- function(id, comments, redlines, docs = list()) {
    cols <- c(id, paste0(id, "_comment"))
    has_entries(c(
      unlist(comments[intersect(cols, names(comments))]),
      unlist(redlines[intersect(cols, names(redlines))]),
      unlist(lapply(docs, function(d) d[intersect(cols, names(d))]))
    ))
  }

  # A reviewer owns an `<id>` column plus an `<id>_comment` column ...
  headers <- unique(c(names(existing_comments), names(existing_redlines)))
  comment_cols <- grep("_comment$", headers, value = TRUE)
  candidates <- intersect(sub("_comment$", "", comment_cols), headers)

  # ... and, for fork reviewers, a `<tracker>_<id>.<ext>` workbook
  stem <- tools::file_path_sans_ext(tracker)
  ext <- tools::file_ext(tracker)
  ext <- if (nzchar(ext)) paste0(".", ext) else ".xlsx"
  prefix <- paste0(stem, "_")
  files <- list.files(output_dir)
  forks <- files[startsWith(files, prefix) & endsWith(files, ext)]
  candidates <- setdiff(
    unique(c(candidates, substr(forks, nchar(prefix) + 1L, nchar(forks) - nchar(ext)))),
    reviewer_ids
  )

  in_use <- vapply(candidates, function(id) {
    if (entries_for(id, existing_comments, existing_redlines, existing_docs)) return(TRUE)
    fork_path <- file.path(output_dir, paste0(prefix, id, ext))
    if (!file.exists(fork_path)) return(FALSE)
    fork <- load_existing_tracker(fork_path)
    entries_for(id, fork$comments, fork$redlines)
  }, logical(1))
  candidates[in_use]
}

FORK_BASELINE_SHEET <- "ForkBaseline"

#' A reviewer's sign-off cell in a canonical form, for comparing cells
#' @param x Cell values (logical, "TRUE"/"FALSE" text, or anything typed).
#' @return Character vector: "" for a blank cell, else "TRUE" or "FALSE".
#' @noRd
normalize_flag_cell <- function(x) {
  x <- trimws(as.character(x))
  out <- rep("", length(x))
  filled <- !is.na(x) & nzchar(x)
  out[filled] <- ifelse(toupper(x[filled]) == "TRUE", "TRUE", "FALSE")
  out
}

#' A reviewer's free-text cell in a canonical form, for comparing cells
#' @param x Cell values.
#' @return Character vector with NA as "" and the ends trimmed.
#' @noRd
normalize_note_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

#' The cells of a reviewer's fork workbook as they were generated
#'
#' A fork is regenerated on every run from the merged rows, and the reviewer's
#' edits to it come back on the next run. To tell an edit made in the fork from
#' one made in the master (a coordinator recording a sign-off there, say), each
#' fork carries this table, on a hidden sheet: for every row, the reviewer's
#' sign-off and note as written.
#' @param reviewer The fork's reviewer id.
#' @param comments_rows,redlines_rows The merged rows the fork is written from.
#' @return Character tibble: \code{table}, the row's key columns, \code{value}
#'   and \code{note}.
#' @noRd
build_fork_baseline <- function(reviewer, comments_rows, redlines_rows) {
  part <- function(df, table, key_cols) {
    n <- nrow(df)
    pick <- function(col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, n)
    keys <- stats::setNames(lapply(key_cols, pick), key_cols)
    tibble::as_tibble(c(
      list(table = rep(table, n)),
      keys,
      list(
        value = normalize_flag_cell(pick(reviewer)),
        note = normalize_note_cell(pick(paste0(reviewer, "_comment")))
      )
    ))
  }
  out <- dplyr::bind_rows(
    part(comments_rows, "Comments", c("file", "comment_id", "comment_text")),
    part(redlines_rows, "SuggestedChanges", c("file", "paragraph_number", "original_text"))
  )
  for (col in c("file", "comment_id", "comment_text", "paragraph_number", "original_text")) {
    if (!col %in% names(out)) out[[col]] <- NA_character_
  }
  out[c("table", "file", "comment_id", "comment_text", "paragraph_number", "original_text", "value", "note")]
}

#' Bring one reviewer's fork cells into a master table
#'
#' A cell of the fork is applied only when the reviewer changed it, that is when
#' it differs from the value the fork was generated with (the fork's baseline);
#' a cell the reviewer left alone never replaces what is in the master, so a
#' value typed into the master since the fork was generated survives. A cell the
#' reviewer emptied is emptied in the master. Without a baseline (a fork written
#' by an earlier version) a blank fork cell is taken as untouched. When the
#' master's cell was changed too, to a value that is not the fork's, the fork
#' wins and the cell is reported.
#' @param master Master table (comments or suggested changes) as read back.
#' @param fork The fork's table of the same kind.
#' @param reviewer The fork's reviewer id.
#' @param key_tiers Key columns for pairing fork rows with master rows, most
#'   specific first (see \code{match_rows_one_to_one()}).
#' @param baseline The fork's baseline rows of this kind, or NULL.
#' @param describe Function giving a short description of fork row(s).
#' @return List: the updated \code{master} and \code{conflicts} (a list of
#'   lists with \code{reviewer}, \code{row}, \code{cell}, \code{master} and
#'   \code{fork}).
#' @noRd
overlay_fork_table <- function(master, fork, reviewer, key_tiers, baseline, describe) {
  conflicts <- list()
  pairs <- match_rows_one_to_one(fork, master, key_tiers)
  fork_rows <- which(!is.na(pairs))
  if (length(fork_rows) == 0) return(list(master = master, conflicts = conflicts))
  master_rows <- pairs[fork_rows]

  base_rows <- rep(NA_integer_, length(fork_rows))
  if (!is.null(baseline) && nrow(baseline) > 0) {
    base_rows <- match_rows_one_to_one(fork, baseline, key_tiers)[fork_rows]
  }
  has_base <- !is.na(base_rows)

  for (col in c(reviewer, paste0(reviewer, "_comment"))) {
    if (!col %in% names(fork)) next
    is_flag <- identical(col, reviewer)
    normalize <- if (is_flag) normalize_flag_cell else normalize_note_cell
    if (!col %in% names(master)) master[[col]] <- rep(NA_character_, nrow(master))
    master[[col]] <- as.character(master[[col]])

    fork_cell <- normalize(fork[[col]][fork_rows])
    master_cell <- normalize(master[[col]][master_rows])
    base_cell <- rep("", length(fork_rows))
    base_cell[has_base] <- normalize(baseline[[if (is_flag) "value" else "note"]][base_rows[has_base]])

    fork_changed <- ifelse(has_base, fork_cell != base_cell, nzchar(fork_cell))
    master_changed <- ifelse(has_base, master_cell != base_cell, nzchar(master_cell))
    take <- fork_changed & fork_cell != master_cell

    for (k in which(take & master_changed)) {
      conflicts[[length(conflicts) + 1]] <- list(
        reviewer = reviewer, row = describe(fork, fork_rows[k]), cell = col,
        master = master_cell[k], fork = fork_cell[k]
      )
    }
    raw <- as.character(fork[[col]][fork_rows])
    raw[!nzchar(fork_cell)] <- NA_character_
    master[[col]][master_rows[take]] <- raw[take]
  }
  list(master = master, conflicts = conflicts)
}

#' Apply Reviewer Fork Overlays
#'
#' Reads each configured reviewer's fork workbook and brings the reviewer's own
#' edits (their sign-off and note columns) into the master rows; see
#' \code{overlay_fork_table()} for which cells count as edits. Warns, once, about
#' cells where the master and a fork were both changed.
#' @param output_dir Directory where forks reside
#' @param tracker_basename Base tracker name
#' @param existing_comments Comments tibble
#' @param existing_redlines Redlines tibble
#' @param fork_reviewers Vector of reviewer IDs
#' @param verbose Logical verbose flag
#' @return Updated list of comments and redlines
#' @noRd
apply_reviewer_fork_overlays <- function(output_dir, tracker_basename, existing_comments, existing_redlines, fork_reviewers, verbose = FALSE) {
  if (nrow(existing_comments) == 0 && nrow(existing_redlines) == 0) {
    return(list(comments = existing_comments, redlines = existing_redlines))
  }

  tracker_stem <- tools::file_path_sans_ext(tracker_basename)
  tracker_ext <- tools::file_ext(tracker_basename)
  if (nzchar(tracker_ext)) tracker_ext <- paste0(".", tracker_ext) else tracker_ext <- ".xlsx"

  conflicts <- list()
  for (rev in fork_reviewers) {
    fork_filename <- sprintf("%s_%s%s", tracker_stem, rev, tracker_ext)
    fork_path <- file.path(output_dir, fork_filename)
    if (!file.exists(fork_path)) next

    fork_data <- load_existing_tracker(fork_path)
    baseline <- fork_data$fork_baseline
    baseline_of <- function(table) {
      if (is.null(baseline) || !"table" %in% names(baseline)) return(NULL)
      baseline[!is.na(baseline$table) & baseline$table == table, ]
    }

    # Comments. A fork row is paired with one master row: by file, comment id
    # and text first (ids are not unique once a prior-round row and an active
    # row share one), then by file and comment id alone.
    if (nrow(fork_data$comments) > 0 && nrow(existing_comments) > 0) {
      if (all(c("file", "comment_id") %in% names(fork_data$comments)) &&
          all(c("file", "comment_id") %in% names(existing_comments))) {
        out <- overlay_fork_table(
          existing_comments, fork_data$comments, rev,
          key_tiers = list(c("file", "comment_id", "comment_text"), c("file", "comment_id")),
          baseline = baseline_of("Comments"),
          describe = function(df, i) sprintf("%s comment %s", df$file[i], df$comment_id[i])
        )
        existing_comments <- out$master
        conflicts <- c(conflicts, out$conflicts)
      }
    }

    # SuggestedChanges / Redlines
    if (nrow(fork_data$redlines) > 0 && nrow(existing_redlines) > 0) {
      redline_key <- c("file", "paragraph_number", "original_text")
      if (all(redline_key %in% names(fork_data$redlines)) &&
          all(redline_key %in% names(existing_redlines))) {
        out <- overlay_fork_table(
          existing_redlines, fork_data$redlines, rev,
          key_tiers = list(redline_key),
          baseline = baseline_of("SuggestedChanges"),
          describe = function(df, i) sprintf("%s suggested change at paragraph %s", df$file[i], df$paragraph_number[i])
        )
        existing_redlines <- out$master
        conflicts <- c(conflicts, out$conflicts)
      }
    }

    if (verbose) {
      message(sprintf("Applied %s's fork (%s): %d comment row(s), %d suggested-change row(s).",
                      rev, basename(fork_path), nrow(fork_data$comments), nrow(fork_data$redlines)))
    }
  }

  if (length(conflicts) > 0) {
    shown <- vapply(utils::head(conflicts, 5), function(cf) {
      sprintf("%s, %s of %s: master '%s', fork '%s'", cf$row, cf$cell, cf$reviewer, cf$master, cf$fork)
    }, character(1))
    warning(
      sprintf(
        paste0(
          "%d cell(s) were changed both in the master workbook and in a reviewer's fork since the last run; ",
          "the fork's value was kept and the master's was replaced (the master as it was is in the backups folder): %s%s"
        ),
        length(conflicts), paste(shown, collapse = "; "),
        if (length(conflicts) > 5) sprintf("; and %d more", length(conflicts) - 5L) else ""
      ),
      call. = FALSE
    )
  }

  list(comments = existing_comments, redlines = existing_redlines)
}

# Column types of the merged tables. Rows read back from a tracker workbook are
# all text, freshly extracted rows are typed, and a merge mixes the two, so
# every row is coerced to the schema of a freshly extracted one (the schema a
# first run produces): these integer and logical columns, text everywhere else.
COMMENT_INTEGER_COLUMNS <- c("paragraph_number", "end_paragraph_number", "duplicate_count")
COMMENT_LOGICAL_COLUMNS <- c("is_reply", "resolved_in_docx")
REVISION_INTEGER_COLUMNS <- "paragraph_number"
REDLINE_INTEGER_COLUMNS <- "paragraph_number"
REDLINE_LOGICAL_COLUMNS <- "is_toc_or_lof"

#' Parse TRUE/FALSE flags of any storage type
#' @param x Logical, character or numeric vector ("TRUE", "false", 1, ...)
#' @return Logical vector; anything else, and blanks, are NA
#' @noRd
parse_review_flags <- function(x) {
  if (is.logical(x)) return(x)
  s <- toupper(trimws(as.character(x)))
  out <- rep(NA, length(s))
  out[!is.na(s) & s %in% c("TRUE", "T", "1")] <- TRUE
  out[!is.na(s) & s %in% c("FALSE", "F", "0")] <- FALSE
  out
}

#' Coerce a table of review rows to one schema
#'
#' Selects \code{columns} (a missing one becomes NA), types the integer and
#' logical columns, and makes every other column character.
#' @param df A data frame or tibble.
#' @param columns Character vector of the columns to return, in order.
#' @param integer_cols,logical_cols Columns to type as integer / logical.
#' @return A tibble with exactly \code{columns}.
#' @noRd
coerce_review_columns <- function(df, columns, integer_cols = character(0), logical_cols = character(0)) {
  n <- nrow(df)
  out <- lapply(columns, function(col) {
    x <- if (col %in% names(df)) df[[col]] else rep(NA, n)
    if (col %in% integer_cols) {
      suppressWarnings(as.integer(as.character(x)))
    } else if (col %in% logical_cols) {
      parse_review_flags(x)
    } else {
      as.character(x)
    }
  })
  names(out) <- columns
  tibble::as_tibble(out)
}

#' Build a typed tibble from a list of row lists
#' @param rows List of named lists, one per row (a name that is missing is NA).
#' @inheritParams coerce_review_columns
#' @noRd
review_rows_to_tibble <- function(rows, columns, integer_cols = character(0), logical_cols = character(0)) {
  as_text <- lapply(columns, function(col) {
    vapply(rows, function(r) {
      v <- r[[col]]
      if (is.null(v) || length(v) == 0 || is.na(v[[1]])) NA_character_ else as.character(v[[1]])
    }, character(1))
  })
  names(as_text) <- columns
  coerce_review_columns(tibble::as_tibble(as_text), columns, integer_cols, logical_cols)
}

# =============================================================================
# ROW IDENTITY
# =============================================================================

#' Whitespace-free form of a review text, for comparing texts
#'
#' Texts are compared without any whitespace so that a row written by an earlier
#' version of the extractor, which fused the words of a tab or a line break
#' ("HelloWorld"), still matches the same text read today ("Hello World").
#' @param x Character vector.
#' @return Character vector of the same length.
#' @noRd
review_text_key <- function(x) {
  if (length(x) == 0) return(character(0))
  gsub("[[:space:]]+", "", clean_review_text(x))
}

#' Match key of each row of a review table, built from some of its columns
#' @param df Data frame.
#' @param cols Columns to use: text columns compare without whitespace, paragraph
#'   numbers as integers, everything else trimmed.
#' @return Character vector with one key per row.
#' @noRd
review_match_key <- function(df, cols) {
  parts <- lapply(cols, function(col) {
    x <- df[[col]]
    if (col %in% c("comment_text", "original_text", "accepted_text", "changed_text")) {
      review_text_key(x)
    } else if (col %in% c("paragraph_number", "end_paragraph_number")) {
      v <- suppressWarnings(as.integer(x))
      ifelse(is.na(v), "", as.character(v))
    } else {
      v <- trimws(as.character(x))
      v[is.na(v)] <- ""
      v
    }
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

#' Pair the rows of one table with rows of another, one to one
#'
#' Tiers are tried in order, each over the rows still unpaired, so a
#' more specific key wins over a looser one. A tier that names a column either
#' table lacks is skipped. Rows with equal keys are paired in order.
#' @param from,to Data frames.
#' @param key_tiers List of character vectors of column names, most specific first.
#' @return Integer vector, one entry per row of \code{from}: the paired row of
#'   \code{to}, or NA.
#' @noRd
match_rows_one_to_one <- function(from, to, key_tiers) {
  res <- rep(NA_integer_, nrow(from))
  used <- rep(FALSE, nrow(to))
  for (cols in key_tiers) {
    if (!all(cols %in% names(from)) || !all(cols %in% names(to))) next
    from_key <- review_match_key(from, cols)
    to_key <- review_match_key(to, cols)
    for (i in which(is.na(res))) {
      j <- which(!used & to_key == from_key[i])[1]
      if (!is.na(j)) {
        res[i] <- j
        used[j] <- TRUE
      }
    }
  }
  res
}

#' Match freshly extracted comments to the comment rows of an existing tracker
#'
#' Word renumbers comment ids when it saves a document, so an id says nothing
#' about which comment is meant from one round to the next, but what a comment
#' says does. A comment therefore matches a row of the same file only when its
#' text agrees (compared without whitespace, see \code{review_text_key()}); an
#' id that matches while the text differs is a different comment, and the old
#' row is left to be kept as a prior-round row. Among the rows whose text agrees
#' one is chosen in two passes over the incoming comments, so that an exact
#' match is never taken by a comment that only loosely matches it:
#' \enumerate{
#'   \item the same author and the same date;
#'   \item the same author, or an author missing on one side (the date may
#'     have changed).
#' }
#' Within a pass the best candidate has, in this order, the same date, the same
#' author, the same comment id, and the nearest paragraph number; ties go to
#' the first row. Each existing row is used at most once. A comment with no text
#' matches only a row that also has none and has the same id, author and date.
#' @param incoming Freshly extracted comments (file, comment_id, author, date,
#'   comment_text, paragraph_number).
#' @param existing Existing tracker rows with the same columns, as text.
#' @return Integer vector, one entry per row of \code{incoming}: the matched row
#'   of \code{existing}, or NA.
#' @noRd
match_incoming_comments <- function(incoming, existing) {
  matched <- rep(NA_integer_, nrow(incoming))
  if (nrow(incoming) == 0 || nrow(existing) == 0) return(matched)

  norm <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }
  as_int <- function(x) suppressWarnings(as.integer(x))
  in_file <- norm(incoming$file)
  in_id <- norm(incoming$comment_id)
  in_author <- norm(incoming$author)
  in_date <- norm(incoming$date)
  in_text <- review_text_key(incoming$comment_text)
  in_para <- as_int(incoming$paragraph_number)
  ex_file <- norm(existing$file)
  ex_id <- norm(existing$comment_id)
  ex_author <- norm(existing$author)
  ex_date <- norm(existing$date)
  ex_text <- review_text_key(existing$comment_text)
  ex_para <- as_int(existing$paragraph_number)

  taken <- rep(FALSE, nrow(existing))
  pick <- function(i, strict) {
    cand <- which(!taken & ex_file == in_file[i] & ex_text == in_text[i])
    if (!nzchar(in_text[i])) {
      cand <- cand[ex_id[cand] == in_id[i] & ex_author[cand] == in_author[i] & ex_date[cand] == in_date[i]]
    } else if (strict) {
      cand <- cand[ex_author[cand] == in_author[i] & ex_date[cand] == in_date[i]]
    } else {
      cand <- cand[ex_author[cand] == in_author[i] | !nzchar(ex_author[cand]) | !nzchar(in_author[i])]
    }
    if (length(cand) == 0) return(NA_integer_)
    para_gap <- abs(ex_para[cand] - in_para[i])
    para_gap[is.na(para_gap)] <- .Machine$integer.max
    best <- order(
      ex_date[cand] != in_date[i], ex_author[cand] != in_author[i],
      ex_id[cand] != in_id[i], para_gap, cand
    )[1]
    cand[best]
  }

  for (strict in c(TRUE, FALSE)) {
    for (i in which(is.na(matched))) {
      j <- pick(i, strict)
      if (!is.na(j)) {
        matched[i] <- j
        taken[j] <- TRUE
      }
    }
  }
  matched
}

#' Check if Row Contains Human Reviewer Feedback
#' @param row Named list or 1-row data frame
#' @param ignored_cols Columns to ignore
#' @return Logical TRUE/FALSE
#' @noRd
has_review_feedback <- function(row, ignored_cols) {
  for (nm in names(row)) {
    if (!nm %in% ignored_cols) {
      val <- row[[nm]]
      if (!is.null(val) && !is.na(val) && nzchar(trimws(as.character(val)))) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Merge Comments Non-Destructively
#' @param incoming_comments Extracted comments
#' @param existing_rows Existing comments
#' @param existing_headers Existing header names
#' @param catalog Pipeline catalog
#' @param scanned_files Vector of scanned files
#' @param fork_reviewers Character vector of fork-reviewer identifiers; defaults to \code{\link{review_config}}.
#' @param signoff_reviewers Character vector of sign-off reviewer identifiers; defaults to \code{\link{review_config}}.
#' @return List with columns and merged rows
#' @noRd
merge_comments <- function(incoming_comments, existing_rows, existing_headers, catalog, scanned_files,
                           fork_reviewers = NULL, signoff_reviewers = NULL) {
  # Build full column list
  all_columns <- canonical_columns(fork_reviewers, signoff_reviewers)
  for (col in existing_headers) {
    if (!col %in% all_columns) all_columns <- c(all_columns, col)
  }

  ignored_cols <- c(METADATA_COLUMNS, SYSTEM_COLUMNS, "resolved")
  available_existing <- if (nrow(existing_rows) > 0) existing_rows else tibble::tibble()

  # Ensure columns exist in available_existing
  for (col in all_columns) {
    if (!col %in% names(available_existing)) {
      available_existing[[col]] <- character(nrow(available_existing))
    }
  }

  # Rows are collected as plain lists and typed once, at the end (see
  # review_rows_to_tibble()): bind_rows() of rows read back as text and rows
  # freshly extracted as integer/logical stops with a vctrs type error.
  merged_list <- list()
  matched_existing_indices <- integer(0)

  # Which existing row, if any, each incoming comment continues. Matching is by
  # content, not by comment id: Word renumbers ids on save (see
  # match_incoming_comments() for the rule).
  incoming_match <- match_incoming_comments(incoming_comments, available_existing)

  if (nrow(incoming_comments) > 0) {
    for (i in seq_len(nrow(incoming_comments))) {
      inc <- incoming_comments[i, ]
      matched_idx <- incoming_match[i]

      if (!is.na(matched_idx)) {
        matched_existing_indices <- c(matched_existing_indices, matched_idx)
        merged_row <- as.list(available_existing[matched_idx, ])
        # Update metadata
        merged_row$file <- inc$file
        merged_row$comment_id <- inc$comment_id
        if (!is.na(inc$author)) merged_row$author <- inc$author
        if (!is.na(inc$date)) merged_row$date <- inc$date
        if (nzchar(inc$comment_text)) merged_row$comment_text <- inc$comment_text
        if (nzchar(inc$selected_text)) merged_row$selected_text <- inc$selected_text
        if (!is.na(inc$paragraph_number)) merged_row$paragraph_number <- inc$paragraph_number
        if (!is.na(inc$end_paragraph_number)) merged_row$end_paragraph_number <- inc$end_paragraph_number
        if (nzchar(inc$paragraph_context)) merged_row$paragraph_context <- inc$paragraph_context
        merged_row$is_reply <- inc$is_reply
        merged_row$reply_to_id <- inc$reply_to_id
        if ("resolved_in_docx" %in% names(inc)) merged_row$resolved_in_docx <- inc$resolved_in_docx
        merged_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- merged_row
      } else {
        # New comment
        new_row <- as.list(stats::setNames(rep(NA_character_, length(all_columns)), all_columns))
        new_row$file <- inc$file
        new_row$comment_id <- inc$comment_id
        new_row$author <- inc$author
        new_row$date <- inc$date
        new_row$comment_text <- inc$comment_text
        new_row$selected_text <- inc$selected_text
        new_row$paragraph_number <- inc$paragraph_number
        new_row$end_paragraph_number <- inc$end_paragraph_number
        new_row$paragraph_context <- inc$paragraph_context
        new_row$is_reply <- inc$is_reply
        new_row$reply_to_id <- inc$reply_to_id
        if ("resolved_in_docx" %in% names(inc)) new_row$resolved_in_docx <- inc$resolved_in_docx
        new_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- new_row
      }
    }
  }

  # Retain prior round comments
  if (nrow(available_existing) > 0) {
    unmatched_existing <- setdiff(seq_len(nrow(available_existing)), matched_existing_indices)
    for (idx in unmatched_existing) {
      row <- as.list(available_existing[idx, ])
      rf <- trimws(as.character(row$file))
      has_fb <- has_review_feedback(row, ignored_cols)
      if (!rf %in% scanned_files || has_fb) {
        if (is.null(row$doc_status) || is.na(row$doc_status) || !nzchar(as.character(row$doc_status)) || row$doc_status == "Active") {
          row$doc_status <- "Prior Round / Not in docx"
        }
        merged_list[[length(merged_list) + 1]] <- row
      }
    }
  }

  merged_df <- review_rows_to_tibble(
    merged_list, all_columns, COMMENT_INTEGER_COLUMNS, COMMENT_LOGICAL_COLUMNS
  )

  # Duplicate count
  if (nrow(merged_df) > 0) {
    clean_txts <- clean_review_text(merged_df$comment_text)
    counts <- table(clean_txts[clean_txts != ""])
    merged_df$duplicate_count <- vapply(clean_txts, function(t) {
      if (nzchar(t) && t %in% names(counts)) as.integer(counts[[t]]) else 1L
    }, integer(1))

    # Pipeline stage
    stages <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$stage, character(1))
    ranks <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
    merged_df$pipeline_stage <- stages

    # Sort
    pnums <- as.integer(merged_df$paragraph_number)
    pnums[is.na(pnums)] <- 999999L
    cids <- suppressWarnings(as.integer(merged_df$comment_id))
    cids[is.na(cids)] <- 999999L

    ord <- order(ranks, pnums, cids, merged_df$file, method = "radix")
    merged_df <- merged_df[ord, all_columns]
  }

  list(columns = all_columns, rows = merged_df)
}

#' Merge Revisions Non-Destructively
#' @param incoming_revisions Extracted revisions
#' @param existing_revisions Existing revisions
#' @param catalog Pipeline catalog
#' @param scanned_files Scanned file list
#' @return Merged revisions tibble
#' @noRd
merge_revisions <- function(incoming_revisions, existing_revisions, catalog, scanned_files) {
  rev_headers <- c("file", "revision_type", "author", "date", "changed_text", "paragraph_number", "paragraph_context")

  # Freshly extracted rows are typed and rows read back from the tracker are
  # text, so both go through the same schema before they are bound together.
  merged_list <- list()
  if (nrow(incoming_revisions) > 0) {
    merged_list[[length(merged_list) + 1]] <- coerce_review_columns(
      incoming_revisions, rev_headers, REVISION_INTEGER_COLUMNS
    )
  }

  if (nrow(existing_revisions) > 0) {
    ex_files <- trimws(as.character(existing_revisions$file))
    unscanned_mask <- !ex_files %in% scanned_files
    if (any(unscanned_mask)) {
      merged_list[[length(merged_list) + 1]] <- coerce_review_columns(
        existing_revisions[unscanned_mask, ], rev_headers, REVISION_INTEGER_COLUMNS
      )
    }
  }

  merged_df <- if (length(merged_list) > 0) {
    dplyr::bind_rows(merged_list)
  } else {
    coerce_review_columns(tibble::tibble(), rev_headers, REVISION_INTEGER_COLUMNS)
  }

  if (nrow(merged_df) > 0) {
    ranks <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
    pnums <- as.integer(merged_df$paragraph_number)
    pnums[is.na(pnums)] <- 999999L
    ord <- order(ranks, pnums, merged_df$file, method = "radix")
    merged_df <- merged_df[ord, rev_headers]
  }

  merged_df
}

#' Merge Paragraph Redlines Non-Destructively
#' @param incoming_redlines Extracted redlines
#' @param existing_rows Existing redlines
#' @param existing_headers Existing header names
#' @param catalog Pipeline catalog
#' @param scanned_files Vector of scanned files
#' @param fork_reviewers Character vector of fork-reviewer identifiers; defaults to \code{\link{review_config}}.
#' @param signoff_reviewers Character vector of sign-off reviewer identifiers; defaults to \code{\link{review_config}}.
#' @return List with columns and merged rows
#' @noRd
merge_redlines <- function(incoming_redlines, existing_rows, existing_headers, catalog, scanned_files,
                           fork_reviewers = NULL, signoff_reviewers = NULL) {
  all_columns <- redline_canonical_columns(fork_reviewers, signoff_reviewers)
  for (col in existing_headers) {
    if (!col %in% all_columns) all_columns <- c(all_columns, col)
  }

  ignored_cols <- c(REDLINE_METADATA_COLUMNS, REDLINE_SYSTEM_COLUMNS, "resolved")
  available_existing <- if (nrow(existing_rows) > 0) existing_rows else tibble::tibble()

  for (col in all_columns) {
    if (!col %in% names(available_existing)) available_existing[[col]] <- character(nrow(available_existing))
  }

  # Rows are collected as lists and typed once at the end (see merge_comments()).
  merged_list <- list()
  matched_existing_indices <- integer(0)

  # The keys of the existing rows do not change while incoming redlines are matched
  # against them, so they are computed once, not once per incoming row
  if (nrow(available_existing) > 0) {
    ex_files <- trimws(as.character(available_existing$file))
    ex_pnums <- suppressWarnings(as.integer(available_existing$paragraph_number))
    ex_origs <- review_text_key(available_existing$original_text)
    ex_accs <- review_text_key(available_existing$accepted_text)
  }

  if (nrow(incoming_redlines) > 0) {
    for (i in seq_len(nrow(incoming_redlines))) {
      inc <- incoming_redlines[i, ]
      f <- trimws(as.character(inc$file))
      pnum <- inc$paragraph_number
      # texts are compared without whitespace, so a row written by an earlier
      # version that fused the words of a tab or break still matches
      orig_txt <- review_text_key(inc$original_text)
      acc_txt <- review_text_key(inc$accepted_text)

      matched_idx <- NA_integer_

      if (nrow(available_existing) > 0) {
        unmatched_mask <- !seq_len(nrow(available_existing)) %in% matched_existing_indices

        # Priority 1: (file, paragraph_number, original_text)
        p1 <- which(unmatched_mask & ex_files == f & ex_pnums == pnum & ex_origs == orig_txt)
        if (length(p1) > 0) {
          matched_idx <- p1[1]
        } else if (nzchar(orig_txt)) {
          # Priority 2: (file, original_text)
          p2 <- which(unmatched_mask & ex_files == f & ex_origs == orig_txt)
          if (length(p2) > 0) matched_idx <- p2[1]
        }
        # Priority 3: (file, accepted_text)
        if (is.na(matched_idx) && nzchar(acc_txt)) {
          p3 <- which(unmatched_mask & ex_files == f & ex_accs == acc_txt)
          if (length(p3) > 0) matched_idx <- p3[1]
        }
      }

      if (!is.na(matched_idx)) {
        matched_existing_indices <- c(matched_existing_indices, matched_idx)
        merged_row <- as.list(available_existing[matched_idx, ])
        merged_row$file <- inc$file
        merged_row$paragraph_number <- inc$paragraph_number
        if (!is.na(inc$author)) merged_row$author <- inc$author
        if (!is.na(inc$date)) merged_row$date <- inc$date
        merged_row$original_text <- inc$original_text
        merged_row$accepted_text <- inc$accepted_text
        merged_row$is_toc_or_lof <- inc$is_toc_or_lof
        merged_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- merged_row
      } else {
        new_row <- as.list(stats::setNames(rep(NA_character_, length(all_columns)), all_columns))
        new_row$file <- inc$file
        new_row$paragraph_number <- inc$paragraph_number
        new_row$author <- inc$author
        new_row$date <- inc$date
        new_row$original_text <- inc$original_text
        new_row$accepted_text <- inc$accepted_text
        new_row$is_toc_or_lof <- inc$is_toc_or_lof
        new_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- new_row
      }
    }
  }

  # Retain prior round redlines
  if (nrow(available_existing) > 0) {
    unmatched_existing <- setdiff(seq_len(nrow(available_existing)), matched_existing_indices)
    for (idx in unmatched_existing) {
      row <- as.list(available_existing[idx, ])
      rf <- trimws(as.character(row$file))
      has_fb <- has_review_feedback(row, ignored_cols)
      if (!rf %in% scanned_files || has_fb) {
        if (is.null(row$doc_status) || is.na(row$doc_status) || !nzchar(as.character(row$doc_status)) || row$doc_status == "Active") {
          row$doc_status <- "Prior Round / Not in docx"
        }
        merged_list[[length(merged_list) + 1]] <- row
      }
    }
  }

  merged_df <- review_rows_to_tibble(
    merged_list, all_columns, REDLINE_INTEGER_COLUMNS, REDLINE_LOGICAL_COLUMNS
  )

  if (nrow(merged_df) > 0) {
    stages <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$stage, character(1))
    ranks <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
    merged_df$pipeline_stage <- stages

    pnums <- as.integer(merged_df$paragraph_number)
    pnums[is.na(pnums)] <- 999999L
    ord <- order(ranks, pnums, merged_df$file, method = "radix")
    merged_df <- merged_df[ord, all_columns]
  }

  list(columns = all_columns, rows = merged_df)
}

# =============================================================================
# EXCEL WORKBOOK GENERATOR (openxlsx)
# =============================================================================

#' Metadata columns that never go into a fork workbook: the Word author of a
#' comment or tracked change is a reviewer identity (every author in the
#' documents, sign-off readers included), so a fork carries none of them.
#' @noRd
FORK_EXCLUDED_METADATA_COLUMNS <- "author"

#' Drop Other Reviewers' Columns for Fork View
#' @param reviewer Current reviewer
#' @param fork_reviewers Vector of all fork reviewers
#' @param signoff_reviewers Vector of sign-off reviewers (always hidden from every fork)
#' @return Vector of column names to omit: the other fork reviewers' and every
#'   sign-off reviewer's columns, plus the Word \code{author} column
#' @noRd
reviewer_fork_drop_columns <- function(reviewer, fork_reviewers, signoff_reviewers = character(0)) {
  drop <- unlist(lapply(signoff_reviewers, function(r) c(r, paste0(r, "_comment"))))
  for (other in fork_reviewers) {
    if (other != reviewer) {
      drop <- c(drop, other, paste0(other, "_comment"))
    }
  }
  c(drop, FORK_EXCLUDED_METADATA_COLUMNS)
}

#' Build live Excel formulas that AND together each fork reviewer's TRUE/"TRUE" cell
#' @param col_letters Named character vector of column letters, one per fork reviewer
#'   (NA entries, meaning that reviewer has no column in this sheet, are dropped).
#' @param row_num Row number(s) the formulas are being written for; one formula is
#'   returned per element.
#' @return A character vector of formulas starting with "=", or NULL if no reviewer columns are present.
#' @noRd
build_resolved_formula <- function(col_letters, row_num) {
  col_letters <- col_letters[!is.na(col_letters)]
  if (length(col_letters) == 0) return(NULL)
  clauses <- lapply(
    unname(col_letters),
    function(cl) sprintf('OR(%s%d=TRUE,%s%d="TRUE")', cl, row_num, cl, row_num)
  )
  sprintf('=IF(AND(%s), TRUE, FALSE)', do.call(paste, c(clauses, sep = ",")))
}

#' Rows the Documents-Sheet Rollup Formulas Look At
#'
#' The rollup compares SUMPRODUCT over a bounded range of the Comments or
#' SuggestedChanges sheet with a COUNTIF over the whole column, so the bound has
#' to reach the last row written or a document whose rows extend past it can
#' never show \code{resolved = TRUE}. (It used to be a fixed 5000.) The bound is
#' the last row written plus 1000 rows of headroom, and never below 5000, so a
#' tracker of up to 3999 rows keeps the formulas it always had. A bounded range
#' (rather than whole-column references) keeps SUMPRODUCT cheap to recalculate.
#' @param n_rows Rows written to the sheet
#' @return Integer row number
#' @noRd
rollup_bound <- function(n_rows) {
  max(5000L, as.integer(n_rows) + 1L + 1000L)
}

#' The Two Conditions of a Documents-Sheet Rollup (Comments, SuggestedChanges)
#'
#' Each is TRUE when every row of the document in that sheet carries TRUE for the
#' reviewer: the SUMPRODUCT count of such rows equals the COUNTIF of the
#' document's rows.
#' @param c_col,rl_col Column letters of the reviewer in Comments / SuggestedChanges
#' @param row_num Documents-sheet row numbers (a vector gives a vector)
#' @param c_bound,rl_bound Last row the ranges cover, see \code{rollup_bound()}
#' @return List of two character vectors, \code{comments} and \code{redlines}
#' @noRd
rollup_conditions <- function(c_col, rl_col, row_num, c_bound, rl_bound) {
  list(
    comments = sprintf(
      'SUMPRODUCT((Comments!$A$2:$A$%d=A%d)*((Comments!$%s$2:$%s$%d=TRUE)+(Comments!$%s$2:$%s$%d="TRUE"))) = COUNTIF(Comments!$A:$A, A%d)',
      c_bound, row_num, c_col, c_col, c_bound, c_col, c_col, c_bound, row_num
    ),
    redlines = sprintf(
      'SUMPRODUCT((SuggestedChanges!$A$2:$A$%d=A%d)*((SuggestedChanges!$%s$2:$%s$%d=TRUE)+(SuggestedChanges!$%s$2:$%s$%d="TRUE"))) = COUNTIF(SuggestedChanges!$A:$A, A%d)',
      rl_bound, row_num, rl_col, rl_col, rl_bound, rl_col, rl_col, rl_bound, row_num
    )
  )
}

# -----------------------------------------------------------------------------
# Cell kinds. A column of a table is written with ONE writeData() call (or one per
# run, for a count column) and styled with ONE addStyle() call per style, instead
# of a writeData()/addStyle(stack = TRUE) pair per cell, whose cost grows with
# every style already in the workbook.
# -----------------------------------------------------------------------------

#' Cells of a Flag Column: TRUE / FALSE, blank when the value is missing or empty
#' @param x Logical or character vector
#' @return Logical vector with \code{NA} for a blank cell
#' @noRd
review_flag_cells <- function(x) {
  blank <- is.na(x) | !nzchar(as.character(x))
  out <- rep(NA, length(x))
  out[!blank] <- is_review_true_vec(x[!blank])
  out
}

#' Cells of a Count Column: a number, or an empty string where it is missing
#' @param x Vector coercible to integer
#' @return Integer vector of class \code{review_count}
#' @noRd
review_count_cells <- function(x) {
  structure(suppressWarnings(as.integer(x)), class = "review_count")
}

#' Cells of a Text Column: the text, an empty string where it is missing
#' @param x Vector
#' @return Character vector
#' @noRd
review_text_cells <- function(x) {
  out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

#' Mark Formula Strings for \code{write_review_column()}
#' @param x Character vector of formulas starting with "="
#' @noRd
review_formula_cells <- function(x) structure(x, class = "review_formula")

#' Write One Column of Cells Down a Sheet
#'
#' Equivalent to one \code{writeData()} per cell (a count column keeps an empty
#' string, not a blank cell, where the number is missing), but one call per run
#' of cells of the same kind.
#' @param wb openxlsx workbook
#' @param sheet Sheet name
#' @param col Column number
#' @param start_row First row
#' @param x Character, logical or integer vector, or the result of
#'   \code{review_count_cells()} / \code{review_formula_cells()}
#' @noRd
write_review_column <- function(wb, sheet, col, start_row, x) {
  n <- length(x)
  if (n == 0) return(invisible(NULL))
  if (inherits(x, "review_formula")) {
    openxlsx::writeFormula(wb, sheet, unclass(x), startCol = col, startRow = start_row)
  } else if (inherits(x, "review_count")) {
    values <- unclass(x)
    runs <- rle(is.na(values))
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    for (k in seq_along(runs$lengths)) {
      idx <- starts[k]:ends[k]
      chunk <- if (runs$values[k]) rep("", length(idx)) else values[idx]
      openxlsx::writeData(wb, sheet, chunk, startCol = col, startRow = start_row + idx[1] - 1L, colNames = FALSE)
    }
  } else {
    openxlsx::writeData(wb, sheet, x, startCol = col, startRow = start_row, colNames = FALSE)
  }
  invisible(NULL)
}

#' Style Whole Columns of a Block of Rows with One Call
#' @param wb openxlsx workbook
#' @param sheet Sheet name
#' @param style openxlsx style
#' @param rows,cols Rows and columns (every combination is styled)
#' @noRd
style_review_block <- function(wb, sheet, style, rows, cols) {
  if (length(rows) == 0 || length(cols) == 0) return(invisible(NULL))
  openxlsx::addStyle(wb, sheet, style, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
}

#' Write the Header Row of a Sheet
#' @param wb openxlsx workbook
#' @param sheet Sheet name
#' @param headers Character vector of column titles
#' @param fills List of header-fill styles, one per header (recycled)
#' @param title_style Style applied on top of the fill
#' @noRd
write_review_header <- function(wb, sheet, headers, fills, title_style) {
  openxlsx::writeData(wb, sheet, matrix(headers, nrow = 1), startCol = 1, startRow = 1, colNames = FALSE)
  fills <- rep_len(fills, length(headers))
  # one addStyle per distinct fill (styles are objects, so compare by identity),
  # then the title style over every header cell: the same fill-then-title order
  # per cell as one pair of calls per cell, so the stacked result is unchanged
  distinct <- list()
  group <- integer(length(fills))
  for (i in seq_along(fills)) {
    seen <- which(vapply(distinct, identical, logical(1), fills[[i]]))
    if (length(seen) == 0) {
      distinct[[length(distinct) + 1L]] <- fills[[i]]
      seen <- length(distinct)
    }
    group[i] <- seen[1]
  }
  for (g in seq_along(distinct)) style_review_block(wb, sheet, distinct[[g]], 1L, which(group == g))
  style_review_block(wb, sheet, title_style, 1L, seq_along(headers))
  invisible(NULL)
}

#' Cell Values of the Comments or SuggestedChanges Table, Column by Column
#'
#' @param df Table of rows
#' @param columns Column names the sheets may show
#' @param n Number of rows
#' @param flag_cols Columns written as TRUE / FALSE / blank
#' @param count_cols Columns written as numbers
#' @return List with \code{cells} (named list of column vectors, \code{resolved}
#'   excluded) and \code{kind} (named character: "flag", "count" or "text")
#' @noRd
review_table_cells <- function(df, columns, n, flag_cols, count_cols) {
  cells <- list()
  kind <- character(0)
  for (cn in setdiff(columns, "resolved")) {
    x <- df[[cn]]
    if (is.null(x)) x <- rep(NA, n)
    if (cn %in% flag_cols) {
      cells[[cn]] <- review_flag_cells(x); kind[[cn]] <- "flag"
    } else if (cn %in% count_cols) {
      cells[[cn]] <- review_count_cells(x); kind[[cn]] <- "count"
    } else {
      cells[[cn]] <- review_text_cells(x); kind[[cn]] <- "text"
    }
  }
  list(cells = cells, kind = kind)
}

#' Value of the "resolved" Column When No Live Formula Can Be Written
#' @param df Table of rows
#' @param n Number of rows
#' @param fork_reviewers Fork-reviewer ids
#' @return Logical vector
#' @noRd
review_resolved_values <- function(df, n, fork_reviewers) {
  if (length(fork_reviewers) == 0) return(rep(FALSE, n))
  per_reviewer <- lapply(fork_reviewers, function(r) {
    if (is.null(df[[r]])) rep(FALSE, n) else is_review_true_vec(df[[r]])
  })
  Reduce(`&`, per_reviewer)
}

#' Prepare Everything the Workbook Writer Needs Once
#'
#' Cell values, per-document figures, the crosswalk and the comment summary do
#' not depend on whose workbook is being written, so they are computed once and
#' the master and every fork workbook are written from the same prepared data
#' (\code{write_review_tracker_excel(prepared = )}).
#' @inheritParams write_review_tracker_excel
#' @return List with \code{comments}, \code{redlines}, \code{revisions},
#'   \code{docs}, \code{docxwalk}, \code{summary} and \code{errors}
#' @noRd
prepare_review_workbook_data <- function(columns,
                                         comments_rows,
                                         revisions_rows,
                                         processed_docs,
                                         errors,
                                         catalog,
                                         redline_columns,
                                         redlines_rows,
                                         existing_docs,
                                         fork_reviewers,
                                         signoff_reviewers) {
  all_reviewer_ids <- c(fork_reviewers, signoff_reviewers)
  n_c <- nrow(comments_rows)
  n_rl <- nrow(redlines_rows)

  comments <- review_table_cells(
    comments_rows, columns, n_c,
    flag_cols = all_reviewer_ids,
    count_cols = c("paragraph_number", "end_paragraph_number", "duplicate_count")
  )
  comments$resolved <- review_resolved_values(comments_rows, n_c, fork_reviewers)

  redlines <- review_table_cells(
    redlines_rows, redline_columns, n_rl,
    flag_cols = c("is_comment", "is_toc_or_lof", all_reviewer_ids),
    count_cols = "paragraph_number"
  )
  redlines$resolved <- review_resolved_values(redlines_rows, n_rl, fork_reviewers)

  rev_headers <- c("file", "revision_type", "author", "date", "changed_text", "paragraph_number", "paragraph_context")
  revisions <- list(cells = stats::setNames(
    lapply(rev_headers, function(h) {
      x <- revisions_rows[[h]]
      review_text_cells(if (is.null(x)) rep(NA, nrow(revisions_rows)) else x)
    }),
    rev_headers
  ))

  # Documents sheet: one entry per document, computed from row indices
  all_doc_files <- unique(c(basename(processed_docs), comments_rows$file))
  all_doc_files <- all_doc_files[!is.na(all_doc_files) & nzchar(all_doc_files)]
  ranks <- vapply(all_doc_files, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  sorted_docs <- all_doc_files[order(ranks, all_doc_files, method = "radix")]

  rows_of <- function(df, f) if (nrow(df) > 0) which(df$file == f) else integer(0)
  c_idx <- lapply(sorted_docs, function(f) rows_of(comments_rows, f))
  rl_idx <- lapply(sorted_docs, function(f) rows_of(redlines_rows, f))
  tc_idx <- lapply(sorted_docs, function(f) rows_of(revisions_rows, f))
  is_reply <- if ("is_reply" %in% names(comments_rows)) is_review_true_vec(comments_rows$is_reply) else logical(n_c)

  all_true <- function(df, rid, idx) {
    if (!rid %in% names(df)) return(FALSE)
    all(is_review_true_vec(df[[rid]][idx]))
  }
  flags <- stats::setNames(lapply(fork_reviewers, function(rid) {
    vapply(seq_along(sorted_docs), function(i) {
      if (length(c_idx[[i]]) == 0 && length(rl_idx[[i]]) == 0) return(FALSE)
      all_true(comments_rows, rid, c_idx[[i]]) && all_true(redlines_rows, rid, rl_idx[[i]])
    }, logical(1))
  }), fork_reviewers)

  doc_map <- if (!is.null(existing_docs)) existing_docs else list()
  signoff <- stats::setNames(lapply(signoff_reviewers, function(rid) {
    dfb <- lapply(sorted_docs, function(f) if (f %in% names(doc_map)) doc_map[[f]] else list())
    list(
      value = vapply(dfb, function(d) {
        raw <- d[[rid]]
        if (!is.null(raw) && !is.na(raw)) is_review_true(raw) else NA
      }, logical(1)),
      comment = vapply(dfb, function(d) {
        raw <- d[[paste0(rid, "_comment")]]
        if (!is.null(raw) && !is.na(raw)) as.character(raw) else ""
      }, character(1))
    )
  }), signoff_reviewers)

  docs <- list(
    files = sorted_docs,
    stage = vapply(sorted_docs, function(f) match_docx_to_pipeline(f, catalog)$stage, character(1), USE.NAMES = FALSE),
    n_comments = lengths(c_idx),
    n_replies = vapply(c_idx, function(i) sum(is_reply[i]), integer(1)),
    n_tracked = lengths(tc_idx),
    flags = flags,
    signoff = signoff
  )

  # the crosswalk of the master (paths relative to the project) and of a fork (file names only)
  walk_headers <- c("file", "source_qmd", "rendered_pdf", "rendered_docx")
  walk_cells <- function(basename_only) {
    walk_df <- build_docxwalk_df(sorted_docs, catalog, doc_paths = processed_docs, basename_only = basename_only)
    stats::setNames(lapply(walk_headers, function(h) review_text_cells(walk_df[[h]])), walk_headers)
  }
  docxwalk <- walk_cells(FALSE)
  docxwalk_basename <- walk_cells(TRUE)

  sum_df <- build_comment_summary_df(comments_rows)
  summary <- list(
    comment_text = review_text_cells(sum_df$comment_text),
    occurrences = sum_df$occurrences,
    files = review_text_cells(sum_df$files)
  )

  err_headers <- c("file", "error", "traceback")
  errors_cells <- stats::setNames(lapply(err_headers, function(h) review_text_cells(errors[[h]])), err_headers)

  list(
    comments = comments, redlines = redlines, revisions = revisions,
    docs = docs, docxwalk = docxwalk, docxwalk_basename = docxwalk_basename,
    summary = summary, errors = errors_cells
  )
}

#' Write Formatted Review Tracker Excel Workbook
#' @param prepared Result of \code{prepare_review_workbook_data()} for the same
#'   tables, so the master and fork workbooks of one run share it; computed here
#'   when \code{NULL}.
#' @noRd
write_review_tracker_excel <- function(columns,
                                       comments_rows,
                                       revisions_rows,
                                       processed_docs,
                                       errors,
                                       output_path,
                                       catalog,
                                       redline_columns,
                                       redlines_rows,
                                       existing_docs,
                                       fork_reviewers = NULL,
                                       signoff_reviewers = NULL,
                                       documents_sheet_mode = NULL,
                                       reviewer_view = NULL,
                                       prepared = NULL) {
  if (is.null(fork_reviewers)) fork_reviewers <- review_config()$fork_reviewers
  if (is.null(signoff_reviewers)) signoff_reviewers <- review_config()$signoff_reviewers
  if (is.null(documents_sheet_mode)) documents_sheet_mode <- review_config()$documents_sheet_mode

  if (is.null(prepared)) {
    prepared <- prepare_review_workbook_data(
      columns = columns, comments_rows = comments_rows, revisions_rows = revisions_rows,
      processed_docs = processed_docs, errors = errors, catalog = catalog,
      redline_columns = redline_columns, redlines_rows = redlines_rows,
      existing_docs = existing_docs, fork_reviewers = fork_reviewers,
      signoff_reviewers = signoff_reviewers
    )
  }

  # openxlsx would stamp the operating-system account name into the file's
  # author and last-modified-by properties, in every fork as well
  wb <- openxlsx::createWorkbook(creator = "TempleCBE")

  # A fork workbook (reviewer_view = <id>) is sent to that reviewer, so it holds
  # nothing of the other reviewers: not their columns, not the sign-off
  # reviewers' document-level status and notes, not the Word authors' names.
  fork_view <- !is.null(reviewer_view)
  if (fork_view) {
    drop_cols <- reviewer_fork_drop_columns(reviewer_view, fork_reviewers, signoff_reviewers)
    sheet_columns <- columns[!columns %in% drop_cols]
    sheet_redline_columns <- redline_columns[!redline_columns %in% drop_cols]
    # The Documents sheet's per-reviewer rollup columns, and its sign-off columns
    doc_fork_reviewers <- reviewer_view
    doc_signoff_reviewers <- character(0)
    existing_docs <- list()
  } else {
    sheet_columns <- columns
    sheet_redline_columns <- redline_columns
    doc_fork_reviewers <- fork_reviewers
    doc_signoff_reviewers <- signoff_reviewers
  }

  # Styles
  font_title_style <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "center", valign = "center", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )
  font_regular_left <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 10, halign = "left", valign = "top", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )
  font_regular_center <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 10, halign = "center", valign = "top", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )

  # Conditional formatting styles
  green_fill_style <- openxlsx::createStyle(bgFill = "#E2EFDA", fontColour = "#375623", textDecoration = "bold")
  red_fill_style <- openxlsx::createStyle(bgFill = "#FCE4D6", fontColour = "#C65911")

  header_fill_styles <- list(
    metadata = openxlsx::createStyle(fgFill = COLOR_METADATA),
    workflow = openxlsx::createStyle(fgFill = COLOR_WORKFLOW),
    resolved = openxlsx::createStyle(fgFill = COLOR_RESOLVED),
    system   = openxlsx::createStyle(fgFill = COLOR_SYSTEM),
    tracked  = openxlsx::createStyle(fgFill = COLOR_TRACKED),
    redline  = openxlsx::createStyle(fgFill = COLOR_REDLINE),
    docs     = openxlsx::createStyle(fgFill = COLOR_DOCS),
    summary  = openxlsx::createStyle(fgFill = COLOR_SUMMARY),
    errors   = openxlsx::createStyle(fgFill = COLOR_ERRORS)
  )

  # Write a table sheet's body: one column per call, one addStyle per style
  write_table_body <- function(sheet, cols, n_rows, value_of, center_of) {
    rows <- seq_len(n_rows) + 1L
    for (ci in seq_along(cols)) write_review_column(wb, sheet, ci, 2L, value_of(cols[ci]))
    centered <- unname(vapply(cols, center_of, logical(1)))
    style_review_block(wb, sheet, font_regular_center, rows, which(centered))
    style_review_block(wb, sheet, font_regular_left, rows, which(!centered))
  }

  # ---------------------------------------------------------------------------
  # Sheet 1: Comments
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Comments")
  col_map <- stats::setNames(seq_along(sheet_columns), sheet_columns)
  reviewer_col_letters <- stats::setNames(
    vapply(fork_reviewers, function(r) if (r %in% names(col_map)) openxlsx::int2col(col_map[[r]]) else NA_character_, character(1)),
    fork_reviewers
  )
  col_resolved <- if ("resolved" %in% names(col_map)) openxlsx::int2col(col_map[["resolved"]]) else NULL
  all_reviewer_ids <- c(fork_reviewers, signoff_reviewers)

  # Write Header
  write_review_header(
    wb, "Comments", sheet_columns,
    lapply(sheet_columns, function(cn) {
      if (cn %in% METADATA_COLUMNS) header_fill_styles$metadata
      else if (cn == "resolved") header_fill_styles$resolved
      else if (cn %in% workflow_columns(fork_reviewers, signoff_reviewers)) header_fill_styles$workflow
      else header_fill_styles$system
    }),
    font_title_style
  )

  n_c_rows <- nrow(comments_rows)
  if (n_c_rows > 0) {
    prep_c <- prepared$comments
    write_table_body(
      "Comments", sheet_columns, n_c_rows,
      value_of = function(cn) {
        if (cn == "resolved") {
          formulas <- build_resolved_formula(reviewer_col_letters, seq_len(n_c_rows) + 1L)
          if (!is.null(formulas)) review_formula_cells(formulas) else prep_c$resolved
        } else {
          prep_c$cells[[cn]]
        }
      },
      center_of = function(cn) cn == "resolved" || !identical(unname(prep_c$kind[cn]), "text")
    )

    # Data Validation
    for (cn in all_reviewer_ids) {
      if (cn %in% names(col_map)) {
        openxlsx::dataValidation(
          wb, "Comments", cols = col_map[[cn]], rows = 2:(n_c_rows + 1L),
          type = "list", value = '"TRUE,FALSE"'
        )
      }
    }

    # Conditional Formatting for resolved
    if (!is.null(col_resolved)) {
      res_c_idx <- col_map[["resolved"]]
      openxlsx::conditionalFormatting(
        wb, "Comments", cols = res_c_idx, rows = 2:(n_c_rows + 1L),
        rule = "TRUE", type = "contains", style = green_fill_style
      )
      openxlsx::conditionalFormatting(
        wb, "Comments", cols = res_c_idx, rows = 2:(n_c_rows + 1L),
        rule = "FALSE", type = "contains", style = red_fill_style
      )
    }
  }

  # Column widths & Hidden columns
  for (ci in seq_along(sheet_columns)) {
    cn <- sheet_columns[ci]
    is_hidden <- cn %in% COMMENTS_DEFAULT_HIDDEN_COLUMNS
    w <- if (cn %in% c("comment_text", "selected_text", "paragraph_context")) 50 else if (cn == "file") 32 else 15
    openxlsx::setColWidths(wb, "Comments", cols = ci, widths = w, hidden = is_hidden)
  }
  openxlsx::freezePane(wb, "Comments", firstActiveRow = 2, firstActiveCol = 3)

  # ---------------------------------------------------------------------------
  # Sheet 2: TrackedChanges
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "TrackedChanges")
  rev_headers <- c("file", "revision_type", "author", "date", "changed_text", "paragraph_number", "paragraph_context")
  if (fork_view) rev_headers <- setdiff(rev_headers, FORK_EXCLUDED_METADATA_COLUMNS)

  write_review_header(wb, "TrackedChanges", rev_headers, list(header_fill_styles$tracked), font_title_style)

  n_tc_rows <- nrow(revisions_rows)
  if (n_tc_rows > 0) {
    write_table_body(
      "TrackedChanges", rev_headers, n_tc_rows,
      value_of = function(h) prepared$revisions$cells[[h]],
      center_of = function(h) !h %in% c("changed_text", "paragraph_context")
    )
  }

  for (ci in seq_along(rev_headers)) {
    h <- rev_headers[ci]
    w <- if (h %in% c("changed_text", "paragraph_context")) 35 else 18
    openxlsx::setColWidths(wb, "TrackedChanges", cols = ci, widths = w)
  }
  openxlsx::freezePane(wb, "TrackedChanges", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 3: SuggestedChanges
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "SuggestedChanges")
  rl_col_map <- stats::setNames(seq_along(sheet_redline_columns), sheet_redline_columns)
  rl_reviewer_col_letters <- stats::setNames(
    vapply(fork_reviewers, function(r) if (r %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[[r]]) else NA_character_, character(1)),
    fork_reviewers
  )
  rl_col_resolved <- if ("resolved" %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[["resolved"]]) else NULL
  rl_reviewer_comment_cols <- paste0(all_reviewer_ids, "_comment")

  write_review_header(
    wb, "SuggestedChanges", sheet_redline_columns,
    lapply(sheet_redline_columns, function(cn) {
      if (cn %in% REDLINE_METADATA_COLUMNS) header_fill_styles$redline
      else if (cn == "resolved") header_fill_styles$resolved
      else if (cn %in% redline_workflow_columns(fork_reviewers, signoff_reviewers)) header_fill_styles$workflow
      else header_fill_styles$system
    }),
    font_title_style
  )

  n_rl_rows <- nrow(redlines_rows)
  if (n_rl_rows > 0) {
    prep_rl <- prepared$redlines
    write_table_body(
      "SuggestedChanges", sheet_redline_columns, n_rl_rows,
      value_of = function(cn) {
        if (cn == "resolved") {
          formulas <- build_resolved_formula(rl_reviewer_col_letters, seq_len(n_rl_rows) + 1L)
          if (!is.null(formulas)) review_formula_cells(formulas) else prep_rl$resolved
        } else {
          prep_rl$cells[[cn]]
        }
      },
      center_of = function(cn) {
        if (cn == "resolved") return(TRUE)
        kind <- unname(prep_rl$kind[cn])
        if (!identical(kind, "text")) return(TRUE)
        !cn %in% c("original_text", "accepted_text", rl_reviewer_comment_cols)
      }
    )

    # Data Validation
    for (cn in c("is_comment", all_reviewer_ids)) {
      if (cn %in% names(rl_col_map)) {
        openxlsx::dataValidation(
          wb, "SuggestedChanges", cols = rl_col_map[[cn]], rows = 2:(n_rl_rows + 1L),
          type = "list", value = '"TRUE,FALSE"'
        )
      }
    }

    # Conditional formatting
    if (!is.null(rl_col_resolved)) {
      res_rl_c_idx <- rl_col_map[["resolved"]]
      openxlsx::conditionalFormatting(
        wb, "SuggestedChanges", cols = res_rl_c_idx, rows = 2:(n_rl_rows + 1L),
        rule = "TRUE", type = "contains", style = green_fill_style
      )
      openxlsx::conditionalFormatting(
        wb, "SuggestedChanges", cols = res_rl_c_idx, rows = 2:(n_rl_rows + 1L),
        rule = "FALSE", type = "contains", style = red_fill_style
      )
    }
  }

  for (ci in seq_along(sheet_redline_columns)) {
    cn <- sheet_redline_columns[ci]
    is_hidden <- cn %in% REDLINE_DEFAULT_HIDDEN_COLUMNS
    w <- if (cn %in% c("original_text", "accepted_text", rl_reviewer_comment_cols)) 50 else if (cn == "file") 32 else 15
    openxlsx::setColWidths(wb, "SuggestedChanges", cols = ci, widths = w, hidden = is_hidden)
  }
  openxlsx::freezePane(wb, "SuggestedChanges", firstActiveRow = 2, firstActiveCol = 3)

  # ---------------------------------------------------------------------------
  # Sheet 4: Documents (+ one "Documents_<id>" sheet per reviewer when
  # documents_sheet_mode = "per_reviewer")
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Documents")

  # In a fork, nd is 1 (the fork reviewer's own rollup column), ns is 0 and
  # there are no Documents_<id> sheets, whatever documents_sheet_mode says.
  nd <- length(doc_fork_reviewers)
  ns <- length(doc_signoff_reviewers)
  per_reviewer_mode <- !fork_view && identical(documents_sheet_mode, "per_reviewer")

  if (per_reviewer_mode) {
    doc_headers <- c(
      "file", "pipeline_stage", "comment_count", "resolved_comment_count",
      "reply_count", "tracked_change_count", "resolved"
    )
    detail_cols <- integer(0)
    resolved_col <- 7L
    signoff_cols <- integer(0)
  } else {
    detail_start <- 7L
    detail_cols <- if (nd > 0) detail_start:(detail_start + nd - 1L) else integer(0)
    resolved_col <- detail_start + nd
    signoff_start <- resolved_col + 1L
    signoff_cols <- if (ns > 0) signoff_start:(signoff_start + 2L * ns - 1L) else integer(0)

    doc_headers <- c(
      "file", "pipeline_stage", "comment_count", "resolved_comment_count",
      "reply_count", "tracked_change_count",
      doc_fork_reviewers,
      "resolved",
      unlist(lapply(doc_signoff_reviewers, function(r) c(r, paste0(r, "_comment"))))
    )
  }

  write_review_header(
    wb, "Documents", doc_headers,
    lapply(doc_headers, function(h) {
      if (h %in% c("file", "pipeline_stage", "comment_count", "resolved_comment_count", "reply_count", "tracked_change_count")) header_fill_styles$docs
      else if (h == "resolved") header_fill_styles$resolved
      else header_fill_styles$workflow
    }),
    font_title_style
  )

  docs <- prepared$docs
  sorted_docs <- docs$files
  n_doc_rows <- length(sorted_docs)
  doc_rows <- seq_len(n_doc_rows) + 1L

  # Rows the rollup formulas look at, sized from the rows actually written
  c_bound <- rollup_bound(n_c_rows)
  rl_bound <- rollup_bound(n_rl_rows)

  # Per-reviewer sheets (only in "per_reviewer" mode): a fork reviewer gets a
  # read-only rollup column, a sign-off reviewer gets a status + comment pair.
  reviewer_sheet_names <- list()
  if (per_reviewer_mode) {
    for (rid in c(fork_reviewers, signoff_reviewers)) {
      sheet_name <- paste0("Documents_", rid)
      openxlsx::addWorksheet(wb, sheet_name)
      headers <- if (rid %in% fork_reviewers) c("file", "pipeline_stage", rid) else c("file", "pipeline_stage", rid, paste0(rid, "_comment"))
      write_review_header(
        wb, sheet_name, headers,
        list(header_fill_styles$docs, header_fill_styles$docs, header_fill_styles$workflow, header_fill_styles$workflow),
        font_title_style
      )
      reviewer_sheet_names[[rid]] <- sheet_name
    }
  }

  if (n_doc_rows > 0) {
    # Cols 1-2: file, pipeline_stage
    write_review_column(wb, "Documents", 1L, 2L, docs$files)
    write_review_column(wb, "Documents", 2L, 2L, docs$stage)
    style_review_block(wb, "Documents", font_regular_left, doc_rows, 1:2)

    # Col 3: comment_count
    write_review_column(wb, "Documents", 3L, 2L, docs$n_comments)

    # Col 4: resolved_comment_count (live formula)
    res_col_letter <- if (!is.null(col_resolved)) col_resolved else "J"
    formula_res_count <- sprintf('=COUNTIFS(Comments!$A:$A, A%d, Comments!$%s:$%s, TRUE)', doc_rows, res_col_letter, res_col_letter)
    write_review_column(wb, "Documents", 4L, 2L, review_formula_cells(formula_res_count))

    # Col 5: reply_count, Col 6: tracked_change_count
    write_review_column(wb, "Documents", 5L, 2L, docs$n_replies)
    write_review_column(wb, "Documents", 6L, 2L, docs$n_tracked)
    style_review_block(wb, "Documents", font_regular_center, doc_rows, 3:6)

    # Per-fork-reviewer rollup: a live formula referencing that reviewer's
    # Comments/SuggestedChanges column when found, else a plain computed
    # boolean. Written to its own "Documents" column in "columns" mode, or
    # to that reviewer's own "Documents_<id>" sheet in "per_reviewer" mode.
    # (In a fork the only reviewer on the Documents sheet is the fork's own.)
    has_letters <- vapply(doc_fork_reviewers, function(rid) {
      c_col <- reviewer_col_letters[[rid]]
      rl_col <- rl_reviewer_col_letters[[rid]]
      !is.null(c_col) && !is.na(c_col) && !is.null(rl_col) && !is.na(rl_col)
    }, logical(1))

    for (k in seq_along(doc_fork_reviewers)) {
      rid <- doc_fork_reviewers[k]
      rollup <- if (has_letters[k]) {
        cond <- rollup_conditions(reviewer_col_letters[[rid]], rl_reviewer_col_letters[[rid]], doc_rows, c_bound, rl_bound)
        review_formula_cells(sprintf("=IF(AND(%s, %s), TRUE, FALSE)", cond$comments, cond$redlines))
      } else {
        docs$flags[[rid]]
      }

      target_sheet <- if (per_reviewer_mode) reviewer_sheet_names[[rid]] else "Documents"
      target_col <- if (per_reviewer_mode) 3L else detail_cols[k]

      if (per_reviewer_mode) {
        write_review_column(wb, target_sheet, 1L, 2L, docs$files)
        write_review_column(wb, target_sheet, 2L, 2L, docs$stage)
        style_review_block(wb, target_sheet, font_regular_left, doc_rows, 1:2)
      }
      write_review_column(wb, target_sheet, target_col, 2L, rollup)
      style_review_block(wb, target_sheet, font_regular_center, doc_rows, target_col)
    }

    # Resolved: AND of every fork reviewer's rollup condition, computed
    # directly (independent of Documents-sheet layout so it works the same
    # in both documents_sheet_mode settings). In a fork the only reviewer is
    # the fork's own, so `resolved` never carries anything of the others: it
    # is that reviewer's own rollup, as on the fork's Comments sheet.
    if (nd == 0) {
      resolved_cells <- rep(FALSE, n_doc_rows)
    } else if (any(has_letters)) {
      clauses <- list()
      for (k in which(has_letters)) {
        rid <- doc_fork_reviewers[k]
        cond <- rollup_conditions(reviewer_col_letters[[rid]], rl_reviewer_col_letters[[rid]], doc_rows, c_bound, rl_bound)
        clauses <- c(clauses, list(cond$comments, cond$redlines))
      }
      bool_terms <- lapply(doc_fork_reviewers[!has_letters], function(rid) ifelse(docs$flags[[rid]], "TRUE()", "FALSE()"))
      resolved_cells <- review_formula_cells(
        sprintf("=IF(AND(%s), TRUE, FALSE)", do.call(paste, c(c(clauses, bool_terms), sep = ", ")))
      )
    } else {
      resolved_cells <- Reduce(`&`, docs$flags[doc_fork_reviewers])
    }
    write_review_column(wb, "Documents", resolved_col, 2L, resolved_cells)
    style_review_block(wb, "Documents", font_regular_center, doc_rows, resolved_col)

    # Sign-off reviewers: a document-level status + free-text comment,
    # manually entered on the Documents sheet (or its own per-reviewer
    # sheet), read back from the previous run's existing_docs.
    for (k in seq_along(doc_signoff_reviewers)) {
      rid <- doc_signoff_reviewers[k]
      target_sheet <- if (per_reviewer_mode) reviewer_sheet_names[[rid]] else "Documents"
      val_col <- if (per_reviewer_mode) 3L else signoff_cols[(k - 1L) * 2L + 1L]
      cmt_col <- if (per_reviewer_mode) 4L else signoff_cols[(k - 1L) * 2L + 2L]

      if (per_reviewer_mode) {
        write_review_column(wb, target_sheet, 1L, 2L, docs$files)
        write_review_column(wb, target_sheet, 2L, 2L, docs$stage)
        style_review_block(wb, target_sheet, font_regular_left, doc_rows, 1:2)
      }
      write_review_column(wb, target_sheet, val_col, 2L, docs$signoff[[rid]]$value)
      style_review_block(wb, target_sheet, font_regular_center, doc_rows, val_col)
      write_review_column(wb, target_sheet, cmt_col, 2L, docs$signoff[[rid]]$comment)
      style_review_block(wb, target_sheet, font_regular_left, doc_rows, cmt_col)
    }
  }

  if (n_doc_rows > 0) {
    if (per_reviewer_mode) {
      # Data validation + conditional formatting live on each reviewer's own sheet.
      for (rid in fork_reviewers) {
        sheet_name <- reviewer_sheet_names[[rid]]
        openxlsx::conditionalFormatting(wb, sheet_name, cols = 3, rows = 2:(n_doc_rows + 1L), rule = "TRUE", type = "contains", style = green_fill_style)
        openxlsx::conditionalFormatting(wb, sheet_name, cols = 3, rows = 2:(n_doc_rows + 1L), rule = "FALSE", type = "contains", style = red_fill_style)
      }
      for (rid in signoff_reviewers) {
        sheet_name <- reviewer_sheet_names[[rid]]
        openxlsx::dataValidation(wb, sheet_name, cols = 3, rows = 2:(n_doc_rows + 1L), type = "list", value = '"TRUE,FALSE"')
        openxlsx::conditionalFormatting(wb, sheet_name, cols = 3, rows = 2:(n_doc_rows + 1L), rule = "TRUE", type = "contains", style = green_fill_style)
        openxlsx::conditionalFormatting(wb, sheet_name, cols = 3, rows = 2:(n_doc_rows + 1L), rule = "FALSE", type = "contains", style = red_fill_style)
      }
      openxlsx::conditionalFormatting(wb, "Documents", cols = resolved_col, rows = 2:(n_doc_rows + 1L), rule = "TRUE", type = "contains", style = green_fill_style)
      openxlsx::conditionalFormatting(wb, "Documents", cols = resolved_col, rows = 2:(n_doc_rows + 1L), rule = "FALSE", type = "contains", style = red_fill_style)
    } else {
      # Data validation on each sign-off reviewer's status column
      if (ns > 0) {
        signoff_val_cols <- signoff_cols[seq(1L, length(signoff_cols), by = 2L)]
        for (ci in signoff_val_cols) {
          openxlsx::dataValidation(wb, "Documents", cols = ci, rows = 2:(n_doc_rows + 1L), type = "list", value = '"TRUE,FALSE"')
        }
      }
      # Conditional formatting on the fork-reviewer rollup columns + resolved
      for (ci in c(detail_cols, resolved_col)) {
        openxlsx::conditionalFormatting(wb, "Documents", cols = ci, rows = 2:(n_doc_rows + 1L), rule = "TRUE", type = "contains", style = green_fill_style)
        openxlsx::conditionalFormatting(wb, "Documents", cols = ci, rows = 2:(n_doc_rows + 1L), rule = "FALSE", type = "contains", style = red_fill_style)
      }
    }
  }

  openxlsx::setColWidths(wb, "Documents", cols = 1:2, widths = 40)
  openxlsx::setColWidths(wb, "Documents", cols = 3:6, widths = 22)
  if (per_reviewer_mode) {
    openxlsx::setColWidths(wb, "Documents", cols = resolved_col, widths = 14)
    for (rid in fork_reviewers) {
      openxlsx::setColWidths(wb, reviewer_sheet_names[[rid]], cols = 1:2, widths = 40)
      openxlsx::setColWidths(wb, reviewer_sheet_names[[rid]], cols = 3, widths = 14)
      openxlsx::freezePane(wb, reviewer_sheet_names[[rid]], firstActiveRow = 2, firstActiveCol = 1)
    }
    for (rid in signoff_reviewers) {
      openxlsx::setColWidths(wb, reviewer_sheet_names[[rid]], cols = 1:2, widths = 40)
      openxlsx::setColWidths(wb, reviewer_sheet_names[[rid]], cols = 3, widths = 14)
      openxlsx::setColWidths(wb, reviewer_sheet_names[[rid]], cols = 4, widths = 40)
      openxlsx::freezePane(wb, reviewer_sheet_names[[rid]], firstActiveRow = 2, firstActiveCol = 1)
    }
  } else {
    if (nd > 0) openxlsx::setColWidths(wb, "Documents", cols = detail_cols, widths = 12)
    openxlsx::setColWidths(wb, "Documents", cols = resolved_col, widths = 14)
    if (ns > 0) {
      signoff_val_cols <- signoff_cols[seq(1L, length(signoff_cols), by = 2L)]
      signoff_cmt_cols <- signoff_cols[seq(2L, length(signoff_cols), by = 2L)]
      openxlsx::setColWidths(wb, "Documents", cols = signoff_val_cols, widths = 14)
      openxlsx::setColWidths(wb, "Documents", cols = signoff_cmt_cols, widths = 40)
    }
  }
  openxlsx::freezePane(wb, "Documents", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 5: docXwalk
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "docXwalk")
  walk_headers <- c("file", "source_qmd", "rendered_pdf", "rendered_docx")
  write_review_header(wb, "docXwalk", walk_headers, list(header_fill_styles$docs), font_title_style)
  # A fork lists file names only: nothing of the project's folder layout leaves the master
  walk <- if (fork_view) prepared$docxwalk_basename else prepared$docxwalk
  n_walk <- length(walk$file)
  for (ci in seq_along(walk_headers)) write_review_column(wb, "docXwalk", ci, 2L, walk[[walk_headers[ci]]])
  style_review_block(wb, "docXwalk", font_regular_left, seq_len(n_walk) + 1L, seq_along(walk_headers))
  openxlsx::setColWidths(wb, "docXwalk", cols = 1:4, widths = 65)
  openxlsx::freezePane(wb, "docXwalk", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 6: CommentSummary
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "CommentSummary")
  sum_headers <- c("comment_text", "occurrences", "files")
  write_review_header(wb, "CommentSummary", sum_headers, list(header_fill_styles$summary), font_title_style)
  n_sum <- length(prepared$summary$comment_text)
  sum_rows <- seq_len(n_sum) + 1L
  write_review_column(wb, "CommentSummary", 1L, 2L, prepared$summary$comment_text)
  write_review_column(wb, "CommentSummary", 2L, 2L, prepared$summary$occurrences)
  write_review_column(wb, "CommentSummary", 3L, 2L, prepared$summary$files)
  style_review_block(wb, "CommentSummary", font_regular_left, sum_rows, c(1L, 3L))
  style_review_block(wb, "CommentSummary", font_regular_center, sum_rows, 2L)
  openxlsx::setColWidths(wb, "CommentSummary", cols = 1, widths = 50)
  openxlsx::setColWidths(wb, "CommentSummary", cols = 2, widths = 14)
  openxlsx::setColWidths(wb, "CommentSummary", cols = 3, widths = 40)
  openxlsx::freezePane(wb, "CommentSummary", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 7: Errors (if any)
  # ---------------------------------------------------------------------------
  if (nrow(errors) > 0) {
    openxlsx::addWorksheet(wb, "Errors")
    err_headers <- c("file", "error", "traceback")
    write_review_header(wb, "Errors", err_headers, list(header_fill_styles$errors), font_title_style)
    for (ci in seq_along(err_headers)) write_review_column(wb, "Errors", ci, 2L, prepared$errors[[err_headers[ci]]])
    style_review_block(wb, "Errors", font_regular_left, seq_len(nrow(errors)) + 1L, seq_along(err_headers))
    openxlsx::setColWidths(wb, "Errors", cols = 1:3, widths = 40)
    openxlsx::freezePane(wb, "Errors", firstActiveRow = 2, firstActiveCol = 1)
  }

  # ---------------------------------------------------------------------------
  # Hidden sheet of a fork: the reviewer's cells as generated, so that the next
  # run can tell the reviewer's edits from edits made in the master workbook
  # ---------------------------------------------------------------------------
  if (!is.null(reviewer_view)) {
    openxlsx::addWorksheet(wb, FORK_BASELINE_SHEET)
    openxlsx::writeData(wb, FORK_BASELINE_SHEET, build_fork_baseline(reviewer_view, comments_rows, redlines_rows))
    visibility <- openxlsx::sheetVisibility(wb)
    visibility[names(wb) == FORK_BASELINE_SHEET] <- "veryHidden"
    openxlsx::sheetVisibility(wb) <- visibility
  }

  # Order sheets according to preferred order
  preferred_order <- c("Comments", "SuggestedChanges", "Documents", "docXwalk", "TrackedChanges", "CommentSummary", "Errors", FORK_BASELINE_SHEET)
  present_sheets <- names(wb)
  final_order <- preferred_order[preferred_order %in% present_sheets]
  if (length(final_order) == length(present_sheets)) {
    openxlsx::worksheetOrder(wb) <- match(final_order, present_sheets)
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}

# =============================================================================
# SUMMARY TABLE HELPERS & CSV EXPORT
# =============================================================================

#' Build Documents Summary Tibble
#' @param fork_reviewers Character vector of fork-reviewer identifiers; defaults to \code{\link{review_config}}.
#' @param signoff_reviewers Character vector of sign-off reviewer identifiers; defaults to \code{\link{review_config}}.
#' @noRd
build_documents_summary_df <- function(processed_docs, comments_rows, revisions_rows, redlines_rows, catalog, existing_docs,
                                       fork_reviewers = NULL, signoff_reviewers = NULL) {
  if (is.null(fork_reviewers)) fork_reviewers <- review_config()$fork_reviewers
  if (is.null(signoff_reviewers)) signoff_reviewers <- review_config()$signoff_reviewers

  all_files <- unique(c(basename(processed_docs), comments_rows$file))
  all_files <- all_files[!is.na(all_files) & nzchar(all_files)]
  ranks <- vapply(all_files, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  sorted_files <- all_files[order(ranks, all_files, method = "radix")]

  doc_map <- if (!is.null(existing_docs)) existing_docs else list()

  rows <- lapply(sorted_files, function(f) {
    fc <- comments_rows[comments_rows$file == f, ]
    fr <- revisions_rows[revisions_rows$file == f, ]
    frl <- redlines_rows[redlines_rows$file == f, ]
    freplies <- if ("is_reply" %in% names(fc)) fc[is_review_true_vec(fc$is_reply), ] else fc[0, ]
    stage <- match_docx_to_pipeline(f, catalog)$stage

    dfb <- if (f %in% names(doc_map)) doc_map[[f]] else list()

    reviewer_vals <- stats::setNames(
      lapply(fork_reviewers, function(r) {
        if (r %in% names(fc) && nrow(fc) > 0) all(vapply(fc[[r]], is_review_true, logical(1))) else FALSE
      }),
      fork_reviewers
    )
    resolved_val <- if (length(fork_reviewers) == 0) FALSE else all(unlist(reviewer_vals))

    resolved_count <- if ("resolved" %in% names(fc)) sum(vapply(fc$resolved, is_review_true, logical(1))) else 0L

    signoff_vals <- list()
    for (r in signoff_reviewers) {
      raw_val <- dfb[[r]]
      raw_cmt <- dfb[[paste0(r, "_comment")]]
      signoff_vals[[r]] <- if (!is.null(raw_val) && !is.na(raw_val)) is_review_true(raw_val) else NA
      signoff_vals[[paste0(r, "_comment")]] <- if (!is.null(raw_cmt) && !is.na(raw_cmt)) as.character(raw_cmt) else ""
    }

    tibble::as_tibble(c(
      list(
        file = f,
        pipeline_stage = stage,
        comment_count = nrow(fc),
        resolved_comment_count = resolved_count,
        reply_count = nrow(freplies),
        tracked_change_count = nrow(fr)
      ),
      reviewer_vals,
      list(resolved = resolved_val),
      signoff_vals
    ))
  })

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble()
}

#' Build Comment Summary Tibble
#' @noRd
build_comment_summary_df <- function(comments_rows) {
  if (nrow(comments_rows) == 0 || !"comment_text" %in% names(comments_rows)) {
    return(tibble::tibble(comment_text = character(0), occurrences = integer(0), files = character(0)))
  }
  txt <- clean_review_text(comments_rows$comment_text)
  fn <- trimws(as.character(comments_rows$file))
  keep <- nzchar(txt)
  if (!any(keep)) {
    return(tibble::tibble(comment_text = character(0), occurrences = integer(0), files = character(0)))
  }

  # one group per distinct text, in order of first appearance; most frequent first,
  # ties keeping that order (order() is stable)
  distinct <- unique(txt[keep])
  group <- match(txt[keep], distinct)
  counts <- tabulate(group, nbins = length(distinct))
  ord <- order(counts, decreasing = TRUE)
  files_by_group <- split(fn[keep], factor(group, levels = seq_along(distinct)))

  tibble::tibble(
    comment_text = distinct[ord],
    occurrences = counts[ord],
    files = vapply(files_by_group[ord], function(f) paste(sort(unique(f), method = "radix"), collapse = "; "), character(1), USE.NAMES = FALSE)
  )
}

#' Compute a "resolved" vector by ANDing each fork reviewer's column per row
#' @param df Data frame containing one column per id in \code{fork_reviewers}.
#' @param fork_reviewers Character vector of fork-reviewer identifiers.
#' @return Logical vector, one entry per row of \code{df}.
#' @noRd
compute_resolved_vector <- function(df, fork_reviewers) {
  n <- nrow(df)
  if (n == 0) return(logical(0))
  if (length(fork_reviewers) == 0) return(rep(FALSE, n))
  per_reviewer <- lapply(fork_reviewers, function(r) vapply(df[[r]], is_review_true, logical(1)))
  Reduce(`&`, per_reviewer)
}

#' Neutralise Cells a Spreadsheet Would Run as a Formula
#'
#' Comment, selected and tracked text and the reviewers' own free-text columns
#' are attacker-controlled, and Excel evaluates a CSV cell that starts with
#' \code{=}, \code{+}, \code{-} or \code{@} (after any leading blanks), or with a
#' tab or a carriage return, as a formula (hyperlinks, data-exfiltrating
#' functions, DDE in older builds). Such a cell gets a leading single quote,
#' which Excel treats as a text marker.
#' @param x Character vector (other types are returned unchanged).
#' @return \code{x} with the risky cells prefixed by a single quote.
#' @noRd
neutralise_csv_formulas <- function(x) {
  if (!is.character(x)) return(x)
  risky <- !is.na(x) & (grepl("^[=+@-]", trimws(x)) | grepl("^[\t\r]", x))
  x[risky] <- paste0("'", x[risky])
  x
}

#' Write a Review Table to CSV with Formula Neutralisation
#' @param df Data frame; every character (and factor) column is neutralised.
#' @param path Output CSV path.
#' @noRd
write_review_csv <- function(df, path) {
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) col <- as.character(col)
    neutralise_csv_formulas(col)
  })
  utils::write.csv(df, file = path, row.names = FALSE, fileEncoding = "UTF-8")
}

#' Export Tabular Data to CSVs
#'
#' The CSVs are loose files that travel further than the fork workbooks, so they
#' hold reviewer-identifying columns only as far as \code{reviewer_columns} says.
#' @param fork_reviewers Character vector of fork-reviewer identifiers; defaults to \code{\link{review_config}}.
#' @param signoff_reviewers Character vector of sign-off reviewer identifiers; defaults to \code{\link{review_config}}.
#' @param reviewer_columns Which reviewer columns to write. \code{"fork"} (the
#'   default of \code{\link{review_config}}) keeps the fork reviewers' sign-off and comment
#'   columns and drops the sign-off reviewers' columns and the Word \code{author};
#'   \code{"none"} drops every reviewer column (the \code{resolved} flag stays) and the
#'   Word \code{author}; \code{"all"} writes every column, as the CSVs did before.
#' @noRd
export_review_csvs <- function(output_dir, columns, comments_rows, revisions_rows, redline_columns, redlines_rows,
                               fork_reviewers = NULL, signoff_reviewers = NULL,
                               reviewer_columns = c("fork", "all", "none")) {
  if (is.null(fork_reviewers)) fork_reviewers <- review_config()$fork_reviewers
  if (is.null(signoff_reviewers)) signoff_reviewers <- review_config()$signoff_reviewers
  reviewer_columns <- match.arg(reviewer_columns)

  with_comment <- function(ids) c(ids, paste0(ids, "_comment"))
  omit <- switch(
    reviewer_columns,
    all = character(0),
    fork = c(with_comment(signoff_reviewers), FORK_EXCLUDED_METADATA_COLUMNS),
    none = c(with_comment(fork_reviewers), with_comment(signoff_reviewers), FORK_EXCLUDED_METADATA_COLUMNS)
  )
  keep_columns <- function(df, wanted = names(df)) {
    df[, setdiff(intersect(wanted, names(df)), omit), drop = FALSE]
  }

  c_csv <- file.path(output_dir, "comments.csv")
  r_csv <- file.path(output_dir, "tracked_changes.csv")
  rl_csv <- file.path(output_dir, "suggested_changes.csv")

  # Clean comments for CSV (`resolved` is computed before the reviewer columns it needs are dropped)
  c_df <- comments_rows
  if (nrow(c_df) > 0) {
    c_df$resolved <- compute_resolved_vector(c_df, fork_reviewers)
    write_review_csv(keep_columns(c_df, columns), c_csv)
  } else {
    write_review_csv(keep_columns(c_df), c_csv)
  }

  # Revisions
  write_review_csv(keep_columns(revisions_rows), r_csv)

  # Suggested Changes / Redlines
  rl_df <- redlines_rows
  if (nrow(rl_df) > 0) {
    rl_df$resolved <- compute_resolved_vector(rl_df, fork_reviewers)
    write_review_csv(keep_columns(rl_df, redline_columns), rl_csv)
  } else {
    write_review_csv(keep_columns(rl_df), rl_csv)
  }

  list(
    comments_csv = c_csv,
    tracked_changes_csv = r_csv,
    suggested_changes_csv = rl_csv
  )
}
