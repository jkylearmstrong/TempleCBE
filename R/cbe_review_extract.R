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
#' @param input_dir Character path to a directory containing `.docx` files, or to
#'   a single `.docx` file. Defaults to finding an edits directory or the current working directory.
#' @param output_dir Character path to the directory where review tracker workbooks
#'   and CSV files should be saved. If `NULL` (default), uses `<input_dir>/review_extract`
#'   (or `<input_dir>` if it is already named `review_extract`).
#' @param tracker Character filename for the master Excel workbook. Defaults to
#'   `"review_tracker.xlsx"`.
#' @param min_revision_length Integer minimum character count required to record a
#'   tracked change. Defaults to `2`.
#' @param fork_reviewers Character vector of reviewer identifiers who receive
#'   their own single-reviewer fork workbooks. Defaults to `c("jka", "darina", "zhao")`.
#' @param manifest_path Optional character path to `reports_to_render.xlsx`. If `NULL`,
#'   the function attempts to locate it automatically in standard repository paths.
#' @param verbose Logical indicating whether to print detailed progress messages.
#'   Defaults to `FALSE`.
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
#' res <- cbe_docx_review_extract("tasks/edits", verbose = TRUE)
#' print(res)
#' }
cbe_docx_review_extract <- function(input_dir = NULL,
                                   output_dir = NULL,
                                   tracker = "review_tracker.xlsx",
                                   min_revision_length = 2,
                                   fork_reviewers = c("jka", "darina", "zhao"),
                                   manifest_path = NULL,
                                   verbose = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for cbe_docx_review_extract(). Please install it.", call. = FALSE)
  }

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
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  master_tracker_path <- file.path(output_dir, tracker)

  # Backup existing master tracker before touching it
  backup_review_file(master_tracker_path)

  # 2. Discover DOCX files
  if (is_single_file) {
    all_docx <- input_dir
  } else {
    all_docx <- sort(list.files(scan_dir, pattern = "\\.docx$", full.names = TRUE, ignore.case = TRUE))
  }

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

  valid_mask <- vapply(all_docx, is_valid_docx, logical(1))
  valid_docx <- all_docx[valid_mask]
  invalid_docx <- all_docx[!valid_mask]

  if (length(invalid_docx) > 0 && verbose) {
    message(sprintf("Skipping %d corrupted or non-docx file(s): %s",
                    length(invalid_docx), paste(basename(invalid_docx), collapse = ", ")))
  }

  if (length(valid_docx) == 0) {
    warning("No valid DOCX files found to process.")
    return(structure(
      list(
        comments = tibble::tibble(),
        revisions = tibble::tibble(),
        redlines = tibble::tibble(),
        documents = tibble::tibble(),
        docxwalk = tibble::tibble(),
        summary = tibble::tibble(),
        errors = tibble::tibble(file = basename(invalid_docx), error = "Invalid or corrupted DOCX", traceback = ""),
        paths = list(master = master_tracker_path)
      ),
      class = "cbe_review_extract"
    ))
  }

  # 3. Pipeline catalog and compute graph order
  if (is.null(manifest_path)) {
    manifest_path <- find_pipeline_manifest(scan_dir)
  }
  catalog <- load_pipeline_catalog(manifest_path)
  if (verbose) {
    if (!is.null(manifest_path) && file.exists(manifest_path)) {
      message(sprintf("Using pipeline manifest: %s", manifest_path))
    } else {
      message(sprintf("Using built-in fallback pipeline catalog (%d stages).", length(catalog)))
    }
  }

  # Sort docx files by pipeline rank
  docx_ranks <- vapply(basename(valid_docx), function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  valid_docx <- valid_docx[order(docx_ranks, basename(valid_docx))]

  # 4. Load existing tracker data (if any)
  existing_data <- load_existing_tracker(master_tracker_path)
  existing_headers <- existing_data$headers
  existing_comments <- existing_data$comments
  existing_revisions <- existing_data$revisions
  existing_redlines <- existing_data$redlines
  existing_redline_headers <- existing_data$redline_headers
  existing_docs <- existing_data$documents

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

  for (doc in valid_docx) {
    doc_name <- basename(doc)
    res <- tryCatch({
      extract_from_docx(doc, min_revision_length = min_revision_length)
    }, error = function(e) {
      if (verbose) message(sprintf("Failed processing %s: %s", doc_name, conditionMessage(e)))
      errors_list[[length(errors_list) + 1]] <<- tibble::tibble(
        file = doc_name,
        error = conditionMessage(e),
        traceback = paste(conditionCall(e), collapse = "\n")
      )
      NULL
    })

    if (!is.null(res)) {
      if (nrow(res$comments) > 0) incoming_comments_list[[length(incoming_comments_list) + 1]] <- res$comments
      if (nrow(res$revisions) > 0) incoming_revisions_list[[length(incoming_revisions_list) + 1]] <- res$revisions
      if (nrow(res$redlines) > 0) incoming_redlines_list[[length(incoming_redlines_list) + 1]] <- res$redlines

      if (verbose) {
        message(sprintf("Processed %s: %d comments, %d revisions, %d redlined paragraphs",
                        doc_name, nrow(res$comments), nrow(res$revisions), nrow(res$redlines)))
      }
    }
  }

  incoming_comments <- if (length(incoming_comments_list) > 0) dplyr::bind_rows(incoming_comments_list) else tibble::tibble()
  incoming_revisions <- if (length(incoming_revisions_list) > 0) dplyr::bind_rows(incoming_revisions_list) else tibble::tibble()
  incoming_redlines <- if (length(incoming_redlines_list) > 0) dplyr::bind_rows(incoming_redlines_list) else tibble::tibble()
  errors_df <- if (length(errors_list) > 0) dplyr::bind_rows(errors_list) else tibble::tibble(file = character(0), error = character(0), traceback = character(0))

  scanned_files <- unique(basename(valid_docx))

  # 6. Non-destructively merge incoming with existing records
  merged_c <- merge_comments(
    incoming_comments = incoming_comments,
    existing_rows = existing_comments,
    existing_headers = existing_headers,
    catalog = catalog,
    scanned_files = scanned_files
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
    scanned_files = scanned_files
  )
  redline_columns <- merged_rl$columns
  merged_redlines <- merged_rl$rows

  # 7. Generate Excel Master Workbook
  write_review_tracker_excel(
    columns = comments_columns,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    processed_docs = valid_docx,
    errors = errors_df,
    output_path = master_tracker_path,
    catalog = catalog,
    redline_columns = redline_columns,
    redlines_rows = merged_redlines,
    existing_docs = existing_docs,
    fork_reviewers = fork_reviewers,
    reviewer_view = NULL
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
      processed_docs = valid_docx,
      errors = errors_df,
      output_path = fork_path,
      catalog = catalog,
      redline_columns = redline_columns,
      redlines_rows = merged_redlines,
      existing_docs = existing_docs,
      fork_reviewers = fork_reviewers,
      reviewer_view = rev
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
    redlines_rows = merged_redlines
  )

  # 9. Build return tables
  doc_summary <- build_documents_summary_df(
    processed_docs = valid_docx,
    comments_rows = merged_comments,
    revisions_rows = merged_revisions,
    redlines_rows = merged_redlines,
    catalog = catalog,
    existing_docs = existing_docs
  )

  docxwalk_df <- build_docxwalk_df(
    files = sort(unique(c(basename(valid_docx), merged_comments$file))),
    catalog = catalog
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
# CONSTANTS & METADATA
# =============================================================================

W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
XML_NAMESPACES <- c(w = W_NS)

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

WORKFLOW_COLUMNS <- c(
  "resolved",
  "jka",
  "jka_comment",
  "darina",
  "darina_comment",
  "zhao",
  "zhao_comment",
  "wolfson",
  "wolfson_comment"
)

SYSTEM_COLUMNS <- c(
  "doc_status",
  "pipeline_stage",
  "is_reply",
  "reply_to_id",
  "duplicate_count"
)

CANONICAL_COLUMNS <- c(METADATA_COLUMNS, WORKFLOW_COLUMNS, SYSTEM_COLUMNS)

REDLINE_METADATA_COLUMNS <- c(
  "file",
  "paragraph_number",
  "author",
  "date",
  "original_text",
  "accepted_text",
  "is_toc_or_lof"
)

REDLINE_WORKFLOW_COLUMNS <- c(
  "is_comment",
  "resolved",
  "jka",
  "jka_comment",
  "darina",
  "darina_comment",
  "zhao",
  "zhao_comment",
  "wolfson",
  "wolfson_comment"
)

REDLINE_SYSTEM_COLUMNS <- c("doc_status", "pipeline_stage")

REDLINE_CANONICAL_COLUMNS <- c(REDLINE_METADATA_COLUMNS, REDLINE_WORKFLOW_COLUMNS, REDLINE_SYSTEM_COLUMNS)

COMMENTS_DEFAULT_HIDDEN_COLUMNS <- c(
  "comment_id", "author", "date", "selected_text", "paragraph_number",
  "end_paragraph_number", "paragraph_context",
  "doc_status", "pipeline_stage", "is_reply", "reply_to_id", "duplicate_count"
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

KNOWN_STEM_ALIASES <- c(
  "miss_data_imputat_non_repeat" = "missing_data_imputation_non_repeat",
  "miss_data_imputat_5_plus" = "missing_data_imputation_5_plus",
  "missing_data_imput_time_5" = "missing_data_imputation_time_5",
  "missing_input_vars_imputati" = "missing_input_vars_imputation"
)

DEFAULT_PIPELINE_CATALOG <- list(
  list(stem = "intro", heading = 1, name = "Introduction", stage = "Heading 1: Introduction"),
  list(stem = "eda", heading = 2, name = "Exploratory Data Analysis", stage = "Heading 2: Exploratory Data Analysis"),
  list(stem = "single_var_mixed_models", heading = 2, name = "Initial Univariate GLM and Mixed Models", stage = "Heading 2: Initial Univariate GLM and Mixed Models"),
  list(stem = "raw_index", heading = 2, name = "Compute Index", stage = "Heading 2: Compute Index"),
  list(stem = "pi_index_select", heading = 2, name = "PI Index as a GLM function of selected inputs", stage = "Heading 2: PI Index as a GLM function of selected inputs"),
  list(stem = "index_select", heading = 2, name = "Indexes as a GLM function of selected inputs", stage = "Heading 2: Indexes as a GLM function of selected inputs"),
  list(stem = "missing_input_vars_imputation", heading = 3, name = "Missing Input Variables Imputation", stage = "Heading 3: Missing Input Variables Imputation"),
  list(stem = "missing_data_imputation_non_repeat", heading = 3, name = "Missing Data Imputation (Non-Repeated)", stage = "Heading 3: Missing Data Imputation (Non-Repeated)"),
  list(stem = "missing_data_imputation_time_5", heading = 3, name = "Missing Data Imputation (Time <= 5)", stage = "Heading 3: Missing Data Imputation (Time <= 5)"),
  list(stem = "missing_data_imputation_5_plus", heading = 3, name = "Missing Data Imputation (Time > 5)", stage = "Heading 3: Missing Data Imputation (Time > 5)"),
  list(stem = "combine_imputes", heading = 3, name = "Combine Imputations", stage = "Heading 3: Combine Imputations"),
  list(stem = "eda_impute", heading = 4, name = "Exploratory Data Analysis (Original + Imputed)", stage = "Heading 4: Exploratory Data Analysis (Original + Imputed)"),
  list(stem = "single_var_mixed_models_impute", heading = 4, name = "Univariate GLM and Mixed Models", stage = "Heading 4: Univariate GLM and Mixed Models"),
  list(stem = "index_impute", heading = 4, name = "Compute Index (Original + Imputed)", stage = "Heading 4: Compute Index (Original + Imputed)"),
  list(stem = "imp_boot_survival", heading = 4, name = "Missing Input Parameters on Survival", stage = "Heading 4: Missing Input Parameters on Survival"),
  list(stem = "relation_inputs_2_pi_outputs_uni", heading = 5, name = "Univariate", stage = "Heading 5: Univariate"),
  list(stem = "relation_inputs_2_pi_outputs", heading = 5, name = "Multivariate (bootstraped)", stage = "Heading 5: Multivariate (bootstraped)"),
  list(stem = "relation_inputs_2_pi_outputs_pca", heading = 5, name = "Multivariate (PCA)", stage = "Heading 5: Multivariate (PCA)"),
  list(stem = "relation_inputs_2_pi_outputs_pick6", heading = 5, name = "Multivariate (Multimodel Approach)", stage = "Heading 5: Multivariate (Multimodel Approach)"),
  list(stem = "pi_index_select_impute", heading = 6, name = "PI Index as a GLM function of selected inputs (Original + Imputed)", stage = "Heading 6: PI Index GLM (Original + Imputed)"),
  list(stem = "index_select_impute", heading = 6, name = "Indexes as a GLM function of selected inputs (Original + Imputed)", stage = "Heading 6: Indexes GLM (Original + Imputed)"),
  list(stem = "survival", heading = 7, name = "Survival Analysis on Input Parameters", stage = "Heading 7: Survival Analysis on Input Parameters"),
  list(stem = "index_survival", heading = 7, name = "Optimal Cutpoints and Univariate Cox Models For Indexes as Predictors of Survival", stage = "Heading 7: Optimal Cutpoints and Univariate Cox Models"),
  list(stem = "inputs_mixed_models_abg", heading = 8, name = "Mixed Models for Input Parameters (ABG)", stage = "Heading 8: Mixed Models for Input Parameters (ABG)"),
  list(stem = "inputs_mixed_models_cardiovascular", heading = 8, name = "Mixed Models for Input Parameters (Cardiovascular)", stage = "Heading 8: Mixed Models for Input Parameters (Cardio)"),
  list(stem = "inputs_mixed_models_pulmonary", heading = 8, name = "Mixed Models for Input Parameters (Pulmonary)", stage = "Heading 8: Mixed Models for Input Parameters (Pulmonary)"),
  list(stem = "index_cutpoints", heading = 8, name = "Cutpoints for Indexes", stage = "Heading 8: Cutpoints for Indexes"),
  list(stem = "output_cutpoints", heading = 8, name = "Cutpoints for Output parameters", stage = "Heading 8: Cutpoints for Output parameters"),
  list(stem = "norm_uni_cox_estimates", heading = 9, name = "Normalized Univariate Cox Estimates for Output Parameters", stage = "Heading 9: Normalized Univariate Cox Estimates"),
  list(stem = "surv_multi_cox_estimates_by_domain", heading = 9, name = "Normalized Multivariate Cox Estimates for Output Parameters by Domain", stage = "Heading 9: Normalized Multivariate Cox Estimates by Domain"),
  list(stem = "surv_multi_cox_estimates", heading = 9, name = "Normalized Multivariate Cox Estimates for Output Parameters", stage = "Heading 9: Normalized Multivariate Cox Estimates"),
  list(stem = "surv_multi_cox_estimates_w_inputs", heading = 9, name = "Survival Multi Cox Estimates w/ Inputs", stage = "Heading 9: Survival Multi Cox Estimates w/ Inputs"),
  list(stem = "compare_original_impute_index", heading = 10, name = "Compare Index (Original VS Original + Imputed)", stage = "Heading 10: Compare Index"),
  list(stem = "badly_injured_univariate", heading = 12, name = "Badly Injured Univariate", stage = "Heading 12: Badly Injured Univariate"),
  list(stem = "badly_injured_multi_estimates_by_domain", heading = 12, name = "Badly Injured Multi Estimates By Domain", stage = "Heading 12: Badly Injured Multi Estimates By Domain"),
  list(stem = "badly_injured_multi_estimates_w_inputs", heading = 12, name = "Badly Injured Multi Estimates w/ Inputs", stage = "Heading 12: Badly Injured Multi Estimates w/ Inputs"),
  list(stem = "data_sci_intro", heading = 13, name = "Introduction to Data Science", stage = "Heading 13: Walkthroughs (Introduction to Data Science)"),
  list(stem = "index_intro", heading = 13, name = "Theoretical Overview of Index Construction", stage = "Heading 13: Walkthroughs (Index Construction Overview)")
)

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
#' @return Clean lowercased stem string
#' @noRd
extract_docx_stem <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  stem <- sub("_?\\d{1,2}_\\d{1,2}_\\d{2,4}$", "", stem)
  stem <- sub("_([A-Z]{2,4}|[a-z]{2,3}|zhao)$", "", stem)
  stem <- sub("_+$", "", stem)
  tolower(stem)
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

#' Find Pipeline Manifest File
#' @param start_dir Search directory
#' @return File path or NULL
#' @noRd
find_pipeline_manifest <- function(start_dir = NULL) {
  candidates <- character(0)
  if (!is.null(start_dir) && nzchar(start_dir)) {
    p1 <- file.path(start_dir, "analysis", "2024_09", "reports_to_render.xlsx")
    p2 <- file.path(dirname(start_dir), "analysis", "2024_09", "reports_to_render.xlsx")
    p3 <- file.path(dirname(dirname(start_dir)), "analysis", "2024_09", "reports_to_render.xlsx")
    candidates <- c(candidates, p1, p2, p3)
  }
  candidates <- c(
    candidates,
    file.path(getwd(), "analysis", "2024_09", "reports_to_render.xlsx"),
    file.path("S:", "Clinical Science", "Staff", "Wolfson", "analysis", "2024_09", "reports_to_render.xlsx")
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
  }
  NULL
}

#' Load Pipeline Compute Graph Catalog
#' @param manifest_path Optional path to manifest file
#' @return Named list of stages
#' @noRd
load_pipeline_catalog <- function(manifest_path = NULL) {
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

  # Augment with default catalog
  for (item in DEFAULT_PIPELINE_CATALOG) {
    stem <- item$stem
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
#' @return List with rank, matched_stem, and stage
#' @noRd
match_docx_to_pipeline <- function(filename, catalog) {
  raw_stem <- extract_docx_stem(filename)
  stem <- if (raw_stem %in% names(KNOWN_STEM_ALIASES)) KNOWN_STEM_ALIASES[[raw_stem]] else raw_stem

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
#' @param search_root Directory to search for .qmd files
#' @return Named character vector mapping lowercased stems to file paths
#' @noRd
build_qmd_index <- function(search_root = NULL) {
  if (is.null(search_root)) {
    search_root <- file.path(getwd(), "analysis", "2024_09")
  }
  index <- character(0)
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
#' @param path File path
#' @return Formatted character path with backslashes
#' @noRd
format_repo_path <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  norm_path <- normalizePath(path, mustWork = FALSE)
  repo_root <- tryCatch(here::here(), error = function(e) getwd())
  norm_root <- normalizePath(repo_root, mustWork = FALSE)

  if (startsWith(norm_path, norm_root)) {
    rel <- substring(norm_path, nchar(norm_root) + 2)
    paste0(basename(norm_root), "\\", gsub("/", "\\\\", rel))
  } else {
    gsub("/", "\\\\", path)
  }
}

#' Crosswalk DOCX to Pipeline Source & Rendered Outputs
#' @param files Vector of DOCX filenames
#' @param catalog Pipeline catalog
#' @param qmd_index Optional QMD index
#' @return Tibble of crosswalk rows
#' @noRd
build_docxwalk_df <- function(files, catalog, qmd_index = NULL) {
  if (is.null(qmd_index)) {
    qmd_index <- build_qmd_index()
  }

  rows <- lapply(files, function(fname) {
    matched <- match_docx_to_pipeline(fname, catalog)
    matched_stem <- matched$matched_stem
    qmd_path <- if (!is.null(matched_stem) && matched_stem %in% names(qmd_index)) qmd_index[[matched_stem]] else NULL

    source_qmd <- if (!is.null(qmd_path)) format_repo_path(qmd_path) else NA_character_
    rendered_pdf <- NA_character_
    rendered_docx <- NA_character_

    if (!is.null(qmd_path) && file.exists(qmd_path)) {
      pdf_cand <- sub("\\.qmd$", ".pdf", qmd_path, ignore.case = TRUE)
      if (file.exists(pdf_cand)) rendered_pdf <- format_repo_path(pdf_cand)
      docx_cand <- sub("\\.qmd$", ".docx", qmd_path, ignore.case = TRUE)
      if (file.exists(docx_cand)) rendered_docx <- format_repo_path(docx_cand)
    }

    tibble::tibble(
      file = format_repo_path(file.path(getwd(), "tasks", "edits", fname)),
      source_qmd = source_qmd,
      rendered_pdf = rendered_pdf,
      rendered_docx = rendered_docx
    )
  })

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble(file = character(0), source_qmd = character(0), rendered_pdf = character(0), rendered_docx = character(0))
}

#' Find Default Review Directory
#' @return Path to input directory
#' @noRd
find_default_review_input_dir <- function() {
  candidates <- c(
    file.path(getwd(), "tasks", "edits"),
    file.path(getwd(), "tasks", "to_do", "review_tracker"),
    file.path("S:", "Clinical Science", "Staff", "Wolfson", "tasks", "edits"),
    getwd()
  )
  for (c in candidates) {
    if (dir.exists(c) && length(list.files(c, pattern = "\\.docx$", ignore.case = TRUE)) > 0) {
      return(c)
    }
  }
  getwd()
}

#' Backup File Before Modification
#' @param filepath Path to file
#' @return Backup path or NULL
#' @noRd
backup_review_file <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  bdir <- file.path(dirname(filepath), "backups")
  if (!dir.exists(bdir)) dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stem <- tools::file_path_sans_ext(basename(filepath))
  ext <- tools::file_ext(filepath)
  if (nzchar(ext)) ext <- paste0(".", ext)
  bak_path <- file.path(bdir, sprintf("%s_%s%s.bak", stem, ts, ext))
  file.copy(filepath, bak_path, overwrite = TRUE)
  bak_path
}

# =============================================================================
# DOCX XML EXTRACTION
# =============================================================================

#' Validate DOCX ZIP Archive
#' @param filepath Path to DOCX file
#' @return Logical TRUE/FALSE
#' @noRd
is_valid_docx <- function(filepath) {
  if (!file.exists(filepath)) return(FALSE)
  tryCatch({
    files <- utils::unzip(filepath, list = TRUE)$Name
    "[Content_Types].xml" %in% files
  }, error = function(e) FALSE)
}

#' Read Comment Metadata from word/comments.xml
#' @param comments_xml_path Path to comments.xml
#' @return Named list of comment metadata
#' @noRd
read_comments_metadata <- function(comments_xml_path) {
  meta <- list()
  if (!file.exists(comments_xml_path)) return(meta)

  root <- tryCatch(xml2::read_xml(comments_xml_path), error = function(e) NULL)
  if (is.null(root)) return(meta)

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

    t_nodes <- xml2::xml_find_all(c_node, "./w:p//w:t | ./w:p//w:delText", XML_NAMESPACES)
    raw_text <- paste(xml2::xml_text(t_nodes), collapse = "")

    meta[[cid]] <- list(
      comment_id = cid,
      author = xml2::xml_attr(c_node, "author"),
      date = xml2::xml_attr(c_node, "date"),
      comment_text = clean_review_text(raw_text),
      resolved_in_docx = identical(xml2::xml_attr(c_node, "done"), "1"),
      is_reply = is_reply,
      reply_to_id = reply_to_id
    )
  }
  meta
}

#' Extract Comment Locations and Spans from word/document.xml
#' @param doc_xml xml2 document
#' @return List of comment location records
#' @noRd
extract_comment_locations <- function(doc_xml) {
  paragraphs <- xml2::xml_find_all(doc_xml, "//w:p", XML_NAMESPACES)
  if (length(paragraphs) == 0) return(list())

  starts <- list()
  ends <- list()

  # Identify paragraph indices for commentRangeStart and commentRangeEnd
  for (para_idx in seq_along(paragraphs)) {
    p <- paragraphs[[para_idx]]
    start_nodes <- xml2::xml_find_all(p, ".//w:commentRangeStart", XML_NAMESPACES)
    for (sn in start_nodes) {
      cid <- xml2::xml_attr(sn, "id")
      if (!is.na(cid) && nzchar(cid)) starts[[cid]] <- para_idx
    }
    end_nodes <- xml2::xml_find_all(p, ".//w:commentRangeEnd", XML_NAMESPACES)
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
      t_all <- xml2::xml_find_all(p, ".//w:t | .//w:delText", XML_NAMESPACES)
      context_pieces <- c(context_pieces, paste(xml2::xml_text(t_all), collapse = ""))

      # Extract elements between commentRangeStart and commentRangeEnd
      descendants <- xml2::xml_find_all(p, ".//*", XML_NAMESPACES)
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
        if (collecting && tag %in% c("t", "delText")) {
          txt <- xml2::xml_text(el)
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
    insertion = "//w:ins",
    deletion = "//w:del",
    move_from = "//w:moveFrom",
    move_to = "//w:moveTo"
  )

  paragraphs <- xml2::xml_find_all(doc_xml, "//w:p", XML_NAMESPACES)
  para_ptrs <- vapply(paragraphs, function(p) format(p), character(1))

  rows <- list()
  for (rev_type in names(rev_xpaths)) {
    xpath <- rev_xpaths[[rev_type]]
    rev_nodes <- xml2::xml_find_all(doc_xml, xpath, XML_NAMESPACES)
    for (node in rev_nodes) {
      t_nodes <- xml2::xml_find_all(node, ".//w:t | .//w:delText", XML_NAMESPACES)
      txt <- clean_review_text(paste(xml2::xml_text(t_nodes), collapse = ""))
      if (nchar(txt) < min_length) next

      # Find ancestor paragraph
      p_anc <- xml2::xml_find_first(node, "ancestor::w:p", XML_NAMESPACES)
      p_num <- NA_integer_
      p_ctx <- ""
      if (!is.na(xml2::xml_name(p_anc))) {
        anc_ptr <- format(p_anc)
        m_idx <- which(para_ptrs == anc_ptr)
        if (length(m_idx) > 0) {
          p_num <- as.integer(m_idx[1])
          p_all_t <- xml2::xml_find_all(paragraphs[[p_num]], ".//w:t | .//w:delText", XML_NAMESPACES)
          p_ctx <- clean_review_text(paste(xml2::xml_text(p_all_t), collapse = ""))
        }
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
    if (tag == "ins") in_ins <- TRUE
    else if (tag == "del") in_del <- TRUE
    else if (tag == "moveTo") in_moveto <- TRUE
    else if (tag == "moveFrom") in_movefrom <- TRUE

    if (tag %in% c("t", "delText")) {
      txt <- xml2::xml_text(el)
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
  paragraphs <- xml2::xml_find_all(doc_xml, "//w:p", XML_NAMESPACES)
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

    descendants <- xml2::xml_find_all(p, ".//*", XML_NAMESPACES)
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

#' Extract Paragraph Redlines from word/document.xml
#' @param doc_xml xml2 document
#' @param filename DOCX filename
#' @return Tibble of paragraph redlines
#' @noRd
extract_docx_redlines <- function(doc_xml, filename) {
  paragraphs <- xml2::xml_find_all(doc_xml, "//w:p", XML_NAMESPACES)
  if (length(paragraphs) == 0) return(tibble::tibble())

  toc_lof_para_nums <- compute_toc_lof_paragraph_numbers(doc_xml)
  rows <- list()

  for (para_idx in seq_along(paragraphs)) {
    p <- paragraphs[[para_idx]]
    rev_nodes <- xml2::xml_find_all(p, ".//w:ins | .//w:del | .//w:moveFrom | .//w:moveTo", XML_NAMESPACES)
    if (length(rev_nodes) == 0) next

    texts <- build_redline_texts(p)
    if (identical(texts$original_text, texts$accepted_text)) next

    last_node <- rev_nodes[[length(rev_nodes)]]
    author <- xml2::xml_attr(last_node, "author")
    date <- xml2::xml_attr(last_node, "date")

    rows[[length(rows) + 1]] <- tibble::tibble(
      file = filename,
      paragraph_number = as.integer(para_idx),
      author = if (!is.na(author)) author else NA_character_,
      date = if (!is.na(date)) date else NA_character_,
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

#' Extract All Elements from a Single DOCX File
#' @param docx_path Path to DOCX file
#' @param min_revision_length Minimum revision length
#' @return List of comments, revisions, and redlines tibbles
#' @noRd
extract_from_docx <- function(docx_path, min_revision_length = 2) {
  filename <- basename(docx_path)
  td <- tempfile(pattern = "docx_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  utils::unzip(docx_path, exdir = td)

  comments_xml_path <- file.path(td, "word", "comments.xml")
  doc_xml_path <- file.path(td, "word", "document.xml")

  metadata <- if (file.exists(comments_xml_path)) read_comments_metadata(comments_xml_path) else list()
  doc_xml <- if (file.exists(doc_xml_path)) xml2::read_xml(doc_xml_path) else NULL

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
  list(comments = comments, revisions = revisions, redlines = redlines)
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
    documents = list()
  )
  if (!file.exists(tracker_path)) return(res)

  sheets <- tryCatch(readxl::excel_sheets(tracker_path), error = function(e) character(0))
  if (length(sheets) == 0) return(res)

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

  if ("Documents" %in% sheets) {
    df_d <- tryCatch(readxl::read_excel(tracker_path, sheet = "Documents", col_types = "text"), error = function(e) tibble::tibble())
    if (nrow(df_d) > 0 && "file" %in% names(df_d)) {
      doc_map <- list()
      for (i in seq_len(nrow(df_d))) {
        fn <- as.character(df_d$file[i])
        if (!is.na(fn) && nzchar(fn)) {
          doc_map[[fn]] <- as.list(df_d[i, ])
        }
      }
      res$documents <- doc_map
    }
  }

  res
}

#' Apply Reviewer Fork Overlays
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

  for (rev in fork_reviewers) {
    fork_filename <- sprintf("%s_%s%s", tracker_stem, rev, tracker_ext)
    fork_path <- file.path(output_dir, fork_filename)
    if (!file.exists(fork_path)) next

    fork_data <- load_existing_tracker(fork_path)
    cols <- c(rev, paste0(rev, "_comment"))

    # Overlay Comments
    if (nrow(fork_data$comments) > 0 && nrow(existing_comments) > 0) {
      if ("file" %in% names(fork_data$comments) && "comment_id" %in% names(fork_data$comments)) {
        for (i in seq_len(nrow(fork_data$comments))) {
          ff <- trimws(as.character(fork_data$comments$file[i]))
          fcid <- trimws(as.character(fork_data$comments$comment_id[i]))
          match_idx <- which(trimws(as.character(existing_comments$file)) == ff &
                             trimws(as.character(existing_comments$comment_id)) == fcid)
          if (length(match_idx) > 0) {
            for (col in cols) {
              if (col %in% names(fork_data$comments)) {
                existing_comments[[col]][match_idx] <- fork_data$comments[[col]][i]
              }
            }
          }
        }
      }
    }

    # Overlay SuggestedChanges / Redlines
    if (nrow(fork_data$redlines) > 0 && nrow(existing_redlines) > 0) {
      if (all(c("file", "paragraph_number", "original_text") %in% names(fork_data$redlines))) {
        for (i in seq_len(nrow(fork_data$redlines))) {
          ff <- trimws(as.character(fork_data$redlines$file[i]))
          fp <- as.character(fork_data$redlines$paragraph_number[i])
          forig <- clean_review_text(fork_data$redlines$original_text[i])
          match_idx <- which(trimws(as.character(existing_redlines$file)) == ff &
                             as.character(existing_redlines$paragraph_number) == fp &
                             clean_review_text(existing_redlines$original_text) == forig)
          if (length(match_idx) > 0) {
            for (col in cols) {
              if (col %in% names(fork_data$redlines)) {
                existing_redlines[[col]][match_idx] <- fork_data$redlines[[col]][i]
              }
            }
          }
        }
      }
    }

    if (verbose) {
      message(sprintf("Applied %s's fork (%s): %d comment row(s), %d suggested-change row(s).",
                      rev, basename(fork_path), nrow(fork_data$comments), nrow(fork_data$redlines)))
    }
  }

  list(comments = existing_comments, redlines = existing_redlines)
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
#' @return List with columns and merged rows
#' @noRd
merge_comments <- function(incoming_comments, existing_rows, existing_headers, catalog, scanned_files) {
  # Build full column list
  all_columns <- CANONICAL_COLUMNS
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

  merged_list <- list()
  matched_existing_indices <- integer(0)

  if (nrow(incoming_comments) > 0) {
    for (i in seq_len(nrow(incoming_comments))) {
      inc <- incoming_comments[i, ]
      f <- trimws(as.character(inc$file))
      cid <- trimws(as.character(inc$comment_id))
      dt <- trimws(as.character(if (!is.na(inc$date)) inc$date else ""))
      txt <- clean_review_text(inc$comment_text)

      matched_idx <- NA_integer_

      if (nrow(available_existing) > 0) {
        ex_files <- trimws(as.character(available_existing$file))
        ex_cids <- trimws(as.character(available_existing$comment_id))
        ex_dates <- trimws(as.character(available_existing$date))
        ex_texts <- clean_review_text(available_existing$comment_text)
        ex_pnums <- as.integer(available_existing$paragraph_number)

        unmatched_mask <- !seq_len(nrow(available_existing)) %in% matched_existing_indices

        # Priority 1: (file, comment_id)
        p1 <- which(unmatched_mask & ex_files == f & ex_cids == cid)
        if (length(p1) > 0) {
          matched_idx <- p1[1]
        } else if (nzchar(dt) && nzchar(txt)) {
          # Priority 2: (file, date, text)
          p2 <- which(unmatched_mask & ex_files == f & ex_dates == dt & ex_texts == txt)
          if (length(p2) > 0) matched_idx <- p2[1]
        }
        # Priority 3: (file, text, paragraph_number)
        if (is.na(matched_idx) && nzchar(txt) && !is.na(inc$paragraph_number)) {
          p3 <- which(unmatched_mask & ex_files == f & ex_texts == txt & ex_pnums == inc$paragraph_number)
          if (length(p3) > 0) matched_idx <- p3[1]
        }
        # Priority 4: (file, text)
        if (is.na(matched_idx) && nzchar(txt)) {
          p4 <- which(unmatched_mask & ex_files == f & ex_texts == txt)
          if (length(p4) > 0) matched_idx <- p4[1]
        }
        # Priority 5: (file, date)
        if (is.na(matched_idx) && nzchar(dt)) {
          p5 <- which(unmatched_mask & ex_files == f & ex_dates == dt)
          if (length(p5) > 0) matched_idx <- p5[1]
        }
      }

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
        merged_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(merged_row)
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
        new_row$doc_status <- "Active"

        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(new_row)
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
        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(row)
      }
    }
  }

  merged_df <- if (length(merged_list) > 0) dplyr::bind_rows(merged_list) else tibble::as_tibble(matrix(nrow = 0, ncol = length(all_columns), dimnames = list(NULL, all_columns)))

  # Ensure all columns present
  for (col in all_columns) {
    if (!col %in% names(merged_df)) merged_df[[col]] <- NA_character_
  }

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

    ord <- order(ranks, pnums, cids, merged_df$file)
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

  merged_list <- list()
  if (nrow(incoming_revisions) > 0) {
    merged_list[[length(merged_list) + 1]] <- incoming_revisions
  }

  if (nrow(existing_revisions) > 0) {
    ex_files <- trimws(as.character(existing_revisions$file))
    unscanned_mask <- !ex_files %in% scanned_files
    if (any(unscanned_mask)) {
      merged_list[[length(merged_list) + 1]] <- existing_revisions[unscanned_mask, ]
    }
  }

  merged_df <- if (length(merged_list) > 0) dplyr::bind_rows(merged_list) else tibble::as_tibble(matrix(nrow = 0, ncol = length(rev_headers), dimnames = list(NULL, rev_headers)))

  for (col in rev_headers) {
    if (!col %in% names(merged_df)) merged_df[[col]] <- NA_character_
  }

  if (nrow(merged_df) > 0) {
    ranks <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
    pnums <- as.integer(merged_df$paragraph_number)
    pnums[is.na(pnums)] <- 999999L
    ord <- order(ranks, pnums, merged_df$file)
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
#' @return List with columns and merged rows
#' @noRd
merge_redlines <- function(incoming_redlines, existing_rows, existing_headers, catalog, scanned_files) {
  all_columns <- REDLINE_CANONICAL_COLUMNS
  for (col in existing_headers) {
    if (!col %in% all_columns) all_columns <- c(all_columns, col)
  }

  ignored_cols <- c(REDLINE_METADATA_COLUMNS, REDLINE_SYSTEM_COLUMNS, "resolved")
  available_existing <- if (nrow(existing_rows) > 0) existing_rows else tibble::tibble()

  for (col in all_columns) {
    if (!col %in% names(available_existing)) available_existing[[col]] <- character(nrow(available_existing))
  }

  merged_list <- list()
  matched_existing_indices <- integer(0)

  if (nrow(incoming_redlines) > 0) {
    for (i in seq_len(nrow(incoming_redlines))) {
      inc <- incoming_redlines[i, ]
      f <- trimws(as.character(inc$file))
      pnum <- inc$paragraph_number
      orig_txt <- clean_review_text(inc$original_text)
      acc_txt <- clean_review_text(inc$accepted_text)

      matched_idx <- NA_integer_

      if (nrow(available_existing) > 0) {
        ex_files <- trimws(as.character(available_existing$file))
        ex_pnums <- as.integer(available_existing$paragraph_number)
        ex_origs <- clean_review_text(available_existing$original_text)
        ex_accs <- clean_review_text(available_existing$accepted_text)

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

        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(merged_row)
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

        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(new_row)
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
        merged_list[[length(merged_list) + 1]] <- tibble::as_tibble(row)
      }
    }
  }

  merged_df <- if (length(merged_list) > 0) dplyr::bind_rows(merged_list) else tibble::as_tibble(matrix(nrow = 0, ncol = length(all_columns), dimnames = list(NULL, all_columns)))

  for (col in all_columns) {
    if (!col %in% names(merged_df)) merged_df[[col]] <- NA_character_
  }

  if (nrow(merged_df) > 0) {
    stages <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$stage, character(1))
    ranks <- vapply(merged_df$file, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
    merged_df$pipeline_stage <- stages

    pnums <- as.integer(merged_df$paragraph_number)
    pnums[is.na(pnums)] <- 999999L
    ord <- order(ranks, pnums, merged_df$file)
    merged_df <- merged_df[ord, all_columns]
  }

  list(columns = all_columns, rows = merged_df)
}

# =============================================================================
# EXCEL WORKBOOK GENERATOR (openxlsx)
# =============================================================================

#' Drop Other Reviewers' Columns for Fork View
#' @param reviewer Current reviewer
#' @param fork_reviewers Vector of all fork reviewers
#' @return Vector of column names to omit
#' @noRd
reviewer_fork_drop_columns <- function(reviewer, fork_reviewers) {
  drop <- c("wolfson", "wolfson_comment")
  for (other in fork_reviewers) {
    if (other != reviewer) {
      drop <- c(drop, other, paste0(other, "_comment"))
    }
  }
  drop
}

#' Write Formatted Review Tracker Excel Workbook
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
                                       fork_reviewers = c("jka", "darina", "zhao"),
                                       reviewer_view = NULL) {
  wb <- openxlsx::createWorkbook()

  if (!is.null(reviewer_view)) {
    drop_cols <- reviewer_fork_drop_columns(reviewer_view, fork_reviewers)
    sheet_columns <- columns[!columns %in% drop_cols]
    sheet_redline_columns <- redline_columns[!redline_columns %in% drop_cols]
  } else {
    sheet_columns <- columns
    sheet_redline_columns <- redline_columns
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

  # ---------------------------------------------------------------------------
  # Sheet 1: Comments
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Comments")
  col_map <- stats::setNames(seq_along(sheet_columns), sheet_columns)
  col_jka <- if ("jka" %in% names(col_map)) openxlsx::int2col(col_map[["jka"]]) else NULL
  col_darina <- if ("darina" %in% names(col_map)) openxlsx::int2col(col_map[["darina"]]) else NULL
  col_zhao <- if ("zhao" %in% names(col_map)) openxlsx::int2col(col_map[["zhao"]]) else NULL
  col_resolved <- if ("resolved" %in% names(col_map)) openxlsx::int2col(col_map[["resolved"]]) else NULL

  # Write Header
  for (ci in seq_along(sheet_columns)) {
    cn <- sheet_columns[ci]
    openxlsx::writeData(wb, "Comments", cn, startCol = ci, startRow = 1)
    fill_st <- if (cn %in% METADATA_COLUMNS) header_fill_styles$metadata
      else if (cn == "resolved") header_fill_styles$resolved
      else if (cn %in% WORKFLOW_COLUMNS) header_fill_styles$workflow
      else header_fill_styles$system
    openxlsx::addStyle(wb, "Comments", fill_st, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "Comments", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  n_c_rows <- nrow(comments_rows)
  if (n_c_rows > 0) {
    for (ri in seq_len(n_c_rows)) {
      row_num <- ri + 1L
      for (ci in seq_along(sheet_columns)) {
        cn <- sheet_columns[ci]
        val <- comments_rows[[cn]][ri]

        if (cn == "resolved") {
          if (!is.null(col_jka) && !is.null(col_darina) && !is.null(col_zhao)) {
            formula_str <- sprintf(
              '=IF(AND(OR(%s%d=TRUE,%s%d="TRUE"),OR(%s%d=TRUE,%s%d="TRUE"),OR(%s%d=TRUE,%s%d="TRUE")), TRUE, FALSE)',
              col_jka, row_num, col_jka, row_num,
              col_darina, row_num, col_darina, row_num,
              col_zhao, row_num, col_zhao, row_num
            )
            openxlsx::writeFormula(wb, "Comments", formula_str, startCol = ci, startRow = row_num)
          } else {
            j_val <- is_review_true(comments_rows$jka[ri])
            d_val <- is_review_true(comments_rows$darina[ri])
            z_val <- is_review_true(comments_rows$zhao[ri])
            openxlsx::writeData(wb, "Comments", (j_val && d_val && z_val), startCol = ci, startRow = row_num)
          }
          openxlsx::addStyle(wb, "Comments", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else if (cn %in% c("jka", "darina", "zhao", "wolfson")) {
          b_val <- if (is.na(val) || !nzchar(as.character(val))) NA else is_review_true(val)
          openxlsx::writeData(wb, "Comments", b_val, startCol = ci, startRow = row_num)
          openxlsx::addStyle(wb, "Comments", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else if (cn %in% c("paragraph_number", "end_paragraph_number", "duplicate_count")) {
          int_val <- suppressWarnings(as.integer(val))
          openxlsx::writeData(wb, "Comments", if (is.na(int_val)) "" else int_val, startCol = ci, startRow = row_num)
          openxlsx::addStyle(wb, "Comments", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else {
          txt_val <- if (is.na(val)) "" else as.character(val)
          openxlsx::writeData(wb, "Comments", txt_val, startCol = ci, startRow = row_num)
          openxlsx::addStyle(wb, "Comments", font_regular_left, rows = row_num, cols = ci, stack = TRUE)
        }
      }
    }

    # Data Validation
    for (cn in c("jka", "darina", "zhao", "wolfson")) {
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

  for (ci in seq_along(rev_headers)) {
    h <- rev_headers[ci]
    openxlsx::writeData(wb, "TrackedChanges", h, startCol = ci, startRow = 1)
    openxlsx::addStyle(wb, "TrackedChanges", header_fill_styles$tracked, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "TrackedChanges", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  n_tc_rows <- nrow(revisions_rows)
  if (n_tc_rows > 0) {
    for (ri in seq_len(n_tc_rows)) {
      row_num <- ri + 1L
      for (ci in seq_along(rev_headers)) {
        h <- rev_headers[ci]
        val <- revisions_rows[[h]][ri]
        txt_val <- if (is.na(val)) "" else as.character(val)
        openxlsx::writeData(wb, "TrackedChanges", txt_val, startCol = ci, startRow = row_num)
        st <- if (h %in% c("changed_text", "paragraph_context")) font_regular_left else font_regular_center
        openxlsx::addStyle(wb, "TrackedChanges", st, rows = row_num, cols = ci, stack = TRUE)
      }
    }
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
  rl_col_jka <- if ("jka" %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[["jka"]]) else NULL
  rl_col_darina <- if ("darina" %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[["darina"]]) else NULL
  rl_col_zhao <- if ("zhao" %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[["zhao"]]) else NULL
  rl_col_resolved <- if ("resolved" %in% names(rl_col_map)) openxlsx::int2col(rl_col_map[["resolved"]]) else NULL

  for (ci in seq_along(sheet_redline_columns)) {
    cn <- sheet_redline_columns[ci]
    openxlsx::writeData(wb, "SuggestedChanges", cn, startCol = ci, startRow = 1)
    fill_st <- if (cn %in% REDLINE_METADATA_COLUMNS) header_fill_styles$redline
      else if (cn == "resolved") header_fill_styles$resolved
      else if (cn %in% REDLINE_WORKFLOW_COLUMNS) header_fill_styles$workflow
      else header_fill_styles$system
    openxlsx::addStyle(wb, "SuggestedChanges", fill_st, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "SuggestedChanges", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  n_rl_rows <- nrow(redlines_rows)
  if (n_rl_rows > 0) {
    for (ri in seq_len(n_rl_rows)) {
      row_num <- ri + 1L
      for (ci in seq_along(sheet_redline_columns)) {
        cn <- sheet_redline_columns[ci]
        val <- redlines_rows[[cn]][ri]

        if (cn == "resolved") {
          if (!is.null(rl_col_jka) && !is.null(rl_col_darina) && !is.null(rl_col_zhao)) {
            formula_str <- sprintf(
              '=IF(AND(OR(%s%d=TRUE,%s%d="TRUE"),OR(%s%d=TRUE,%s%d="TRUE"),OR(%s%d=TRUE,%s%d="TRUE")), TRUE, FALSE)',
              rl_col_jka, row_num, rl_col_jka, row_num,
              rl_col_darina, row_num, rl_col_darina, row_num,
              rl_col_zhao, row_num, rl_col_zhao, row_num
            )
            openxlsx::writeFormula(wb, "SuggestedChanges", formula_str, startCol = ci, startRow = row_num)
          } else {
            j_val <- is_review_true(redlines_rows$jka[ri])
            d_val <- is_review_true(redlines_rows$darina[ri])
            z_val <- is_review_true(redlines_rows$zhao[ri])
            openxlsx::writeData(wb, "SuggestedChanges", (j_val && d_val && z_val), startCol = ci, startRow = row_num)
          }
          openxlsx::addStyle(wb, "SuggestedChanges", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else if (cn %in% c("is_comment", "is_toc_or_lof", "jka", "darina", "zhao", "wolfson")) {
          b_val <- if (is.na(val) || !nzchar(as.character(val))) NA else is_review_true(val)
          openxlsx::writeData(wb, "SuggestedChanges", b_val, startCol = ci, startRow = row_num)
          openxlsx::addStyle(wb, "SuggestedChanges", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else if (cn == "paragraph_number") {
          int_val <- suppressWarnings(as.integer(val))
          openxlsx::writeData(wb, "SuggestedChanges", if (is.na(int_val)) "" else int_val, startCol = ci, startRow = row_num)
          openxlsx::addStyle(wb, "SuggestedChanges", font_regular_center, rows = row_num, cols = ci, stack = TRUE)
        } else {
          txt_val <- if (is.na(val)) "" else as.character(val)
          openxlsx::writeData(wb, "SuggestedChanges", txt_val, startCol = ci, startRow = row_num)
          st <- if (cn %in% c("original_text", "accepted_text", "jka_comment", "darina_comment", "zhao_comment", "wolfson_comment")) font_regular_left else font_regular_center
          openxlsx::addStyle(wb, "SuggestedChanges", st, rows = row_num, cols = ci, stack = TRUE)
        }
      }
    }

    # Data Validation
    for (cn in c("is_comment", "jka", "darina", "zhao", "wolfson")) {
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
    w <- if (cn %in% c("original_text", "accepted_text", "jka_comment", "darina_comment", "zhao_comment", "wolfson_comment")) 50 else if (cn == "file") 32 else 15
    openxlsx::setColWidths(wb, "SuggestedChanges", cols = ci, widths = w, hidden = is_hidden)
  }
  openxlsx::freezePane(wb, "SuggestedChanges", firstActiveRow = 2, firstActiveCol = 3)

  # ---------------------------------------------------------------------------
  # Sheet 4: Documents
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Documents")
  doc_headers <- c(
    "file", "pipeline_stage", "comment_count", "resolved_comment_count",
    "reply_count", "tracked_change_count", "jka", "darina", "zhao",
    "resolved", "wolfson", "wolfson_comment"
  )

  for (ci in seq_along(doc_headers)) {
    h <- doc_headers[ci]
    openxlsx::writeData(wb, "Documents", h, startCol = ci, startRow = 1)
    fill_st <- if (h %in% c("file", "pipeline_stage", "comment_count", "resolved_comment_count", "reply_count", "tracked_change_count")) header_fill_styles$docs
      else if (h == "resolved") header_fill_styles$resolved
      else header_fill_styles$workflow
    openxlsx::addStyle(wb, "Documents", fill_st, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "Documents", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  all_doc_files <- unique(c(basename(processed_docs), comments_rows$file))
  all_doc_files <- all_doc_files[!is.na(all_doc_files) & nzchar(all_doc_files)]
  ranks <- vapply(all_doc_files, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  sorted_docs <- all_doc_files[order(ranks, all_doc_files)]

  doc_map <- if (!is.null(existing_docs)) existing_docs else list()
  SUMPRODUCT_BOUND <- 5000L

  for (ri in seq_along(sorted_docs)) {
    row_num <- ri + 1L
    fname <- sorted_docs[ri]
    f_comments <- comments_rows[comments_rows$file == fname, ]
    f_revisions <- revisions_rows[revisions_rows$file == fname, ]
    f_redlines <- redlines_rows[redlines_rows$file == fname, ]
    f_replies <- if ("is_reply" %in% names(f_comments)) f_comments[is_review_true(f_comments$is_reply), ] else f_comments[0, ]
    doc_stage <- match_docx_to_pipeline(fname, catalog)$stage

    # Col 1: file
    openxlsx::writeData(wb, "Documents", fname, startCol = 1, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_left, rows = row_num, cols = 1, stack = TRUE)

    # Col 2: pipeline_stage
    openxlsx::writeData(wb, "Documents", doc_stage, startCol = 2, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_left, rows = row_num, cols = 2, stack = TRUE)

    # Col 3: comment_count
    openxlsx::writeData(wb, "Documents", nrow(f_comments), startCol = 3, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 3, stack = TRUE)

    # Col 4: resolved_comment_count (live formula)
    res_col_letter <- if (!is.null(col_resolved)) col_resolved else "J"
    formula_res_count <- sprintf('=COUNTIFS(Comments!$A:$A, A%d, Comments!$%s:$%s, TRUE)', row_num, res_col_letter, res_col_letter)
    openxlsx::writeFormula(wb, "Documents", formula_res_count, startCol = 4, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 4, stack = TRUE)

    # Col 5: reply_count
    openxlsx::writeData(wb, "Documents", nrow(f_replies), startCol = 5, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 5, stack = TRUE)

    # Col 6: tracked_change_count
    openxlsx::writeData(wb, "Documents", nrow(f_revisions), startCol = 6, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 6, stack = TRUE)

    # Reviewer rollups (jka = col 7, darina = col 8, zhao = col 9)
    doc_reviewer_rollup <- function(rev_id, c_col, rl_col) {
      if (!is.null(c_col) && !is.null(rl_col)) {
        sprintf(
          '=IF(AND(SUMPRODUCT((Comments!$A$2:$A$%d=A%d)*((Comments!$%s$2:$%s$%d=TRUE)+(Comments!$%s$2:$%s$%d="TRUE"))) = COUNTIF(Comments!$A:$A, A%d), SUMPRODUCT((SuggestedChanges!$A$2:$A$%d=A%d)*((SuggestedChanges!$%s$2:$%s$%d=TRUE)+(SuggestedChanges!$%s$2:$%s$%d="TRUE"))) = COUNTIF(SuggestedChanges!$A:$A, A%d)), TRUE, FALSE)',
          SUMPRODUCT_BOUND, row_num, c_col, c_col, SUMPRODUCT_BOUND, c_col, c_col, SUMPRODUCT_BOUND, row_num,
          SUMPRODUCT_BOUND, row_num, rl_col, rl_col, SUMPRODUCT_BOUND, rl_col, rl_col, SUMPRODUCT_BOUND, row_num
        )
      } else {
        if (nrow(f_comments) == 0 && nrow(f_redlines) == 0) return(FALSE)
        c_all <- if (rev_id %in% names(f_comments)) all(vapply(f_comments[[rev_id]], is_review_true, logical(1))) else FALSE
        rl_all <- if (rev_id %in% names(f_redlines)) all(vapply(f_redlines[[rev_id]], is_review_true, logical(1))) else FALSE
        c_all && rl_all
      }
    }

    # JKA (col 7)
    jka_rollup <- doc_reviewer_rollup("jka", col_jka, rl_col_jka)
    if (is.character(jka_rollup) && startsWith(jka_rollup, "=")) {
      openxlsx::writeFormula(wb, "Documents", jka_rollup, startCol = 7, startRow = row_num)
    } else {
      openxlsx::writeData(wb, "Documents", isTRUE(jka_rollup), startCol = 7, startRow = row_num)
    }
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 7, stack = TRUE)

    # Darina (col 8)
    darina_rollup <- doc_reviewer_rollup("darina", col_darina, rl_col_darina)
    if (is.character(darina_rollup) && startsWith(darina_rollup, "=")) {
      openxlsx::writeFormula(wb, "Documents", darina_rollup, startCol = 8, startRow = row_num)
    } else {
      openxlsx::writeData(wb, "Documents", isTRUE(darina_rollup), startCol = 8, startRow = row_num)
    }
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 8, stack = TRUE)

    # Zhao (col 9)
    zhao_rollup <- doc_reviewer_rollup("zhao", col_zhao, rl_col_zhao)
    if (is.character(zhao_rollup) && startsWith(zhao_rollup, "=")) {
      openxlsx::writeFormula(wb, "Documents", zhao_rollup, startCol = 9, startRow = row_num)
    } else {
      openxlsx::writeData(wb, "Documents", isTRUE(zhao_rollup), startCol = 9, startRow = row_num)
    }
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 9, stack = TRUE)

    # Resolved (col 10)
    formula_doc_resolved <- sprintf('=IF(AND(G%d=TRUE, H%d=TRUE, I%d=TRUE), TRUE, FALSE)', row_num, row_num, row_num)
    openxlsx::writeFormula(wb, "Documents", formula_doc_resolved, startCol = 10, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 10, stack = TRUE)

    # Wolfson (col 11) and comment (col 12)
    dfb <- if (fname %in% names(doc_map)) doc_map[[fname]] else list()
    w_val <- if (!is.null(dfb$wolfson) && !is.na(dfb$wolfson)) is_review_true(dfb$wolfson) else NA
    wc_val <- if (!is.null(dfb$wolfson_comment) && !is.na(dfb$wolfson_comment)) as.character(dfb$wolfson_comment) else ""

    openxlsx::writeData(wb, "Documents", w_val, startCol = 11, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_center, rows = row_num, cols = 11, stack = TRUE)

    openxlsx::writeData(wb, "Documents", wc_val, startCol = 12, startRow = row_num)
    openxlsx::addStyle(wb, "Documents", font_regular_left, rows = row_num, cols = 12, stack = TRUE)
  }

  n_doc_rows <- length(sorted_docs)
  if (n_doc_rows > 0) {
    # Data validation on Wolfson column (col 11)
    openxlsx::dataValidation(
      wb, "Documents", cols = 11, rows = 2:(n_doc_rows + 1L),
      type = "list", value = '"TRUE,FALSE"'
    )
    # Conditional formatting on G:J (cols 7:10)
    for (ci in 7:10) {
      openxlsx::conditionalFormatting(
        wb, "Documents", cols = ci, rows = 2:(n_doc_rows + 1L),
        rule = "TRUE", type = "contains", style = green_fill_style
      )
      openxlsx::conditionalFormatting(
        wb, "Documents", cols = ci, rows = 2:(n_doc_rows + 1L),
        rule = "FALSE", type = "contains", style = red_fill_style
      )
    }
  }

  openxlsx::setColWidths(wb, "Documents", cols = 1:2, widths = 40)
  openxlsx::setColWidths(wb, "Documents", cols = 3:6, widths = 22)
  openxlsx::setColWidths(wb, "Documents", cols = 7:9, widths = 12)
  openxlsx::setColWidths(wb, "Documents", cols = 10:11, widths = 14)
  openxlsx::setColWidths(wb, "Documents", cols = 12, widths = 40)
  openxlsx::freezePane(wb, "Documents", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 5: docXwalk
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "docXwalk")
  walk_headers <- c("file", "source_qmd", "rendered_pdf", "rendered_docx")
  for (ci in seq_along(walk_headers)) {
    h <- walk_headers[ci]
    openxlsx::writeData(wb, "docXwalk", h, startCol = ci, startRow = 1)
    openxlsx::addStyle(wb, "docXwalk", header_fill_styles$docs, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "docXwalk", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  walk_df <- build_docxwalk_df(sorted_docs, catalog)
  if (nrow(walk_df) > 0) {
    for (ri in seq_len(nrow(walk_df))) {
      row_num <- ri + 1L
      for (ci in seq_along(walk_headers)) {
        h <- walk_headers[ci]
        val <- walk_df[[h]][ri]
        openxlsx::writeData(wb, "docXwalk", if (is.na(val)) "" else as.character(val), startCol = ci, startRow = row_num)
        openxlsx::addStyle(wb, "docXwalk", font_regular_left, rows = row_num, cols = ci, stack = TRUE)
      }
    }
  }
  openxlsx::setColWidths(wb, "docXwalk", cols = 1:4, widths = 65)
  openxlsx::freezePane(wb, "docXwalk", firstActiveRow = 2, firstActiveCol = 1)

  # ---------------------------------------------------------------------------
  # Sheet 6: CommentSummary
  # ---------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "CommentSummary")
  sum_headers <- c("comment_text", "occurrences", "files")
  for (ci in seq_along(sum_headers)) {
    h <- sum_headers[ci]
    openxlsx::writeData(wb, "CommentSummary", h, startCol = ci, startRow = 1)
    openxlsx::addStyle(wb, "CommentSummary", header_fill_styles$summary, rows = 1, cols = ci, stack = TRUE)
    openxlsx::addStyle(wb, "CommentSummary", font_title_style, rows = 1, cols = ci, stack = TRUE)
  }

  sum_df <- build_comment_summary_df(comments_rows)
  if (nrow(sum_df) > 0) {
    for (ri in seq_len(nrow(sum_df))) {
      row_num <- ri + 1L
      openxlsx::writeData(wb, "CommentSummary", sum_df$comment_text[ri], startCol = 1, startRow = row_num)
      openxlsx::addStyle(wb, "CommentSummary", font_regular_left, rows = row_num, cols = 1, stack = TRUE)

      openxlsx::writeData(wb, "CommentSummary", sum_df$occurrences[ri], startCol = 2, startRow = row_num)
      openxlsx::addStyle(wb, "CommentSummary", font_regular_center, rows = row_num, cols = 2, stack = TRUE)

      openxlsx::writeData(wb, "CommentSummary", sum_df$files[ri], startCol = 3, startRow = row_num)
      openxlsx::addStyle(wb, "CommentSummary", font_regular_left, rows = row_num, cols = 3, stack = TRUE)
    }
  }
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
    for (ci in seq_along(err_headers)) {
      h <- err_headers[ci]
      openxlsx::writeData(wb, "Errors", h, startCol = ci, startRow = 1)
      openxlsx::addStyle(wb, "Errors", header_fill_styles$errors, rows = 1, cols = ci, stack = TRUE)
      openxlsx::addStyle(wb, "Errors", font_title_style, rows = 1, cols = ci, stack = TRUE)
    }
    for (ri in seq_len(nrow(errors))) {
      row_num <- ri + 1L
      for (ci in seq_along(err_headers)) {
        h <- err_headers[ci]
        val <- errors[[h]][ri]
        openxlsx::writeData(wb, "Errors", if (is.na(val)) "" else as.character(val), startCol = ci, startRow = row_num)
        openxlsx::addStyle(wb, "Errors", font_regular_left, rows = row_num, cols = ci, stack = TRUE)
      }
    }
    openxlsx::setColWidths(wb, "Errors", cols = 1:3, widths = 40)
    openxlsx::freezePane(wb, "Errors", firstActiveRow = 2, firstActiveCol = 1)
  }

  # Order sheets according to preferred order
  preferred_order <- c("Comments", "SuggestedChanges", "Documents", "docXwalk", "TrackedChanges", "CommentSummary", "Errors")
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
#' @noRd
build_documents_summary_df <- function(processed_docs, comments_rows, revisions_rows, redlines_rows, catalog, existing_docs) {
  all_files <- unique(c(basename(processed_docs), comments_rows$file))
  all_files <- all_files[!is.na(all_files) & nzchar(all_files)]
  ranks <- vapply(all_files, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1))
  sorted_files <- all_files[order(ranks, all_files)]

  doc_map <- if (!is.null(existing_docs)) existing_docs else list()

  rows <- lapply(sorted_files, function(f) {
    fc <- comments_rows[comments_rows$file == f, ]
    fr <- revisions_rows[revisions_rows$file == f, ]
    frl <- redlines_rows[redlines_rows$file == f, ]
    freplies <- if ("is_reply" %in% names(fc)) fc[is_review_true(fc$is_reply), ] else fc[0, ]
    stage <- match_docx_to_pipeline(f, catalog)$stage

    dfb <- if (f %in% names(doc_map)) doc_map[[f]] else list()
    w_val <- if (!is.null(dfb$wolfson) && !is.na(dfb$wolfson)) is_review_true(dfb$wolfson) else NA
    wc_val <- if (!is.null(dfb$wolfson_comment) && !is.na(dfb$wolfson_comment)) as.character(dfb$wolfson_comment) else ""

    jka_val <- if ("jka" %in% names(fc) && nrow(fc) > 0) all(vapply(fc$jka, is_review_true, logical(1))) else FALSE
    darina_val <- if ("darina" %in% names(fc) && nrow(fc) > 0) all(vapply(fc$darina, is_review_true, logical(1))) else FALSE
    zhao_val <- if ("zhao" %in% names(fc) && nrow(fc) > 0) all(vapply(fc$zhao, is_review_true, logical(1))) else FALSE

    resolved_count <- if ("resolved" %in% names(fc)) sum(vapply(fc$resolved, is_review_true, logical(1))) else 0L

    tibble::tibble(
      file = f,
      pipeline_stage = stage,
      comment_count = nrow(fc),
      resolved_comment_count = resolved_count,
      reply_count = nrow(freplies),
      tracked_change_count = nrow(fr),
      jka = jka_val,
      darina = darina_val,
      zhao = zhao_val,
      resolved = (jka_val && darina_val && zhao_val),
      wolfson = w_val,
      wolfson_comment = wc_val
    )
  })

  if (length(rows) > 0) dplyr::bind_rows(rows) else tibble::tibble()
}

#' Build Comment Summary Tibble
#' @noRd
build_comment_summary_df <- function(comments_rows) {
  if (nrow(comments_rows) == 0 || !"comment_text" %in% names(comments_rows)) {
    return(tibble::tibble(comment_text = character(0), occurrences = integer(0), files = character(0)))
  }
  groups <- list()
  for (i in seq_len(nrow(comments_rows))) {
    txt <- clean_review_text(comments_rows$comment_text[i])
    fn <- trimws(as.character(comments_rows$file[i]))
    if (nzchar(txt)) {
      if (!txt %in% names(groups)) groups[[txt]] <- character(0)
      groups[[txt]] <- c(groups[[txt]], fn)
    }
  }
  if (length(groups) == 0) {
    return(tibble::tibble(comment_text = character(0), occurrences = integer(0), files = character(0)))
  }

  sorted_txts <- names(groups)[order(vapply(groups, length, integer(1)), decreasing = TRUE)]
  rows <- lapply(sorted_txts, function(txt) {
    flist <- sort(unique(groups[[txt]]))
    tibble::tibble(
      comment_text = txt,
      occurrences = length(groups[[txt]]),
      files = paste(flist, collapse = "; ")
    )
  })
  dplyr::bind_rows(rows)
}

#' Export Tabular Data to CSVs
#' @noRd
export_review_csvs <- function(output_dir, columns, comments_rows, revisions_rows, redline_columns, redlines_rows) {
  c_csv <- file.path(output_dir, "comments.csv")
  r_csv <- file.path(output_dir, "tracked_changes.csv")
  rl_csv <- file.path(output_dir, "suggested_changes.csv")

  # Clean comments for CSV
  c_df <- comments_rows
  if (nrow(c_df) > 0) {
    for (i in seq_len(nrow(c_df))) {
      j_ok <- is_review_true(c_df$jka[i])
      d_ok <- is_review_true(c_df$darina[i])
      z_ok <- is_review_true(c_df$zhao[i])
      c_df$resolved[i] <- (j_ok && d_ok && z_ok)
    }
    cols_present <- intersect(columns, names(c_df))
    utils::write.csv(c_df[, cols_present], file = c_csv, row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    utils::write.csv(c_df, file = c_csv, row.names = FALSE, fileEncoding = "UTF-8")
  }

  # Revisions
  utils::write.csv(revisions_rows, file = r_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # Suggested Changes / Redlines
  rl_df <- redlines_rows
  if (nrow(rl_df) > 0) {
    for (i in seq_len(nrow(rl_df))) {
      j_ok <- is_review_true(rl_df$jka[i])
      d_ok <- is_review_true(rl_df$darina[i])
      z_ok <- is_review_true(rl_df$zhao[i])
      rl_df$resolved[i] <- (j_ok && d_ok && z_ok)
    }
    cols_present <- intersect(redline_columns, names(rl_df))
    utils::write.csv(rl_df[, cols_present], file = rl_csv, row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    utils::write.csv(rl_df, file = rl_csv, row.names = FALSE, fileEncoding = "UTF-8")
  }

  list(
    comments_csv = c_csv,
    tracked_changes_csv = r_csv,
    suggested_changes_csv = rl_csv
  )
}
