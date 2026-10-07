#' Package Multiple Already-Rendered Reports Into an Indexed Zip
#'
#' Given a data frame describing an ordered set of already-rendered Quarto
#' reports grouped into stage folders, copies their existing PDF/DOCX/HTML
#' outputs into a staged build directory, builds an Excel index with
#' hyperlinks to each report, optionally bundles arbitrary data-deliverable
#' files alongside them, and zips the result.
#'
#' Unlike \code{\link{zip_render}} (which renders and zips a single
#' document), \code{zip_reports} does not render anything itself — it
#' packages outputs that have already been rendered, e.g. by
#' \code{\link{render}} (or \code{\link{render_me}}). Report order and staging is entirely the
#' caller's decision (for example, the topological sort of a project's own
#' dependency graph) — pass \code{reports} pre-ordered. If multiple reports
#' share the same source stem (for example \code{analysis1/analysis.qmd} and
#' \code{analysis2/analysis.qmd}), staged output names are disambiguated to
#' prevent overwrites within a stage/format folder.
#'
#' @param reports A data frame/tibble with one row per report and columns
#'   \code{name} (display name), \code{path} (path to the \code{.qmd}
#'   source — the corresponding \code{.pdf}/\code{.docx}/\code{.html}
#'   output paths are derived by swapping its extension), and optionally
#'   \code{stage} (subfolder to group the report under; \code{NA}/empty
#'   becomes \code{"99_Other"}) and \code{description}. The stage is used as a
#'   folder name inside the zip, so path separators and the characters a file
#'   name cannot hold on Windows become \code{-} and leading or trailing dots,
#'   dashes and spaces are dropped (\code{"../x"} gives \code{"x"}); ordinary
#'   names such as \code{"01 Results"} are kept as they are. The index shows the
#'   stage as it was given.
#' @param output_formats Character vector of formats to look for and
#'   include: any of \code{"pdf"}, \code{"docx"}, \code{"html"}, or
#'   \code{"all"} for all three. Defaults to \code{c("pdf", "docx")}.
#' @param data_deliverables Character vector of additional file paths to
#'   copy into a top-level \code{data/} folder in the zip. Defaults to
#'   \code{character(0)}. A path that does not exist is skipped. Files that
#'   share a file name (\code{a/results.csv}, \code{b/results.csv}) are all
#'   kept, the later ones with a numeric suffix (\code{results_2.csv}) and a
#'   warning. A file that cannot be copied (open in another program, a folder
#'   instead of a file) is left out with a warning.
#' @param zip_name Name of the output zip file. Defaults to
#'   \verb{all_reports_<date>.zip}.
#' @param output_dir Directory the finished zip is written to. Defaults to
#'   \code{getwd()}.
#' @param docx_from_pdf Optional function \code{function(src_pdf, dest_docx)}
#'   used to create \code{dest_docx} from an existing \code{src_pdf} when a
#'   report requests \code{"docx"} output but no (or a stale) \code{.docx}
#'   file exists next to its \code{.qmd} source. A DOCX is stale when it is
#'   older than its PDF. If \code{NULL} (default), a missing DOCX is skipped
#'   silently and a stale one is skipped with a warning; it is never shipped.
#'   The function counts as having failed when it returns \code{FALSE} (as
#'   \code{\link{convert_pdf_to_docx}} does) or when no DOCX at least as new as
#'   the PDF is there afterwards; a warning says so and no DOCX is shipped for
#'   that report. Any other return value is taken as success. In the index, the
#'   DOCX cell of a report whose PDF exists but that has no DOCX to ship reads
#'   \code{"not converted"}.
#' @return The path to the created zip file.
#' @export
#'
#' @examples
#' \dontrun{
#' reports <- data.frame(
#'   name = c("Introduction", "Results"),
#'   path = c("analysis/intro.qmd", "analysis/results.qmd"),
#'   stage = c("00_intro", "01_results"),
#'   description = c("Project overview", "Primary analysis results")
#' )
#' zip_reports(reports, output_formats = c("pdf", "docx"))
#' }
zip_reports <- function(reports,
                         output_formats = c("pdf", "docx"),
                         data_deliverables = character(0),
                         zip_name = NULL,
                         output_dir = getwd(),
                         docx_from_pdf = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required by zip_reports(). Install it with install.packages(\"openxlsx\").", call. = FALSE)
  }
  stopifnot(all(c("name", "path") %in% names(reports)))
  if (!("stage" %in% names(reports))) reports$stage <- NA_character_
  if (!("description" %in% names(reports))) reports$description <- NA_character_

  if ("all" %in% output_formats) output_formats <- c("pdf", "docx", "html")

  make_staged_stems <- function(paths) {
    paths <- as.character(paths)
    stems <- tools::file_path_sans_ext(basename(paths))
    parent_tags <- basename(dirname(paths))
    parent_tags[is.na(parent_tags) | !nzchar(parent_tags) | parent_tags == "."] <- "report"

    shared_stems <- duplicated(stems) | duplicated(stems, fromLast = TRUE)
    staged <- ifelse(shared_stems, paste(parent_tags, stems, sep = "__"), stems)

    staged <- gsub("[^A-Za-z0-9._-]+", "-", staged)
    staged <- gsub("^-+|-+$", "", staged)
    staged[!nzchar(staged)] <- "report"

    make.unique(staged, sep = "__")
  }
  staged_stems <- make_staged_stems(reports$path)
  docx_not_converted <- "not converted" # the index cell of a DOCX that could not be provided

  # Copies one report file into its staged folder. FALSE, with a warning, when
  # the copy fails (a file another program holds open, a directory where a file
  # was expected): the caller then leaves the file out of the index, which would
  # otherwise link to something that is not in the zip.
  stage_copy <- function(from, dest_dir, name) {
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    if (isTRUE(suppressWarnings(file.copy(from, file.path(dest_dir, name), overwrite = TRUE)))) {
      return(TRUE)
    }
    warning("Could not copy '", from, "' into the zip; it is left out of the archive and of the index.", call. = FALSE)
    FALSE
  }

  build_dir <- file.path(tempdir(), paste0("report_build_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (dir.exists(build_dir)) unlink(build_dir, recursive = TRUE)
  dir.create(build_dir, recursive = TRUE)
  # The staged copies exist only to be zipped: a copy of every report would
  # otherwise stay in tempdir() until the R session ends.
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

  index_data <- tibble::tibble(
    Order = integer(), `Report Name` = character(), Stage = character(),
    `PDF Link` = character(), `DOCX Link` = character(), `HTML Link` = character(),
    `Modified Date` = character(), Description = character()
  )

  for (i in seq_len(nrow(reports))) {
    report <- reports[i, ]
    stage_label <- report$stage
    if (is.na(stage_label) || !nzchar(stage_label)) stage_label <- "99_Other"
    # The stage is a folder name inside the zip, so it is cleaned (no path
    # separators, no leading dots); the index keeps the stage as it was given.
    stage_folder <- safe_stage_dir(stage_label, "99_Other")

    row_data <- list(
      Order = i, `Report Name` = report$name, Stage = stage_label,
      `PDF Link` = NA_character_, `DOCX Link` = NA_character_, `HTML Link` = NA_character_,
      `Modified Date` = NA_character_, Description = report$description
    )

    pdf_output_path <- sub("\\.qmd$", ".pdf", report$path)
    pdf_exists <- file.exists(pdf_output_path)

    for (fmt in output_formats) {
      output_path <- sub("\\.qmd$", paste0(".", fmt), report$path)
      staged_output_name <- paste0(staged_stems[i], ".", fmt)

      dest_dir <- file.path(build_dir, fmt, stage_folder)

      if (fmt == "pdf") {
        if (pdf_exists && stage_copy(output_path, dest_dir, staged_output_name)) {
          row_data$`PDF Link` <- file.path(".", fmt, stage_folder, staged_output_name)
          row_data$`Modified Date` <- format(file.info(output_path)$mtime, "%Y-%m-%d %H:%M")
        }
      } else if (fmt == "docx") {
        # A DOCX is only shipped when it is as new as its PDF: one that is
        # older was made from a different render, and a conversion that failed
        # leaves the old file where it was.
        docx_is_current <- function() {
          file.exists(output_path) &&
            !(pdf_exists && file.info(output_path)$mtime < file.info(pdf_output_path)$mtime)
        }
        ship_docx <- docx_is_current()

        if (pdf_exists && !ship_docx && !is.null(docx_from_pdf)) {
          converted <- docx_from_pdf(pdf_output_path, output_path)
          # `FALSE` is what convert_pdf_to_docx() returns when it fails; any
          # other value (that function returns the path, others return NULL)
          # counts, as long as a current file is there.
          ship_docx <- !isFALSE(converted) && docx_is_current()
          if (!ship_docx) {
            warning("docx_from_pdf() did not produce a current DOCX for '", report$name, "' (",
                    output_path, "); no DOCX is included for it.", call. = FALSE)
          }
        } else if (!ship_docx && file.exists(output_path)) {
          warning("The DOCX for '", report$name, "' (", output_path, ") is older than its PDF and ",
                  "was left out of the zip. Render it again, or pass docx_from_pdf to rebuild it.",
                  call. = FALSE)
        }

        if (ship_docx) {
          if (stage_copy(output_path, dest_dir, staged_output_name)) {
            row_data$`DOCX Link` <- file.path(".", fmt, stage_folder, staged_output_name)
            row_data$`Modified Date` <- format(file.info(output_path)$mtime, "%Y-%m-%d %H:%M")
          }
        } else if (pdf_exists) {
          # Asked for, the report was rendered, but there is no current DOCX
          # for it: say so in the index instead of leaving the cell blank.
          row_data$`DOCX Link` <- docx_not_converted
        }
      } else {
        if (file.exists(output_path) && stage_copy(output_path, dest_dir, staged_output_name)) {
          if (fmt == "html") row_data$`HTML Link` <- file.path(".", fmt, stage_folder, staged_output_name)
          row_data$`Modified Date` <- format(file.info(output_path)$mtime, "%Y-%m-%d %H:%M")
        }
      }
    }
    index_data <- dplyr::bind_rows(index_data, row_data)
  }

  if (length(data_deliverables) > 0) {
    data_dir <- file.path(build_dir, "data")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    staged_data <- character(0)
    for (data_file in data_deliverables) {
      if (!file.exists(data_file)) next
      # Two files with one name (a/results.csv, b/results.csv) must not be one
      # file in the zip: the later one is kept under a numbered name, with a warning.
      dest_file <- unique_staging_path(data_dir, basename(data_file), staged_data)
      if (isTRUE(suppressWarnings(file.copy(data_file, dest_file)))) {
        staged_data <- c(staged_data, dest_file)
      } else {
        warning("Could not copy the data deliverable '", data_file, "' into the zip; it is left out of the archive.",
                call. = FALSE)
      }
    }
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Report Index")
  openxlsx::writeData(wb, "Report Index", index_data, startRow = 1, startCol = 1,
                       headerStyle = openxlsx::createStyle(textDecoration = "bold"))

  for (r in seq_len(nrow(index_data))) {
    for (col in c("PDF Link", "DOCX Link", "HTML Link")) {
      link <- index_data[[col]][r]
      if (!is.na(link) && !identical(link, docx_not_converted)) {
        text <- if (col == "HTML Link") "HTML" else index_data$`Report Name`[r]
        formula <- openxlsx::makeHyperlinkString(text = text, file = link)
        openxlsx::writeFormula(wb, "Report Index", startRow = r + 1, startCol = which(names(index_data) == col), x = formula)
      }
    }
  }
  openxlsx::saveWorkbook(wb, file.path(build_dir, "report_order.xlsx"), overwrite = TRUE)

  if (is.null(zip_name) || !nzchar(zip_name)) {
    zip_name <- paste0("all_reports_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  }
  if (!grepl("\\.zip$", zip_name, ignore.case = TRUE)) zip_name <- paste0(zip_name, ".zip")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Resolved before the working directory changes. The folder exists by now, so
  # it resolves on every platform; the zip itself does not exist yet, and
  # normalizePath() leaves such a path as written on Linux and macOS.
  output_zip_path <- file.path(normalizePath(output_dir, winslash = "/", mustWork = FALSE), zip_name)

  old_wd <- setwd(build_dir)
  # Back out of the build folder first: it cannot be removed while it is the working directory.
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  files_to_zip <- list.files(".", recursive = TRUE)

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = output_zip_path, files = files_to_zip)
  } else {
    utils::zip(zipfile = output_zip_path, files = files_to_zip, flags = "-r9Xq")
  }

  output_zip_path
}
