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
#' \code{\link{render_me}}. Report order and staging is entirely the
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
#'   becomes \code{"99_Other"}) and \code{description}.
#' @param output_formats Character vector of formats to look for and
#'   include: any of \code{"pdf"}, \code{"docx"}, \code{"html"}, or
#'   \code{"all"} for all three. Defaults to \code{c("pdf", "docx")}.
#' @param data_deliverables Character vector of additional file paths to
#'   copy into a top-level \code{data/} folder in the zip. Defaults to
#'   \code{character(0)}.
#' @param zip_name Name of the output zip file. Defaults to
#'   \verb{all_reports_<date>.zip}.
#' @param output_dir Directory the finished zip is written to. Defaults to
#'   \code{getwd()}.
#' @param docx_from_pdf Optional function \code{function(src_pdf, dest_docx)}
#'   used to create \code{dest_docx} from an existing \code{src_pdf} when a
#'   report requests \code{"docx"} output but no (or a stale) \code{.docx}
#'   file exists next to its \code{.qmd} source. If \code{NULL} (default),
#'   missing/stale DOCX files are silently skipped rather than generated.
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

  build_dir <- file.path(tempdir(), paste0("report_build_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (dir.exists(build_dir)) unlink(build_dir, recursive = TRUE)
  dir.create(build_dir, recursive = TRUE)

  index_data <- tibble::tibble(
    Order = integer(), `Report Name` = character(), Stage = character(),
    `PDF Link` = character(), `DOCX Link` = character(), `HTML Link` = character(),
    `Modified Date` = character(), Description = character()
  )

  for (i in seq_len(nrow(reports))) {
    report <- reports[i, ]
    stage_folder <- report$stage
    if (is.na(stage_folder) || !nzchar(stage_folder)) stage_folder <- "99_Other"

    row_data <- list(
      Order = i, `Report Name` = report$name, Stage = stage_folder,
      `PDF Link` = NA_character_, `DOCX Link` = NA_character_, `HTML Link` = NA_character_,
      `Modified Date` = NA_character_, Description = report$description
    )

    pdf_output_path <- sub("\\.qmd$", ".pdf", report$path)
    pdf_exists <- file.exists(pdf_output_path)

    for (fmt in output_formats) {
      output_path <- sub("\\.qmd$", paste0(".", fmt), report$path)
      staged_output_name <- paste0(staged_stems[i], ".", fmt)

      if (fmt == "pdf") {
        if (pdf_exists) {
          dest_dir <- file.path(build_dir, fmt, stage_folder)
          if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
          file.copy(output_path, file.path(dest_dir, staged_output_name), overwrite = TRUE)
          row_data$`PDF Link` <- file.path(".", fmt, stage_folder, staged_output_name)
          row_data$`Modified Date` <- format(file.info(output_path)$mtime, "%Y-%m-%d %H:%M")
        }
      } else if (fmt == "docx") {
        docx_exists <- file.exists(output_path)
        is_stale_docx <- docx_exists && pdf_exists && file.info(output_path)$mtime < file.info(pdf_output_path)$mtime

        if (pdf_exists && (!docx_exists || is_stale_docx) && !is.null(docx_from_pdf)) {
          docx_from_pdf(pdf_output_path, output_path)
          docx_exists <- file.exists(output_path)
        }

        if (docx_exists) {
          dest_dir <- file.path(build_dir, fmt, stage_folder)
          if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
          file.copy(output_path, file.path(dest_dir, staged_output_name), overwrite = TRUE)
          row_data$`DOCX Link` <- file.path(".", fmt, stage_folder, staged_output_name)
          row_data$`Modified Date` <- format(file.info(output_path)$mtime, "%Y-%m-%d %H:%M")
        }
      } else {
        if (file.exists(output_path)) {
          dest_dir <- file.path(build_dir, fmt, stage_folder)
          if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
          file.copy(output_path, file.path(dest_dir, staged_output_name), overwrite = TRUE)
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
    for (data_file in data_deliverables) {
      if (file.exists(data_file)) file.copy(data_file, data_dir)
    }
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Report Index")
  openxlsx::writeData(wb, "Report Index", index_data, startRow = 1, startCol = 1,
                       headerStyle = openxlsx::createStyle(textDecoration = "bold"))

  for (r in seq_len(nrow(index_data))) {
    for (col in c("PDF Link", "DOCX Link", "HTML Link")) {
      link <- index_data[[col]][r]
      if (!is.na(link)) {
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
  output_zip_path <- normalizePath(file.path(output_dir, zip_name), winslash = "/", mustWork = FALSE)

  old_wd <- setwd(build_dir)
  on.exit(setwd(old_wd), add = TRUE)
  files_to_zip <- list.files(".", recursive = TRUE)

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = output_zip_path, files = files_to_zip)
  } else {
    utils::zip(zipfile = output_zip_path, files = files_to_zip, flags = "-r9Xq")
  }

  output_zip_path
}
