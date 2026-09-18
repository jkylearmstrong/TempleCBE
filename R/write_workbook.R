#' Write an R Database or Data Frames to an Excel Workbook
#'
#' Writes a named list of data frames (an R database) or a single data frame to
#' a multi-worksheet Excel workbook, with an optional automated data dictionary
#' worksheet.
#'
#' @param x A named list of data frames (an R database) or a single data frame/tibble.
#' @param path File path to save the \code{.xlsx} workbook.
#' @param include_metadata Logical; if \code{TRUE} (default), computes and appends
#'   a data dictionary sheet generated via \code{\link{get_dataset_info}}.
#' @param metadata Optional pre-computed metadata data frame. If \code{NULL} and
#'   \code{include_metadata = TRUE}, metadata is computed automatically.
#' @param metadata_sheet Character string naming the metadata worksheet (default: \code{"METADATA"}).
#' @param ... Additional arguments passed to \code{\link[writexl]{write_xlsx}}.
#' @return The file path invisibly.
#' @name write_database_to_excel
#' @export
#' @examples
#' \dontrun{
#' db <- list(patients = data.frame(id = 1:3), labs = data.frame(id = 1:3, val = 4:6))
#' write_database_to_excel(db, "clinical_database.xlsx")
#' }
write_database_to_excel <- function(x, path,
                                    include_metadata = TRUE,
                                    metadata = NULL,
                                    metadata_sheet = "METADATA",
                                    ...) {
  if (is.data.frame(x)) {
    nm <- deparse(substitute(x))
    if (!nzchar(nm) || grepl("[\\(\\)]", nm)) nm <- "Data"
    x <- stats::setNames(list(x), nm)
  }

  if (!is.list(x)) {
    stop("`x` must be a data frame or named list of data frames.", call. = FALSE)
  }

  if (length(x) == 0) {
    stop("`x` is empty; nothing to write.", call. = FALSE)
  }

  # Ensure names exist and sanitize worksheet names for Excel (max 31 chars, valid chars)
  sheet_names <- names(x)
  if (is.null(sheet_names)) {
    sheet_names <- paste0("Table", seq_along(x))
  } else {
    sheet_names[sheet_names == ""] <- paste0("Table", which(sheet_names == ""))
  }

  clean_sheet_name <- function(nm) {
    nm <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm)
    if (nchar(nm) > 31) substr(nm, 1, 31) else nm
  }

  sheet_names <- vapply(sheet_names, clean_sheet_name, character(1))
  sheet_names <- make.unique(sheet_names, sep = "_")
  names(x) <- sheet_names

  # Convert list elements to data frames
  x <- lapply(x, as.data.frame)

  # Prepare sheets to write
  sheets_to_write <- x

  if (isTRUE(include_metadata)) {
    if (is.null(metadata)) {
      metadata <- get_dataset_info(x)
    }
    meta_df <- as.data.frame(metadata)
    meta_sheet_name <- clean_sheet_name(metadata_sheet)
    if (meta_sheet_name %in% names(sheets_to_write)) {
      meta_sheet_name <- make.unique(c(names(sheets_to_write), meta_sheet_name), sep = "_")[length(sheets_to_write) + 1]
    }
    sheets_to_write[[meta_sheet_name]] <- meta_df
  }

  writexl::write_xlsx(sheets_to_write, path = path, ...)
  invisible(path)
}

#' @rdname write_database_to_excel
#' @export
write_workbook <- write_database_to_excel

#' @rdname write_database_to_excel
#' @export
cbe_write_database <- write_database_to_excel

#' Read an R Database from an Excel Workbook
#'
#' Reads all worksheets from an Excel workbook into a named list of tibbles,
#' optionally extracting and attaching the \code{METADATA} sheet as an attribute.
#'
#' @param path Path to the \code{.xls}/\code{.xlsx} file.
#' @param include_metadata Logical; if \code{TRUE} (default) and a sheet named
#'   \code{metadata_sheet} is found, it is separated and attached as attribute
#'   \code{"metadata"} on the returned database list.
#' @param metadata_sheet Name of the metadata sheet to recognize (default: \code{"METADATA"}).
#' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}.
#' @return A named list of tibbles. If \code{include_metadata = TRUE} and the metadata sheet
#'   was found, it is accessible via \code{attr(result, "metadata")}.
#' @name read_database_from_excel
#' @export
#' @examples
#' \dontrun{
#' db <- read_database_from_excel("clinical_database.xlsx")
#' meta <- attr(db, "metadata")
#' }
read_database_from_excel <- function(path,
                                     include_metadata = TRUE,
                                     metadata_sheet = "METADATA",
                                     ...) {
  sheets <- readxl::excel_sheets(path)
  names(sheets) <- sheets
  out <- purrr::map(sheets, \(sheet) readxl::read_excel(path = path, sheet = sheet, ...))

  if (isTRUE(include_metadata)) {
    meta_idx <- which(tolower(names(out)) == tolower(metadata_sheet))
    if (length(meta_idx) > 0) {
      meta_data <- out[[meta_idx[1]]]
      out[[meta_idx[1]]] <- NULL
      attr(out, "metadata") <- meta_data
    }
  }

  out
}

#' @rdname read_database_from_excel
#' @export
cbe_read_database <- read_database_from_excel

#' Export or Import Dataset Metadata
#'
#' Exports and imports data dictionaries/metadata to and from \code{.csv} or
#' \code{.xlsx} formats for in-place review and editing.
#'
#' @param metadata A tibble/data.frame of dataset metadata (e.g. from \code{\link{get_dataset_info}}).
#' @param path File path ending in \code{.csv} or \code{.xlsx}.
#' @param sheet Optional sheet name when reading from Excel (defaults to 1 or \code{"METADATA"}).
#' @param ... Additional arguments passed to the underlying writer/reader.
#' @return For writers, \code{path} invisibly. For readers, a tibble of metadata.
#' @name database_metadata_io
#' @export
write_database_metadata <- function(metadata, path, ...) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    writexl::write_xlsx(list(METADATA = as.data.frame(metadata)), path = path, ...)
  } else {
    utils::write.csv(as.data.frame(metadata), file = path, row.names = FALSE, ...)
  }
  invisible(path)
}

#' @rdname database_metadata_io
#' @export
read_database_metadata <- function(path, sheet = NULL, ...) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    sheet_to_use <- if (!is.null(sheet)) {
      sheet
    } else if ("METADATA" %in% sheets) {
      "METADATA"
    } else {
      1
    }
    tibble::as_tibble(readxl::read_excel(path, sheet = sheet_to_use, ...))
  } else {
    tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...))
  }
}

#' Apply Metadata and Variable Roles to an R Database
#'
#' Synchronizes updated metadata (such as variable labels, dataset descriptions,
#' database names, and variable roles) back into a database list or data frame.
#'
#' @param db A named list of data frames (an R database) or a single data frame.
#' @param metadata A data frame/tibble of metadata containing columns \code{columns}
#'   and optionally \code{dataset_name}, \code{labels}, \code{dataset_label},
#'   \code{database_name}, \code{role}, \code{X_var}, \code{Y_var}, \code{ID_var}, \code{Time_var}.
#' @return The modified database or data frame with attributes and labels updated.
#' @export
#' @examples
#' db <- list(demo = data.frame(id = 1:2, age = c(20, 30)))
#' meta <- get_dataset_info(db)
#' meta$labels[meta$columns == "age"] <- "Age at Enrollment (years)"
#' db <- apply_database_metadata(db, meta)
apply_database_metadata <- function(db, metadata) {
  if (is.null(metadata) || nrow(metadata) == 0) return(db)

  is_single_df <- is.data.frame(db)
  if (is_single_df) {
    db_list <- list(Data = db)
  } else {
    db_list <- db
  }

  if ("database_name" %in% names(metadata)) {
    db_nm <- stats::na.omit(metadata$database_name)
    if (length(db_nm) > 0 && nzchar(as.character(db_nm[1]))) {
      cbe_database_name(db_list) <- as.character(db_nm[1])
    }
  }

  for (tbl_name in names(db_list)) {
    tbl_meta <- if ("dataset_name" %in% names(metadata)) {
      dplyr::filter(metadata, .data$dataset_name == tbl_name)
    } else {
      metadata
    }

    if (nrow(tbl_meta) == 0) next

    # Dataset label
    if ("dataset_label" %in% names(tbl_meta)) {
      ds_lbl <- stats::na.omit(tbl_meta$dataset_label)
      if (length(ds_lbl) > 0 && nzchar(as.character(ds_lbl[1]))) {
        cbe_dataset_label(db_list[[tbl_name]]) <- as.character(ds_lbl[1])
      }
    }

    # Column labels
    if ("labels" %in% names(tbl_meta) && "columns" %in% names(tbl_meta)) {
      for (r in seq_len(nrow(tbl_meta))) {
        col_nm <- tbl_meta$columns[r]
        lbl_val <- tbl_meta$labels[r]
        if (col_nm %in% names(db_list[[tbl_name]]) && !is.na(lbl_val) && nzchar(as.character(lbl_val))) {
          labelled::var_label(db_list[[tbl_name]][[col_nm]]) <- as.character(lbl_val)
        }
      }
    }

    # Variable role attributes
    role_cols <- intersect(c("role", "X_var", "Y_var", "ID_var", "Time_var", "variable_type"), names(tbl_meta))
    if (length(role_cols) > 0) {
      attr(db_list[[tbl_name]], "variable_roles") <- tbl_meta[, c("columns", role_cols), drop = FALSE]
    }
  }

  attr(db_list, "metadata") <- metadata

  if (is_single_df) {
    db_list[[1]]
  } else {
    db_list
  }
}
