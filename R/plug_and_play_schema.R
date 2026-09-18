#' Plug-and-Play Data Dictionary & Schema Mapping Engine
#'
#' Tools to validate column mapping dictionaries, read disparate raw data files
#' according to declared schemas, resolve duplicate/twin columns, and generate
#' longitudinal clinical summaries.
#'
#' @keywords internal
#' @name plug_and_play_schema
NULL

#' Validate a Plug-and-Play Column Mapping
#'
#' Verifies that a mapping dictionary has the required schema and logical constraints:
#' columns \code{INDEX}, \code{old}, \code{new}, \code{X_var}, \code{Y_var}, \code{ID_var},
#' \code{Time_var}, \code{duplicate_of}, and \code{duplicate_action}.
#'
#' @param mapping A data frame/tibble containing the column mapping rules.
#' @return \code{TRUE}, invisibly, if valid; otherwise raises an error listing all issues.
#' @export
validate_column_mapping <- function(mapping) {
  required_cols <- c(
    "INDEX", "old", "new", "X_var", "Y_var", "ID_var", "Time_var",
    "duplicate_of", "duplicate_action"
  )
  missing_cols <- setdiff(required_cols, names(mapping))
  if (length(missing_cols) > 0) {
    stop(
      "`mapping` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  problems <- character(0)

  role_cols <- c("X_var", "Y_var", "ID_var", "Time_var")
  non_logical <- role_cols[!vapply(mapping[role_cols], is.logical, logical(1))]
  if (length(non_logical) > 0) {
    problems <- c(problems, paste0(
      "Column(s) must be logical (TRUE/FALSE): ", paste(non_logical, collapse = ", ")
    ))
  }

  bad_action <- unique(stats::na.omit(mapping$duplicate_action))
  bad_action <- setdiff(bad_action, c("drop", "prefer"))
  if (length(bad_action) > 0) {
    problems <- c(problems, paste0(
      "duplicate_action must be 'drop', 'prefer', or NA; found: ",
      paste(bad_action, collapse = ", ")
    ))
  }

  action_without_dup <- which(!is.na(mapping$duplicate_action) & is.na(mapping$duplicate_of))
  if (length(action_without_dup) > 0) {
    problems <- c(problems, paste0(
      "Row(s) have duplicate_action set but duplicate_of is NA: rows ",
      paste(action_without_dup, collapse = ", ")
    ))
  }

  target_names <- unique(stats::na.omit(mapping$duplicate_of))
  unknown_targets <- setdiff(target_names, mapping$new)
  if (length(unknown_targets) > 0) {
    problems <- c(problems, paste0(
      "duplicate_of references `new` name(s) not present in the mapping: ",
      paste(unknown_targets, collapse = ", ")
    ))
  }

  # Check duplicate new names per INDEX section
  sections <- split(mapping, mapping$INDEX)
  for (sec_name in names(sections)) {
    sec <- sections[[sec_name]]
    non_dropped <- sec[is.na(sec$duplicate_action) | sec$duplicate_action != "drop", , drop = FALSE]
    dup_news <- non_dropped$new[duplicated(non_dropped$new)]
    if (length(dup_news) > 0) {
      problems <- c(problems, paste0(
        "INDEX '", sec_name, "' has duplicate active `new` name(s) without `duplicate_action = 'drop'`: ",
        paste(unique(dup_news), collapse = ", ")
      ))
    }
  }

  if (length(problems) > 0) {
    stop("Mapping validation failed:\n - ", paste(problems, collapse = "\n - "), call. = FALSE)
  }

  invisible(TRUE)
}

#' Find Section Data File in a Directory
#'
#' @param data_dir Directory to search.
#' @param index Section name/token to look for.
#' @return File path.
#' @export
find_section_file <- function(data_dir, index) {
  files <- list.files(data_dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  matches <- files[grepl(index, basename(files), ignore.case = TRUE)]
  if (length(matches) == 0) {
    stop("No .csv or .xlsx file in '", data_dir, "' matching INDEX '", index, "'.", call. = FALSE)
  }
  matches[[1]]
}

#' Read Raw Table with Multi-Row Header Support
#'
#' @param file Path to .csv or .xlsx file.
#' @param sheet Sheet name or number (for Excel).
#' @param header_rows Number of rows to combine into column headers (default: 1).
#' @return A tibble with raw data.
#' @export
read_raw_table <- function(file, sheet = NULL, header_rows = 1) {
  is_xlsx <- grepl("\\.xlsx$", file, ignore.case = TRUE)

  if (is_xlsx) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("readxl package required to read .xlsx files.", call. = FALSE)
    }
    raw <- if (is.null(sheet)) readxl::read_excel(file, col_names = FALSE) else readxl::read_excel(file, sheet = sheet, col_names = FALSE)
  } else {
    raw <- utils::read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  }

  if (header_rows <= 1) {
    colnames(raw) <- as.character(unlist(raw[1, ]))
    out <- raw[-1, , drop = FALSE]
  } else {
    hdr_matrix <- as.matrix(raw[seq_len(header_rows), , drop = FALSE])
    combined_names <- apply(hdr_matrix, 2, function(vals) {
      vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      paste(vals, collapse = " | ")
    })
    colnames(raw) <- combined_names
    out <- raw[-seq_len(header_rows), , drop = FALSE]
  }

  tibble::as_tibble(out)
}

#' Read One Section's Data Using a Column Mapping
#'
#' Generic schema-enforced reader: reads whichever file corresponds to an \code{INDEX}
#' value in a mapping table, renames raw headers to standardized names, resolves duplicate
#' columns per \code{duplicate_action}, and tolerates missing or unmapped columns.
#'
#' @param mapping Validated column mapping table.
#' @param index Character string identifying the section/domain in \code{mapping$INDEX}.
#' @param file Optional explicit path to the data file.
#' @param data_dir Directory to search for the section's file if \code{file} is NULL.
#' @param sheet Sheet name or number for Excel files.
#' @param header_rows Number of header rows to concatenate (default: 1).
#' @return A tibble with standardized column names.
#' @export
read_mapped_section_data <- function(mapping,
                                     index,
                                     file = NULL,
                                     data_dir = NULL,
                                     sheet = NULL,
                                     header_rows = 1) {
  validate_column_mapping(mapping)

  section_map <- dplyr::filter(mapping, .data$INDEX == index)
  if (nrow(section_map) == 0) {
    stop("No rows in `mapping` for INDEX = '", index, "'.", call. = FALSE)
  }

  kept <- is.na(section_map$duplicate_action) | section_map$duplicate_action != "drop"
  section_map <- section_map[kept, , drop = FALSE]

  if (is.null(file)) {
    if (is.null(data_dir)) {
      stop("Either `file` or `data_dir` must be supplied.", call. = FALSE)
    }
    file <- find_section_file(data_dir, index)
  }

  raw <- read_raw_table(file, sheet = sheet, header_rows = header_rows)

  missing_old <- setdiff(section_map$old, names(raw))
  if (length(missing_old) > 0) {
    missing_new <- section_map$new[match(missing_old, section_map$old)]
    warning(
      "INDEX '", index, "': mapped column(s) not found in data file, filled with NA: ",
      paste(missing_new, collapse = ", "),
      call. = FALSE
    )
  }

  extra_cols <- setdiff(names(raw), section_map$old)
  if (length(extra_cols) > 0) {
    warning(
      "INDEX '", index, "': unmapped column(s) dropped: ",
      paste(extra_cols, collapse = ", "),
      call. = FALSE
    )
  }

  present_map <- section_map[section_map$old %in% names(raw), , drop = FALSE]
  out <- raw[, present_map$old, drop = FALSE]
  names(out) <- present_map$new

  if (length(missing_old) > 0) {
    for (m_new in missing_new) {
      out[[m_new]] <- if (nrow(out) > 0) NA else logical(0)
    }
  }

  out[, section_map$new, drop = FALSE]
}

#' Summarize One Section By Its Time Variable
#'
#' Generates summary tables (using \pkg{gtsummary} by default, or optionally
#' \pkg{arsenal}) grouped by whichever column is flagged with \code{Time_var == TRUE}
#' in the mapping dictionary.
#'
#' @param df Data frame already renamed via \code{\link{read_mapped_section_data}}.
#' @param mapping Validated column mapping table.
#' @param index Character string identifying the section/domain.
#' @param id_cols Column names to exclude from summary (defaults to columns with \code{ID_var == TRUE}).
#' @param engine Character string specifying the summary engine: \code{"gtsummary"}
#'   (the default) or \code{"arsenal"}.
#' @return A \code{\link[gtsummary]{tbl_summary}} object (when \code{engine = "gtsummary"})
#'   or an \code{arsenal::tableby} summary object (when \code{engine = "arsenal"}).
#' @export
summarize_section_by_time <- function(df, mapping, index, id_cols = NULL,
                                      engine = c("gtsummary", "arsenal")) {
  engine <- match.arg(engine)
  section_map <- dplyr::filter(mapping, .data$INDEX == index)

  if (is.null(id_cols)) {
    id_cols <- section_map$new[section_map$ID_var]
  }
  id_cols <- intersect(id_cols, names(df))

  time_cols <- intersect(section_map$new[section_map$Time_var], names(df))
  body <- df[, setdiff(names(df), id_cols), drop = FALSE]

  non_time <- setdiff(names(body), time_cols)
  all_na <- non_time[vapply(body[non_time], function(x) all(is.na(x)), logical(1))]
  body <- body[, setdiff(names(body), all_na), drop = FALSE]

  if (ncol(body) == 0) {
    stop("Nothing left to summarize for INDEX '", index, "' -- every column is entirely missing.", call. = FALSE)
  }

  if (engine == "gtsummary") {
    if (length(time_cols) == 0) {
      return(gtsummary::tbl_summary(body))
    }

    time_col <- time_cols[[1]]
    if (length(setdiff(names(body), time_col)) == 0) {
      stop("Nothing left to summarize for INDEX '", index, "' -- every non-time column is entirely missing.", call. = FALSE)
    }

    return(gtsummary::tbl_summary(body, by = dplyr::all_of(time_col)))
  }

  # Fallback engine: arsenal
  if (!requireNamespace("arsenal", quietly = TRUE)) {
    stop("arsenal package required for summarize_section_by_time() when engine = 'arsenal'.", call. = FALSE)
  }

  if (length(time_cols) == 0) {
    return(arsenal::tableby(~., data = body))
  }

  time_col <- time_cols[[1]]
  if (length(setdiff(names(body), time_col)) == 0) {
    stop("Nothing left to summarize for INDEX '", index, "' -- every non-time column is entirely missing.", call. = FALSE)
  }

  form <- stats::as.formula(paste0("`", time_col, "` ~ ."))
  arsenal::tableby(form, data = body)
}

#' Create Sample Plug-and-Play Mapping Dictionary
#'
#' Generates a demonstration column mapping table for documentation and testing.
#'
#' @return A tibble with sample mapping entries.
#' @export
demo_cbe_mapping <- function() {
  tibble::tribble(
    ~INDEX, ~old,       ~new,         ~X_var, ~Y_var, ~ID_var, ~Time_var, ~duplicate_of, ~duplicate_action,
    "Demo", "Subject",  "subject_id", FALSE,  FALSE,  TRUE,    FALSE,     NA_character_, NA_character_,
    "Demo", "Visit",    "time_point", FALSE,  FALSE,  FALSE,   TRUE,      NA_character_, NA_character_,
    "Demo", "MAP_mmHg", "map_mean",   TRUE,   FALSE,  FALSE,   FALSE,     NA_character_, NA_character_,
    "Demo", "Outcome",  "status",     FALSE,  TRUE,   FALSE,   FALSE,     NA_character_, NA_character_
  )
}
