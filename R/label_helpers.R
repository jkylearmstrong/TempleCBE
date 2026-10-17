#' Variable Label and Excel Header Helpers
#'
#' Utilities for converting raw or snake_case column names to human-readable
#' display names, formatting Excel-safe column headers, and copying variable
#' labels across data frames.
#'
#' @name label_helpers
NULL

#' Convert Raw Variable Names to Human-Readable Display Labels
#'
#' Converts snake_case, camelCase, or dot-delimited variable names into clean,
#' title-cased labels suitable for tables and figures. Preserves common clinical
#' acronyms (e.g. BMI, ID, IBS, HR, SD, IQR, CBE, RNA, DNA).
#'
#' @param x Character vector of variable names.
#' @param acronyms Optional character vector of terms to keep uppercase.
#' @return Character vector of formatted display labels.
#' @export
#' @examples
#' pretty_name(c("patient_bmi", "follow_up_time", "hr_ratio_sd"))
pretty_name <- function(x, acronyms = c("BMI", "ID", "IBS", "HR", "SD", "IQR", "CBE", "RNA", "DNA", "CV", "AUC", "ROC", "IPCW")) {
  if (is.null(x)) return(character())
  x <- as.character(x)

  # Replace underscores, dots, hyphens with spaces
  clean <- gsub("[._-]+", " ", x)
  # Insert space before camelCase capitals
  clean <- gsub("([a-z])([A-Z])", "\\1 \\2", clean)
  # Trim excess whitespace
  clean <- trimws(gsub("\\s+", " ", clean))

  # Title case
  res <- tools::toTitleCase(clean)

  # Restore acronyms
  if (length(acronyms) > 0) {
    for (acr in acronyms) {
      pattern <- paste0("\\b", acr, "\\b")
      res <- gsub(pattern, acr, res, ignore.case = TRUE)
    }
  }

  res
}

#' Sanitize Column Names for Excel Worksheets
#'
#' Replaces characters forbidden by Excel (e.g. `\ / ? * : [ ]`) with underscores
#' or spaces, trims leading/trailing spaces, and optionally truncates names.
#'
#' @param names Character vector of proposed column or sheet names.
#' @param max_char Optional integer maximum character length (e.g. 31 for Excel sheet names).
#' @param replacement Replacement character for invalid symbols (default `"_"`).
#' @return Character vector of Excel-safe names.
#' @export
#' @examples
#' excel_colnames(c("Patient/Subject [ID]", "Cost ($/hr)?"))
excel_colnames <- function(names, max_char = NULL, replacement = "_") {
  if (is.null(names)) return(character())
  names <- as.character(names)

  # Forbidden Excel characters: \ / ? * : [ ]
  cleaned <- gsub("[\\\\/?*:[\\]]", replacement, names)
  # Remove duplicate replacement characters
  if (nzchar(replacement)) {
    pattern <- paste0(replacement, "+")
    cleaned <- gsub(pattern, replacement, cleaned)
  }
  cleaned <- trimws(cleaned)

  if (!is.null(max_char) && is.numeric(max_char) && max_char > 0) {
    cleaned <- substr(cleaned, 1L, as.integer(max_char))
  }

  # Ensure names are non-empty
  empty_idx <- which(is.na(cleaned) | cleaned == "")
  if (length(empty_idx) > 0) {
    cleaned[empty_idx] <- paste0("Col_", empty_idx)
  }

  cleaned
}

#' Copy Variable Labels and Attributes Across Data Frames
#'
#' Transfers column-level attributes (such as `label`, `labels`, and `format.sas`)
#' from a source data frame (`from`) to matching columns in a target data frame (`to`).
#'
#' @param from Source data frame containing labelled variables.
#' @param to Target data frame to receive variable attributes.
#' @param attrs Character vector of attribute names to copy (default `c("label", "labels", "format.sas")`).
#' @return Target data frame `to` with transferred attributes.
#' @export
#' @examples
#' df1 <- data.frame(age = 50, bmi = 24.5)
#' attr(df1$age, "label") <- "Age at Enrollment (years)"
#' df2 <- data.frame(age = 60, bmi = 28.0)
#' df2 <- copy_var_labels(df1, df2)
#' attr(df2$age, "label")
copy_var_labels <- function(from, to, attrs = c("label", "labels", "format.sas")) {
  if (!is.data.frame(from) || !is.data.frame(to)) {
    stop("Both `from` and `to` must be data frames.", call. = FALSE)
  }

  common_cols <- intersect(names(from), names(to))
  for (col in common_cols) {
    for (att in attrs) {
      val <- attr(from[[col]], att, exact = TRUE)
      if (!is.null(val)) {
        attr(to[[col]], att) <- val
      }
    }
  }

  to
}

.cbe_label_env <- new.env(parent = emptyenv())
.cbe_label_env$map <- NULL
.cbe_label_env$style <- "short"

#' Normalize a Label Mapping Object Into a Canonical `cbe_label_map` Tibble
#'
#' @param map Named character vector, named list of `c(short, full)`, data frame
#'   with `term`/`short`/`full` columns, or `cbe_label_map`.
#' @param style `"short"` or `"full"`.
#' @return A `cbe_label_map` tibble with columns `term`, `short`, `full`.
#' @keywords internal
#' @noRd
as_cbe_label_map <- function(map, style = c("short", "full")) {
  style <- match.arg(style)
  if (is.null(map)) {
    return(NULL)
  }
  if (inherits(map, "cbe_label_map") && is.data.frame(map) && all(c("term", "short", "full") %in% names(map))) {
    attr(map, "style") <- style
    return(map)
  }
  if (is.data.frame(map)) {
    nms <- tolower(names(map))
    term_idx <- match(TRUE, nms %in% c("term", "variable", "var", "name", "id"), nomatch = 1L)
    short_idx <- match(TRUE, nms %in% c("short", "short_label", "label", "short_name"), nomatch = min(2L, ncol(map)))
    full_idx <- match(TRUE, nms %in% c("full", "full_label", "long", "description", "full_name"), nomatch = NA_integer_)
    if (ncol(map) < 2L) {
      stop("A data frame `label_map` must have at least 2 columns (`term` and `short`).", call. = FALSE)
    }
    term_col <- as.character(map[[term_idx]])
    short_col <- as.character(map[[short_idx]])
    full_col <- if (!is.na(full_idx)) {
      as.character(map[[full_idx]])
    } else if (ncol(map) >= 3L && !3L %in% c(term_idx, short_idx)) {
      as.character(map[[3L]])
    } else {
      short_col
    }
    out <- tibble::tibble(term = term_col, short = short_col, full = full_col)
  } else if (is.character(map) && !is.null(names(map))) {
    out <- tibble::tibble(
      term = names(map),
      short = unname(as.character(map)),
      full = unname(as.character(map))
    )
  } else if (is.list(map) && !is.null(names(map))) {
    terms_v <- names(map)
    shorts <- character(length(map))
    fulls <- character(length(map))
    for (i in seq_along(map)) {
      el <- map[[i]]
      if (is.null(el) || length(el) == 0L) {
        shorts[i] <- terms_v[i]
        fulls[i] <- terms_v[i]
      } else if (!is.null(names(el)) && any(c("short", "full") %in% names(el))) {
        s_val <- if ("short" %in% names(el)) as.character(el[["short"]])[1] else as.character(el[[1]])[1]
        f_val <- if ("full" %in% names(el)) as.character(el[["full"]])[1] else s_val
        shorts[i] <- s_val
        fulls[i] <- f_val
      } else {
        el_c <- as.character(unlist(el))
        shorts[i] <- el_c[1]
        fulls[i] <- if (length(el_c) >= 2L) el_c[2] else el_c[1]
      }
    }
    out <- tibble::tibble(term = terms_v, short = shorts, full = fulls)
  } else {
    stop("`map` must be a named character vector, a named list of `c(short, full)`, or a data frame with `term`, `short`, and `full` columns.", call. = FALSE)
  }

  attr(out, "style") <- style
  class(out) <- c("cbe_label_map", class(out))
  out
}

#' Set or Retrieve the Package-Level Variable Label Map
#'
#' Configures or retrieves a unified mapping from raw model/dataset variable
#' names (`term`) to `"short"` labels (for compact figure axes) and `"full"`
#' labels (for tables, legends, and report appendices). Plot and table helpers
#' across \pkg{TempleCBE} (`plot_tune_*`, `tbl_tune_*`, `plot_cox_forest*`,
#' `cbe_cox_table`, `plot_permutation_importance`, `tbl_permutation_importance`,
#' `summarize_outer_importance`, `plot_outer_importance`,
#' `tbl_censoring_comparison`) honour both the active package map set via
#' `cbe_labels()` and an explicit `label_map =` argument.
#'
#' @param map Optional label map:
#'   * A data frame or tibble with columns `term`, `short`, and (optionally) `full`.
#'   * A named list mapping each variable name to `c(short = "...", full = "...")`
#'     or a length-2 character vector `c("short", "full")`.
#'   * A named character vector `c(var = "Label")`.
#'   * `NULL` (default) to query the currently active label map or change its
#'     active `style`.
#' @param style Default label style to apply when resolving labels: `"short"`
#'   (default) or `"full"`.
#' @param reset Logical (default `FALSE`). When `TRUE`, clears the active
#'   package-level label map and resets `style` to `"short"`.
#' @return A `cbe_label_map` tibble with columns `term`, `short`, and `full`
#'   (or `NULL` when no map is set).
#' @seealso [tbl_label_map()], [pretty_name()]
#' @export
#' @examples
#' cbe_labels(
#'   tibble::tibble(
#'     term = c("x1", "x2"),
#'     short = c("FEV1", "DLCO"),
#'     full = c("Forced Expiratory Volume in 1s (L)", "Diffusing Capacity for CO")
#'   ),
#'   style = "short"
#' )
#' tbl_label_map()
#' cbe_labels(reset = TRUE)
cbe_labels <- function(map = NULL,
                       style = c("short", "full"),
                       reset = FALSE) {
  if (isTRUE(reset)) {
    .cbe_label_env$map <- NULL
    .cbe_label_env$style <- "short"
    options(templecbe.label_map = NULL)
    return(invisible(NULL))
  }

  has_style <- !missing(style) && !is.null(style)
  if (has_style) {
    style <- match.arg(style)
    .cbe_label_env$style <- style
  } else {
    style <- .cbe_label_env$style %||% "short"
  }

  if (!is.null(map)) {
    norm_map <- as_cbe_label_map(map, style = style)
    .cbe_label_env$map <- norm_map
    options(templecbe.label_map = norm_map)
    return(norm_map)
  }

  cur <- .cbe_label_env$map %||% getOption("templecbe.label_map")
  if (!is.null(cur) && has_style) {
    attr(cur, "style") <- style
    .cbe_label_env$map <- cur
    options(templecbe.label_map = cur)
  }
  cur
}

#' Format a Variable Label Map Table for Report Appendices
#'
#' Produces a summary table listing raw variable names (`term`), short figure
#' labels (`short`), and full descriptive names (`full`) for inclusion in a
#' report legend or appendix.
#'
#' @param label_map Optional label map (anything accepted by [cbe_labels()]).
#'   Defaults to the active package-level map from [cbe_labels()].
#' @param terms Optional character vector of variable names to include (and order
#'   by).
#' @param format Output format: `"tibble"` (default) or `"gt"` (requires \pkg{gt}).
#' @return A tibble with columns `term`, `short`, and `full`, or a `gt_tbl`
#'   object when `format = "gt"`.
#' @seealso [cbe_labels()]
#' @export
tbl_label_map <- function(label_map = NULL,
                          terms = NULL,
                          format = c("tibble", "gt")) {
  format <- match.arg(format)
  eff_map <- if (!is.null(label_map)) {
    as_cbe_label_map(label_map)
  } else {
    cbe_labels()
  }
  if (is.null(eff_map)) {
    if (!is.null(terms)) {
      eff_map <- tibble::tibble(
        term = as.character(terms),
        short = as.character(terms),
        full = pretty_name(terms)
      )
    } else {
      stop("No `label_map` provided and no active package label map set via `cbe_labels()`.", call. = FALSE)
    }
  }

  out <- tibble::as_tibble(eff_map)[, c("term", "short", "full"), drop = FALSE]
  if (!is.null(terms)) {
    terms <- unique(as.character(terms))
    idx <- match(terms, out$term)
    found <- !is.na(idx)
    out <- dplyr::bind_rows(
      out[idx[found], , drop = FALSE],
      if (any(!found)) tibble::tibble(
        term = terms[!found],
        short = terms[!found],
        full = pretty_name(terms[!found])
      ) else NULL
    )
  }

  if (format == "gt") {
    require_packages("gt", "tbl_label_map(format = 'gt')")
    gt_tbl <- gt::gt(out)
    gt_tbl <- gt::tab_header(
      gt_tbl,
      title = "Variable Label Reference",
      subtitle = "Short figure labels and full variable descriptions"
    )
    gt_tbl <- gt::cols_label(
      gt_tbl,
      term = "Variable",
      short = "Short Label",
      full = "Full Description"
    )
    return(gt_tbl)
  }

  out
}

#' @rdname tbl_label_map
#' @export
cbe_label_table <- tbl_label_map

#' Resolve Variable Names Through a Label Map
#'
#' @param x Character vector of variable/term names.
#' @param label_map Optional explicit label map, or `NULL` to use active [cbe_labels()].
#' @param style Optional style override (`"short"` or `"full"`).
#' @return Character vector of resolved labels with the same length as `x`.
#' @keywords internal
#' @noRd
resolve_cbe_labels <- function(x, label_map = NULL, style = NULL) {
  if (is.null(x)) return(character())
  x_chr <- as.character(x)
  if (length(x_chr) == 0L) return(x_chr)

  eff_map <- if (!is.null(label_map)) {
    as_cbe_label_map(label_map, style = if (!is.null(style)) match.arg(style, c("short", "full")) else (attr(label_map, "style") %||% .cbe_label_env$style %||% "short"))
  } else {
    cbe_labels()
  }
  if (is.null(eff_map) || nrow(eff_map) == 0L) {
    return(x_chr)
  }

  eff_style <- if (!is.null(style)) {
    match.arg(style, c("short", "full"))
  } else {
    attr(eff_map, "style") %||% .cbe_label_env$style %||% "short"
  }

  target_col <- if (identical(eff_style, "full")) eff_map$full else eff_map$short
  lookup <- stats::setNames(target_col, eff_map$term)

  idx <- match(x_chr, names(lookup))
  matched <- !is.na(idx) & !is.na(x_chr)
  out <- x_chr
  out[matched] <- unname(lookup[idx[matched]])
  out
}

#' Resolve `+`-Separated Formula Strings Through a Label Map
#'
#' @keywords internal
#' @noRd
resolve_cbe_formula_labels <- function(formulas, label_map = NULL, style = NULL) {
  if (is.null(formulas)) return(character())
  f_chr <- as.character(formulas)
  eff_map <- if (!is.null(label_map)) label_map else cbe_labels()
  if (is.null(eff_map)) return(f_chr)

  vapply(f_chr, function(f) {
    if (is.na(f) || !nzchar(trimws(f))) return(f)
    parts <- trimws(strsplit(f, "\\+")[[1]])
    mapped <- resolve_cbe_labels(parts, label_map = label_map, style = style)
    paste(mapped, collapse = " + ")
  }, character(1), USE.NAMES = FALSE)
}

