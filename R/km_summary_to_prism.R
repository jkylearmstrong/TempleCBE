#' Convert a Kaplan-Meier Summary Table to a GraphPad Prism Survival Table
#'
#' Expands a summary-by-time table of event and censoring counts (as from
#' `summary(survival::survfit(...))`) into Prism's survival data layout: one
#' row per subject, an `X` column of times, and one column per group holding 1
#' for an event, 0 for censored, and `NA` for the other groups.
#'
#' @param x A data frame, or a path to an `.xlsx`, `.xls`, `.csv`, or `.txt`
#'   file, with one row per time per group.
#' @param sheet Sheet name or index when `x` is an Excel file.
#' @param time_col,strata_col,n_event_col,n_censor_col Column names of the
#'   time, group, number of events, and number censored.
#' @param strata_levels Optional group order for the output columns. Every
#'   group in the data must be listed.
#' @param drop_time0 Drop rows at time 0 (default `TRUE`).
#' @param validate_totals If `TRUE` (default) and `x` has an `n.risk` column,
#'   warn when a group's `n.risk` at its earliest time differs from its total
#'   events plus censored.
#' @param out_xlsx Optional path to write the table as Excel (requires
#'   `writexl`).
#' @param out_xlsx_paste Optional path to write a copy with blanks instead of
#'   `NA`, convenient for pasting into Prism.
#' @return A data frame with column `X` and one integer column per group,
#'   with a `"notes"` attribute describing the layout.
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#'   s <- summary(fit, censored = TRUE)
#'   km <- data.frame(time = s$time, strata = s$strata, n.risk = s$n.risk,
#'                    n.event = s$n.event, n.censor = s$n.censor)
#'   head(km_summary_to_prism(km))
#' }
km_summary_to_prism <- function(x,
                                sheet = NULL,
                                time_col = "time",
                                strata_col = "strata",
                                n_event_col = "n.event",
                                n_censor_col = "n.censor",
                                strata_levels = NULL,
                                drop_time0 = TRUE,
                                validate_totals = TRUE,
                                out_xlsx = NULL,
                                out_xlsx_paste = NULL) {
  df <- read_km_summary(x, sheet)

  needed <- c(time_col, strata_col, n_event_col, n_censor_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df0 <- as.data.frame(df)[, intersect(c(needed, "n.risk"), names(df)), drop = FALSE]
  df0[[time_col]] <- suppressWarnings(as.numeric(df0[[time_col]]))
  keep <- !is.na(df0[[time_col]])
  if (drop_time0) keep <- keep & df0[[time_col]] != 0
  df0 <- df0[keep, , drop = FALSE]
  df0 <- df0[order(df0[[time_col]]), , drop = FALSE]

  long <- dplyr::select(df0, dplyr::all_of(c(time_col, strata_col, n_event_col, n_censor_col)))
  long <- stats::setNames(long, c("X", ".strata", ".nevent", ".ncensor"))
  long$.strata <- as.character(long$.strata)
  long <- tidyr::pivot_longer(long, cols = c(".nevent", ".ncensor"), names_to = ".type", values_to = ".count")
  long$.status <- ifelse(long$.type == ".nevent", 1L, 0L)
  long <- long[!is.na(long$.count) & long$.count > 0, , drop = FALSE]
  if (nrow(long) == 0) {
    stop("No positive counts in the event or censored columns after filtering.", call. = FALSE)
  }
  expanded <- tidyr::uncount(long, weights = .data$.count)

  present <- unique(expanded$.strata)
  if (is.null(strata_levels)) {
    groups <- sort(present)
  } else {
    unknown <- setdiff(present, strata_levels)
    if (length(unknown) > 0) {
      stop("Group(s) not in `strata_levels`: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    groups <- intersect(strata_levels, present)
  }

  out <- data.frame(X = expanded$X, check.names = FALSE)
  status <- matrix(NA_integer_, nrow = nrow(expanded), ncol = length(groups))
  status[cbind(seq_len(nrow(expanded)), match(expanded$.strata, groups))] <- expanded$.status
  out[groups] <- as.data.frame(status)

  if (validate_totals && "n.risk" %in% names(df)) {
    warn_km_totals(df, expanded, time_col, strata_col)
  }

  if (!is.null(out_xlsx) || !is.null(out_xlsx_paste)) {
    require_packages("writexl", "km_summary_to_prism")
  }
  if (!is.null(out_xlsx)) {
    writexl::write_xlsx(out, out_xlsx)
  }
  if (!is.null(out_xlsx_paste)) {
    pasted <- out
    pasted[groups] <- lapply(pasted[groups], function(col) ifelse(is.na(col), "", col))
    writexl::write_xlsx(pasted, out_xlsx_paste)
  }

  attr(out, "notes") <- c(
    "Prism survival table: X = time; one column per group; 1 = event, 0 = censored; one non-missing group value per row.",
    "Paste or import into a Prism 'Survival' table; Prism builds the Kaplan-Meier curves and log-rank tests."
  )
  out
}

#' Read a Kaplan-Meier Summary From a Data Frame or File
#'
#' @keywords internal
#' @noRd
read_km_summary <- function(x, sheet = NULL) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (!is.character(x) || length(x) != 1) {
    stop("`x` must be a data frame or a path to an .xlsx, .xls, .csv, or .txt file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(x))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet)) readxl::read_excel(x) else readxl::read_excel(x, sheet = sheet)
  } else if (ext %in% c("csv", "txt")) {
    utils::read.csv(x, check.names = FALSE)
  } else {
    stop("Unsupported file extension: .", ext, call. = FALSE)
  }
}

#' Warn When Initial `n.risk` Disagrees With Events Plus Censored
#'
#' @keywords internal
#' @noRd
warn_km_totals <- function(df, expanded, time_col, strata_col) {
  initial <- data.frame(
    .strata = as.character(df[[strata_col]]),
    .time = suppressWarnings(as.numeric(df[[time_col]])),
    initial_n = df[["n.risk"]]
  )
  initial <- initial[!is.na(initial$.time), , drop = FALSE]
  initial <- initial[order(initial$.strata, initial$.time), , drop = FALSE]
  initial <- initial[!duplicated(initial$.strata), c(".strata", "initial_n")]

  totals <- as.data.frame(table(.strata = expanded$.strata), responseName = "from_counts")
  totals$.strata <- as.character(totals$.strata)
  check <- merge(initial, totals, by = ".strata")
  bad <- check[!is.na(check$initial_n) & check$initial_n != check$from_counts, , drop = FALSE]
  if (nrow(bad) > 0) {
    warning(
      "Initial n.risk differs from events + censored for: ",
      paste0(bad$.strata, " (n.risk = ", bad$initial_n, ", counts = ", bad$from_counts, ")", collapse = "; "),
      call. = FALSE
    )
  }
  invisible(bad)
}
