#' Summarize a Data Frame's Columns
#'
#' Per-column metadata: class, variable label (if set via \pkg{labelled}),
#' mean/sd for numeric columns, most-frequent value, distinct-value count,
#' and missingness. \code{survival::Surv} columns are summarized from their
#' underlying time/status matrix rather than unrolled as plain numerics.
#'
#' @param df A data frame or tibble.
#' @return A tibble with one row per column of \code{df}: \code{dataset_name},
#'   \code{labels}, \code{columns}, \code{class}, \code{mean}, \code{sd},
#'   \code{most_freq}, \code{n_distinct}, \code{SumNa}, \code{PctNa}.
#' @export
#' @examples
#' get_dataset_info(mtcars)
get_dataset_info <- function(df) {
  dataset_name <- deparse(substitute(df))
  columns <- colnames(df)

  is_surv <- vapply(df, function(x) inherits(x, "Surv"), logical(1))
  surv_cols <- columns[is_surv]

  # is.numeric() is TRUE for Surv objects (they're numeric matrices under the
  # hood), so exclude them here rather than flattening time+status together.
  numeric_cols <- setdiff(names(dplyr::select(df, dplyr::where(is.numeric))), surv_cols)

  class <- unname(vapply(columns, function(col) {
    trimws(sub("labelled", "", paste0(class(df[[col]]), collapse = "")))
  }, character(1)))

  # labelled::var_label() reads attr(x, "label"), but a label set on a raw
  # time/status column is typically lost once it's wrapped in Surv() -- fall
  # back to checking attr(x, "label") directly on the column itself.
  var_labels <- tryCatch(labelled::var_label(df), error = function(e) list())
  label_for <- function(col) {
    lbl <- var_labels[[col]]
    if (is.null(lbl) || length(lbl) != 1 || is.na(lbl) || !nzchar(lbl)) {
      lbl <- attr(df[[col]], "label", exact = TRUE)
    }
    if (is.null(lbl) || length(lbl) != 1 || is.na(lbl) || !nzchar(as.character(lbl))) {
      col
    } else {
      as.character(lbl)
    }
  }
  labels <- tibble::tibble(columns = columns, labels = unname(vapply(columns, label_for, character(1))))

  na_info <- if (ncol(df) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::everything(), \(x) SumNa(x))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "SumNa") |>
      dplyr::mutate(PctNa = .data$SumNa / nrow(df))
  } else {
    tibble::tibble(columns = character(), SumNa = numeric(), PctNa = numeric())
  }

  # dplyr::n_distinct() recurses infinitely on Surv matrices (no vctrs method
  # for the class), so those columns are counted separately via base R below.
  n_distinct_cols <- setdiff(columns, surv_cols)
  n_distinct <- if (length(n_distinct_cols) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::all_of(n_distinct_cols), \(x) tryCatch(dplyr::n_distinct(x), error = \(e) NA))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "n_distinct")
  } else {
    tibble::tibble(columns = character(), n_distinct = numeric())
  }
  if (length(surv_cols) > 0) {
    surv_n_distinct <- unname(vapply(surv_cols, function(col) {
      tryCatch(nrow(unique(unclass(df[[col]]))), error = \(e) NA_integer_)
    }, numeric(1)))
    n_distinct <- dplyr::bind_rows(n_distinct, tibble::tibble(columns = surv_cols, n_distinct = surv_n_distinct))
  }

  if (length(numeric_cols) > 0) {
    long <- df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), as.numeric)) |>
      tidyr::pivot_longer(dplyr::all_of(numeric_cols), names_to = "columns", values_to = "value_") |>
      dplyr::group_by(.data$columns)
    mean_tbl <- dplyr::summarise(long, mean = mean(.data$value_, na.rm = TRUE), .groups = "drop")
    sd_tbl <- dplyr::summarise(long, sd = stats::sd(.data$value_, na.rm = TRUE), .groups = "drop")
  } else {
    mean_tbl <- tibble::tibble(columns = character(), mean = numeric())
    sd_tbl <- tibble::tibble(columns = character(), sd = numeric())
  }

  # Surv columns: report mean/sd of follow-up time and an event-rate summary
  # in most_freq, rather than treating time+status as one flattened vector.
  surv_summary <- function(col) {
    m <- unclass(df[[col]])
    cn <- colnames(m)
    time_idx <- if (!is.null(cn) && "time" %in% cn) which(cn == "time") else 1L
    status_idx <- if (!is.null(cn) && "status" %in% cn) which(cn == "status") else ncol(m)
    time <- m[, time_idx]
    status <- m[, status_idx]
    n_events <- sum(status == 1, na.rm = TRUE)
    pct_events <- if (length(status) > 0) round(100 * n_events / length(status), 1) else NA_real_
    tibble::tibble(
      columns = col,
      mean = mean(time, na.rm = TRUE),
      sd = stats::sd(time, na.rm = TRUE),
      most_freq = paste0("Events: ", n_events, " (", pct_events, "%)")
    )
  }
  surv_tbl <- if (length(surv_cols) > 0) {
    dplyr::bind_rows(lapply(surv_cols, surv_summary))
  } else {
    tibble::tibble(columns = character(), mean = numeric(), sd = numeric(), most_freq = character())
  }
  mean_tbl <- dplyr::bind_rows(mean_tbl, dplyr::select(surv_tbl, "columns", "mean"))
  sd_tbl <- dplyr::bind_rows(sd_tbl, dplyr::select(surv_tbl, "columns", "sd"))

  most_freq_val <- function(x) {
    tab <- table(x)
    if (length(tab) == 0) return(NA_character_)
    names(which.max(tab))
  }

  mf_cols <- setdiff(names(dplyr::select(df, dplyr::where(is.factor) | dplyr::where(is.character) |
                                    dplyr::where(is.logical) | dplyr::where(is.numeric))), surv_cols)
  most_freq <- if (length(mf_cols) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::all_of(mf_cols), most_freq_val)) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "most_freq")
  } else {
    tibble::tibble(columns = character(), most_freq = character())
  }
  most_freq <- dplyr::bind_rows(most_freq, dplyr::select(surv_tbl, "columns", "most_freq"))

  tibble::tibble(columns = columns, class = class) |>
    dplyr::mutate(dataset_name = dataset_name) |>
    dplyr::left_join(labels, by = "columns") |>
    dplyr::relocate("labels") |>
    dplyr::relocate("dataset_name") |>
    dplyr::left_join(mean_tbl, by = "columns") |>
    dplyr::left_join(sd_tbl, by = "columns") |>
    dplyr::left_join(most_freq, by = "columns") |>
    dplyr::left_join(n_distinct, by = "columns") |>
    dplyr::left_join(na_info, by = "columns")
}

#' @rdname get_dataset_info
#' @export
proc_contents <- get_dataset_info
