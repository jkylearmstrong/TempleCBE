#' Summarize a Data Frame's Columns
#'
#' Per-column metadata: class, variable label (if set via \pkg{labelled}),
#' mean/sd for numeric columns, most-frequent value, distinct-value count,
#' and missingness.
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
  numeric_cols <- names(dplyr::select(df, dplyr::where(is.numeric)))

  class <- vapply(columns, function(col) {
    trimws(sub("labelled", "", paste0(class(df[[col]]), collapse = "")))
  }, character(1))

  labels <- unlist(labelled::var_label(df))
  labels <- if (length(labels) == 0) {
    tibble::tibble(labels = columns, columns = columns)
  } else {
    tibble::as_tibble(labels, rownames = "columns") |> dplyr::rename(labels = "value")
  }

  na_info <- if (ncol(df) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::everything(), \(x) SumNa(x))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "SumNa") |>
      dplyr::mutate(PctNa = .data$SumNa / nrow(df))
  } else {
    tibble::tibble(columns = character(), SumNa = numeric(), PctNa = numeric())
  }

  n_distinct <- if (ncol(df) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::everything(), \(x) tryCatch(dplyr::n_distinct(x), error = \(e) NA))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "n_distinct")
  } else {
    tibble::tibble(columns = character(), n_distinct = numeric())
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

  mf_cols <- names(dplyr::select(df, dplyr::where(is.factor) | dplyr::where(is.character) |
                                    dplyr::where(is.logical) | dplyr::where(is.numeric)))
  most_freq <- if (length(mf_cols) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::all_of(mf_cols), \(x) names(which.max(table(x))))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "most_freq")
  } else {
    tibble::tibble(columns = character(), most_freq = character())
  }

  tibble::tibble(columns = columns, class = class) |>
    dplyr::mutate(dataset_name = dataset_name) |>
    dplyr::left_join(labels, by = "columns") |>
    dplyr::mutate(labels = dplyr::if_else(is.na(.data$labels), .data$columns, .data$labels)) |>
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
