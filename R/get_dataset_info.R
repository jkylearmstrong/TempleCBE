#' Summarize a Data Frame or Joint Model's Columns and Components
#'
#' Per-column metadata: class, variable label (if set via \pkg{labelled}),
#' mean/sd for numeric columns, most-frequent value, distinct-value count,
#' and missingness. \code{survival::Surv} columns are summarized from their
#' underlying time/status or start/stop counting-process matrix rather than
#' unrolled as plain numerics. When provided a \code{\link[TempleCBE]{joint_model}}
#' object, summarizes the fitted training data and attaches model metadata.
#'
#' @param x A data frame, tibble, or a fitted \code{\link[TempleCBE]{joint_model}} object.
#' @param ... Additional arguments passed to methods.
#' @return A tibble with one row per column of \code{x}: \code{dataset_name},
#'   \code{labels}, \code{columns}, \code{class}, \code{mean}, \code{sd},
#'   \code{most_freq}, \code{n_distinct}, \code{SumNa}, \code{PctNa}, and
#'   optionally \code{variable_type} when \code{subject_id} is specified.
#' @export
#' @examples
#' get_dataset_info(mtcars)
get_dataset_info <- function(x, ...) {
  UseMethod("get_dataset_info")
}

#' @rdname get_dataset_info
#' @param subject_id Optional character string specifying the subject identifier column
#'   for repeated-measures longitudinal datasets to audit time-varying vs. baseline features.
#' @param dataset_name Optional character string overriding the displayed dataset name.
#' @export
get_dataset_info.data.frame <- function(x, subject_id = NULL, dataset_name = NULL, ...) {
  df <- x
  if (is.null(dataset_name)) {
    dataset_name <- deparse(substitute(x))
  }
  columns <- colnames(df)

  is_surv <- vapply(df, function(col) inherits(col, "Surv"), logical(1))
  surv_cols <- columns[is_surv]

  # is.numeric() is TRUE for Surv objects (they're numeric matrices under the
  # hood), so exclude them here rather than flattening time+status together.
  numeric_cols <- setdiff(names(dplyr::select(df, dplyr::where(is.numeric))), surv_cols)

  class_vec <- unname(vapply(columns, function(col) {
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
    dplyr::summarise(df, dplyr::across(dplyr::everything(), \(v) SumNa(v))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "columns", values_to = "SumNa") |>
      dplyr::mutate(PctNa = .data$SumNa / nrow(df))
  } else {
    tibble::tibble(columns = character(), SumNa = numeric(), PctNa = numeric())
  }

  # dplyr::n_distinct() recurses infinitely on Surv matrices (no vctrs method
  # for the class), so those columns are counted separately via base R below.
  n_distinct_cols <- setdiff(columns, surv_cols)
  n_distinct <- if (length(n_distinct_cols) > 0) {
    dplyr::summarise(df, dplyr::across(dplyr::all_of(n_distinct_cols), \(v) tryCatch(dplyr::n_distinct(v), error = \(e) NA))) |>
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

  # Surv columns: report mean/sd of duration/follow-up time and event summary
  # supporting both right-censored and counting process Surv(start, stop, status).
  surv_summary <- function(col) {
    s_obj <- df[[col]]
    s_type <- attr(s_obj, "type") %||% "right"
    m <- unclass(s_obj)
    cn <- colnames(m)

    if (identical(s_type, "counting")) {
      start_idx <- if (!is.null(cn) && "start" %in% cn) which(cn == "start") else 1L
      stop_idx <- if (!is.null(cn) && "stop" %in% cn) which(cn == "stop") else 2L
      status_idx <- if (!is.null(cn) && "status" %in% cn) which(cn == "status") else ncol(m)

      start_t <- m[, start_idx]
      stop_t <- m[, stop_idx]
      status <- m[, status_idx]
      duration <- stop_t - start_t
      n_events <- sum(status == 1, na.rm = TRUE)
      pct_events <- if (length(status) > 0) round(100 * n_events / length(status), 1) else NA_real_

      tibble::tibble(
        columns = col,
        mean = mean(duration, na.rm = TRUE),
        sd = stats::sd(duration, na.rm = TRUE),
        most_freq = paste0("Counting (Events: ", n_events, " [", pct_events, "%], Median Stop: ",
                           round(stats::median(stop_t, na.rm = TRUE), 1), ")")
      )
    } else {
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

  out <- tibble::tibble(columns = columns, class = class_vec) |>
    dplyr::mutate(dataset_name = dataset_name) |>
    dplyr::left_join(labels, by = "columns") |>
    dplyr::relocate("labels") |>
    dplyr::relocate("dataset_name") |>
    dplyr::left_join(mean_tbl, by = "columns") |>
    dplyr::left_join(sd_tbl, by = "columns") |>
    dplyr::left_join(most_freq, by = "columns") |>
    dplyr::left_join(n_distinct, by = "columns") |>
    dplyr::left_join(na_info, by = "columns")

  # Longitudinal repeated-measures covariate classification if subject_id is supplied
  if (!is.null(subject_id) && subject_id %in% columns) {
    var_types <- unname(vapply(columns, function(col) {
      if (col == subject_id) return("Subject ID")
      if (nrow(df) == 0) return("Unknown")
      vals_per_sub <- tapply(df[[col]], df[[subject_id]], function(v) length(unique(v[!is.na(v)])))
      varies <- any(vals_per_sub > 1L, na.rm = TRUE)
      if (isTRUE(varies)) "Longitudinal (Time-Varying)" else "Baseline (Time-Invariant)"
    }, character(1)))
    out <- dplyr::mutate(out, variable_type = var_types)
  }

  out
}

#' @rdname get_dataset_info
#' @export
get_dataset_info.joint_model <- function(x, ...) {
  comp <- x$components
  df <- comp$predictors
  resp_name <- deparse(comp$formula[[2L]])
  df[[resp_name]] <- comp$surv_obj

  res <- get_dataset_info.data.frame(df, dataset_name = paste0("joint_model(", x$engine, ")"), ...)
  attr(res, "joint_model_summary") <- list(
    engine = x$engine,
    calibration = x$calibration,
    n_obs = length(comp$time),
    n_events = sum(comp$status),
    event_pct = round(100 * mean(comp$status), 1),
    median_followup = stats::median(comp$time)
  )
  res
}

#' @rdname get_dataset_info
#' @export
proc_contents <- get_dataset_info
