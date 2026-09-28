#' Tidy Construction of Counting-Process (Start-Stop) Survival Data
#'
#' Merges repeated longitudinal measurement data with event/censoring data (and optional
#' baseline covariates) to build tidy start-stop survival intervals (\code{tstart}, \code{tstop},
#' \code{event}, \code{event_label}). This provides a tidy, pipe-friendly alternative to SAS
#' DATA-step macros and \code{survival::tmerge} for creating counting-process datasets
#' for time-dependent Cox proportional hazards models.
#'
#' @param measure_df A data frame containing repeated longitudinal measurements.
#' @param event_df A data frame containing subject event or censoring times and event types.
#' @param id Column name identifying subjects across data frames (default: \code{"subject_id"}).
#' @param measure_time Column name in \code{measure_df} indicating measurement observation times (default: \code{"time"}).
#' @param event_time Column name in \code{event_df} indicating event or censoring time (default: \code{"event_time"}).
#' @param event_type Column name in \code{event_df} indicating event label or description (default: \code{"event_type"}).
#' @param baseline_df Optional data frame containing time-fixed baseline covariates keyed by \code{id}.
#' @param post_event Character string: \code{"exclude"} (default) drops measurements occurring after
#'   the subject's event time; \code{"include"} extends \code{event_time} to the maximum observed measurement time.
#' @param censor_types Optional character vector of \code{event_type} values that mark a
#'   censoring time rather than an event, such as \code{"Censored"}. Those subjects' last
#'   interval gets \code{event = 0} (and no \code{event_label}). By default (\code{NULL}) every
#'   subject with an \code{event_time} has \code{event = 1} at it, whatever its
#'   \code{event_type}, so censoring times in \code{event_df} must be declared here (or the
#'   resulting \code{event} recoded) before the data is used to fit a survival model.
#' @return A tibble with columns \code{tstart}, \code{tstop}, \code{event} (1 if event occurred at \code{tstop},
#'   0 otherwise), \code{event_label}, and all merged measurement and baseline covariates.
#' @seealso [cbe_cox_multi()]
#' @export
tidy_tmerge_cox <- function(
  measure_df,
  event_df,
  id = "subject_id",
  measure_time = "time",
  event_time = "event_time",
  event_type = "event_type",
  baseline_df = NULL,
  post_event = c("exclude", "include"),
  censor_types = NULL
) {
  post_event <- match.arg(post_event)

  id_sym <- rlang::sym(id)
  measure_time_sym <- rlang::sym(measure_time)
  event_time_sym <- rlang::sym(event_time)
  event_type_sym <- rlang::sym(event_type)

  # Merge measurement + event data
  df <- dplyr::left_join(measure_df, event_df, by = id)

  # If baseline covariates provided, merge them
  if (!is.null(baseline_df)) {
    df <- dplyr::left_join(df, baseline_df, by = id)
  }

  # If post_event = "include", push event_time forward
  if (post_event == "include") {
    df <- df |>
      dplyr::group_by(!!id_sym) |>
      dplyr::mutate(
        max_time = max(!!measure_time_sym, na.rm = TRUE),
        !!event_time_sym := dplyr::if_else(
          !is.na(!!event_time_sym) & !!event_time_sym < .data$max_time,
          .data$max_time,
          !!event_time_sym
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-dplyr::all_of("max_time"))
  }

  # Remove post-event measurements if exclude mode (must be done before lead intervals)
  if (post_event == "exclude") {
    df <- df |>
      dplyr::filter(
        is.na(!!event_time_sym) | !!measure_time_sym < !!event_time_sym
      )
  }

  # Build start-stop intervals
  df <- df |>
    dplyr::arrange(!!id_sym, !!measure_time_sym) |>
    dplyr::group_by(!!id_sym) |>
    dplyr::mutate(
      tstart = !!measure_time_sym,
      tstop = dplyr::lead(
        !!measure_time_sym,
        default = dplyr::first(!!event_time_sym)
      ),
      event = dplyr::if_else(
        !is.na(!!event_time_sym) & .data$tstop == !!event_time_sym &
          !(as.character(!!event_type_sym) %in% censor_types),
        1,
        0
      ),
      event_label = dplyr::if_else(
        .data$event == 1,
        as.character(!!event_type_sym),
        NA_character_
      )
    ) |>
    dplyr::ungroup()

  # Remove intervals with missing tstop
  df <- df |> dplyr::filter(!is.na(.data$tstop))

  empty <- df$tstop <= df$tstart
  if (any(empty)) {
    warning(
      sum(empty), " interval(s) have `tstop <= tstart` (repeated measurement times, or an event ",
      "time at or before a measurement), which survival models can't use; see subject(s): ",
      format_ids(df[[id]][empty]), ".",
      call. = FALSE
    )
  }

  tibble::as_tibble(df)
}

utils::globalVariables(c("event", "max_time", "tstart", "tstop"))
