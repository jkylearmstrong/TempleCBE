#' Tidy Construction of Counting-Process (Start-Stop) Survival Data
#'
#' Merges repeated longitudinal measurement data with event/censoring data (and optional
#' baseline covariates) to build tidy start-stop survival intervals (\code{tstart}, \code{tstop},
#' \code{event}, \code{event_label}). This provides a tidy, pipe-friendly alternative to SAS
#' DATA-step macros and \code{survival::tmerge} for creating counting-process datasets
#' for time-dependent Cox proportional hazards models.
#'
#' Each measurement starts an interval that ends at the subject's next measurement; the
#' last interval ends at \code{event_time}, where \code{event} is 1 (unless the subject's
#' \code{event_type} is in \code{censor_types}). A subject therefore needs an
#' \code{event_time}, and at least one measurement before it (before the moved
#' \code{event_time}, with \code{post_event = "include"}), to appear in the result.
#'
#' @param measure_df A data frame containing repeated longitudinal measurements.
#' @param event_df A data frame containing subject event or censoring times and event types.
#'   It must have one row per subject: repeated ids are an error, because the join would
#'   multiply the measurement rows. Subjects that are in \code{event_df} but have no
#'   measurement are not in the result.
#' @param id Column name identifying subjects across data frames (default: \code{"subject_id"}).
#' @param measure_time Column name in \code{measure_df} indicating measurement observation times (default: \code{"time"}).
#' @param event_time Column name in \code{event_df} indicating event or censoring time (default: \code{"event_time"}).
#' @param event_type Column name in \code{event_df} indicating event label or description (default: \code{"event_type"}).
#' @param baseline_df Optional data frame containing time-fixed baseline covariates keyed by \code{id}.
#'   Like \code{event_df}, it must have one row per subject.
#' @param post_event Character string: what to do with measurements made at or after
#'   a subject's \code{event_time}.
#'   \itemize{
#'     \item \code{"exclude"} (default) drops them, so each subject's last interval ends at
#'       the recorded \code{event_time}.
#'     \item \code{"include"} keeps the follow-up that the later measurements show: when a
#'       subject's last measurement is later than \code{event_time}, \code{event_time} is
#'       moved forward to that last measurement time, and the event (or censoring) happens
#'       there. The recorded \code{event_time} is not kept, so \strong{the survival time
#'       of such a subject changes}: with measurements at 0, 3 and 10 and an
#'       \code{event_time} of 8, the intervals are (0, 3] and (3, 10], with \code{event = 1}
#'       on (3, 10] only. The measurement at 10 gives that end point and no interval of
#'       its own, so the result never has a zero-length interval (10, 10] or a second
#'       \code{event = 1} for the subject. See the examples.
#'   }
#' @param censor_types Optional character vector of \code{event_type} values that mark a
#'   censoring time rather than an event, such as \code{"Censored"}. Those subjects' last
#'   interval gets \code{event = 0} (and no \code{event_label}). By default (\code{NULL}) every
#'   subject with an \code{event_time} has \code{event = 1} at it, whatever its
#'   \code{event_type}, so censoring times in \code{event_df} must be declared here (or the
#'   resulting \code{event} recoded) before the data is used to fit a survival model.
#' @return A tibble with columns \code{tstart}, \code{tstop}, \code{event} (1 if event occurred at \code{tstop},
#'   0 otherwise), \code{event_label}, and all merged measurement and baseline covariates.
#'
#'   Subjects can be missing from the result, or lose their last measurement, and a
#'   warning names them: (1) subjects in \code{measure_df} without an \code{event_time} in
#'   \code{event_df} (no row, or \code{NA}) have no end for their last interval, so that
#'   measurement gets no interval, and a subject with a single measurement drops out;
#'   (2) subjects with no row in the result at all: with \code{post_event = "exclude"}
#'   because their \code{event_time} is at or before their first measurement, with
#'   \code{"include"} because all their measurements are at one time, at or after
#'   their \code{event_time}. A warning also names the subjects with an interval
#'   whose \code{tstop} is not after its \code{tstart}, which comes from repeated
#'   measurement times and which survival models can't use.
#' @seealso [cbe_cox_multi()]
#' @examples
#' measures <- data.frame(
#'   subject_id = c(1, 1, 1),
#'   time = c(0, 3, 10),
#'   biomarker = c(1.2, 1.5, 1.8)
#' )
#' events <- data.frame(subject_id = 1, event_time = 8, event_type = "Relapse")
#'
#' # "exclude": the measurement at 10 is after the event and dropped; the last
#' # interval ends at the event time, (3, 8].
#' tidy_tmerge_cox(measures, events)
#'
#' # "include": the event time moves to the last measurement time, so the
#' # survival time is 10, not 8, and the last interval is (3, 10].
#' tidy_tmerge_cox(measures, events, post_event = "include")
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

  # A repeated id would multiply the measurement rows in the joins below.
  check_one_row_per_subject(event_df, id, "event_df")
  if (!is.null(baseline_df)) {
    check_one_row_per_subject(baseline_df, id, "baseline_df")
  }

  # Check for column collisions between frames
  event_extra <- setdiff(names(event_df), id)
  collision_event <- intersect(names(measure_df), event_extra)
  if (length(collision_event) > 0) {
    stop(
      "Column(s) present in both `measure_df` and `event_df`: ",
      paste(collision_event, collapse = ", "),
      ". Rename or drop them before calling tidy_tmerge_cox().",
      call. = FALSE
    )
  }
  if (!is.null(baseline_df)) {
    baseline_extra <- setdiff(names(baseline_df), id)
    collision_base <- intersect(names(measure_df), baseline_extra)
    if (length(collision_base) > 0) {
      stop(
        "Column(s) present in both `measure_df` and `baseline_df`: ",
        paste(collision_base, collapse = ", "),
        ". Rename or drop them before calling tidy_tmerge_cox().",
        call. = FALSE
      )
    }
  }

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

  # Keep only the measurements before the event time (must be done before lead
  # intervals). With "exclude" these are the pre-event measurements. With
  # "include" event_time is now at or after every subject's last measurement,
  # so only a measurement at exactly that time goes: it would start the
  # zero-length interval (t, t], carrying a second event = 1.
  df <- df |>
    dplyr::filter(
      is.na(!!event_time_sym) | !!measure_time_sym < !!event_time_sym
    )

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

  # Subjects that are missing from the result or lose their last measurement
  # would otherwise vanish without a trace.
  measure_ids <- unique(measure_df[[id]])
  with_event <- event_df[[id]][!is.na(event_df[[event_time]])]
  no_event <- measure_ids[!(measure_ids %in% with_event)]
  if (length(no_event) > 0) {
    warning(
      length(no_event), " subject(s) in `measure_df` have no `", event_time, "` in `event_df` ",
      "(no row, or NA), so their last measurement ends no interval and a subject with a single ",
      "measurement drops out; see subject(s): ", format_ids(no_event), ".",
      call. = FALSE
    )
  }
  no_rows <- measure_ids[!(measure_ids %in% df[[id]])]
  if (length(no_rows) > 0) {
    warning(
      length(no_rows), " subject(s) in `measure_df` have no interval in the result (no `", event_time,
      "`, or no measurement before it); see subject(s): ", format_ids(no_rows), ".",
      call. = FALSE
    )
  }

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

# Stops when `ids` has repeated subjects, naming some of them.
check_one_row_per_subject <- function(data, id, arg) {
  ids <- data[[id]]
  if (anyDuplicated(ids) > 0) {
    stop(
      "`", arg, "` must have one row per subject, but `", id, "` is repeated for subject(s): ",
      format_ids(ids[duplicated(ids)]), ".",
      call. = FALSE
    )
  }
  invisible(data)
}

utils::globalVariables(c("event", "max_time", "tstart", "tstop"))
