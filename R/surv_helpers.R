# Survival truth, censoring weights, and survival predictions for right-censored
# and counting-process (start, stop] outcomes, shaped for yardstick.
#
# yardstick's survival metrics expect one row per subject with a right-censored
# `Surv(time, event)` truth. Handed a counting-process `Surv(start, stop,
# event)` they return numbers without complaint, but those numbers treat every
# interval as its own subject. The helpers here collapse start/stop data to one
# row per subject and weight it for censoring, so any model's predictions can
# be scored correctly.

#' Split a `Surv` Object Into Start, Stop, and Status
#'
#' @param truth A right-censored or counting-process [survival::Surv()] object.
#' @return A list with numeric `start` (0 for right-censored data), `stop`,
#'   `status` (0/1), and `type`.
#' @keywords internal
#' @noRd
surv_components <- function(truth, arg = "truth") {
  if (!inherits(truth, "Surv")) {
    stop("`", arg, "` must be a survival::Surv() object.", call. = FALSE)
  }
  type <- attr(truth, "type")
  m <- unclass(truth)
  if (identical(type, "right")) {
    out <- list(start = rep(0, nrow(m)), stop = unname(m[, "time"]), status = unname(m[, "status"]))
  } else if (identical(type, "counting")) {
    out <- list(start = unname(m[, "start"]), stop = unname(m[, "stop"]), status = unname(m[, "status"]))
  } else {
    stop(
      "`", arg, "` must be right-censored, Surv(time, event), or counting-process, ",
      "Surv(start, stop, event); got type \"", type, "\".",
      call. = FALSE
    )
  }
  out$type <- type
  out
}

#' Collapse Survival Outcomes to One Row per Subject
#'
#' Turns right-censored or counting-process (start/stop) survival outcomes into
#' the one-row-per-subject, right-censored truth that yardstick's survival
#' metrics require.
#'
#' For counting-process data, a subject's time is the stop time of their last
#' interval and their event status is the status of that interval. Rows are
#' checked first: within a subject, intervals must not overlap, and an event
#' may only occur in the last interval (recurrent events are not supported).
#'
#' @param truth A [survival::Surv()] object: `Surv(time, event)` or
#'   `Surv(start, stop, event)`.
#' @param subject_id Subject identifiers, one per element of `truth`. Required
#'   for counting-process data; for right-censored data each row is its own
#'   subject when `subject_id` is `NULL`.
#' @return A tibble with one row per subject, sorted by `subject_id`:
#'   `.subject_id`, `.entry` (start of the first interval; 0 for right-censored
#'   data), and `.truth`, a right-censored `Surv` object.
#' @seealso [censoring_km()], [graf_weights()], [add_graf_weights()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   long <- data.frame(
#'     subject_id = c(1, 1, 2, 3, 3),
#'     tstart = c(0, 5, 0, 0, 4),
#'     tstop = c(5, 9, 6, 4, 10),
#'     status = c(0, 1, 0, 0, 0)
#'   )
#'   surv_subject_truth(
#'     survival::Surv(long$tstart, long$tstop, long$status),
#'     subject_id = long$subject_id
#'   )
#' }
surv_subject_truth <- function(truth, subject_id = NULL) {
  rlang::check_installed("survival", reason = "to build survival truth.")
  parts <- surv_components(truth)
  n <- length(parts$stop)
  if (is.null(subject_id)) {
    if (parts$type == "counting") {
      stop(
        "`subject_id` is required for counting-process Surv(start, stop, event) outcomes, ",
        "so each subject's intervals can be combined.",
        call. = FALSE
      )
    }
    subject_id <- seq_len(n)
  }
  if (length(subject_id) != n) {
    stop(
      "`subject_id` must have one value per element of `truth` (", n, "); it has ",
      length(subject_id), ".",
      call. = FALSE
    )
  }
  if (anyNA(subject_id) || anyNA(parts$start) || anyNA(parts$stop) || anyNA(parts$status)) {
    stop("`truth` and `subject_id` must not contain missing values.", call. = FALSE)
  }

  ord <- order(subject_id, parts$start, parts$stop)
  sid <- subject_id[ord]
  start <- parts$start[ord]
  stop <- parts$stop[ord]
  status <- parts$status[ord]
  first <- !duplicated(sid)
  last <- !duplicated(sid, fromLast = TRUE)

  overlap <- !first & start < c(NA, stop[-length(stop)])
  if (any(overlap, na.rm = TRUE)) {
    stop("Intervals overlap within subject(s): ", format_ids(sid[which(overlap)]), ".", call. = FALSE)
  }
  early <- status == 1 & !last
  if (any(early)) {
    stop(
      "Subject(s) with an event before their last interval: ", format_ids(sid[early]),
      ". Recurrent events are not supported.",
      call. = FALSE
    )
  }

  tibble::tibble(
    .subject_id = sid[last],
    .entry = start[first],
    .truth = survival::Surv(stop[last], status[last])
  )
}

#' Kaplan-Meier Estimate of the Censoring Distribution
#'
#' Fits the "reverse" Kaplan-Meier estimator \eqn{G(t) = P(C > t)}, treating
#' censoring as the event, for inverse-probability-of-censoring weights.
#' Estimate it on the data a model was trained on, and apply it to the data
#' being scored.
#'
#' @param truth A right-censored [survival::Surv()] object with one element
#'   per subject, such as the `.truth` column of [surv_subject_truth()].
#' @return An object of class `censoring_km`, with `time` and `surv`; use
#'   `predict(object, time)` for \eqn{G(t)}.
#' @seealso [graf_weights()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   cens <- censoring_km(survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1)))
#'   predict(cens, c(1, 2, 5, 7))
#' }
censoring_km <- function(truth) {
  rlang::check_installed("survival", reason = "to estimate the censoring distribution.")
  parts <- surv_components(truth)
  if (parts$type != "right") {
    stop(
      "`truth` must be right-censored with one element per subject. ",
      "Collapse start/stop data with surv_subject_truth() first.",
      call. = FALSE
    )
  }
  fit <- survival::survfit(survival::Surv(parts$stop, 1 - parts$status) ~ 1)
  structure(list(time = fit$time, surv = fit$surv, n = length(parts$stop)), class = "censoring_km")
}

#' @param object A `censoring_km` object.
#' @param time Numeric vector of times.
#' @param left If `TRUE`, the left limit \eqn{G(t^-)}, i.e. censoring strictly
#'   before `time`.
#' @param trunc Lower bound applied to \eqn{G}, so weights stay finite.
#' @param ... Not used.
#' @rdname censoring_km
#' @export
predict.censoring_km <- function(object, time, left = FALSE, trunc = 0, ...) {
  check_trunc(trunc)
  idx <- findInterval(time, object$time, left.open = left)
  pmax(c(1, object$surv)[idx + 1], trunc)
}

#' @export
print.censoring_km <- function(x, ...) {
  cat("<censoring_km> reverse Kaplan-Meier censoring distribution\n")
  cat("  subjects:", x$n, " censoring times:", length(x$time), "\n")
  invisible(x)
}

#' Inverse Probability of Censoring Weights (Graf et al.)
#'
#' Computes the weights time-dependent survival metrics need so that subjects
#' lost to follow-up don't bias the score: at evaluation time \eqn{t}, a
#' subject still under observation (\eqn{T > t}) gets \eqn{1/G(t)}; a subject
#' with an event by \eqn{t} gets \eqn{1/G(T^-)}; a subject censored by
#' \eqn{t} gets 0, as their status at \eqn{t} is unknown.
#'
#' The weights assume follow-up starts at time 0 for every subject: they do not
#' correct for delayed entry (left truncation).
#'
#' @param truth A right-censored [survival::Surv()] object, one element per
#'   subject (see [surv_subject_truth()]).
#' @param eval_time Numeric vector of evaluation times.
#' @param censoring A [censoring_km()] object, estimated on the training data.
#' @param trunc Lower bound for \eqn{G}, capping weights at `1 / trunc`
#'   (default 0.05, as in tidymodels' `parsnip`).
#' @return A numeric matrix with one row per element of `truth` and one column
#'   per `eval_time`.
#' @references Graf E, Schmoor C, Sauerbrei W, Schumacher M (1999). Assessment
#'   and comparison of prognostic classification schemes for survival data.
#'   *Statistics in Medicine*, 18(17-18), 2529-2545.
#' @seealso [add_graf_weights()], [censoring_km()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   train <- survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1))
#'   test <- survival::Surv(c(5, 7), c(1, 0))
#'   graf_weights(test, eval_time = c(3, 6), censoring = censoring_km(train))
#' }
graf_weights <- function(truth, eval_time, censoring, trunc = 0.05) {
  parts <- surv_components(truth)
  if (parts$type != "right") {
    stop("`truth` must be right-censored, one element per subject; see surv_subject_truth().", call. = FALSE)
  }
  check_eval_time(eval_time, min_length = 1)
  if (!inherits(censoring, "censoring_km")) {
    stop("`censoring` must be a censoring_km() object.", call. = FALSE)
  }
  check_trunc(trunc)

  n <- length(parts$stop)
  n_times <- length(eval_time)
  time <- matrix(parts$stop, n, n_times)
  status <- matrix(parts$status, n, n_times)
  t <- matrix(eval_time, n, n_times, byrow = TRUE)

  at_risk <- time > t
  event_by_t <- !at_risk & status == 1
  weights <- matrix(0, n, n_times)
  weights[at_risk] <- 1 / stats::predict(censoring, t[at_risk], trunc = trunc)
  weights[event_by_t] <- 1 / stats::predict(censoring, time[event_by_t], left = TRUE, trunc = trunc)
  dimnames(weights) <- list(NULL, format(eval_time))
  weights
}

#' Add Censoring Weights to Survival Predictions for yardstick
#'
#' Fills the `.weight_censored` column of each element of a `.pred`
#' list-column with [graf_weights()], so the data can go straight to
#' `yardstick::brier_survival()`, `brier_survival_integrated()`, or
#' `roc_auc_survival()`. Use it to score any model's per-subject survival
#' predictions, including models fit to start/stop data.
#'
#' @param data A data frame with one row per subject.
#' @param truth Column of `data` holding right-censored `Surv` truth.
#' @param estimate List-column of data frames with `.eval_time` and
#'   `.pred_survival`, one data frame per row, all with the same evaluation
#'   times.
#' @inheritParams graf_weights
#' @return `data`, with `.weight_censored` added to (or replaced in) every
#'   data frame in `estimate`.
#' @seealso [surv_subject_truth()], [censoring_km()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("yardstick", quietly = TRUE)) {
#'   train <- survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1))
#'   scored <- tibble::tibble(
#'     .truth = survival::Surv(c(5, 7), c(1, 0)),
#'     .pred = list(
#'       tibble::tibble(.eval_time = c(3, 6), .pred_survival = c(0.9, 0.5)),
#'       tibble::tibble(.eval_time = c(3, 6), .pred_survival = c(0.8, 0.6))
#'     )
#'   )
#'   scored <- add_graf_weights(scored, censoring = censoring_km(train))
#'   yardstick::brier_survival(scored, truth = .truth, .pred)
#' }
add_graf_weights <- function(data, truth = ".truth", estimate = ".pred", censoring, trunc = 0.05) {
  truth <- rlang::as_name(rlang::enquo(truth))
  estimate <- rlang::as_name(rlang::enquo(estimate))
  if (!is.data.frame(data) || !all(c(truth, estimate) %in% names(data))) {
    stop("`data` must be a data frame with columns `", truth, "` and `", estimate, "`.", call. = FALSE)
  }
  preds <- data[[estimate]]
  if (!is.list(preds) || !length(preds) || !all(vapply(preds, is.data.frame, logical(1)))) {
    stop("`", estimate, "` must be a list-column of data frames.", call. = FALSE)
  }
  eval_time <- preds[[1]]$.eval_time
  same_times <- vapply(preds, function(p) identical(p$.eval_time, eval_time), logical(1))
  if (is.null(eval_time) || !all(same_times)) {
    stop("Every data frame in `", estimate, "` needs the same `.eval_time` values.", call. = FALSE)
  }
  weights <- graf_weights(data[[truth]], eval_time, censoring, trunc)
  data[[estimate]] <- lapply(seq_along(preds), function(i) {
    p <- preds[[i]]
    p$.weight_censored <- unname(weights[i, ])
    p
  })
  data
}

#' Default Evaluation Times: Deciles of the Observed Event Times
#'
#' @param truth Right-censored subject-level truth.
#' @return Sorted, unique event-time deciles (10th to 90th percentile).
#' @keywords internal
#' @noRd
default_eval_time <- function(truth) {
  parts <- surv_components(truth)
  event_times <- parts$stop[parts$status == 1]
  times <- unique(stats::quantile(event_times, probs = seq(0.1, 0.9, by = 0.1), type = 1, names = FALSE))
  if (length(times) < 2) {
    stop(
      "Too few distinct event times to choose default evaluation times; pass `eval_time`.",
      call. = FALSE
    )
  }
  sort(times)
}

#' Breslow Cumulative Baseline Hazard, for a Path of Linear Predictors
#'
#' Computes \eqn{H_0(t) = \sum_{t_j \le t} d_j / \sum_{i \in R(t_j)} e^{\eta_i}}
#' for every column of `lp` at once. The risk set at \eqn{t} holds the rows with
#' `start < t <= stop`, summed without an n-by-times indicator matrix: rows
#' with `stop >= t` minus rows with `start >= t`.
#'
#' @param lp Numeric matrix of training linear predictors, one row per
#'   training row and one column per penalty.
#' @param start,stop,status Training outcome components.
#' @return A list with `time` (distinct event times) and `cumhaz` (a matrix,
#'   one row per event time and one column per column of `lp`).
#' @keywords internal
#' @noRd
breslow_cumhaz <- function(lp, start, stop, status) {
  lp <- as.matrix(lp)
  event_times <- sort(unique(stop[status == 1]))
  if (!length(event_times)) {
    return(list(time = numeric(0), cumhaz = matrix(0, 0, ncol(lp))))
  }
  risk <- exp(lp)
  total <- colSums(risk)

  sum_at_or_after <- function(times) {
    ord <- order(times)
    cum <- rbind(0, col_cumsum(risk[ord, , drop = FALSE]))
    before <- findInterval(event_times, times[ord], left.open = TRUE)
    sweep(-cum[before + 1, , drop = FALSE], 2, total, "+")
  }
  at_risk <- sum_at_or_after(stop) - sum_at_or_after(start)
  deaths <- tabulate(match(stop[status == 1], event_times), nbins = length(event_times))

  list(time = event_times, cumhaz = col_cumsum(deaths / at_risk))
}

#' Predicted Survival per Subject From a Covariate Path
#'
#' For a subject with intervals \eqn{(s_k, e_k]} and linear predictors
#' \eqn{\eta_k}, \eqn{S(t) = \exp(-\sum_k [H_0(\min(t, e_k)) - H_0(\min(t, s_k))]
#' e^{\eta_k})}. Each interval's covariates are carried forward to the start of
#' the next (bridging gaps) and past the last interval; the first interval's
#' covariates also cover time 0 up to the subject's entry. With
#' `covariates = "baseline"`, only the first interval's covariates are used
#' for all times. Right-censored data is the one-interval case.
#'
#' @param lp Linear predictor matrix (rows by penalties).
#' @param start,stop,id Row components of the scored data.
#' @param eval_time Evaluation times.
#' @param bh Output of [breslow_cumhaz()].
#' @param covariates `"path"` or `"baseline"`.
#' @return A list with `id` (sorted subjects) and `surv`, an array of subjects
#'   by evaluation times by penalties.
#' @keywords internal
#' @noRd
predict_subject_survival <- function(lp, start, stop, id, eval_time, bh, covariates = "path") {
  lp <- as.matrix(lp)
  ord <- order(id, start, stop)
  lp <- lp[ord, , drop = FALSE]
  start <- start[ord]
  id <- id[ord]
  first <- !duplicated(id)
  last <- !duplicated(id, fromLast = TRUE)

  if (covariates == "baseline") {
    lp <- lp[first, , drop = FALSE]
    id <- id[first]
    start_eff <- rep(0, length(id))
    stop_eff <- rep(Inf, length(id))
  } else {
    start_eff <- ifelse(first, 0, start)
    stop_eff <- ifelse(last, Inf, c(start[-1], Inf))
  }

  n_rows <- length(id)
  subjects <- id[!duplicated(id)]
  hazard <- rbind(0, bh$cumhaz)
  upper <- findInterval(outer(stop_eff, eval_time, pmin), bh$time) + 1
  lower <- findInterval(outer(start_eff, eval_time, pmin), bh$time) + 1
  group <- factor(id, levels = unique(id))

  surv <- array(NA_real_, c(length(subjects), length(eval_time), ncol(lp)))
  for (l in seq_len(ncol(lp))) {
    h <- hazard[, l]
    increments <- matrix((h[upper] - h[lower]) * exp(lp[, l]), nrow = n_rows)
    surv[, , l] <- exp(-rowsum(increments, group, reorder = FALSE))
  }
  list(id = subjects, surv = surv)
}

#' Subject Keys That Tell Bootstrap Copies of a Subject Apart
#'
#' Bootstrap resamples, such as [rsample::group_bootstraps()], repeat whole
#' subjects, so one identifier appears with the same intervals more than once.
#' Each repeat counts as a separate subject when scoring. Rows are numbered by
#' how many times their (subject, start, stop) combination has appeared so
#' far, and copies after the first get a distinct key. Without repeats, the
#' identifiers come back unchanged.
#'
#' @param y The `Surv` outcome of the rows.
#' @param subject Subject identifiers of the rows.
#' @return A vector of subject keys, one per row.
#' @keywords internal
#' @noRd
subject_keys <- function(y, subject) {
  parts <- surv_components(y)
  copy <- stats::ave(seq_along(subject), subject, parts$start, parts$stop, FUN = seq_along)
  if (all(copy == 1)) {
    return(subject)
  }
  paste0(subject, "#", copy)
}

#' Column-wise Cumulative Sum of a Matrix
#' @keywords internal
#' @noRd
col_cumsum <- function(m) {
  m <- as.matrix(m)
  for (j in seq_len(ncol(m))) m[, j] <- cumsum(m[, j])
  m
}

#' @keywords internal
#' @noRd
check_eval_time <- function(eval_time, min_length = 2) {
  if (!is.numeric(eval_time) || anyNA(eval_time) || any(!is.finite(eval_time)) ||
      any(eval_time < 0) || length(unique(eval_time)) < min_length) {
    stop(
      "`eval_time` must be ", if (min_length > 1) paste0("at least ", min_length, " distinct ") else "",
      "finite, non-negative numbers.",
      call. = FALSE
    )
  }
  if (is.unsorted(eval_time, strictly = TRUE)) {
    stop("`eval_time` must be sorted, with no duplicates.", call. = FALSE)
  }
  invisible(eval_time)
}

#' @keywords internal
#' @noRd
check_trunc <- function(trunc) {
  if (!is.numeric(trunc) || length(trunc) != 1 || is.na(trunc) || trunc < 0 || trunc >= 1) {
    stop("`trunc` must be a single number in [0, 1).", call. = FALSE)
  }
  invisible(trunc)
}

#' @keywords internal
#' @noRd
format_ids <- function(ids, max = 10) {
  ids <- unique(ids)
  paste0(paste(utils::head(ids, max), collapse = ", "), if (length(ids) > max) ", ..." else "")
}

#' Pre-Flight Validation for Counting-Process (Start/Stop) Survival Data
#'
#' Inspects counting-process survival data before model tuning and reports
#' subject, interval, event, and censored counts alongside structural checks for
#' overlapping or gapped intervals, non-positive interval lengths (`stop <= start`),
#' events occurring before a subject's last interval, rows appearing after an
#' event, and tied stop/event times. Emits a prominent warning when no subjects
#' are censored (e.g. 100% event rate).
#'
#' @param data A data frame containing counting-process survival intervals.
#' @param id,start,stop,status Column names (as strings or bare symbols) for the
#'   subject identifier, interval start time, interval stop time, and event
#'   status indicator (`0` = censored, `1` = event). Defaults to `"id"`,
#'   `"tstart"`, `"tstop"`, and `"status"`.
#' @param warn_no_censoring Logical (default `TRUE`). Emit a warning when every
#'   subject experiences an event (`n_censored == 0`).
#' @param error_on_invalid Logical (default `FALSE`). When `TRUE`, stops with an
#'   error if any structural issue (missing values, `stop <= start`, overlapping
#'   intervals, events before the last interval, or rows after an event) is
#'   detected.
#' @return A 1-row tibble of class `cbe_start_stop_validation` with columns:
#'   `n_subjects`, `n_intervals`, `n_events`, `n_subjects_with_event`,
#'   `n_censored`, `censoring_rate`, `n_overlapping`, `n_gaps`,
#'   `n_events_before_last`, `n_rows_after_event`, `n_nonpositive_length`,
#'   `n_tied_stop_times`, `n_tied_event_times`, `n_missing`, `has_censoring`,
#'   and `valid`. Detailed vectors of affected subject IDs are stored in
#'   `attr(x, "details")`.
#' @seealso [surv_subject_truth()], [glmnet_IBS()]
#' @export
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 3),
#'   tstart = c(0, 5, 0, 0),
#'   tstop = c(5, 10, 8, 12),
#'   status = c(0, 1, 0, 1)
#' )
#' validate_start_stop(df, id = "id", start = "tstart", stop = "tstop", status = "status")
validate_start_stop <- function(data,
                                id = "id",
                                start = "tstart",
                                stop = "tstop",
                                status = "status",
                                warn_no_censoring = TRUE,
                                error_on_invalid = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  resolve_col <- function(quo_arg, default_nm) {
    if (rlang::quo_is_missing(quo_arg)) {
      return(default_nm)
    }
    expr <- rlang::quo_get_expr(quo_arg)
    if (is.symbol(expr)) {
      nm <- as.character(expr)
      if (nm %in% names(data)) {
        return(nm)
      }
    }
    val <- rlang::eval_tidy(quo_arg)
    if (is.character(val) && length(val) == 1L) {
      return(val)
    }
    stop("Column specification must be a single column name or symbol.", call. = FALSE)
  }

  id_col <- resolve_col(rlang::enquo(id), "id")
  start_col <- resolve_col(rlang::enquo(start), "tstart")
  stop_col <- resolve_col(rlang::enquo(stop), "tstop")
  status_col <- resolve_col(rlang::enquo(status), "status")

  req_cols <- c(id_col, start_col, stop_col, status_col)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  sid_raw <- data[[id_col]]
  t0_raw <- as.numeric(data[[start_col]])
  t1_raw <- as.numeric(data[[stop_col]])
  st_raw <- data[[status_col]]
  if (is.logical(st_raw)) {
    st_raw <- as.integer(st_raw)
  } else if (is.factor(st_raw)) {
    st_raw <- as.numeric(as.character(st_raw))
  } else {
    st_raw <- as.numeric(st_raw)
  }

  na_row <- is.na(sid_raw) | is.na(t0_raw) | is.na(t1_raw) | is.na(st_raw)
  n_missing <- sum(na_row)

  keep <- !na_row
  sid <- sid_raw[keep]
  t0 <- t0_raw[keep]
  t1 <- t1_raw[keep]
  st <- st_raw[keep]

  n_intervals <- nrow(data)
  if (length(sid) == 0L) {
    out <- tibble::tibble(
      n_subjects = 0L,
      n_intervals = as.integer(n_intervals),
      n_events = 0L,
      n_subjects_with_event = 0L,
      n_censored = 0L,
      censoring_rate = NA_real_,
      n_overlapping = 0L,
      n_gaps = 0L,
      n_events_before_last = 0L,
      n_rows_after_event = 0L,
      n_nonpositive_length = 0L,
      n_tied_stop_times = 0L,
      n_tied_event_times = 0L,
      n_missing = as.integer(n_missing),
      has_censoring = FALSE,
      valid = n_missing == 0L
    )
    attr(out, "details") <- list(
      overlapping_ids = character(),
      gapped_ids = character(),
      events_before_last_ids = character(),
      rows_after_event_ids = character(),
      nonpositive_length_ids = character(),
      issues = if (n_missing > 0L) sprintf("%d row(s) with missing values", n_missing) else character()
    )
    class(out) <- c("cbe_start_stop_validation", class(out))
    return(out)
  }

  # Sort within subject by start, stop, while preserving original order for "rows after event" check
  ord <- order(sid, t0, t1)
  sid_s <- sid[ord]
  t0_s <- t0[ord]
  t1_s <- t1[ord]
  st_s <- st[ord]

  first <- !duplicated(sid_s)
  last <- !duplicated(sid_s, fromLast = TRUE)
  prev_stop <- c(NA_real_, t1_s[-length(t1_s)])
  prev_stop[first] <- NA_real_

  nonpos <- t1_s <= t0_s
  overlap <- !first & (t0_s < prev_stop)
  gap <- !first & (t0_s > prev_stop)
  early_event <- (st_s == 1) & !last

  # Rows appearing after an event within the same subject
  prior_events <- stats::ave(st_s == 1, sid_s, FUN = function(v) {
    if (length(v) <= 1L) return(rep(FALSE, length(v)))
    c(FALSE, cumsum(v[-length(v)]) > 0)
  })
  after_event <- as.logical(prior_events)

  u_subjects <- unique(sid_s)
  n_subjects <- length(u_subjects)
  subj_has_event <- tapply(st_s == 1, sid_s, any)
  n_subj_event <- sum(subj_has_event, na.rm = TRUE)
  n_censored <- as.integer(n_subjects - n_subj_event)
  n_events <- sum(st_s == 1, na.rm = TRUE)

  # Tied times
  n_tied_stop <- as.integer(sum(duplicated(t1_s)))
  ev_times <- t1_s[st_s == 1]
  n_tied_event <- as.integer(sum(duplicated(ev_times)))

  nonpos_ids <- unique(sid_s[nonpos])
  overlap_ids <- unique(sid_s[overlap])
  gap_ids <- unique(sid_s[gap])
  early_ids <- unique(sid_s[early_event])
  after_ids <- unique(sid_s[after_event])

  issues <- character()
  if (n_missing > 0L) {
    issues <- c(issues, sprintf("%d row(s) contain missing values in id/start/stop/status", n_missing))
  }
  if (sum(nonpos) > 0L) {
    issues <- c(issues, sprintf("%d interval(s) have stop <= start (subjects: %s)", sum(nonpos), format_ids(nonpos_ids)))
  }
  if (sum(overlap) > 0L) {
    issues <- c(issues, sprintf("%d overlapping interval(s) detected (subjects: %s)", sum(overlap), format_ids(overlap_ids)))
  }
  if (sum(early_event) > 0L) {
    issues <- c(issues, sprintf("%d event(s) occurred before the last interval (subjects: %s)", sum(early_event), format_ids(early_ids)))
  }
  if (sum(after_event) > 0L) {
    issues <- c(issues, sprintf("%d row(s) appear after an event (subjects: %s)", sum(after_event), format_ids(after_ids)))
  }

  is_valid <- length(issues) == 0L
  has_cens <- n_censored > 0L

  if (!has_cens && n_subjects > 0L && isTRUE(warn_no_censoring)) {
    warning(sprintf(
      "No censoring detected in start/stop data: all %d of %d subject(s) experienced an event (0 censored subjects). Verify `%s` before model tuning.",
      n_subj_event, n_subjects, status_col
    ), call. = FALSE)
  }

  if (!is_valid && isTRUE(error_on_invalid)) {
    stop("Invalid start/stop survival data:\n  - ", paste(issues, collapse = "\n  - "), call. = FALSE)
  }

  out <- tibble::tibble(
    n_subjects = as.integer(n_subjects),
    n_intervals = as.integer(n_intervals),
    n_events = as.integer(n_events),
    n_subjects_with_event = as.integer(n_subj_event),
    n_censored = as.integer(n_censored),
    censoring_rate = as.numeric(n_censored / n_subjects),
    n_overlapping = as.integer(sum(overlap)),
    n_gaps = as.integer(sum(gap)),
    n_events_before_last = as.integer(sum(early_event)),
    n_rows_after_event = as.integer(sum(after_event)),
    n_nonpositive_length = as.integer(sum(nonpos)),
    n_tied_stop_times = n_tied_stop,
    n_tied_event_times = n_tied_event,
    n_missing = as.integer(n_missing),
    has_censoring = has_cens,
    valid = is_valid
  )

  attr(out, "details") <- list(
    overlapping_ids = overlap_ids,
    gapped_ids = gap_ids,
    events_before_last_ids = early_ids,
    rows_after_event_ids = after_ids,
    nonpositive_length_ids = nonpos_ids,
    issues = issues
  )
  class(out) <- c("cbe_start_stop_validation", class(out))
  out
}

#' Clean Counting-Process Intervals
#'
#' Companion to [validate_start_stop()]: drops intervals with `stop <= start`, drops rows recorded
#' after a subject's first event, and closes tiny gaps or overlaps (smaller than `tol`) between a
#' subject's consecutive intervals by moving the later `start` to the earlier `stop`. Rows with a
#' missing id, time or status are dropped too. Gaps and overlaps larger than `tol` are real and
#' left alone; check them with [validate_start_stop()].
#'
#' @param data A data frame of counting-process intervals.
#' @param id,start,stop,status Column names (strings). Status is 0/1, logical or a 0/1 factor.
#' @param tol Largest gap or overlap treated as rounding error (default `1e-6`).
#' @return `data` with the offending rows removed, sorted by `id`, `start`, `stop`. The counts are in
#'   `attr(x, "pruned")`: `missing`, `nonpositive`, `after_event` and `gaps_closed`.
#' @seealso [validate_start_stop()]
#' @export
#' @examples
#' df <- data.frame(id = c(1, 1, 1, 2), tstart = c(0, 5.0000001, 10, 0),
#'                  tstop = c(5, 10, 12, 0), status = c(0, 1, 0, 0))
#' prune_start_stop(df)
prune_start_stop <- function(data, id = "id", start = "tstart", stop = "tstop", status = "status",
                             tol = 1e-6) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative number.", call. = FALSE)
  }
  cols <- c(id, start, stop, status)
  if (!is.character(cols) || length(cols) != 4L) stop("Column names must be single strings.", call. = FALSE)
  gone <- setdiff(cols, names(data))
  if (length(gone)) stop("`data` is missing required column(s): ", paste(gone, collapse = ", "), call. = FALSE)

  st <- data[[status]]
  st <- if (is.factor(st)) as.numeric(as.character(st)) else as.numeric(st)
  t0 <- as.numeric(data[[start]])
  t1 <- as.numeric(data[[stop]])
  sid <- data[[id]]

  ok <- !(is.na(sid) | is.na(t0) | is.na(t1) | is.na(st))
  n_missing <- sum(!ok)
  data <- data[ok, , drop = FALSE]
  st <- st[ok]; t0 <- t0[ok]; t1 <- t1[ok]; sid <- sid[ok]

  pos <- t1 > t0
  n_nonpos <- sum(!pos)
  data <- data[pos, , drop = FALSE]
  st <- st[pos]; t0 <- t0[pos]; t1 <- t1[pos]; sid <- sid[pos]

  ord <- order(sid, t0, t1)
  data <- data[ord, , drop = FALSE]
  st <- st[ord]; t0 <- t0[ord]; t1 <- t1[ord]; sid <- sid[ord]

  prior_event <- as.logical(stats::ave(st == 1, sid, FUN = function(v) {
    c(FALSE, cumsum(v)[-length(v)] > 0)
  }))
  n_after <- sum(prior_event)
  keep <- !prior_event
  data <- data[keep, , drop = FALSE]
  t0 <- t0[keep]; t1 <- t1[keep]; sid <- sid[keep]

  first <- !duplicated(sid)
  prev_stop <- c(NA_real_, t1[-length(t1)])
  near <- !first & !is.na(prev_stop) & t0 != prev_stop & abs(t0 - prev_stop) < tol & t1 > prev_stop
  data[[start]][near] <- prev_stop[near]
  rownames(data) <- NULL
  attr(data, "pruned") <- list(missing = n_missing, nonpositive = n_nonpos, after_event = n_after,
                               gaps_closed = sum(near))
  data
}

#' @export
print.cbe_start_stop_validation <- function(x, ...) {
  cat("TempleCBE Start/Stop Survival Data Check\n")
  cat("----------------------------------------\n")
  cat(sprintf("Subjects:               %d (%d with event, %d censored [%.1f%%])\n",
              x$n_subjects[1], x$n_subjects_with_event[1], x$n_censored[1], 100 * x$n_censored[1] / max(1L, x$n_subjects[1])))
  cat(sprintf("Intervals (rows):       %d (events: %d, missing: %d)\n",
              x$n_intervals[1], x$n_events[1], x$n_missing[1]))
  cat(sprintf("Overlapping intervals:  %d\n", x$n_overlapping[1]))
  cat(sprintf("Gapped intervals:       %d\n", x$n_gaps[1]))
  cat(sprintf("Events before last row: %d\n", x$n_events_before_last[1]))
  cat(sprintf("Rows after an event:    %d\n", x$n_rows_after_event[1]))
  cat(sprintf("Non-positive intervals: %d\n", x$n_nonpositive_length[1]))
  cat(sprintf("Tied times:             %d stop time(s), %d event time(s)\n",
              x$n_tied_stop_times[1], x$n_tied_event_times[1]))
  cat(sprintf("Status:                 %s%s\n",
              if (isTRUE(x$valid[1])) "VALID" else "INVALID",
              if (!isTRUE(x$has_censoring[1])) " (WARNING: NO CENSORING)" else ""))
  details <- attr(x, "details")
  if (!is.null(details$issues) && length(details$issues) > 0L) {
    cat("Issues:\n")
    for (iss in details$issues) {
      cat(sprintf("  * %s\n", iss))
    }
  }
  invisible(x)
}

