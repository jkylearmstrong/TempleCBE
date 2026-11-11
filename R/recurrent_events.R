# Recurrent-event survival models for counting-process (start, stop] data:
# Andersen-Gill, Prentice-Williams-Peterson (total and gap time), Wei-Lin-Weissfeld
# and gamma-frailty fits, plus builders for repeated hospital readmissions.

#' Build Counting-Process Intervals From Recurrent Event Times
#'
#' Turns one row per event (for example one row per readmission) and one row per
#' subject (the end of follow-up) into the `(tstart, tstop, status)` intervals that
#' [recurrent_cox()] fits. Time zero is the start of risk, such as the index
#' discharge. A subject with `k` events gets `k` event intervals, plus a final
#' censored interval when follow-up continues after the last event.
#'
#' Events at or before time zero, events after the end of follow-up, and events
#' that fall within `min_gap` of the previous kept event (a transfer or a duplicated
#' record rather than a new readmission) are dropped, each with a warning that
#' counts them. Death is a terminal event that is not a readmission: end the
#' subject's follow-up at death. That treats death as censoring, which is the usual
#' assumption of the Andersen-Gill and PWP models and is informative when death
#' is common.
#'
#' @param events A data frame with one row per event, holding the subject
#'   identifier (`id`) and the event time (`time`), measured from time zero.
#' @param followup A data frame with one row per subject, holding `id` and the end
#'   of follow-up (`end`, same time scale as `time`). Subjects without any event
#'   row get one censored interval.
#' @param id,time,end Column names. `id` is looked up in both data frames; `time`
#'   in `events` and `end` in `followup`.
#' @param min_gap Events that are no more than `min_gap` after the previous kept
#'   event are dropped (default `0`, which drops only ties and events at time zero).
#' @return A tibble with the subject identifier (named as `id`), `tstart`, `tstop`,
#'   `status` (1 = event, 0 = censored at the end of follow-up) and
#'   `event_number` (1 for the first interval, 2 for the second, and so on), sorted
#'   by subject and time.
#' @seealso [recurrent_cox()], [truncate_start_stop()], [validate_start_stop()]
#' @export
#' @examples
#' events <- data.frame(id = c(1, 1, 3), time = c(12, 40, 25))
#' followup <- data.frame(id = 1:3, end = c(90, 60, 30))
#' recurrent_intervals(events, followup)
recurrent_intervals <- function(events, followup, id = "id", time = "time", end = "end", min_gap = 0) {
  if (!is.data.frame(events) || !is.data.frame(followup)) {
    stop("`events` and `followup` must be data frames.", call. = FALSE)
  }
  if (!is.numeric(min_gap) || length(min_gap) != 1L || is.na(min_gap) || min_gap < 0) {
    stop("`min_gap` must be a single non-negative number.", call. = FALSE)
  }
  gone_e <- setdiff(c(id, time), names(events))
  gone_f <- setdiff(c(id, end), names(followup))
  if (length(gone_e)) stop("`events` is missing column(s): ", paste(gone_e, collapse = ", "), call. = FALSE)
  if (length(gone_f)) stop("`followup` is missing column(s): ", paste(gone_f, collapse = ", "), call. = FALSE)
  if (anyDuplicated(followup[[id]])) {
    stop("`followup` must have one row per subject; repeated: ", format_ids(followup[[id]][duplicated(followup[[id]])]), ".", call. = FALSE)
  }
  if (anyNA(followup[[id]]) || anyNA(followup[[end]])) {
    stop("`followup` must not contain missing identifiers or end times.", call. = FALSE)
  }
  not_followed <- setdiff(unique(events[[id]]), followup[[id]])
  if (length(not_followed)) {
    stop("Subject(s) with events but no follow-up row: ", format_ids(not_followed), ".", call. = FALSE)
  }

  end_t <- as.numeric(followup[[end]])
  bad_end <- end_t <= 0
  if (any(bad_end)) {
    warning(sum(bad_end), " subject(s) with follow-up of zero or less were dropped.", call. = FALSE)
  }
  fu <- tibble::tibble(.id = followup[[id]][!bad_end], .end = end_t[!bad_end])

  ev <- tibble::tibble(.id = events[[id]], .time = as.numeric(events[[time]]))
  ev <- ev[!is.na(ev$.id) & !is.na(ev$.time), , drop = FALSE]
  ev <- ev[ev$.id %in% fu$.id, , drop = FALSE]
  n0 <- nrow(ev)
  ev$.end <- fu$.end[match(ev$.id, fu$.id)]
  late <- ev$.time > ev$.end | ev$.time <= 0
  if (any(late)) {
    warning(sum(late), " event(s) at or before time zero, or after the end of follow-up, were dropped.", call. = FALSE)
  }
  ev <- ev[!late, , drop = FALSE]
  ev <- ev[order(ev$.id, ev$.time), , drop = FALSE]

  keep_after_gap <- function(t) {
    kept <- logical(length(t))
    last <- 0
    for (i in seq_along(t)) {
      if (t[i] - last > min_gap) {
        kept[i] <- TRUE
        last <- t[i]
      }
    }
    kept
  }
  keep <- if (nrow(ev) == 0L) logical() else as.logical(unlist(
    lapply(split(ev$.time, factor(ev$.id, levels = unique(ev$.id))), keep_after_gap),
    use.names = FALSE
  ))
  if (any(!keep)) {
    warning(sum(!keep), " event(s) within `min_gap` of the previous event (or tied with it) were dropped.", call. = FALSE)
  }
  ev <- ev[keep, , drop = FALSE]

  last_event <- if (nrow(ev)) tapply(ev$.time, ev$.id, max) else numeric()
  last_t <- rep(0, nrow(fu))
  has <- as.character(fu$.id) %in% names(last_event)
  last_t[has] <- as.numeric(last_event[as.character(fu$.id[has])])
  trailing <- fu$.end > last_t

  rows <- dplyr::bind_rows(
    tibble::tibble(.id = ev$.id, tstop = ev$.time, status = 1L),
    tibble::tibble(.id = fu$.id[trailing], tstop = fu$.end[trailing], status = 0L)
  )
  rows <- rows[order(match(rows$.id, fu$.id), rows$tstop), , drop = FALSE]
  grp <- match(rows$.id, fu$.id)
  rows$tstart <- ifelse(duplicated(grp), c(0, rows$tstop[-nrow(rows)]), 0)
  rows$event_number <- as.integer(stats::ave(seq_len(nrow(rows)), grp, FUN = seq_along))
  out <- rows[, c(".id", "tstart", "tstop", "status", "event_number")]
  names(out)[1L] <- id
  tibble::as_tibble(out[order(match(out[[id]], fu$.id), out$tstart), , drop = FALSE])
}

#' Cut Counting-Process Data at a Time Window
#'
#' Restricts start/stop data to `(0, at]`: intervals that start at or after `at` are
#' dropped, and an interval that runs past `at` is cut there and becomes censored
#' (`status = 0`), because its event, if any, falls outside the window. Use it for
#' 30-, 60- and 90-day readmission windows; [recurrent_cox_windows()] does so for
#' several windows at once.
#'
#' @param data A data frame of counting-process intervals.
#' @param at The window length, in the units of the time columns.
#' @param start,stop,status Column names (strings).
#' @return `data` restricted to the window, with the same columns.
#' @seealso [recurrent_cox_windows()], [recurrent_intervals()]
#' @export
#' @examples
#' d <- data.frame(id = c(1, 1, 2), tstart = c(0, 20, 0), tstop = c(20, 70, 50), status = c(1, 1, 0))
#' truncate_start_stop(d, 30)
truncate_start_stop <- function(data, at, start = "tstart", stop = "tstop", status = "status") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.numeric(at) || length(at) != 1L || is.na(at) || at <= 0) {
    stop("`at` must be a single positive number.", call. = FALSE)
  }
  gone <- setdiff(c(start, stop, status), names(data))
  if (length(gone)) stop("`data` is missing column(s): ", paste(gone, collapse = ", "), call. = FALSE)
  out <- data[as.numeric(data[[start]]) < at, , drop = FALSE]
  cut <- as.numeric(out[[stop]]) > at
  out[[stop]][cut] <- at
  out[[status]][cut] <- if (is.factor(out[[status]])) levels(out[[status]])[1L] else if (is.logical(out[[status]])) FALSE else 0
  rownames(out) <- NULL
  out
}

# predictors as a character vector of right-hand-side terms
#' @noRd
recurrent_terms <- function(predictors) {
  if (inherits(predictors, "formula")) {
    if (length(predictors) != 2L) stop("`predictors` must be a one-sided formula such as `~ age + sex`.", call. = FALSE)
    return(attr(stats::terms(predictors), "term.labels"))
  }
  if (is.character(predictors)) {
    return(ifelse(make.names(predictors) == predictors, predictors, paste0("`", predictors, "`")))
  }
  stop("`predictors` must be column names or a one-sided formula.", call. = FALSE)
}

#' Recurrent-Event Cox Models: Andersen-Gill, PWP, Wei-Lin-Weissfeld and Frailty
#'
#' Fits a Cox model to recurrent events in counting-process form, for example repeated
#' hospital readmissions, instead of treating readmission as one binary outcome or
#' stopping at the first event. The data have one row per at-risk interval with a
#' start, a stop and an event indicator ([recurrent_intervals()] builds them from event
#' dates). Standard errors are robust (clustered on the subject) except for
#' `"frailty"`.
#'
#' * `"ag"`, Andersen-Gill: one baseline hazard, events of a subject are independent
#'   given the covariates. The hazard ratio is the effect on the rate of events.
#' * `"pwp_total"`, Prentice-Williams-Peterson with total time: a subject is at risk
#'   for event `k` only after event `k - 1`, with a baseline hazard per event number
#'   (stratified), on the time since time zero.
#' * `"pwp_gap"`: as `"pwp_total"` on the gap time since the previous event. The
#'   time scale restarts after each event.
#' * `"wlw"`, Wei-Lin-Weissfeld: marginal model in which a subject is at risk for every
#'   event number from time zero; a common effect is estimated over event numbers
#'   (stratified by event number).
#' * `"frailty"`: Andersen-Gill with a gamma frailty for the subject
#'   ([survival::frailty()]), so the variance of the frailty (`frailty_variance`)
#'   measures how much subjects differ beyond the covariates.
#'
#' Rows after an event are valid here: [validate_start_stop()] and
#' [prune_start_stop()] take `recurrent = TRUE` for the same data.
#'
#' @param data A data frame of counting-process intervals.
#' @param predictors Covariates: a character vector of column names, or a one-sided
#'   formula such as `~ age + sex`.
#' @param id,start,stop,status Column names (strings) of the subject identifier, the
#'   interval start and stop, and the event indicator (0/1 or logical).
#' @param model One of `"ag"`, `"pwp_total"`, `"pwp_gap"`, `"wlw"`, `"frailty"`.
#' @param max_events For the stratified models, event numbers above `max_events` are
#'   pooled into one stratum (default `NULL`: the largest number with at least
#'   `min_stratum_events` events and all below it). Ignored by `"ag"` and `"frailty"`.
#' @param min_stratum_events With `max_events = NULL`, the smallest number of events a
#'   stratum needs; later strata are pooled into the last one that has enough
#'   (default `10`).
#' @param conf_level Confidence level of the hazard ratios (default `0.95`).
#' @return An object of class `recurrent_cox`: a list with `fit` (the [survival::coxph()]
#'   fit), `model`, `terms` (a tibble with `term`, `estimate` (log hazard ratio),
#'   `std_error`, `hr`, `conf_low`, `conf_high`, `statistic` and `p_value`), `n_subjects`,
#'   `n_intervals`, `n_events`, `events_per_subject` (a table), `strata_events`
#'   (events per event number, `NULL` for the unstratified models), `frailty_variance`
#'   (`NA` unless `"frailty"`) and `conf_level`. `tidy()` returns `terms`.
#' @seealso [recurrent_intervals()], [recurrent_cox_windows()], [truncate_start_stop()],
#'   [validate_start_stop()], [cox_single()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(1)
#'   n <- 150
#'   subj <- data.frame(id = seq_len(n), age = rnorm(n), end = 120)
#'   ev <- do.call(rbind, lapply(seq_len(n), function(i) {
#'     k <- rpois(1, exp(0.3 * subj$age[i]) * 1.2)
#'     if (k == 0) return(NULL)
#'     data.frame(id = i, time = sort(runif(k, 1, 120)))
#'   }))
#'   long <- recurrent_intervals(ev, subj)
#'   long$age <- subj$age[long$id]
#'   fit <- recurrent_cox(long, ~ age, model = "ag")
#'   fit
#'   recurrent_cox(long, "age", model = "pwp_gap")$terms
#' }
recurrent_cox <- function(data,
                          predictors,
                          id = "id",
                          start = "tstart",
                          stop = "tstop",
                          status = "status",
                          model = c("ag", "pwp_total", "pwp_gap", "wlw", "frailty"),
                          max_events = NULL,
                          min_stratum_events = 10,
                          conf_level = 0.95) {
  rlang::check_installed("survival", reason = "to fit recurrent-event Cox models.")
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(max_events) && (!is.numeric(max_events) || length(max_events) != 1L || max_events < 1)) {
    stop("`max_events` must be a single number of at least 1.", call. = FALSE)
  }
  terms_rhs <- recurrent_terms(predictors)
  if (length(terms_rhs) == 0L) stop("`predictors` is empty.", call. = FALSE)
  cols <- c(id, start, stop, status)
  gone <- setdiff(cols, names(data))
  if (length(gone)) stop("`data` is missing column(s): ", paste(gone, collapse = ", "), call. = FALSE)

  val <- suppressWarnings(validate_start_stop(
    data, id = id, start = start, stop = stop, status = status,
    warn_no_censoring = FALSE, error_on_invalid = TRUE, recurrent = TRUE
  ))
  if (val$n_events == 0L) stop("`data` contains no events.", call. = FALSE)

  st <- data[[status]]
  st <- if (is.logical(st)) as.integer(st) else if (is.factor(st)) as.numeric(as.character(st)) else as.numeric(st)
  d <- data
  d$.rc_id <- d[[id]]
  d$.rc_t0 <- as.numeric(d[[start]])
  d$.rc_t1 <- as.numeric(d[[stop]])
  d$.rc_ev <- st
  d <- d[order(d$.rc_id, d$.rc_t0, d$.rc_t1), , drop = FALSE]
  # number of the event the interval is at risk for: 1 + earlier events of the subject
  prior <- as.numeric(stats::ave(d$.rc_ev, d$.rc_id, FUN = function(v) c(0, cumsum(v)[-length(v)])))
  d$.rc_k <- as.integer(prior + 1)

  stratified <- model %in% c("pwp_total", "pwp_gap", "wlw")
  if (stratified) {
    ev_by_k <- tapply(d$.rc_ev, d$.rc_k, sum)
    cap <- max_events
    if (is.null(cap)) {
      ok <- which(as.numeric(ev_by_k) >= min_stratum_events)
      ok <- if (length(ok)) ok[ok == seq_along(ok)] else 1L
      cap <- max(ok, 1L)
    }
    d$.rc_k <- pmin(d$.rc_k, as.integer(cap))
  }

  if (model == "pwp_gap") {
    last_event <- as.numeric(stats::ave(ifelse(d$.rc_ev == 1, d$.rc_t1, NA_real_), d$.rc_id, FUN = function(v) {
      out <- c(0, v[-length(v)])
      for (i in seq_along(out)) if (is.na(out[i])) out[i] <- if (i == 1L) 0 else out[i - 1L]
      out
    }))
    d$.rc_t0 <- d$.rc_t0 - last_event
    d$.rc_t1 <- d$.rc_t1 - last_event
  }

  if (model == "wlw") {
    # each subject is at risk for event k from time zero: the time of the k-th event, or the end of follow-up
    ks <- seq_len(cap)
    per_subject <- split(d, d$.rc_id)
    d <- do.call(rbind, lapply(per_subject, function(s) {
      ev_times <- s$.rc_t1[s$.rc_ev == 1]
      end_t <- max(s$.rc_t1)
      do.call(rbind, lapply(ks, function(k) {
        row <- s[1L, , drop = FALSE]
        if (k <= length(ev_times)) {
          row$.rc_t1 <- ev_times[k]
          row$.rc_ev <- 1
        } else {
          row$.rc_t1 <- end_t
          row$.rc_ev <- 0
        }
        row$.rc_t0 <- 0
        row$.rc_k <- k
        row
      }))
    }))
    rownames(d) <- NULL
  }

  rhs <- paste(terms_rhs, collapse = " + ")
  rhs <- switch(
    model,
    ag = paste0(rhs, " + cluster(.rc_id)"),
    pwp_total = paste0(rhs, " + strata(.rc_k) + cluster(.rc_id)"),
    pwp_gap = paste0(rhs, " + strata(.rc_k) + cluster(.rc_id)"),
    wlw = paste0(rhs, " + strata(.rc_k) + cluster(.rc_id)"),
    frailty = paste0(rhs, " + frailty(.rc_id)")
  )
  surv_lhs <- if (model == "wlw") "survival::Surv(.rc_t1, .rc_ev)" else "survival::Surv(.rc_t0, .rc_t1, .rc_ev)"
  # the survival namespace is the formula's parent, so strata(), cluster() and frailty() resolve without attaching it
  env <- new.env(parent = asNamespace("survival"))
  fml <- stats::as.formula(paste(surv_lhs, "~", rhs), env = env)
  fit <- survival::coxph(fml, data = d, ties = "efron", model = FALSE, x = FALSE, y = FALSE)

  beta <- stats::coef(fit)
  vc <- stats::vcov(fit)
  se <- sqrt(diag(vc))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  stat <- beta / se
  tab <- tibble::tibble(
    term = names(beta),
    estimate = unname(beta),
    std_error = unname(se),
    hr = exp(unname(beta)),
    conf_low = exp(unname(beta) - z * unname(se)),
    conf_high = exp(unname(beta) + z * unname(se)),
    statistic = unname(stat),
    p_value = 2 * stats::pnorm(-abs(unname(stat)))
  )

  fv <- NA_real_
  if (model == "frailty") {
    fv <- tryCatch(as.numeric(fit$history[[1L]]$theta), error = function(e) NA_real_)
    if (length(fv) != 1L) fv <- NA_real_
  }
  per_subj <- tapply(st, data[[id]], sum)
  structure(
    list(
      fit = fit,
      model = model,
      terms = tab,
      n_subjects = length(unique(data[[id]])),
      n_intervals = nrow(data),
      n_events = sum(st),
      events_per_subject = table(events = as.integer(per_subj)),
      strata_events = if (stratified) tapply(d$.rc_ev, d$.rc_k, sum) else NULL,
      frailty_variance = fv,
      conf_level = conf_level
    ),
    class = "recurrent_cox"
  )
}

#' Tidy a `recurrent_cox` Model
#'
#' @param x A [recurrent_cox()] fit.
#' @param ... Not used.
#' @return The `terms` tibble: `term`, `estimate` (log hazard ratio), `std_error`, `hr`,
#'   `conf_low`, `conf_high`, `statistic` and `p_value`.
#' @exportS3Method generics::tidy
tidy.recurrent_cox <- function(x, ...) {
  x$terms
}

#' @export
print.recurrent_cox <- function(x, ...) {
  labels <- c(
    ag = "Andersen-Gill", pwp_total = "Prentice-Williams-Peterson (total time)",
    pwp_gap = "Prentice-Williams-Peterson (gap time)", wlw = "Wei-Lin-Weissfeld", frailty = "Andersen-Gill with gamma frailty"
  )
  cat("Recurrent-event Cox model:", labels[[x$model]], "\n")
  cat(sprintf("  %d subject(s), %d interval(s), %d event(s)\n", x$n_subjects, x$n_intervals, x$n_events))
  if (!is.null(x$strata_events)) {
    cat("  Events per event number: ", paste(names(x$strata_events), x$strata_events, sep = ": ", collapse = ", "), "\n", sep = "")
  }
  if (!is.na(x$frailty_variance)) cat(sprintf("  Frailty variance: %.3f\n", x$frailty_variance))
  tab <- x$terms
  show <- data.frame(
    term = tab$term, HR = round(tab$hr, 3),
    CI = sprintf("%.3f to %.3f", tab$conf_low, tab$conf_high),
    p = signif(tab$p_value, 3)
  )
  print(show, row.names = FALSE)
  invisible(x)
}

#' Recurrent-Event Cox Models Over 30-, 60- and 90-Day Windows
#'
#' Cuts the counting-process data at each window with [truncate_start_stop()] and fits
#' [recurrent_cox()] to every cut, so the effect on recurrent readmissions within 30,
#' 60 and 90 days can be read side by side.
#'
#' @inheritParams recurrent_cox
#' @param windows Window lengths in the units of the time columns (default `c(30, 60, 90)`).
#' @param ... Passed to [recurrent_cox()] (`model`, `max_events`, `conf_level`, ...).
#' @return A tibble with one row per window and term: `window`, the columns of the `terms`
#'   table of [recurrent_cox()], and `n_events` and `n_subjects` for the window. The fits
#'   are in `attr(x, "fits")`, a list named by window. A window without events is skipped
#'   with a warning.
#' @seealso [recurrent_cox()], [truncate_start_stop()]
#' @export
#' @examples
#' d <- data.frame(
#'   id = rep(1:40, each = 2), age = rep(seq(-1, 1, length.out = 40), each = 2),
#'   tstart = rep(c(0, 25), 40), tstop = rep(c(25, 80), 40),
#'   status = rep(c(1, 0), 40)
#' )
#' d$status[c(5, 6, 12, 30, 31, 40, 52, 70)] <- 1
#' recurrent_cox_windows(d, ~ age, windows = c(30, 60), model = "ag")
recurrent_cox_windows <- function(data,
                                  predictors,
                                  windows = c(30, 60, 90),
                                  id = "id",
                                  start = "tstart",
                                  stop = "tstop",
                                  status = "status",
                                  ...) {
  if (!is.numeric(windows) || length(windows) == 0L || anyNA(windows) || any(windows <= 0)) {
    stop("`windows` must be positive numbers.", call. = FALSE)
  }
  fits <- list()
  rows <- list()
  for (w in sort(unique(windows))) {
    cut <- truncate_start_stop(data, w, start = start, stop = stop, status = status)
    fit <- tryCatch(
      recurrent_cox(cut, predictors, id = id, start = start, stop = stop, status = status, ...),
      error = function(e) {
        warning("Window ", w, " was skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(fit)) next
    key <- as.character(w)
    fits[[key]] <- fit
    rows[[key]] <- dplyr::bind_cols(
      tibble::tibble(window = w),
      fit$terms,
      tibble::tibble(n_events = fit$n_events, n_subjects = fit$n_subjects)
    )
  }
  out <- if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble()
  attr(out, "fits") <- fits
  out
}
