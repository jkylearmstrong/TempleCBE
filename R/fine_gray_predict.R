#' Fine-Gray Subdistribution Hazard Model, Fitted for Prediction
#'
#' Fits the Fine-Gray (1999) proportional subdistribution hazards model of one
#' cause with the other causes treated as competing events, using
#' `cmprsk::crr()`, and keeps the fit so [predict.fine_gray_model()] can return
#' the predicted cumulative incidence function (CIF). It is the fitted-model
#' counterpart of [tbl_fine_gray_multi()], which returns a coefficient table
#' only: the design matrix, factor levels and event codes are stored here, so
#' new data are encoded the same way as the training data.
#'
#' The CIF for covariates `x` is `1 - exp(-exp(x'b) * H0(t))`, where `H0` is
#' the baseline cumulative subdistribution hazard, evaluated by
#' `cmprsk::predict.crr()` at the event times of the cause of interest. A
#' competing event keeps a subject in the risk set, which is what `crr()`
#' does; the CIF is therefore the probability of the cause of interest by
#' time `t` in the presence of the competing causes, not a Kaplan-Meier risk.
#'
#' @param data A data frame, one row per subject.
#' @param time_col Follow-up time.
#' @param event_col Event type, as in [tbl_cumulative_incidence()]: a
#'   censoring code, then one code per cause.
#' @param formula One-sided formula of the predictors, for example
#'   `~ age + arm + sex`.
#' @param failcode Code of the cause of interest (default 1).
#' @param cencode Code for censoring (default 0).
#' @param ... Passed to `cmprsk::crr()` (for example `maxiter`).
#'   Prediction works whether `variance` is `TRUE` or `FALSE`: `crr()`
#'   stores the baseline hazard either way.
#' @return An object of class `fine_gray_model` with the `crr` fit (`$fit`),
#'   the design matrix (`$x`), the model terms and factor levels (`$terms`,
#'   `$xlevels`), the event codes (`$failcode`, `$cencode`), and `n` /
#'   `n_event` (rows used / events of the cause of interest) / `n_dropped`
#'   (rows dropped for missing values; a warning reports the count when it
#'   is not zero).
#' @seealso [predict.fine_gray_model()], [predict_fine_gray()],
#'   [tbl_fine_gray_multi()], [tbl_fine_gray_single()],
#'   [tbl_cumulative_incidence()]
#' @param x The object to print (the result of the function).
#' @export
#' @examples
#' if (requireNamespace("cmprsk", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                   age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
#'   fit <- fine_gray(d, "time", "type", ~ age + arm)
#'   fit
#'   predict(fit, d[1:3, ])
#' }
fine_gray <- function(data, time_col, event_col, formula, failcode = 1, cencode = 0, ...) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `fine_gray()`.", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("`formula` must be one-sided, for example `~ age + arm`.", call. = FALSE)
  }
  for (v in c(time_col, event_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[time_col]])) stop("`time_col` must be numeric.", call. = FALSE)
  if (length(failcode) != 1L || length(cencode) != 1L || identical(failcode, cencode)) {
    stop("`failcode` and `cencode` must be two different single codes.", call. = FALSE)
  }
  miss <- setdiff(all.vars(formula), names(data))
  if (length(miss)) stop("Not columns of `data`: ", paste(miss, collapse = ", "), ".", call. = FALSE)

  # Event codes are compared in character space, so a factor event column
  # works: `1 %in% c("0", "1")` is FALSE, and `crr()` would otherwise read a
  # character failcode against numeric codes (or the reverse) as no event.
  # A factor of numeric codes ("0", "1", "2") is read back as numbers; other
  # codes stay character and need matching character `failcode`/`cencode`.
  ev_raw <- data[[event_col]]
  if (is.factor(ev_raw)) ev_raw <- as.character(ev_raw)
  if (is.character(ev_raw)) {
    ev_num <- suppressWarnings(as.numeric(ev_raw))
    if (!anyNA(ev_num)) ev_raw <- ev_num
  }
  ev_is_char <- is.character(ev_raw)
  failcode <- if (ev_is_char) as.character(failcode) else {
    if (!is.numeric(failcode)) stop("`failcode` must be numeric when `event_col` is numeric.", call. = FALSE)
    failcode
  }
  cencode <- if (ev_is_char) as.character(cencode) else {
    if (!is.numeric(cencode)) stop("`cencode` must be numeric when `event_col` is numeric.", call. = FALSE)
    cencode
  }
  ev <- if (ev_is_char) ev_raw else as.numeric(ev_raw)

  rhs <- data[, all.vars(formula), drop = FALSE]
  keep <- !is.na(data[[time_col]]) & !is.na(ev) & stats::complete.cases(rhs)
  n_dropped <- sum(!keep)
  d <- data[keep, , drop = FALSE]
  ev <- ev[keep]
  time <- d[[time_col]]
  if (!failcode %in% ev) stop("No row has the event code `failcode` = ", failcode, ".", call. = FALSE)
  if (!cencode %in% ev) stop("No row has the censoring code `cencode` = ", cencode, ".", call. = FALSE)
  if (any(time < 0)) stop("`time_col` has negative values.", call. = FALSE)
  for (v in names(rhs)) {
    if (is.character(d[[v]])) d[[v]] <- factor(d[[v]])
    if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  }

  mf <- stats::model.frame(formula, data = d)
  terms <- stats::terms(mf)
  xlevels <- lapply(mf, function(col) if (is.factor(col)) levels(col) else NULL)
  xlevels <- xlevels[!vapply(xlevels, is.null, logical(1))]
  mm <- stats::model.matrix(terms, data = mf)
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  if (ncol(mm) == 0L) stop("`formula` has no predictor.", call. = FALSE)
  const <- apply(mm, 2L, function(col) !isTRUE(stats::var(col) > 0))
  if (any(const)) {
    stop("Zero-variance term(s) in the rows used: ", paste(colnames(mm)[const], collapse = ", "), ".",
         call. = FALSE)
  }

  fit <- cmprsk::crr(ftime = time, fstatus = ev, cov1 = mm, failcode = failcode, cencode = cencode, ...)
  if (n_dropped > 0L) {
    warning("`fine_gray()` dropped ", n_dropped, " of ", nrow(data),
            " rows with missing values in the follow-up time, the event code or a predictor; ",
            "the fit uses ", nrow(d), " rows.", call. = FALSE)
  }
  structure(
    list(
      fit = fit, x = mm, terms = terms, xlevels = xlevels,
      time_col = time_col, event_col = event_col, formula = formula,
      failcode = failcode, cencode = cencode,
      n = nrow(d), n_event = sum(ev == failcode), n_dropped = n_dropped
    ),
    class = "fine_gray_model"
  )
}

#' Predict the Cumulative Incidence Function from a Fine-Gray Model
#'
#' Predicted cumulative incidence of the cause of interest for new subjects
#' under a [fine_gray()] fit, at the event times of that cause (or at
#' `times`). The CIF is a step function: a requested time takes the value at
#' the latest event time at or before it, and 0 before the first event time.
#'
#' @param object A [fine_gray()] fit.
#' @param new_data A data frame of new predictors. `NULL` (the default)
#'   predicts on the training data.
#' @param times Times to predict the CIF at. `NULL` (the default) is every
#'   event time of the cause of interest in the training data.
#' @param ... Not used.
#' @return A tibble with one row per row of `new_data`: `.pred`, a list-column
#'   of tibbles with `.eval_time` and `.pred_cif` (the predicted cumulative
#'   incidence of the cause of interest by that time). Rows of `new_data`
#'   with a missing predictor are kept, with an all-`NA` CIF, so the output
#'   always aligns positionally with `new_data`.
#' @seealso [fine_gray()], [predict_fine_gray()], [tbl_cumulative_incidence()]
#' @exportS3Method
predict.fine_gray_model <- function(object, new_data = NULL, times = NULL, ...) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `predict.fine_gray_model()`.", call. = FALSE)
  }
  if (is.null(new_data)) {
    new_mm <- object$x
    n_new <- nrow(new_mm)
    na_rows <- rep(FALSE, n_new)
  } else {
    if (!is.data.frame(new_data)) stop("`new_data` must be a data frame.", call. = FALSE)
    if (nrow(new_data) == 0L) stop("`new_data` has no rows.", call. = FALSE)
    n_new <- nrow(new_data)
    # Rows with a missing predictor keep their place in the output with an
    # NA CIF. They are split out before encoding: model.frame() would
    # otherwise drop them silently (its default na.omit), and model.matrix()
    # on an all-NA column does not even keep the training columns, so the
    # dropped rows could not be re-attached afterwards.
    pred_vars <- all.vars(object$terms)
    miss_vars <- setdiff(pred_vars, names(new_data))
    if (length(miss_vars)) {
      stop("`new_data` is missing predictor(s): ", paste(miss_vars, collapse = ", "), ".", call. = FALSE)
    }
    na_rows <- !stats::complete.cases(new_data[, pred_vars, drop = FALSE])
    complete_data <- new_data[!na_rows, , drop = FALSE]
    if (nrow(complete_data) == 0L) {
      new_mm <- NULL
    } else {
      mf <- tryCatch(
        stats::model.frame(object$terms, data = complete_data, xlev = object$xlevels),
        error = function(e) stop("Could not encode `new_data` with the training predictors: ", conditionMessage(e), call. = FALSE)
      )
      new_mm <- stats::model.matrix(object$terms, data = mf)
      new_mm <- new_mm[, colnames(new_mm) != "(Intercept)", drop = FALSE]
      if (!identical(colnames(new_mm), colnames(object$x))) {
        stop("`new_data` does not give the training predictors (expected: ",
             paste(colnames(object$x), collapse = ", "), ").", call. = FALSE)
      }
    }
  }
  if (!is.null(times)) {
    if (!is.numeric(times) || anyNA(times) || any(times < 0)) {
      stop("`times` must be non-negative numbers.", call. = FALSE)
    }
    times <- sort(unique(times))
    eval_time <- times
  } else {
    # the training event times of the cause of interest, from the fit itself
    eval_time <- object$fit$uftime
  }

  # predict.crr() returns a matrix whose first column is the event times and
  # whose remaining columns are the CIF of one subject each. Only the rows
  # with complete predictors go through it; the NA rows stay NA below.
  complete_idx <- which(!na_rows)
  cif <- matrix(NA_real_, nrow = n_new, ncol = length(eval_time))
  if (length(complete_idx) > 0L) {
    pr <- cmprsk::predict.crr(object$fit, cov1 = as.matrix(new_mm))
    uftime <- unname(pr[, 1L])
    cif_complete <- t(unclass(pr)[, -1L, drop = FALSE]) # rows: subjects, columns: uftime
    if (!is.null(times)) {
      # step-function lookup: the value at the latest event time at or before
      # each requested time, 0 before the first event time
      idx <- findInterval(times, uftime)
      cif_req <- matrix(0, nrow = length(complete_idx), ncol = length(times))
      hit <- idx > 0L
      # no `drop = FALSE` on the replacement side: `[<-` does not take it
      if (any(hit)) cif_req[, hit] <- cif_complete[, idx[hit], drop = FALSE]
      cif_complete <- cif_req
    } else {
      eval_time <- uftime
    }
    cif[complete_idx, ] <- cif_complete
  }
  if (any(!is.finite(cif) & !is.na(cif))) stop("Predicted cumulative incidence must be finite.", call. = FALSE)
  cif <- pmin(pmax(cif, 0), 1)
  tibble::tibble(.pred = lapply(seq_len(n_new), function(i) {
    tibble::tibble(.eval_time = eval_time, .pred_cif = cif[i, ])
  }))
}

#' Predict the Cumulative Incidence Function in One Step
#'
#' Fits a Fine-Gray model with [fine_gray()] and predicts the cumulative
#' incidence function for `new_data` (or the training data) in one call.
#'
#' @param data A data frame, one row per subject.
#' @param time_col Follow-up time.
#' @param event_col Event type, as in [tbl_cumulative_incidence()]: a
#'   censoring code, then one code per cause.
#' @param formula One-sided formula of the predictors, for example
#'   `~ age + arm`.
#' @param new_data A data frame of new predictors. `NULL` (the default)
#'   predicts on `data`.
#' @param times Times to predict the CIF at; `NULL` is every event time of
#'   the cause of interest in `data`.
#' @param failcode Code of the cause of interest (default 1).
#' @param cencode Code for censoring (default 0).
#' @param ... Passed to [fine_gray()] (and through it to `cmprsk::crr()`).
#' @return As [predict.fine_gray_model()]: a tibble with one row per row of
#'   `new_data`, with a `.pred` list-column of `.eval_time` / `.pred_cif`
#'   tibbles.
#' @seealso [fine_gray()], [predict.fine_gray_model()]
#' @export
#' @examples
#' if (requireNamespace("cmprsk", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                   age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
#'   predict_fine_gray(d, "time", "type", ~ age + arm, new_data = d[1:3, ], times = c(5, 10))
#' }
predict_fine_gray <- function(data, time_col, event_col, formula, new_data = NULL, times = NULL,
                              failcode = 1, cencode = 0, ...) {
  fit <- fine_gray(data, time_col, event_col, formula, failcode = failcode, cencode = cencode, ...)
  predict.fine_gray_model(fit, new_data = new_data, times = times)
}

#' @rdname fine_gray
#' @exportS3Method
print.fine_gray_model <- function(x, ...) {
  cat("<fine_gray_model> Fine-Gray subdistribution hazard model\n")
  cat("  rows:", x$n, " events of cause", format(x$failcode), ":", x$n_event, "\n")
  cat("  subdistribution hazard ratios:\n")
  beta <- x$fit$coef
  shr <- exp(beta)
  for (nm in names(beta)) {
    cat("   ", nm, ": ", format(shr[[nm]], digits = 4), "\n", sep = "")
  }
  invisible(x)
}
