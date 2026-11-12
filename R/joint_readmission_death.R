# Joint model of recurrent readmission and death (DRAFT).
#
# This departs from the package's standing assumption, stated in NEWS.md 0.8.0
# ("Not done, and why") and in the help of recurrent_intervals(): that death
# ends follow-up and is treated as censoring, the usual Andersen-Gill / PWP
# assumption, which is informative when death is common. This file implements
# the draft extension the maintainer asked for anyway; see the Details of
# joint_readmission_death() for what it does and does not do.

#' Joint Model of Recurrent Readmission and Death (Draft)
#'
#' Fits readmission and death together instead of treating death as censoring
#' of the readmission process. Two sub-models are fit on the same
#' counting-process data:
#' \enumerate{
#'   \item \strong{Readmission}: an Andersen-Gill recurrent-event Cox model
#'     ([recurrent_cox()], `model = "ag"`, or `"frailty"` with
#'     `frailty = TRUE`) for the rate of readmissions.
#'   \item \strong{Death}: a Cox model for the time to death, with the
#'     subject's readmission history as a time-varying covariate
#'     (`n_prior_readmissions`, the number of readmissions before the
#'     interval), so the death hazard depends on the recurrent process. With
#'     `frailty = TRUE` it adds a gamma frailty for the subject.
#' }
#'
#' @details
#' **Draft status -- this departs from the package's standing assumption.**
#' `NEWS.md` 0.8.0 ("Not done, and why") explicitly declined joint models of
#' readmission and death, with the reason that death ends follow-up
#' (censoring), the usual assumption of the Andersen-Gill and PWP models,
#' which is informative when death is common; that is also stated in the help
#' of [recurrent_intervals()]. This function is a draft extension built at the
#' maintainer's request, and it does **not** replace that assumption elsewhere
#' in the package: [recurrent_cox()], [cv_coxnet()] and the other recurrent
#' tools still treat death as censoring.
#'
#' **What the draft does and does not do.** The two processes are linked by
#' the `n_prior_readmissions` covariate in the death model, not by a single
#' shared frailty estimated jointly: with `frailty = TRUE` each sub-model
#' gets its own gamma frailty (one for readmissions, one for death), and the
#' two frailty variances are reported separately. A fully joint likelihood
#' with one shared frailty (as in `frailtypack::frailtyPenal()`) is follow-up
#' work and is not implemented here; it would also add a dependency the
#' package does not currently have. Death-survival predictions are
#' conditional on a fixed number of prior readmissions (see `predict()`):
#' they do not integrate over the readmission process.
#'
#' **Death in the data.** `death` marks the subject's death at the stop time
#' of an interval: `1` on the subject's last interval when they died there,
#' `0` elsewhere. A subject who is readmitted and later dies gets a final
#' interval ending at death (`status = 0`, `death = 1`); a row must not carry
#' both a readmission and a death, and no rows may follow a death. Subjects
#' who do not die are censored for death at the end of follow-up.
#'
#' **Frailty with competing risks.** With `frailty = TRUE`, this is also the
#' draft's frailty-with-competing-risks support: the readmission and death
#' processes -- which compete, in that death ends the readmission process --
#' each carry a subject frailty. As above, the frailties are fitted
#' separately per process, not shared; a shared frailty across the competing
#' processes is follow-up work.
#'
#' @param data A data frame of counting-process intervals, as for
#'   [recurrent_cox()].
#' @param readmission_predictors Covariates of the readmission model: a
#'   character vector of column names, or a one-sided formula such as
#'   `~ age + sex`.
#' @param death_predictors Covariates of the death model: a character vector
#'   of column names, or a one-sided formula. In this draft they must be
#'   numeric columns (see `predict()`).
#' @param id,start,stop,status,death Column names (strings) of the subject
#'   identifier, the interval start and stop, the readmission indicator
#'   (0/1), and the death indicator (0/1; see Details).
#' @param frailty Logical (default `FALSE`). With `TRUE`, both sub-models get
#'   a gamma frailty for the subject.
#' @param conf_level Confidence level of the hazard ratios (default `0.95`).
#' @return An object of class `joint_readmission_death`: a list with
#'   `readmission` (the [recurrent_cox()] fit), `death_fit` (the
#'   [survival::coxph()] death fit), `terms` (a combined tibble with
#'   `process`, `term`, `estimate`, `std_error`, `hr`, `conf_low`,
#'   `conf_high`, `statistic` and `p_value`), `death_terms` (the death
#'   sub-model's terms), `n_subjects`, `n_intervals`, `n_readmissions`,
#'   `n_deaths`, `frailty`, `frailty_variance` (named `readmission` and
#'   `death`, `NA` unless `frailty = TRUE`), and `conf_level`. `tidy()`
#'   returns `terms`.
#' @seealso [recurrent_cox()], [recurrent_intervals()], [joint_model()],
#'   [cv_coxnet()]
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
#'   # Deaths: about a quarter of subjects, at the end of follow-up
#'   long$death <- 0L
#'   died <- rbinom(n, 1, plogis(-1 + 0.5 * subj$age))
#'   for (i in which(died == 1)) {
#'     idx <- which(long$id == i)
#'     long$death[idx[length(idx)]] <- 1L
#'   }
#'   fit <- joint_readmission_death(long, ~ age, ~ age)
#'   fit
#'   generics::tidy(fit)
#' }
joint_readmission_death <- function(data,
                                    readmission_predictors,
                                    death_predictors,
                                    id = "id",
                                    start = "tstart",
                                    stop = "tstop",
                                    status = "status",
                                    death = "death",
                                    frailty = FALSE,
                                    conf_level = 0.95) {
  rlang::check_installed("survival", reason = "to fit the joint readmission/death model.")
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.logical(frailty) || length(frailty) != 1L || is.na(frailty)) {
    stop("`frailty` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number between 0 and 1.", call. = FALSE)
  }
  key_cols <- c(id, start, stop, status, death)
  gone <- setdiff(key_cols, names(data))
  if (length(gone)) {
    stop("`data` is missing column(s): ", paste(gone, collapse = ", "), call. = FALSE)
  }
  if ("n_prior_readmissions" %in% names(data)) {
    stop("`data` must not contain a column named `n_prior_readmissions`; it is computed by `joint_readmission_death()`.", call. = FALSE)
  }
  read_terms <- recurrent_terms(readmission_predictors)
  death_terms <- recurrent_terms(death_predictors)
  if (!length(read_terms)) stop("`readmission_predictors` is empty.", call. = FALSE)
  if (!length(death_terms)) stop("`death_predictors` is empty.", call. = FALSE)
  read_vars <- gsub("`", "", read_terms)
  death_vars <- gsub("`", "", death_terms)
  clash <- intersect(c(read_vars, death_vars), key_cols)
  if (length(clash)) {
    stop("Predictors must not be outcome or identifier columns: ", paste(clash, collapse = ", "), ".", call. = FALSE)
  }
  gone_pred <- setdiff(c(read_vars, death_vars), names(data))
  if (length(gone_pred)) {
    stop("`data` is missing predictor column(s): ", paste(gone_pred, collapse = ", "), call. = FALSE)
  }
  not_numeric <- death_vars[!vapply(data[death_vars], is.numeric, logical(1))]
  if (length(not_numeric)) {
    stop("Death-model predictors must be numeric columns in this draft; not numeric: ", paste(not_numeric, collapse = ", "), ".", call. = FALSE)
  }

  # Validate the death column: 0/1, only on a subject's last interval, never
  # on the same row as a readmission, and nothing after a death.
  d <- check_joint_death_column(data, id, start, stop, status, death)
  if (d$n_deaths == 0L) {
    stop("`data` contains no deaths (`", death, "` is 0 everywhere); there is nothing for the death sub-model to fit.", call. = FALSE)
  }

  # 1. Readmission process: Andersen-Gill, or gamma frailty with `frailty`.
  read_fit <- recurrent_cox(
    data, readmission_predictors,
    id = id, start = start, stop = stop, status = status,
    model = if (isTRUE(frailty)) "frailty" else "ag",
    conf_level = conf_level
  )

  # 2. Death process: Cox on (tstart, tstop, death), with the number of prior
  # readmissions as a time-varying covariate and, with `frailty`, a gamma
  # frailty for the subject.
  dd <- d$data
  dd$n_prior_readmissions <- d$cum_readmissions
  death_rhs <- c(death_terms, "n_prior_readmissions")
  if (isTRUE(frailty)) {
    death_rhs <- c(death_rhs, paste0("frailty(", tick_name(id), ")"))
  }
  lhs_str <- paste0(
    "survival::Surv(", tick_name(start), ", ", tick_name(stop), ", ", tick_name(death), ")"
  )
  env <- new.env(parent = asNamespace("survival"))
  death_fml <- stats::as.formula(paste(lhs_str, "~", paste(death_rhs, collapse = " + ")), env = env)
  death_fit <- survival::coxph(death_fml, data = dd, ties = "efron", model = FALSE, x = FALSE, y = FALSE)

  beta <- stats::coef(death_fit)
  keep <- !is.na(beta)
  beta <- beta[keep]
  vc <- stats::vcov(death_fit)[keep, keep, drop = FALSE]
  se <- sqrt(diag(vc))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  stat <- beta / se
  death_terms_tbl <- tibble::tibble(
    term = names(beta),
    estimate = unname(beta),
    std_error = unname(se),
    hr = exp(unname(beta)),
    conf_low = exp(unname(beta) - z * unname(se)),
    conf_high = exp(unname(beta) + z * unname(se)),
    statistic = unname(stat),
    p_value = 2 * stats::pnorm(-abs(unname(stat)))
  )

  fv_death <- NA_real_
  if (isTRUE(frailty)) {
    fv_death <- tryCatch(as.numeric(death_fit$history[[1L]]$theta), error = function(e) NA_real_)
    if (length(fv_death) != 1L) fv_death <- NA_real_
  }

  terms <- dplyr::bind_rows(
    dplyr::bind_cols(tibble::tibble(process = "readmission"), read_fit$terms),
    dplyr::bind_cols(tibble::tibble(process = "death"), death_terms_tbl)
  )

  structure(
    list(
      readmission = read_fit,
      death_fit = death_fit,
      terms = terms,
      death_terms = death_terms_tbl,
      n_subjects = read_fit$n_subjects,
      n_intervals = read_fit$n_intervals,
      n_readmissions = read_fit$n_events,
      n_deaths = d$n_deaths,
      frailty = isTRUE(frailty),
      frailty_variance = c(readmission = read_fit$frailty_variance, death = fv_death),
      conf_level = conf_level,
      # Stored for predict(): the training intervals with the predictors the
      # sub-models were fit on, and the death model's time-varying covariate.
      train_data = dd,
      id_col = id, start_col = start, stop_col = stop, status_col = status, death_col = death,
      readmission_vars = read_vars,
      death_vars = death_vars
    ),
    class = "joint_readmission_death"
  )
}

#' Validate the Death Column of Joint Readmission/Death Data
#'
#' @param data A data frame of counting-process intervals.
#' @param id,start,stop,status,death Column names.
#' @return A list with `data` (the rows sorted by subject and time, with the
#'   `death` column coerced to 0/1), `cum_readmissions` (prior readmissions
#'   per row) and `n_deaths`.
#' @keywords internal
#' @noRd
check_joint_death_column <- function(data, id, start, stop, status, death) {
  st <- data[[status]]
  st <- if (is.logical(st)) as.integer(st) else if (is.factor(st)) as.numeric(as.character(st)) else as.numeric(st)
  dh <- data[[death]]
  dh <- if (is.logical(dh)) as.integer(dh) else if (is.factor(dh)) as.numeric(as.character(dh)) else as.numeric(dh)
  t0 <- as.numeric(data[[start]])
  t1 <- as.numeric(data[[stop]])
  sid <- data[[id]]
  if (anyNA(sid) || anyNA(t0) || anyNA(t1) || anyNA(st) || anyNA(dh)) {
    stop("The `", id, "`, `", start, "`, `", stop, "`, `", status, "` and `", death, "` columns must not contain missing values.", call. = FALSE)
  }
  if (any(!st %in% c(0, 1))) {
    stop("`", status, "` must be 0/1.", call. = FALSE)
  }
  if (any(!dh %in% c(0, 1))) {
    stop("`", death, "` must be 0/1.", call. = FALSE)
  }
  ord <- order(sid, t0, t1)
  sid <- sid[ord]; t0 <- t0[ord]; t1 <- t1[ord]; st <- st[ord]; dh <- dh[ord]
  d <- data[ord, , drop = FALSE]
  d[[death]] <- dh
  last <- !duplicated(sid, fromLast = TRUE)
  death_not_last <- dh == 1 & !last
  if (any(death_not_last)) {
    stop(
      "Death must be on a subject's last interval; subject(s) with death before their last interval: ",
      format_ids(sid[death_not_last]), ".",
      call. = FALSE
    )
  }
  both <- st == 1 & dh == 1
  if (any(both)) {
    stop(
      "An interval must not carry both a readmission and a death; subject(s): ",
      format_ids(sid[both]), ". Put the death on a final interval with `", status, " = 0`.",
      call. = FALSE
    )
  }
  # Prior readmissions per row: the count of earlier event intervals of the
  # subject (0 for the first interval).
  cum <- as.numeric(stats::ave(st, sid, FUN = function(v) c(0, cumsum(v)[-length(v)])))
  list(data = d, cum_readmissions = cum, n_deaths = sum(dh == 1))
}

#' Backtick a Column Name When It Is Not Syntactic
#' @keywords internal
#' @noRd
tick_name <- function(nm) {
  if (make.names(nm) == nm) nm else paste0("`", nm, "`")
}

#' Tidy a `joint_readmission_death` Model
#'
#' @param x A [joint_readmission_death()] fit.
#' @param ... Not used.
#' @return The `terms` tibble: `process` (`"readmission"` or `"death"`),
#'   `term`, `estimate` (log hazard ratio), `std_error`, `hr`, `conf_low`,
#'   `conf_high`, `statistic` and `p_value`.
#' @exportS3Method generics::tidy
tidy.joint_readmission_death <- function(x, ...) {
  x$terms
}

#' @rdname templecbe_methods
#' @export
print.joint_readmission_death <- function(x, ...) {
  cat("Joint model of readmission and death (DRAFT -- see ?joint_readmission_death)\n")
  cat(sprintf("  %d subject(s), %d interval(s), %d readmission(s), %d death(s)\n",
              x$n_subjects, x$n_intervals, x$n_readmissions, x$n_deaths))
  if (isTRUE(x$frailty)) {
    cat(sprintf("  Frailty variance: readmission %.3f, death %.3f (separate gamma frailties, not shared)\n",
                x$frailty_variance[["readmission"]], x$frailty_variance[["death"]]))
  }
  for (proc in c("readmission", "death")) {
    tab <- x$terms[x$terms$process == proc, , drop = FALSE]
    cat(sprintf("  %s:\n", proc))
    show <- data.frame(
      term = tab$term, HR = round(tab$hr, 3),
      CI = sprintf("%.3f to %.3f", tab$conf_low, tab$conf_high),
      p = signif(tab$p_value, 3)
    )
    print(show, row.names = FALSE)
  }
  invisible(x)
}

#' Predict From a `joint_readmission_death` Model
#'
#' @param object A [joint_readmission_death()] fit.
#' @param new_data A data frame of new predictors (one row per subject, with
#'   the predictors of both sub-models).
#' @param type `"death_survival"` for survival from death at `eval_time`, or
#'   `"readmission_cumhaz"` for the expected cumulative number of readmissions
#'   by `eval_time`.
#' @param eval_time Times to predict at (required).
#' @param n_prior_readmissions Number of prior readmissions the death-survival
#'   prediction is conditional on (default `0`, i.e. no readmission yet). The
#'   prediction does not integrate over the readmission process; it is the
#'   death survival of a subject with that many prior readmissions throughout.
#' @param ... Not used.
#' @return A tibble with one row per row of `new_data`: `.pred`, a list-column
#'   of tibbles with `.eval_time` and `.pred_survival` (death survival) or
#'   `.pred_cum_readmissions` (expected cumulative readmissions).
#'
#' Death survival is \eqn{S(t) = \exp(-H_0(t) e^{lp})}, with \eqn{H_0} the
#' Breslow baseline hazard of the death sub-model and `lp` the linear
#' predictor at `n_prior_readmissions`. Expected cumulative readmissions are
#' \eqn{H_0(t) e^{lp}} from the readmission sub-model's Breslow baseline
#' hazard. Predictions use a frailty of 1 (the population mean) when the fit
#' used a frailty. In this draft the predictors of both sub-models must be
#' numeric columns of `new_data`.
#' @export
predict.joint_readmission_death <- function(object, new_data, type = c("death_survival", "readmission_cumhaz"),
                                            eval_time = NULL, n_prior_readmissions = 0, ...) {
  type <- match.arg(type)
  if (!is.data.frame(new_data) || nrow(new_data) == 0L) {
    stop("`new_data` must be a non-empty data frame.", call. = FALSE)
  }
  if (is.null(eval_time)) {
    stop("`eval_time` is required.", call. = FALSE)
  }
  check_eval_time(eval_time, min_length = 1)
  if (!is.numeric(n_prior_readmissions) || length(n_prior_readmissions) != 1L ||
      is.na(n_prior_readmissions) || n_prior_readmissions < 0 ||
      n_prior_readmissions != round(n_prior_readmissions)) {
    stop("`n_prior_readmissions` must be a single whole number of at least 0.", call. = FALSE)
  }
  train <- object$train_data
  if (type == "death_survival") {
    vars <- object$death_vars
    beta <- stats::coef(object$death_fit)
    beta <- beta[!is.na(beta)]
    outcome <- train[[object$death_col]]
    lp_train <- joint_lp(train, vars, beta, n_prior = train$n_prior_readmissions,
                         n_prior_coef = unname(beta["n_prior_readmissions"]))
    bh <- breslow_cumhaz(matrix(lp_train, ncol = 1), train[[object$start_col]], train[[object$stop_col]], outcome)
    lp_new <- joint_lp(new_data, vars, beta, n_prior = rep(n_prior_readmissions, nrow(new_data)),
                       n_prior_coef = unname(beta["n_prior_readmissions"]))
    cumhaz <- cumhaz_at(bh, eval_time)[, 1]
    surv <- exp(-outer(exp(lp_new), cumhaz))
    tibble::tibble(.pred = lapply(seq_len(nrow(new_data)), function(i) {
      tibble::tibble(.eval_time = eval_time, .pred_survival = surv[i, ])
    }))
  } else {
    vars <- object$readmission_vars
    beta <- stats::coef(object$readmission$fit)
    beta <- beta[!is.na(beta)]
    outcome <- train[[object$status_col]]
    lp_train <- joint_lp(train, vars, beta)
    bh <- breslow_cumhaz(matrix(lp_train, ncol = 1), train[[object$start_col]], train[[object$stop_col]], outcome)
    lp_new <- joint_lp(new_data, vars, beta)
    cumhaz <- cumhaz_at(bh, eval_time)[, 1]
    cum_readm <- outer(exp(lp_new), cumhaz)
    tibble::tibble(.pred = lapply(seq_len(nrow(new_data)), function(i) {
      tibble::tibble(.eval_time = eval_time, .pred_cum_readmissions = cum_readm[i, ])
    }))
  }
}

#' Linear Predictor From Numeric Predictors and Named Coefficients
#'
#' @param data A data frame holding the predictor columns.
#' @param vars Predictor column names.
#' @param beta Named coefficients; names must match `vars` (plus
#'   `"n_prior_readmissions"` when `n_prior` is given).
#' @param n_prior Optional numeric vector of prior-readmission counts.
#' @param n_prior_coef Coefficient of `n_prior_readmissions`.
#' @return A numeric vector, one value per row of `data`.
#' @keywords internal
#' @noRd
joint_lp <- function(data, vars, beta, n_prior = NULL, n_prior_coef = NULL) {
  gone <- setdiff(vars, names(data))
  if (length(gone)) {
    stop("Missing predictor column(s): ", paste(gone, collapse = ", "), call. = FALSE)
  }
  not_numeric <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(not_numeric)) {
    stop("Predictors must be numeric columns in this draft; not numeric: ", paste(not_numeric, collapse = ", "), ".", call. = FALSE)
  }
  missing_beta <- setdiff(vars, names(beta))
  if (length(missing_beta)) {
    stop("No fitted coefficient for predictor(s): ", paste(missing_beta, collapse = ", "), ".", call. = FALSE)
  }
  x <- as.matrix(data[, vars, drop = FALSE])
  storage.mode(x) <- "numeric"
  if (anyNA(x)) {
    stop("Predictors must not contain missing values.", call. = FALSE)
  }
  lp <- as.numeric(x %*% beta[vars])
  if (!is.null(n_prior)) {
    if (is.null(n_prior_coef) || is.na(n_prior_coef)) {
      stop("Internal error: the death sub-model has no `n_prior_readmissions` coefficient.", call. = FALSE)
    }
    lp <- lp + n_prior * n_prior_coef
  }
  lp
}
