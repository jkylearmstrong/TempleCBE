#' Sweep Candidate Knots of a Piecewise-Linear Mixed Model
#'
#' Fits `y ~ t + pmax(t - k, 0) + (1 + t | id)` by maximum likelihood for each candidate change
#' point `k` and reports AIC, BIC and the in-sample RMSE (observed minus the conditional fit).
#' A knot whose model fails to fit gets `NA` metrics and a `status`.
#'
#' @param data A data frame in long format.
#' @param response Name of the numeric response.
#' @param time_col,id_col Names of the time and subject columns.
#' @param knots Candidate change points. `NULL` uses the observed times strictly between the
#'   smallest and the largest.
#' @param metric Criterion that picks the selected knot (lowest wins): `"RMSE"`, `"AIC"` or `"BIC"`.
#' @return A tibble with `knot`, `AIC`, `BIC`, `RMSE`, `singular`, `status` and `selected`
#'   (`TRUE` for the best knot by `metric`; none if every fit failed).
#' @details Needs `lme4`. Rows with a missing response, time or id are not used. Note that the
#'   RMSE favours flexible fits; prefer AIC or BIC when the knot should generalise.
#' @seealso [impute_piecewise_lmm()]
#' @export
#' @examples
#' \dontrun{
#' d <- lme4::sleepstudy
#' tune_breakpoint_lmm(d, "Reaction", "Days", "Subject", knots = 2:7, metric = "AIC")
#' }
tune_breakpoint_lmm <- function(data, response, time_col = "time", id_col = "arl_number",
                                knots = NULL, metric = c("RMSE", "AIC", "BIC")) {
  metric <- match.arg(metric)
  rlang::check_installed("lme4", reason = "to fit piecewise mixed models.")
  d <- .piecewise_data(data, response, time_col, id_col)
  obs <- d[!is.na(d$.y) & !is.na(d$.t) & !is.na(d$.id), , drop = FALSE]
  if (nrow(obs) < 3L || length(unique(obs$.id)) < 2L) {
    stop("Need at least 3 observed rows from at least 2 subjects.", call. = FALSE)
  }
  if (is.null(knots)) {
    tt <- sort(unique(obs$.t))
    knots <- tt[tt > min(tt) & tt < max(tt)]
  }
  if (!is.numeric(knots) || length(knots) == 0L || anyNA(knots)) {
    stop("`knots` must be a non-empty numeric vector without missing values.", call. = FALSE)
  }
  knots <- sort(unique(knots))
  rows <- lapply(knots, function(k) {
    fit <- .piecewise_fit(obs, k, REML = FALSE)
    if (is.character(fit)) {
      return(tibble::tibble(knot = k, AIC = NA_real_, BIC = NA_real_, RMSE = NA_real_,
                            singular = NA, status = fit))
    }
    tibble::tibble(
      knot = k, AIC = stats::AIC(fit), BIC = stats::BIC(fit),
      RMSE = sqrt(mean(stats::residuals(fit)^2)),
      singular = lme4::isSingular(fit), status = "ok"
    )
  })
  out <- dplyr::bind_rows(rows)
  out$selected <- FALSE
  if (any(!is.na(out[[metric]]))) out$selected[which.min(out[[metric]])] <- TRUE
  out
}

#' Impute Missing Values with a Piecewise-Linear Mixed Model
#'
#' Fits `y ~ t + pmax(t - knot, 0) + (1 + t | id)` (REML) to the observed rows and fills the
#' missing responses with the model prediction, using each subject's random effects when the
#' subject has observations and the population curve otherwise. A single imputation: it does not
#' propagate the imputation uncertainty.
#'
#' @inheritParams tune_breakpoint_lmm
#' @param knot Change point. `NULL` picks it with [tune_breakpoint_lmm()] using `metric`.
#' @param metric Criterion for the automatic knot.
#' @return `data` as a tibble with two added columns, `<response>_imputed` (the response with
#'   missing values filled) and `<response>_was_imputed` (`TRUE` for filled rows). Rows with a
#'   missing time or id stay `NA`. The attribute `knot` holds the change point used.
#' @seealso [tune_breakpoint_lmm()]
#' @export
#' @examples
#' \dontrun{
#' d <- lme4::sleepstudy
#' d$Reaction[c(5, 25, 60)] <- NA
#' out <- impute_piecewise_lmm(d, "Reaction", "Days", "Subject", knot = 4)
#' }
impute_piecewise_lmm <- function(data, response, time_col = "time", id_col = "arl_number",
                                 knot = NULL, metric = c("AIC", "BIC", "RMSE")) {
  metric <- match.arg(metric)
  rlang::check_installed("lme4", reason = "to fit piecewise mixed models.")
  d <- .piecewise_data(data, response, time_col, id_col)
  obs <- d[!is.na(d$.y) & !is.na(d$.t) & !is.na(d$.id), , drop = FALSE]
  if (is.null(knot)) {
    tuned <- tune_breakpoint_lmm(data, response, time_col, id_col, metric = metric)
    if (!any(tuned$selected)) stop("No candidate knot could be fitted.", call. = FALSE)
    knot <- tuned$knot[tuned$selected]
  } else if (!is.numeric(knot) || length(knot) != 1L || is.na(knot)) {
    stop("`knot` must be a single number.", call. = FALSE)
  }
  fit <- .piecewise_fit(obs, knot, REML = TRUE)
  if (is.character(fit)) stop("The piecewise model failed at knot ", knot, ": ", fit, call. = FALSE)

  need <- is.na(d$.y) & !is.na(d$.t) & !is.na(d$.id)
  filled <- d$.y
  if (any(need)) {
    nd <- d[need, , drop = FALSE]
    nd$.id <- factor(as.character(nd$.id), levels = levels(obs$.id))
    nd$.k <- pmax(nd$.t - knot, 0)
    filled[need] <- stats::predict(fit, newdata = nd, allow.new.levels = TRUE)
  }
  out <- tibble::as_tibble(data)
  out[[paste0(response, "_imputed")]] <- filled
  out[[paste0(response, "_was_imputed")]] <- need
  attr(out, "knot") <- knot
  out
}

.piecewise_data <- function(data, response, time_col, id_col) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(response, time_col, id_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[response]]) || !is.numeric(data[[time_col]])) {
    stop("`response` and `time_col` must be numeric.", call. = FALSE)
  }
  data.frame(.y = data[[response]], .t = data[[time_col]], .id = factor(data[[id_col]]))
}

# Fit one knot; returns the model or a one-line failure message.
.piecewise_fit <- function(obs, knot, REML) {
  obs$.k <- pmax(obs$.t - knot, 0)
  if (length(unique(obs$.k > 0)) < 2L) return("no observations on one side of the knot")
  tryCatch(
    suppressWarnings(suppressMessages(
      lme4::lmer(.y ~ .t + .k + (1 + .t | .id), data = obs, REML = REML,
                 control = lme4::lmerControl(check.nobs.vs.nRE = "ignore", check.conv.singular = "ignore"))
    )),
    error = function(e) conditionMessage(e)
  )
}
