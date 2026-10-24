#' Univariate Cox Screening of Many Features
#'
#' Fits one Cox model per feature (`Surv(time, status) ~ feature`, or `Surv(start, time, status) ~ feature`
#' for counting-process data), optionally in parallel, and returns one consolidated table with hazard
#' ratios, confidence intervals and a multiple-testing adjusted p-value per feature. A feature whose
#' model fails is reported with its message instead of stopping the screen.
#'
#' @section Rows, p-values and adjustment:
#' A numeric or two-level feature gives one row; a factor with `k` levels gives `k - 1` rows (one per
#' non-reference level). `p.value` is the Wald p-value of that coefficient. `p_overall` is the p-value
#' of the feature as a whole (the Wald test of all its coefficients, `summary(fit)$waldtest`) and is the
#' one adjusted: `p_adj = p.adjust(p_overall, adjust_method)` over **features** (each feature counted
#' once). Features that failed or have no p-value are left out of the adjustment, so `p_adj` is
#' adjusted over the fitted features only.
#'
#' @param data A data frame.
#' @param time,status Names of the follow-up time and event columns (event 1/`TRUE`, censored 0/`FALSE`).
#' @param features Character vector of feature columns.
#' @param time_start Optional name of a start-time column for start-stop (counting process) data;
#'   `time` is then the stop time.
#' @param adjust_method Any method of [stats::p.adjust()] (default `"BH"`), or `"none"`.
#' @param exponentiate If `TRUE` (default) report hazard ratios with their limits; otherwise raw
#'   coefficients (log hazard ratios).
#' @param conf_level Confidence level (default 0.95).
#' @param parallel If `TRUE` (default) and the `furrr` package is installed, features are fitted with
#'   `furrr::future_map()` under the current [future::plan()] (sequential unless you set a plan). Without
#'   `furrr` the fits run one after the other.
#' @return A tibble of class `cbe_batch_cox` ordered by `p_adj`, with `feature`, `term`, `estimate`,
#'   `conf.high`, `std.error`, `statistic`, `p.value`, `p_overall`, `p_adj`, `n`, `n_events`,
#'   `status` (`"ok"`, `"not estimable"` for a coefficient `coxph` could not estimate, or the error message)
#'   and `warning` (the warnings `coxph` raised for the feature, joined with `"; "`, otherwise `""`).
#'   Look at `warning` before trusting a result: "coefficient may be infinite" means perfect separation,
#'   and the hazard ratio and its p-value are then not meaningful.
#' @seealso [cox_single()]
#' @export
#' @examples
#' lung <- survival::lung
#' lung$status <- lung$status - 1
#' batch_cox_single(lung, "time", "status", c("age", "sex", "ph.ecog", "wt.loss"), parallel = FALSE)
batch_cox_single <- function(data,
                             time = "time",
                             status = "status",
                             features,
                             time_start = NULL,
                             adjust_method = "BH",
                             exponentiate = TRUE,
                             conf_level = 0.95,
                             parallel = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  one_name <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% names(data)) {
      stop("`", arg, "` must name one column of `data`.", call. = FALSE)
    }
  }
  one_name(time, "time")
  one_name(status, "status")
  if (!is.null(time_start)) one_name(time_start, "time_start")
  features <- unique(as.character(features))
  if (length(features) == 0L) stop("`features` must name at least one column.", call. = FALSE)
  gone <- setdiff(features, names(data))
  if (length(gone)) stop("Feature(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (!adjust_method %in% c(stats::p.adjust.methods, "none")) {
    stop("`adjust_method` must be one of: ", paste(stats::p.adjust.methods, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.logical(exponentiate) || length(exponentiate) != 1L || is.na(exponentiate)) {
    stop("`exponentiate` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(data[[status]]) && !is.logical(data[[status]])) {
    stop("`status` must be numeric (0/1) or logical.", call. = FALSE)
  }
  if (!all(data[[status]][!is.na(data[[status]])] %in% c(0, 1, TRUE, FALSE))) {
    stop("`status` must be 0/1 or FALSE/TRUE.", call. = FALSE)
  }
  if (time %in% features || status %in% features || (!is.null(time_start) && time_start %in% features)) {
    stop("`features` must not include the time or status columns.", call. = FALSE)
  }

  fit_one <- function(feat) {
    d <- data[, c(if (!is.null(time_start)) time_start, time, status, feat), drop = FALSE]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    names(d) <- c(if (!is.null(time_start)) ".start", ".time", ".status", ".feat")
    if (is.factor(d$.feat)) d$.feat <- droplevels(d$.feat)
    fail <- function(msg) tibble::tibble(feature = feat, term = NA_character_, estimate = NA_real_,
                                         conf.low = NA_real_, conf.high = NA_real_, std.error = NA_real_,
                                         statistic = NA_real_, p.value = NA_real_, p_overall = NA_real_,
                                         n = nrow(d), n_events = sum(d$.status == 1), status = msg, warning = "")
    if (nrow(d) == 0L) return(fail("no complete cases"))
    if (length(unique(d$.feat)) < 2L) return(fail("feature has a single value"))
    f <- if (is.null(time_start)) {
      survival::Surv(.time, .status) ~ .feat
    } else {
      survival::Surv(.start, .time, .status) ~ .feat
    }
    warns <- character()
    res <- tryCatch({
      withCallingHandlers(
        survival::coxph(f, data = d),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) conditionMessage(e))
    if (is.character(res)) return(fail(res))
    sm <- summary(res, conf.int = conf_level)
    cf <- sm$coefficients
    ci <- stats::confint(res, level = conf_level)
    est <- cf[, "coef"]
    lo <- ci[, 1]
    hi <- ci[, 2]
    if (exponentiate) {
      est <- exp(est)
      lo <- exp(lo)
      hi <- exp(hi)
    }
    terms <- sub("^\\.feat", "", rownames(cf))
    tibble::tibble(
      feature = feat,
      term = ifelse(nzchar(terms), paste0(feat, terms), feat),
      estimate = unname(est), conf.low = unname(lo), conf.high = unname(hi),
      std.error = unname(cf[, "se(coef)"]), statistic = unname(cf[, ncol(cf) - 1L]),
      p.value = unname(cf[, ncol(cf)]),
      p_overall = unname(sm$waldtest[["pvalue"]]),
      n = res$n, n_events = res$nevent,
      status = ifelse(is.na(est), "not estimable", "ok"),
      warning = paste(unique(warns), collapse = "; ")
    )
  }

  use_furrr <- isTRUE(parallel) && length(features) > 1L && requireNamespace("furrr", quietly = TRUE)
  pieces <- if (use_furrr) {
    furrr::future_map(features, fit_one, .options = furrr::furrr_options(seed = NULL, globals = TRUE, packages = "survival"))
  } else {
    lapply(features, fit_one)
  }
  out <- dplyr::bind_rows(pieces)

  per_feat <- !duplicated(out$feature)
  adj <- rep(NA_real_, nrow(out))
  p_feat <- out$p_overall[per_feat]
  ok <- !is.na(p_feat)
  p_adj_feat <- rep(NA_real_, length(p_feat))
  if (any(ok)) p_adj_feat[ok] <- stats::p.adjust(p_feat[ok], method = adjust_method)
  adj <- p_adj_feat[match(out$feature, out$feature[per_feat])]
  out$p_adj <- adj
  out <- out[, c("feature", "term", "estimate", "conf.low", "conf.high", "std.error", "statistic",
                 "p.value", "p_overall", "p_adj", "n", "n_events", "status", "warning")]
  out <- out[order(is.na(out$p_adj), out$p_adj, out$feature, out$term), , drop = FALSE]
  class(out) <- c("cbe_batch_cox", class(out))
  out
}
