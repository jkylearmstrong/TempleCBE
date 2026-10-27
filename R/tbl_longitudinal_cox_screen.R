#' Cox Screen of Longitudinal Summary Statistics
#'
#' For each feature, summarises every subject's repeated measurements (see
#' [transform_longitudinal_features()]) and fits one univariate Cox model per summary, with the
#' summary as the only covariate. Answers "does the way a marker moves over time, not only its
#' level, predict survival?".
#'
#' @section Survival time and status:
#' Each subject has one follow-up time and one status. `surv_time_col` gives the time; if it is
#' `NULL`, the subject's largest `time_col` is used (the last measurement). The status is the one
#' on the subject's last row (by `time_col`). A subject whose status is missing is dropped.
#'
#' @section Metrics:
#' `"pct_change_directional"`: percent change from the first to the last value, signed (positive
#' = rose). `"variance"` and `"sd"`: within-subject. `"excursion"`: `max - min`, never negative.
#' Hazard ratios are per one unit of the summary, so they depend on its scale; standardise the
#' features first if the ratios should be compared across features.
#'
#' @param data A data frame in long format.
#' @param id_col,time_col Subject and measurement-time columns.
#' @param status_col Event indicator (1 = event, 0 = censored; or logical).
#' @param features Numeric feature columns.
#' @param metrics Any of `"pct_change_directional"`, `"variance"`, `"excursion"`, `"sd"`.
#' @param surv_time_col Optional follow-up time column (see above).
#' @param metadata Optional data frame with `columns` and optionally `labels` and `domain`; adds
#'   `label` and `domain` to the result.
#' @param conf_level Confidence level of the hazard ratio.
#' @return A tibble with `feature`, `label`, `domain` (only with `metadata`), `metric`, `n`,
#'   `events`, `hr`, `conf.low`, `conf.high`, `p.value`, `status` (`"ok"` or why no model was
#'   fitted).
#' @seealso [transform_longitudinal_features()], [batch_cox_single()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:40, each = 4), t = rep(c(0, 5, 15, 30), 40))
#' d$hr <- rnorm(160, 80, 10)
#' d$status <- rep(rbinom(40, 1, 0.5), each = 4)
#' tbl_longitudinal_cox_screen(d, "id", "t", "status", "hr")
tbl_longitudinal_cox_screen <- function(data, id_col = "subject_id", time_col = "tstart",
                                        status_col = "status", features,
                                        metrics = c("pct_change_directional", "variance", "excursion", "sd"),
                                        surv_time_col = NULL, metadata = NULL, conf_level = 0.95) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(id_col, time_col, status_col, surv_time_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  all_metrics <- c("pct_change_directional", "variance", "excursion", "sd")
  bad <- setdiff(metrics, all_metrics)
  if (length(bad) || length(metrics) == 0L) stop("Unknown metric(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.null(metadata) && !(is.data.frame(metadata) && "columns" %in% names(metadata))) {
    stop("`metadata` must be a data frame with a `columns` column.", call. = FALSE)
  }

  src <- c(pct_change_directional = "pct_change_baseline", variance = "variance",
           excursion = "max_signed_diff", sd = "sd")
  tr <- transform_longitudinal_features(data, id_col, time_col, features, methods = unique(unname(src[metrics])))
  names(tr)[1] <- ".id"

  ord <- order(data[[id_col]], data[[time_col]])
  d <- data[ord, , drop = FALSE]
  last <- !duplicated(d[[id_col]], fromLast = TRUE)
  tm <- if (is.null(surv_time_col)) {
    stats::ave(d[[time_col]], d[[id_col]], FUN = function(x) suppressWarnings(max(x, na.rm = TRUE)))
  } else {
    d[[surv_time_col]]
  }
  subj <- data.frame(.id = d[[id_col]][last], .time = tm[last], .status = as.numeric(d[[status_col]][last]))
  subj <- subj[stats::complete.cases(subj) & is.finite(subj$.time), , drop = FALSE]

  rows <- list()
  for (f in features) {
    for (m in metrics) {
      v <- tr[tr$feature == f & tr$method == src[[m]], c(".id", "value"), drop = FALSE]
      if (m == "excursion") v$value <- abs(v$value)
      df <- merge(subj, v, by = ".id")
      df <- df[is.finite(df$value), , drop = FALSE]
      rows[[length(rows) + 1L]] <- .cox_one_metric(df, f, m, conf_level)
    }
  }
  out <- dplyr::bind_rows(rows)
  if (!is.null(metadata)) {
    i <- match(out$feature, metadata$columns)
    lab <- if ("labels" %in% names(metadata)) as.character(metadata$labels[i]) else out$feature
    out$label <- ifelse(is.na(lab), out$feature, lab)
    if ("domain" %in% names(metadata)) out$domain <- as.character(metadata$domain[i])
    first <- intersect(c("feature", "label", "domain", "metric"), names(out))
    out <- out[, c(first, setdiff(names(out), first)), drop = FALSE]
  }
  out
}

# One univariate Cox fit on a data frame with .time, .status, value; failures become a status.
.cox_one_metric <- function(df, feature, metric, conf_level) {
  empty <- function(st) {
    tibble::tibble(feature = feature, metric = metric, n = nrow(df), events = sum(df$.status == 1),
                   hr = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, status = st)
  }
  if (nrow(df) < 3L || sum(df$.status == 1) < 1L) return(empty("too few subjects or events"))
  if (length(unique(df$value)) < 2L) return(empty("summary is constant"))
  fit <- tryCatch(
    suppressWarnings(survival::coxph(survival::Surv(.time, .status) ~ value, data = df)),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) return(empty(fit))
  co <- summary(fit, conf.int = conf_level)$conf.int
  p <- summary(fit)$coefficients[1, "Pr(>|z|)"]
  if (!is.finite(p)) return(empty("model did not converge"))
  res <- empty("ok")
  res$hr <- co[1, "exp(coef)"]
  res$conf.low <- co[1, 3]
  res$conf.high <- co[1, 4]
  res$p.value <- p
  res
}
