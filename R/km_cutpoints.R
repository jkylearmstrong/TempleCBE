#' Kaplan-Meier Optimal Cutpoint Search and Visualization
#'
#' Evaluates candidate thresholds for continuous biomarkers or risk scores to find
#' the optimal cutpoint maximizing log-rank separation or hazard ratio divergence,
#' with support for standard right-censored and time-varying counting-process data.
#'
#' @name km_cutpoints
NULL

# -log10(p) with a floor so that p = 0 stays finite
.neg_log10_p <- function(p) -log10(pmax(p, 1e-16))

.cutpoint_criteria_levels <- function(alpha) {
  c("median", "maxstat", "max_logrank", paste0("first_p<", format(alpha, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)),
    "lowest_p")
}

# One row per selection rule, from the scanned candidates. `eval_cut(c, enforce = FALSE)` scores a
# cutpoint that is not on the candidate grid (the median, the maxstat estimate).
.cutpoint_criteria <- function(candidates, vals, cox_coef, alpha, eval_cut, maxstat_cut = NULL) {
  rows <- list()
  add <- function(name, row) {
    if (is.null(row) || nrow(row) == 0L) return(invisible())
    row$criterion <- name
    rows[[length(rows) + 1L]] <<- row
  }
  add("median", eval_cut(stats::median(vals), enforce = FALSE))
  if (!is.null(maxstat_cut)) add("maxstat", eval_cut(maxstat_cut, enforce = FALSE))
  add("max_logrank", candidates[which.max(candidates$chisq), , drop = FALSE])
  # the most inclusive split on the risk side: scan up from the lowest cutpoint when larger values carry
  # more risk, down from the highest otherwise
  ascending <- isTRUE(cox_coef > 0)
  for (a in alpha) {
    s <- candidates[!is.na(candidates$p_value) & candidates$p_value < a, , drop = FALSE]
    if (nrow(s) == 0L) next
    pick <- if (ascending) which.min(s$cutpoint) else which.max(s$cutpoint)
    add(paste0("first_p<", format(a, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)), s[pick, , drop = FALSE])
  }
  add("lowest_p", candidates[which.min(candidates$p_value), , drop = FALSE])
  out <- dplyr::bind_rows(rows)
  out$criterion <- factor(out$criterion, levels = intersect(.cutpoint_criteria_levels(alpha), out$criterion))
  out$neg_log10_p <- .neg_log10_p(out$p_value)
  out[c("criterion", setdiff(names(out), "criterion"))]
}

.check_cutpoint_alpha <- function(alpha) {
  if (is.null(alpha)) return(numeric())
  if (!is.numeric(alpha) || anyNA(alpha) || any(alpha <= 0 | alpha > 1)) {
    stop("`alpha` must be numbers in (0, 1].", call. = FALSE)
  }
  unique(alpha)
}

.candidate_cutpoints <- function(vals, cutpoints, min_prop, n_cuts) {
  if (cutpoints == "midpoints") {
    v <- sort(unique(vals))
    (v[-1L] + v[-length(v)]) / 2
  } else {
    unique(stats::quantile(vals, probs = seq(min_prop, 1 - min_prop, length.out = n_cuts), na.rm = TRUE))
  }
}

#' Search for Optimal Survival Cutpoint via Log-Rank Separation
#'
#' Evaluates candidate thresholds of a continuous variable to stratify patients into
#' low-risk and high-risk groups. Evaluates log-rank test statistics and univariable
#' Cox hazard ratios across candidate percentiles to identify the optimal stratification.
#'
#' Replaces inline cutpoint selection scripts across clinical survival reports.
#'
#' @section Selection criteria:
#' `criteria` has one row per rule, each a split of the cohort at `value > cutpoint`:
#' * `median`: the median of the variable (scored even when it is not a candidate).
#' * `maxstat`: the maximally selected log-rank statistic of `maxstat::maxstat.test()`, which is what
#'   `survminer::surv_cutpoint()` reports. Present only when the \pkg{maxstat} package is installed.
#' * `max_logrank`: the candidate with the largest log-rank chi-square. The optimum of the object
#'   (`optimal_cutpoint`) is this row.
#' * `first_p<alpha`, one per value of `alpha`: the first candidate with a p-value below `alpha`,
#'   scanning up from the lowest candidate when the Cox coefficient of the continuous variable is
#'   positive and down from the highest otherwise (the most inclusive split on the risk side). Absent
#'   when no candidate reaches that level.
#' * `lowest_p`: the candidate with the smallest p-value (the same split as `max_logrank` for
#'   right-censored data, where the p-value is a function of the chi-square).
#'
#' All of these except `median` choose the cutpoint by looking at the outcome, so their p-values and
#' hazard ratios are optimistic and are not valid tests; report them as exploratory or validate the
#' cutpoint in other data.
#'
#' @param data Data frame containing survival follow-up and biomarker columns.
#' @param time_col Character string naming the follow-up time column.
#' @param status_col Character string naming the event status column (1 = event, 0 = censored).
#' @param var_col Character string naming the continuous biomarker or index variable.
#' @param min_prop Minimum proportion of subjects required in each stratified subgroup (default 0.10).
#' @param n_cuts Number of quantile evaluation points between `min_prop` and `1 - min_prop` (default 50).
#' @param cutpoints Candidate cutpoints: `"quantiles"` (default; `n_cuts` quantiles between `min_prop`
#'   and `1 - min_prop`) or `"midpoints"` (every midpoint between consecutive distinct values, still
#'   subject to `min_prop`).
#' @param alpha Significance levels for the `first_p<alpha` criteria (default `c(0.1, 0.05, 0.001)`;
#'   `NULL` for none).
#' @return A list with class `"cbe_km_cutpoint"` containing:
#'   \item{optimal_cutpoint}{Numeric value of the optimal cutpoint threshold}
#'   \item{optimal_p}{P-value at the optimal cutpoint}
#'   \item{optimal_hr}{Hazard ratio of high vs low group at the optimal cutpoint}
#'   \item{candidates}{A tibble of evaluated candidate cutpoints and statistics}
#'   \item{criteria}{A tibble with one row per selection criterion (see Selection criteria): `criterion`,
#'     the columns of `candidates`, and `neg_log10_p`}
#'   \item{cox_coef}{Coefficient of the continuous variable in a univariable Cox model}
#'   \item{data}{The input data frame augmented with a \code{<var_col>_strata} binary factor column}
#'   \item{time_col}{Name of time column}
#'   \item{status_col}{Name of status column}
#'   \item{var_col}{Name of biomarker column}
#' @seealso [plot_km_cutpoints()], [plot_km_cutpoint()], [plot_cutpoint_bar()], [km_cutpoint_time_varying()],
#'   [batch_cutpoint_survdiff()]
#' @export
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(123)
#'   df <- data.frame(
#'     time = stats::rexp(100, 0.1),
#'     status = stats::rbinom(100, 1, 0.7),
#'     marker = stats::rnorm(100, 50, 10)
#'   )
#'   res <- km_cutpoint_search(df, "time", "status", "marker")
#'   res$optimal_cutpoint
#'   res$criteria
#' }
km_cutpoint_search <- function(data,
                               time_col,
                               status_col,
                               var_col,
                               min_prop = 0.10,
                               n_cuts = 50,
                               cutpoints = c("quantiles", "midpoints"),
                               alpha = c(0.1, 0.05, 0.001)) {
  cutpoints <- match.arg(cutpoints)
  alpha <- .check_cutpoint_alpha(alpha)
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package \"survival\" is required for km_cutpoint_search().", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  req_cols <- c(time_col, status_col, var_col)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- data[!is.na(data[[time_col]]) & !is.na(data[[status_col]]) & !is.na(data[[var_col]]), , drop = FALSE]
  n <- nrow(df)
  if (n < 10) {
    stop("At least 10 complete observations required for cutpoint evaluation.", call. = FALSE)
  }

  vals <- df[[var_col]]
  cuts <- .candidate_cutpoints(vals, cutpoints, min_prop, n_cuts)
  form_diff <- stats::as.formula(sprintf("survival::Surv(%s, %s) ~ group_bin", time_col, status_col))

  eval_cut <- function(c_val, enforce = TRUE) {
    grp <- ifelse(vals > c_val, 1L, 0L)
    n_high <- sum(grp == 1L)
    n_low <- sum(grp == 0L)
    if (n_high == 0L || n_low == 0L) return(NULL)
    if (enforce && (n_high < min_prop * n || n_low < min_prop * n)) return(NULL)

    df_sub <- df
    df_sub$group_bin <- factor(grp, levels = c(0, 1))

    # Log-rank test
    sdiff <- tryCatch(survival::survdiff(form_diff, data = df_sub), error = function(e) NULL)
    if (is.null(sdiff)) return(NULL)

    chisq <- sdiff$chisq
    p_val <- 1 - stats::pchisq(chisq, df = 1)

    # Cox model
    cfit <- tryCatch(survival::coxph(form_diff, data = df_sub), error = function(e) NULL)
    hr <- if (!is.null(cfit)) exp(stats::coef(cfit)[1]) else NA_real_
    ci <- if (!is.null(cfit)) exp(stats::confint(cfit)[1, ]) else c(NA_real_, NA_real_)

    tibble::tibble(
      cutpoint = unname(c_val),
      n_low = n_low,
      n_high = n_high,
      chisq = chisq,
      p_value = p_val,
      hr = unname(hr),
      conf_low = unname(ci[1]),
      conf_high = unname(ci[2])
    )
  }

  results <- dplyr::bind_rows(lapply(cuts, eval_cut))
  if (nrow(results) == 0L) {
    stop("No valid candidate cutpoints found satisfying `min_prop` constraints.", call. = FALSE)
  }

  best_idx <- which.max(results$chisq)
  opt_cut <- results$cutpoint[best_idx]

  sv <- survival::Surv(df[[time_col]], df[[status_col]])
  cox_coef <- .cutpoint_cox_coef(sv, vals)
  maxstat_cut <- NULL
  if (requireNamespace("maxstat", quietly = TRUE)) {
    md <- data.frame(.t = df[[time_col]], .s = df[[status_col]], .x = vals)
    maxstat_cut <- tryCatch(
      unname(maxstat::maxstat.test(survival::Surv(.t, .s) ~ .x, data = md, smethod = "LogRank", pmethod = "none",
                                   minprop = min_prop, maxprop = 1 - min_prop)$estimate),
      error = function(e) NULL
    )
  }

  # Augment data with strata
  strata_name <- paste0(var_col, "_strata")
  data[[strata_name]] <- factor(
    ifelse(data[[var_col]] > opt_cut, "High", "Low"),
    levels = c("Low", "High")
  )

  out <- list(
    optimal_cutpoint = opt_cut,
    optimal_p = results$p_value[best_idx],
    optimal_hr = results$hr[best_idx],
    candidates = results,
    criteria = .cutpoint_criteria(results, vals, cox_coef, alpha, eval_cut, maxstat_cut),
    cox_coef = cox_coef,
    data = data,
    time_col = time_col,
    status_col = status_col,
    var_col = var_col
  )
  class(out) <- c("cbe_km_cutpoint", "list")
  out
}

#' Plot Cutpoint Search Profile
#'
#' Displays the profile of log-rank statistics (-log10(p)) across evaluated
#' candidate cutpoints, highlighting the optimal threshold.
#'
#' @param x An object of class `"cbe_km_cutpoint"` from [km_cutpoint_search()].
#' @param ... Additional arguments (unused).
#' @param alpha Significance levels drawn as labeled horizontal reference lines at `-log10(alpha)`
#'   (default `c(0.1, 0.05, 0.001)`; `NULL` for none). A level far above every point is not drawn, so
#'   it does not stretch the axis.
#' @return A `ggplot2` object.
#' @seealso [km_cutpoint_search()], [plot_cutpoint_bar()]
#' @export
plot_km_cutpoints <- function(x, ..., alpha = c(0.1, 0.05, 0.001)) {
  if (!inherits(x, "cbe_km_cutpoint")) {
    stop("`x` must be a 'cbe_km_cutpoint' object from km_cutpoint_search().", call. = FALSE)
  }
  alpha <- .check_cutpoint_alpha(alpha)

  df <- x$candidates
  df$neg_log10_p <- .neg_log10_p(df$p_value)

  opt_x <- x$optimal_cutpoint
  opt_y <- .neg_log10_p(x$optimal_p)

  p_str <- if (x$optimal_p < 0.001) "p < 0.001" else sprintf("p = %.4f", x$optimal_p)
  hr_str <- sprintf("HR = %.2f", x$optimal_hr)
  sub_text <- sprintf("Optimal Cutpoint: %.3f (%s, %s)", opt_x, hr_str, p_str)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p))
  # only the levels within reach of the profile: a far-away line would flatten it
  shown <- alpha[-log10(alpha) <= max(df$neg_log10_p, na.rm = TRUE) * 1.25]
  if (length(shown)) {
    ref <- data.frame(y = -log10(shown), label = paste0("p = ", format(shown, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)))
    p <- p +
      ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = .data$y), linetype = "dotted", color = "grey40") +
      ggplot2::geom_text(data = ref, ggplot2::aes(x = min(df$cutpoint), y = .data$y, label = .data$label),
                         hjust = 0, vjust = -0.4, size = 3, color = "grey30")
  }
  p +
    ggplot2::geom_line(color = "#6F6F6F", linewidth = 1) +
    ggplot2::geom_point(color = "#2B2B2B", size = 2) +
    ggplot2::geom_vline(xintercept = opt_x, linetype = "dashed", color = "#A41E35", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(cutpoint = opt_x, neg_log10_p = opt_y),
                        ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p),
                        color = "#A41E35", size = 4) +
    theme_cbe() +
    ggplot2::labs(
      title = paste0("Optimal Cutpoint Profile: ", x$var_col),
      subtitle = sub_text,
      x = paste0("Candidate Cutpoint (", x$var_col, ")"),
      y = expression(-log[10](italic(p)-value))
    )
}

# criteria of one search, of a named list of searches, or a ready-made criteria table
.cutpoint_criteria_frame <- function(x) {
  if (inherits(x, "cbe_km_cutpoint")) {
    out <- x$criteria
    if (is.null(out)) stop("`x` has no `criteria`; run km_cutpoint_search() again with this version.", call. = FALSE)
    out$feature <- x$var_col
  } else if (is.data.frame(x)) {
    need <- setdiff(c("criterion", "cutpoint", "p_value"), names(x))
    if (length(need)) stop("`x` lacks column(s): ", paste(need, collapse = ", "), ".", call. = FALSE)
    out <- x
    if (!"feature" %in% names(out)) out$feature <- ""
    if (!"neg_log10_p" %in% names(out)) out$neg_log10_p <- .neg_log10_p(out$p_value)
  } else if (is.list(x) && length(x) && all(vapply(x, inherits, logical(1), "cbe_km_cutpoint"))) {
    parts <- lapply(seq_along(x), function(i) {
      d <- .cutpoint_criteria_frame(x[[i]])
      d$criterion <- as.character(d$criterion)
      if (!is.null(names(x)) && nzchar(names(x)[i])) d$feature <- names(x)[i]
      d
    })
    out <- dplyr::bind_rows(parts)
    lev <- unique(unlist(lapply(x, function(z) levels(z$criteria$criterion))))
    out$criterion <- factor(out$criterion, levels = lev)
  } else {
    stop("`x` must be a km_cutpoint_search() result, a list of them, or a criteria data frame.", call. = FALSE)
  }
  out$feature <- factor(out$feature, levels = unique(out$feature))
  tibble::as_tibble(out)
}

#' Compare Cutpoint Selection Criteria
#'
#' `plot_cutpoint_bar()` draws one bar per selection criterion (median split, maximal log-rank,
#' first significant cutpoint at each level, lowest p-value) of [km_cutpoint_search()] or
#' [km_cutpoint_time_varying()], with the cutpoint value written on the bar.
#' `plot_cutpoint_scatter()` places the same criteria at their cutpoint (x) and `-log10(p)` (y).
#' Give a named list of results to compare several features, one panel each.
#'
#' @param x A `cbe_km_cutpoint` object, a (named) list of them, or a data frame with `criterion`,
#'   `cutpoint` and `p_value` (and optionally `feature`, `hr`).
#' @param metric What the bar height is: `"neg_log10_p"` (default), `"hr"` (hazard ratio of high
#'   versus low, on a log axis) or `"cutpoint"`.
#' @param alpha Significance levels drawn as dotted reference lines when the y-axis is `-log10(p)`
#'   (default `0.05`; `NULL` for none).
#' @param digits Significant digits of the cutpoint labels.
#' @return A `ggplot2` object.
#' @seealso [km_cutpoint_search()], [plot_km_cutpoints()], [plot_km_cutpoint()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(120, 0.1), status = rbinom(120, 1, 0.7), marker = rnorm(120))
#' res <- km_cutpoint_search(d, "time", "status", "marker")
#' plot_cutpoint_bar(res)
#' plot_cutpoint_scatter(res)
plot_cutpoint_bar <- function(x, metric = c("neg_log10_p", "hr", "cutpoint"), alpha = 0.05, digits = 3) {
  metric <- match.arg(metric)
  alpha <- .check_cutpoint_alpha(alpha)
  df <- .cutpoint_criteria_frame(x)
  if (metric == "hr" && !"hr" %in% names(df)) stop("`x` has no `hr` column.", call. = FALSE)
  df$.y <- df[[metric]]
  df$.lab <- formatC(df$cutpoint, digits = digits, format = "g")
  y_lab <- switch(metric,
    neg_log10_p = expression(-log[10](italic(p)-value)),
    hr = "Hazard ratio, high vs low (log scale)",
    cutpoint = "Cutpoint"
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$criterion, y = .data$.y)) +
    ggplot2::geom_col(fill = "#A41E35", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$.lab), vjust = -0.4, size = 3) +
    theme_cbe() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::labs(title = "Cutpoint Selection Criteria", x = NULL, y = y_lab,
                  caption = "Bar labels are the cutpoint values. Data-driven cutpoints give optimistic p-values.")
  if (metric == "neg_log10_p" && length(alpha)) {
    p <- p + ggplot2::geom_hline(yintercept = -log10(alpha), linetype = "dotted", color = "grey40")
  }
  if (metric == "hr") {
    p <- p + ggplot2::scale_y_log10() + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey50")
  } else {
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12)))
  }
  if (nlevels(df$feature) > 1L) p <- p + ggplot2::facet_wrap(~feature, scales = "free_y")
  p
}

#' @rdname plot_cutpoint_bar
#' @export
plot_cutpoint_scatter <- function(x, alpha = 0.05) {
  alpha <- .check_cutpoint_alpha(alpha)
  df <- .cutpoint_criteria_frame(x)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p,
                                        color = .data$criterion, shape = .data$criterion))
  if (inherits(x, "cbe_km_cutpoint")) {
    cand <- x$candidates
    cand$neg_log10_p <- .neg_log10_p(cand$p_value)
    p <- p + ggplot2::geom_line(data = cand, ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p),
                                inherit.aes = FALSE, color = "grey70")
  }
  if (length(alpha)) p <- p + ggplot2::geom_hline(yintercept = -log10(alpha), linetype = "dotted", color = "grey40")
  n_crit <- nlevels(droplevels(factor(df$criterion)))
  p <- p +
    ggplot2::geom_point(size = 3.5, stroke = 1.1) +
    ggplot2::scale_shape_manual(values = rep_len(c(16, 17, 15, 18, 8, 4, 3, 1), n_crit)) +
    theme_cbe() +
    ggplot2::labs(title = "Cutpoints by Selection Criterion", x = "Cutpoint",
                  y = expression(-log[10](italic(p)-value)), color = "Criterion", shape = "Criterion")
  if (nlevels(df$feature) > 1L) p <- p + ggplot2::facet_wrap(~feature, scales = "free")
  p
}

#' Kaplan-Meier Curves at a Selected Cutpoint
#'
#' Splits the cohort of a [km_cutpoint_search()] (or [km_cutpoint_time_varying()]) result into Low
#' (`<= cutpoint`) and High (`> cutpoint`) at the cutpoint of one selection criterion and draws the
#' two Kaplan-Meier curves with [plot_survival_km()], with the cutpoint, hazard ratio and p-value in
#' the subtitle.
#'
#' @param x A `cbe_km_cutpoint` object.
#' @param criterion Name of a row of `x$criteria` (default `"max_logrank"`, the optimum), or a number
#'   to use as the cutpoint directly.
#' @param risk_table,time_breaks,base_size Passed to [plot_survival_km()].
#' @param title Plot title (default: the variable name).
#' @return A `ggplot2` / `ggsurvfit` object.
#' @seealso [km_cutpoint_search()], [plot_survival_km()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(120, 0.1), status = rbinom(120, 1, 0.7), marker = rnorm(120))
#' res <- km_cutpoint_search(d, "time", "status", "marker")
#' plot_km_cutpoint(res, criterion = "median")
plot_km_cutpoint <- function(x, criterion = "max_logrank", risk_table = TRUE, time_breaks = NULL,
                             title = NULL, base_size = 13) {
  if (!inherits(x, "cbe_km_cutpoint")) {
    stop("`x` must be a 'cbe_km_cutpoint' object from km_cutpoint_search().", call. = FALSE)
  }
  if (length(criterion) != 1L || is.na(criterion)) stop("`criterion` must be one name or one number.", call. = FALSE)
  if (is.numeric(criterion)) {
    cut <- criterion
    sub_text <- sprintf("Cutpoint %s", formatC(cut, digits = 4, format = "g"))
  } else {
    row <- x$criteria[as.character(x$criteria$criterion) == criterion, , drop = FALSE]
    if (nrow(row) == 0L) {
      stop("`criterion` \"", criterion, "\" is not in `x$criteria`. Available: ",
           paste(as.character(x$criteria$criterion), collapse = ", "), ".", call. = FALSE)
    }
    cut <- row$cutpoint[1]
    p_str <- if (row$p_value[1] < 0.001) "p < 0.001" else sprintf("p = %.3f", row$p_value[1])
    sub_text <- sprintf("%s cutpoint %s: HR (high vs low) = %.2f, %s", criterion,
                        formatC(cut, digits = 4, format = "g"), row$hr[1], p_str)
  }
  d <- x$data
  grp <- paste0(".", "cut_group")
  lab <- formatC(cut, digits = 4, format = "g")
  d[[grp]] <- factor(ifelse(d[[x$var_col]] > cut, "High", "Low"), levels = c("Low", "High"),
                     labels = c(paste0("Low (\u2264 ", lab, ")"), paste0("High (> ", lab, ")")))
  keep <- !is.na(d[[grp]]) & !is.na(d[[x$time_col]]) & !is.na(d[[x$status_col]])
  d <- d[keep, , drop = FALSE]
  names(d)[names(d) == grp] <- "cut_group"
  plot_survival_km(d, x$time_col, x$status_col, group_col = "cut_group", title = title %||% x$var_col,
                   base_size = base_size, risk_table = risk_table, time_breaks = time_breaks,
                   start_col = x$start_col) +
    ggplot2::labs(subtitle = sub_text)
}

#' Optimal Survival Cutpoint Search for Time-Varying Counting Process Data
#'
#' Evaluates candidate biomarker cutpoints on start-stop survival data
#' (`Surv(tstart, tstop, status)`), clustering by subject ID to avoid data leakage.
#'
#' @param data Long-format data frame with counting process layout.
#' @param tstart_col Character name of interval start time.
#' @param tstop_col Character name of interval stop time.
#' @param status_col Character name of event status column.
#' @param var_col Character name of continuous biomarker column.
#' @param id_col Character name of subject ID column.
#' @param min_prop Minimum proportion of total records in each subgroup (default 0.10).
#' @param n_cuts Number of candidate cutpoints (default 30).
#' @param cutpoints,alpha As in [km_cutpoint_search()]. The criteria use the robust (clustered) Wald
#'   test of the Cox model in place of the log-rank test, and there is no `maxstat` criterion.
#' @return A list with class `"cbe_km_cutpoint"` containing optimal cutpoint, candidate statistics and
#'   the `criteria` table described in [km_cutpoint_search()].
#' @export
km_cutpoint_time_varying <- function(data,
                                     tstart_col,
                                     tstop_col,
                                     status_col,
                                     var_col,
                                     id_col,
                                     min_prop = 0.10,
                                     n_cuts = 30,
                                     cutpoints = c("quantiles", "midpoints"),
                                     alpha = c(0.1, 0.05, 0.001)) {
  cutpoints <- match.arg(cutpoints)
  alpha <- .check_cutpoint_alpha(alpha)
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package \"survival\" is required for km_cutpoint_time_varying().", call. = FALSE)
  }
  req_cols <- c(tstart_col, tstop_col, status_col, var_col, id_col)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- data[!is.na(data[[tstart_col]]) & !is.na(data[[tstop_col]]) & !is.na(data[[status_col]]) & !is.na(data[[var_col]]), , drop = FALSE]
  n <- nrow(df)

  vals <- df[[var_col]]
  cuts <- .candidate_cutpoints(vals, cutpoints, min_prop, n_cuts)
  form <- stats::as.formula(
    sprintf("survival::Surv(%s, %s, %s) ~ group_bin + cluster(%s)",
            tstart_col, tstop_col, status_col, id_col)
  )

  eval_cut <- function(c_val, enforce = TRUE) {
    grp <- ifelse(vals > c_val, 1L, 0L)
    n_high <- sum(grp == 1L)
    n_low <- sum(grp == 0L)
    if (n_high == 0L || n_low == 0L) return(NULL)
    if (enforce && (n_high < min_prop * n || n_low < min_prop * n)) return(NULL)

    df_sub <- df
    df_sub$group_bin <- factor(grp, levels = c(0, 1))

    cfit <- tryCatch(survival::coxph(form, data = df_sub), error = function(e) NULL)
    if (is.null(cfit)) return(NULL)

    s <- summary(cfit)
    coef_info <- s$coefficients["group_bin1", , drop = FALSE]
    hr <- coef_info[, "exp(coef)"]
    p_val <- coef_info[, "Pr(>|z|)"]
    chisq <- (coef_info[, "z"])^2
    ci <- s$conf.int["group_bin1", , drop = FALSE]

    tibble::tibble(
      cutpoint = unname(c_val),
      n_low = n_low,
      n_high = n_high,
      chisq = unname(chisq),
      p_value = unname(p_val),
      hr = unname(hr),
      conf_low = unname(ci[, "lower .95"]),
      conf_high = unname(ci[, "upper .95"])
    )
  }

  results <- dplyr::bind_rows(lapply(cuts, eval_cut))
  if (nrow(results) == 0L) {
    stop("No valid candidate cutpoints found.", call. = FALSE)
  }

  best_idx <- which.max(results$chisq)
  opt_cut <- results$cutpoint[best_idx]

  sv <- survival::Surv(df[[tstart_col]], df[[tstop_col]], df[[status_col]])
  cox_coef <- .cutpoint_cox_coef(sv, vals)

  strata_name <- paste0(var_col, "_strata")
  data[[strata_name]] <- factor(
    ifelse(data[[var_col]] > opt_cut, "High", "Low"),
    levels = c("Low", "High")
  )

  out <- list(
    optimal_cutpoint = opt_cut,
    optimal_p = results$p_value[best_idx],
    optimal_hr = results$hr[best_idx],
    candidates = results,
    criteria = .cutpoint_criteria(results, vals, cox_coef, alpha, eval_cut),
    cox_coef = cox_coef,
    data = data,
    time_col = tstop_col,
    status_col = status_col,
    var_col = var_col,
    start_col = tstart_col
  )
  class(out) <- c("cbe_km_cutpoint", "list")
  out
}

# The Cox coefficient of the continuous variable, whose sign gives the direction in which the
# cutpoints are scanned; NA when the model cannot be fitted.
.cutpoint_cox_coef <- function(sv, vals) {
  tryCatch(
    unname(stats::coef(suppressWarnings(survival::coxph(sv ~ vals)))[1]),
    error = function(e) NA_real_
  )
}
