#' Kaplan-Meier Optimal Cutpoint Search and Visualization
#'
#' Evaluates candidate thresholds for continuous biomarkers or risk scores to find
#' the optimal cutpoint maximizing log-rank separation or hazard ratio divergence,
#' with support for standard right-censored and time-varying counting-process data.
#'
#' @name km_cutpoints
NULL

#' Search for Optimal Survival Cutpoint via Log-Rank Separation
#'
#' Evaluates candidate thresholds of a continuous variable to stratify patients into
#' low-risk and high-risk groups. Evaluates log-rank test statistics and univariable
#' Cox hazard ratios across candidate percentiles to identify the optimal stratification.
#'
#' Replaces inline cutpoint selection scripts across clinical survival reports.
#'
#' @param data Data frame containing survival follow-up and biomarker columns.
#' @param time_col Character string naming the follow-up time column.
#' @param status_col Character string naming the event status column (1 = event, 0 = censored).
#' @param var_col Character string naming the continuous biomarker or index variable.
#' @param min_prop Minimum proportion of subjects required in each stratified subgroup (default 0.10).
#' @param n_cuts Number of quantile evaluation points between `min_prop` and `1 - min_prop` (default 50).
#' @return A list with class `"cbe_km_cutpoint"` containing:
#'   \item{optimal_cutpoint}{Numeric value of the optimal cutpoint threshold}
#'   \item{optimal_p}{P-value at the optimal cutpoint}
#'   \item{optimal_hr}{Hazard ratio of high vs low group at the optimal cutpoint}
#'   \item{candidates}{A tibble of evaluated candidate cutpoints and statistics}
#'   \item{data}{The input data frame augmented with a `{var_col}_strata` binary factor column}
#'   \item{time_col}{Name of time column}
#'   \item{status_col}{Name of status column}
#'   \item{var_col}{Name of biomarker column}
#' @seealso [plot_km_cutpoints()], [km_cutpoint_time_varying()]
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
#' }
km_cutpoint_search <- function(data,
                               time_col,
                               status_col,
                               var_col,
                               min_prop = 0.10,
                               n_cuts = 50) {
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
  probs <- seq(min_prop, 1 - min_prop, length.out = n_cuts)
  cuts <- unique(stats::quantile(vals, probs = probs, na.rm = TRUE))

  results <- lapply(cuts, function(c_val) {
    grp <- ifelse(vals > c_val, 1L, 0L)
    n_high <- sum(grp == 1L)
    n_low <- sum(grp == 0L)
    if (n_high < min_prop * n || n_low < min_prop * n) return(NULL)

    df_sub <- df
    df_sub$group_bin <- factor(grp, levels = c(0, 1))

    # Log-rank test
    form_diff <- stats::as.formula(sprintf("survival::Surv(%s, %s) ~ group_bin", time_col, status_col))
    sdiff <- tryCatch(survival::survdiff(form_diff, data = df_sub), error = function(e) NULL)
    if (is.null(sdiff)) return(NULL)

    chisq <- sdiff$chisq
    p_val <- 1 - stats::pchisq(chisq, df = 1)

    # Cox model
    cfit <- tryCatch(survival::coxph(form_diff, data = df_sub), error = function(e) NULL)
    hr <- if (!is.null(cfit)) exp(stats::coef(cfit)[1]) else NA_real_
    ci <- if (!is.null(cfit)) exp(stats::confint(cfit)[1, ]) else c(NA_real_, NA_real_)

    tibble::tibble(
      cutpoint = c_val,
      n_low = n_low,
      n_high = n_high,
      chisq = chisq,
      p_value = p_val,
      hr = unname(hr),
      conf_low = unname(ci[1]),
      conf_high = unname(ci[2])
    )
  })

  results <- dplyr::bind_rows(results)
  if (nrow(results) == 0L) {
    stop("No valid candidate cutpoints found satisfying `min_prop` constraints.", call. = FALSE)
  }

  best_idx <- which.max(results$chisq)
  opt_cut <- results$cutpoint[best_idx]

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
#' Displays the profile of log-rank statistics ($-\\log_{10}(p)$) across evaluated
#' candidate cutpoints, highlighting the optimal threshold.
#'
#' @param x An object of class `"cbe_km_cutpoint"` from [km_cutpoint_search()].
#' @param ... Additional arguments (unused).
#' @return A `ggplot2` object.
#' @seealso [km_cutpoint_search()]
#' @export
plot_km_cutpoints <- function(x, ...) {
  if (!inherits(x, "cbe_km_cutpoint")) {
    stop("`x` must be a 'cbe_km_cutpoint' object from km_cutpoint_search().", call. = FALSE)
  }

  df <- x$candidates
  df$neg_log10_p <- -log10(pmax(df$p_value, 1e-16))

  opt_x <- x$optimal_cutpoint
  opt_y <- -log10(max(x$optimal_p, 1e-16))

  p_str <- if (x$optimal_p < 0.001) "p < 0.001" else sprintf("p = %.4f", x$optimal_p)
  hr_str <- sprintf("HR = %.2f", x$optimal_hr)
  sub_text <- sprintf("Optimal Cutpoint: %.3f (%s, %s)", opt_x, hr_str, p_str)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p)) +
    ggplot2::geom_line(color = "#6F6F6F", linewidth = 1) +
    ggplot2::geom_point(color = "#2B2B2B", size = 2) +
    ggplot2::geom_vline(xintercept = opt_x, linetype = "dashed", color = "#9D2235", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(cutpoint = opt_x, neg_log10_p = opt_y),
                        ggplot2::aes(x = .data$cutpoint, y = .data$neg_log10_p),
                        color = "#9D2235", size = 4) +
    theme_cbe() +
    ggplot2::labs(
      title = paste0("Optimal Cutpoint Profile: ", x$var_col),
      subtitle = sub_text,
      x = paste0("Candidate Cutpoint (", x$var_col, ")"),
      y = expression(-log[10](italic(p)-value))
    )
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
#' @return A list with class `"cbe_km_cutpoint"` containing optimal cutpoint and candidate statistics.
#' @export
km_cutpoint_time_varying <- function(data,
                                     tstart_col,
                                     tstop_col,
                                     status_col,
                                     var_col,
                                     id_col,
                                     min_prop = 0.10,
                                     n_cuts = 30) {
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
  probs <- seq(min_prop, 1 - min_prop, length.out = n_cuts)
  cuts <- unique(stats::quantile(vals, probs = probs, na.rm = TRUE))

  results <- lapply(cuts, function(c_val) {
    grp <- ifelse(vals > c_val, 1L, 0L)
    n_high <- sum(grp == 1L)
    n_low <- sum(grp == 0L)
    if (n_high < min_prop * n || n_low < min_prop * n) return(NULL)

    df_sub <- df
    df_sub$group_bin <- factor(grp, levels = c(0, 1))

    form <- stats::as.formula(
      sprintf("survival::Surv(%s, %s, %s) ~ group_bin + cluster(%s)",
              tstart_col, tstop_col, status_col, id_col)
    )

    cfit <- tryCatch(survival::coxph(form, data = df_sub), error = function(e) NULL)
    if (is.null(cfit)) return(NULL)

    s <- summary(cfit)
    coef_info <- s$coefficients["group_bin1", , drop = FALSE]
    hr <- coef_info[, "exp(coef)"]
    p_val <- coef_info[, "Pr(>|z|)"]
    chisq <- (coef_info[, "z"])^2
    ci <- s$conf.int["group_bin1", , drop = FALSE]

    tibble::tibble(
      cutpoint = c_val,
      n_low = n_low,
      n_high = n_high,
      chisq = chisq,
      p_value = p_val,
      hr = unname(hr),
      conf_low = unname(ci[, "lower .95"]),
      conf_high = unname(ci[, "upper .95"])
    )
  })

  results <- dplyr::bind_rows(results)
  if (nrow(results) == 0L) {
    stop("No valid candidate cutpoints found.", call. = FALSE)
  }

  best_idx <- which.max(results$chisq)
  opt_cut <- results$cutpoint[best_idx]

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
    data = data,
    time_col = tstop_col,
    status_col = status_col,
    var_col = var_col
  )
  class(out) <- c("cbe_km_cutpoint", "list")
  out
}
