#' Tuning Diagnostics and Visualization Family
#'
#' A comprehensive family of diagnostic plots, summary tables, and mixed-effects
#' models for evaluating hyperparameter tuning results from [tune_alpha_by_split()]
#' and [tune_over_alpha()].
#'
#' @name tune_diagnostics
NULL

#' Fit a Mixed-Effects Model for Integrated Brier Score Against Alpha
#'
#' Fits a linear mixed-effects model (or fixed-effects regression fallback) of
#' Integrated Brier Score (IBS) or `log10(IBS)` against `alpha`, accounting for
#' grouping across resampling splits (`inner_resamples_splits`). Returns the fitted
#' model, tidy coefficients, a formatted equation string, and model predictions
#' along the alpha spectrum.
#'
#' This dynamic fitting eliminates hard-coded regression coefficients in reports.
#'
#' @param data A data frame containing tuning results from [tune_alpha_by_split()].
#'   Must contain columns `alpha`, `IBS`, and `inner_resamples_splits`.
#' @param log_ibs Logical. When `TRUE` (default), models `log10(IBS)`. When `FALSE`,
#'   models raw `IBS`.
#' @return A list with class `"cbe_tune_mixed_model"` containing:
#'   \item{model}{The fitted `merMod` or `lm` model object}
#'   \item{equation}{A character string with the fitted equation (e.g. `"log10(IBS) = -0.8712 + 0.0450 * alpha"`)}
#'   \item{coefficients}{A tibble with `term`, `estimate`, and standard error}
#'   \item{fitted}{A tibble of predicted values across the alpha continuum}
#'   \item{data}{The deduplicated per-split fit data used for modeling}
#' @export
fit_ibs_mixed_model <- function(data, log_ibs = TRUE) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  req_cols <- c("alpha", "IBS")
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  group_col <- if ("inner_resamples_splits" %in% names(data)) "inner_resamples_splits" else if ("split" %in% names(data)) "split" else NULL

  # Deduplicate to unique model fits (since data contains one row per coefficient term)
  keep_cols <- intersect(c(group_col, "alpha", "IBS", "lambda", "formula"), names(data))
  df <- unique(data[, keep_cols, drop = FALSE])
  df <- df[!is.na(df$IBS) & !is.na(df$alpha), , drop = FALSE]

  if (isTRUE(log_ibs)) {
    df$y <- log10(df$IBS)
    y_name <- "log10(IBS)"
  } else {
    df$y <- df$IBS
    y_name <- "IBS"
  }

  fit <- NULL
  has_lme4 <- requireNamespace("lme4", quietly = TRUE)
  has_groups <- !is.null(group_col) && length(unique(df[[group_col]])) > 1L

  if (has_lme4 && has_groups) {
    fit <- tryCatch({
      form <- stats::as.formula(paste0("y ~ alpha + (1 | ", group_col, ")"))
      lme4::lmer(form, data = df)
    }, error = function(e) NULL)
  }

  if (is.null(fit)) {
    fit <- stats::lm(y ~ alpha, data = df)
  }

  coefs <- if (inherits(fit, "merMod")) {
    lme4::fixef(fit)
  } else {
    stats::coef(fit)
  }

  intercept <- unname(coefs[1])
  slope <- unname(coefs[2])
  sign_str <- if (slope >= 0) "+" else "-"
  equation <- sprintf("%s = %.4f %s %.4f * alpha", y_name, intercept, sign_str, abs(slope))

  coef_tbl <- if (inherits(fit, "merMod")) {
    se <- sqrt(diag(as.matrix(stats::vcov(fit))))
    tibble::tibble(
      term = names(coefs),
      estimate = unname(coefs),
      std_error = unname(se)
    )
  } else {
    s <- summary(fit)
    tibble::tibble(
      term = rownames(s$coefficients),
      estimate = s$coefficients[, "Estimate"],
      std_error = s$coefficients[, "Std. Error"],
      p_value = s$coefficients[, "Pr(>|t|)"]
    )
  }

  alpha_seq <- seq(min(df$alpha, na.rm = TRUE), max(df$alpha, na.rm = TRUE), length.out = 100)
  pred_df <- data.frame(alpha = alpha_seq)
  if (!is.null(group_col)) pred_df[[group_col]] <- df[[group_col]][1]

  pred_df$predicted <- if (inherits(fit, "merMod")) {
    stats::predict(fit, newdata = pred_df, re.form = NA)
  } else {
    stats::predict(fit, newdata = pred_df)
  }

  out <- list(
    model = fit,
    equation = equation,
    coefficients = coef_tbl,
    fitted = tibble::as_tibble(pred_df[, c("alpha", "predicted")]),
    data = tibble::as_tibble(df)
  )
  class(out) <- c("cbe_tune_mixed_model", "list")
  out
}

#' Plot Integrated Brier Score Against Alpha With Dynamic Model Fit
#'
#' Visualizes the relationship between elastic net mixing parameter `alpha` and
#' Integrated Brier Score across resampling folds, overlaying the fitted line
#' from [fit_ibs_mixed_model()] and its dynamic equation.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param log_ibs Logical. When `TRUE` (default), plots `log10(IBS)`.
#' @param show_points Logical. Plot individual resample points. Default `TRUE`.
#' @param show_line Logical. Overlay the fitted regression line. Default `TRUE`.
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @seealso [fit_ibs_mixed_model()], [plot_tune_lambda_ibs()], [plot_tune_coef_boxplot()]
#' @export
plot_tune_alpha_ibs <- function(data,
                                log_ibs = TRUE,
                                show_points = TRUE,
                                show_line = TRUE,
                                label_map = NULL,
                                style = NULL) {
  mm <- fit_ibs_mixed_model(data, log_ibs = log_ibs)
  y_col <- if (isTRUE(log_ibs)) "log10(IBS)" else "IBS"

  p <- ggplot2::ggplot()

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point(
      data = mm$data,
      ggplot2::aes(x = .data$alpha, y = .data$y),
      color = "#6F6F6F", alpha = 0.6, size = 2
    )
  }

  if (isTRUE(show_line)) {
    p <- p + ggplot2::geom_line(
      data = mm$fitted,
      ggplot2::aes(x = .data$alpha, y = .data$predicted),
      color = "#9D2235", linewidth = 1.1
    )
  }

  p <- p +
    theme_cbe() +
    ggplot2::labs(
      title = "Tuning Performance Across Elastic Net Mixing (Alpha)",
      subtitle = mm$equation,
      x = expression(paste("Elastic Net Mixing Parameter (", alpha, ")")),
      y = y_col
    )

  p
}

#' Plot Penalty Lambda Versus Integrated Brier Score
#'
#' Visualizes `log10(lambda)` against Integrated Brier Score across resamples,
#' colored by `alpha`.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @export
plot_tune_lambda_ibs <- function(data, label_map = NULL, style = NULL) {
  if (!all(c("lambda", "IBS", "alpha") %in% names(data))) {
    stop("`data` must contain 'lambda', 'IBS', and 'alpha' columns.", call. = FALSE)
  }
  keep_cols <- intersect(c("inner_resamples_splits", "alpha", "IBS", "lambda"), names(data))
  df <- unique(data[, keep_cols, drop = FALSE])
  df <- df[!is.na(df$lambda) & !is.na(df$IBS) & !is.na(df$alpha), , drop = FALSE]

  df$alpha_factor <- factor(round(df$alpha, 2))

  ggplot2::ggplot(df, ggplot2::aes(x = log10(.data$lambda), y = .data$IBS, color = .data$alpha_factor)) +
    ggplot2::geom_point(alpha = 0.7, size = 2.5) +
    theme_cbe() +
    ggplot2::labs(
      title = "Penalty Parameter (Lambda) vs Integrated Brier Score",
      x = expression(log[10](lambda)),
      y = "Integrated Brier Score (IBS)",
      color = expression(alpha)
    )
}

#' Plot Integrated Brier Score Distribution Across Resampling Splits
#'
#' Displays the distribution of IBS across cross-validation or bootstrap splits.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @export
plot_tune_ibs_by_resample <- function(data, label_map = NULL, style = NULL) {
  split_col <- if ("inner_resamples_splits" %in% names(data)) "inner_resamples_splits" else if ("split" %in% names(data)) "split" else stop("`data` must contain a split identifier column.", call. = FALSE)
  keep_cols <- c(split_col, "alpha", "IBS")
  df <- unique(data[, keep_cols, drop = FALSE])
  df <- df[!is.na(df$IBS), , drop = FALSE]
  df[[split_col]] <- resolve_cbe_labels(df[[split_col]], label_map = label_map, style = style)

  ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[split_col]]), y = .data$IBS)) +
    ggplot2::geom_boxplot(fill = "#D9D9D9", color = "#2B2B2B", outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.5, color = "#9D2235") +
    theme_cbe() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "IBS Variation Across Resampling Folds",
      x = "Resampling Split",
      y = "Integrated Brier Score (IBS)"
    )
}

#' Plot Coefficient Estimates Distribution Across Tuning Resamples
#'
#' Generates horizontal boxplots showing the stability and shrinkage of coefficient
#' estimates for each feature across resamples.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param top_n Optional integer limit on the number of features to plot (ordered
#'   by median absolute effect size).
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @export
plot_tune_coef_boxplot <- function(data, top_n = NULL, label_map = NULL, style = NULL) {
  if (!all(c("term", "estimate") %in% names(data))) {
    stop("`data` must contain 'term' and 'estimate' columns.", call. = FALSE)
  }
  df <- data[data$term != "(Intercept)" & !is.na(data$estimate), , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("No feature coefficients found in `data`.", call. = FALSE)
  }

  term_order <- dplyr::group_by(df, .data$term)
  term_order <- dplyr::summarize(term_order, median_abs = stats::median(abs(.data$estimate), na.rm = TRUE), .groups = "drop")
  term_order <- dplyr::arrange(term_order, dplyr::desc(.data$median_abs))

  if (!is.null(top_n) && top_n < nrow(term_order)) {
    selected_terms <- utils::head(term_order$term, top_n)
    df <- df[df$term %in% selected_terms, , drop = FALSE]
    term_order <- term_order[term_order$term %in% selected_terms, , drop = FALSE]
  }

  mapped_levels <- make.unique(resolve_cbe_labels(term_order$term, label_map = label_map, style = style), sep = " ")
  lookup <- stats::setNames(mapped_levels, term_order$term)
  df$term <- factor(unname(lookup[df$term]), levels = rev(mapped_levels))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$term)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_boxplot(fill = "#D9D9D9", color = "#9D2235", outlier.alpha = 0.4) +
    theme_cbe() +
    ggplot2::labs(
      title = "Feature Coefficient Estimates Across Resampling Folds",
      x = "Penalized Log Hazard Ratio Estimate",
      y = "Feature Term"
    )
}

#' Summary Table of Tuning Performance Across Alpha Values
#'
#' Calculates summary statistics (mean, SD, median, IQR, min, max) of the
#' Integrated Brier Score for each evaluated `alpha` value.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A tibble of per-alpha IBS statistics.
#' @export
tbl_tune_alpha <- function(data, label_map = NULL, style = NULL) {
  if (!all(c("alpha", "IBS") %in% names(data))) {
    stop("`data` must contain 'alpha' and 'IBS' columns.", call. = FALSE)
  }
  keep_cols <- intersect(c("inner_resamples_splits", "alpha", "IBS", "lambda"), names(data))
  df <- unique(data[, keep_cols, drop = FALSE])
  df <- df[!is.na(df$alpha) & !is.na(df$IBS), , drop = FALSE]

  res <- dplyr::group_by(df, .data$alpha)
  dplyr::summarize(
    res,
    n_fits = dplyr::n(),
    mean_ibs = mean(.data$IBS),
    sd_ibs = stats::sd(.data$IBS),
    median_ibs = stats::median(.data$IBS),
    iqr_ibs = stats::IQR(.data$IBS),
    min_ibs = min(.data$IBS),
    max_ibs = max(.data$IBS),
    .groups = "drop"
  )
}

#' Summary Table of Coefficient Stability Across Tuning Resamples
#'
#' Calculates selection frequency and distribution of coefficient estimates for
#' each feature across resamples.
#'
#' @param data A data frame from [tune_alpha_by_split()].
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A tibble of feature coefficient stability statistics.
#' @export
tbl_tune_coef <- function(data, label_map = NULL, style = NULL) {
  if (!all(c("term", "estimate") %in% names(data))) {
    stop("`data` must contain 'term' and 'estimate' columns.", call. = FALSE)
  }
  df <- data[data$term != "(Intercept)" & !is.na(data$estimate), , drop = FALSE]
  total_splits <- if ("inner_resamples_splits" %in% names(data)) length(unique(data$inner_resamples_splits)) else length(unique(df$inner_resamples_splits))

  res <- dplyr::group_by(df, .data$term)
  out <- dplyr::summarize(
    res,
    n_selected = sum(.data$estimate != 0),
    prop_selected = sum(.data$estimate != 0) / max(1, total_splits),
    mean_estimate = mean(.data$estimate),
    sd_estimate = stats::sd(.data$estimate),
    median_estimate = stats::median(.data$estimate),
    q25 = stats::quantile(.data$estimate, 0.25),
    q75 = stats::quantile(.data$estimate, 0.75),
    .groups = "drop"
  )
  out$term <- resolve_cbe_labels(out$term, label_map = label_map, style = style)
  out
}

