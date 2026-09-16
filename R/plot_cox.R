#' Cox Proportional Hazards Diagnostic and Reporting Visualizations
#'
#' Functions for creating hazard ratio forest plots, model-predicted survival curves,
#' and continuous predictor vs. event probability diagnostic plots.
#'
#' @importFrom rlang %||% .data
#' @keywords internal
#' @name plot_cox
NULL

#' Forest Plot of Hazard Ratios with Confidence Intervals
#'
#' Generates a publication/deck-ready horizontal forest plot of hazard ratios.
#'
#' @param data Data frame containing tidy Cox regression results (e.g. from \code{broom::tidy} or multi-model runs).
#' @param label_col Column name for labels / terms (character string or unquoted).
#' @param hr_col Column name for hazard ratios (default: "estimate").
#' @param low_col Column name for lower CI bounds (default: "conf.low").
#' @param high_col Column name for upper CI bounds (default: "conf.high").
#' @param p_col Optional column name for p-values to display significance shapes.
#' @param color Primary color for points and error bars (default: Temple Cherry \code{"#9D2235"}).
#' @param x_limits Optional 2-element numeric vector for x-axis limits.
#' @param x_breaks Optional numeric vector of axis break points.
#' @param title Optional plot title.
#' @param caption Optional plot caption.
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @export
plot_cox_forest <- function(data,
                            label_col = "index_label",
                            hr_col = "estimate",
                            low_col = "conf.low",
                            high_col = "conf.high",
                            p_col = NULL,
                            color = "#9D2235",
                            x_limits = NULL,
                            x_breaks = NULL,
                            title = NULL,
                            caption = NULL,
                            base_size = 13) {
  df <- as.data.frame(data)
  y_var <- df[[label_col]]
  x_hr  <- df[[hr_col]]
  x_low <- df[[low_col]]
  x_hi  <- df[[high_col]]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x_hr, y = stats::reorder(y_var, seq_along(y_var)))) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = x_low, xmax = x_hi),
      width = 0.25,
      orientation = "y",
      color = color,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 3.5, color = color) +
    theme_cbe(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5)) +
    ggplot2::labs(
      title = title,
      x = "Hazard Ratio (95% CI)",
      y = NULL,
      caption = caption
    )

  if (!is.null(x_limits)) {
    p <- p + ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks %||% scales::breaks_pretty(n = 5))
  }

  p
}

#' Model-Predicted Survival Curves Stratified by Predictor
#'
#' Generates stratified predicted survival curves from a Cox model. For continuous predictors,
#' values are automatically binned into quantile strata (e.g. quartiles).
#'
#' @param fit A fitted \code{survival::coxph} model or a \code{cbe_cox} object.
#' @param data Data frame used for fitting the model.
#' @param feature Character name of the predictor column.
#' @param id_col Character name of subject ID column (default: "arl_number" or row number).
#' @param n_tiles Number of quantile bins for continuous predictors (default: 4).
#' @param label_endpoints Logical; if TRUE, repels labels for stratum values at the final time point.
#' @param base_size Base font size (default: 12).
#' @return A ggplot2 object.
#' @export
plot_cox_survival <- function(fit,
                              data,
                              feature = NULL,
                              id_col = NULL,
                              n_tiles = 4,
                              label_endpoints = TRUE,
                              base_size = 12) {
  if (inherits(fit, "cbe_cox")) {
    feature <- fit$feature
    fit <- fit$model
  }

  if (is.null(feature)) {
    stop("Must specify 'feature' name if 'fit' is a standard coxph object.")
  }

  df <- data
  var_vals <- df[[feature]]
  var_lbl <- if (requireNamespace("labelled", quietly = TRUE)) {
    labelled::var_label(var_vals) %||% feature
  } else {
    feature
  }

  is_num <- is.numeric(var_vals)

  # Prepare subject ID
  if (!is.null(id_col) && id_col %in% names(df)) {
    df$subject_id <- as.character(df[[id_col]])
  } else {
    df$subject_id <- as.character(seq_len(nrow(df)))
  }

  # Build stratum column
  if (is_num) {
    df_binned <- df |>
      dplyr::mutate(tile = dplyr::ntile(.data[[feature]], n_tiles)) |>
      dplyr::group_by(tile) |>
      dplyr::mutate(
        min_v = min(.data[[feature]], na.rm = TRUE),
        max_v = max(.data[[feature]], na.rm = TRUE),
        strata = sprintf("[%s \u2013 %s]", fmt_num(min_v, 1), fmt_num(max_v, 1))
      ) |>
      dplyr::ungroup()
  } else {
    df_binned <- df |>
      dplyr::mutate(strata = as.factor(.data[[feature]]))
  }

  # Predict curves from coxph
  sf <- survival::survfit(fit, newdata = df)
  td_sf <- broom::tidy(sf)

  # Check if tidy format has columns by index
  if (any(grepl("^estimate\\.", names(td_sf)))) {
    est_df <- td_sf |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("estimate."),
        names_to = "subject_id",
        names_prefix = "estimate.",
        values_to = "estimate"
      ) |>
      dplyr::select(time, subject_id, estimate)
  } else {
    est_df <- td_sf |>
      dplyr::mutate(subject_id = "1") |>
      dplyr::select(time, subject_id, estimate)
  }

  plot_data <- est_df |>
    dplyr::inner_join(df_binned, by = "subject_id")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = estimate, color = strata, group = subject_id)) +
    ggplot2::geom_line(alpha = 0.7, linewidth = 0.8) +
    ggplot2::geom_point(size = 1.2, alpha = 0.7) +
    theme_cbe(base_size = base_size) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::scale_color_manual(values = unname(cbe_palette)) +
    ggplot2::labs(
      title = sprintf("Cox Proportional Hazards Model: %s", var_lbl),
      subtitle = "Estimated Survival Probability Curves",
      x = "Follow-up Time",
      y = "Survival Probability",
      color = if (is_num) sprintf("%s (Quantiles)", var_lbl) else var_lbl
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = min(n_tiles, 4)))

  p
}

#' Marginal Event Probability Diagnostic Plot
#'
#' Plots predicted probability of the event vs. a continuous predictor, overlaid
#' with actual binary event observations and smoothed confidence intervals.
#'
#' @param fit A fitted \code{survival::coxph} model or a \code{cbe_cox} object.
#' @param data Data frame containing survival inputs.
#' @param feature Character name of continuous predictor column.
#' @param status_col Character name of status/event column (0 = censored, 1 = event; default: "status").
#' @param base_size Base font size (default: 12).
#' @return A ggplot2 object.
#' @export
plot_cox_marginal <- function(fit,
                              data,
                              feature = NULL,
                              status_col = "status",
                              base_size = 12) {
  if (inherits(fit, "cbe_cox")) {
    feature <- fit$feature
    fit <- fit$model
  }

  if (is.null(feature)) {
    stop("Must specify 'feature' name if 'fit' is a standard coxph object.")
  }

  var_vals <- data[[feature]]
  var_lbl <- if (requireNamespace("labelled", quietly = TRUE)) {
    labelled::var_label(var_vals) %||% feature
  } else {
    feature
  }

  # Augment survival prediction
  aug <- broom::augment(fit, data = data, type.predict = "survival")
  if (!".fitted" %in% names(aug)) {
    stop("Failed to extract .fitted survival probabilities from model augment.")
  }

  se_fit <- if (".se.fit" %in% names(aug)) aug[[".se.fit"]] else rep(0, nrow(aug))

  aug$lower <- pmax(0, aug[[".fitted"]] - 1.96 * se_fit)
  aug$upper <- pmin(1, aug[[".fitted"]] + 1.96 * se_fit)
  aug$prob_event <- 1 - aug[[".fitted"]]
  aug$prob_event_lower <- pmax(0, 1 - aug$upper)
  aug$prob_event_upper <- pmin(1, 1 - aug$lower)

  status_vals <- if (status_col %in% names(aug)) aug[[status_col]] else rep(0, nrow(aug))

  p <- ggplot2::ggplot(aug, ggplot2::aes(x = .data[[feature]], y = prob_event)) +
    ggplot2::geom_smooth(color = "#9D2235", se = FALSE, linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = status_vals), shape = 1, size = 2, color = "grey30", alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(y = prob_event_lower), color = "#6F6F6F", linetype = "dashed", se = FALSE, linewidth = 0.7) +
    ggplot2::geom_smooth(ggplot2::aes(y = prob_event_upper), color = "#6F6F6F", linetype = "dashed", se = FALSE, linewidth = 0.7) +
    theme_cbe(base_size = base_size) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      title = sprintf("Predicted Probability of Event vs. %s", var_lbl),
      subtitle = "Fitted Risk Curve (solid cherry) with 95% CI bands (dashed grey) and observed status (circles)",
      x = var_lbl,
      y = "Predicted Event Probability"
    )

  p
}

utils::globalVariables(c(
  "tile", "min_v", "max_v", "time", "subject_id",
  "estimate", "strata", "prob_event", "prob_event_lower", "prob_event_upper"
))

