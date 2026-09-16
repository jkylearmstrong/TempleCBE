#' Standardized Presentation Deck and Summary Visualizations
#'
#' Presentation-ready plotting functions based on the Temple CBE executive summary
#' deck templates: Kaplan-Meier curves, longitudinal trajectories, group comparisons,
#' missing data quality audits, and contingency tables.
#'
#' @importFrom rlang .data
#' @keywords internal
#' @name plot_deck
NULL

#' Standardized Kaplan-Meier Survival Curve
#'
#' Renders a clean Kaplan-Meier curve using the Temple Cherry palette, with censor
#' marks, confidence ribbons, and percentage-formatted survival axis.
#'
#' @param data Data frame containing survival inputs.
#' @param time_col Character string naming the follow-up time column.
#' @param status_col Character string naming the event status column (1 = event, 0 = censored).
#' @param group_col Optional character string naming a stratification grouping column.
#' @param color Primary line/ribbon color (default: Temple Cherry \code{"#9D2235"}).
#' @param title Optional plot title.
#' @param caption Optional plot caption.
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @export
plot_survival_km <- function(data,
                             time_col,
                             status_col,
                             group_col = NULL,
                             color = "#9D2235",
                             title = "Kaplan-Meier Survival Estimate",
                             caption = NULL,
                             base_size = 13) {
  surv_fmla_str <- if (!is.null(group_col)) {
    sprintf("survival::Surv(%s, %s) ~ %s", time_col, status_col, group_col)
  } else {
    sprintf("survival::Surv(%s, %s) ~ 1", time_col, status_col)
  }
  surv_fmla <- stats::as.formula(surv_fmla_str)
  km_fit <- survival::survfit(surv_fmla, data = data)

  if (requireNamespace("ggsurvfit", quietly = TRUE)) {
    p <- ggsurvfit::survfit2(surv_fmla, data = data) |>
      ggsurvfit::ggsurvfit(color = color, linewidth = 1) +
      ggsurvfit::add_confidence_interval(fill = color, alpha = 0.15) +
      ggsurvfit::add_censor_mark(size = 2) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      theme_cbe_deck(base_size = base_size) +
      ggplot2::labs(
        title = title,
        x = "Minutes / Time Units",
        y = "Survival Probability",
        caption = caption
      )
    return(p)
  }

  # Fallback to standard broom + ggplot2 if ggsurvfit is not present
  td_km <- broom::tidy(km_fit)
  p <- ggplot2::ggplot(td_km, ggplot2::aes(x = time, y = estimate)) +
    ggplot2::geom_step(color = color, linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), fill = color, alpha = 0.15) +
    ggplot2::geom_point(data = td_km[td_km$n.censor > 0, ], shape = 3, size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme_cbe_deck(base_size = base_size) +
    ggplot2::labs(
      title = title,
      x = "Follow-up Time",
      y = "Survival Probability",
      caption = caption
    )
  p
}

#' Longitudinal Biomarker Trajectories by Cohort / Outcome
#'
#' Plots mean trajectory over time with standard error bars, stratified by outcome
#' or treatment cohort (e.g. Survived vs. Died).
#'
#' @param data Long-format data frame containing longitudinal measurements.
#' @param value_col Character name of measurement column.
#' @param time_col Character name of time point column.
#' @param group_col Character name of cohort/grouping column.
#' @param facet_col Optional character name of domain or biomarker column to facet wrap.
#' @param colors Named character vector of colors matching group levels.
#'   Defaults to \code{c(Survived = "#6F6F6F", Died = "#9D2235")}.
#' @param base_size Base font size (default: 12).
#' @return A ggplot2 object.
#' @export
plot_dynamic_trajectory <- function(data,
                                    value_col,
                                    time_col,
                                    group_col,
                                    facet_col = NULL,
                                    colors = c(Survived = "#6F6F6F", Died = "#9D2235"),
                                    base_size = 12) {
  grouping_vars <- c(time_col, group_col)
  if (!is.null(facet_col)) grouping_vars <- c(facet_col, grouping_vars)

  summary_df <- data |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(.data[[value_col]], na.rm = TRUE),
      se = stats::sd(.data[[value_col]], na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = .data[[time_col]],
      y = mean,
      color = .data[[group_col]],
      group = .data[[group_col]]
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 3) +
    theme_cbe_deck(base_size = base_size) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      x = "Protocol Time",
      y = "Mean Index Value (\u00b1 SE)",
      color = NULL
    )

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_col)), scales = "free_y")
  }

  p
}

#' Group Comparison Bar Chart with Standard Errors
#'
#' Faceted bar chart comparing biomarker means and standard errors across groups.
#'
#' @param data Data frame containing summary means and standard errors (or raw data).
#' @param value_col Column containing index values or means.
#' @param group_col Cohort/outcome grouping column (e.g. "outcome_label").
#' @param facet_col Column to facet wrap across (e.g. "index_label").
#' @param se_col Optional standard error column. If NULL, calculated automatically.
#' @param colors Named color vector (default: \code{c(Survived = "#6F6F6F", Died = "#9D2235")}).
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @export
plot_group_comparison <- function(data,
                                  value_col,
                                  group_col,
                                  facet_col = NULL,
                                  se_col = NULL,
                                  colors = c(Survived = "#6F6F6F", Died = "#9D2235"),
                                  base_size = 13) {
  df <- data
  if (is.null(se_col) || !se_col %in% names(df)) {
    group_vars <- group_col
    if (!is.null(facet_col)) group_vars <- c(facet_col, group_vars)

    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(.data[[value_col]], na.rm = TRUE),
        se = stats::sd(.data[[value_col]], na.rm = TRUE) / sqrt(n),
        .groups = "drop"
      )
    val_var <- "mean"
    err_var <- "se"
  } else {
    val_var <- value_col
    err_var <- se_col
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[group_col]], y = .data[[val_var]], fill = .data[[group_col]])) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[val_var]] - .data[[err_var]], ymax = .data[[val_var]] + .data[[err_var]]), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::guides(fill = "none") +
    theme_cbe_deck(base_size = base_size) +
    ggplot2::labs(
      x = NULL,
      y = "Mean Index Value (\u00b1 SE)"
    )

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_col)), scales = "free_y")
  }

  p
}

#' Missing Data Audit Plot
#'
#' Generates dodged percentage bar charts comparing data completeness across modalities.
#'
#' @param data Data frame with columns: \code{parameter}, \code{pct} (fraction missing), \code{method}.
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @export
plot_missingness <- function(data,
                             base_size = 13) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = parameter, y = pct, fill = method)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = fmt_pct(pct)),
      position = ggplot2::position_dodge(width = 0.75),
      vjust = -0.4,
      size = 4
    ) +
    ggplot2::scale_fill_manual(values = c("OSM3/Spin" = "#6F6F6F", "ABL" = "#9D2235")) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = scales::percent) +
    theme_cbe_deck(base_size = base_size) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::labs(
      x = NULL,
      y = "Percent Missing",
      fill = NULL
    )
  p
}

#' Formatted 2x2 Contingency Table with Fisher's Exact Test
#'
#' Builds an institutional 2x2 contingency table with row counts, row percentages,
#' marginal totals, and Fisher's exact test p-value.
#'
#' @param data Data frame containing the categorical variables.
#' @param row_var Character name of the row variable.
#' @param col_var Character name of the column variable.
#' @param row_label Optional display label for the row variable.
#' @param col_label Optional display label for the column variable.
#' @return A list containing \code{table} (tibble) and \code{note} (character).
#' @export
table_two_by_two <- function(data, row_var, col_var, row_label = row_var, col_label = col_var) {
  d <- data |> dplyr::filter(!is.na(.data[[row_var]]), !is.na(.data[[col_var]]))
  tab <- table(d[[row_var]], d[[col_var]])
  n <- sum(tab)
  n_excluded <- nrow(data) - n

  cells <- matrix(sprintf("%d (%s)", tab, fmt_pct(prop.table(tab, 1), 0)), nrow = nrow(tab))
  body <- tibble::tibble(
    !!row_label := rownames(tab),
    !!!stats::setNames(as.list(as.data.frame(cells)), colnames(tab)),
    Total = as.character(rowSums(tab))
  )
  total_row <- tibble::tibble(
    !!row_label := "Total",
    !!!stats::setNames(as.list(sprintf("%d (%s)", colSums(tab), fmt_pct(colSums(tab) / n, 0))), colnames(tab)),
    Total = as.character(n)
  )

  ft <- stats::fisher.test(tab)

  list(
    table = dplyr::bind_rows(body, total_row),
    p_value = ft$p.value,
    note = sprintf(
      "n = %d%s. Cells are n (row %%). Fisher's exact test, %s (unadjusted).",
      n,
      if (n_excluded > 0) sprintf(" (%d excluded for missingness)", n_excluded) else "",
      fmt_p(ft$p.value)
    )
  )
}

utils::globalVariables(c(
  "se", "parameter", "pct", "method", "time", "estimate", "conf.low", "conf.high"
))

