#' Visualizations for 2-Way Contingency Tables and Categorical Tests
#'
#' Generates publication-ready clinical visualizations for 2-way contingency tables
#' with hypothesis test statistics (Central Fisher's Exact Test with mid-p adjustment when
#' zero-cells occur, or Pearson Chi-squared test) automatically formatted in the caption.
#'
#' Supported visualization types:
#' \itemize{
#'   \item \code{"balloon"} (default): Proportional bubble/circle plot with counts displayed inside circles.
#'   \item \code{"bar"}: Grouped, stacked, or 100\% proportional fill bar chart.
#'   \item \code{"mosaic"}: Two-dimensional mosaic plot with tile area proportional to joint frequency.
#'   \item \code{"heatmap"}: Shaded tile heatmap with cell frequencies and overall percentages.
#'   \item \code{"square"}: Standard 4-quadrant report for 2x2 tables (q1 | q2 // q3 | q4 // p = pformat),
#'     or proportional square tiles for RxC tables.
#'   \item \code{"corrplot"}: Circular or square matrix plot rendered via \pkg{corrplot}.
#' }
#'
#' @param data A data frame containing the categorical variables, or a 2-way table/matrix.
#' @param var1 Character string of the row variable column name (ignored if \code{data} is a table).
#' @param var2 Character string of the column variable column name (ignored if \code{data} is a table).
#' @param label1 Optional display label for \code{var1} (defaults to \code{var1}).
#' @param label2 Optional display label for \code{var2} (defaults to \code{var2}).
#' @param type Visualization type: \code{"balloon"} (default), \code{"bar"}, \code{"mosaic"},
#'   \code{"heatmap"}, \code{"square"}, or \code{"corrplot"}.
#' @param bar_position Position adjustment when \code{type = "bar"}: \code{"fill"}
#'   (100\% proportional stacked bar, default), \code{"dodge"} (side-by-side grouped),
#'   or \code{"stack"} (count stacked).
#' @param engine Engine for balloon/square rendering: \code{"ggplot"} (default), \code{"corrplot"},
#'   or \code{"ggpubr"}.
#' @param size_range Numeric vector of length 2 giving min and max glyph sizes for balloon and square plots
#'   (default \code{c(6, 22)}).
#' @param fill_colors Character vector of colors for the palette or gradient. Defaults to
#'   Temple University brand palette (\code{c("#F4F4F4", "#9D2235")}).
#' @param show_counts Logical (default \code{TRUE}); whether to print cell counts inside plots.
#' @param title Plot title. If \code{NULL}, defaults to \code{"{label1} \u00d7 {label2}"}.
#' @param caption Optional plot caption. If \code{NULL}, automatically computed and formatted
#'   using \code{\link{pformat}} with test description.
#' @param p_format Optional p-value formatting function (default \code{\link{pformat}}).
#' @param test Hypothesis test engine: \code{"auto"} (default CBE hierarchy),
#'   \code{"exact"} (force Central Fisher exact test for 2x2 or simulated Fisher for RxC),
#'   \code{"chisq"} (force Pearson's Chi-squared test via \code{\link[stats]{chisq.test}}),
#'   or \code{"fisher"} (force \code{\link[stats]{fisher.test}}).
#' @param correct Logical; whether to apply continuity correction when \code{test = "chisq"}
#'   (default \code{FALSE} following CBE standard protocol).
#' @param ... Additional arguments passed to underlying plotting functions.
#'
#' @return A \code{ggplot} object (for \code{"balloon"}, \code{"bar"}, \code{"mosaic"}, \code{"heatmap"}, \code{"square"}),
#'   or invisibly the contingency table (for \code{"corrplot"}).
#' @export
#' @examples
#' df <- data.frame(
#'   Treatment = factor(c(rep("Active", 20), rep("Control", 20))),
#'   Outcome   = factor(c(rep("Response", 14), rep("None", 6),
#'                         rep("Response", 5), rep("None", 15)))
#' )
#'
#' # 1. Balloon Plot (Temple Brand Colors)
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "balloon")
#'
#' # 2. Proportional Bar Plot (100% Fill)
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "bar", bar_position = "fill")
#'
#' # 3. Mosaic Plot
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "mosaic")
#'
#' # 4. Heatmap
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "heatmap")
#'
#' # 5. Standard 4-Quadrant Square Plot (q1 | q2 // q3 | q4 // p = pformat)
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "square")
#'
#' # 6. Force Pearson Chi-Square Test
#' cbe_contingency_plot(df, "Outcome", "Treatment", type = "square", test = "chisq")
cbe_contingency_plot <- function(data, var1 = NULL, var2 = NULL,
                                 label1 = NULL, label2 = NULL,
                                 type = c("balloon", "bar", "mosaic", "heatmap", "square", "corrplot"),
                                 bar_position = c("fill", "dodge", "stack"),
                                 engine = c("ggplot", "corrplot", "ggpubr"),
                                 test = c("auto", "exact", "chisq", "fisher"),
                                 correct = FALSE,
                                 size_range = c(6, 22),
                                 fill_colors = c("#F4F4F4", "#9D2235"),
                                 show_counts = TRUE,
                                 title = NULL,
                                 caption = NULL,
                                 p_format = NULL,
                                 ...) {
  type <- match.arg(type)
  bar_position <- match.arg(bar_position)
  engine <- match.arg(engine)
  test <- match.arg(test)

  # Extract contingency table
  if (is.matrix(data) || is.table(data)) {
    tab <- as.table(data)
    dnames <- dimnames(tab)
    lbl1 <- if (!is.null(label1)) label1 else (if (!is.null(names(dnames)[1])) names(dnames)[1] else "Var1")
    lbl2 <- if (!is.null(label2)) label2 else (if (!is.null(names(dnames)[2])) names(dnames)[2] else "Var2")
  } else if (is.data.frame(data) && !is.null(var1) && !is.null(var2)) {
    d <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), , drop = FALSE]
    tab <- table(d[[var1]], d[[var2]])
    lbl1 <- if (!is.null(label1)) label1 else var1
    lbl2 <- if (!is.null(label2)) label2 else var2
  } else {
    stop("cbe_contingency_plot() requires a 2-way table/matrix or (data, var1, var2).", call. = FALSE)
  }

  dims <- dim(tab)
  is_2x2 <- identical(as.integer(dims), c(2L, 2L))
  plot_title <- if (!is.null(title)) title else paste0(lbl1, " \u00d7 ", lbl2)

  # Compute test results for caption
  has_zero <- any(tab == 0L)
  p_fmt_fn <- if (!is.null(p_format) && is.function(p_format)) p_format else function(p) pformat(p, add_p = TRUE)

  exp_counts <- tryCatch({
    suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
  }, error = function(e) matrix(0, nrow = dims[1], ncol = dims[2]))
  has_sparse <- any(exp_counts < 5, na.rm = TRUE)

  if (test == "chisq") {
    cs <- stats::chisq.test(tab, correct = correct)
    p_val <- cs$p.value
    p_fmt <- p_fmt_fn(p_val)
    stat_str <- sprintf(" (\u03c7\u00b2 = %.2f, df = %d)", cs$statistic, cs$parameter)
    caption_txt <- if (!is.null(caption)) caption else paste0(p_fmt, " (Pearson's Chi-Square Test", stat_str, ")")
  } else if (test == "fisher") {
    sim <- (sum(tab) > 500L || any(dims > 2L))
    ft <- stats::fisher.test(tab, simulate.p.value = sim, B = 2000L)
    p_val <- ft$p.value
    p_fmt <- p_fmt_fn(p_val)
    caption_txt <- if (!is.null(caption)) caption else paste0(p_fmt, " (Fisher's Exact Test)")
  } else if (test == "exact") {
    if (is_2x2) {
      tst <- cbe_exact2x2(tab, midp = has_zero)
      p_val <- tst$p.value
      p_fmt <- p_fmt_fn(p_val)
      caption_txt <- if (!is.null(caption)) {
        caption
      } else if (has_zero) {
        paste0(p_fmt, " (Central Fisher's Exact Test, mid-p)")
      } else {
        paste0(p_fmt, " (Central Fisher's Exact Test)")
      }
    } else {
      sim <- (sum(tab) > 500L || any(dims > 2L))
      ft <- stats::fisher.test(tab, simulate.p.value = sim, B = 2000L)
      p_val <- ft$p.value
      p_fmt <- p_fmt_fn(p_val)
      caption_txt <- if (!is.null(caption)) caption else paste0(p_fmt, " (Fisher's Exact Test, sparse cells)")
    }
  } else {
    # test == "auto" (default CBE hierarchy)
    if (is_2x2) {
      tst <- cbe_exact2x2(tab, midp = has_zero)
      p_val <- tst$p.value
      p_fmt <- p_fmt_fn(p_val)
      caption_txt <- if (!is.null(caption)) {
        caption
      } else if (has_zero) {
        paste0(p_fmt, " (Central Fisher's Exact Test, mid-p)")
      } else {
        paste0(p_fmt, " (Central Fisher's Exact Test)")
      }
    } else {
      if (has_sparse) {
        ft <- stats::fisher.test(tab, simulate.p.value = (sum(tab) > 500L || any(dims > 2L)), B = 2000L)
        p_fmt <- p_fmt_fn(ft$p.value)
        caption_txt <- if (!is.null(caption)) caption else paste0(p_fmt, " (Fisher's Exact Test, expected < 5)")
      } else {
        cs <- stats::chisq.test(tab, correct = correct)
        p_fmt <- p_fmt_fn(cs$p.value)
        caption_txt <- if (!is.null(caption)) caption else paste0(p_fmt, " (Pearson's Chi-Square Test)")
      }
    }
  }

  # --- Type 1: corrplot engine ---
  if (type == "corrplot" || ((type == "balloon" || type == "square") && engine == "corrplot")) {
    corr_pal <- grDevices::colorRampPalette(fill_colors)
    c_method <- if (type == "square") "square" else "circle"
    corrplot::corrplot(
      tab,
      is.corr = FALSE,
      method = c_method,
      title = plot_title,
      col = corr_pal(100),
      cl.pos = "r",
      mar = c(1, 1, 3, 1),
      ...
    )
    return(invisible(tab))
  }

  # --- Type 2: Bar Chart (fill, dodge, or stack) ---
  if (type == "bar") {
    df_tab <- as.data.frame(tab)
    colnames(df_tab) <- c("Var1", "Var2", "Freq")

    pal <- grDevices::colorRampPalette(c("#8c1d2f", "#d67c8c", "#2b2b2b", "#707070"))(nrow(tab))

    p <- ggplot2::ggplot(df_tab, ggplot2::aes(x = .data$Var2, y = .data$Freq, fill = .data$Var1))

    if (bar_position == "fill") {
      p <- p +
        ggplot2::geom_bar(position = "fill", stat = "identity", color = "white", linewidth = 0.6) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(y = "Proportion")
      if (isTRUE(show_counts)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%d", .data$Freq)),
          position = ggplot2::position_fill(vjust = 0.5),
          color = "white",
          fontface = "bold",
          size = 4
        )
      }
    } else if (bar_position == "dodge") {
      p <- p +
        ggplot2::geom_bar(position = ggplot2::position_dodge(width = 0.8), stat = "identity", color = "white", width = 0.7) +
        ggplot2::labs(y = "Count") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))
      if (isTRUE(show_counts)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data$Freq),
          position = ggplot2::position_dodge(width = 0.8),
          vjust = -0.4,
          fontface = "bold",
          size = 3.8
        )
      }
    } else {
      # stack
      p <- p +
        ggplot2::geom_bar(position = "stack", stat = "identity", color = "white") +
        ggplot2::labs(y = "Total Count") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
      if (isTRUE(show_counts)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = .data$Freq),
          position = ggplot2::position_stack(vjust = 0.5),
          color = "white",
          fontface = "bold",
          size = 4
        )
      }
    }

    p <- p +
      ggplot2::scale_fill_manual(values = pal, name = lbl1) +
      ggplot2::labs(
        title = plot_title,
        caption = caption_txt,
        x = lbl2
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = ggplot2::element_text(face = "italic", size = 10, color = "#555555", margin = ggplot2::margin(t = 10)),
        axis.title = ggplot2::element_text(face = "bold", size = 11),
        axis.text = ggplot2::element_text(size = 10),
        legend.position = "top",
        panel.grid.major.x = ggplot2::element_blank()
      )

    return(p)
  }

  # --- Type 3: Mosaic Plot ---
  if (type == "mosaic") {
    n_tot <- sum(tab)
    col_totals <- colSums(tab)
    col_widths <- col_totals / n_tot
    x_maxs <- cumsum(col_widths)
    x_mins <- c(0, utils::head(x_maxs, -1))

    col_names <- colnames(tab)
    row_names <- rownames(tab)

    rect_rows <- list()
    for (j in seq_along(col_names)) {
      c_tot <- col_totals[j]
      if (c_tot == 0) next
      c_counts <- tab[, j]
      r_props <- c_counts / c_tot
      y_maxs <- cumsum(r_props)
      y_mins <- c(0, utils::head(y_maxs, -1))

      for (i in seq_along(row_names)) {
        cnt <- c_counts[i]
        rect_rows[[length(rect_rows) + 1]] <- data.frame(
          var2 = col_names[j],
          var1 = row_names[i],
          count = cnt,
          prop_within = r_props[i],
          xmin = x_mins[j],
          xmax = x_maxs[j],
          ymin = y_mins[i],
          ymax = y_maxs[i],
          xmid = (x_mins[j] + x_maxs[j]) / 2,
          ymid = (y_mins[i] + y_maxs[i]) / 2,
          stringsAsFactors = FALSE
        )
      }
    }
    df_mosaic <- do.call(rbind, rect_rows)
    pal <- grDevices::colorRampPalette(c("#9D2235", "#D97B89", "#2B2B2B", "#6E6E6E"))(nrow(tab))

    x_ticks <- unique(df_mosaic[, c("var2", "xmid")])

    p <- ggplot2::ggplot(df_mosaic) +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$var1),
        color = "white",
        linewidth = 0.8
      ) +
      ggplot2::scale_fill_manual(values = pal, name = lbl1) +
      ggplot2::scale_x_continuous(
        breaks = x_ticks$xmid,
        labels = x_ticks$var2,
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = plot_title,
        subtitle = "Width \u221d Marginal Sample Size | Height \u221d Conditional Proportion",
        caption = caption_txt,
        x = lbl2,
        y = paste0("Within-", lbl2, " Proportion")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#666666", hjust = 0.5, margin = ggplot2::margin(b = 8)),
        plot.caption = ggplot2::element_text(face = "italic", size = 10, color = "#555555", margin = ggplot2::margin(t = 10)),
        axis.title = ggplot2::element_text(face = "bold", size = 11),
        axis.text = ggplot2::element_text(size = 10),
        legend.position = "top",
        panel.grid = ggplot2::element_blank()
      )

    if (isTRUE(show_counts)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          x = .data$xmid,
          y = .data$ymid,
          label = paste0(.data$count, "\n(", sprintf("%.0f%%", .data$prop_within * 100), ")")
        ),
        color = "white",
        fontface = "bold",
        size = 3.8
      )
    }

    return(p)
  }

  # --- Type 4: Heatmap ---
  if (type == "heatmap") {
    df_plot <- as.data.frame(tab)
    colnames(df_plot) <- c("Var1", "Var2", "Freq")
    n_tot <- sum(df_plot$Freq)
    max_freq <- max(df_plot$Freq, na.rm = TRUE)
    text_col <- ifelse(df_plot$Freq > (max_freq * 0.55), "white", "black")

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$Var2, y = .data$Var1, fill = .data$Freq)) +
      ggplot2::geom_tile(color = "white", linewidth = 1.2) +
      ggplot2::scale_fill_gradient(
        low = fill_colors[1],
        high = fill_colors[length(fill_colors)],
        name = "Count"
      ) +
      ggplot2::labs(
        title = plot_title,
        caption = caption_txt,
        x = lbl2,
        y = lbl1
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = ggplot2::element_text(face = "italic", size = 10, color = "#555555", margin = ggplot2::margin(t = 10)),
        axis.title = ggplot2::element_text(face = "bold", size = 11),
        axis.text = ggplot2::element_text(size = 10),
        panel.grid = ggplot2::element_blank()
      )

    if (isTRUE(show_counts)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = paste0(.data$Freq, "\n(", sprintf("%.1f%%", (.data$Freq / n_tot) * 100), ")")),
        color = text_col,
        fontface = "bold",
        size = 4
      )
    }

    return(p)
  }

  # --- Type 5: Square (Standard 4-Quadrant Report for 2x2, or square tiles for RxC) ---
  if (type == "square") {
    if (is_2x2) {
      n_tot <- sum(tab)
      r_names <- rownames(tab)
      c_names <- colnames(tab)

      # Standard 4-quadrant layout:
      # q1: top-left (Row 1, Col 1) -> x = 1, y = 2
      # q2: top-right (Row 1, Col 2) -> x = 2, y = 2
      # q3: bottom-left (Row 2, Col 1) -> x = 1, y = 1
      # q4: bottom-right (Row 2, Col 2) -> x = 2, y = 1
      df_q <- data.frame(
        quadrant = c("q1", "q2", "q3", "q4"),
        x = c(1, 2, 1, 2),
        y = c(2, 2, 1, 1),
        row_lvl = c(r_names[1], r_names[1], r_names[2], r_names[2]),
        col_lvl = c(c_names[1], c_names[2], c_names[1], c_names[2]),
        count = c(tab[1, 1], tab[1, 2], tab[2, 1], tab[2, 2]),
        stringsAsFactors = FALSE
      )
      df_q$pct_tot <- (df_q$count / n_tot) * 100

      # Format box text:
      # q1 | q2
      # q3 | q4
      df_q$label_box <- if (isTRUE(show_counts)) {
        paste0(df_q$quadrant, "\n\n", df_q$count, "\n(", sprintf("%.1f%%", df_q$pct_tot), ")")
      } else {
        paste0(df_q$quadrant, "\n\n", sprintf("%.1f%%", df_q$pct_tot))
      }

      max_cnt <- max(df_q$count, na.rm = TRUE)
      text_col <- ifelse(df_q$count > (max_cnt * 0.6), "white", "black")

      p <- ggplot2::ggplot(df_q, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data$count),
          color = "#2b2b2b",
          linewidth = 1.5,
          width = 0.94,
          height = 0.94
        ) +
        ggplot2::scale_fill_gradient(
          low = fill_colors[1],
          high = fill_colors[length(fill_colors)],
          name = "Count"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = .data$label_box),
          color = text_col,
          fontface = "bold",
          size = 4.5
        ) +
        ggplot2::scale_x_continuous(
          breaks = c(1, 2),
          labels = c_names,
          position = "top",
          expand = c(0.08, 0.08)
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(2, 1),
          labels = r_names,
          expand = c(0.08, 0.08)
        ) +
        ggplot2::labs(
          title = plot_title,
          subtitle = "Standard 4-Quadrant Report (q1 | q2 // q3 | q4)",
          caption = caption_txt,
          x = lbl2,
          y = lbl1
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 10, color = "#666666", hjust = 0.5, margin = ggplot2::margin(b = 10)),
          plot.caption = ggplot2::element_text(face = "bold", size = 12, color = "#9D2235", hjust = 0.5, margin = ggplot2::margin(t = 12)),
          axis.title.x = ggplot2::element_text(face = "bold", size = 11, margin = ggplot2::margin(b = 6)),
          axis.title.y = ggplot2::element_text(face = "bold", size = 11, margin = ggplot2::margin(r = 6)),
          axis.text = ggplot2::element_text(face = "bold", size = 11, color = "#2b2b2b"),
          panel.grid = ggplot2::element_blank()
        )

      return(p)
    }

    # General RxC square tiles
    df_plot <- as.data.frame(tab)
    colnames(df_plot) <- c("Var1", "Var2", "Freq")

    max_freq <- max(df_plot$Freq, na.rm = TRUE)
    text_col <- ifelse(df_plot$Freq > (max_freq * 0.65), "white", "black")

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$Var2, y = .data$Var1)) +
      ggplot2::geom_point(
        ggplot2::aes(size = .data$Freq, fill = .data$Freq),
        shape = 22,
        color = "#2b2b2b",
        stroke = 0.8
      ) +
      ggplot2::scale_size_continuous(range = size_range, guide = "none") +
      ggplot2::scale_fill_gradient(
        low = fill_colors[1],
        high = fill_colors[length(fill_colors)],
        name = "Count"
      ) +
      ggplot2::labs(
        title = plot_title,
        caption = caption_txt,
        x = lbl2,
        y = lbl1
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = ggplot2::element_text(face = "italic", size = 10, color = "#555555", margin = ggplot2::margin(t = 10)),
        axis.title = ggplot2::element_text(face = "bold", size = 11),
        axis.text = ggplot2::element_text(size = 10),
        panel.grid.major = ggplot2::element_line(color = "#e5e5e5", linewidth = 0.5),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (isTRUE(show_counts)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$Freq),
        color = text_col,
        fontface = "bold",
        size = 4
      )
    }

    return(p)
  }

  # --- Type 6: Balloon Plot (default) ---
  if (engine == "ggpubr" && requireNamespace("ggpubr", quietly = TRUE)) {
    p <- ggpubr::ggballoonplot(
      as.data.frame(tab),
      main = plot_title,
      size.range = size_range,
      fill = "Freq",
      show.label = show_counts,
      font.label = list(color = "black"),
      caption = caption_txt,
      ...
    ) +
      ggplot2::scale_fill_gradient(low = fill_colors[1], high = fill_colors[length(fill_colors)]) +
      ggplot2::guides(size = "none")
    return(p)
  }

  # Native ggplot2 balloon plot
  df_plot <- as.data.frame(tab)
  colnames(df_plot) <- c("Var1", "Var2", "Freq")

  max_freq <- max(df_plot$Freq, na.rm = TRUE)
  text_col <- ifelse(df_plot$Freq > (max_freq * 0.65), "white", "black")

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$Var2, y = .data$Var1)) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$Freq, fill = .data$Freq),
      shape = 21,
      color = "#2b2b2b",
      stroke = 0.8
    ) +
    ggplot2::scale_size_continuous(range = size_range, guide = "none") +
    ggplot2::scale_fill_gradient(
      low = fill_colors[1],
      high = fill_colors[length(fill_colors)],
      name = "Count"
    ) +
    ggplot2::labs(
      title = plot_title,
      caption = caption_txt,
      x = lbl2,
      y = lbl1
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.caption = ggplot2::element_text(face = "italic", size = 10, color = "#555555", margin = ggplot2::margin(t = 10)),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_line(color = "#e5e5e5", linewidth = 0.5),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(show_counts)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$Freq),
      color = text_col,
      fontface = "bold",
      size = 4
    )
  }

  p
}

#' Balloon Plot for 2-Way Contingency Tables
#'
#' Dedicated wrapper for \code{\link{cbe_contingency_plot}} with \code{type = "balloon"}.
#'
#' @inheritParams cbe_contingency_plot
#' @return A \code{ggplot} object or invisibly the contingency table.
#' @export
cbe_balloon_plot <- function(data, var1 = NULL, var2 = NULL,
                             label1 = NULL, label2 = NULL,
                             engine = c("ggplot", "corrplot", "ggpubr"),
                             size_range = c(6, 22),
                             fill_colors = c("#F4F4F4", "#9D2235"),
                             show_counts = TRUE,
                             title = NULL,
                             ...) {
  engine <- match.arg(engine)
  cbe_contingency_plot(
    data = data, var1 = var1, var2 = var2,
    label1 = label1, label2 = label2,
    type = "balloon",
    engine = engine,
    size_range = size_range,
    fill_colors = fill_colors,
    show_counts = show_counts,
    title = title,
    ...
  )
}

#' Bar Chart for 2-Way Contingency Tables
#'
#' Dedicated wrapper for \code{\link{cbe_contingency_plot}} with \code{type = "bar"}.
#'
#' @inheritParams cbe_contingency_plot
#' @return A \code{ggplot} object.
#' @export
cbe_bar_plot <- function(data, var1 = NULL, var2 = NULL,
                         label1 = NULL, label2 = NULL,
                         bar_position = c("fill", "dodge", "stack"),
                         show_counts = TRUE,
                         title = NULL,
                         ...) {
  cbe_contingency_plot(
    data = data, var1 = var1, var2 = var2,
    label1 = label1, label2 = label2,
    type = "bar",
    bar_position = bar_position,
    show_counts = show_counts,
    title = title,
    ...
  )
}

#' Mosaic Plot for 2-Way Contingency Tables
#'
#' Dedicated wrapper for \code{\link{cbe_contingency_plot}} with \code{type = "mosaic"}.
#'
#' @inheritParams cbe_contingency_plot
#' @return A \code{ggplot} object.
#' @export
cbe_mosaic_plot <- function(data, var1 = NULL, var2 = NULL,
                            label1 = NULL, label2 = NULL,
                            show_counts = TRUE,
                            title = NULL,
                            ...) {
  cbe_contingency_plot(
    data = data, var1 = var1, var2 = var2,
    label1 = label1, label2 = label2,
    type = "mosaic",
    show_counts = show_counts,
    title = title,
    ...
  )
}

#' Heatmap Plot for 2-Way Contingency Tables
#'
#' Dedicated wrapper for \code{\link{cbe_contingency_plot}} with \code{type = "heatmap"}.
#'
#' @inheritParams cbe_contingency_plot
#' @return A \code{ggplot} object.
#' @export
cbe_heatmap_plot <- function(data, var1 = NULL, var2 = NULL,
                             label1 = NULL, label2 = NULL,
                             fill_colors = c("#F4F4F4", "#9D2235"),
                             show_counts = TRUE,
                             title = NULL,
                             ...) {
  cbe_contingency_plot(
    data = data, var1 = var1, var2 = var2,
    label1 = label1, label2 = label2,
    type = "heatmap",
    fill_colors = fill_colors,
    show_counts = show_counts,
    title = title,
    ...
  )
}

#' Standard 4-Quadrant Clinical Contingency Report
#'
#' Generates a standard 4-quadrant clinical report for a 2x2 contingency table:
#' \preformatted{
#' q1 | q2
#' q3 | q4
#' p = pformat
#' }
#' where q1 is (Row 1, Col 1), q2 is (Row 1, Col 2), q3 is (Row 2, Col 1), and q4 is (Row 2, Col 2).
#' If any cell count is zero, hypothesis testing automatically defaults to the
#' mid-p version of Central Fisher's exact test via \code{\link{cbe_exact2x2}}.
#'
#' @param data A data frame or a 2x2 matrix/table.
#' @param var1 Character string of row variable column name (if \code{data} is a data frame).
#' @param var2 Character string of column variable column name (if \code{data} is a data frame).
#' @param label1 Display label for row variable.
#' @param label2 Display label for column variable.
#' @param test Hypothesis test engine: \code{"auto"} (default CBE hierarchy: mid-p exact if zero-cell,
#'   otherwise Central Fisher's exact), \code{"exact"} (force exact test),
#'   \code{"chisq"} (force Pearson's Chi-squared test via \code{\link[stats]{chisq.test}}),
#'   or \code{"fisher"} (force \code{\link[stats]{fisher.test}}).
#' @param correct Logical; whether to apply continuity correction for \code{"chisq"} (default \code{FALSE}).
#' @param p_format Function for p-value formatting (default \code{\link{pformat}}).
#' @param ... Additional arguments passed to \code{\link{cbe_contingency_plot}}.
#'
#' @return A list containing:
#'   \item{quadrants}{A tibble with counts and percentages for q1, q2, q3, and q4.}
#'   \item{p_value}{Numeric exact test p-value.}
#'   \item{p_formatted}{Formatted p-value string (e.g. \code{"p = 0.024"}).}
#'   \item{test_method}{Name of the exact test used.}
#'   \item{compact_report}{Compact 3-line string formatted as \code{q1 | q2 // q3 | q4 // p = pformat}.}
#'   \item{text_card}{Console-ready formatted 4-quadrant text card.}
#'   \item{plot}{A \code{ggplot} object rendering the 4-quadrant square report.}
#' @export
#' @examples
#' tab <- matrix(c(14, 6, 4, 16), nrow = 2,
#'               dimnames = list(c("Yes", "No"), c("Active", "Placebo")))
#' rep <- cbe_four_quadrant_report(tab, label1 = "Response", label2 = "Treatment")
#' cat(rep$compact_report, "\n\n")
#' cat(rep$text_card)
#'
#' # Force Pearson Chi-squared test:
#' rep_chi <- cbe_four_quadrant_report(tab, test = "chisq")
#' cat(rep_chi$compact_report, "\n")
cbe_four_quadrant_report <- function(data, var1 = NULL, var2 = NULL,
                                     label1 = NULL, label2 = NULL,
                                     test = c("auto", "exact", "chisq", "fisher"),
                                     correct = FALSE,
                                     p_format = NULL, ...) {
  test <- match.arg(test)
  if (is.matrix(data) || is.table(data)) {
    tab <- as.table(data)
    dnames <- dimnames(tab)
    lbl1 <- if (!is.null(label1)) label1 else (if (!is.null(names(dnames)[1])) names(dnames)[1] else "Row")
    lbl2 <- if (!is.null(label2)) label2 else (if (!is.null(names(dnames)[2])) names(dnames)[2] else "Col")
  } else if (is.data.frame(data) && !is.null(var1) && !is.null(var2)) {
    d <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), , drop = FALSE]
    tab <- table(d[[var1]], d[[var2]])
    lbl1 <- if (!is.null(label1)) label1 else var1
    lbl2 <- if (!is.null(label2)) label2 else var2
  } else {
    stop("cbe_four_quadrant_report() requires a 2x2 table/matrix or (data, var1, var2).", call. = FALSE)
  }

  dims <- dim(tab)
  if (!identical(as.integer(dims), c(2L, 2L))) {
    stop("cbe_four_quadrant_report() requires a 2x2 table; got ", paste(dims, collapse = "x"), ".", call. = FALSE)
  }

  has_zero <- any(tab == 0L)
  if (test == "chisq") {
    cs <- stats::chisq.test(tab, correct = correct)
    p_val <- cs$p.value
    tst_method <- sprintf("Pearson's Chi-squared test (\u03c7\u00b2 = %.2f, df = %d)", cs$statistic, cs$parameter)
  } else if (test == "fisher") {
    ft <- stats::fisher.test(tab)
    p_val <- ft$p.value
    tst_method <- "Fisher's Exact Test"
  } else {
    # test == "auto" or "exact"
    tst <- cbe_exact2x2(tab, midp = has_zero)
    p_val <- tst$p.value
    tst_method <- tst$method
  }

  p_fmt_fn <- if (!is.null(p_format) && is.function(p_format)) {
    p_format
  } else {
    function(p) pformat(p, add_p = TRUE)
  }
  p_formatted <- p_fmt_fn(p_val)

  n_tot <- sum(tab)
  r_names <- rownames(tab)
  c_names <- colnames(tab)

  q_df <- tibble::tibble(
    quadrant = c("q1", "q2", "q3", "q4"),
    row_level = c(r_names[1], r_names[1], r_names[2], r_names[2]),
    col_level = c(c_names[1], c_names[2], c_names[1], c_names[2]),
    count = c(tab[1, 1], tab[1, 2], tab[2, 1], tab[2, 2]),
    pct_total = (c(tab[1, 1], tab[1, 2], tab[2, 1], tab[2, 2]) / n_tot) * 100,
    pct_row = c(tab[1, 1] / sum(tab[1, ]), tab[1, 2] / sum(tab[1, ]),
                tab[2, 1] / sum(tab[2, ]), tab[2, 2] / sum(tab[2, ])) * 100,
    pct_col = c(tab[1, 1] / sum(tab[, 1]), tab[1, 2] / sum(tab[, 2]),
                tab[2, 1] / sum(tab[, 1]), tab[2, 2] / sum(tab[, 2])) * 100
  )

  # Standard 4 quant report:
  # q1 | q2
  # q3 | q4
  # p = pformat
  compact_report <- sprintf(
    "q1: n = %d (%.1f%%) | q2: n = %d (%.1f%%)\nq3: n = %d (%.1f%%) | q4: n = %d (%.1f%%)\n%s",
    tab[1, 1], q_df$pct_total[1], tab[1, 2], q_df$pct_total[2],
    tab[2, 1], q_df$pct_total[3], tab[2, 2], q_df$pct_total[4],
    p_formatted
  )

  card_text <- sprintf(
    "q1: %s / %s | q2: %s / %s\n  n = %d (%.1f%%) |   n = %d (%.1f%%)\n-----------------------+-----------------------\nq3: %s / %s | q4: %s / %s\n  n = %d (%.1f%%) |   n = %d (%.1f%%)\n%s (%s)\n",
    r_names[1], c_names[1], r_names[1], c_names[2],
    tab[1, 1], q_df$pct_total[1], tab[1, 2], q_df$pct_total[2],
    r_names[2], c_names[1], r_names[2], c_names[2],
    tab[2, 1], q_df$pct_total[3], tab[2, 2], q_df$pct_total[4],
    p_formatted, tst_method
  )

  # Generate ggplot
  plt <- cbe_contingency_plot(
    data = tab,
    label1 = lbl1,
    label2 = lbl2,
    type = "square",
    test = test,
    correct = correct,
    p_format = p_fmt_fn,
    ...
  )

  list(
    quadrants = q_df,
    p_value = p_val,
    p_formatted = p_formatted,
    test_method = tst_method,
    compact_report = compact_report,
    text_card = card_text,
    plot = plt
  )
}

#' Square Plot: Standard 4-Quadrant Contingency Report
#'
#' Dedicated wrapper for \code{\link{cbe_contingency_plot}} with \code{type = "square"}.
#' For 2x2 contingency tables, renders the standard 4-quadrant report:
#' \preformatted{
#' q1 | q2
#' q3 | q4
#' p = pformat
#' }
#' If any cell count is zero, hypothesis testing automatically defaults to the
#' mid-p version of Central Fisher's exact test via \code{\link{cbe_exact2x2}}.
#'
#' @inheritParams cbe_contingency_plot
#' @return A \code{ggplot} object or invisibly the contingency table.
#' @export
cbe_square_plot <- function(data, var1 = NULL, var2 = NULL,
                            label1 = NULL, label2 = NULL,
                            test = c("auto", "exact", "chisq", "fisher"),
                            correct = FALSE,
                            engine = c("ggplot", "corrplot"),
                            size_range = c(6, 22),
                            fill_colors = c("#F4F4F4", "#9D2235"),
                            show_counts = TRUE,
                            title = NULL,
                            caption = NULL,
                            p_format = NULL,
                            ...) {
  test <- match.arg(test)
  engine <- match.arg(engine)
  cbe_contingency_plot(
    data = data, var1 = var1, var2 = var2,
    label1 = label1, label2 = label2,
    type = "square",
    test = test,
    correct = correct,
    engine = engine,
    size_range = size_range,
    fill_colors = fill_colors,
    show_counts = show_counts,
    title = title,
    caption = caption,
    p_format = p_format,
    ...
  )
}

#' Plot Categorical Association Matrix Using corrplot
#'
#' Computes pairwise associations (Cram\u00e9r's V or -log10 p-values) across all
#' categorical/factor columns in a data frame and renders an association matrix plot
#' via \pkg{corrplot}. This complements \code{\link{correlation_plot}} for numeric
#' variables.
#'
#' @param data A data frame or tibble.
#' @param cols Optional character vector of column names to include. If \code{NULL},
#'   all factor and character columns are selected.
#' @param method Association metric: \code{"cramer_v"} (Cramer's V correlation,
#'   default) or \code{"p_value"} (\eqn{-\log_{10}(p)} from categorical tests).
#' @param title Plot title (defaults to \code{"Categorical Association Matrix"}).
#' @param fill_colors Character vector for the color gradient (defaults to Temple palette).
#' @param tl.cex Label character expansion (default 0.8).
#' @param number.cex Numeric value character expansion inside circles (default 0.75).
#' @param show_coef Logical (default \code{TRUE}); whether to print coefficients.
#' @param ... Additional arguments passed to \code{\link[corrplot]{corrplot}}.
#'
#' @return Invisibly, the association matrix.
#' @export
#' @examples
#' df <- data.frame(
#'   A = factor(sample(c("Yes", "No"), 50, replace = TRUE)),
#'   B = factor(sample(c("High", "Low"), 50, replace = TRUE)),
#'   C = factor(sample(c("Group1", "Group2", "Group3"), 50, replace = TRUE))
#' )
#' plot_categorical_associations(df)
plot_categorical_associations <- function(data, cols = NULL,
                                          method = c("cramer_v", "p_value"),
                                          title = "Categorical Association Matrix",
                                          fill_colors = c("#FFFFFF", "#9D2235"),
                                          tl.cex = 0.8,
                                          number.cex = 0.75,
                                          show_coef = TRUE,
                                          ...) {
  method <- match.arg(method)

  if (is.null(cols)) {
    cat_df <- dplyr::select(data, dplyr::where(is.character) | dplyr::where(is.factor))
  } else {
    cat_df <- dplyr::select(data, dplyr::all_of(cols))
  }

  p <- ncol(cat_df)
  if (p < 2) {
    stop("plot_categorical_associations() requires at least 2 categorical columns; got ", p, ".", call. = FALSE)
  }

  col_names <- colnames(cat_df)
  mat <- matrix(NA_real_, nrow = p, ncol = p, dimnames = list(col_names, col_names))

  for (i in seq_len(p)) {
    mat[i, i] <- if (method == "cramer_v") 1.0 else 0.0
    if (i < p) {
      for (j in (i + 1):p) {
        v1 <- cat_df[[i]]
        v2 <- cat_df[[j]]
        complete_idx <- !is.na(v1) & !is.na(v2)
        tab <- table(v1[complete_idx], v2[complete_idx])
        n <- sum(tab)
        dims <- dim(tab)

        if (n == 0 || any(dims < 2)) {
          val <- NA_real_
        } else if (method == "cramer_v") {
          cs <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
          k <- min(dims[1] - 1L, dims[2] - 1L)
          val <- if (k > 0 && n > 0) sqrt(max(0, cs / (n * k))) else 0.0
        } else {
          # -log10(p)
          test_res <- cbe_test_categorical(
            data = data.frame(v1 = v1[complete_idx], v2 = v2[complete_idx]),
            variable = "v1", by = "v2"
          )
          pval <- max(test_res$p.value, 1e-16, na.rm = TRUE)
          val <- -log10(pval)
        }
        mat[i, j] <- val
        mat[j, i] <- val
      }
    }
  }

  corr_pal <- grDevices::colorRampPalette(fill_colors)
  col_lim <- if (method == "cramer_v") c(0, 1) else c(0, max(mat, na.rm = TRUE))

  coef_args <- if (isTRUE(show_coef)) {
    list(addCoef.col = "black", number.cex = number.cex, number.digits = if (method == "cramer_v") 2 else 1)
  } else {
    list()
  }

  do.call(corrplot::corrplot, c(
    list(
      mat,
      is.corr = FALSE,
      method = "circle",
      col = corr_pal(100),
      col.lim = col_lim,
      title = title,
      mar = c(0, 0, 2, 0),
      tl.cex = tl.cex,
      tl.col = "black"
    ),
    coef_args,
    list(...)
  ))

  invisible(mat)
}

#' Generate All Pairwise Categorical Combinations
#'
#' Helper for report generation to enumerate all pairwise combinations of
#' categorical variables in a dataset with sequential integer IDs and labels.
#'
#' @param data A data frame or tibble.
#' @param cols Optional character vector of column names to include. If \code{NULL},
#'   all factor and character columns are included.
#' @param labels_df Optional lookup data frame with columns \code{variable} and \code{label}.
#'
#' @return A tibble with columns \code{id}, \code{var1}, \code{var2}, \code{label1}, \code{label2},
#'   and \code{comparison_label}.
#' @export
#' @examples
#' df <- data.frame(
#'   A = factor(c("X", "Y")),
#'   B = factor(c("1", "2")),
#'   C = factor(c("M", "N"))
#' )
#' cbe_pairwise_combos(df)
cbe_pairwise_combos <- function(data, cols = NULL, labels_df = NULL) {
  if (is.null(cols)) {
    cat_cols <- colnames(dplyr::select(data, dplyr::where(is.character) | dplyr::where(is.factor)))
  } else {
    cat_cols <- intersect(cols, colnames(data))
  }

  if (length(cat_cols) < 2) {
    return(tibble::tibble(
      id = integer(),
      var1 = character(),
      var2 = character(),
      label1 = character(),
      label2 = character(),
      comparison_label = character()
    ))
  }

  combos <- utils::combn(cat_cols, 2, simplify = FALSE)

  get_lbl <- function(v) {
    if (!is.null(labels_df) && all(c("variable", "label") %in% names(labels_df))) {
      match_row <- labels_df$label[labels_df$variable == v]
      if (length(match_row) > 0 && nzchar(match_row[1])) return(match_row[1])
    }
    v
  }

  res_list <- lapply(seq_along(combos), function(i) {
    pair <- combos[[i]]
    l1 <- get_lbl(pair[1])
    l2 <- get_lbl(pair[2])
    tibble::tibble(
      id = i,
      var1 = pair[1],
      var2 = pair[2],
      label1 = l1,
      label2 = l2,
      comparison_label = paste0(l1, " \u00d7 ", l2)
    )
  })

  dplyr::bind_rows(res_list)
}
