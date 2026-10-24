#' Scatter Plot With a Regression Line and an Automatic Statistics Sentence
#'
#' A scatter plot of `y` against `x` with a fitted straight line and confidence band, and, optionally,
#' a sentence reporting Pearson's correlation and the equation of the line:
#' "Pearson's r of 0.52, with p-value of 0.001. The regression line is given by y = 1.2*x + 3.4."
#'
#' @section Statistics:
#' Rows with a missing `x` or `y` are dropped. `r` and its p-value come from [stats::cor.test()]
#' (Pearson). The line is the ordinary least-squares fit of `y` on `x` ([stats::lm()], the same as
#' `glm()` with a Gaussian family), drawn with its `conf_level` confidence band for the mean. With fewer than
#' three complete pairs, or no variation in `x` or `y`, the statistics are `NA` and no line is drawn.
#' With `facet_by`, the statistics are computed within each panel.
#'
#' @param data A data frame.
#' @param x,y Names of the numeric columns.
#' @param conf_level Confidence level of the band (default 0.95).
#' @param annotate_stats If `TRUE` (default) the sentence is added as the caption (one line per panel
#'   when faceting).
#' @param x_label,y_label Axis labels (default: the column names).
#' @param facet_by Optional column to facet by.
#' @param digits Significant digits in the sentence.
#' @return A ggplot2 object with attribute `"stats"`, a tibble with `panel`, `n`, `r`, `p.value`, `slope`,
#'   `intercept` and `sentence`.
#' @seealso [plot_target_correlation_heatmap()], [correlation_plot()]
#' @export
#' @examples
#' plot_scatter_glm(mtcars, "wt", "mpg")
#' attr(plot_scatter_glm(mtcars, "wt", "mpg"), "stats")$sentence
plot_scatter_glm <- function(data,
                             x,
                             y,
                             conf_level = 0.95,
                             annotate_stats = TRUE,
                             x_label = NULL,
                             y_label = NULL,
                             facet_by = NULL,
                             digits = 3) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (a in c("x", "y")) {
    v <- get(a)
    if (!is.character(v) || length(v) != 1L || !v %in% names(data)) stop("`", a, "` must name one column of `data`.", call. = FALSE)
    if (!is.numeric(data[[v]])) stop("`", a, "` must be a numeric column.", call. = FALSE)
  }
  if (!is.null(facet_by) && (!is.character(facet_by) || length(facet_by) != 1L || !facet_by %in% names(data))) {
    stop("`facet_by` must name one column of `data`.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }
  df <- data[stats::complete.cases(data[, c(x, y, facet_by), drop = FALSE]), , drop = FALSE]
  if (nrow(df) == 0L) stop("No complete pairs of `x` and `y` to plot.", call. = FALSE)

  panels <- if (is.null(facet_by)) list(all = seq_len(nrow(df))) else split(seq_len(nrow(df)), as.character(df[[facet_by]]))
  stat_rows <- lapply(names(panels), function(nm) {
    d <- df[panels[[nm]], , drop = FALSE]
    n <- nrow(d)
    r <- p <- slope <- intercept <- NA_real_
    if (n >= 3L && stats::sd(d[[x]]) > 0 && stats::sd(d[[y]]) > 0) {
      ct <- stats::cor.test(d[[x]], d[[y]])
      r <- unname(ct$estimate)
      p <- ct$p.value
      cf <- stats::coef(stats::lm(d[[y]] ~ d[[x]]))
      intercept <- unname(cf[1])
      slope <- unname(cf[2])
    }
    sentence <- if (is.na(r)) {
      "Not enough variation to compute a correlation."
    } else {
      sprintf("Pearson's r of %s, with p-value of %s. The regression line is given by y = %s*x %s %s.",
              signif(r, digits), if (p < 0.001) "<0.001" else format(signif(p, digits), scientific = FALSE),
              signif(slope, digits), if (intercept < 0) "-" else "+", signif(abs(intercept), digits))
    }
    tibble::tibble(panel = nm, n = n, r = r, p.value = p, slope = slope, intercept = intercept, sentence = sentence)
  })
  stats_tbl <- dplyr::bind_rows(stat_rows)

  # a line only in panels with enough points and some variation (the same ones that get statistics)
  fit_ok <- unlist(lapply(names(panels), function(nm) {
    d <- df[panels[[nm]], , drop = FALSE]
    rep(nrow(d) >= 3L && stats::sd(d[[x]]) > 0 && stats::sd(d[[y]]) > 0, nrow(d))
  }))
  df_line <- df[unlist(panels)[fit_ok], , drop = FALSE]
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(alpha = 0.6, color = "#6F6F6F") +
    ggplot2::geom_smooth(data = df_line, method = "lm", formula = y ~ x, level = conf_level, color = "#9D2235",
                         fill = "#9D2235", alpha = 0.15, na.rm = TRUE) +
    theme_cbe() +
    ggplot2::labs(x = x_label %||% x, y = y_label %||% y)
  if (!is.null(facet_by)) p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_by)))
  if (isTRUE(annotate_stats)) {
    cap <- if (is.null(facet_by)) stats_tbl$sentence else paste0(stats_tbl$panel, ": ", stats_tbl$sentence, collapse = "\n")
    p <- p + ggplot2::labs(caption = cap)
  }
  attr(p, "stats") <- stats_tbl
  p
}
