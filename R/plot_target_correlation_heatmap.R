#' Correlation of One Target With Many Candidate Predictors
#'
#' A one-column tile heatmap for screening predictors of a single continuous target: each tile is the
#' correlation of one predictor with the target, with the number printed on it. Tiles with
#' `abs(r) < cutoff` are drawn faded, so the predictors worth a closer look stand out. For a full
#' predictor-by-predictor matrix use [correlation_plot()].
#'
#' @param data A data frame.
#' @param target Name of the numeric target column.
#' @param predictors Character vector of numeric predictor columns (the target itself is ignored if listed).
#' @param cutoff Absolute correlation below which a tile is faded (default 0.3; `0` fades nothing).
#' @param sort_by_abs If `TRUE` (default) the strongest predictor is on top; otherwise the order of `predictors`.
#' @param palette A diverging ColorBrewer palette name (default `"RdBu"`); the scale is fixed to -1..1.
#' @param method `"pearson"` (default), `"spearman"` or `"kendall"`.
#' @param digits Decimals printed on the tiles.
#' @return A ggplot2 object. Its `data` holds `predictor`, `r`, `n` (complete pairs) and `faded`.
#'   A predictor with fewer than 3 complete pairs or no variation has `r = NA` and is drawn blank.
#' @seealso [correlation_plot()]
#' @export
#' @examples
#' plot_target_correlation_heatmap(mtcars, "mpg", c("cyl", "disp", "hp", "wt", "qsec"))
plot_target_correlation_heatmap <- function(data,
                                            target,
                                            predictors,
                                            cutoff = 0.3,
                                            sort_by_abs = TRUE,
                                            palette = "RdBu",
                                            method = c("pearson", "spearman", "kendall"),
                                            digits = 2) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(target) || length(target) != 1L || !target %in% names(data)) {
    stop("`target` must name one column of `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[target]])) stop("`target` must be numeric.", call. = FALSE)
  predictors <- setdiff(as.character(predictors), target)
  if (length(predictors) == 0L) stop("`predictors` must name at least one column other than `target`.", call. = FALSE)
  gone <- setdiff(predictors, names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  bad <- predictors[!vapply(data[predictors], is.numeric, logical(1))]
  if (length(bad)) stop("Predictor(s) must be numeric: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(cutoff) || length(cutoff) != 1L || is.na(cutoff) || cutoff < 0 || cutoff > 1) {
    stop("`cutoff` must be a number between 0 and 1.", call. = FALSE)
  }

  rows <- lapply(predictors, function(p) {
    ok <- stats::complete.cases(data[[target]], data[[p]])
    n <- sum(ok)
    r <- if (n >= 3L && stats::sd(data[[target]][ok]) > 0 && stats::sd(data[[p]][ok]) > 0) {
      suppressWarnings(stats::cor(data[[target]][ok], data[[p]][ok], method = method))
    } else {
      NA_real_
    }
    tibble::tibble(predictor = p, r = r, n = n)
  })
  df <- dplyr::bind_rows(rows)
  df$faded <- !is.na(df$r) & abs(df$r) < cutoff
  ord <- if (isTRUE(sort_by_abs)) order(-abs(replace(df$r, is.na(df$r), -1)), seq_len(nrow(df))) else seq_len(nrow(df))
  df <- df[ord, , drop = FALSE]
  df$predictor <- factor(df$predictor, levels = rev(df$predictor))
  df$.x <- target
  df$.label <- ifelse(is.na(df$r), "", formatC(df$r, format = "f", digits = digits))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$predictor, fill = .data$r, alpha = !.data$faded)) +
    ggplot2::geom_tile(color = "white", width = 0.9, height = 0.95) +
    ggplot2::geom_text(ggplot2::aes(label = .data$.label), size = 3.5, show.legend = FALSE) +
    ggplot2::scale_fill_distiller(palette = palette, limits = c(-1, 1), direction = 1, na.value = "grey95",
                                  name = paste0(tools::toTitleCase(method), " r")) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    theme_cbe() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = if (cutoff > 0) sprintf("Faded: |r| < %s", format(cutoff)) else NULL)
}
