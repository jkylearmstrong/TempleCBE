#' Forest Plot Contrasting the Raw and the Normalized Effect Scale
#'
#' [glm_single()] and [lmm_single()] fit every predictor twice: `scale = "raw"` (change per **one unit**,
#' for example per mm or per kPa) and `scale = "normalized"` (change across the predictor's **whole
#' observed range**). This plot puts the two next to each other, one row per predictor.
#' * `layout = "side_by_side"`: two forest panels with their own axes (raw units on the left, normalized
#'   on the right), same row order, so a predictor that looks strong in one scale and weak in the other
#'   is easy to spot.
#' * `layout = "ratio"`: one point per predictor, normalized estimate divided by raw estimate. For a
#'   linear effect this is the observed range of the predictor in its own unit, which shows
#'   how much room each unit-effect has to act. Estimates near zero make the ratio unstable; they are
#'   marked when `abs(raw) < ratio_floor`.
#'
#' @param results_df Results of [glm_single()] / [lmm_single()] (several can be stacked), with the columns
#'   `scale`, `estimate`, `conf.low`, `conf.high`.
#' @param term_col Column that labels the rows (default `"var_label"`).
#' @param layout `"side_by_side"` (default) or `"ratio"`.
#' @param ratio_floor Absolute raw estimate below which a ratio is marked unreliable (default `1e-8`).
#' @param x_label Optional axis label for the side-by-side layout.
#' @return A ggplot. Rows whose fit failed (`NA` estimate) are dropped. Its `data` is the wide table
#'   (one row per predictor) for `"ratio"` and the long table for `"side_by_side"`.
#' @seealso [plot_coef_forest()], [glm_single()], [lmm_single()]
#' @export
#' @examples
#' res <- rbind(glm_single(mtcars, "mpg", "wt"), glm_single(mtcars, "mpg", "hp"))
#' plot_marginal_effect_contrast(res)
#' plot_marginal_effect_contrast(res, layout = "ratio")
plot_marginal_effect_contrast <- function(results_df,
                                          term_col = "var_label",
                                          layout = c("side_by_side", "ratio"),
                                          ratio_floor = 1e-8,
                                          x_label = NULL) {
  layout <- match.arg(layout)
  need <- c(term_col, "scale", "estimate", "conf.low", "conf.high")
  if (!is.data.frame(results_df)) stop("`results_df` must be a data frame.", call. = FALSE)
  gone <- setdiff(need, names(results_df))
  if (length(gone)) stop("Column(s) not in `results_df`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  df <- as.data.frame(results_df)
  df <- df[!is.na(df$estimate) & df$scale %in% c("raw", "normalized"), , drop = FALSE]
  if (nrow(df) == 0L) stop("No raw/normalized estimate to plot.", call. = FALSE)
  df$label <- as.character(df[[term_col]])
  # a categorical predictor has several terms: keep them apart
  if ("term" %in% names(df)) {
    multi <- stats::ave(seq_len(nrow(df)), df$label, df$scale, FUN = length) > 1L
    df$label[multi] <- paste0(df$label[multi], ": ", df$term[multi])
  }
  df$scale <- factor(df$scale, levels = c("raw", "normalized"), labels = c("Raw units (per 1 unit)", "Normalized (full observed range)"))
  both <- Reduce(intersect, split(df$label, df$scale))
  lv <- unique(df$label)

  if (layout == "side_by_side") {
    df$label <- factor(df$label, levels = rev(lv))
    return(
      ggplot2::ggplot(df, ggplot2::aes(.data$estimate, .data$label)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high), width = 0.2, orientation = "y") +
        ggplot2::geom_point(size = 2, colour = "#A41E35") +
        ggplot2::facet_wrap(ggplot2::vars(.data$scale), scales = "free_x") +
        ggplot2::labs(x = if (is.null(x_label)) "Estimate with confidence interval" else x_label, y = NULL) +
        ggplot2::theme_minimal()
    )
  }

  raw <- df[df$scale == levels(df$scale)[1L], c("label", "estimate")]
  nrm <- df[df$scale == levels(df$scale)[2L], c("label", "estimate")]
  names(raw)[2L] <- "raw"
  names(nrm)[2L] <- "normalized"
  wide <- merge(raw, nrm, by = "label")
  if (nrow(wide) == 0L) stop("No predictor has both a raw and a normalized estimate.", call. = FALSE)
  wide <- wide[match(intersect(lv, wide$label), wide$label), , drop = FALSE]
  wide$unreliable <- abs(wide$raw) < ratio_floor
  wide$ratio <- ifelse(wide$unreliable, NA_real_, wide$normalized / wide$raw)
  wide$label <- factor(wide$label, levels = rev(wide$label))
  ggplot2::ggplot(wide, ggplot2::aes(.data$ratio, .data$label)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = 2.5, colour = "#A41E35", na.rm = TRUE) +
    ggplot2::labs(x = "Normalized estimate / raw estimate (observed range of the predictor)", y = NULL,
                  caption = if (any(wide$unreliable)) "Predictors with a raw estimate near zero are left out." else NULL) +
    ggplot2::theme_minimal()
}
