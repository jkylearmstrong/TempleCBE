#' Heatmap of Features by Transformation, Coloured by Significance
#'
#' After screening several transformations of many features (see [transform_longitudinal_features()] and
#' [batch_cox_single()]), shows every feature (rows) under every transformation (columns). Each tile is
#' coloured by the significance bucket of its p-value and shows the feature's rank within that
#' transformation.
#'
#' @section Buckets:
#' With `p_breaks = c(0.001, 0.01, 0.05, 0.20)` the buckets are `p < 0.001`, `p < 0.01`, `p < 0.05`,
#' `p < 0.2` and `NS` (p of 0.2 or more, or missing). A p-value goes in the first bucket whose break it is
#' strictly below. `palette` has one colour per bucket, most significant first.
#'
#' @section Ranks:
#' If `rank_col` is not a column of `results_df`, the rank is computed within each transformation by
#' ascending p-value (ties share the smaller rank; missing p-values are not ranked).
#'
#' @param results_df A data frame with one row per feature and transformation.
#' @param feature_col,transform_col,p_col,rank_col Column names. Set `rank_col = NULL` to print no rank.
#' @param p_breaks Increasing p-value cut-offs.
#' @param palette Colours, one more than `p_breaks`.
#' @param order_features `"input"` (default; order of first appearance) or `"best_p"` (smallest p over the
#'   transformations first).
#' @return A ggplot2 object.
#' @export
#' @examples
#' res <- expand.grid(feature = c("a", "b", "c"), transform_method = c("Raw", "SD"))
#' res$p.value <- c(0.0004, 0.03, 0.4, 0.2, 0.008, 0.06)
#' plot_feature_transform_heatmap(res)
plot_feature_transform_heatmap <- function(results_df,
                                           feature_col = "feature",
                                           transform_col = "transform_method",
                                           p_col = "p.value",
                                           rank_col = "rank",
                                           p_breaks = c(0.001, 0.01, 0.05, 0.20),
                                           palette = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
                                           order_features = c("input", "best_p")) {
  order_features <- match.arg(order_features)
  if (!is.data.frame(results_df)) stop("`results_df` must be a data frame.", call. = FALSE)
  miss <- setdiff(c(feature_col, transform_col, p_col), names(results_df))
  if (length(miss)) stop("`results_df` lacks column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(p_breaks) || anyNA(p_breaks) || is.unsorted(p_breaks, strictly = TRUE) ||
      any(p_breaks <= 0 | p_breaks >= 1)) {
    stop("`p_breaks` must be increasing numbers between 0 and 1.", call. = FALSE)
  }
  if (!is.character(palette) || length(palette) != length(p_breaks) + 1L) {
    stop("`palette` must have one more colour than `p_breaks`.", call. = FALSE)
  }
  df <- as.data.frame(results_df)
  df$.feature <- as.character(df[[feature_col]])
  df$.transform <- as.character(df[[transform_col]])
  if (anyDuplicated(df[c(".feature", ".transform")])) {
    stop("`results_df` has more than one row for a feature and transformation.", call. = FALSE)
  }
  p <- suppressWarnings(as.numeric(df[[p_col]]))
  labels <- c(paste0("p < ", format(p_breaks, scientific = FALSE)), paste0("NS (p ≥ ", format(p_breaks[length(p_breaks)], scientific = FALSE), ")"))
  idx <- ifelse(is.na(p), length(p_breaks) + 1L, vapply(p, function(x) if (is.na(x)) NA_integer_ else sum(p_breaks <= x) + 1L, integer(1)))
  df$.bucket <- factor(labels[idx], levels = labels)

  show_rank <- !is.null(rank_col)
  if (show_rank) {
    if (rank_col %in% names(df)) {
      df$.rank <- df[[rank_col]]
    } else {
      df$.rank <- NA_integer_
      for (tr in unique(df$.transform)) {
        sel <- df$.transform == tr & !is.na(p)
        df$.rank[sel] <- rank(p[sel], ties.method = "min")
      }
    }
  }
  feat_levels <- unique(df$.feature)
  if (order_features == "best_p") {
    best <- vapply(feat_levels, function(f) suppressWarnings(min(p[df$.feature == f], na.rm = TRUE)), numeric(1))
    feat_levels <- feat_levels[order(best)]
  }
  df$.feature <- factor(df$.feature, levels = rev(feat_levels))
  df$.transform <- factor(df$.transform, levels = unique(as.character(df[[transform_col]])))
  df$.label <- if (show_rank) ifelse(is.na(df$.rank), "", as.character(df$.rank)) else ""

  ggplot2::ggplot(df, ggplot2::aes(x = .data$.transform, y = .data$.feature, fill = .data$.bucket)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$.label), size = 3.2) +
    ggplot2::scale_fill_manual(values = stats::setNames(palette, labels), drop = FALSE, name = "Significance") +
    theme_cbe() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = NULL, y = NULL, caption = if (show_rank) "Number in tile: rank of the feature within the transformation (1 = smallest p)" else NULL)
}
