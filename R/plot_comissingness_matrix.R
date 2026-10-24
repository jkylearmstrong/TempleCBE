#' Co-Missingness Correlation Matrix
#'
#' Heatmap of the correlation between the missingness indicators of columns (1 = missing, 0 = observed).
#' Values near 1 mean two columns are missing together (an instrument or a line that failed and took
#' several measurements with it); values near 0 are consistent with unrelated missingness. With
#' `cluster = TRUE` the columns are ordered by hierarchical clustering of `1 - correlation`.
#'
#' Columns with no missing value, or only missing values, have no variation in their indicator and
#' are left out (listed in the attribute `"dropped"` of the result).
#'
#' @param data A data frame.
#' @param vars Columns to use (default all).
#' @param cluster Order the columns by hierarchical clustering (default `TRUE`).
#' @param k Optional number of clusters to cut the tree into; adds a `cluster` column to the plot data.
#' @param palette A sequential ColorBrewer palette name (default `"Reds"`); the scale runs 0 to 1
#'   (negative correlations are drawn as 0).
#' @param label Print the correlation on the tiles (default `TRUE` up to 15 columns).
#' @return A ggplot; its `data` holds `var1`, `var2`, `r` (and `cluster` for `var1` when `k` is given).
#' @seealso [missmap()], [features_percent_miss()]
#' @export
#' @examples
#' d <- data.frame(a = c(1, NA, 3, NA, 5, 6), b = c(1, NA, 3, NA, 5, NA), c = c(NA, 2, 3, 4, 5, 6))
#' plot_comissingness_matrix(d)
plot_comissingness_matrix <- function(data,
                                      vars = NULL,
                                      cluster = TRUE,
                                      k = NULL,
                                      palette = "Reds",
                                      label = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (is.null(vars)) vars <- names(data)
  gone <- setdiff(vars, names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  M <- vapply(data[vars], function(x) as.numeric(is.na(x)), numeric(nrow(data)))
  if (!is.matrix(M)) M <- matrix(M, nrow = nrow(data), dimnames = list(NULL, vars))
  varies <- apply(M, 2, function(x) length(unique(x)) > 1L)
  dropped <- vars[!varies]
  if (sum(varies) < 2L) {
    stop("Fewer than two columns have both missing and observed values; there is no co-missingness to show.", call. = FALSE)
  }
  M <- M[, varies, drop = FALSE]
  R <- stats::cor(M)
  ord <- seq_len(ncol(R))
  groups <- NULL
  if (isTRUE(cluster) && ncol(R) > 2L) {
    hc <- stats::hclust(stats::as.dist(1 - R), method = "average")
    ord <- hc$order
    if (!is.null(k)) groups <- stats::cutree(hc, k = min(k, ncol(R)))
  } else if (!is.null(k)) {
    groups <- stats::cutree(stats::hclust(stats::as.dist(1 - R), method = "average"), k = min(k, ncol(R)))
  }
  nm <- colnames(R)[ord]
  df <- expand.grid(var1 = nm, var2 = nm, stringsAsFactors = FALSE)
  df$r <- R[cbind(match(df$var1, colnames(R)), match(df$var2, colnames(R)))]
  if (!is.null(groups)) df$cluster <- unname(groups[df$var1])
  df$var1 <- factor(df$var1, levels = nm)
  df$var2 <- factor(df$var2, levels = rev(nm))
  if (is.null(label)) label <- length(nm) <= 15L

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$var1, .data$var2, fill = .data$r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_distiller(palette = palette, direction = 1, limits = c(0, 1), oob = scales::squish,
                                  name = "Co-missingness\ncorrelation") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), panel.grid = ggplot2::element_blank())
  if (isTRUE(label)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$r)), size = 3)
  }
  attr(p, "dropped") <- dropped
  p
}
