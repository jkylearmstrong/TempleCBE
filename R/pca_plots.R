#' PCA Feature-Loading Heatmap
#'
#' Heatmap of each original feature's loading onto each principal component.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_feature_loading_heatmap(pca_model)
pca_feature_loading_heatmap <- function(pca_model) {
  rot <- pca_model$rotation
  long <- tibble::as_tibble(rot, rownames = "feature") |>
    tidyr::pivot_longer(-"feature", names_to = "PC", values_to = "value") |>
    dplyr::mutate(PC = as.numeric(sub("^PC", "", .data$PC)))

  num_comp <- ncol(rot)

  ggplot2::ggplot(long, ggplot2::aes(x = .data$PC, y = .data$feature, fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(breaks = seq_len(num_comp)) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red")
}

#' PCA Biplot
#'
#' Biplot of feature loading vectors against two principal components.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @param newdata Data to project onto \code{pca_model}.
#' @param column Column in \code{newdata} to use as point labels.
#' @param x,y Which principal components to plot on the x/y axes (default 1, 2).
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' mtcars2 <- tibble::rownames_to_column(mtcars, "model")
#' plot_pca_bi(pca_model, mtcars2, column = "model")
plot_pca_bi <- function(pca_model, newdata, column, x = 1, y = 2) {
  if (!column %in% colnames(newdata)) {
    message("Column '", column, "' not found in newdata; using the first column instead.")
    column <- colnames(newdata)[1]
  }

  projection <- stats::predict(pca_model, newdata)
  n_components <- ncol(projection)

  if (n_components < 2) {
    stop("plot_pca_bi() requires at least 2 principal components to plot; ",
         "pca_model only has ", n_components, ".")
  }

  if (x > n_components) { message("x exceeds available components; using 1."); x <- 1 }
  if (y > n_components) { message("y exceeds available components; using 1."); y <- 1 }
  if (x == y) {
    y <- if (x == n_components) 1 else x + 1
    message("x and y were equal; using y = ", y, ".")
  }

  plot_df <- dplyr::bind_cols(
    tibble::as_tibble(projection),
    dplyr::select(newdata, dplyr::all_of(column))
  )

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_segment(xend = 0, yend = 0,
                           arrow = grid::arrow(angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))) +
    ggplot2::geom_text(ggplot2::aes(label = .data[[column]]), hjust = 1, nudge_x = -0.02, color = "#904C2F") +
    ggplot2::coord_fixed()
}

#' PCA Loadings Biplot
#'
#' A classic PCA biplot: the observation scores (\code{pca_model$x}) as a
#' muted point cloud, overlaid with the variable loading vectors
#' (\code{pca_model$rotation}) drawn as labeled arrows from the origin.
#' Unlike \code{\link{plot_pca_bi}}, no separate \code{newdata} is required --
#' a fitted \code{\link[stats]{prcomp}} object already carries both the
#' scores and the loadings needed to draw the biplot.
#'
#' Loading vectors are unit-scale by construction and would be invisible
#' next to the score cloud if plotted as-is, so they are rescaled so that
#' their maximum extent is 80% of the score cloud's maximum extent (a
#' standard biplot convention) before being drawn.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @param x,y Which principal components to plot on the x/y axes (default 1, 2).
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_biplot(pca_model, x = 1, y = 2)
pca_biplot <- function(pca_model, x = 1, y = 2) {
  rotation <- pca_model$rotation
  n_components <- ncol(rotation)

  if (x > n_components || y > n_components) {
    stop("pca_biplot() requires components up to ", max(x, y), "; ",
         "pca_model only has ", n_components, ".")
  }

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  scores <- tibble::as_tibble(pca_model$x)

  loadings <- tibble::as_tibble(rotation, rownames = "feature") |>
    dplyr::select("feature", dplyr::all_of(c(x_col, y_col)))

  score_extent <- max(abs(c(scores[[x_col]], scores[[y_col]])))
  loading_extent <- max(abs(c(loadings[[x_col]], loadings[[y_col]])))
  scale_factor <- (score_extent / loading_extent) * 0.8

  loadings <- loadings |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(x_col, y_col)), \(v) v * scale_factor))

  ggplot2::ggplot() +
    ggplot2::geom_point(data = scores, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]),
                         alpha = 0.5, color = "grey40") +
    ggplot2::geom_segment(data = loadings,
                           ggplot2::aes(x = 0, y = 0, xend = .data[[x_col]], yend = .data[[y_col]]),
                           arrow = grid::arrow(angle = 20, type = "closed", length = grid::unit(8, "pt")),
                           color = "#904C2F") +
    ggplot2::geom_text(data = loadings,
                        ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data$feature),
                        color = "#904C2F", hjust = -0.1, vjust = -0.1) +
    ggplot2::labs(x = paste0("PC", x), y = paste0("PC", y), title = "PCA Biplot") +
    ggplot2::coord_fixed()
}

#' PCA Rotation Matrix (Loadings)
#'
#' @param PC_mod A \code{\link[stats]{prcomp}} object.
#' @return A tibble of feature loadings onto each principal component, with
#'   a \code{feature_num} column (\code{"f1"}, \code{"f2"}, ...) for compact
#'   labeling in \code{\link{pca_eqns}}.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' rotation_matrix(pca_model)
rotation_matrix <- function(PC_mod) {
  tibble::as_tibble(PC_mod$rotation, rownames = "feature") |>
    dplyr::mutate(feature_num = paste0("f", dplyr::row_number()))
}

#' @rdname rotation_matrix
#' @export
pca_loadings <- rotation_matrix

#' PCA Equations
#'
#' Writes out each principal component as a linear equation in the original
#' features (abbreviated \code{f1}, \code{f2}, ... — see the returned
#' \code{labels} table for what each abbreviation means).
#'
#' @param PC_mod A \code{\link[stats]{prcomp}} object.
#' @param precision Digits to round loadings to (default 3).
#' @return A list with two tibbles: \code{eqns} (one row per component, with
#'   its equation as text) and \code{labels} (feature-number-to-name key).
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_eqns(pca_model)$eqns
#' pca_eqns(pca_model)$labels
pca_eqns <- function(PC_mod, precision = 3) {
  pc_pc <- pca_loadings(PC_mod)
  pc_cols <- names(dplyr::select(pc_pc, dplyr::starts_with("PC")))

  eqns <- pc_pc |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) round(x, precision))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) ifelse(x >= 0, paste0("+", x), paste0(x)))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) paste0(x, "*(", .data$feature_num, ")"))) |>
    dplyr::select(-"feature", -"feature_num") |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "PC", values_to = "fct") |>
    dplyr::mutate(PC_num = as.numeric(sub("^PC", "", .data$PC))) |>
    dplyr::group_by(.data$PC_num, .data$PC) |>
    dplyr::summarise(rhs = paste(.data$fct, collapse = " "), .groups = "drop") |>
    dplyr::arrange(.data$PC_num) |>
    dplyr::mutate(PC = paste0(.data$PC, "= ")) |>
    dplyr::select("PC", "rhs")

  labels <- pc_pc |>
    dplyr::mutate(fi = paste0(.data$feature_num, " = ")) |>
    dplyr::select("fi", "feature")

  list(eqns = eqns, labels = labels)
}

#' Percent Variance Explained by Each Principal Component
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @return A ggplot object showing per-component and cumulative variance explained.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_percent_var_explained(pca_model)
pca_percent_var_explained <- function(pca_model) {
  eig <- broom::tidy(pca_model, matrix = "eigenvalues")
  n_comp <- max(eig$PC)

  eig |>
    dplyr::select("PC", "percent", "cumulative") |>
    tidyr::pivot_longer(cols = c("percent", "cumulative"), names_to = "variance", values_to = "percent") |>
    dplyr::mutate(variance = factor(.data$variance, levels = c("cumulative", "percent"))) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC, y = .data$percent, fill = .data$variance, alpha = .data$variance)) +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::scale_fill_manual(values = c(cumulative = "#56B4E9", percent = "black")) +
    ggplot2::scale_x_continuous(breaks = seq_len(n_comp)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format(),
                                 expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::scale_alpha_manual(values = c(.75, 1)) +
    ggplot2::labs(x = "Principal Component", y = "Percent of Variance Explained",
                  title = "Variance Explained by Principal Component")
}

#' Difference in PCA Loadings Between Two Fits
#'
#' Compares the variable loadings of two independently-fit
#' \code{\link[stats]{prcomp}} objects on the same set of variables (e.g. the
#' same domain's data fit at baseline vs. at a later timepoint), matching
#' components positionally (both fits' PC1, both fits' PC2, ...).
#'
#' PCA loading vectors are only unique up to sign: a component can flip
#' orientation between two otherwise-equivalent fits without changing the
#' pattern it represents. Naively differencing loadings would then show a
#' spuriously large change (up to roughly double the loading) for a
#' component that hasn't meaningfully changed at all. To avoid this, for
#' each shared component \code{pca_comparison}'s loading vector is sign-
#' aligned to \code{pca_baseline}'s: it is flipped (multiplied by -1) if
#' doing so reduces the total absolute difference across variables relative
#' to leaving it as-is. The difference is then computed as
#' (sign-aligned comparison) minus baseline.
#'
#' Variables are matched by name (the rownames of \code{$rotation}). If the
#' two fits were built on different variable sets, only the intersection is
#' used; no error is raised.
#'
#' @param pca_baseline A \code{\link[stats]{prcomp}} object treated as the reference.
#' @param pca_comparison A \code{\link[stats]{prcomp}} object to compare against
#'   \code{pca_baseline}, fit on the same (or overlapping) variables.
#' @param n_components Number of leading components to compare. Defaults to
#'   \code{NULL}, meaning all components shared by both fits.
#' @return A tibble with one row per shared variable (\code{feature} column)
#'   and one column per compared component (\code{PC1}, \code{PC2}, ...)
#'   holding the sign-aligned difference (comparison minus baseline).
#' @export
#' @examples
#' set.seed(1)
#' baseline <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' comparison <- prcomp(mtcars[sample(nrow(mtcars)), ], center = TRUE, scale. = TRUE)
#' pca_loading_diff(baseline, comparison)
pca_loading_diff <- function(pca_baseline, pca_comparison, n_components = NULL) {
  rot_baseline <- pca_baseline$rotation
  rot_comparison <- pca_comparison$rotation

  shared_features <- intersect(rownames(rot_baseline), rownames(rot_comparison))
  rot_baseline <- rot_baseline[shared_features, , drop = FALSE]
  rot_comparison <- rot_comparison[shared_features, , drop = FALSE]

  n_shared <- min(ncol(rot_baseline), ncol(rot_comparison))
  if (!is.null(n_components)) {
    n_shared <- min(n_shared, n_components)
  }

  diff_mat <- vapply(seq_len(n_shared), function(i) {
    baseline_vec <- rot_baseline[, i]
    comparison_vec <- rot_comparison[, i]

    # Loading vectors are only unique up to sign; flip `comparison_vec` if
    # doing so reduces the total absolute difference from `baseline_vec`.
    if (sum(abs(-comparison_vec - baseline_vec)) < sum(abs(comparison_vec - baseline_vec))) {
      comparison_vec <- -comparison_vec
    }

    comparison_vec - baseline_vec
  }, FUN.VALUE = numeric(length(shared_features)))

  colnames(diff_mat) <- paste0("PC", seq_len(n_shared))

  tibble::as_tibble(diff_mat) |>
    dplyr::mutate(feature = shared_features, .before = 1)
}

#' Heatmap of PCA Loading Differences Between Two Fits
#'
#' Renders the output of \code{\link{pca_loading_diff}} as a heatmap (feature
#' by component), using the same visual language as
#' \code{\link{pca_feature_loading_heatmap}}: a diverging fill scale centered
#' at zero, so components/variables with little sign-aligned change are
#' white and larger changes in either direction stand out in blue or red.
#'
#' @inheritParams pca_loading_diff
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(1)
#' baseline <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' comparison <- prcomp(mtcars[sample(nrow(mtcars)), ], center = TRUE, scale. = TRUE)
#' pca_loading_diff_heatmap(baseline, comparison)
pca_loading_diff_heatmap <- function(pca_baseline, pca_comparison, n_components = NULL) {
  diff_df <- pca_loading_diff(pca_baseline, pca_comparison, n_components)

  long <- diff_df |>
    tidyr::pivot_longer(-"feature", names_to = "PC", values_to = "value") |>
    dplyr::mutate(PC = as.numeric(sub("^PC", "", .data$PC)))

  num_comp <- max(long$PC)

  ggplot2::ggplot(long, ggplot2::aes(x = .data$PC, y = .data$feature, fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(breaks = seq_len(num_comp)) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    ggplot2::labs(x = "Principal Component", y = "Feature", fill = "Loading\ndifference",
                  title = "PCA Loading Differences Between Fits")
}

#' Generic Plot Method for \code{prcomp} Objects
#'
#' @param x A \code{\link[stats]{prcomp}} object.
#' @param type One of \code{"variance"} (\code{\link{pca_percent_var_explained}}),
#'   \code{"heatmap"} (\code{\link{pca_feature_loading_heatmap}}),
#'   \code{"bi"} (\code{\link{plot_pca_bi}}), or \code{"biplot"}
#'   (\code{\link{pca_biplot}}).
#' @param ... Passed on to the underlying plot function (needed for
#'   \code{type = "bi"}, which requires \code{newdata} and \code{column}; and
#'   optionally used by \code{type = "biplot"} to pass \code{x}/\code{y}).
#' @return A ggplot object.
#' @exportS3Method base::plot
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' plot(pca_model, type = "variance")
plot.prcomp <- function(x, type = c("variance", "heatmap", "bi", "biplot"), ...) {
  type <- match.arg(type)
  switch(type,
    variance = pca_percent_var_explained(x),
    heatmap = pca_feature_loading_heatmap(x),
    bi = plot_pca_bi(x, ...),
    biplot = pca_biplot(x, ...)
  )
}
