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
    scale_fill_temple("diverging", discrete = FALSE)
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
    ggplot2::geom_text(ggplot2::aes(label = .data[[column]]), hjust = 1, nudge_x = -0.02, color = temple_hex[["cherry"]]) +
    ggplot2::coord_fixed()
}

#' PCA Loadings Biplot
#'
#' A classic PCA biplot: the observation scores (\code{pca_model$x}) overlaid with
#' variable loading vectors (\code{pca_model$rotation}) drawn as labeled arrows from the origin.
#' Observations can optionally be colored by a categorical grouping variable with 95\%
#' concentration ellipses.
#'
#' Loading vectors are unit-scale by construction and would be invisible
#' next to the score cloud if plotted as-is, so they are rescaled so that
#' their maximum extent is 80\% of the score cloud's maximum extent (a
#' standard biplot convention) before being drawn.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @param x,y Which principal components to plot on the x/y axes (default 1, 2).
#' @param group Optional categorical vector (e.g. \code{iris$Species}) to color observations by.
#' @param ellipse Logical (default \code{FALSE}); if \code{TRUE} and \code{group} is provided,
#'   draws 95\% confidence/concentration ellipses around each group.
#' @param title Optional plot title (default \code{"PCA Biplot"}).
#' @param percent Logical (default \code{FALSE}); if \code{TRUE}, appends percent variance explained to axis labels.
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
#' pca_biplot(pca_model, group = iris$Species, ellipse = TRUE)
pca_biplot <- function(pca_model, x = 1, y = 2, group = NULL, ellipse = FALSE, title = "PCA Biplot", percent = FALSE) {
  pca_obj <- if (inherits(pca_model, "prcomp")) {
    pca_model
  } else if (!is.null(attr(pca_model, "prcomp"))) {
    attr(pca_model, "prcomp")
  } else {
    stop("`pca_model` must be a prcomp object or proc_pca output.", call. = FALSE)
  }

  rotation <- pca_obj$rotation
  n_components <- ncol(rotation)

  if (x > n_components || y > n_components) {
    stop("pca_biplot() requires components up to ", max(x, y), "; ",
         "pca_model only has ", n_components, ".")
  }

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  scores <- tibble::as_tibble(pca_obj$x)

  loadings <- tibble::as_tibble(rotation, rownames = "feature") |>
    dplyr::select("feature", dplyr::all_of(c(x_col, y_col)))

  score_extent <- max(abs(c(scores[[x_col]], scores[[y_col]])))
  loading_extent <- max(abs(c(loadings[[x_col]], loadings[[y_col]])))
  scale_factor <- (score_extent / loading_extent) * 0.8

  loadings <- loadings |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(x_col, y_col)), \(v) v * scale_factor))

  vars <- pca_obj$sdev^2
  pct_x <- round(vars[x] / sum(vars) * 100, 1)
  pct_y <- round(vars[y] / sum(vars) * 100, 1)
  xlab <- if (isTRUE(percent)) sprintf("PC%d (%s%%)", x, pct_x) else paste0("PC", x)
  ylab <- if (isTRUE(percent)) sprintf("PC%d (%s%%)", y, pct_y) else paste0("PC", y)

  p <- ggplot2::ggplot()

  if (!is.null(group)) {
    scores$.group <- as.factor(group)
    p <- p + ggplot2::geom_point(data = scores,
                                 ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data$.group),
                                 alpha = 0.7, size = 2.5) +
      scale_color_cbe() +
      ggplot2::labs(color = "Group")

    if (isTRUE(ellipse)) {
      p <- p + ggplot2::stat_ellipse(data = scores,
                                     ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data$.group),
                                     level = 0.95, type = "t", linetype = "dashed")
    }
  } else {
    p <- p + ggplot2::geom_point(data = scores,
                                 ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]),
                                 alpha = 0.5, color = "grey40")
  }

  p <- p +
    ggplot2::geom_segment(data = loadings,
                          ggplot2::aes(x = 0, y = 0, xend = .data[[x_col]], yend = .data[[y_col]]),
                          arrow = grid::arrow(angle = 20, type = "closed", length = grid::unit(8, "pt")),
                          color = temple_hex[["cherry"]], linewidth = 0.8) +
    ggplot2::geom_text(data = loadings,
                       ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data$feature),
                       color = temple_hex[["cherry"]], fontface = "bold", hjust = -0.1, vjust = -0.1) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    theme_cbe_deck() +
    ggplot2::coord_fixed()

  p
}

#' PCA Scree Plot
#'
#' Scree plot displaying the eigenvalues or proportion of variance explained by
#' each principal component, with an optional horizontal reference line at
#' eigenvalue = 1 (Kaiser-Guttman criterion).
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object or output from \code{\link{proc_pca}}.
#' @param metric Which metric to plot: \code{"eigenvalue"} (default, with Kaiser line at 1)
#'   or \code{"variance"} (percent of variance explained).
#' @param kaiser Logical (default \code{TRUE}); draw horizontal dashed line at eigenvalue = 1.
#' @param title Optional plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
#' pca_scree_plot(pca_model)
pca_scree_plot <- function(pca_model, metric = c("eigenvalue", "variance"), kaiser = TRUE, title = "PCA Scree Plot") {
  metric <- match.arg(metric)
  pca_obj <- if (inherits(pca_model, "prcomp")) {
    pca_model
  } else if (!is.null(attr(pca_model, "prcomp"))) {
    attr(pca_model, "prcomp")
  } else {
    stop("`pca_model` must be a prcomp object or output from proc_pca().", call. = FALSE)
  }

  vars <- pca_obj$sdev^2
  df <- tibble::tibble(
    PC = seq_along(vars),
    PC_label = factor(paste0("PC", seq_along(vars)), levels = paste0("PC", seq_along(vars))),
    eigenvalue = vars,
    variance_pct = vars / sum(vars) * 100
  )

  y_var <- if (metric == "eigenvalue") "eigenvalue" else "variance_pct"
  y_lab <- if (metric == "eigenvalue") "Eigenvalue" else "Variance Explained (%)"

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$PC, y = .data[[y_var]])) +
    ggplot2::geom_line(color = temple_hex[["cherry"]], linewidth = 1) +
    ggplot2::geom_point(color = temple_hex[["cherry"]], size = 3) +
    ggplot2::scale_x_continuous(breaks = df$PC, labels = df$PC_label) +
    theme_cbe_deck() +
    ggplot2::labs(x = "Principal Component", y = y_lab, title = title)

  if (metric == "eigenvalue" && isTRUE(kaiser)) {
    p <- p +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
      ggplot2::annotate("text", x = max(df$PC), y = 1.05, label = "Kaiser Criterion (Eigenvalue = 1)",
                        hjust = 1, vjust = 0, color = "grey40", size = 3.5)
  }
  p
}

#' PCA Variable Correlation Circle
#'
#' Plots the projection of variables onto the unit circle for two principal components,
#' showing correlations between original variables and principal components.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object or output from \code{\link{proc_pca}}.
#' @param x,y Which principal components to plot (default 1, 2).
#' @param title Optional plot title (default \code{"Variables Correlation Circle"}).
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
#' pca_variables_plot(pca_model)
pca_variables_plot <- function(pca_model, x = 1, y = 2, title = "Variables Correlation Circle") {
  pca_obj <- if (inherits(pca_model, "prcomp")) {
    pca_model
  } else if (!is.null(attr(pca_model, "prcomp"))) {
    attr(pca_model, "prcomp")
  } else {
    stop("`pca_model` must be a prcomp object or output from proc_pca().", call. = FALSE)
  }

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  # Correlation between variable j and PC k: loading_jk * sdev_k
  corrs <- as.data.frame(pca_obj$rotation %*% diag(pca_obj$sdev))
  colnames(corrs) <- paste0("PC", seq_len(ncol(corrs)))
  corrs$feature <- rownames(pca_obj$rotation)

  vars <- pca_obj$sdev^2
  pct_x <- round(vars[x] / sum(vars) * 100, 1)
  pct_y <- round(vars[y] / sum(vars) * 100, 1)

  # Unit circle coordinates
  theta <- seq(0, 2 * pi, length.out = 100)
  circle_df <- data.frame(x = cos(theta), y = sin(theta))

  ggplot2::ggplot() +
    ggplot2::geom_path(data = circle_df, ggplot2::aes(x = .data$x, y = .data$y),
                       color = "grey60", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey70") +
    ggplot2::geom_segment(data = corrs,
                          ggplot2::aes(x = 0, y = 0, xend = .data[[x_col]], yend = .data[[y_col]]),
                          arrow = grid::arrow(angle = 20, type = "closed", length = grid::unit(8, "pt")),
                          color = temple_hex[["cherry"]], linewidth = 0.8) +
    ggplot2::geom_text(data = corrs,
                       ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], label = .data$feature),
                       color = temple_hex[["cherry"]], fontface = "bold", hjust = -0.1, vjust = -0.1) +
    theme_cbe_deck() +
    ggplot2::labs(
      x = sprintf("PC%d (%s%%)", x, pct_x),
      y = sprintf("PC%d (%s%%)", y, pct_y),
      title = title
    ) +
    ggplot2::coord_fixed(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15))
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
    ggplot2::scale_fill_manual(values = c(cumulative = temple_hex[["diamond-acres"]], percent = temple_hex[["black"]])) +
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
    scale_fill_temple("diverging", discrete = FALSE) +
    ggplot2::labs(x = "Principal Component", y = "Feature", fill = "Loading\ndifference",
                  title = "PCA Loading Differences Between Fits")
}

#' Plot a PCA Fit
#'
#' One entry point to this package's PCA plots.
#'
#' This is an ordinary function rather than a `plot()` method: \pkg{stats}
#' already registers `plot()` for `prcomp` objects (a scree plot), and a
#' package that replaces another package's method for a class it doesn't own
#' changes `plot()` for everyone who loads it. Call `pca_plot(x, type = )`,
#' or [stats::screeplot()] for the base scree plot.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object. (Not `x`, which
#'   would capture the `x` component argument of `type = "bi"` and
#'   `"biplot"`.)
#' @param type One of \code{"variance"} (\code{\link{pca_percent_var_explained}}),
#'   \code{"heatmap"} (\code{\link{pca_feature_loading_heatmap}}),
#'   \code{"bi"} (\code{\link{plot_pca_bi}}), \code{"biplot"} (\code{\link{pca_biplot}}),
#'   \code{"scree"} (\code{\link{pca_scree_plot}}), or \code{"circle"} (\code{\link{pca_variables_plot}}).
#' @param ... Passed on to the underlying plot function (needed for
#'   \code{type = "bi"}, which requires \code{newdata} and \code{column}; and
#'   optionally used by \code{type = "bi"} or \code{"biplot"} to pick
#'   components with \code{x}/\code{y}).
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_plot(pca_model, type = "variance")
#' pca_plot(pca_model, type = "biplot", x = 1, y = 3)
pca_plot <- function(pca_model, type = c("variance", "heatmap", "bi", "biplot", "scree", "circle"), ...) {
  pca_obj <- if (inherits(pca_model, "prcomp")) {
    pca_model
  } else if (!is.null(attr(pca_model, "prcomp"))) {
    attr(pca_model, "prcomp")
  } else {
    stop("`pca_model` must be a prcomp object, from stats::prcomp().", call. = FALSE)
  }
  type <- match.arg(type)
  switch(type,
    variance = pca_percent_var_explained(pca_obj),
    heatmap = pca_feature_loading_heatmap(pca_obj),
    bi = plot_pca_bi(pca_obj, ...),
    biplot = pca_biplot(pca_obj, ...),
    scree = pca_scree_plot(pca_obj, ...),
    circle = pca_variables_plot(pca_obj, ...)
  )
}
