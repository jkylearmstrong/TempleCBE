#' Compare an Original and an Imputed Version of One Domain's Dataset
#'
#' Bundles the imputation diagnostics for one set of related variables (a
#' "domain") into a list of plain R objects, so a report can loop over the
#' domains and show or assemble the pieces itself. Nothing is rendered.
#'
#' The two data frames must describe the same rows in the same order: `imp_df`
#' is `orig_df` after imputation. When `id_col` is given and present in both,
#' the ids (and `time_col`, if given) must agree row by row, and the function
#' stops otherwise.
#'
#' The pieces are:
#' * **Distributions**: the imputed values against the observed ones, with
#'   [compare_imputed_ecdf()] and [test_imputed_distributions()] (Kolmogorov-Smirnov
#'   and t tests). Only cells that were missing in `orig_df` count as imputed.
#' * **Correlations**: [correlation_diff()] and [correlation_diff_heatmap()],
#'   comparison (`imp_df`) minus baseline (`orig_df`).
#' * **PCA loadings**: with `pca = TRUE`, a [stats::prcomp()] fit (centred and
#'   scaled) to the complete cases of `vars` in each data frame, then
#'   [pca_loading_diff_heatmap()]. `NULL`, with a warning, if there are fewer
#'   than two variables, fewer complete cases than variables plus one, or a
#'   variable without variance in either fit.
#' * **Trajectories**, when `id_col` and `time_col` are both given: the mean and
#'   standard error of each variable at each time point in each data frame, the
#'   difference of the means, and a paired comparison by `id_col` within each
#'   time point (`test`).
#'
#' The paired comparison uses the rows that have a value in both data frames, so
#' for an imputation that leaves the observed values unchanged (the usual case)
#' every paired difference is zero and `p_value` is `NA`; `delta` still shows
#' the shift in the mean that the imputed values cause. The `p_value`s are
#' point-wise, one per variable and time point; `p_adj` adjusts them with
#' Holm's step-down method over all of them.
#'
#' @param orig_df Data frame before imputation.
#' @param imp_df Data frame after imputation: the same rows as `orig_df`, in the
#'   same order.
#' @param vars Character vector of numeric columns to compare. `NULL` (the
#'   default) uses the numeric columns present in both data frames, without
#'   `id_col` and `time_col`.
#' @param id_col,time_col Optional names of the subject id and time columns.
#'   Both are needed for the trajectory pieces.
#' @param pca Logical; also compare PCA loadings?
#' @param test Paired test for the trajectory comparison: `"t"`
#'   ([stats::t.test()]) or `"wilcoxon"` ([stats::wilcox.test()]).
#' @return A list with elements `ecdf_plots` (ggplot, or `NULL` if `orig_df` has
#'   no missing value among `vars`), `ks_tests` (tibble), `corr_diff_table`
#'   (tibble), `corr_diff_heatmap` (ggplot, or `NULL` with fewer than two
#'   `vars`), `pca_loading_diff` (ggplot or `NULL`), `trajectory_plot` (ggplot or
#'   `NULL`) and `trajectory_deltas` (tibble with `variable`, `time`, `mean_orig`,
#'   `mean_imp`, `delta`, `n_paired`, `p_value`, `p_adj`, or `NULL`).
#' @seealso [compare_imputed_ecdf()], [test_imputed_distributions()],
#'   [correlation_diff()], [pca_loading_diff_heatmap()]
#' @export
#' @examples
#' set.seed(1)
#' d <- expand.grid(id = 1:30, time = 1:3)
#' d$hr <- rnorm(nrow(d), 80, 10)
#' d$map <- rnorm(nrow(d), 90, 8)
#' orig <- d
#' orig$hr[sample(nrow(d), 15)] <- NA
#' imp <- orig
#' imp$hr[is.na(imp$hr)] <- mean(imp$hr, na.rm = TRUE)
#' out <- compare_domain_datasets(orig, imp, id_col = "id", time_col = "time")
#' out$ks_tests
#' out$trajectory_deltas
compare_domain_datasets <- function(orig_df,
                                    imp_df,
                                    vars = NULL,
                                    id_col = NULL,
                                    time_col = NULL,
                                    pca = TRUE,
                                    test = c("t", "wilcoxon")) {
  test <- match.arg(test)
  if (!is.data.frame(orig_df) || !is.data.frame(imp_df)) {
    stop("`orig_df` and `imp_df` must be data frames.", call. = FALSE)
  }
  if (nrow(orig_df) != nrow(imp_df)) {
    stop("`orig_df` and `imp_df` must have the same rows in the same order (",
         nrow(orig_df), " and ", nrow(imp_df), " rows).", call. = FALSE)
  }
  for (arg in c("id_col", "time_col")) {
    val <- get(arg)
    if (!is.null(val) && (!is.character(val) || length(val) != 1L || is.na(val))) {
      stop("`", arg, "` must be NULL or a single column name.", call. = FALSE)
    }
  }
  if (!is.logical(pca) || length(pca) != 1L || is.na(pca)) {
    stop("`pca` must be TRUE or FALSE.", call. = FALSE)
  }

  # rows must correspond: ids (and times) agree wherever both data frames have the column
  for (col in c(id_col, time_col)) {
    if (col %in% names(orig_df) && col %in% names(imp_df) &&
        !identical(as.character(orig_df[[col]]), as.character(imp_df[[col]]))) {
      stop("`", col, "` differs between `orig_df` and `imp_df`: the rows must correspond.",
           call. = FALSE)
    }
  }

  if (is.null(vars)) {
    num_orig <- names(orig_df)[vapply(orig_df, is.numeric, logical(1))]
    num_imp <- names(imp_df)[vapply(imp_df, is.numeric, logical(1))]
    vars <- setdiff(intersect(num_orig, num_imp), c(id_col, time_col))
  } else {
    if (!is.character(vars) || anyNA(vars)) {
      stop("`vars` must be NULL or a character vector of column names.", call. = FALSE)
    }
    vars <- unique(vars)
    absent <- vars[!(vars %in% names(orig_df) & vars %in% names(imp_df))]
    if (length(absent)) {
      stop("`vars` not in both data frames: ", paste(absent, collapse = ", "), call. = FALSE)
    }
    non_num <- vars[!vapply(vars, function(v) is.numeric(orig_df[[v]]) && is.numeric(imp_df[[v]]), logical(1))]
    if (length(non_num)) {
      stop("`vars` must be numeric: ", paste(non_num, collapse = ", "), call. = FALSE)
    }
  }
  if (!length(vars)) {
    stop("No numeric columns to compare.", call. = FALSE)
  }

  any_missing <- any(vapply(vars, function(v) anyNA(orig_df[[v]]), logical(1)))
  ecdf_plots <- if (any_missing) {
    compare_imputed_ecdf(orig_df, imp_df, cols = vars)
  } else {
    message("`orig_df` has no missing value among `vars`: nothing was imputed, no ECDF plot.")
    NULL
  }

  ks_tests <- test_imputed_distributions(orig_df, imp_df, cols = vars)

  have_pairs <- length(vars) >= 2L
  corr_diff_table <- if (have_pairs) {
    correlation_diff(orig_df[, vars, drop = FALSE], imp_df[, vars, drop = FALSE])
  } else {
    tibble::tibble(var1 = character(), var2 = character(), diff = numeric())
  }
  corr_diff_heatmap <- if (have_pairs) {
    correlation_diff_heatmap(orig_df[, vars, drop = FALSE], imp_df[, vars, drop = FALSE])
  } else {
    NULL
  }

  pca_loading_diff <- if (isTRUE(pca)) .domain_pca_diff(orig_df, imp_df, vars) else NULL

  traj <- NULL
  if (!is.null(id_col) && !is.null(time_col)) {
    needed <- c(id_col, time_col)
    if (!all(needed %in% names(orig_df)) || !all(needed %in% names(imp_df))) {
      stop("`id_col` and `time_col` must be columns of both data frames.", call. = FALSE)
    }
    traj <- .domain_trajectories(orig_df, imp_df, vars, id_col, time_col, test)
  }

  list(
    ecdf_plots = ecdf_plots,
    ks_tests = ks_tests,
    corr_diff_table = corr_diff_table,
    corr_diff_heatmap = corr_diff_heatmap,
    pca_loading_diff = pca_loading_diff,
    trajectory_plot = if (is.null(traj)) NULL else traj$plot,
    trajectory_deltas = if (is.null(traj)) NULL else traj$deltas
  )
}

# prcomp fits to the complete cases of `vars` in each data frame, or NULL with a warning
.domain_pca_diff <- function(orig_df, imp_df, vars) {
  skip <- function(why) {
    warning("PCA loading comparison skipped: ", why, call. = FALSE)
    NULL
  }
  if (length(vars) < 2L) {
    return(skip("it needs at least two variables."))
  }
  fit <- function(df) {
    x <- df[stats::complete.cases(df[, vars, drop = FALSE]), vars, drop = FALSE]
    if (nrow(x) <= length(vars)) return(NULL)
    if (any(vapply(x, function(v) stats::sd(v) == 0, logical(1)))) return(NULL)
    stats::prcomp(x, center = TRUE, scale. = TRUE)
  }
  p_orig <- fit(orig_df)
  p_imp <- fit(imp_df)
  if (is.null(p_orig) || is.null(p_imp)) {
    return(skip("a fit needs more complete cases than variables and no constant variable."))
  }
  pca_loading_diff_heatmap(p_orig, p_imp)
}

# mean and SE per variable and time, delta, paired test by id within time, Holm adjustment
.domain_trajectories <- function(orig_df, imp_df, vars, id_col, time_col, test) {
  se <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2L) NA_real_ else stats::sd(x) / sqrt(length(x))
  }
  paired_p <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    d <- a[ok] - b[ok]
    if (length(d) < 2L || all(d == 0)) return(NA_real_)
    res <- suppressWarnings(tryCatch(
      if (test == "t") stats::t.test(b[ok], a[ok], paired = TRUE) else stats::wilcox.test(b[ok], a[ok], paired = TRUE),
      error = function(e) NULL
    ))
    if (is.null(res)) NA_real_ else unname(res$p.value)
  }

  times <- sort(unique(orig_df[[time_col]]))
  rows <- list()
  long <- list()
  for (v in vars) {
    for (t in times) {
      i <- which(orig_df[[time_col]] == t)
      if (!length(i)) next
      # the rows correspond (ids and times were checked to agree), so row i pairs the two values
      a <- orig_df[[v]][i]
      b <- imp_df[[v]][i]
      m_orig <- if (all(is.na(a))) NA_real_ else mean(a, na.rm = TRUE)
      m_imp <- if (all(is.na(b))) NA_real_ else mean(b, na.rm = TRUE)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        variable = v, time = t, mean_orig = m_orig, mean_imp = m_imp,
        delta = m_imp - m_orig, n_paired = sum(!is.na(a) & !is.na(b)),
        p_value = paired_p(a, b)
      )
      long[[length(long) + 1L]] <- tibble::tibble(
        variable = v, time = t, dataset = c("Original", "Imputed"),
        mean = c(m_orig, m_imp), se = c(se(a), se(b))
      )
    }
  }
  deltas <- dplyr::bind_rows(rows)
  deltas$p_adj <- stats::p.adjust(deltas$p_value, method = "holm")

  long_df <- dplyr::bind_rows(long)
  long_df$lo <- long_df$mean - long_df$se
  long_df$hi <- long_df$mean + long_df$se
  colors <- c(Original = "#2B2B2B", Imputed = "#9D2235")
  plot <- ggplot2::ggplot(long_df, ggplot2::aes(
    x = .data$time, y = .data$mean, color = .data$dataset, fill = .data$dataset, group = .data$dataset
  )) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), alpha = 0.2, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free_y") +
    theme_cbe() +
    ggplot2::labs(
      title = "Mean trajectories: original vs. imputed",
      x = time_col, y = "Mean ± SE", color = "Data", fill = "Data"
    )
  list(deltas = deltas[, c("variable", "time", "mean_orig", "mean_imp", "delta", "n_paired", "p_value", "p_adj")],
       plot = plot)
}
