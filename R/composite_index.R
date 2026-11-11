#' Composite Index Construction and Variable Correlation Table
#'
#' Functions for centering variables, calculating standardized or PCA-based
#' composite prognostic indices, and generating summary tables of component variable
#' loadings and correlations.
#'
#' @name composite_index
NULL

#' Center a Numeric Vector
#'
#' Subtracts the mean from a numeric vector, returning a vector (not a 1D matrix).
#'
#' `center` is a common name: other attached packages, or an object of your own
#' called `center`, can mask it (and [compute_composite_index()] has an argument
#' of that name). In scripts, write `TempleCBE::center()` to be sure which one
#' is called.
#'
#' @param x Numeric vector.
#' @param na.rm Logical. Remove missing values when computing the mean (default `TRUE`).
#' @return Numeric vector centered at zero.
#' @export
#' @examples
#' center(c(10, 20, 30))
center <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  x - mean(x, na.rm = na.rm)
}

#' Compute a Composite Clinical or Biomarker Index
#'
#' Combines multiple numeric variables into a single composite score using
#' standardized z-scores, first principal component (PCA), or weighted sums.
#'
#' @param data A data frame or tibble.
#' @param cols Character vector of numeric column names to include in the index.
#' @param method Index construction method: `"zscore"` (default, mean of z-scores),
#'   `"pca"` (first principal component score, scaled so higher values indicate greater severity),
#'   `"sum"` (weighted sum), or `"min_max"` (each variable rescaled to the range 0 to 1 over its
#'   observed values, optionally centered, then the weighted sum; a constant variable contributes 0).
#' @param weights Optional numeric vector of weights matching `cols`. If `NULL`,
#'   equal weights are used.
#' @param index_name Character string naming the output index column (default `"composite_index"`).
#' @param polarities Optional vector of `+1` / `-1`, one per column of `cols` (in that order, or
#'   named by column): `-1` for a variable whose larger values mean a *lower* index, so that all
#'   components push the index in the same direction. Multiplies the weights. Not used by
#'   `method = "pca"`, which chooses its own signs (giving `polarities` there is an error).
#' @param center For `method = "min_max"`: if `TRUE` (default) each rescaled variable is centered on
#'   its mean, so an average subject scores 0. To center on a baseline visit instead of all rows,
#'   use [compute_longitudinal_composite_index()].
#' @return The input data frame augmented with the new composite index column.
#' @seealso [index_variable_table()], [center()]
#' @export
#' @examples
#' df <- data.frame(sbp = c(120, 140, 160), dbp = c(80, 90, 100), bmi = c(22, 28, 35))
#' compute_composite_index(df, cols = c("sbp", "dbp", "bmi"), method = "zscore")
compute_composite_index <- function(data,
                                    cols,
                                    method = c("zscore", "pca", "sum", "min_max"),
                                    weights = NULL,
                                    index_name = "composite_index",
                                    polarities = NULL,
                                    center = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  method <- match.arg(method)
  mat <- as.matrix(data[, cols, drop = FALSE])
  p <- length(cols)
  polarities <- .index_polarities(polarities, cols)
  if (!is.null(polarities) && method == "pca") {
    stop("`polarities` cannot be used with `method = \"pca\"`.", call. = FALSE)
  }

  if (!is.null(weights)) {
    if (length(weights) != p) {
      stop("`weights` length (", length(weights), ") must match `cols` length (", p, ").", call. = FALSE)
    }
  } else {
    weights <- rep(1 / p, p)
  }
  if (!is.null(polarities)) weights <- weights * polarities

  if (method == "min_max") {
    scaled_mat <- apply(mat, 2, function(x) {
      r <- range(x, na.rm = TRUE)
      z <- if (!all(is.finite(r)) || r[1] == r[2]) ifelse(is.na(x), NA_real_, 0) else (x - r[1]) / (r[2] - r[1])
      if (isTRUE(center)) z - mean(z, na.rm = TRUE) else z
    })
    score <- as.vector(matrix(scaled_mat, ncol = p) %*% weights)
  } else if (method == "zscore") {
    # Scale each column to mean 0, sd 1
    scaled_mat <- apply(mat, 2, function(x) {
      s <- stats::sd(x, na.rm = TRUE)
      if (is.na(s) || s == 0) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / s
    })
    # Weighted average
    score <- as.vector(scaled_mat %*% weights)
  } else if (method == "pca") {
    complete_cases <- stats::complete.cases(mat)
    pca <- stats::prcomp(mat[complete_cases, , drop = FALSE], center = TRUE, scale. = TRUE)
    # Predict all rows
    pc1_all <- stats::predict(pca, newdata = as.data.frame(mat))[, 1]
    # Ensure positive correlation with sum of variables
    cors <- vapply(seq_len(p), function(j) stats::cor(pc1_all, mat[, j], use = "pairwise.complete.obs"), numeric(1))
    if (mean(cors, na.rm = TRUE) < 0) {
      pc1_all <- -pc1_all
    }
    score <- as.vector(pc1_all)
  } else if (method == "sum") {
    score <- as.vector(mat %*% weights)
  }

  data[[index_name]] <- score
  data
}

# +1/-1 per column, in `cols` order; NULL stays NULL
.index_polarities <- function(polarities, cols) {
  if (is.null(polarities)) return(NULL)
  if (!is.numeric(polarities) || anyNA(polarities) || !all(polarities %in% c(-1, 1))) {
    stop("`polarities` must be +1 or -1 for every column.", call. = FALSE)
  }
  if (!is.null(names(polarities))) {
    gap <- setdiff(cols, names(polarities))
    if (length(gap)) stop("`polarities` has no value for: ", paste(gap, collapse = ", "), ".", call. = FALSE)
    return(unname(polarities[cols]))
  }
  if (length(polarities) != length(cols)) {
    stop("`polarities` length (", length(polarities), ") must match `cols` length (", length(cols), ").", call. = FALSE)
  }
  polarities
}

#' Summary Table of Composite Index Component Variables
#'
#' Generates a tidy summary table of component variables comprising a composite index,
#' reporting weights/loadings, correlation with the index, mean, SD, and p-value.
#'
#' @param data Data frame containing component columns and the composite index column.
#' @param cols Character vector of component variable names.
#' @param index_col Character name of the composite index column.
#' @param method Correlation method: `"pearson"` (default) or `"spearman"`.
#' @param polarities Optional `+1` / `-1` per component, as in [compute_composite_index()]; adds a
#'   `polarity` column.
#' @param weights Optional weights per component (in `cols` order); adds a `weight` column.
#' @return A tibble with one row per component variable: `variable`, `polarity` and `weight` (when
#'   given), `mean`, `sd`, `min`, `max`, `r_with_index` and `p_value`.
#' @seealso [compute_composite_index()]
#' @export
#' @examples
#' df <- data.frame(sbp = c(120, 140, 160), dbp = c(80, 90, 100), bmi = c(22, 28, 35))
#' df <- compute_composite_index(df, cols = c("sbp", "dbp", "bmi"))
#' index_variable_table(df, cols = c("sbp", "dbp", "bmi"), index_col = "composite_index")
index_variable_table <- function(data,
                                 cols,
                                 index_col = "composite_index",
                                 method = c("pearson", "spearman"),
                                 polarities = NULL,
                                 weights = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!index_col %in% names(data)) {
    stop("`index_col` '", index_col, "' not found in `data`.", call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  method <- match.arg(method)
  polarities <- .index_polarities(polarities, cols)
  if (!is.null(weights) && (!is.numeric(weights) || length(weights) != length(cols))) {
    stop("`weights` must be numeric, one per column of `cols`.", call. = FALSE)
  }

  rows <- lapply(cols, function(col) {
    x <- data[[col]]
    y <- data[[index_col]]
    complete <- !is.na(x) & !is.na(y)

    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)

    ct <- tryCatch(
      stats::cor.test(x[complete], y[complete], method = method),
      error = function(e) list(estimate = NA_real_, p.value = NA_real_)
    )

    rng <- if (all(is.na(x))) c(NA_real_, NA_real_) else range(x, na.rm = TRUE)
    tibble::tibble(
      variable = col,
      mean = m,
      sd = s,
      min = rng[1],
      max = rng[2],
      r_with_index = unname(ct$estimate),
      p_value = unname(ct$p.value)
    )
  })

  out <- dplyr::bind_rows(rows)
  if (!is.null(weights)) out <- tibble::add_column(out, weight = weights, .after = "variable")
  if (!is.null(polarities)) out <- tibble::add_column(out, polarity = polarities, .after = "variable")
  out
}
