#' Imputation Quality Auditing and Method Comparison Toolkit
#'
#' Tools for evaluating imputation quality by comparing original vs. imputed
#' distributions (ECDF, Kolmogorov-Smirnov tests, ridgeline density plots) and
#' benchmarking numeric imputation algorithms via Relative Squared Error (RSE).
#'
#' @name imputation_diagnostics
NULL

#' Compare Original vs Imputed Empirical Cumulative Distribution Functions
#'
#' Overlays the empirical cumulative distribution function (ECDF) of observed
#' versus imputed values for one or more variables.
#'
#' @param orig Data frame containing original (pre-imputation) values.
#' @param imp Data frame containing post-imputation values.
#' @param cols Character vector of variable names to compare.
#' @param colors Named vector of colors for `c(Original = "...", Imputed = "...")`.
#' @return A `ggplot2` object.
#' @seealso [test_imputed_distributions()], [plot_imputed_ridgeline()]
#' @export
#' @examples
#' df_orig <- data.frame(x = c(1, 2, NA, 4, 5))
#' df_imp <- data.frame(x = c(1, 2, 3, 4, 5))
#' compare_imputed_ecdf(df_orig, df_imp, cols = "x")
compare_imputed_ecdf <- function(orig,
                                 imp,
                                 cols,
                                 colors = c(Original = "#2B2B2B", Imputed = "#9D2235")) {
  missing_orig <- setdiff(cols, names(orig))
  missing_imp <- setdiff(cols, names(imp))
  if (length(missing_orig) > 0 || length(missing_imp) > 0) {
    stop("Specified column(s) missing from datasets.", call. = FALSE)
  }

  long_data <- lapply(cols, function(col) {
    v_orig <- orig[[col]]
    v_imp <- imp[[col]]
    # Identify which entries were originally missing
    was_na <- is.na(v_orig)

    df_obs <- data.frame(
      variable = col,
      value = v_orig[!was_na],
      dataset = "Original",
      stringsAsFactors = FALSE
    )
    df_imp <- data.frame(
      variable = col,
      value = v_imp[was_na],
      dataset = "Imputed",
      stringsAsFactors = FALSE
    )
    rbind(df_obs, df_imp)
  })

  long_df <- dplyr::bind_rows(long_data)
  if (nrow(long_df) == 0L) {
    stop("No data available to plot.", call. = FALSE)
  }

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data$value, color = .data$dataset)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    theme_cbe() +
    ggplot2::labs(
      title = "Empirical Cumulative Distribution: Original vs. Imputed",
      x = "Value",
      y = "Cumulative Probability",
      color = "Data"
    )

  if (length(cols) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$variable, scales = "free_x")
  }

  p
}

#' Statistical Distribution Comparison of Original vs Imputed Values
#'
#' Runs Kolmogorov-Smirnov (KS) tests and two-sample comparisons on observed vs
#' imputed values across specified variables, reporting test statistics and p-values.
#'
#' @param orig Data frame containing original (pre-imputation) values.
#' @param imp Data frame containing post-imputation values.
#' @param cols Character vector of variable names to test.
#' @return A tibble with one row per variable summarizing distribution comparison tests.
#' @seealso [compare_imputed_ecdf()]
#' @export
test_imputed_distributions <- function(orig, imp, cols) {
  rows <- lapply(cols, function(col) {
    v_orig <- orig[[col]]
    v_imp <- imp[[col]]
    was_na <- is.na(v_orig)

    obs_vals <- v_orig[!was_na]
    imp_vals <- v_imp[was_na]

    n_obs <- length(obs_vals)
    n_imp <- length(imp_vals)

    if (n_imp == 0) {
      return(tibble::tibble(
        variable = col, n_observed = n_obs, n_imputed = 0L,
        mean_observed = mean(obs_vals, na.rm = TRUE), mean_imputed = NA_real_,
        ks_statistic = NA_real_, ks_p_value = NA_real_, t_p_value = NA_real_
      ))
    }

    ks_res <- suppressWarnings(tryCatch(
      stats::ks.test(obs_vals, imp_vals),
      error = function(e) list(statistic = NA_real_, p.value = NA_real_)
    ))

    t_res <- suppressWarnings(tryCatch(
      stats::t.test(obs_vals, imp_vals),
      error = function(e) list(p.value = NA_real_)
    ))

    tibble::tibble(
      variable = col,
      n_observed = n_obs,
      n_imputed = n_imp,
      mean_observed = mean(obs_vals, na.rm = TRUE),
      mean_imputed = mean(imp_vals, na.rm = TRUE),
      ks_statistic = unname(ks_res$statistic),
      ks_p_value = unname(ks_res$p.value),
      t_p_value = unname(t_res$p.value)
    )
  })

  dplyr::bind_rows(rows)
}

#' Ridgeline Density Comparison of Original vs Imputed Distributions
#'
#' Displays overlapping density ridgelines comparing observed versus imputed distributions.
#'
#' @param orig Data frame containing original (pre-imputation) values.
#' @param imp Data frame containing post-imputation values.
#' @param cols Character vector of variable names to compare.
#' @return A `ggplot2` object.
#' @export
plot_imputed_ridgeline <- function(orig, imp, cols) {
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Package \"ggridges\" is required for plot_imputed_ridgeline().", call. = FALSE)
  }

  long_data <- lapply(cols, function(col) {
    v_orig <- orig[[col]]
    v_imp <- imp[[col]]
    was_na <- is.na(v_orig)

    df_obs <- data.frame(variable = col, value = v_orig[!was_na], dataset = "Original", stringsAsFactors = FALSE)
    df_imp <- data.frame(variable = col, value = v_imp[was_na], dataset = "Imputed", stringsAsFactors = FALSE)
    rbind(df_obs, df_imp)
  })

  long_df <- dplyr::bind_rows(long_data)
  if (nrow(long_df) == 0L) {
    stop("No data available to plot.", call. = FALSE)
  }

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$value, y = .data$variable, fill = .data$dataset)) +
    ggridges::geom_density_ridges(alpha = 0.6, scale = 1.2, color = "white") +
    ggplot2::scale_fill_manual(values = c(Original = "#6F6F6F", Imputed = "#9D2235")) +
    theme_cbe() +
    ggplot2::labs(
      title = "Distribution Density Comparison: Original vs Imputed",
      x = "Value",
      y = "Variable",
      fill = "Dataset"
    )
}

#' Benchmark Numeric Imputation Methods Using Relative Squared Error (RSE)
#'
#' Evaluates multiple numeric imputation methods (mean, median, and KNN) by artificially
#' masking observed values and computing Relative Squared Error (RSE) and MAE.
#'
#' @param data A data frame with numeric columns.
#' @param target_cols Character vector of numeric columns to evaluate.
#' @param methods Imputation methods to benchmark: `"mean"`, `"median"`, and/or `"knn"`.
#' @param prop_mask Proportion of observed values to artificially mask (default 0.15).
#' @param seed Random seed for reproducible masking.
#' @return A tibble ranking the imputation methods by RSE and MAE across variables.
#' @export
tune_impute_numeric <- function(data,
                                target_cols,
                                methods = c("mean", "median", "knn"),
                                prop_mask = 0.15,
                                seed = 123) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  missing_cols <- setdiff(target_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- data[, target_cols, drop = FALSE]
  # Keep rows with complete data for evaluation
  complete_df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(complete_df) < 10) {
    stop("At least 10 complete cases are required for imputation benchmarking.", call. = FALSE)
  }

  results <- list()

  for (col in target_cols) {
    true_vals <- complete_df[[col]]
    n <- length(true_vals)
    n_mask <- max(1L, round(prop_mask * n))
    mask_idx <- sample.int(n, size = n_mask)

    masked_col <- true_vals
    masked_col[mask_idx] <- NA_real_

    # Denominator for RSE: sum of squared deviations from mean
    ss_tot <- sum((true_vals[mask_idx] - mean(true_vals[-mask_idx]))^2)
    if (ss_tot == 0) ss_tot <- 1e-6

    for (m in methods) {
      pred_vals <- if (m == "mean") {
        rep(mean(masked_col, na.rm = TRUE), n_mask)
      } else if (m == "median") {
        rep(stats::median(masked_col, na.rm = TRUE), n_mask)
      } else if (m == "knn") {
        # Simple 5-NN imputation using other columns if available
        other_cols <- setdiff(target_cols, col)
        if (length(other_cols) > 0) {
          ref_mat <- as.matrix(complete_df[, other_cols, drop = FALSE])
          # Scale
          ref_scaled <- scale(ref_mat)
          dists <- as.matrix(stats::dist(ref_scaled))
          vapply(mask_idx, function(i) {
            # Find closest 5 neighbors among non-masked
            non_masked <- setdiff(seq_len(n), mask_idx)
            closest <- non_masked[order(dists[i, non_masked])[1:min(5, length(non_masked))]]
            mean(true_vals[closest], na.rm = TRUE)
          }, numeric(1))
        } else {
          rep(stats::median(masked_col, na.rm = TRUE), n_mask)
        }
      } else {
        rep(mean(masked_col, na.rm = TRUE), n_mask)
      }

      ss_res <- sum((true_vals[mask_idx] - pred_vals)^2)
      rse <- ss_res / ss_tot
      mae <- mean(abs(true_vals[mask_idx] - pred_vals))

      results[[length(results) + 1]] <- tibble::tibble(
        variable = col,
        method = m,
        rse = rse,
        mae = mae,
        n_masked = n_mask
      )
    }
  }

  res_df <- dplyr::bind_rows(results)
  dplyr::arrange(res_df, .data$variable, .data$rse)
}
