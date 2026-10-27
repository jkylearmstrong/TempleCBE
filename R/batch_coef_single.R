#' Univariable GLM / LMM Screens Over Inputs and Outputs
#'
#' Fits [glm_single()] or [lmm_single()] for every combination of an input (predictor) and an
#' output (outcome). With `normalize = TRUE` the main estimate is the one on the 0 to 1 scaled
#' predictor (comparable across inputs: the change in the output over the full observed range of
#' the input); with `include_dy_dx = TRUE` the estimate on the original scale (the marginal
#' effect `dy/dx`, change per unit of the input) is added with the suffix `_dy_dx`.
#'
#' @section Ranking and significance:
#' `rank` orders the inputs within each output by decreasing absolute main estimate (1 = largest;
#' ties share the lowest rank). `significance` is the label of `p_breaks` that the main p-value
#' falls below (the first break it is strictly below, as in [plot_coef_forest()]), or the last
#' label if none. No multiple-testing adjustment is applied to `p.value`; `p.adj` is the
#' Benjamini-Hochberg adjustment over all combinations of the main fits.
#'
#' @param data A data frame (long, with `id_col`, for `batch_lmm_single()`).
#' @param inputs,outputs Character vectors of predictor and response columns.
#' @param normalize Fit on the 0 to 1 scaled predictor as the main result. If `FALSE`, only the
#'   original scale is fitted and `include_dy_dx` is ignored.
#' @param include_dy_dx Add the original-scale columns (see above).
#' @param family Family for `batch_glm_single()`, passed to [glm_single()].
#' @param id_col Subject column of `batch_lmm_single()` (random intercept).
#' @param conf_level Confidence level.
#' @param p_breaks,p_labels Significance scheme; `p_labels` has one more entry than `p_breaks`.
#' @return A tibble with `output`, `input`, `term`, `estimate`, `std.error`, `statistic`,
#'   `p.value`, `conf.low`, `conf.high`, `n`, `status`, `warning`, `p.adj`, `rank`, `significance`
#'   and, with `include_dy_dx`, the same estimate columns suffixed `_dy_dx`. A combination that could not be
#'   fitted keeps a row with its `status`.
#' @seealso [glm_single()], [lmm_single()], [plot_coef_forest()]
#' @export
#' @examples
#' batch_glm_single(mtcars, c("wt", "hp", "disp"), c("mpg", "qsec"))
batch_glm_single <- function(data, inputs, outputs, normalize = TRUE, include_dy_dx = TRUE,
                             family = stats::gaussian(), conf_level = 0.95,
                             p_breaks = c(0.001, 0.01, 0.05), p_labels = c("***", "**", "*", "ns")) {
  .batch_coef(data, inputs, outputs, normalize, include_dy_dx, p_breaks, p_labels,
              fit = function(out, inp, norm) glm_single(data, out, inp, family = family, normalize = norm, conf_level = conf_level))
}

#' @rdname batch_glm_single
#' @export
batch_lmm_single <- function(data, inputs, outputs, id_col = "subject_id", normalize = TRUE,
                             include_dy_dx = TRUE, conf_level = 0.95,
                             p_breaks = c(0.001, 0.01, 0.05), p_labels = c("***", "**", "*", "ns")) {
  .batch_coef(data, inputs, outputs, normalize, include_dy_dx, p_breaks, p_labels,
              fit = function(out, inp, norm) lmm_single(data, out, inp, id_col = id_col, normalize = norm, conf_level = conf_level))
}

.batch_coef <- function(data, inputs, outputs, normalize, include_dy_dx, p_breaks, p_labels, fit) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (a in list(inputs, outputs)) {
    if (!is.character(a) || length(a) == 0L || anyNA(a)) stop("`inputs` and `outputs` must be non-empty character vectors.", call. = FALSE)
  }
  gone <- setdiff(c(inputs, outputs), names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (length(p_labels) != length(p_breaks) + 1L) stop("`p_labels` needs one more entry than `p_breaks`.", call. = FALSE)
  .check_star_scheme(p_breaks, p_labels)

  grid <- expand.grid(input = inputs, output = outputs, stringsAsFactors = FALSE)
  grid <- grid[grid$input != grid$output, , drop = FALSE]
  if (nrow(grid) == 0L) stop("Every input is also the output.", call. = FALSE)
  res <- lapply(seq_len(nrow(grid)), function(i) {
    r <- fit(grid$output[i], grid$input[i], isTRUE(normalize))
    r$output <- grid$output[i]
    r$input <- grid$input[i]
    r
  })
  all <- dplyr::bind_rows(res)
  keep <- c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  all$scale <- as.character(all$scale)
  main_scale <- if (isTRUE(normalize)) "normalized" else "raw"
  # a failed fit has only a "raw" status row
  failed <- all$scale == "raw" & is.na(all$estimate) & all$status != "ok"
  main <- all[all$scale == main_scale | (isTRUE(normalize) & failed), , drop = FALSE]
  base_cols <- c("output", "input", "term", keep, "n", "status", "warning")
  out <- main[, base_cols, drop = FALSE]
  if (isTRUE(normalize) && isTRUE(include_dy_dx)) {
    raw <- all[all$scale == "raw" & !failed, c("output", "input", "term", keep), drop = FALSE]
    names(raw)[4:9] <- paste0(keep, "_dy_dx")
    out <- merge(out, raw, by = c("output", "input", "term"), all.x = TRUE, sort = FALSE)
  }
  ok <- out$status == "ok"
  out$p.adj <- NA_real_
  out$p.adj[ok] <- stats::p.adjust(out$p.value[ok], method = "BH")
  out$rank <- NA_integer_
  for (o in unique(out$output)) {
    i <- which(out$output == o & ok)
    out$rank[i] <- as.integer(rank(-abs(out$estimate[i]), ties.method = "min"))
  }
  out$significance <- ifelse(is.na(out$p.value), NA_character_, .coef_stars(out$p.value, p_breaks, p_labels))
  out <- out[order(out$output, is.na(out$rank), out$rank), , drop = FALSE]
  tibble::as_tibble(out)
}
