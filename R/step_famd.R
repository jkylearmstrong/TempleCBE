#' Factor Analysis of Mixed Data (FAMD) Recipe Step
#'
#' `step_famd` creates a *specification* of a recipe step that will extract
#' Factor Analysis of Mixed Data (FAMD) principal components from numeric and categorical variables.
#'
#' @param recipe A recipe object. The step will be added to the sequence of steps for this recipe.
#' @param ... One or more selector functions to choose variables.
#' @param role Role for created variables (default "predictor").
#' @param trained Logical indicating if step has been trained.
#' @param num_comp Number of components to extract (default 2). Ignored if
#'   \code{threshold} is set.
#' @param threshold A fraction of the total variance that should be covered
#'   by the components (a number in \code{(0, 1]}). When set (non-\code{NA}),
#'   the step extracts the smallest number of components whose cumulative
#'   variance meets this threshold, overriding \code{num_comp}.
#' @param options A named list of additional arguments passed on to
#'   \code{\link[FactoMineR]{FAMD}} (e.g. \code{list(row.w = ...)}); merged
#'   over the step's own defaults (\code{ncp} and \code{graph = FALSE}), so
#'   \code{options} can override those defaults too if desired.
#' @param res FAMD object.
#' @param columns Vector of column names.
#' @param prefix Component prefix (default "PC").
#' @param keep_original_cols Logical; whether to retain original columns.
#' @param skip Logical; skip step when baking.
#' @param id Unique step identifier.
#' @return An updated recipe object.
#' @importFrom recipes add_step step rand_id check_new_data check_name remove_original_cols print_step is_trained sel2char get_keep_original_cols
#' @importFrom rlang enquos expr na_dbl na_chr
#' @importFrom tibble as_tibble tibble
#' @export
step_famd <- function(recipe,
                      ...,
                      role = "predictor",
                      trained = FALSE,
                      num_comp = 2,
                      threshold = NA,
                      options = list(),
                      res = NULL,
                      columns = NULL,
                      prefix = "PC",
                      keep_original_cols = FALSE,
                      skip = FALSE,
                      id = recipes::rand_id("famd")) {
  recipes::add_step(
    recipe,
    step_famd_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      num_comp = num_comp,
      threshold = threshold,
      options = options,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_famd_new <- function(terms, role, trained, num_comp, threshold, options, res, columns,
                          prefix, keep_original_cols, skip, id, case_weights) {
  recipes::step(
    subclass = "famd",
    terms = terms,
    role = role,
    trained = trained,
    num_comp = num_comp,
    threshold = threshold,
    options = options,
    res = res,
    columns = columns,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id,
    case_weights = case_weights
  )
}

#' @keywords internal
#' @noRd
famd_num_comp <- function(cum_pct, threshold, requested_num_comp) {
  max_possible <- length(cum_pct)
  if (is.na(threshold)) {
    return(min(requested_num_comp, max_possible))
  }
  n_needed <- unname(which(cum_pct >= threshold * 100)[1])
  if (is.na(n_needed)) max_possible else n_needed
}

#' @keywords internal
#' @noRd
famd_available <- function() requireNamespace("FactoMineR", quietly = TRUE)

#' @exportS3Method recipes::prep
prep.step_famd <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (x$num_comp > 0 && length(col_names) > 0) {
    if (famd_available()) {
      selected <- training[, col_names, drop = FALSE]
      is_quant <- vapply(selected, is.numeric, logical(1))
      if (all(is_quant) || !any(is_quant)) {
        stop(
          "step_famd() requires both quantitative and qualitative columns among ",
          "the selected variables (FAMD is for mixed data); the current selection ",
          "is ", if (all(is_quant)) "entirely quantitative" else "entirely qualitative",
          ". Use step_pca() for all-quantitative data or step_dummy() + step_pca() ",
          "for all-qualitative data instead.",
          call. = FALSE
        )
      }

      # When selecting components via a variance threshold, fit with the
      # maximum feasible number of components so the cumulative-variance
      # cutoff has the full spectrum available to choose from.
      max_possible <- length(col_names)
      fit_ncp <- if (is.na(x$threshold)) min(x$num_comp, max_possible) else max_possible

      famd_args <- utils::modifyList(
        list(base = selected, ncp = fit_ncp, graph = FALSE),
        x$options
      )
      famd_obj <- do.call(FactoMineR::FAMD, famd_args)
      cum_pct <- famd_obj$eig[, "cumulative percentage of variance"]
    } else {
      # Fallback PCA on numeric subset if FactoMineR is not installed
      num_cols <- col_names[sapply(training[, col_names, drop = FALSE], is.numeric)]
      if (length(num_cols) > 0) {
        famd_obj <- stats::prcomp(training[, num_cols, drop = FALSE], scale. = TRUE)
        cum_pct <- cumsum(famd_obj$sdev^2) / sum(famd_obj$sdev^2) * 100
      } else {
        famd_obj <- NULL
        cum_pct <- numeric(0)
      }
    }

    x$num_comp <- if (is.null(famd_obj)) 0 else famd_num_comp(cum_pct, x$threshold, x$num_comp)
  } else {
    famd_obj <- NULL
  }

  step_famd_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    threshold = x$threshold,
    options = x$options,
    res = famd_obj,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = NULL
  )
}

#' @exportS3Method recipes::bake
bake.step_famd <- function(object, new_data, ...) {
  recipes::check_new_data(object$columns, object, new_data)

  if (is.null(object$columns) || length(object$columns) == 0 || is.null(object$res)) {
    return(new_data)
  }

  if (inherits(object$res, "FAMD")) {
    if (!famd_available()) {
      stop(
        "step_famd() was trained using FactoMineR, but the FactoMineR ",
        "package is not available in this session. Install it to bake ",
        "this recipe.",
        call. = FALSE
      )
    }
    comps <- as.data.frame(FactoMineR::predict.FAMD(object$res, newdata = new_data)$coord)
  } else if (inherits(object$res, "prcomp")) {
    num_cols <- object$columns[sapply(new_data[, object$columns, drop = FALSE], is.numeric)]
    comps <- as.data.frame(stats::predict(object$res, newdata = new_data[, num_cols, drop = FALSE]))
  } else {
    return(new_data)
  }

  comps <- comps[, seq_len(min(object$num_comp, ncol(comps))), drop = FALSE]
  colnames(comps) <- paste0(object$prefix, seq_len(ncol(comps)))

  res_df <- tibble::as_tibble(comps)
  new_data <- vctrs::vec_cbind(new_data, res_df)

  if (!object$keep_original_cols) {
    new_data <- new_data[, !(names(new_data) %in% object$columns), drop = FALSE]
  }

  new_data
}

#' @exportS3Method base::print
print.step_famd <- function(x, width = max(20, options()$width - 29), ...) {
  title <- "FAMD extraction with "
  recipes::print_step(x$columns, x$terms, x$trained, title, width, case_weights = x$case_weights)
  invisible(x)
}

#' @keywords internal
#' @noRd
famd_coefs <- function(x) {
  if (x$num_comp == 0 || length(x$columns) == 0 || is.null(x$res)) {
    return(tibble::tibble(terms = unname(x$columns), value = rlang::na_dbl, component = rlang::na_chr))
  }

  if (inherits(x$res, "FAMD")) {
    # `res$var$contrib` is FactoMineR's per-variable (not per-category)
    # percentage contribution to each dimension -- the standard FAMD
    # output for comparing quantitative and qualitative variables on a
    # common scale, since raw "loadings" aren't directly comparable
    # across mixed variable types.
    loadings <- x$res$var$contrib[, seq_len(x$num_comp), drop = FALSE]
  } else if (inherits(x$res, "prcomp")) {
    loadings <- x$res$rotation[, seq_len(min(x$num_comp, ncol(x$res$rotation))), drop = FALSE]
  } else {
    return(tibble::tibble(terms = unname(x$columns), value = rlang::na_dbl, component = rlang::na_chr))
  }

  colnames(loadings) <- paste0(x$prefix, seq_len(ncol(loadings)))
  res <- utils::stack(as.data.frame(loadings))
  colnames(res) <- c("value", "component")
  res$component <- as.character(res$component)
  res$terms <- rep(rownames(loadings), ncol(loadings))
  tibble::as_tibble(res)[, c("terms", "value", "component")]
}

#' @exportS3Method recipes::tidy
tidy.step_famd <- function(x, type = "coef", ...) {
  if (!recipes::is_trained(x)) {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      value = rlang::na_dbl,
      component = rlang::na_chr
    )
  } else {
    res <- famd_coefs(x)
  }
  res$id <- x$id
  res
}

#' @exportS3Method recipes::tunable
tunable.step_famd <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_famd",
    component_id = x$id
  )
}
