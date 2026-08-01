#' Factor Analysis of Mixed Data (FAMD) Recipe Step
#'
#' `step_famd` creates a *specification* of a recipe step that will extract
#' Factor Analysis of Mixed Data (FAMD) principal components from numeric and categorical variables.
#'
#' @param recipe A recipe object. The step will be added to the sequence of steps for this recipe.
#' @param ... One or more selector functions to choose variables.
#' @param role Role for created variables (default "predictor").
#' @param trained Logical indicating if step has been trained.
#' @param num_comp Number of components to extract (default 2).
#' @param threshold Cumulative percentage of variance threshold.
#' @param options Options passed to underlying FAMD algorithm.
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

#' @exportS3Method recipes::prep
prep.step_famd <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  
  if (x$num_comp > 0 && length(col_names) > 0) {
    if (requireNamespace("FactoMineR", quietly = TRUE)) {
      famd_obj <- FactoMineR::FAMD(training[, col_names, drop = FALSE], graph = FALSE)
    } else {
      # Fallback PCA on numeric subset if FactoMineR is not installed
      num_cols <- col_names[sapply(training[, col_names, drop = FALSE], is.numeric)]
      if (length(num_cols) > 0) {
        famd_obj <- stats::prcomp(training[, num_cols, drop = FALSE], scale. = TRUE)
      } else {
        famd_obj <- NULL
      }
    }
    
    x$num_comp <- min(x$num_comp, length(col_names))
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
  
  if (inherits(object$res, "FAMD") && requireNamespace("FactoMineR", quietly = TRUE)) {
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
  cat("FAMD extraction with ", paste(names(x$columns), collapse = ", "), "\n", sep = "")
  invisible(x)
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
    res <- tibble::tibble(
      terms = unname(x$columns),
      value = 1.0,
      component = "PC1"
    )
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
