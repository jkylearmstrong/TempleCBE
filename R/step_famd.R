#' Factor Analysis of Mixed Data (FAMD) Recipe Step
#'
#' `step_famd()` creates a *specification* of a recipe step that converts a mix
#' of numeric and categorical variables into principal components with Factor
#' Analysis of Mixed Data ([FactoMineR::FAMD()]). It is the mixed-data
#' counterpart of [recipes::step_pca()]: numeric variables are standardized and
#' categorical variables are weighted, so each variable contributes on a
#' comparable scale.
#'
#' @details
#' The selected variables must include at least one numeric and one categorical
#' (factor, character, or logical) variable; use [recipes::step_pca()] for
#' all-numeric data. Character and logical variables are treated as factors,
#' with levels learned by `prep()`. Missing values, and categories that `bake()`
#' meets but training did not, are errors: impute or collapse levels first,
#' e.g. with [recipes::step_impute_mode()] or [recipes::step_other()].
#'
#' FAMD has at most \eqn{\min(n - 1, p + \sum_j (L_j - 1))} dimensions, for
#' \eqn{n} rows, \eqn{p} numeric variables, and categorical variables with
#' \eqn{L_j} levels, which can be more than the number of variables.
#' `num_comp` is capped there. With `threshold`, the step keeps the fewest
#' components whose cumulative share of variance reaches `threshold`, choosing
#' among all dimensions.
#'
#' Frequency weights ([hardhat::frequency_weights()]) are passed to FAMD as
#' row weights. Like other unsupervised recipe steps, such as
#' [recipes::step_pca()], it ignores importance weights.
#'
#' # Tidying
#'
#' `tidy()` with `type = "coef"` returns each variable's percentage
#' contribution to each component (`value`): FactoMineR's per-variable summary,
#' which is comparable between numeric and categorical variables. With
#' `type = "variance"`, it returns each component's variance, cumulative
#' variance, percent variance, and cumulative percent variance, as for
#' [recipes::step_pca()].
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [recipes::selections()].
#' @param role Role of the new component columns (default `"predictor"`).
#' @param trained A logical indicating whether the step has been trained.
#' @param num_comp Number of components to keep (default 2); ignored when
#'   `threshold` is set. `0` leaves the data unchanged.
#' @param threshold Fraction of total variance, in (0, 1], that the kept
#'   components should cover. Overrides `num_comp` when not `NA`.
#' @param options A named list of further arguments to [FactoMineR::FAMD()].
#' @param res The fitted FAMD object, once trained.
#' @param columns The selected column names, once trained.
#' @param levels The factor levels of the categorical columns, once trained.
#' @param prefix Prefix of the new column names (default `"FAMD"`), numbered by
#'   [recipes::names0()]. It differs from [recipes::step_pca()]'s `"PC"`, so
#'   both steps can be used in one recipe.
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `FALSE`.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [recipes::bake()]?
#' @param id A character string that is unique to this step.
#' @return An updated recipe object.
#' @references Pagès J (2004). Analyse factorielle de données mixtes. *Revue de
#'   Statistique Appliquée*, 52(4), 93-111.
#' @importFrom recipes add_step step rand_id
#' @importFrom rlang enquos na_dbl na_chr
#' @importFrom tibble as_tibble tibble
#' @export
#' @examples
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   library(recipes)
#'   rec <- recipe(~ ., data = iris) |>
#'     step_famd(all_predictors(), num_comp = 3)
#'   prepped <- prep(rec)
#'   head(bake(prepped, new_data = NULL))
#'   tidy(prepped, number = 1, type = "variance")
#' }
step_famd <- function(recipe,
                      ...,
                      role = "predictor",
                      trained = FALSE,
                      num_comp = 2,
                      threshold = NA,
                      options = list(),
                      res = NULL,
                      columns = NULL,
                      levels = NULL,
                      prefix = "FAMD",
                      keep_original_cols = FALSE,
                      skip = FALSE,
                      id = recipes::rand_id("famd")) {
  check_famd_args(num_comp, threshold, options, prefix)
  recipes::add_step(
    recipe,
    step_famd_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      threshold = threshold,
      options = options,
      res = res,
      columns = columns,
      levels = levels,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_famd_new <- function(terms, role, trained, num_comp, threshold, options, res, columns,
                          levels, prefix, keep_original_cols, skip, id, case_weights) {
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
    levels = levels,
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

  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) wts <- NULL

  res <- NULL
  levels <- NULL
  num_comp <- x$num_comp
  wants_components <- !is.na(x$threshold) || num_comp > 0

  if (length(col_names) > 0 && wants_components) {
    rlang::check_installed("FactoMineR", reason = "for `step_famd()`.")
    selected <- famd_as_factors(training[, col_names, drop = FALSE])
    is_quant <- vapply(selected, is.numeric, logical(1))
    if (all(is_quant) || !any(is_quant)) {
      stop(
        "`step_famd()` needs both numeric and categorical variables (FAMD is for mixed data); ",
        "the selection is entirely ", if (all(is_quant)) "numeric" else "categorical", ". ",
        "Use step_pca() for numeric data, or step_dummy() then step_pca() for categorical data.",
        call. = FALSE
      )
    }
    if (anyNA(selected)) {
      stop(
        "`step_famd()` can't handle missing values in: ",
        paste(names(selected)[vapply(selected, anyNA, logical(1))], collapse = ", "),
        ". Impute them in an earlier step.",
        call. = FALSE
      )
    }
    levels <- lapply(selected[!is_quant], base::levels)

    n_dims <- famd_max_dims(selected, is_quant)
    famd_args <- utils::modifyList(
      list(base = as.data.frame(selected), ncp = if (is.na(x$threshold)) min(num_comp, n_dims) else n_dims, graph = FALSE),
      x$options
    )
    if (!is.null(wts)) {
      if (!is.null(x$options$row.w)) {
        stop("Use case weights or `options = list(row.w = )`, not both.", call. = FALSE)
      }
      famd_args$row.w <- as.double(wts)
    }
    res <- do.call(FactoMineR::FAMD, famd_args)

    available <- ncol(as.matrix(res$ind$coord))
    num_comp <- if (is.na(x$threshold)) {
      min(num_comp, available)
    } else {
      cum_pct <- res$eig[, "cumulative percentage of variance"]
      reached <- which(cum_pct >= x$threshold * 100 - sqrt(.Machine$double.eps))
      min(if (length(reached)) reached[1] else length(cum_pct), available)
    }
  }

  step_famd_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = num_comp,
    threshold = x$threshold,
    options = x$options,
    res = res,
    columns = col_names,
    levels = levels,
    prefix = x$prefix,
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @exportS3Method recipes::bake
bake.step_famd <- function(object, new_data, ...) {
  col_names <- object$columns
  recipes::check_new_data(col_names, object, new_data)
  if (is.null(object$res) || object$num_comp == 0 || length(col_names) == 0) {
    return(new_data)
  }
  rlang::check_installed("FactoMineR", reason = "to bake a recipe with `step_famd()`.")

  comp_names <- recipes::names0(object$num_comp, object$prefix)
  if (nrow(new_data) == 0) {
    comps <- tibble::as_tibble(stats::setNames(rep(list(double()), length(comp_names)), comp_names))
  } else {
    selected <- famd_align_levels(new_data[, col_names, drop = FALSE], object$levels)
    if (anyNA(selected)) {
      stop(
        "`step_famd()` can't handle missing values in: ",
        paste(names(selected)[vapply(selected, anyNA, logical(1))], collapse = ", "),
        ". Impute them in an earlier step.",
        call. = FALSE
      )
    }
    coord <- as.matrix(stats::predict(object$res, newdata = as.data.frame(selected))$coord)
    coord <- matrix(coord, nrow = nrow(selected))[, seq_len(object$num_comp), drop = FALSE]
    colnames(coord) <- comp_names
    comps <- tibble::as_tibble(coord)
  }

  comps <- recipes::check_name(comps, new_data, object)
  new_data <- vctrs::vec_cbind(new_data, comps)
  recipes::remove_original_cols(new_data, object, col_names)
}

#' @exportS3Method base::print
print.step_famd <- function(x, width = max(20, options()$width - 29), ...) {
  title <- "FAMD extraction with "
  recipes::print_step(x$columns, x$terms, x$trained, title, width, case_weights = x$case_weights)
  invisible(x)
}

#' @rdname step_famd
#' @param x A `step_famd` object.
#' @param type For `tidy()`: `"coef"` (variable contributions) or
#'   `"variance"`.
#' @exportS3Method recipes::tidy
tidy.step_famd <- function(x, type = "coef", ...) {
  type <- rlang::arg_match(type, c("coef", "variance"))
  if (!recipes::is_trained(x)) {
    res <- tibble::tibble(terms = recipes::sel2char(x$terms), value = rlang::na_dbl, component = rlang::na_chr)
  } else if (is.null(x$res)) {
    res <- tibble::tibble(terms = unname(x$columns), value = rlang::na_dbl, component = rlang::na_chr)
  } else if (type == "coef") {
    contrib <- as.matrix(x$res$var$contrib)[, seq_len(x$num_comp), drop = FALSE]
    components <- recipes::names0(x$num_comp, x$prefix)
    res <- tibble::tibble(
      terms = rep(rownames(contrib), times = ncol(contrib)),
      value = as.vector(contrib),
      component = rep(components, each = nrow(contrib))
    )
  } else {
    eig <- x$res$eig
    variance <- unname(eig[, "eigenvalue"])
    k <- length(variance)
    res <- tibble::tibble(
      terms = rep(c("variance", "cumulative variance", "percent variance", "cumulative percent variance"), each = k),
      value = c(variance, cumsum(variance), unname(eig[, "percentage of variance"]), unname(eig[, "cumulative percentage of variance"])),
      component = rep(seq_len(k), times = 4)
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

#' @exportS3Method generics::required_pkgs
required_pkgs.step_famd <- function(x, ...) {
  c("FactoMineR", "TempleCBE")
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

check_famd_args <- function(num_comp, threshold, options, prefix) {
  if (!is.numeric(num_comp) || length(num_comp) != 1 || is.na(num_comp) || num_comp < 0 || num_comp != round(num_comp)) {
    stop("`num_comp` must be a single whole number, 0 or more.", call. = FALSE)
  }
  if (length(threshold) != 1 || (!is.na(threshold) && (!is.numeric(threshold) || threshold <= 0 || threshold > 1))) {
    stop("`threshold` must be NA or a single number in (0, 1].", call. = FALSE)
  }
  if (!is.list(options) || (length(options) && (is.null(names(options)) || any(!nzchar(names(options)))))) {
    stop("`options` must be a named list of arguments to FactoMineR::FAMD().", call. = FALSE)
  }
  reserved <- intersect(names(options), c("base", "graph"))
  if (length(reserved)) {
    stop("`options` can't set ", paste0("`", reserved, "`", collapse = ", "), ".", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1 || !nzchar(prefix)) {
    stop("`prefix` must be a single non-empty string.", call. = FALSE)
  }
  invisible(TRUE)
}

# Character and logical columns become factors, without unused levels.
famd_as_factors <- function(data) {
  data <- as.data.frame(data)
  for (col in names(data)) {
    if (is.character(data[[col]]) || is.logical(data[[col]])) {
      data[[col]] <- factor(data[[col]])
    } else if (is.factor(data[[col]])) {
      data[[col]] <- droplevels(data[[col]])
    }
  }
  data
}

famd_align_levels <- function(data, levels) {
  data <- as.data.frame(data)
  for (col in names(levels)) {
    values <- as.character(data[[col]])
    unseen <- setdiff(unique(values[!is.na(values)]), levels[[col]])
    if (length(unseen)) {
      stop(
        "`step_famd()` found categories not seen in training in `", col, "`: ",
        paste(utils::head(unseen, 10), collapse = ", "),
        ". Collapse rare levels in an earlier step, e.g. step_other() or step_novel().",
        call. = FALSE
      )
    }
    data[[col]] <- factor(values, levels = levels[[col]])
  }
  data
}

famd_max_dims <- function(data, is_quant) {
  n_levels <- vapply(data[!is_quant], nlevels, integer(1))
  as.integer(min(nrow(data) - 1, sum(is_quant) + sum(n_levels - 1)))
}
