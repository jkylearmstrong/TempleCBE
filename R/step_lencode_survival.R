#' Supervised Linear Encoding of Factors via Penalized Cox Models
#'
#' \code{step_lencode_coxnet} creates a \emph{specification} of a recipe step that
#' encodes categorical predictors into a single numeric variable representing the
#' estimated log hazard ratio from a penalized Cox proportional hazards model
#' on a survival outcome (\code{Surv(time, status)} or \code{Surv(tstart, tstop, status)}).
#' Inspired by \code{embed::step_lencode_glm}.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose variables to be encoded.
#'   Must select factor or character columns.
#' @param outcome A call to \code{recipes::vars()} selecting the survival outcome columns,
#'   e.g. \code{recipes::vars(time, status)} or \code{recipes::vars(tstart, tstop, status)}.
#' @param role For model terms created by this step, what analysis role should they
#'   be assigned? By default, the function assumes that the new variables will be
#'   used as predictors in a model.
#' @param trained A logical indicating whether the recipe has been trained.
#' @param penalty A non-negative numeric value specifying the L1/L2 penalty for \code{coxnet}.
#'   Default is 0.05.
#' @param mixture Elastic net mixing parameter between 0 and 1. Default is 1 (Lasso).
#' @param mapping A named list of numeric vectors containing the level-to-score mappings,
#'   generated during \code{prep()}.
#' @param skip A logical indicating whether the step should be skipped when the
#'   recipe is baked.
#' @param id A unique identifier for the step.
#'
#' @return An updated version of \code{recipe} with the new step added.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("recipes", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
#'   data(lung, package = "survival")
#'   lung_df <- na.omit(lung[, c("time", "status", "sex", "ph.ecog")])
#'   lung_df$ph.ecog <- factor(lung_df$ph.ecog)
#'   lung_df$sex <- factor(lung_df$sex)
#'
#'   rec <- recipes::recipe(time + status ~ ., data = lung_df) |>
#'     step_lencode_coxnet(ph.ecog, sex, outcome = recipes::vars(time, status))
#'   prepped <- recipes::prep(rec)
#'   baked <- recipes::bake(prepped, new_data = NULL)
#' }
#' }
step_lencode_coxnet <- function(recipe,
                                ...,
                                outcome = recipes::vars(time, status),
                                role = "predictor",
                                trained = FALSE,
                                penalty = 0.05,
                                mixture = 1,
                                mapping = NULL,
                                skip = FALSE,
                                id = recipes::rand_id("lencode_coxnet")) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("Package 'recipes' is required for step_lencode_coxnet().", call. = FALSE)
  }

  recipes::add_step(
    recipe,
    step_lencode_coxnet_new(
      terms = rlang::enquos(...),
      outcome = outcome,
      role = role,
      trained = trained,
      penalty = penalty,
      mixture = mixture,
      mapping = mapping,
      skip = skip,
      id = id
    )
  )
}

step_lencode_coxnet_new <- function(terms, outcome, role, trained, penalty, mixture,
                                    mapping, skip, id) {
  recipes::step(
    subclass = "lencode_coxnet",
    terms = terms,
    outcome = outcome,
    role = role,
    trained = trained,
    penalty = penalty,
    mixture = mixture,
    mapping = mapping,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_lencode_coxnet <- function(x, training, info = NULL, ...) {
  # 1. Resolve predictor terms
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  if (length(col_names) == 0) {
    return(step_lencode_coxnet_new(
      terms = x$terms,
      outcome = x$outcome,
      role = x$role,
      trained = TRUE,
      penalty = x$penalty,
      mixture = x$mixture,
      mapping = list(),
      skip = x$skip,
      id = x$id
    ))
  }

  # 2. Resolve outcome columns
  outcome_names <- recipes::recipes_eval_select(x$outcome, training, info)
  if (length(outcome_names) < 2) {
    stop("`outcome` in step_lencode_coxnet must specify at least two columns (e.g. time, status).", call. = FALSE)
  }

  # Build Surv object from outcome columns
  surv_obj <- if (length(outcome_names) == 2) {
    survival::Surv(training[[outcome_names[1]]], training[[outcome_names[2]]])
  } else {
    survival::Surv(training[[outcome_names[1]]], training[[outcome_names[2]]], training[[outcome_names[3]]])
  }

  # 3. Fit Coxnet for each categorical predictor to learn level log hazard ratios
  mapping <- list()
  for (col in col_names) {
    vals <- training[[col]]
    if (!is.factor(vals)) vals <- factor(vals)
    levs <- levels(vals)

    if (length(levs) <= 1) {
      map_vec <- stats::setNames(0, levs)
      mapping[[col]] <- map_vec
      next
    }

    # Model matrix with contrast coding
    mm <- stats::model.matrix(~ vals)[, -1, drop = FALSE]
    coefs <- tryCatch({
      fit <- glmnet::glmnet(
        x = mm,
        y = surv_obj,
        family = "cox",
        alpha = x$mixture,
        lambda = x$penalty
      )
      as.numeric(fit$beta[, 1])
    }, error = function(e) {
      rep(0, ncol(mm))
    })

    # Reference level gets 0
    map_vec <- stats::setNames(c(0, coefs), levs)
    mapping[[col]] <- map_vec
  }

  step_lencode_coxnet_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    penalty = x$penalty,
    mixture = x$mixture,
    mapping = mapping,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lencode_coxnet <- function(object, new_data, ...) {
  col_names <- names(object$mapping)
  for (col in col_names) {
    map_vec <- object$mapping[[col]]
    raw_vals <- as.character(new_data[[col]])
    encoded <- map_vec[raw_vals]

    # Impute unknown or NA levels with baseline (0)
    encoded[is.na(encoded)] <- 0
    new_data[[col]] <- as.numeric(encoded)
  }

  new_data
}

#' @export
print.step_lencode_coxnet <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Penalized Cox linear encoding for "
  recipes::print_step(names(x$mapping), x$terms, x$trained, title, width)
  invisible(x)
}

#' @export
tidy.step_lencode_coxnet <- function(x, ...) {
  if (recipes::is_trained(x)) {
    if (length(x$mapping) == 0) {
      res <- tibble::tibble(terms = character(), level = character(), value = numeric(), id = character())
    } else {
      res_list <- lapply(names(x$mapping), function(col) {
        mv <- x$mapping[[col]]
        tibble::tibble(
          terms = col,
          level = names(mv),
          value = as.numeric(mv),
          id = x$id
        )
      })
      res <- do.call(rbind, res_list)
    }
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = term_names, level = NA_character_, value = NA_real_, id = x$id)
  }
  res
}

#' @export
tunable.step_lencode_coxnet <- function(x, ...) {
  tibble::tibble(
    name = c("penalty", "mixture"),
    call_info = list(
      list(pkg = "dials", fun = "penalty"),
      list(pkg = "dials", fun = "mixture")
    ),
    source = "recipe",
    component = "step_lencode_coxnet",
    component_id = x$id
  )
}

#' @export
required_pkgs.step_lencode_coxnet <- function(x, ...) {
  c("TempleCBE", "glmnet", "survival")
}


#' Supervised Linear Encoding of Factors via Joint Survival-Status-Time Model
#'
#' \code{step_lencode_joint_model} creates a \emph{specification} of a recipe step that
#' encodes categorical predictors into a composite numeric risk score learned from
#' a \code{joint_model} blending survival, binary status classification, and follow-up duration.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose variables to be encoded.
#' @param outcome A call to \code{recipes::vars()} selecting the survival outcome columns.
#' @param role Role for the encoded variables. Default is \code{"predictor"}.
#' @param trained A logical indicating whether the step has been trained.
#' @param engine Engine passed to \code{joint_model()}: \code{"glmnet"}, \code{"baguette"}, or \code{"stacks"}.
#' @param penalty Penalty parameter for the model. Default is 0.05.
#' @param mapping A named list of level-to-score mappings generated during \code{prep()}.
#' @param skip Logical; skip step when baking? Default is \code{FALSE}.
#' @param id A unique identifier for the step.
#'
#' @return An updated version of \code{recipe}.
#' @export
step_lencode_joint_model <- function(recipe,
                                    ...,
                                    outcome = recipes::vars(time, status),
                                    role = "predictor",
                                    trained = FALSE,
                                    engine = "glmnet",
                                    penalty = 0.05,
                                    mapping = NULL,
                                    skip = FALSE,
                                    id = recipes::rand_id("lencode_joint_model")) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("Package 'recipes' is required for step_lencode_joint_model().", call. = FALSE)
  }

  recipes::add_step(
    recipe,
    step_lencode_joint_model_new(
      terms = rlang::enquos(...),
      outcome = outcome,
      role = role,
      trained = trained,
      engine = engine,
      penalty = penalty,
      mapping = mapping,
      skip = skip,
      id = id
    )
  )
}

step_lencode_joint_model_new <- function(terms, outcome, role, trained, engine,
                                         penalty, mapping, skip, id) {
  recipes::step(
    subclass = "lencode_joint_model",
    terms = terms,
    outcome = outcome,
    role = role,
    trained = trained,
    engine = engine,
    penalty = penalty,
    mapping = mapping,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_lencode_joint_model <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  if (length(col_names) == 0) {
    return(step_lencode_joint_model_new(
      terms = x$terms,
      outcome = x$outcome,
      role = x$role,
      trained = TRUE,
      engine = x$engine,
      penalty = x$penalty,
      mapping = list(),
      skip = x$skip,
      id = x$id
    ))
  }

  outcome_names <- recipes::recipes_eval_select(x$outcome, training, info)
  if (length(outcome_names) < 2) {
    stop("`outcome` in step_lencode_joint_model must specify outcome columns.", call. = FALSE)
  }

  mapping <- list()
  for (col in col_names) {
    vals <- training[[col]]
    if (!is.factor(vals)) vals <- factor(vals)
    levs <- levels(vals)

    if (length(levs) <= 1) {
      mapping[[col]] <- stats::setNames(0, levs)
      next
    }

    # Build sub-data frame for single categorical predictor
    sub_df <- training[, outcome_names, drop = FALSE]
    sub_df[[col]] <- vals

    surv_formula <- if (length(outcome_names) == 2) {
      stats::as.formula(paste0("survival::Surv(", outcome_names[1], ", ", outcome_names[2], ") ~ ", col))
    } else {
      stats::as.formula(paste0("survival::Surv(", outcome_names[1], ", ", outcome_names[2], ", ", outcome_names[3], ") ~ ", col))
    }

    # Only the Cox component's .pred_risk_score is used below, so calibration
    # (which needs the `probably` package and fits extra classification/
    # regression models we never look at) is switched off.
    jm_fit <- tryCatch({
      joint_model(
        sub_df,
        outcome = surv_formula,
        engine = x$engine,
        penalty = x$penalty,
        calibration = FALSE
      )
    }, error = function(e) {
      warning(
        "step_lencode_joint_model(): failed to fit a joint_model() for column '", col,
        "'; encoding all levels to 0. Original error: ", conditionMessage(e),
        call. = FALSE
      )
      NULL
    })

    if (is.null(jm_fit)) {
      mapping[[col]] <- stats::setNames(rep(0, length(levs)), levs)
      next
    }

    # Evaluate predicted composite risk on a 1-row data frame per level
    level_df <- data.frame(level_col = factor(levs, levels = levs))
    names(level_df) <- col
    preds <- stats::predict(jm_fit, new_data = level_df)
    risk_scores <- as.numeric(preds$.pred_risk_score)

    # Normalize relative to baseline level
    map_vec <- stats::setNames(risk_scores - risk_scores[1], levs)
    mapping[[col]] <- map_vec
  }

  step_lencode_joint_model_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    engine = x$engine,
    penalty = x$penalty,
    mapping = mapping,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lencode_joint_model <- function(object, new_data, ...) {
  col_names <- names(object$mapping)
  for (col in col_names) {
    map_vec <- object$mapping[[col]]
    raw_vals <- as.character(new_data[[col]])
    encoded <- map_vec[raw_vals]
    encoded[is.na(encoded)] <- 0
    new_data[[col]] <- as.numeric(encoded)
  }
  new_data
}

#' @export
print.step_lencode_joint_model <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Joint model (", x$engine, ") linear encoding for ")
  recipes::print_step(names(x$mapping), x$terms, x$trained, title, width)
  invisible(x)
}

#' @export
tidy.step_lencode_joint_model <- function(x, ...) {
  if (recipes::is_trained(x)) {
    if (length(x$mapping) == 0) {
      res <- tibble::tibble(terms = character(), level = character(), value = numeric(), id = character())
    } else {
      res_list <- lapply(names(x$mapping), function(col) {
        mv <- x$mapping[[col]]
        tibble::tibble(
          terms = col,
          level = names(mv),
          value = as.numeric(mv),
          id = x$id
        )
      })
      res <- do.call(rbind, res_list)
    }
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = term_names, level = NA_character_, value = NA_real_, id = x$id)
  }
  res
}

#' @export
required_pkgs.step_lencode_joint_model <- function(x, ...) {
  c("TempleCBE", "glmnet", "survival")
}
