#' Penalized Cox Regression for Right-Censored or Start/Stop Survival Data
#'
#' Fits an elastic-net penalized Cox proportional hazards model with
#' [glmnet::glmnet()], through the tidymodels \pkg{hardhat} interface: a
#' formula, a recipe, or predictors and outcome given separately. The outcome is
#' a [survival::Surv()] object, either right-censored, `Surv(time, event)`, or
#' counting-process, `Surv(start, stop, event)`, for time-varying covariates.
#'
#' The whole regularization path is fit, as glmnet recommends; `penalty` only
#' sets the default penalty for [predict()][predict.coxnet_model()] and
#' [tidy()][tidy.coxnet_model()]. To choose `penalty` and `mixture` by
#' cross-validation, use [cv_coxnet()].
#'
#' Predictors must be numeric. The formula interface expands factors into one
#' indicator column per level, which suits penalized models; with a recipe, add
#' `recipes::step_dummy()`. In a formula, write `survival::Surv()` unless
#' \pkg{survival} is attached.
#'
#' @param x A data frame or matrix of predictors, or a [recipes::recipe()]
#'   whose outcome is a single `Surv` column.
#' @param y A [survival::Surv()] object with one element per row of `x`.
#' @param formula A formula with a `Surv()` outcome, such as
#'   `survival::Surv(tstart, tstop, status) ~ age + sex`.
#' @param data A data frame containing the variables in `formula` or used by
#'   the recipe.
#' @param penalty Default penalty (glmnet's `lambda`) used by `predict()` and
#'   `tidy()`. A single non-negative number, or `NULL` to require it there.
#' @param mixture Elastic-net mixing parameter (glmnet's `alpha`): 1 is the
#'   lasso, 0 is ridge regression.
#' @param path Optional decreasing sequence of penalties to fit, instead of
#'   glmnet's default path.
#' @param ... Further arguments passed to [glmnet::glmnet()], such as
#'   `nlambda` or `cox.ties`. Not `x`, `y`, `family`, `alpha`, or `lambda`.
#'   `cox.ties` defaults to `"breslow"`, matching the Breslow baseline hazard
#'   used for survival predictions, so results don't depend on glmnet's own
#'   default (Breslow in glmnet 5.0, Efron from 5.1).
#' @return A `coxnet_model` object: the glmnet fit, `penalty`, `mixture`, the
#'   training predictor matrix `x` and outcome `y` (kept so survival can be
#'   predicted at any penalty), and the hardhat `blueprint`.
#' @seealso [predict.coxnet_model()], [tidy.coxnet_model()], [cv_coxnet()],
#'   [nested_cv_coxnet()]
#' @importFrom rlang %||%
#' @importFrom generics tidy required_pkgs
#' @export
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE)) {
#'   lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog", "wt.loss")])
#'   fit <- coxnet(survival::Surv(time, status) ~ ., data = lung, penalty = 0.05, mixture = 0.5)
#'   generics::tidy(fit)
#'   predict(fit, lung[1:3, ], type = "survival", eval_time = c(180, 365))$.pred[[1]]
#' }
coxnet <- function(x, ...) {
  UseMethod("coxnet")
}

#' @rdname coxnet
#' @export
coxnet.default <- function(x, ...) {
  stop(
    "`coxnet()` is not defined for a '", class(x)[1], "'. Pass a data frame or matrix ",
    "with `y`, a formula with `data`, or a recipe with `data`.",
    call. = FALSE
  )
}

#' @rdname coxnet
#' @export
coxnet.data.frame <- function(x, y, penalty = NULL, mixture = 1, path = NULL, ...) {
  processed <- mold_xy(x)
  coxnet_bridge(processed, y, penalty, mixture, path, ...)
}

#' @rdname coxnet
#' @export
coxnet.matrix <- function(x, y, penalty = NULL, mixture = 1, path = NULL, ...) {
  coxnet.data.frame(matrix_to_df(x), y, penalty = penalty, mixture = mixture, path = path, ...)
}

#' @rdname coxnet
#' @export
coxnet.formula <- function(formula, data, penalty = NULL, mixture = 1, path = NULL, ...) {
  processed <- hardhat::mold(formula, data, blueprint = coxnet_formula_blueprint())
  coxnet_bridge(processed, surv_outcome(processed$outcomes), penalty, mixture, path, ...)
}

#' @rdname coxnet
#' @export
coxnet.recipe <- function(x, data, penalty = NULL, mixture = 1, path = NULL, ...) {
  processed <- hardhat::mold(x, data)
  coxnet_bridge(processed, surv_outcome(processed$outcomes), penalty, mixture, path, ...)
}

#' Predict From a `coxnet` Model
#'
#' @param object A [coxnet()] model.
#' @param new_data A data frame (or matrix) of new predictors.
#' @param type `"linear_pred"` for the linear predictor, or `"survival"` for
#'   survival probabilities at `eval_time`.
#' @param penalty The penalty to predict at; defaults to the model's `penalty`.
#' @param eval_time For `type = "survival"`, the times to predict survival at.
#' @param increasing For `type = "linear_pred"`: if `TRUE` (the default, as in
#'   tidymodels' \pkg{censored} package), the sign is flipped so larger values
#'   mean longer survival. Use `FALSE` for glmnet's own sign, where larger values
#'   mean higher risk.
#' @param ... Not used.
#' @return A tibble with one row per row of `new_data`: `.pred_linear_pred`, or
#'   `.pred`, a list-column of tibbles with `.eval_time` and `.pred_survival`.
#'
#' Survival is \eqn{S(t \mid x) = \exp(-H_0(t) e^{x^\top \beta})}, with
#' \eqn{H_0} the Breslow estimate of the cumulative baseline hazard from the
#' training data at `penalty`. Each row's covariates are taken as constant from
#' time 0; to predict along a subject's start/stop covariate path, and score it,
#' use [cv_coxnet()].
#' @export
predict.coxnet_model <- function(object, new_data, type = c("linear_pred", "survival"),
                           penalty = NULL, eval_time = NULL, increasing = TRUE, ...) {
  type <- match.arg(type)
  penalty <- resolve_penalty(object, penalty)
  x <- forge_matrix(new_data, object)
  lp <- coxnet_link(object$fit, x, penalty)[, 1]

  out <- if (type == "linear_pred") {
    tibble::tibble(.pred_linear_pred = if (isTRUE(increasing)) -lp else lp)
  } else {
    if (is.null(eval_time)) {
      stop("`eval_time` is required for `type = \"survival\"`.", call. = FALSE)
    }
    check_eval_time(eval_time, min_length = 1)
    train <- surv_components(object$y)
    bh <- breslow_cumhaz(coxnet_link(object$fit, object$x, penalty), train$start, train$stop, train$status)
    surv <- exp(-outer(exp(lp), cumhaz_at(bh, eval_time)[, 1]))
    tibble::tibble(.pred = lapply(seq_along(lp), function(i) {
      tibble::tibble(.eval_time = eval_time, .pred_survival = surv[i, ])
    }))
  }
  hardhat::validate_prediction_size(out, new_data)
  out
}

#' Tidy the Coefficients of a `coxnet` Model
#'
#' @param x A [coxnet()] model.
#' @param penalty The penalty to report coefficients at; defaults to the
#'   model's `penalty`.
#' @param ... Not used.
#' @return A tibble with `term`, `estimate` (log hazard ratio per unit of the
#'   predictor; 0 for predictors the penalty removed), and `penalty`.
#' @exportS3Method generics::tidy
tidy.coxnet_model <- function(x, penalty = NULL, ...) {
  penalty <- resolve_penalty(x, penalty)
  beta <- as.matrix(stats::coef(x$fit, s = penalty))
  tibble::tibble(term = rownames(beta), estimate = unname(beta[, 1]), penalty = penalty)
}

#' @export
print.coxnet_model <- function(x, ...) {
  type <- attr(x$y, "type")
  cat("<coxnet_model> penalized Cox model (", if (type == "counting") "start/stop" else "right-censored", " outcome)\n", sep = "")
  cat("  rows:", nrow(x$x), " predictors:", ncol(x$x), " events:", sum(surv_components(x$y)$status), "\n")
  cat("  mixture:", format(x$mixture), " penalties on path:", length(x$fit$lambda), "\n")
  cat("  default penalty:", if (is.null(x$penalty)) "none (pass `penalty` to predict())" else format(x$penalty, digits = 4), "\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

coxnet_bridge <- function(processed, y, penalty, mixture, path, ...) {
  rlang::check_installed(c("glmnet", "survival"), reason = "to fit `coxnet()` models.")
  x <- predictors_matrix(processed$predictors)
  check_coxnet_data(x, y)
  check_mixture(mixture, single = TRUE)
  if (!is.null(penalty)) check_single_penalty(penalty)
  path <- check_path(path)
  glmnet_args <- check_glmnet_args(list(...))

  fit <- fit_glmnet_cox(x, y, mixture, path, glmnet_args)
  new_coxnet(fit, penalty, mixture, x, y, processed$blueprint)
}

new_coxnet <- function(fit, penalty, mixture, x, y, blueprint) {
  hardhat::new_model(
    fit = fit, penalty = penalty, mixture = mixture, x = x, y = y,
    blueprint = blueprint, class = "coxnet_model"
  )
}

fit_glmnet_cox <- function(x, y, mixture, path, glmnet_args) {
  # Breslow ties by default: it matches the Breslow baseline hazard used for
  # survival predictions, and doesn't change with glmnet's default (Breslow
  # in 5.0, Efron from 5.1).
  if (is.null(glmnet_args$cox.ties) && "cox.ties" %in% names(formals(glmnet::glmnet))) {
    glmnet_args$cox.ties <- "breslow"
  }
  do.call(glmnet::glmnet, c(list(x = x, y = y, family = "cox", alpha = mixture, lambda = path), glmnet_args))
}

coxnet_formula_blueprint <- function() {
  hardhat::default_formula_blueprint(intercept = FALSE, indicators = "one_hot")
}

# A data frame of predictors, molded through a recipe that treats every column
# as a predictor, so new data is checked and ordered the same way.
mold_xy <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or matrix of predictors.", call. = FALSE)
  }
  rec <- recipes::recipe(x, vars = names(x), roles = rep("predictor", ncol(x)))
  hardhat::mold(rec, x)
}

matrix_to_df <- function(x) {
  if (is.null(colnames(x))) colnames(x) <- paste0("x", seq_len(ncol(x)))
  as.data.frame(x, stringsAsFactors = FALSE)
}

predictors_matrix <- function(predictors) {
  numeric_cols <- vapply(predictors, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop(
      "Predictors must be numeric; not numeric: ", paste(names(predictors)[!numeric_cols], collapse = ", "),
      ". Use the formula interface, or a recipe with recipes::step_dummy().",
      call. = FALSE
    )
  }
  x <- as.matrix(predictors)
  storage.mode(x) <- "double"
  x
}

forge_matrix <- function(new_data, object) {
  if (is.matrix(new_data)) new_data <- matrix_to_df(new_data)
  forged <- hardhat::forge(new_data, object$blueprint)
  x <- predictors_matrix(forged$predictors)
  x[, colnames(object$x), drop = FALSE]
}

surv_outcome <- function(outcomes) {
  if (ncol(outcomes) != 1 || !inherits(outcomes[[1]], "Surv")) {
    stop(
      "The outcome must be a single survival::Surv() column. With a recipe, create it first, ",
      "e.g. `data$surv <- survival::Surv(tstart, tstop, status)`, then `recipe(surv ~ ., data)`.",
      call. = FALSE
    )
  }
  outcomes[[1]]
}

check_coxnet_data <- function(x, y) {
  parts <- surv_components(y, arg = "y")
  if (nrow(x) != length(parts$stop)) {
    stop("The predictors have ", nrow(x), " rows but the outcome has ", length(parts$stop), ".", call. = FALSE)
  }
  if (ncol(x) < 2) {
    stop("glmnet needs at least two predictor columns; got ", ncol(x), ".", call. = FALSE)
  }
  if (anyNA(x) || anyNA(parts$stop) || anyNA(parts$status) || anyNA(parts$start)) {
    stop("Predictors and outcome must not contain missing values; impute or drop them first.", call. = FALSE)
  }
  if (any(parts$stop <= parts$start)) {
    stop("Every interval must have `stop > start`.", call. = FALSE)
  }
  if (!any(parts$status == 1)) {
    stop("The outcome has no events.", call. = FALSE)
  }
  invisible(TRUE)
}

check_mixture <- function(mixture, single = FALSE) {
  ok <- is.numeric(mixture) && length(mixture) >= 1 && !anyNA(mixture) && all(mixture >= 0 & mixture <= 1)
  if (!ok || (single && length(mixture) != 1)) {
    stop("`mixture` must be ", if (single) "a single number" else "numbers", " between 0 and 1.", call. = FALSE)
  }
  sort(unique(mixture))
}

check_single_penalty <- function(penalty) {
  if (!is.numeric(penalty) || length(penalty) != 1 || is.na(penalty) || penalty < 0) {
    stop("`penalty` must be a single non-negative number.", call. = FALSE)
  }
  penalty
}

check_path <- function(path) {
  if (is.null(path)) return(NULL)
  if (!is.numeric(path) || !length(path) || anyNA(path) || any(path < 0)) {
    stop("A penalty path must be non-negative numbers.", call. = FALSE)
  }
  sort(unique(path), decreasing = TRUE)
}

check_glmnet_args <- function(args) {
  reserved <- intersect(names(args), c("x", "y", "family", "alpha", "lambda", "weights", "offset"))
  if (length(reserved)) {
    stop(
      "Don't pass ", paste0("`", reserved, "`", collapse = ", "), " to glmnet here: ",
      "use `mixture` for alpha and `penalty`/`path` for lambda.",
      call. = FALSE
    )
  }
  args
}

resolve_penalty <- function(object, penalty) {
  penalty <- penalty %||% object$penalty
  if (is.null(penalty)) {
    stop("Pass `penalty`: this model was fit without a default penalty.", call. = FALSE)
  }
  check_single_penalty(penalty)
}

coxnet_link <- function(fit, x, penalty) {
  lp <- stats::predict(fit, newx = x, s = penalty, type = "link")
  matrix(as.numeric(lp), nrow = nrow(x))
}

cumhaz_at <- function(bh, times) {
  rbind(0, bh$cumhaz)[findInterval(times, bh$time) + 1, , drop = FALSE]
}
