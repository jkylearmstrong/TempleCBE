#' Cox Tie-Handling Configuration and Quiet Scopes
#'
#' Manages tie-handling methods for Cox models (via `glmnet` and `parsnip`)
#' consistently across TempleCBE tuning and evaluation wrappers.
#'
#' @description
#' `glmnet` 5.0 and 5.1 introduced transitional warnings and changed the default
#' Cox tie-handling method from `"breslow"` to `"efron"`. To ensure reproducible
#' results across `glmnet` package updates, TempleCBE pins tie-handling via the
#' package option `templecbe.cox_ties` (defaulting to `"breslow"`).
#'
#' @param expr An expression to evaluate within a quiet tie-handling scope.
#' @param object A `workflow` or `model_spec` to inspect and configure with
#'   pinned Cox tie handling.
#' @return
#' * `get_templecbe_cox_ties()` returns the active tie-handling string (`"breslow"` or `"efron"`).
#' * `with_quiet_cox_ties()` returns the result of evaluating `expr`.
#' * `apply_workflow_cox_ties()` returns the modified `workflow` or `model_spec`.
#' @export
#' @examples
#' get_templecbe_cox_ties()
get_templecbe_cox_ties <- function() {
  getOption("templecbe.cox_ties", "breslow")
}

#' @rdname get_templecbe_cox_ties
#' @keywords internal
#' @noRd
note_cox_ties_once <- function() {
  if (!isTRUE(getOption("templecbe_cox_ties_noted", FALSE))) {
    tie_val <- get_templecbe_cox_ties()
    message(sprintf("TempleCBE: Pinned Cox tie-handling to '%s' (option 'templecbe.cox_ties').", tie_val))
    options(templecbe_cox_ties_noted = TRUE)
  }
}

#' @rdname get_templecbe_cox_ties
#' @export
with_quiet_cox_ties <- function(expr) {
  note_cox_ties_once()
  withCallingHandlers(
    expr,
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("default Cox tie-handling method|Starting in glmnet 5\\.1", msg, ignore.case = TRUE)) {
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("default Cox tie-handling method|Starting in glmnet 5\\.1", msg, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @rdname get_templecbe_cox_ties
#' @export
apply_workflow_cox_ties <- function(object) {
  if (is.null(object)) return(object)

  tie_val <- get_templecbe_cox_ties()

  if (inherits(object, "workflow")) {
    if (!requireNamespace("workflows", quietly = TRUE)) return(object)
    spec <- tryCatch(workflows::extract_spec_parsnip(object), error = function(e) NULL)
    if (!is.null(spec) && inherits(spec, "model_spec")) {
      spec <- apply_spec_cox_ties(spec, tie_val)
      object <- tryCatch(workflows::update_model(object, spec), error = function(e) object)
    }
    return(object)
  }

  if (inherits(object, "model_spec")) {
    return(apply_spec_cox_ties(object, tie_val))
  }

  object
}

apply_spec_cox_ties <- function(spec, tie_val) {
  if (!requireNamespace("parsnip", quietly = TRUE)) return(spec)
  engine <- spec$engine
  mode <- spec$mode

  is_cox <- identical(mode, "censored regression") ||
    engine %in% c("glmnet", "coxnet") ||
    inherits(spec, "proportional_hazards")

  if (is_cox) {
    cur_args <- spec$engine_args
    if (is.null(cur_args) || !"cox.ties" %in% names(cur_args)) {
      new_args <- c(cur_args, list(cox.ties = tie_val))
      spec$engine_args <- new_args
    }
  }
  spec
}
