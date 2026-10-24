#' Survival Probabilities From a Tidymodels Workflow as a Matrix (for survex)
#'
#' `survex::explain_survival()` wants a function that returns an N by T matrix of survival
#' probabilities, one row per subject and one column per time. A tidymodels censored-regression workflow
#' returns a nested tibble instead (`predict(type = "survival", eval_time = times)`). This unnests it and
#' checks the shape, so it can be used as `predict_survival_function`: pass a function of
#' `(model, newdata, times)` that calls `adapter_tidymodels_survex(model, newdata, times)`.
#'
#' @param workflow A fitted tidymodels workflow (or any object whose `predict()` method accepts
#'   `new_data`, `type = "survival"` and `eval_time`).
#' @param new_data Data frame of subjects.
#' @param times Numeric vector of evaluation times (finite, non-negative, increasing; duplicates are
#'   not allowed). The columns of the result follow this order.
#' @return A numeric matrix with `nrow(new_data)` rows and `length(times)` columns, column names the times
#'   as text. Values are probabilities between 0 and 1; a time a model cannot predict is an error, not `NA`.
#' @seealso [explain_survival()]
#' @export
adapter_tidymodels_survex <- function(workflow, new_data, times) {
  if (!is.data.frame(new_data)) stop("`new_data` must be a data frame.", call. = FALSE)
  if (!is.numeric(times) || length(times) == 0L || anyNA(times) || any(!is.finite(times)) || any(times < 0)) {
    stop("`times` must be finite, non-negative numbers.", call. = FALSE)
  }
  if (anyDuplicated(times) || is.unsorted(times)) {
    stop("`times` must be strictly increasing.", call. = FALSE)
  }
  pred <- stats::predict(workflow, new_data = new_data, type = "survival", eval_time = times)
  if (!is.data.frame(pred) || !".pred" %in% names(pred)) {
    stop("`predict(type = \"survival\")` did not return a `.pred` column; is this a censored-regression workflow?",
         call. = FALSE)
  }
  if (nrow(pred) != nrow(new_data)) {
    stop("The workflow returned ", nrow(pred), " predictions for ", nrow(new_data), " rows of `new_data`.", call. = FALSE)
  }
  mat <- matrix(NA_real_, nrow = nrow(pred), ncol = length(times), dimnames = list(NULL, as.character(times)))
  for (i in seq_len(nrow(pred))) {
    pr <- pred$.pred[[i]]
    if (!is.data.frame(pr) || !all(c(".eval_time", ".pred_survival") %in% names(pr))) {
      stop("Prediction ", i, " lacks `.eval_time` or `.pred_survival`.", call. = FALSE)
    }
    pos <- match(times, pr$.eval_time)
    if (anyNA(pos)) {
      stop("The workflow gave no prediction for time(s): ", paste(times[is.na(pos)], collapse = ", "), ".", call. = FALSE)
    }
    mat[i, ] <- pr$.pred_survival[pos]
  }
  if (anyNA(mat) || any(mat < 0 | mat > 1)) {
    stop("Survival probabilities must be between 0 and 1 and not missing.", call. = FALSE)
  }
  mat
}
