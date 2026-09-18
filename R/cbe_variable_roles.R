#' Define and Manage Clinical Variable Roles
#'
#' Assigns functional roles (e.g. predictor, outcome, identifier, time variable,
#' strata, weights) to variables across a single data frame or an entire R database list.
#' Produces standardized schemas compatible with \code{\link{validate_column_mapping}},
#' \pkg{recipes}, and modeling workflows.
#'
#' @param data A data frame or a named list of data frames (an R database).
#' @param id Character vector of column names acting as subject/patient/cluster identifiers.
#' @param outcome Character vector of column names acting as response/outcomes (\code{Y_var}).
#' @param time Character vector of column names acting as longitudinal visit/time variables (\code{Time_var}).
#' @param predictors Character vector of column names acting as features/predictors (\code{X_var}).
#' @param strata Character vector of column names used for stratification or subgroup analysis.
#' @param weight Character vector of column names used for case weights or survey weights.
#' @param ignore Character vector of column names to ignore/exclude from modeling.
#' @param roles Optional named character vector or list mapping column names to role names.
#' @return A tibble with columns \code{columns}, \code{role}, \code{X_var}, \code{Y_var},
#'   \code{ID_var}, \code{Time_var}, and \code{dataset_name} (when multi-table).
#' @name cbe_variable_roles
#' @export
#' @examples
#' df <- data.frame(id = 1:5, time = 1:5, sbp = c(120, 130, 115, 140, 125), death = c(0, 0, 1, 0, 1))
#' roles <- cbe_variable_roles(df, id = "id", time = "time", outcome = "death", predictors = "sbp")
cbe_variable_roles <- function(data,
                               id = NULL,
                               outcome = NULL,
                               time = NULL,
                               predictors = NULL,
                               strata = NULL,
                               weight = NULL,
                               ignore = NULL,
                               roles = NULL) {
  is_list_db <- is.list(data) && !is.data.frame(data)

  if (is_list_db) {
    tbl_names <- names(data) %||% paste0("Table", seq_along(data))
    res_list <- lapply(seq_along(data), function(i) {
      nm <- tbl_names[[i]]
      df <- data[[i]]
      if (!is.data.frame(df)) return(NULL)

      # Extract per-table role specs if lists were passed. A named list is a
      # per-table map: use this table's entry, or NULL if it has none. A
      # plain (non-list) spec applies uniformly to every table.
      pick_role <- function(spec) {
        if (is.list(spec)) {
          if (nm %in% names(spec)) spec[[nm]] else NULL
        } else {
          spec
        }
      }

      cur_res <- cbe_variable_roles(
        data = df,
        id = pick_role(id),
        outcome = pick_role(outcome),
        time = pick_role(time),
        predictors = pick_role(predictors),
        strata = pick_role(strata),
        weight = pick_role(weight),
        ignore = pick_role(ignore),
        roles = pick_role(roles)
      )
      cur_res$dataset_name <- nm
      dplyr::relocate(cur_res, "dataset_name")
    })
    return(dplyr::bind_rows(res_list))
  }

  cols <- colnames(data)
  if (is.null(cols) || length(cols) == 0) {
    return(tibble::tibble(
      columns = character(),
      role = character(),
      X_var = logical(),
      Y_var = logical(),
      ID_var = logical(),
      Time_var = logical()
    ))
  }

  # Build role lookup
  role_vec <- stats::setNames(rep("unknown", length(cols)), cols)

  if (!is.null(id)) {
    valid_id <- intersect(id, cols)
    role_vec[valid_id] <- "id"
  }
  if (!is.null(outcome)) {
    valid_outcome <- intersect(outcome, cols)
    role_vec[valid_outcome] <- "outcome"
  }
  if (!is.null(time)) {
    valid_time <- intersect(time, cols)
    role_vec[valid_time] <- "time"
  }
  if (!is.null(predictors)) {
    valid_pred <- intersect(predictors, cols)
    role_vec[valid_pred] <- "predictor"
  }
  if (!is.null(strata)) {
    valid_strata <- intersect(strata, cols)
    role_vec[valid_strata] <- "strata"
  }
  if (!is.null(weight)) {
    valid_wt <- intersect(weight, cols)
    role_vec[valid_wt] <- "weight"
  }
  if (!is.null(ignore)) {
    valid_ign <- intersect(ignore, cols)
    role_vec[valid_ign] <- "ignore"
  }

  if (!is.null(roles)) {
    for (cn in names(roles)) {
      if (cn %in% cols) {
        role_vec[cn] <- as.character(roles[[cn]])
      }
    }
  }

  # Fill remaining unknown as predictor if not explicitly classified
  tibble::tibble(
    columns = cols,
    role = unname(role_vec[cols]),
    X_var = role_vec[cols] %in% c("predictor", "unknown"),
    Y_var = role_vec[cols] == "outcome",
    ID_var = role_vec[cols] == "id",
    Time_var = role_vec[cols] == "time"
  )
}

#' @rdname cbe_variable_roles
#' @export
cbe_set_roles <- function(data, ...) {
  role_tbl <- cbe_variable_roles(data, ...)
  apply_database_metadata(data, role_tbl)
}

#' @rdname cbe_variable_roles
#' @export
cbe_get_roles <- function(data) {
  if (is.list(data) && !is.data.frame(data)) {
    cbe_variable_roles(data)
  } else {
    attr(data, "variable_roles", exact = TRUE) %||% cbe_variable_roles(data)
  }
}

#' @rdname cbe_variable_roles
#' @param roles Role table (e.g. from \code{\link{cbe_variable_roles}}) or data object.
#' @export
cbe_get_predictors <- function(roles) {
  if (!is.data.frame(roles)) roles <- cbe_get_roles(roles)
  roles$columns[roles$X_var | roles$role == "predictor"]
}

#' @rdname cbe_variable_roles
#' @export
cbe_get_outcomes <- function(roles) {
  if (!is.data.frame(roles)) roles <- cbe_get_roles(roles)
  roles$columns[roles$Y_var | roles$role == "outcome"]
}

#' @rdname cbe_variable_roles
#' @export
cbe_get_id_cols <- function(roles) {
  if (!is.data.frame(roles)) roles <- cbe_get_roles(roles)
  roles$columns[roles$ID_var | roles$role == "id"]
}

#' @rdname cbe_variable_roles
#' @export
cbe_get_time_cols <- function(roles) {
  if (!is.data.frame(roles)) roles <- cbe_get_roles(roles)
  roles$columns[roles$Time_var | roles$role == "time"]
}
