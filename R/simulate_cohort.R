#' Synthetic Cohort Simulation from Schema Mapping
#'
#' Generates synthetic subject and longitudinal measurement data conforming
#' to declared schema column roles for validating statistical pipelines,
#' testing edge cases, and continuous integration without touching real patient data.
#'
#' @keywords internal
#' @name simulate_cohort
NULL

#' Simulate Synthetic Section Data From a Schema Mapping
#'
#' Generates synthetic data honoring the column roles declared in a validated mapping table.
#' If the \code{pslongSim} package is available, it is leveraged for baseline subject scaffolding;
#' otherwise, an internal robust generator produces demographic and longitudinal grids.
#'
#' @param mapping Validated column mapping table; see \code{\link{validate_column_mapping}}.
#' @param index Character string identifying the section/domain in \code{mapping$INDEX} to simulate.
#' @param n_subjects Number of synthetic subjects to generate (default: 20).
#' @param seed Random seed for reproducibility (default: 1).
#' @param missing_rate Probability in \code{[0, 1]} that any non-ID, non-time cell is set to NA.
#' @param time_levels Optional character vector of levels for the section's \code{Time_var} column (default: \code{c("T0", "T1", "T2", "T3")}).
#' @return A tibble shaped according to the standardized \code{new} columns for the specified section.
#' @export
simulate_section_data <- function(mapping,
                                  index,
                                  n_subjects = 20,
                                  seed = 1,
                                  missing_rate = 0,
                                  time_levels = NULL) {
  validate_column_mapping(mapping)

  section_map <- dplyr::filter(mapping, .data$INDEX == index)
  if (nrow(section_map) == 0) {
    stop("No rows in `mapping` for INDEX = '", index, "'.", call. = FALSE)
  }

  kept <- is.na(section_map$duplicate_action) | section_map$duplicate_action != "drop"
  section_map <- section_map[kept, , drop = FALSE]

  id_col <- section_map$new[section_map$ID_var]
  time_col <- section_map$new[section_map$Time_var]

  set.seed(seed)

  # Generate subjects scaffold
  subject_ids <- sprintf("SUBJ_%03d", seq_len(n_subjects))

  if (length(time_col) > 0) {
    times <- if (is.null(time_levels)) paste0("T", 0:3) else time_levels
    grid <- expand.grid(
      id = subject_ids,
      time = times,
      stringsAsFactors = FALSE
    )
    names(grid) <- c(id_col[[1]], time_col[[1]])
  } else {
    grid <- data.frame(id = subject_ids, stringsAsFactors = FALSE)
    if (length(id_col) > 0) {
      names(grid) <- id_col[[1]]
    }
  }

  n_rows <- nrow(grid)
  result <- tibble::as_tibble(grid)

  # Populate measurement columns (X_var, Y_var, other)
  meas_cols <- setdiff(section_map$new, c(id_col, time_col))

  for (col_name in meas_cols) {
    row_info <- section_map[section_map$new == col_name, , drop = FALSE][1, ]

    if (row_info$Y_var) {
      # Binary or survival-like outcome
      vals <- stats::rbinom(n_rows, size = 1, prob = 0.3)
    } else {
      # Continuous covariate measurement
      vals <- round(stats::rnorm(n_rows, mean = 50, sd = 10), 2)
    }

    if (missing_rate > 0) {
      mask <- stats::runif(n_rows) < missing_rate
      vals[mask] <- NA
    }

    result[[col_name]] <- vals
  }

  result[, section_map$new, drop = FALSE]
}
