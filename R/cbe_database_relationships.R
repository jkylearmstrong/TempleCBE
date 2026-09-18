#' Analyze Relationships and Linkages Across Datasets in an R Database
#'
#' Evaluates common key variables (e.g., subject identifiers, visit times) across
#' multiple datasets in an R database list. Computes cross-table overlap, record
#' coverage, key cardinality (1:1, 1:many, many:many), and identifies orphan records.
#'
#' @param db A named list of data frames (an R database).
#' @param id_cols Optional character vector of key/ID column names to evaluate. If \code{NULL},
#'   shared keys are automatically detected across datasets.
#' @return A tibble summarizing dataset pairs, shared key columns, record counts,
#'   overlap counts, coverage percentages, and cardinality.
#' @name cbe_database_relationships
#' @export
#' @examples
#' db <- list(
#'   patients = data.frame(id = 1:5, age = c(20, 30, 40, 50, 60)),
#'   vitals = data.frame(id = c(1, 1, 2, 2, 3, 6), bp = c(120, 122, 130, 132, 110, 115))
#' )
#' cbe_database_relationships(db)
cbe_database_relationships <- function(db, id_cols = NULL) {
  if (!is.list(db) || is.data.frame(db)) {
    stop("`db` must be a named list of data frames (an R database).", call. = FALSE)
  }

  tbl_names <- names(db)
  if (is.null(tbl_names) || length(tbl_names) < 2) {
    return(tibble::tibble(
      from_table = character(),
      to_table = character(),
      key_column = character(),
      from_rows = integer(),
      to_rows = integer(),
      from_unique_keys = integer(),
      to_unique_keys = integer(),
      shared_keys = integer(),
      from_coverage_pct = numeric(),
      to_coverage_pct = numeric(),
      cardinality = character()
    ))
  }

  shared_key_info <- cbe_find_shared_keys(db)
  keys_to_eval <- if (!is.null(id_cols)) {
    id_cols
  } else {
    unique(shared_key_info$column)
  }

  if (length(keys_to_eval) == 0) {
    return(tibble::tibble(
      from_table = character(),
      to_table = character(),
      key_column = character(),
      from_rows = integer(),
      to_rows = integer(),
      from_unique_keys = integer(),
      to_unique_keys = integer(),
      shared_keys = integer(),
      from_coverage_pct = numeric(),
      to_coverage_pct = numeric(),
      cardinality = character()
    ))
  }

  results <- list()

  for (k in keys_to_eval) {
    # Tables containing this key
    tables_with_key <- names(db)[vapply(db, function(d) is.data.frame(d) && k %in% names(d), logical(1))]
    if (length(tables_with_key) < 2) next

    pairs <- utils::combn(tables_with_key, 2, simplify = FALSE)
    for (p in pairs) {
      t1_nm <- p[1]
      t2_nm <- p[2]
      t1 <- db[[t1_nm]]
      t2 <- db[[t2_nm]]

      k1 <- stats::na.omit(t1[[k]])
      k2 <- stats::na.omit(t2[[k]])

      u1 <- unique(k1)
      u2 <- unique(k2)

      shared <- intersect(u1, u2)

      is_t1_unique <- length(k1) == length(u1)
      is_t2_unique <- length(k2) == length(u2)

      cardinality <- if (is_t1_unique && is_t2_unique) {
        "1:1"
      } else if (is_t1_unique && !is_t2_unique) {
        "1:Many"
      } else if (!is_t1_unique && is_t2_unique) {
        "Many:1"
      } else {
        "Many:Many"
      }

      from_cov <- if (length(u1) > 0) round(100 * length(shared) / length(u1), 1) else 0
      to_cov <- if (length(u2) > 0) round(100 * length(shared) / length(u2), 1) else 0

      results[[length(results) + 1]] <- tibble::tibble(
        from_table = t1_nm,
        to_table = t2_nm,
        key_column = k,
        from_rows = nrow(t1),
        to_rows = nrow(t2),
        from_unique_keys = length(u1),
        to_unique_keys = length(u2),
        shared_keys = length(shared),
        from_coverage_pct = from_cov,
        to_coverage_pct = to_cov,
        cardinality = cardinality
      )
    }
  }

  out <- dplyr::bind_rows(results)
  attr(out, "database_tables") <- names(db)
  class(out) <- c("cbe_database_relationships", class(out))
  out
}

#' @rdname cbe_database_relationships
#' @param x A \code{cbe_database_relationships} object.
#' @param ... Additional arguments passed to methods.
#' @export
as_tbl_graph.cbe_database_relationships <- function(x, ...) {
  all_tables <- attr(x, "database_tables", exact = TRUE) %||% unique(c(x$from_table, x$to_table))
  nodes <- tibble::tibble(name = all_tables)
  if (nrow(x) == 0) {
    return(tidygraph::tbl_graph(nodes = nodes, edges = tibble::tibble(from = integer(), to = integer())))
  }
  edges <- dplyr::rename(x, from = .data$from_table, to = .data$to_table)
  tidygraph::tbl_graph(nodes = nodes, edges = edges)
}

#' @export
as_igraph.cbe_database_relationships <- function(x, ...) {
  tg <- as_tbl_graph.cbe_database_relationships(x, ...)
  igraph::as.igraph(tg)
}

#' @export
plot.cbe_database_relationships <- function(x, ...) {
  if (nrow(x) == 0) {
    message("Empty relationship graph.")
    return(invisible(NULL))
  }
  g <- as_igraph.cbe_database_relationships(x, ...)
  igraph::plot.igraph(
    g,
    edge.label = paste0(igraph::E(g)$key_column, "\n(", igraph::E(g)$cardinality, ")"),
    vertex.color = "#a41e35",
    vertex.label.color = "white",
    vertex.frame.color = "#005a70",
    vertex.size = 28,
    edge.color = "#005a70",
    edge.arrow.size = 0.5,
    ...
  )
}

#' Multi-Dataset Key Overlap Venn Diagram
#'
#' Visualizes shared subject identifiers or key overlap across all tables
#' in an R database list using \pkg{ggVennDiagram} (or native ggplot2 fallback).
#'
#' @param db A named list of data frames (an R database).
#' @param id_col Character string specifying the identifier column to intersect across datasets.
#' @param title Optional plot title.
#' @param ... Additional arguments passed to \code{\link[ggVennDiagram]{ggVennDiagram}}.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @name cbe_database_venn
#' @export
#' @examples
#' \dontrun{
#' db <- list(
#'   inputs = data.frame(id = 1:5),
#'   abg = data.frame(id = 1:3),
#'   survival = data.frame(id = 2:6)
#' )
#' cbe_database_venn(db, id_col = "id")
#' }
cbe_database_venn <- function(db, id_col = "id", title = NULL, ...) {
  if (!is.list(db) || is.data.frame(db)) {
    stop("`db` must be a named list of data frames (an R database).", call. = FALSE)
  }

  tables_with_id <- names(db)[vapply(db, function(d) is.data.frame(d) && id_col %in% names(d), logical(1))]
  if (length(tables_with_id) < 2) {
    stop("Key column '", id_col, "' found in fewer than 2 datasets in `db`.", call. = FALSE)
  }

  set_list <- lapply(tables_with_id, function(nm) {
    unique(stats::na.omit(as.character(db[[nm]][[id_col]])))
  })
  names(set_list) <- tables_with_id

  if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
    p <- ggVennDiagram::ggVennDiagram(set_list, label_alpha = 0, ...) +
      ggplot2::scale_fill_gradient(low = "#e8f8f5", high = "#a41e35") +
      ggplot2::labs(
        title = title %||% paste0("Cohort Identifier Overlap (Key: `", id_col, "`)"),
        subtitle = paste0("Evaluated across ", length(tables_with_id), " database tables")
      ) +
      theme_cbe()
    return(p)
  }

  # Native ggplot2 fallback
  all_keys <- unique(unlist(set_list))
  matrix_df <- do.call(rbind, lapply(names(set_list), function(nm) {
    data.frame(
      dataset = nm,
      id = all_keys,
      present = all_keys %in% set_list[[nm]]
    )
  }))

  p <- ggplot2::ggplot(matrix_df, ggplot2::aes(x = .data$id, y = .data$dataset, fill = .data$present)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#f2f2f2", "TRUE" = "#a41e35"), labels = c("Absent", "Present")) +
    ggplot2::labs(
      title = title %||% paste0("Cohort Identifier Presence Matrix (Key: `", id_col, "`)"),
      x = "Identifier",
      y = "Dataset",
      fill = "Status"
    ) +
    theme_cbe() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  p
}

#' Autoplot Method for Database Relationships
#'
#' @param object A \code{\link{cbe_database_relationships}} object.
#' @param type Plot type: \code{"graph"} for network ER graph, or \code{"venn"} for key overlap.
#' @param db Optional parent database list required when \code{type = "venn"}.
#' @param id_col Identifier column for Venn overlap (default: first shared key).
#' @param ... Additional arguments passed to plotting backends.
#' @return A ggplot2 or plot object.
#' @exportS3Method ggplot2::autoplot
#' @export
autoplot.cbe_database_relationships <- function(object, type = c("graph", "venn"), db = NULL, id_col = NULL, ...) {
  type <- match.arg(type)
  if (type == "venn") {
    if (is.null(db)) {
      stop("`db` (database list) must be provided to autoplot database relationships as a Venn diagram.", call. = FALSE)
    }
    id_col <- id_col %||% object$key_column[1] %||% "id"
    return(cbe_database_venn(db, id_col = id_col, ...))
  }

  # type == "graph"
  if (requireNamespace("ggraph", quietly = TRUE)) {
    tg <- as_tbl_graph.cbe_database_relationships(object)
    p <- ggraph::ggraph(tg, layout = "nicely") +
      ggraph::geom_edge_link(ggplot2::aes(label = paste0(.data$key_column, " (", .data$cardinality, ")")),
                             angle_calc = "along", label_dodge = ggplot2::unit(2.5, "mm"),
                             color = "#005a70", width = 1, alpha = 0.8) +
      ggraph::geom_node_point(color = "#a41e35", size = 12) +
      ggraph::geom_node_text(ggplot2::aes(label = .data$name), color = "white", fontface = "bold", size = 3.5) +
      ggplot2::labs(title = "Database Relational Schema (ER Graph)") +
      ggraph::theme_graph()
    return(p)
  }

  plot(object, ...)
}

#' Autoplot Method for Key Integrity
#'
#' @param object A \code{\link{cbe_check_key_integrity}} object.
#' @param ... Additional arguments passed to methods.
#' @return A ggplot2 object.
#' @exportS3Method ggplot2::autoplot
#' @export
autoplot.cbe_key_integrity <- function(object, ...) {
  df_summary <- tibble::as_tibble(object)
  p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = .data$dataset_name, y = .data$unique_ids)) +
    ggplot2::geom_col(fill = "#005a70", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("N = ", .data$unique_ids)), vjust = -0.3, size = 3.5) +
    ggplot2::labs(
      title = paste0("Unique ID Counts by Dataset (Key: `", attr(object, "id_col") %||% "id", "`)"),
      x = "Dataset",
      y = "Distinct Key Count"
    ) +
    theme_cbe()
  p
}

#' @rdname cbe_database_relationships
#' @export
cbe_find_shared_keys <- function(db) {
  if (!is.list(db) || is.data.frame(db)) {
    stop("`db` must be a named list of data frames (an R database).", call. = FALSE)
  }

  all_cols <- lapply(names(db), function(nm) {
    d <- db[[nm]]
    if (is.data.frame(d)) {
      tibble::tibble(table = nm, column = names(d), class = vapply(d, function(c) class(c)[1], character(1)))
    } else {
      NULL
    }
  })
  col_tbl <- dplyr::bind_rows(all_cols)

  if (nrow(col_tbl) == 0) {
    return(tibble::tibble(column = character(), n_tables = integer(), tables = character(), class = character()))
  }

  col_tbl |>
    dplyr::group_by(.data$column) |>
    dplyr::summarise(
      n_tables = dplyr::n(),
      tables = paste(.data$table, collapse = ", "),
      class = paste(unique(.data$class), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_tables > 1) |>
    dplyr::arrange(dplyr::desc(.data$n_tables), .data$column)
}

#' @rdname cbe_database_relationships
#' @param id_col Character string specifying the identifier column to check.
#' @param master_dataset Character string naming the primary cohort table. If provided,
#'   checks which IDs in other tables are absent from the master cohort table.
#' @param compare Logical; if \code{TRUE} and \code{master_dataset} is supplied,
#'   runs \code{\link{cbe_compare_df}} on overlapping variables between the master dataset
#'   and child datasets, attaching the comparison results.
#' @param tolerance Numeric comparison tolerance passed to \code{\link{cbe_compare_df}}.
#' @param ... Additional arguments passed to \code{\link{cbe_compare_df}}.
#' @export
cbe_check_key_integrity <- function(db, id_col, master_dataset = NULL, compare = FALSE, tolerance = 1e-7, ...) {
  if (!is.list(db) || is.data.frame(db)) {
    stop("`db` must be a named list of data frames (an R database).", call. = FALSE)
  }

  tbl_names <- names(db)[vapply(db, function(d) is.data.frame(d) && id_col %in% names(d), logical(1))]
  if (length(tbl_names) == 0) {
    stop("Column '", id_col, "' not found in any dataset.", call. = FALSE)
  }

  master_ids <- if (!is.null(master_dataset) && master_dataset %in% tbl_names) {
    unique(stats::na.omit(db[[master_dataset]][[id_col]]))
  } else {
    NULL
  }

  orphan_details <- list()
  unrepresented_details <- list()
  comparisons <- list()

  res <- lapply(tbl_names, function(nm) {
    d <- db[[nm]]
    ids <- stats::na.omit(d[[id_col]])
    u_ids <- unique(ids)
    n_na <- sum(is.na(d[[id_col]]))

    out <- tibble::tibble(
      dataset_name = nm,
      total_rows = nrow(d),
      unique_ids = length(u_ids),
      missing_ids = n_na,
      is_unique_key = length(ids) == length(u_ids)
    )

    if (!is.null(master_ids)) {
      orphan_ids <- setdiff(u_ids, master_ids)
      missing_from_master <- setdiff(master_ids, u_ids)
      out$orphan_ids_count <- length(orphan_ids)
      out$unrepresented_master_ids <- length(missing_from_master)

      orphan_details[[nm]] <<- orphan_ids
      unrepresented_details[[nm]] <<- missing_from_master

      if (isTRUE(compare) && nm != master_dataset) {
        # Check if there are common variables to compare beyond the key
        common_v <- intersect(names(db[[master_dataset]]), names(d))
        if (length(common_v) > 1) {
          cmp <- tryCatch(
            cbe_compare_df(
              base = db[[master_dataset]],
              compare = d,
              by = id_col,
              tolerance = tolerance,
              base_name = master_dataset,
              compare_name = nm,
              ...
            ),
            error = function(e) NULL
          )
          if (!is.null(cmp)) {
            comparisons[[nm]] <<- cmp
          }
        }
      }
    }

    out
  })

  summary_tbl <- dplyr::bind_rows(res)
  attr(summary_tbl, "id_col") <- id_col
  attr(summary_tbl, "master_dataset") <- master_dataset
  attr(summary_tbl, "orphan_ids") <- orphan_details
  attr(summary_tbl, "unrepresented_ids") <- unrepresented_details
  if (length(comparisons) > 0) {
    attr(summary_tbl, "comparisons") <- comparisons
  }

  class(summary_tbl) <- c("cbe_key_integrity", class(summary_tbl))
  summary_tbl
}

#' @export
print.cbe_key_integrity <- function(x, ...) {
  cli_line <- paste0(rep("-", 70), collapse = "")
  cat(cli_line, "\n")
  cat("TempleCBE Database Key Integrity Audit\n")
  cat(cli_line, "\n")
  cat(sprintf("Key Column:      %s\n", attr(x, "id_col") %||% "(Unspecified)"))
  if (!is.null(attr(x, "master_dataset"))) {
    cat(sprintf("Master Dataset:  %s\n", attr(x, "master_dataset")))
  }
  cat(cli_line, "\n\n")

  print(tibble::as_tibble(x))

  cmps <- attr(x, "comparisons", exact = TRUE)
  if (!is.null(cmps) && length(cmps) > 0) {
    cat("\n-- Cross-Table Value Concordance (cbe_compare_df) ---------------------\n")
    for (nm in names(cmps)) {
      cmp <- cmps[[nm]]
      n_diff_vars <- sum(cmp$summary$n_diff > 0)
      status_str <- if (cmp$is_concordant) {
        "CONCORDANT"
      } else if (n_diff_vars == 0) {
        "VALUES MATCH (Dim/Obs Differ)"
      } else {
        paste0(n_diff_vars, " DISCREPANT VARS")
      }
      cat(sprintf("  * %s vs %-15s : %s\n", attr(x, "master_dataset"), nm, status_str))
    }
  }
  cat(cli_line, "\n")
  invisible(x)
}
