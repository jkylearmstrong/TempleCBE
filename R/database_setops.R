#' Set Operations and Graph Construction on R Databases
#'
#' Table-level building blocks behind \code{\link{graph_intersect}}/
#' \code{\link{graph_subtract}}: given two R databases in the
#' \code{list(nodes = ..., edges = ...)} shape produced by
#' \code{\link{as_database}}, find their common or exclusive rows, or build
#' an \code{igraph} back out of one. Operating at this level (rather than only
#' on graph objects) means these also work directly on \code{nodes}/\code{edges}
#' tables that never came from a graph -- e.g. two tables read back from
#' \code{\link{read_workbook}}.
#'
#' Node matching defaults to the first column of each database's \code{nodes}
#' table -- the node identifier column, which different sources name
#' differently (\code{name} for an \code{igraph}-derived database, \code{id}
#' for a \pkg{visNetwork}/funviewR one). When the two databases use different
#' column names, pass \code{node_by} explicitly using the same
#' \code{c(x_col = y_col)} convention as \code{dplyr::*_join(by = ...)}; a
#' plain string means the same column name on both sides.
#'
#' \strong{Attribute values always come from \code{db_x}, never \code{db_y},
#' and are never merged.} \code{dplyr::semi_join()}/\code{dplyr::anti_join()}
#' filter \code{db_x}'s rows by whether a key matches in \code{db_y} -- they
#' don't pull any of \code{db_y}'s own columns across. This matters most for
#' graphs whose nodes carry rich, node-specific metadata -- e.g. a pipeline
#' graph built from \code{FilePath}/\code{FileUses}/\code{FileOutputs}
#' objects (see \code{\link{compute_graph}}), where \code{stage},
#' \code{mtime}, and \code{description} differ node by node. Intersecting
#' two snapshots of the same pipeline graph (say, today's vs. yesterday's)
#' for a node present in both keeps \emph{today's} \code{mtime}/\code{stage},
#' silently discarding yesterday's -- there is no reconciliation between the
#' two. To compare attribute values themselves (not just which nodes exist),
#' run \code{\link{cbe_compare_df}} on the two \code{\link{as_database}}
#' results directly instead.
#'
#' @param db_x,db_y R databases: named lists with \code{nodes} and
#'   \code{edges} data frames/tibbles.
#' @param node_by Column(s) to match nodes on. \code{NULL} (default) uses the
#'   first column of each \code{nodes} table. A single string uses that
#'   column name on both sides. A named vector/string \code{c(x_col = y_col)}
#'   matches \code{db_x}'s \code{x_col} to \code{db_y}'s \code{y_col} (as in
#'   \code{dplyr::*_join()}).
#' @param edge_by Column(s) to match edges on (default \code{c("from", "to")},
#'   which every \code{\link{as_database}} method produces).
#' @return \code{database_intersect()}/\code{database_subtract()}: a database
#'   \code{list(nodes = <tibble>, edges = <tibble>)}. Edges are always
#'   constrained to reference only the returned nodes, so the result is a
#'   well-formed graph database even if a matched/unmatched edge's endpoint
#'   didn't independently match on the node side.
#'
#'   One consequence of that well-formedness constraint: an edge can vanish
#'   from \emph{both} \code{database_intersect()} and \code{database_subtract()}
#'   at once. If edge \code{b->c} doesn't literally match one of \code{db_y}'s
#'   edges, it's excluded from the intersection -- but if node \code{b} is
#'   itself one of \code{db_y}'s nodes, \code{b} (and anything touching it)
#'   also drops out of the subtraction's node set, taking \code{b->c} with it.
#'   The edge belongs to neither side; it isn't double-dropped or
#'   double-counted, just genuinely unrepresentable in a well-formed result
#'   for either operation.
#' @name database_setops
NULL

#' @noRd
.resolve_node_by <- function(db_x, db_y, node_by) {
  if (!is.null(node_by)) {
    return(node_by)
  }
  x_key <- names(db_x$nodes)[1]
  y_key <- names(db_y$nodes)[1]
  if (identical(x_key, y_key)) x_key else stats::setNames(y_key, x_key)
}

#' @noRd
.node_by_x_col <- function(node_by) {
  if (is.null(names(node_by))) node_by[1] else names(node_by)[1]
}

#' @rdname database_setops
#' @export
#' @examples
#' db_x <- list(
#'   nodes = data.frame(name = c("a", "b", "c")),
#'   edges = data.frame(from = c("a", "b"), to = c("b", "c"))
#' )
#' db_y <- list(
#'   nodes = data.frame(name = c("a", "b")),
#'   edges = data.frame(from = "a", to = "b")
#' )
#' database_intersect(db_x, db_y)
#' database_subtract(db_x, db_y)
#'
#' # Attribute values come from db_x only, never merged with db_y's: node "a"
#' # is present in both snapshots below with different `stage`/`mtime`
#' # (mirroring FilePath's own slots, see compute_graph()) -- the
#' # intersection keeps today's values, not yesterday's.
#' snapshot_today <- list(
#'   nodes = data.frame(
#'     name = "a", stage = "02_analysis",
#'     mtime = as.POSIXct("2026-09-18")
#'   ),
#'   edges = data.frame(from = character(0), to = character(0))
#' )
#' snapshot_yesterday <- list(
#'   nodes = data.frame(
#'     name = "a", stage = "01_eda",
#'     mtime = as.POSIXct("2026-09-17")
#'   ),
#'   edges = data.frame(from = character(0), to = character(0))
#' )
#' database_intersect(snapshot_today, snapshot_yesterday)$nodes$stage # "02_analysis"
database_intersect <- function(db_x, db_y, node_by = NULL, edge_by = c("from", "to")) {
  node_by <- .resolve_node_by(db_x, db_y, node_by)
  x_col <- .node_by_x_col(node_by)

  nodes_out <- dplyr::semi_join(db_x$nodes, db_y$nodes, by = node_by)
  edges_out <- dplyr::semi_join(db_x$edges, db_y$edges, by = edge_by)

  shared_ids <- nodes_out[[x_col]]
  edges_out <- edges_out[
    edges_out$from %in% shared_ids & edges_out$to %in% shared_ids,
    ,
    drop = FALSE
  ]

  list(nodes = tibble::as_tibble(nodes_out), edges = tibble::as_tibble(edges_out))
}

#' @rdname database_setops
#' @export
database_subtract <- function(db_x, db_y, node_by = NULL, edge_by = c("from", "to")) {
  node_by <- .resolve_node_by(db_x, db_y, node_by)
  x_col <- .node_by_x_col(node_by)

  nodes_out <- dplyr::anti_join(db_x$nodes, db_y$nodes, by = node_by)
  edges_out <- dplyr::anti_join(db_x$edges, db_y$edges, by = edge_by)

  surviving_ids <- nodes_out[[x_col]]
  edges_out <- edges_out[
    edges_out$from %in% surviving_ids & edges_out$to %in% surviving_ids,
    ,
    drop = FALSE
  ]

  list(nodes = tibble::as_tibble(nodes_out), edges = tibble::as_tibble(edges_out))
}

#' Build an igraph From an R Database's Nodes/Edges Tables
#'
#' The other direction from \code{\link{as_database}}: takes a
#' \code{list(nodes = ..., edges = ...)} database -- such as one produced by
#' \code{\link{as_database}}, \code{\link{database_intersect}}, or
#' \code{\link{database_subtract}} -- and builds an \code{igraph} from it.
#'
#' Deliberately not a new \code{as_igraph()} method: \code{as_igraph.default()}
#' (see \code{\link{as_igraph}}) already dispatches on plain \code{list}
#' objects for its own, unrelated purpose (a list of \code{FilePath}/
#' \code{FileUses}/\code{FileOutputs} pipeline objects); adding
#' \code{as_igraph.list()} here would silently intercept every one of those
#' calls instead of falling through to \code{.default}.
#'
#' @param db An R database: a named list with \code{nodes} and \code{edges}
#'   data frames/tibbles, where \code{nodes}'s first column is the node
#'   identifier used by \code{edges}'s \code{from}/\code{to} columns.
#' @param directed Logical, whether the graph is directed (default \code{TRUE}).
#' @return An \code{igraph} object.
#' @export
#' @examples
#' db <- list(
#'   nodes = data.frame(name = c("a", "b", "c")),
#'   edges = data.frame(from = c("a", "b"), to = c("b", "c"))
#' )
#' database_to_igraph(db)
database_to_igraph <- function(db, directed = TRUE) {
  igraph::graph_from_data_frame(d = db$edges, vertices = db$nodes, directed = directed)
}
