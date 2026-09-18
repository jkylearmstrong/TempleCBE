#' Set Operations on R Databases
#'
#' \code{\link{cbe_database}} methods for the generic \code{dplyr::intersect()}/
#' \code{dplyr::union()}/\code{dplyr::setdiff()} verbs (the same generics
#' \pkg{dbplyr}/\pkg{dtplyr} extend for their own backends) -- the table-level
#' building blocks behind \code{\link{graph_intersect}}/\code{\link{graph_subtract}}/
#' \code{\link{graph_union}}. Given two \code{\link{as_database}} results (or any
#' plain \code{list(nodes = ..., edges = ...)} passed through \code{as_database()}
#' first), these find their common, exclusive, or combined rows. Operating at
#' this level (rather than only on graph objects) means they also work
#' directly on \code{nodes}/\code{edges} tables that never came from a graph --
#' e.g. two tables read back from \code{\link{read_workbook}}.
#'
#' Dispatch is on the \code{cbe_database} class (stamped by \code{\link{as_database}}),
#' not on \code{igraph} -- \pkg{igraph} already registers its own
#' \code{union.igraph()} on this same generic, and S3 method tables are global,
#' so a \code{union.igraph()} defined here would silently collide with it.
#' \code{\link{graph_intersect}}/\code{\link{graph_subtract}}/\code{\link{graph_union}}
#' stay dedicated functions for exactly this reason, built on top of these
#' \code{cbe_database} methods via \code{\link{as_database}}/\code{\link{database_to_igraph}}.
#'
#' Node matching defaults to the first column of each database's \code{nodes}
#' table -- the node identifier column, which different sources name
#' differently (\code{name} for an \code{igraph}-derived database, \code{id}
#' for a \pkg{visNetwork}/funviewR one). When the two databases use different
#' column names, pass \code{node_by} explicitly using the same
#' \code{c(x_col = y_col)} convention as \code{dplyr::*_join(by = ...)}; a
#' plain string means the same column name on both sides.
#'
#' \strong{\code{intersect()}/\code{setdiff()}: attribute values always come
#' from \code{x}, never \code{y}, and are never merged.} \code{dplyr::semi_join()}/
#' \code{dplyr::anti_join()} filter \code{x}'s rows by whether a key matches in
#' \code{y} -- they don't pull any of \code{y}'s own columns across. This
#' matters most for graphs whose nodes carry rich, node-specific metadata --
#' e.g. a pipeline graph built from \code{FilePath}/\code{FileUses}/
#' \code{FileOutputs} objects (see \code{\link{compute_graph}}), where
#' \code{stage}, \code{mtime}, and \code{description} differ node by node.
#' Intersecting two snapshots of the same pipeline graph (say, today's vs.
#' yesterday's) for a node present in both keeps \emph{today's}
#' \code{mtime}/\code{stage}, silently discarding yesterday's -- there is no
#' reconciliation between the two. To compare attribute values themselves (not
#' just which nodes exist), run \code{\link{cbe_compare_df}} on the two
#' \code{\link{as_database}} results directly instead.
#'
#' \strong{\code{union()}: attribute values are coalesced, not taken from one
#' side only.} For a node/edge present in both \code{x} and \code{y}, shared
#' columns keep \code{x}'s value where it's non-\code{NA} and fall back to
#' \code{y}'s otherwise -- so combining two databases never silently drops
#' data the way \code{intersect()}/\code{setdiff()} can. This mirrors
#' \code{\link{join_pipelines}}'s existing union-and-coalesce behavior for
#' \code{tbl_graph} objects, generalized to any \code{\link{as_database}} source.
#'
#' @param x,y \code{cbe_database} objects (see \code{\link{as_database}}):
#'   named lists with \code{nodes} and \code{edges} data frames/tibbles.
#' @param node_by Column(s) to match nodes on. \code{NULL} (default) uses the
#'   first column of each \code{nodes} table. A single string uses that
#'   column name on both sides. A named vector/string \code{c(x_col = y_col)}
#'   matches \code{x}'s \code{x_col} to \code{y}'s \code{y_col} (as in
#'   \code{dplyr::*_join()}).
#' @param edge_by Column(s) to match edges on (default \code{c("from", "to")},
#'   which every \code{\link{as_database}} method produces).
#' @param ... Passed on; unused.
#' @return A \code{cbe_database} \code{list(nodes = <tibble>, edges = <tibble>)}.
#'   For \code{intersect()}/\code{setdiff()}, edges are always constrained to
#'   reference only the returned nodes, so the result is a well-formed graph
#'   database even if a matched/unmatched edge's endpoint didn't independently
#'   match on the node side.
#'
#'   One consequence of that well-formedness constraint: an edge can vanish
#'   from \emph{both} \code{intersect()} and \code{setdiff()} at once. If edge
#'   \code{b->c} doesn't literally match one of \code{y}'s edges, it's
#'   excluded from the intersection -- but if node \code{b} is itself one of
#'   \code{y}'s nodes, \code{b} (and anything touching it) also drops out of
#'   the setdiff result too, taking \code{b->c} with it. The edge belongs to
#'   neither side; it isn't double-dropped or double-counted, just genuinely
#'   unrepresentable in a well-formed result for either operation.
#' @name database_setops
#' @examples
#' db_x <- as_database(list(
#'   nodes = data.frame(name = c("a", "b", "c")),
#'   edges = data.frame(from = c("a", "b"), to = c("b", "c"))
#' ))
#' db_y <- as_database(list(
#'   nodes = data.frame(name = c("a", "b")),
#'   edges = data.frame(from = "a", to = "b")
#' ))
#' dplyr::intersect(db_x, db_y)
#' dplyr::setdiff(db_x, db_y)
#' dplyr::union(db_x, db_y)
#'
#' # intersect()/setdiff(): attribute values come from x only, never merged
#' # with y's. Node "a" is present in both snapshots below with different
#' # `stage`/`mtime` (mirroring FilePath's own slots, see compute_graph()) --
#' # the intersection keeps today's values, not yesterday's.
#' snapshot_today <- as_database(list(
#'   nodes = data.frame(
#'     name = "a", stage = "02_analysis",
#'     mtime = as.POSIXct("2026-09-18")
#'   ),
#'   edges = data.frame(from = character(0), to = character(0))
#' ))
#' snapshot_yesterday <- as_database(list(
#'   nodes = data.frame(
#'     name = "a", stage = "01_eda",
#'     mtime = as.POSIXct("2026-09-17")
#'   ),
#'   edges = data.frame(from = character(0), to = character(0))
#' ))
#' dplyr::intersect(snapshot_today, snapshot_yesterday)$nodes$stage # "02_analysis"
#'
#' # union(): coalesces instead -- yesterday's mtime fills in where a node
#' # only exists in one snapshot, and today's non-NA values win on overlap.
#' dplyr::union(snapshot_today, snapshot_yesterday)$nodes
NULL

#' @noRd
.resolve_node_by <- function(x, y, node_by) {
  if (!is.null(node_by)) {
    return(node_by)
  }
  x_key <- names(x$nodes)[1]
  y_key <- names(y$nodes)[1]
  if (identical(x_key, y_key)) x_key else stats::setNames(y_key, x_key)
}

#' @noRd
.node_by_x_col <- function(node_by) {
  if (is.null(names(node_by))) node_by[1] else names(node_by)[1]
}

#' @exportS3Method dplyr::intersect
intersect.cbe_database <- function(x, y, node_by = NULL, edge_by = c("from", "to"), ...) {
  node_by <- .resolve_node_by(x, y, node_by)
  x_col <- .node_by_x_col(node_by)

  nodes_out <- dplyr::semi_join(x$nodes, y$nodes, by = node_by)
  edges_out <- dplyr::semi_join(x$edges, y$edges, by = edge_by)

  shared_ids <- nodes_out[[x_col]]
  edges_out <- edges_out[
    edges_out$from %in% shared_ids & edges_out$to %in% shared_ids,
    ,
    drop = FALSE
  ]

  new_cbe_database(nodes_out, edges_out)
}

#' @exportS3Method dplyr::setdiff
setdiff.cbe_database <- function(x, y, node_by = NULL, edge_by = c("from", "to"), ...) {
  node_by <- .resolve_node_by(x, y, node_by)
  x_col <- .node_by_x_col(node_by)

  nodes_out <- dplyr::anti_join(x$nodes, y$nodes, by = node_by)
  edges_out <- dplyr::anti_join(x$edges, y$edges, by = edge_by)

  surviving_ids <- nodes_out[[x_col]]
  edges_out <- edges_out[
    edges_out$from %in% surviving_ids & edges_out$to %in% surviving_ids,
    ,
    drop = FALSE
  ]

  new_cbe_database(nodes_out, edges_out)
}

#' @noRd
.coalesce_join <- function(x_df, y_df, by) {
  merged <- dplyr::full_join(x_df, y_df, by = by, suffix = c("", ".y"))

  key_cols <- if (is.null(names(by))) by else names(by)
  shared_cols <- setdiff(intersect(names(x_df), names(y_df)), key_cols)

  for (col in shared_cols) {
    col_y <- paste0(col, ".y")
    if (col_y %in% names(merged)) {
      merged[[col]] <- dplyr::coalesce(merged[[col]], merged[[col_y]])
      merged[[col_y]] <- NULL
    }
  }

  merged
}

#' @exportS3Method dplyr::union
union.cbe_database <- function(x, y, node_by = NULL, edge_by = c("from", "to"), ...) {
  node_by <- .resolve_node_by(x, y, node_by)

  nodes_out <- .coalesce_join(x$nodes, y$nodes, by = node_by)
  edges_out <- .coalesce_join(x$edges, y$edges, by = edge_by)

  new_cbe_database(nodes_out, edges_out)
}

#' Build an igraph From an R Database's Nodes/Edges Tables
#'
#' The other direction from \code{\link{as_database}}: takes a
#' \code{\link{cbe_database}} (or plain \code{list(nodes = ..., edges = ...)})
#' -- such as one produced by \code{\link{as_database}} or the
#' \code{\link{database_setops}} verbs -- and builds an \code{igraph} from it.
#'
#' Deliberately not a new \code{as_igraph()} method: \code{as_igraph.default()}
#' (see \code{\link{as_igraph}}) already dispatches on plain \code{list}
#' objects for its own, unrelated purpose (a list of \code{FilePath}/
#' \code{FileUses}/\code{FileOutputs} pipeline objects); adding
#' \code{as_igraph.list()} here would silently intercept every one of those
#' calls instead of falling through to \code{.default}.
#'
#' @param db A \code{cbe_database} (or plain named list with \code{nodes} and
#'   \code{edges} data frames/tibbles), where \code{nodes}'s first column is
#'   the node identifier used by \code{edges}'s \code{from}/\code{to} columns.
#' @param directed Logical, whether the graph is directed (default \code{TRUE}).
#' @return An \code{igraph} object.
#' @export
#' @examples
#' db <- as_database(list(
#'   nodes = data.frame(name = c("a", "b", "c")),
#'   edges = data.frame(from = c("a", "b"), to = c("b", "c"))
#' ))
#' database_to_igraph(db)
database_to_igraph <- function(db, directed = TRUE) {
  igraph::graph_from_data_frame(d = db$edges, vertices = db$nodes, directed = directed)
}
