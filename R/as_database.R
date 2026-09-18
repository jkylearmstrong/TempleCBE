#' Convert a Graph Object Back Into an R Database
#'
#' Reverses the \code{data.base -> graph_object} direction already covered by
#' \code{\link{as_igraph}} (and \code{\link{cbe_database_relationships}}):
#' extracts a graph's node and edge tables into a named list of data frames
#' (an R database), the same \code{list(nodes = ..., edges = ...)} shape used
#' throughout this package's \code{data.base <-> excel} tooling (see
#' \code{\link{write_workbook}}/\code{\link{read_workbook}}).
#'
#' Every node/edge attribute present on \code{x} is preserved as its own
#' column -- nothing is dropped, renamed, or flattened (e.g. a rich
#' \code{FilePath}-derived pipeline graph's \code{stage}/\code{mtime}/
#' \code{description}/\code{artifact_role}, see \code{\link{compute_graph}},
#' all come through intact). There's no merging step here since only one
#' graph is involved; the "whose attributes win" question only arises once
#' you combine two databases -- see \code{\link{database_setops}} for that.
#'
#' @param x A graph object: an \code{igraph} (or \code{tbl_graph}, which
#'   extends it and is dispatched via the \code{igraph} method), or a
#'   \pkg{visNetwork} htmlwidget (e.g. from \code{visNetwork::visNetwork()}
#'   or \code{funviewR::plot_dependency_graph()}).
#' @param ... Additional arguments passed to methods.
#' @return A named list \code{list(nodes = <tibble>, edges = <tibble>)}.
#' @name as_database
#' @export
#' @examples
#' g <- igraph::make_ring(5)
#' as_database(g)
as_database <- function(x, ...) {
  UseMethod("as_database")
}

#' @rdname as_database
#' @export
as_database.default <- function(x, ...) {
  stop(
    "No `as_database()` method for class(es): ", paste(class(x), collapse = "/"),
    ". Supported: igraph/tbl_graph objects, and visNetwork htmlwidgets.",
    call. = FALSE
  )
}

#' @rdname as_database
#' @export
as_database.igraph <- function(x, ...) {
  list(
    nodes = tibble::as_tibble(igraph::as_data_frame(x, what = "vertices")),
    edges = tibble::as_tibble(igraph::as_data_frame(x, what = "edges"))
  )
}

#' @rdname as_database
#' @export
as_database.visNetwork <- function(x, ...) {
  list(
    nodes = tibble::as_tibble(x$x$nodes),
    edges = tibble::as_tibble(x$x$edges)
  )
}
