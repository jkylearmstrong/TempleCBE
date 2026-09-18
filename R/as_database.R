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
#'   extends it and is dispatched via the \code{igraph} method), a
#'   \pkg{visNetwork} htmlwidget (e.g. from \code{visNetwork::visNetwork()}
#'   or \code{funviewR::plot_dependency_graph()}), or a plain
#'   \code{list(nodes = ..., edges = ...)} (e.g. read back from
#'   \code{\link{read_workbook}}) to be validated and stamped as one.
#' @param ... Additional arguments passed to methods.
#' @return A named list \code{list(nodes = <tibble>, edges = <tibble>)} of
#'   class \code{cbe_database} -- the class \code{\link{database_setops}} and
#'   \code{\link{cbe_compare_df}} dispatch on.
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
    ". Supported: igraph/tbl_graph objects, visNetwork htmlwidgets, and",
    " list(nodes = ..., edges = ...).",
    call. = FALSE
  )
}

#' @noRd
new_cbe_database <- function(nodes, edges) {
  structure(
    list(nodes = tibble::as_tibble(nodes), edges = tibble::as_tibble(edges)),
    class = c("cbe_database", "list")
  )
}

#' @rdname as_database
#' @export
as_database.igraph <- function(x, ...) {
  new_cbe_database(
    igraph::as_data_frame(x, what = "vertices"),
    igraph::as_data_frame(x, what = "edges")
  )
}

#' @rdname as_database
#' @export
as_database.visNetwork <- function(x, ...) {
  new_cbe_database(x$x$nodes, x$x$edges)
}

#' @rdname as_database
#' @export
as_database.list <- function(x, ...) {
  if (!all(c("nodes", "edges") %in% names(x)) ||
      !is.data.frame(x$nodes) || !is.data.frame(x$edges)) {
    stop(
      "`as_database()` on a plain list requires `nodes` and `edges` ",
      "data frame/tibble elements.",
      call. = FALSE
    )
  }
  new_cbe_database(x$nodes, x$edges)
}
