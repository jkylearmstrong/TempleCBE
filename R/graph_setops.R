#' Intersect or Subtract Two Graph Objects
#'
#' Set operations across two graph objects, regardless of their concrete
#' representation (\code{igraph}/\code{tbl_graph}, or a \pkg{visNetwork}
#' htmlwidget such as \code{funviewR::plot_dependency_graph()} returns).
#' Both graphs are normalized via \code{\link{as_database}}, combined with
#' \code{\link{database_intersect}}/\code{\link{database_subtract}}, and
#' rebuilt into an \code{igraph} with \code{\link{database_to_igraph}}.
#'
#' \code{graph_intersect()} keeps nodes/edges present in \emph{both} \code{x}
#' and \code{y}. \code{graph_subtract()} keeps nodes/edges present in
#' \code{x} but \emph{not} in \code{y} (edges are additionally constrained to
#' only reference surviving nodes, so the result is always well-formed).
#'
#' Both are set operations -- which nodes/edges exist where -- not a
#' value-level comparison of attributes on nodes/edges that happen to match.
#' For that, run \code{\link{cbe_compare_df}} on the two \code{\link{as_database}}
#' results directly. Node/edge attribute values in the result are taken from
#' \code{x}.
#'
#' @param x,y Graph objects: \code{igraph}/\code{tbl_graph} objects, or
#'   \pkg{visNetwork} htmlwidgets.
#' @param directed Logical, whether the rebuilt graph is directed (default \code{TRUE}).
#' @return An \code{igraph} object. Call \code{\link{as_database}} on the
#'   result to get the underlying node/edge tibbles.
#' @name graph_setops
#' @export
#' @examples
#' g1 <- igraph::graph_from_data_frame(
#'   data.frame(from = c("a", "b"), to = c("b", "c")),
#'   vertices = data.frame(name = c("a", "b", "c"))
#' )
#' g2 <- igraph::graph_from_data_frame(
#'   data.frame(from = "a", to = "b"),
#'   vertices = data.frame(name = c("a", "b"))
#' )
#' graph_intersect(g1, g2)
#' graph_subtract(g1, g2)
graph_intersect <- function(x, y, directed = TRUE) {
  db <- database_intersect(as_database(x), as_database(y))
  database_to_igraph(db, directed = directed)
}

#' @rdname graph_setops
#' @export
graph_subtract <- function(x, y, directed = TRUE) {
  db <- database_subtract(as_database(x), as_database(y))
  database_to_igraph(db, directed = directed)
}
