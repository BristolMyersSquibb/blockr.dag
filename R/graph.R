#' Graph object
#'
#' R list-based representation of a g6 graph object.
#'
#' @param nodes Graph nodes (i.e. board blocks)
#' @param edges Graph edges (i.e. board links)
#' @param combos Node groups (i.e. board stacks)
#' @param x Object to test or convert
#' @param ... Generic consistency
#'
#' @details
#' \describe{
#'   \item{`new_graph()`}{Creates a new graph
#' object with the specified nodes, edges, and combos.}
#'   \item{`is_graph()`}{Tests whether an object
#' is a valid graph object.}
#'   \item{`graph_nodes()`}{Extracts the nodes
#' component from a graph object.}
#'   \item{`graph_edges()`}{Extracts the edges
#' component from a graph object.}
#'   \item{`graph_combos()`}{Extracts the combos
#' component from a graph object.}
#'   \item{`as_graph()`}{Generic function to
#' convert objects to graph format.}
#' }
#'
#' @rdname graph
#' @export
#' @return
#' \describe{
#'   \item{`new_graph()`}{A graph object
#' of class "graph" containing nodes, edges, and combos lists.}
#'   \item{`is_graph()`}{`TRUE` if `x` is a
#' graph object, `FALSE` otherwise.}
#'   \item{`graph_nodes()`}{A list of graph nodes.}
#'   \item{`graph_edges()`}{A list of graph edges.}
#'   \item{`graph_combos()`}{A list of graph
#' combos (node groups).}
#'   \item{`as_graph()`}{A graph object converted
#' from the input.}
#' }
new_graph <- function(nodes = list(), edges = list(), combos = list()) {
  structure(
    list(nodes = nodes, edges = edges, combos = combos),
    class = "graph"
  )
}

#' @rdname graph
#' @export
is_graph <- function(x) {
  inherits(x, "graph")
}

#' @rdname graph
#' @export
graph_nodes <- function(x) {
  x[["nodes"]]
}

#' @rdname graph
#' @export
graph_edges <- function(x) {
  x[["edges"]]
}

#' @rdname graph
#' @export
graph_combos <- function(x) {
  x[["combos"]]
}

#' @rdname graph
#' @export
as_graph <- function(x, ...) {
  UseMethod("as_graph")
}

#' @rdname graph
#' @export
as_graph.graph <- function(x, ...) {
  x
}

#' @rdname graph
#' @export
as_graph.list <- function(x, ...) {
  do.call(new_graph, x)
}
