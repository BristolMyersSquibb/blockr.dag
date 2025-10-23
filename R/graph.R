#' Graph object
#'
#' R list-based representation of a g6 graph object.
#'
#' @param nodes Graph nodes (i.e. board blocks)
#' @param edges Graph edges (i.e. board links)
#' @param combos Node groups (i.e. board stacks)
#'
#' @rdname graph
#' @export
new_graph <- function(nodes = list(), edges = list(), combos = list()) {
	structure(
    list(nodes = nodes, edges = edges, combos = combos),
    class = "graph"
  )
}

#' @param x Object
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
