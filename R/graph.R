new_graph <- function(nodes = list(), edges = list(), combos = list()) {
	structure(
    list(nodes = nodes, edges = edges, combos = combos),
    class = "graph"
  )
}

is_graph <- function(x) {
  inherits(x, "graph")
}
