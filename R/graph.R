# Internal R list-based representation of a g6 graph (nodes / edges / combos).
#
# This is the wire format we convert to / from g6 at the boundary only. It is
# deliberately NOT part of the public API: the board is the single source of
# truth for nodes / edges / combos and their styling, so callers never ingest
# or emit this shape directly. The extension's own (minimal, board-independent)
# spec is what crosses the constructor / state boundary.
#
# @param nodes Graph nodes (i.e. board blocks).
# @param edges Graph edges (i.e. board links).
# @param combos Node groups (i.e. board stacks).
# @param x Object to test.
# @noRd
new_graph <- function(nodes = list(), edges = list(), combos = list()) {
  structure(
    list(nodes = nodes, edges = edges, combos = combos),
    class = "graph"
  )
}

# @noRd
is_graph <- function(x) {
  inherits(x, "graph")
}

# @noRd
graph_nodes <- function(x) {
  x[["nodes"]]
}

# @noRd
graph_edges <- function(x) {
  x[["edges"]]
}

# @noRd
graph_combos <- function(x) {
  x[["combos"]]
}
