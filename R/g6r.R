g6_from_board <- function(board) {
	stopifnot(is_board(board))
	g6R::g6()
}

g6_from_graph <- function(graph) {
	stopifnot(is_graph(graph))
	g6R::g6()
}

set_g6_options <- function(graph, ...) {
	graph
}

set_g6_layout <- function(graph) {
	graph
}

set_g6_behaviors <- function(graph, ..., ns) {
	graph
}

set_g6_plugins <- function(graph, ..., ns, path, ctx) {
	graph
}

g6_proxy <- function(session = get_session()) {
  g6R::g6_proxy(graph_id(), session = session)
}

init_g6 <- function(board, graph = NULL, ..., session = get_session()) {

	ns <- session$ns

	if (is.null(graph)) {
		res <- g6_from_board(board)
	} else {
		res <- g6_from_graph(graph)
	}

	res <- set_g6_options(res)
	res <- set_g6_layout(res)
	res <- set_g6_behaviors(res, ns = ns)
	res <- set_g6_plugins(res, ns = ns, ...)

	session$output[[graph_id()]] <- g6R::render_g6(res)

	g6_proxy(session)
}
