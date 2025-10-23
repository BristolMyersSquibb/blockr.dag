g6_from_board <- function(board) {
	stopifnot(is_board(board))
	g6R::g6()
}

g6_from_graph <- function(graph) {

	stopifnot(is_graph(graph))

	g6R::g6(
		nodes = graph_nodes(graph),
		edges = graph_edges(graph),
		combos = graph_combos(graph)
	)
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

	g6R::g6_plugins(
    graph,
    ...,
    g6R::context_menu(
      enable = g6R::JS(
        "(e) => {
        let cond = e.targetType === 'edge' ||
          e.targetType === 'node' ||
          e.targetType === 'canvas' ||
          e.targetType === 'combo';
        return cond;
        }"
      ),
      # nolint start
      onClick = g6R::JS(
        context_menu_entry_js(ctx, ns)
      ),
      # nolint end
      getItems = g6R::JS(
        sprintf(
          "async (e) => {
            const response = await fetch(
              ''%s',
              {
                method: 'POST',
                headers: {
                  'Accept': 'application/json',
                  'Content-Type': 'application/json'
                },
                body: JSON.stringify(
                  {
                    id: e.target.id,
                    type: e.targetType
                  }
                )
              }
            );
            const items = await response.json();
            return items;
          }",
          path
        )
      )
    )
  )
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
