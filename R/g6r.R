g6_from_board <- function(board) {
  stopifnot(is_board(board))
  g6()
}

g6_from_graph <- function(graph) {
  stopifnot(is_graph(graph))

  g6(
    nodes = graph_nodes(graph),
    edges = graph_edges(graph),
    combos = graph_combos(graph)
  )
}

set_g6_options <- function(graph, ...) {
  g6_options(
    graph,
    ...,
    animation = FALSE,
    node = list(
      style = list(
        labelBackground = TRUE,
        labelBackgroundRadius = 4,
        labelFontFamily = "Arial",
        labelPadding = c(0, 4),
        labelText = JS(
          "(d) => {
        return d.label
      }"
        )
      )
    ),
    combo = list(
      animation = FALSE,
      badge = TRUE,
      type = "rect",
      style = list(
        labelText = JS(
          "(d) => {
        return `Stack: ${d.label}`
      }"
        )
      )
    ),
    edge = list(
      animation = FALSE,
      style = list(
        endArrow = TRUE,
        lineDash = c(5, 5),
        labelText = JS(
          "(d) => {
        return d.label
      }"
        )
      )
    )
  )
}

set_g6_layout <- function(graph) {
  g6_layout(
    graph,
    layout = antv_dagre_layout(
      begin = c(150, 150),
      nodesep = 50,
      ranksep = 75,
      sortByCombo = TRUE
    )
  )
}

set_g6_behaviors <- function(graph, ..., ns) {
  g6_behaviors(
    graph,
    ...,
    "zoom-canvas",
    drag_canvas(
      enable = JS(
        "(e) => {
        return e.targetType === 'canvas' && !e.shiftKey && !e.altKey;
        }"
      )
    ),
    # So we can add node to stack from the UI by drag and drop
    drag_element(
      enable = JS(
        "(e) => {
        return !e.shiftKey && !e.altKey;
        }"
      ),
      dropEffect = "link"
    ),
    click_select(multiple = TRUE),
    brush_select(
      # Option key on mac
      trigger = "Alt"
    ),
    collapse_expand(),
    # avoid conflict with internal function
    g6R::create_edge(
      enable = JS(
        "(e) => {
        return e.shiftKey;
      }"
      ),
      onFinish = JS(
        sprintf(
          "(edge) => {
            const graph = HTMLWidgets.find('#%s').getWidget();
            const targetType = graph.getElementType(edge.target);
            // Avoid to create edges in combos. If so, we remove it
            if (targetType !== 'node') {
              graph.removeEdgeData([edge.id]);
            } else {
              Shiny.setInputValue('%s', edge);
            }
          }",
          ns("network"),
          ns("added_edge")
        )
      )
    )
  )
}

set_g6_plugins <- function(graph, ..., ns, path, ctx) {
  g6_plugins(
    graph,
    ...,
    context_menu(
      enable = JS(
        "(e) => {
        let cond = e.targetType === 'edge' ||
          e.targetType === 'node' ||
          e.targetType === 'canvas' ||
          e.targetType === 'combo';
        return cond;
        }"
      ),
      # nolint start
      onClick = JS(
        context_menu_entry_js(ctx, ns)
      ),
      # nolint end
      getItems = JS(
        sprintf(
          "async (e) => {
            const response = await fetch(
              '%s',
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

blockr_g6_proxy <- function(session = get_session()) {
  g6_proxy(graph_id(), session = session)
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
  res <- set_g6_plugins(res, ..., ns = ns)

  session$output[[graph_id()]] <- render_g6(res)

  blockr_g6_proxy(session)
}
