g6_from_board <- function(board) {
  stopifnot(is_board(board))
  graph <- g6_data_from_board(board)
  g6(
    nodes = graph_nodes(graph),
    edges = graph_edges(graph),
    combos = graph_combos(graph)
  )
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
  g6_proxy(graph_id(session$ns), session = session)
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

#' @rdname g6-from-board
#' @param links Board links.
g6_edges_from_links <- function(links) {
  unname(lapply(seq_along(links), function(i) {
    link <- links[[i]]
    list(
      id = names(links)[[i]],
      type = "line",
      source = link$from,
      target = link$to,
      label = link$input
    )
  }))
}

#' @rdname g6-from-board
#' @param blocks Board blocks.
#' @param stacks Board stacks.
#' @keywords internal
g6_nodes_from_blocks <- function(blocks, stacks) {
  blocks_in_stacks <- lapply(stacks, stack_blocks)

  lapply(seq_along(blocks), function(i) {
    current <- blocks[[i]]

    info <- get_block_metadata(current)
    blk_color <- blk_color(info$category)

    tmp <- list(
      id = names(blocks)[[i]],
      label = paste(
        block_name(current),
        "\n id:",
        names(blocks)[[i]]
      ),
      style = list(
        fill = blk_color
      )
    )

    # Find in which stack the node is
    tmp$combo <- unlist(lapply(seq_along(blocks_in_stacks), function(i) {
      stack <- blocks_in_stacks[[i]]
      if (tmp$id %in% stack) {
        sprintf("combo-%s", names(blocks_in_stacks)[[i]])
      }
    }))
    tmp
  })
}

#' Create network data from board
#'
#' @keywords internal
#' @param board Board object.
#' @rdname g6-from-board
g6_data_from_board <- function(board) {
  # Cold start
  links <- board_links(board)
  blocks <- board_blocks(board)
  stacks <- board_stacks(board)

  edges_data <- g6_edges_from_links(links)
  combos_data <- NULL # TBD
  nodes_data <- g6_nodes_from_blocks(blocks, stacks)

  new_graph(
    nodes = nodes_data,
    edges = edges_data,
    combos = combos_data
  )
}

remove_nodes <- function(nodes, proxy = blockr_g6_proxy()) {
  g6_remove_nodes(proxy, nodes)
}

remove_edges <- function(edges, proxy = blockr_g6_proxy()) {
  g6_remove_edges(proxy, edges)
}

remove_combos <- function(combos, proxy = blockr_g6_proxy()) {
  g6_remove_combos(proxy, combos)
}

add_nodes <- function(blocks, board, proxy = blockr_g6_proxy()) {
  nodes <- g6_nodes_from_blocks(blocks, board_stacks(board))
  g6_add_nodes(proxy, nodes)
}

add_links <- function(links, proxy = blockr_g6_proxy()) {
  edges <- g6_edges_from_links(links)
  g6_add_edges(proxy, edges)
}
