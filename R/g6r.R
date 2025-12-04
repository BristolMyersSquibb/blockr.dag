to_g6_node_id <- function(x) {

  if (length(x)) {
    x <- paste0("node-", x)
  }

  x
}

from_g6_node_id <- function(x) {
  sub("^node-", "", x)
}

to_g6_edge_id <- function(x) {

  if (length(x)) {
    x <- paste0("edge-", x)
  }

  x
}

from_g6_edge_id <- function(x) {
  sub("^edge-", "", x)
}

to_g6_combo_id <- function(x) {

  if (length(x)) {
    x <- paste0("combo-", x)
  }

  x
}

from_g6_combo_id <- function(x) {
  sub("^combo-", "", x)
}

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
        labelPlacement = "bottom",
        labelOffsetY = 8
      )
    ),
    combo = list(
      animation = FALSE,
      badge = TRUE,
      type = "rect",
      style = list(
        # more bottom padding, because of the badge
        padding = c(20, 20, 40, 20)
      )
    ),
    edge = list(
      animation = FALSE,
      style = list(
        endArrow = TRUE,
        stroke = "#D1D5DB"
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
      ranksep = 50,
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
      # For now, we prevent nodes from being dropped outside combo.
      dropEffect = "move"
    ),
    click_select(multiple = TRUE),
    brush_select(
      # Option key on mac
      trigger = "Alt",
      enableElements = c("node", "edge", "combo"),
      immediately = TRUE,
      outputId = graph_id(ns)
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
            if (targetType === 'combo') {
              graph.removeEdgeData([edge.id]);
            } else {
              Shiny.setInputValue(
                '%s',
                {
                  id: edge.id,
                  source: edge.source.replace(/^node-/, ''),
                  target: edge.target.replace(/^node-/, ''),
                  targetType: edge.targetType
                }
              );
            }
          }",
          graph_id(ns),
          ns("added_edge")
        )
      )
    )
  )
}

set_g6_plugins <- function(graph, ..., ns, path, ctx, tools) {
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
      onClick = JS(context_menu_entry_js(ctx, ns)),
      getItems = JS(
        sprintf(
          "async (e) => {
            var body;
            if (e.targetType === 'canvas') {
              body = {type: e.targetType};
            } else {
              body = {
                id: e.target.id.replace(/^(node|edge|combo)-/, ''),
                type: e.targetType
              };
            }
            const response = await fetch(
              '%s',
              {
                method: 'POST',
                headers: {
                  'Accept': 'application/json',
                  'Content-Type': 'application/json'
                },
                body: JSON.stringify(body)
              }
            );
            const items = await response.json();
            return items;
          }",
          path
        )
      )
    ),
    toolbar(
      style = list(
        backgroundColor = "#f5f5f5",
        padding = "8px",
        boxShadow = "0 2px 8px rgba(0, 0, 0, 0.15)",
        borderRadius = "8px",
        border = "1px solid #e8e8e8",
        opacity = "0.9",
        marginTop = "12px",
        marginLeft = "12px"
      ),
      position = "left",
      getItems = JS(build_toolbar(tools)),
      onClick = JS(toolbar_item_js(tools, ns))
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
    res <- g6_from_graph(as_graph(graph))
  }

  res <- set_g6_options(res)
  res <- set_g6_layout(res)
  res <- set_g6_behaviors(res, ns = ns)
  res <- set_g6_plugins(res, ..., ns = ns)

  session$output[[graph_id()]] <- render_g6(res)

  invisible(res)
}

#' @rdname g6r
#' @param links Board links.
g6_edges_from_links <- function(links) {
  res <- map(
    g6_edge,
    id = to_g6_edge_id(names(links)),
    source = to_g6_node_id(links$from),
    target = to_g6_node_id(links$to),
    style = map(list, labelText = links$input),
    MoreArgs = list(type = "line")
  )

  if (length(res)) {
    do.call(g6_edges, res)
  } else {
    res
  }
}

#' @rdname g6r
#' @param blocks Board blocks.
#' @param stacks Board stacks.
#' @keywords internal
g6_nodes_from_blocks <- function(blocks, stacks) {

  stk_blks <- lapply(stacks, stack_blocks)

  stk_blks <- set_names(
    as.list(rep(names(stk_blks), lengths(stk_blks))),
    do.call("c", stk_blks)
  )

  res <- map(
    g6_node,
    id = to_g6_node_id(names(blocks)),
    type = rep("image", length(blocks)),
    style = map(
      list,
      src = map(
        blockr.dock::blk_icon_data_uri,
        lapply(blocks, blk_icon),
        lapply(chr_ply(blocks, blk_category), blockr.dock::blk_color),
        MoreArgs = list(size = 48)
      ),
      labelText = chr_ply(blocks, block_name),
      MoreArgs = list(size = 48)
    ),
    combo = lapply(stk_blks[names(blocks)], to_g6_combo_id)
  )

  if (length(res)) {
    do.call(g6_nodes, res)
  } else {
    res
  }
}

#' @rdname g6r
#' @param stacks Board stacks.
#' @keywords internal
g6_combos_data_from_stacks <- function(stacks) {
  colors <- blockr.dock::stack_color(stacks)

  colors[is.na(colors)] <- blockr.dock::suggest_new_colors(
    coal(unlst(colors[!is.na(colors)]), character()),
    sum(is.na(colors))
  )

  res <- map(
    g6_combo,
    id = to_g6_combo_id(names(stacks)),
    style = map(
      list,
      fill = colors,
      shadowColor = colors,
      collapsedFill = colors,
      iconFill = colors,
      labelText = chr_ply(stacks, stack_name),
      MoreArgs = list(
        fillOpacity = 0.1,
        labelPlacement = "top",
        lineWidth = 0,
        radius = 8
      )
    )
  )

  if (length(res)) {
    do.call(g6_combos, res)
  } else {
    res
  }
}

#' Create network data from board
#'
#' @keywords internal
#' @param board Board object.
#' @rdname g6r
g6_data_from_board <- function(board) {
  # Cold start
  links <- board_links(board)
  blocks <- board_blocks(board)
  stacks <- board_stacks(board)

  edges_data <- g6_edges_from_links(links)
  combos_data <- g6_combos_data_from_stacks(stacks)
  nodes_data <- g6_nodes_from_blocks(blocks, stacks)

  new_graph(
    nodes = nodes_data,
    edges = edges_data,
    combos = combos_data
  )
}

remove_nodes <- function(nodes, asis = FALSE, proxy = blockr_g6_proxy()) {

  if (!isTRUE(asis)) {
    nodes <- to_g6_node_id(nodes)
  }

  g6_remove_nodes(proxy, nodes)

  invisible()
}

remove_edges <- function(edges, asis = FALSE, proxy = blockr_g6_proxy()) {

  if (!isTRUE(asis)) {
    edges <- to_g6_edge_id(edges)
  }

  g6_remove_edges(proxy, edges)

  invisible()
}

remove_combos <- function(combos, asis = FALSE, proxy = blockr_g6_proxy()) {

  if (!isTRUE(asis)) {
    combos <- to_g6_combo_id(combos)
  }

  g6_remove_combos(proxy, combos)

  invisible()
}

add_nodes <- function(blocks, board, proxy = blockr_g6_proxy()) {
  nodes <- g6_nodes_from_blocks(blocks, board_stacks(board))
  g6_add_nodes(proxy, nodes)
  invisible()
}

update_nodes <- function(blocks, board, proxy = blockr_g6_proxy()) {
  nodes <- g6_nodes_from_blocks(blocks, board_stacks(board))
  g6_update_nodes(proxy, nodes)
  invisible()
}

add_edges <- function(links, proxy = blockr_g6_proxy()) {
  edges <- g6_edges_from_links(links)
  g6_add_edges(proxy, edges)
  invisible()
}

add_combos <- function(stacks, board, proxy = blockr_g6_proxy()) {
  combos <- g6_combos_data_from_stacks(stacks)
  g6_add_combos(proxy, combos)

  # Add nodes to stacks if any
  add_nodes_to_combos(stacks, proxy)

  invisible()
}

add_nodes_to_combos <- function(stacks, proxy = blockr_g6_proxy()) {
  map(
    add_nodes_to_combo,
    lapply(stacks, stack_blocks),
    names(stacks),
    MoreArgs = list(proxy = proxy)
  )

  invisible()
}

add_nodes_to_combo <- function(block_ids, stack_id, proxy = blockr_g6_proxy()) {
  g6_update_nodes(
    proxy,
    map(
      list,
      id = to_g6_node_id(block_ids),
      MoreArgs = list(combo = to_g6_combo_id(stack_id))
    )
  )

  invisible()
}

remove_nodes_from_combo <- function(block_ids, proxy = blockr_g6_proxy()) {
  g6_update_nodes(
    proxy,
    map(list, id = to_g6_node_id(block_ids), MoreArgs = list(combo = NULL))
  )

  invisible()
}

update_combos <- function(stacks, board, proxy = blockr_g6_proxy()) {
  g6_update_combos(proxy, g6_combos_data_from_stacks(stacks))

  cur_stacks <- board_stacks(board)

  for (id in names(stacks)) {
    cur_stack_blocks <- stack_blocks(cur_stacks[[id]])
    new_stack_blocks <- stack_blocks(stacks[[id]])

    to_add <- setdiff(new_stack_blocks, cur_stack_blocks)
    to_remove <- setdiff(cur_stack_blocks, new_stack_blocks)

    # Prevent no-oops from flooding the JS handlers
    if (length(to_remove)) {
      remove_nodes_from_combo(to_remove, proxy)
    }
    if (length(to_add)) {
      add_nodes_to_combo(to_add, id, proxy)
    }
  }

  invisible()
}

setup_remove_elements_kbd <- function(
  key = "Backspace",
  session = get_session()
) {
  input <- session$input
  ns <- session$ns
  observeEvent(req(input[[paste0(graph_id(), "-initialized")]]), {
    session$sendCustomMessage(
      "setup-remove-selected-elements",
      # TBD: key can be a board option
      list(
        key = key,
        id = graph_id(ns)
      )
    )
  })
}
