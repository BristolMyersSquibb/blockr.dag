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

from_g6_port_id <- function(x, node) {
  sub(paste0("^", node, "-"), "", x)
}

to_g6_port_id <- function(x, node) {
  if (length(x)) {
    x <- paste0(node, "-", x)
  }
  x
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
    # Required so that elements are in the DOM
    # for shinytest2 e2e
    renderer = JS("() => new SVGRenderer()"),
    animation = FALSE,
    node = list(
      type = "custom-image-node",
      style = list(
        zIndex = 10,
        labelPlacement = "bottom",
        labelOffsetY = 8,
        labelFontFamily = "Open Sans, system-ui, sans-serif"
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
      type = "cubic-vertical",
      style = list(
        zIndex = 0,
        endArrow = TRUE,
        stroke = "#D1D5DB",
        lineWidth = 2,
        increasedLineWidthForHitTesting = 25,
        curveOffset = list(15, -15),
        labelOpacity = 0,
        labelFill = "#6b7280",
        labelBackground = TRUE,
        labelBackgroundFill = "#FFFFFF",
        labelBackgroundRadius = 4,
        labelBackgroundOpacity = 0,
        labelPadding = c(2, 4)
      ),
      state = list(
        active = list(
          stroke = "#D1D5DB",
          lineWidth = 2,
          shadowBlur = 0,
          shadowColor = "transparent",
          halo = FALSE,
          labelOpacity = 1,
          labelBackgroundOpacity = 1
        ),
        selected = list(
          stroke = "#9ca3af",
          lineWidth = 2,
          shadowBlur = 4,
          shadowColor = "rgba(156, 163, 175, 0.5)",
          halo = FALSE
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
      ranksep = 50,
      sortByCombo = TRUE
    )
  )
}

set_g6_behaviors <- function(graph, ..., ns) {
  g6_behaviors(
    graph,
    ...,
    zoom_canvas(),
    drag_canvas(
      enable = JS(
        "(e) => {
          return e.targetType === 'canvas' && !e.shiftKey && !e.altKey;
        }"
      )
    ),
    # So we can add node to stack from the UI by drag and drop
    # Disable drag when edge creation from port is active
    drag_element(
      enable = JS(
        "(e) => {
          if (e.shiftKey || e.altKey) return false;
          // Access graph via HTMLWidgets and check if edge creation is in progress
          const target = e.nativeEvent?.target;
          const graph = HTMLWidgets.find(`#${target?.closest?.('.g6')?.id}`)?.getWidget();
          try {
            if (graph?.getNodeData?.('g6-create-edge-assist-node-id')) return false;
          } catch (err) {}
          return true;
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
    # Show edge labels on hover (CSS handles the transition animation)
    hover_activate(
      animation = FALSE,
      enable = JS("(e) => e.targetType === 'edge'")
    ),
    # avoid conflict with internal function
    g6R::create_edge(
      enable = JS(
        "(e) => {
          return true;
        }"
      ),
      onFinish = JS(
        sprintf(
          "(edge) => {
            const graph = HTMLWidgets.find('#%s').getWidget();
            // For canvas drops, the assist node is already removed, so check targetType first
            if (edge.targetType === 'canvas') {
              // Get mouse position from Shiny input (captured by g6R on pointer up)
              const mousePos = Shiny.shinyapp.$inputValues['%s-mouse_position'];
              Shiny.setInputValue(
                '%s',
                {
                  id: edge.id,
                  source: edge.source.replace(/^node-/, ''),
                  target: null,
                  targetType: 'canvas',
                  sourcePort: edge.style?.sourcePort
                }
              );
              return;
            }
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
                  targetType: edge.targetType,
                  sourcePort: edge.style.sourcePort,
                  targetPort: edge.style.targetPort
                }
              );
            }
          }",
          graph_id(ns),
          graph_id(ns),
          ns("added_edge"),
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

  # For shinytest2 e2e
  preprocess_graph_state()
  preprocess_mouse_position()

  session$output[[graph_id()]] <- render_g6(res)

  invisible(res)
}

#' @rdname g6r
#' @param links Board links.
g6_edges_from_links <- function(links) {
  if (length(links) == 0) {
    return()
  }
  target_id <- to_g6_node_id(links$to)
  source_id <- to_g6_node_id(links$from)

  res <- map(
    g6_edge,
    id = to_g6_edge_id(names(links)),
    source = to_g6_node_id(links$from),
    target = target_id,
    style = map(
      list,
      labelText = links$input,
      # Currently all nodes only have 1 output that is called out
      sourcePort = paste0(source_id, "-out"),
      # Note: targetPort label is built in create_block_ports()
      # from the link input name so if we prefix by the
      # node id, we are good to go!
      targetPort = paste0(target_id, "-", links$input)
    ),
    MoreArgs = list(type = "cubic-vertical")
  )

  if (length(res)) {
    do.call(g6_edges, res)
  } else {
    res
  }
}

#' Create block ports for g6 node
#' @param block Block object.
#' @param id Block ID.
#' @keywords internal
create_block_ports <- function(block, id) {
  inputs <- blockr.core::block_inputs(block)
  arity <- blockr.core::block_arity(block)
  input_ports <- list()
  fill_col <- blockr.dock::blk_color(blk_category(block))

  if (length(inputs) == 0) {
    if (is.na(arity)) {
      input_ports <- list(g6_input_port(
        key = sprintf("%s-in", id),
        arity = Inf,
        visibility = "hover",
        placement = "top",
        fill = fill_col
      ))
    }
    # else arity == 0: entry, no input ports
  } else if (length(inputs) == 1 && arity == 1) {
    input_ports <- list(g6_input_port(
      key = sprintf("%s-%s", id, inputs[1]),
      arity = 1,
      visibility = "hover",
      placement = "top",
      fill = fill_col
    ))
  } else if (length(inputs) > 1) {
    n <- length(inputs)
    if (n == 1) {
      xs <- 0.5
    } else {
      xs <- seq(0.15, 0.85, length.out = n)
    }
    input_ports <- lapply(seq_along(inputs), function(i) {
      g6_input_port(
        key = sprintf("%s-%s", id, inputs[i]),
        arity = 1,
        visibility = "hover",
        placement = c(xs[i], 0),
        fill = fill_col,
        r = 4
      )
    })
  } else if (length(inputs) == 1 && is.na(arity)) {
    input_ports <- list(g6_input_port(
      key = sprintf("%s-%s", id, inputs[1]),
      arity = Inf,
      visibility = "hover",
      placement = "top",
      fill = fill_col
    ))
  }

  out_id <- sprintf("%s-out", id)

  ports <- c(
    input_ports,
    list(g6_output_port(
      key = out_id,
      arity = Inf,
      visibility = "hover",
      placement = "bottom",
      fill = fill_col
    ))
  )
  do.call(g6_ports, ports)
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

  ids <- to_g6_node_id(names(blocks))

  res <- map(
    g6_node,
    id = ids,
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
    combo = lapply(stk_blks[names(blocks)], to_g6_combo_id),
    ports = map(create_block_ports, blocks, ids)
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

  # Apply mouse position for single node additions (e.g., edge drop to canvas)
  if (length(blocks) == 1 && length(nodes) == 1) {
    mouse_pos <- proxy$session$input[[paste0(graph_id(), "-mouse_position")]]
    if (!is.null(mouse_pos)) {
      nodes[[1]]$style$x <- mouse_pos$x
      nodes[[1]]$style$y <- mouse_pos$y
    }
  }

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
  observeEvent(
    req(input[[paste0(graph_id(), "-initialized")]]),
    {
      session$sendCustomMessage(
        "setup-remove-selected-elements",
        # TBD: key can be a board option
        list(
          key = key,
          id = graph_id(ns)
        )
      )
    },
    once = TRUE
  )
}
