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

g6_from_board <- function(board, positions = NULL) {
  stopifnot(is_board(board))

  graph <- g6_data_from_board(board)

  g6(
    nodes = merge_node_positions(graph_nodes(graph), positions),
    edges = graph_edges(graph),
    combos = graph_combos(graph)
  )
}

# Overlay extension-owned node positions onto board-derived g6 nodes.
# `positions` is keyed by block id; `nodes` carry g6 ids (`node-<id>`).
# Supplied coordinates set `style$x/y`, which g6R's position-preservation
# (BEFORE/AFTER_LAYOUT) honors over the auto-layout. Unknown or stale ids
# are ignored; nodes without an entry keep no preset and fall to the layout.
merge_node_positions <- function(nodes, positions) {
  if (!length(nodes) || !length(positions)) {
    return(nodes)
  }

  lapply(nodes, function(node) {
    pos <- positions[[from_g6_node_id(node[["id"]])]]
    if (!is.null(pos)) {
      node[["style"]][["x"]] <- pos[["x"]]
      node[["style"]][["y"]] <- pos[["y"]]
    }
    node
  })
}

# Round coordinates to whole pixels so sub-pixel jitter echoed back by the
# client doesn't read as a change (used to break the external-control echo
# loop and to debounce client drags).
round_positions <- function(positions) {
  lapply(positions, function(p) list(x = round(p[["x"]]), y = round(p[["y"]])))
}

# Position specs equal up to whole-pixel rounding, compared per block id
# (order-independent).
positions_equal <- function(a, b) {
  if (length(a) != length(b)) {
    return(FALSE)
  }
  a <- round_positions(a)
  b <- round_positions(b)
  ids <- union(names(a), names(b))
  all(vapply(ids, function(id) identical(a[[id]], b[[id]]), logical(1)))
}

# Entries of `target` whose (pixel-rounded) coordinates differ from `live`.
# This is what an external set needs to push to the client, and an empty
# result is the echo-loop guard: a client-originated change leaves nothing
# to push back.
positions_diff <- function(target, live) {
  if (!length(target)) {
    return(list())
  }
  rt <- round_positions(target)
  rl <- round_positions(live)
  keep <- vapply(
    names(rt),
    function(id) !identical(rt[[id]], rl[[id]]),
    logical(1)
  )
  target[keep]
}

# Push node positions to the client. `positions` is keyed by block id.
apply_node_positions <- function(positions, proxy = blockr_g6_proxy()) {
  if (!length(positions)) {
    return(invisible())
  }

  g6_update_nodes(
    proxy,
    map(
      list,
      id = to_g6_node_id(names(positions)),
      style = map(
        list,
        x = vapply(positions, `[[`, numeric(1), "x"),
        y = vapply(positions, `[[`, numeric(1), "y")
      )
    )
  )

  invisible()
}

# Project the extension's position spec out of the live g6 graph state.
# `state` is the preprocessed `graph.getData()` blob: nodes keyed by g6 id,
# each carrying `style$x/y`. Re-key back to block ids and keep only nodes
# with both coordinates. Returns a named list `id -> list(x, y)`.
project_positions <- function(state) {
  nodes <- state[["nodes"]]

  if (!length(nodes)) {
    return(list())
  }

  res <- lapply(nodes, function(node) {
    xy <- node[["style"]][c("x", "y")]
    if (is.null(xy[["x"]]) || is.null(xy[["y"]])) {
      return(NULL)
    }
    list(x = xy[["x"]], y = xy[["y"]])
  })

  res <- filter_null(res)
  set_names(res, from_g6_node_id(names(res)))
}

# Renderer selection. Production uses G6's default canvas renderer: the SVG
# element reports `offsetWidth == 0`, which makes g-lite's client<->canvas scale
# ignore the page zoom factor and desyncs hit-testing below 100% browser zoom.
# The SVG renderer keeps every element in the DOM, which the shinytest2 e2e
# tests query and screenshot, so those opt in via
# `options(blockr.dag.svg_renderer = TRUE)` (passed to `AppDriver$new()`).
use_svg_renderer <- function() {
  isTRUE(getOption("blockr.dag.svg_renderer", FALSE))
}

set_g6_options <- function(graph, ...) {
  renderer <- if (use_svg_renderer()) JS("() => new SVGRenderer()")
  g6_options(
    graph,
    ...,
    renderer = renderer,
    animation = FALSE,
    node = list(
      type = "custom-image-node",
      style = list(
        labelFill = "#6b7280",
        labelBackground = TRUE,
        labelBackgroundFill = "#f3f4f6",
        labelBackgroundStroke = "#e5e7eb",
        labelBackgroundRadius = 4,
        labelPlacement = "bottom",
        labelOffsetY = 8,
        labelBackgroundLineWidth = 1,
        labelBackgroundRadius = 4,
        labelBackgroundOpacity = 1,
        labelPadding = c(1, 6, 1, 6),
        labelFontSize = 11,
        labelFontFamily = "Open Sans, system-ui, sans-serif"
      ),
      state = list(
        selected = list(
          labelBackgroundFill = "#dbeafe",
          labelBackgroundStroke = "#0D99FF",
          labelFontWeight = 700
        )
      )
    ),
    combo = list(
      animation = FALSE,
      badge = TRUE,
      type = "rect-combo-with-extra-button",
      style = list(
        # more bottom padding, because of the badge
        padding = c(20, 20, 40, 20),
        # below edges (-1): a combo otherwise swallows clicks meant for the
        # edges between its member nodes, making in-stack links unselectable
        zIndex = -2
      )
    ),
    edge = list(
      type = "cubic-vertical",
      style = list(
        zIndex = -1,
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
          return e.targetType === 'node' && e.targetType !== 'combo'
        }"
      ),
      onFinish = JS(
        sprintf(
          "(edge) => {
            const graph = HTMLWidgets.find('#%s').getWidget();
            // For canvas drops, the assist node is already removed, so check targetType first
            if (edge.targetType === 'canvas') {
              Shiny.setInputValue(
                '%s',
                {
                  id: edge.id,
                  source: edge.source.replace(/^node-/, ''),
                  target: null,
                  targetType: 'canvas',
                  sourcePort: edge.style?.sourcePort,
                  portType: edge.style?.portType
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
              // We will recreate the edge from the R side with correct ID
              graph.removeEdgeData([edge.id]);
            }
          }",
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
    # Navigating a large board: search jumps to a named block, the outline lists
    # what is there. The outline hangs under the search box as a dropdown, so
    # the two read as one control; it must follow the search in this list, since
    # that is what it anchors to.
    g6_search(
      outputId = graph_id(ns),
      placeholder = "Search blocks",
      # Top-right: the toolbar already owns the left edge, and the search box
      # plus its outline dropdown would sit on top of it.
      position = "top-right",
      width = 260,
      # `combo` is g6's word for what a board calls a stack.
      labels = c(node = "block", combo = "stack", edge = "link")
    ),
    g6_outline(
      outputId = graph_id(ns),
      title = "Board contents",
      anchor = "search",
      open = FALSE,
      labels = c(node = "block", combo = "stack", edge = "link")
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

init_g6 <- function(board, positions = NULL, ..., session = get_session()) {
  ns <- session$ns

  # The board is the single source of truth for nodes / edges / combos and
  # all board-derived styling. The extension owns only board-independent view
  # attributes, which for now is node positions overlaid on top.
  res <- g6_from_board(board, positions)

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

#' Check if a block is variadic
#'
#' A block is variadic if it has NA arity and no named inputs.
#'
#' @param block A block object
#' @return Logical indicating if the block is variadic
#' @keywords internal
is_variadic_block <- function(block) {
  is_block(block) &&
    is.na(blockr.core::block_arity(block)) &&
    length(blockr.core::block_inputs(block)) == 0
}

#' Resolve target port IDs for links
#'
#' @param links Board links
#' @param blocks Board blocks
#' @return Character vector of target port IDs
#' @keywords internal
resolve_target_ports <- function(links, blocks) {
  if (length(links) == 0) {
    return()
  }

  target_id <- to_g6_node_id(links$to)

  vapply(
    seq_along(links$to),
    function(i) {
      blk <- blocks[[links$to[i]]]
      if (is_variadic_block(blk)) {
        paste0(target_id[i], "-in")
      } else {
        paste0(target_id[i], "-", links$input[i])
      }
    },
    character(1)
  )
}

#' @rdname g6r
#' @param links Board links.
#' @param blocks Board blocks
g6_edges_from_links <- function(links, blocks) {
  if (length(links) == 0) {
    return()
  }
  source_id <- to_g6_node_id(links$from)
  target_ports <- resolve_target_ports(links, blocks)

  res <- map(
    g6_edge,
    id = to_g6_edge_id(names(links)),
    source = source_id,
    target = to_g6_node_id(links$to),
    style = map(
      list,
      labelText = links$input,
      # Currently all nodes only have 1 output that is called out
      sourcePort = paste0(source_id, "-out"),
      # Note: targetPort label is built in create_block_ports()
      # from the link input name so if we prefix by the
      # node id, we are good to go!
      targetPort = target_ports
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
  fill_col <- blks_color(block)

  # variadic input
  if (is_variadic_block(block)) {
    input_ports <- list(g6_input_port(
      key = sprintf("%s-in", id),
      label = "in",
      arity = Inf,
      placement = "top",
      fill = fill_col,
      r = 3
    ))
  } else if (length(inputs) == 1 && arity == 1) {
    # Mono input
    input_ports <- list(g6_input_port(
      key = sprintf("%s-%s", id, inputs[1]),
      label = inputs[1],
      arity = 1,
      placement = "top",
      fill = fill_col,
      r = 3
    ))
  } else if (length(inputs) > 1) {
    # Multi input
    n <- length(inputs)
    if (n == 1) {
      xs <- 0.5
    } else {
      xs <- seq(0.15, 0.85, length.out = n)
    }
    input_ports <- lapply(seq_along(inputs), function(i) {
      g6_input_port(
        key = sprintf("%s-%s", id, inputs[i]),
        label = inputs[i],
        arity = 1,
        placement = c(xs[i], 0),
        fill = fill_col,
        r = 3
      )
    })
  }

  out_id <- sprintf("%s-out", id)

  ports <- c(
    input_ports,
    list(g6_output_port(
      key = out_id,
      label = NULL,
      arity = Inf,
      placement = "label-bottom",
      fill = fill_col,
      r = 3
    ))
  )
  do.call(g6_ports, ports)
}

#' Get children relationships from links
#'
#' @param links Board links
#' @return Named list where each element is a list of child node IDs (for JSON array conversion)
#' @keywords internal
get_children_from_links <- function(links) {
  if (length(links) == 0) {
    return(list())
  }

  # Get unique source nodes
  sources <- unique(links$from)

  # For each source, get all its target nodes as a list (for JSON array conversion)
  children <- lapply(sources, function(src) {
    targets <- links$to[links$from == src]
    as.list(to_g6_node_id(unique(targets)))
  })

  set_names(children, to_g6_node_id(sources))
}

#' @rdname g6r
#' @param blocks Board blocks.
#' @param stacks Board stacks.
#' @param children Named list of children node IDs (optional).
#' @keywords internal
g6_nodes_from_blocks <- function(blocks, stacks, children = NULL) {
  stk_blks <- lapply(stacks, stack_blocks)

  stk_blks <- set_names(
    as.list(rep(names(stk_blks), lengths(stk_blks))),
    do.call("c", stk_blks)
  )

  ids <- to_g6_node_id(names(blocks))

  # The node sizes itself to its icon image (custom-image-node adopts the
  # image's natural size on load), and the icon comes from the shared
  # `blockr.dock::blk_icon_data_uri()` -- so the DAG node and the dock block
  # card show the same-sized icon without either side stating a size.
  base_args <- list(
    id = ids,
    style = map(
      list,
      src = blks_icon(blocks),
      labelText = chr_ply(blocks, block_name)
    ),
    combo = lapply(stk_blks[names(blocks)], to_g6_combo_id),
    ports = map(create_block_ports, blocks, ids),
    collapse = lapply(blocks, function(block) {
      g6_collapse_options(visibility = "hover", stroke = "#D1D5DB")
    })
  )

  # Only add children if we have them and they're non-empty
  if (!is.null(children) && length(children) > 0) {
    node_children <- lapply(ids, function(id) {
      child_ids <- children[[id]]
      # Return NULL for nodes without children (will be removed by dropNulls in g6_node)
      if (is.null(child_ids) || length(child_ids) == 0) {
        NULL
      } else {
        child_ids
      }
    })
    base_args$children <- node_children
  }

  res <- do.call(map, c(list(g6_node), base_args))

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
    ),
    collapse = lapply(stacks, function(stack) {
      g6_collapse_options(visibility = "hover", stroke = "#D1D5DB")
    })
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

  # Get children relationships from links
  children <- get_children_from_links(links)

  edges_data <- g6_edges_from_links(links, blocks)
  combos_data <- g6_combos_data_from_stacks(stacks)
  nodes_data <- g6_nodes_from_blocks(blocks, stacks, children)

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

  mouse_pos <- proxy$session$input[[paste0(graph_id(), "-mouse_position")]]
  base_x <- mouse_pos$x %||% 150
  base_y <- mouse_pos$y %||% 150

  if (length(nodes) == 1) {
    nodes[[1]]$style$x <- base_x
    nodes[[1]]$style$y <- base_y
  } else if (length(nodes) > 1) {
    for (i in seq_along(nodes)) {
      nodes[[i]]$style$x <- base_x
      nodes[[i]]$style$y <- base_y + (i - 1) * 130
    }
  }

  g6_add_nodes(proxy, nodes)
  invisible()
}

relabel_nodes <- function(mods, proxy = blockr_g6_proxy()) {
  # A `blocks$mod` delta can carry any externally controllable argument
  # (e.g. the assistant configuring a chart with `chart_type`/`group`/...),
  # not just a rename. Only deltas that actually change `block_name` are
  # label changes the DAG needs to reflect; ignore the rest. Extracting
  # `block_name` from a config-only delta would yield a zero-length value
  # and crash the surrounding observer.
  mods <- Filter(function(m) !is.null(m[["block_name"]]), mods)

  if (!length(mods)) {
    return(invisible())
  }

  # g6 merges the partial style, keeping the icon.
  g6_update_nodes(
    proxy,
    map(
      list,
      id = to_g6_node_id(names(mods)),
      style = map(list, labelText = chr_xtr(mods, "block_name"))
    )
  )

  invisible()
}

add_edges <- function(links, blocks, proxy = blockr_g6_proxy()) {
  edges <- g6_edges_from_links(links, blocks)
  g6_add_edges(proxy, edges)

  invisible()
}

add_combos <- function(stacks, proxy = blockr_g6_proxy()) {
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
    once = TRUE,
    label = "setup_remove_kbd"
  )
}

setup_copy_paste_kbd <- function(session = get_session()) {
  input <- session$input
  ns <- session$ns
  observeEvent(
    req(input[[paste0(graph_id(), "-initialized")]]),
    {
      session$sendCustomMessage(
        "setup-copy-paste",
        list(id = graph_id(ns))
      )
    },
    once = TRUE,
    label = "setup_copy_paste_kbd"
  )
}
