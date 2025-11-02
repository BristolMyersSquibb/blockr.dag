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
            if (targetType !== 'node') {
              graph.removeEdgeData([edge.id]);
            } else {
              Shiny.setInputValue('%s', edge);
            }
          }",
          graph_id(ns),
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

  invisible(res)
}

#' @rdname g6-from-board
#' @param links Board links.
g6_edges_from_links <- function(links) {
  map(
    list,
    id = names(links),
    source = links$from,
    target = links$to,
    label = links$input,
    MoreArgs = list(type = "line")
  )
}

#' @rdname g6-from-board
#' @param blocks Board blocks.
#' @param stacks Board stacks.
#' @keywords internal
g6_nodes_from_blocks <- function(blocks, stacks) {

  stack_blocks <- lapply(stacks, stack_blocks)
  stack_blocks <- set_names(
    rep(names(stack_blocks), lengths(stack_blocks)),
    do.call("c", stack_blocks)
  )

  map(
    list,
    id = names(blocks),
    label = chr_ply(blocks, block_name),
    style = map(
      list,
      fill = chr_ply(
        chr_ply(
          chr_ply(blocks, registry_id_from_block),
          block_metadata,
          "category"
        ),
        blk_color
      )
    ),
    combo = lapply(
      stack_blocks[names(blocks)],
      function(x) if (is.na(x)) list() else x
    )
  )
}

#' @rdname g6-from-board
#' @param stacks Board stacks.
#' @keywords internal
g6_combos_data_from_stacks <- function(stacks) {

  colors <- stack_color(stacks)

  colors[is.na(colors)] <- suggest_new_colors(
    coal(unlst(colors[!is.na(colors)]), character()),
    sum(is.na(colors))
  )

  map(
    list,
    id = names(stacks),
    label = chr_ply(stacks, stack_name),
    style = map(
      list,
      stroke = colors,
      fill = colors,
      shadowColor = colors,
      collapsedFill = colors,
      collapsedStroke = colors,
      iconFill = colors,
      MoreArgs = list(
        fillOpacity = 0.2,
        labelPlacement = "top"
      )
    )
  )
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
  combos_data <- g6_combos_data_from_stacks(stacks)
  nodes_data <- g6_nodes_from_blocks(blocks, stacks)

  new_graph(
    nodes = nodes_data,
    edges = edges_data,
    combos = combos_data
  )
}

remove_nodes <- function(nodes, proxy = blockr_g6_proxy()) {
  g6_remove_nodes(proxy, nodes)
  invisible()
}

remove_edges <- function(edges, proxy = blockr_g6_proxy()) {
  g6_remove_edges(proxy, edges)
  invisible()
}

remove_combos <- function(combos, proxy = blockr_g6_proxy()) {
  # No need to remove the combos data from nodes, this is done
  # automatically by g6 :)
  g6_remove_combos(proxy, combos)
  invisible()
}

add_nodes <- function(blocks, board, proxy = blockr_g6_proxy()) {
  nodes <- g6_nodes_from_blocks(blocks, board_stacks(board))
  g6_add_nodes(proxy, nodes)
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
    map(list, id = block_ids, MoreArgs = list(combo = stack_id))
  )

  invisible()
}

remove_nodes_from_combo <- function(block_ids, proxy = blockr_g6_proxy()) {

  g6_update_nodes(
    proxy,
    map(list, id = block_ids, MoreArgs = list(combo = NULL))
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

    remove_nodes_from_combo(to_remove, proxy)
    add_nodes_to_combo(to_add, id, proxy)
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
