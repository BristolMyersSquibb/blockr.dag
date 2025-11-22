#' DAG extension
#'
#' Visualizes the DAG (directed acyclic graph) underlying a board and provides
#' UI elemtnts to manipulate the board,
#'
#' @param graph A `graph` object (or `NULL`)
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @rdname dag
#' @export
new_dag_extension <- function(graph = NULL, ...) {
  blockr.dock::new_dock_extension(
    dag_ext_srv(graph),
    dag_ext_ui,
    name = "DAG",
    class = "dag_extension",
    ...
  )
}

#' @export
context_menu_items.dag_extension <- function(x) {
  list(
    new_context_menu_entry(
      name = "Create link",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("add_link")
        )
      },
      action = add_link_action(
        function(inp) req(from_g6_node_id(inp[["add_link"]]))
      ),
      condition = function(board, target) {
        target$type == "node"
      },
      id = "create_link"
    ),
    new_context_menu_entry(
      name = "Remove block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_block")
        )
      },
      action = remove_block_action(
        function(inp) req(from_g6_node_id(inp[["remove_block"]]))
      ),
      condition = function(board, target) {
        target$type == "node"
      },
      id = "remove_block"
    ),
    new_context_menu_entry(
      name = "Remove link",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_link")
        )
      },
      action = remove_link_action(
        function(inp) req(from_g6_edge_id(inp[["remove_link"]]))
      ),
      condition = function(board, target) {
        target$type == "edge"
      },
      id = "remove_link"
    ),
    new_context_menu_entry(
      name = "Append block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', current.id, {priority: 'event'});
          }",
          ns("append_block")
        )
      },
      action = append_block_action(
        function(inp) req(from_g6_node_id(inp[["append_block"]]))
      ),
      condition = function(board, target) {
        target$type == "node"
      },
      id = "append_block"
    ),
    new_context_menu_entry(
      name = "Create stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("create_stack")
        )
      },
      action = add_stack_action(
        function(inp) req(inp[["create_stack"]])
      ),
      condition = function(board, target) {
        target$type == "canvas"
      },
      id = "create_stack"
    ),
    new_context_menu_entry(
      name = "Remove stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_stack")
        )
      },
      action = remove_stack_action(
        function(inp) req(from_g6_combo_id(inp[["remove_stack"]]))
      ),
      condition = function(board, target) {
        target$type == "combo"
      },
      id = "remove_stack"
    ),
    new_context_menu_entry(
      name = "Edit stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id, {priority: 'event'});
          }",
          ns("edit_stack")
        )
      },
      action = edit_stack_action(
        function(inp) req(from_g6_combo_id(inp[["edit_stack"]]))
      ),
      condition = function(board, target) {
        target$type == "combo"
      },
      id = "edit_stack"
    ),
    new_context_menu_entry(
      name = "Add block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("add_block")
        )
      },
      action = add_block_action(
        function(inp) req(inp[["add_block"]])
      ),
      condition = function(board, target) {
        target$type == "canvas"
      },
      id = "add_block"
    )
  )
}

#' @export
toolbar_items.dag_extension <- function(x) {
  list(
    new_toolbar_item(
      id = "zoom_in",
      icon = "zoom-in",
      js = "(value, target, current) => {
        const graph = HTMLWidgets.find(
          `#${target.closest('.g6').id}`
        ).getWidget();
        graph.zoomTo(graph.getZoom() + 0.1);
      }"
    ),
    new_toolbar_item(
      id = "zoom_out",
      icon = "zoom-out",
      js = "(value, target, current) => {
        const graph = HTMLWidgets.find(
          `#${target.closest('.g6').id}`
        ).getWidget();
        graph.zoomTo (graph.getZoom() - 0.1);
      }"
    ),
    new_toolbar_item(
      id = "auto_fit",
      icon = "auto-fit",
      js = "(value, target, current) => {
        const graph = HTMLWidgets.find(
          `#${target.closest('.g6').id}`
        ).getWidget();
        graph.fitView();
      }"
    ),
    new_toolbar_item(
      id = "layout",
      icon = "reset",
      js = "(value, target, current) => {
        const graph = HTMLWidgets.find(
          `#${target.closest('.g6').id}`
        ).getWidget();
        graph.layout();
      }"
    ),
    new_toolbar_item(
      id = "add_block",
      icon = "icon-roundaddfill",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("add_block")
        )
      },
      action = add_block_action(
        function(inp) req(inp[["add_block"]])
      )
    ),
    new_toolbar_item(
      id = "add_stack",
      icon = "icon-roundadd",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("add_stack")
        )
      },
      action = add_stack_action(
        function(inp) req(inp[["add_stack"]])
      )
    ),
    new_toolbar_item(
      id = "remove_selected",
      icon = "icon-delete",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("rm_selected")
        )
      },
      action = remove_selected_action(
        function(inp) req(inp[["rm_selected"]])
      )
    )
  )
}
