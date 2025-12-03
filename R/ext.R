#' DAG extension
#'
#' Visualizes the DAG (directed acyclic graph) underlying a board and provides
#' UI elements to manipulate the board.
#'
#' @param graph A `graph` object (or `NULL`).
#' @param ... Forwarded to [blockr.dock::new_dock_extension()].
#'
#' @return A `dag_extension` object that extends the dock extension system
#' for visualizing and manipulating DAG workflows.
#' @rdname dag
#' @export
new_dag_extension <- function(graph = NULL, ...) {
  blockr.dock::new_dock_extension(
    dag_ext_srv(graph),
    dag_ext_ui,
    name = "Workflow",
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
            Shiny.setInputValue('%s', current.id.replace(/^node-/, ''));
          }",
          ns("add_link")
        )
      },
      action = blockr.dock::add_link_action("add_link"),
      condition = function(board, target) target$type == "node",
      id = "create_link"
    ),
    new_context_menu_entry(
      name = "Remove block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id.replace(/^node-/, ''));
          }",
          ns("remove_block")
        )
      },
      action = blockr.dock::remove_block_action("remove_block"),
      condition = function(board, target) target$type == "node",
      id = "remove_block"
    ),
    new_context_menu_entry(
      name = "Remove link",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id.replace(/^edge-/, ''));
          }",
          ns("remove_link")
        )
      },
      action = blockr.dock::remove_link_action("remove_link"),
      condition = function(board, target) target$type == "edge",
      id = "remove_link"
    ),
    new_context_menu_entry(
      name = "Append block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue(
              '%s',
              current.id.replace(/^node-/, ''),
              {priority: 'event'}
            );
          }",
          ns("append_block")
        )
      },
      action = blockr.dock::append_block_action("append_block"),
      condition = function(board, target) target$type == "node",
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
      action = blockr.dock::add_stack_action("create_stack"),
      condition = function(board, target) target$type == "canvas",
      id = "create_stack"
    ),
    new_context_menu_entry(
      name = "Remove stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id.replace(/^combo-/, ''));
          }",
          ns("remove_stack")
        )
      },
      action = blockr.dock::remove_stack_action("remove_stack"),
      condition = function(board, target) target$type == "combo",
      id = "remove_stack"
    ),
    new_context_menu_entry(
      name = "Edit stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue(
              '%s',
              current.id.replace(/^combo-/, ''),
              {priority: 'event'}
            );
          }",
          ns("edit_stack")
        )
      },
      action = blockr.dock::edit_stack_action("edit_stack"),
      condition = function(board, target) target$type == "combo",
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
      action = blockr.dock::add_block_action("add_block"),
      condition = function(board, target) target$type == "canvas",
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
      action = blockr.dock::add_block_action("add_block")
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
      action = blockr.dock::add_stack_action("add_stack")
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
      action = remove_selected_action("rm_selected")
    )
  )
}
