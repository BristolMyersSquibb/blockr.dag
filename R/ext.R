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
          ns("ctx_add_link")
        )
      },
      action = update_action_trigger(
        action_name = "add_link_action",
        input_name = "ctx_add_link"
      ),
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
          ns("ctx_remove_block")
        )
      },
      action = update_action_trigger(
        action_name = "remove_block_action",
        input_name = "ctx_remove_block"
      ),
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
          ns("ctx_remove_link")
        )
      },
      action = update_action_trigger(
        action_name = "remove_link_action",
        input_name = "ctx_remove_link"
      ),
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
          ns("ctx_append_block")
        )
      },
      action = update_action_trigger(
        action_name = "append_block_action",
        input_name = "ctx_append_block"
      ),
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
          ns("ctx_create_stack")
        )
      },
      action = update_action_trigger(
        action_name = "add_stack_action",
        input_name = "ctx_create_stack"
      ),
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
          ns("ctx_remove_stack")
        )
      },
      action = update_action_trigger(
        action_name = "remove_stack_action",
        input_name = "ctx_remove_stack"
      ),
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
          ns("ctx_edit_stack")
        )
      },
      action = update_action_trigger(
        action_name = "edit_stack_action",
        input_name = "ctx_edit_stack"
      ),
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
          ns("ctx_add_block")
        )
      },
      action = update_action_trigger(
        action_name = "add_block_action",
        input_name = "ctx_add_block"
      ),
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
          ns("tool_add_block")
        )
      },
      action = update_action_trigger(
        action_name = "add_block_action",
        input_name = "tool_add_block"
      )
    ),
    new_toolbar_item(
      id = "add_stack",
      icon = "icon-cascades",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("tool_add_stack")
        )
      },
      action = update_action_trigger(
        action_name = "add_stack_action",
        input_name = "tool_add_stack"
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
          ns("tool_rm_selected")
        )
      },
      action = update_action_trigger(
        action_name = "remove_selected_action",
        input_name = "tool_rm_selected"
      )
    )
  )
}

#' @importFrom blockr.dock extension_block_callback
#' @export
extension_block_callback.dag_extension <- function(x, ...) {
  function(
    id,
    board,
    update,
    conditions,
    dag_extension,
    ...,
    session = get_session()
  ) {
    n_cnd <- reactive(
      sum(lengths(conditions()$error))
    )

    badge_count <- reactiveVal(0L)

    observeEvent(
      req(n_cnd() > 0L, n_cnd() != badge_count()),
      {
        n <- n_cnd()

        badge <- list(
          text = "\u26A0",
          textAlign = "center",
          placement = "right-top",
          backgroundFill = "#dc2626",
          stroke = "#dc2626",
          lineWidth = 1,
          fontWeight = 300,
          textDecorationColor = "#fff"
        )

        node_config <- list(
          list(
            id = to_g6_node_id(id),
            style = list(
              badges = list(badge)
            )
          )
        )

        g6_update_nodes(dag_extension$proxy, node_config)
        badge_count(n)
      }
    )

    observeEvent(
      req(n_cnd() == 0L, badge_count() > 0L),
      {
        node_config <- list(
          list(
            id = to_g6_node_id(id),
            style = list(
              badges = list()
            )
          )
        )

        g6_update_nodes(dag_extension$proxy, node_config)
        badge_count(0L)
      }
    )

    NULL
  }
}
