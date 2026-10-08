#' DAG extension
#'
#' Visualizes the DAG (directed acyclic graph) underlying a board and provides
#' UI elements to manipulate the board.
#'
#' @section Options:
#' `blockr.dag.svg_renderer`: when `TRUE`, the DAG is rendered with the SVG
#' renderer instead of the default canvas renderer. Canvas is the default
#' because the SVG element reports `offsetWidth == 0`, which makes the
#' underlying `g-lite` client/canvas coordinate scaling ignore the page zoom
#' factor and desyncs hit-testing below 100% browser zoom. The SVG renderer
#' keeps every element in the DOM, which the `shinytest2` end-to-end tests need
#' to query and screenshot, so they opt in via
#' `AppDriver$new(options = list(blockr.dag.svg_renderer = TRUE))`.
#'
#' @param positions Optional node positions overlaid on the board-derived
#' nodes, as a named list keyed by block id, each element a list with numeric
#' `x` and `y` (e.g. `list(a = list(x = 100, y = 200))`). Persisted across
#' save / restore. Unknown or stale block ids are ignored. This handle is
#' externally controllable: positions can be set programmatically through the
#' board update lifecycle (`update(list(extensions = list(mod = list(<ext_id> =
#' list(positions = ...)))))`), which moves the corresponding nodes. Note: the
#' auto-layout currently computes final node placement at cold start, so
#' supplied positions are not yet honored over it (a follow-up will let
#' positions pin over the layout).
#' @param ... Forwarded to [blockr.dock::new_dock_extension()].
#'
#' @return A `dag_extension` object that extends the dock extension system
#' for visualizing and manipulating DAG workflows.
#' @rdname dag
#' @export
new_dag_extension <- function(positions = NULL, ...) {
  blockr.dock::new_dock_extension(
    dag_ext_srv(positions),
    dag_ext_ui,
    name = "Workflow",
    description = dag_ext_meta(),
    class = "dag_extension",
    external_ctrl = "positions",
    ...
  )
}

# Model-facing metadata, surfaced by `blockr.assistant`'s extension tools
# (`list_extensions`, `describe_extension`) to a client driving the extension
# through `modify_extension`. Structured rather than one blob so each part reaches the
# client on its own: what the view is, what the one controllable variable
# takes, and how to drive it.
dag_ext_meta <- function() {
  blockr.dock::new_ext_meta(
    description = paste(
      "Workflow diagram: the directed-acyclic-graph view of the board's",
      "blocks and the links between them."
    ),
    arguments = blockr.core::new_arg_specs(
      positions = blockr.core::new_arg_spec(
        description = dag_positions_description(),
        example = list(
          my_block = list(x = 120, y = 80),
          other_block = list(x = 270, y = 80)
        )
      )
    ),
    guidance = paste(
      "A block's place in this diagram is a canvas coordinate, not a",
      "dockview panel or view: move a block with `modify_extension` on",
      "`positions`, never with the view or panel tools."
    ),
    examples = list(
      list(positions = list(my_block = list(x = 120, y = 80)))
    )
  )
}

# The `positions` schema plus the arithmetic for placing one block relative to
# another. Kept as prose: the outer block-id map is open, which the closed
# `arg_object()` record cannot express, so only the example carries the shape
# machine-readably.
dag_positions_description <- function() {
  paste(
    "Where each block sits on the workflow canvas.",
    "JSON object mapping block id to an object with numeric `x` and `y`",
    "canvas-pixel coordinates (origin top-left, x rightward, y downward),",
    "e.g. {\"my_block\": {\"x\": 120, \"y\": 80}}. Set only the blocks you",
    "move; omitted blocks keep their current positions. Coordinates are",
    "absolute, so to place a block relative to another (to its",
    "left/right/above/below) first read both blocks' current coordinates from",
    "the `values` field of describe_extension, then compute the target: nodes",
    "are about 50px, so leave ~150px between centres (left = same y and",
    "smaller x, right = same y and larger x, above = same x and smaller y,",
    "below = same x and larger y)."
  )
}

# The entries that open one of the dock's menus (the "+" menu, a link's or a
# stack's menu, Connect to and Add to stack) send where the click happened
# along with the target, as
# the `at` of the action's trigger (see blockr.dock::new_action()). The
# context menu hides right after the click, so rather than the entry's id
# they send the bottom-left corner of its box, in viewport pixels: the menu
# then hangs under the entry as it would under an element named by id.
#' @export
context_menu_items.dag_extension <- function(x) {
  list(
    new_context_menu_entry(
      name = "Connect to\u2026",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: current.id.replace(/^node-/, ''),
                at: {x: box.left, y: box.bottom}
              },
              {priority: 'event'}
            );
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
      name = "Edit link",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: current.id.replace(/^edge-/, ''),
                at: {x: box.left, y: box.bottom}
              },
              {priority: 'event'}
            );
          }",
          ns("ctx_edit_link")
        )
      },
      action = update_action_trigger(
        action_name = "edit_link_action",
        input_name = "ctx_edit_link"
      ),
      condition = function(board, target) target$type == "edge",
      id = "edit_link"
    ),
    new_context_menu_entry(
      name = "Insert block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: current.id.replace(/^edge-/, ''),
                at: {x: box.left, y: box.bottom}
              },
              {priority: 'event'}
            );
          }",
          ns("ctx_insert_block")
        )
      },
      action = update_action_trigger(
        action_name = "insert_block_action",
        input_name = "ctx_insert_block"
      ),
      condition = function(board, target) target$type == "edge",
      id = "insert_block"
    ),
    new_context_menu_entry(
      name = "Append block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: current.id.replace(/^node-/, ''),
                at: {x: box.left, y: box.bottom}
              },
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
    # Puts the block in a stack, or the whole selection when the block is one
    # of several selected: the menu then reads "Add 3 blocks to".
    new_context_menu_entry(
      name = "Add to stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            const el = document.getElementById('%s');
            const w = el && HTMLWidgets.find('#' + el.id);
            const graph = w && w.getWidget();
            const sel = graph
              ? graph.getElementDataByState('node', 'selected').map((n) => n.id)
              : [];
            const ids = sel.length > 1 && sel.indexOf(current.id) >= 0
              ? sel : [current.id];
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: ids.map((id) => id.replace(/^node-/, '')),
                at: {x: box.left, y: box.bottom}
              },
              {priority: 'event'}
            );
          }",
          ns(graph_id()),
          ns("ctx_add_to_stack")
        )
      },
      action = update_action_trigger(
        action_name = "add_stack_action",
        input_name = "ctx_add_to_stack"
      ),
      condition = function(board, target) target$type == "node",
      id = "add_to_stack"
    ),
    new_context_menu_entry(
      name = "Add block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {target: true, at: {x: box.left, y: box.bottom}},
              {priority: 'event'}
            );
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
    ),
    new_context_menu_entry(
      name = "Dissolve stack",
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
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {
                target: current.id.replace(/^combo-/, ''),
                at: {x: box.left, y: box.bottom}
              },
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
      name = "Copy",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("ctx_copy")
        )
      },
      action = update_action_trigger(
        action_name = "copy_selected_action",
        input_name = "ctx_copy"
      ),
      condition = function(board, target) {
        target$type %in% c("node", "combo")
      },
      id = "copy"
    ),
    new_context_menu_entry(
      name = "Cut",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("ctx_cut")
        )
      },
      action = update_action_trigger(
        action_name = "cut_selected_action",
        input_name = "ctx_cut"
      ),
      condition = function(board, target) {
        target$type %in% c("node", "combo")
      },
      id = "cut"
    ),
    new_context_menu_entry(
      name = "Paste",
      js = function(ns) {
        sprintf(
          "async (value, target, current) => {
            try {
              const text = await navigator.clipboard.readText();
              const data = JSON.parse(text);
              if (data && data.object === 'subboard') {
                Shiny.setInputValue('%s', text, {priority: 'event'});
              }
            } catch (err) {}
          }",
          ns("ctx_paste")
        )
      },
      action = update_action_trigger(
        action_name = "paste_action",
        input_name = "ctx_paste"
      ),
      condition = function(board, target) target$type == "canvas",
      id = "paste"
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
      # The item has no id to name, so it sends the corner of its box like
      # "Add block" in the context menu, and the "+" menu hangs under it.
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            const box = target.getBoundingClientRect();
            Shiny.setInputValue(
              '%s',
              {target: true, at: {x: box.left, y: box.bottom}},
              {priority: 'event'}
            );
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
    extensions,
    ...,
    session = get_session()
  ) {
    dag <- dag_ext_result(board, extensions)

    graph_ready <- reactive(
      isTRUE(dag$proxy$session$input[[paste0(graph_id(), "-initialized")]]),
      label = "graph_ready"
    )

    badge <- reactive(
      {
        errors <- sum(lengths(conditions()$error))
        status <- reval_if(board$eval[[id]])

        log_trace("dag node badge [{id}]: eval={coal(status, 'NA')} errors={errors}")

        blockr.dock::block_status_badge(status, errors)
      },
      label = "badge"
    )

    drawn_badge <- reactiveVal(NULL)

    observeEvent(
      list(graph_ready(), badge()),
      {
        req(graph_ready())

        spec <- badge()

        # `NA` means the block is dormant: its status is not currently
        # computed (nothing renders its output), so leave the badge as-is
        # rather than clearing it when the block drops out of the eval set.
        if (isTRUE(is.na(spec)) || identical(spec, drawn_badge())) {
          return()
        }

        badges <- if (is.null(spec)) {
          list()
        } else {
          list(
            list(
              text = "",
              placement = "right-bottom",
              offsetX = -2,
              offsetY = -2,
              backgroundFill = spec$color,
              backgroundStroke = spec$ring_color,
              backgroundLineWidth = spec$ring,
              backgroundWidth = spec$size,
              backgroundHeight = spec$size,
              backgroundRadius = spec$size / 2
            )
          )
        }

        g6_update_nodes(
          dag$proxy,
          list(
            list(
              id = to_g6_node_id(id),
              style = list(badges = badges)
            )
          )
        )

        drawn_badge(spec)
      },
      label = "update_status_badge"
    )

    NULL
  }
}
