dag_ext_srv <- function(positions) {
  function(id, board, update, actions, ...) {
    dot_args <- list(...)

    moduleServer(
      id,
      function(input, output, session) {
        initial_board <- isolate(board$board)

        context_menu <- context_menu_items(initial_board)

        ctx_path <- session$registerDataObj(
          name = "context-menu-items",
          data = list(),
          filterFunc = function(data, req) {
            body_bytes <- req$rook.input$read(-1)
            res <- jsonlite::toJSON(
              build_context_menu(
                context_menu,
                board = initial_board,
                target = jsonlite::fromJSON(rawToChar(body_bytes))
              )
            )
            httpResponse(
              content_type = "application/json",
              content = res
            )
          }
        )

        toolbar <- toolbar_items(initial_board)

        init_g6(
          board = initial_board,
          positions = positions,
          path = ctx_path,
          ctx = context_menu,
          tools = toolbar,
          session = session
        )

        proxy <- blockr_g6_proxy(session)

        ext_positions <- setup_positions_ctrl(positions, proxy)

        context_menu_entry_action(context_menu, actions, session)
        toolbar_item_action(toolbar, actions, session)

        setup_remove_elements_kbd()
        setup_copy_paste_kbd()

        actions_observers(actions, proxy)

        update_observer(update, board, proxy)

        observeEvent(
          input[[paste0(graph_id(), "-selected_node")]],
          {
            sel <- input[[paste0(graph_id(), "-selected_node")]]
            evt <- attr(sel, "eventType")

            if (length(sel) == 1L && !identical(evt, "brush_select")) {
              delta <- reveal_panel_delta(board$board, from_g6_node_id(sel))

              if (!is.null(delta)) {
                update(delta)
              }
            }
          },
          label = "selected_node"
        )

        empty_state_observer(board, session)

        status_badge_observer(board, proxy, session)

        list(
          state = list(
            positions = ext_positions
          ),
          proxy = proxy
        )
      }
    )
  }
}

# Reveal a block's panel in the *current* view: focus it if the view already
# holds it, otherwise add it there -- never switch to another view that happens
# to hold it. Builds a blockr.dock views-delta for `update()`; NULL when no view
# is active (nothing to reveal into).
reveal_panel_delta <- function(board, block) {

  views <- blockr.dock::board_views(board)
  view <- blockr.dock::active_view(views)

  if (is.null(view)) {
    return(NULL)
  }

  pid <- as.character(blockr.dock::as_block_panel_id(block))

  ops <- if (pid %in% blockr.dock::view_members(views[[view]])) {
    list(select = pid)
  } else {
    list(add = set_names(list(list()), pid), select = pid)
  }

  list(views = list(mod = set_names(list(ops), view)))
}

# Bidirectional sync for the externally-controllable `positions` handle.
#
# Reads: the returned `reactiveVal` tracks live user drags, projected from the
# `graph-state` input and debounced (that input fires on every g6 redraw, so
# many times per drag).
#
# Writes: the board update lifecycle writes external sets into this same
# `reactiveVal` (see `blockr.dock::apply_extensions_mod()`); an observer then
# pushes the changed nodes to the client with `g6_update_nodes()`.
#
# The echo loop (external set -> client move -> `graph-state` -> sync back) is
# broken on both ends by `positions_equal()` / `positions_diff()` (compared up
# to whole-pixel rounding): a value the client already shows is neither
# re-stored nor re-pushed, so the `reactiveVal` settles.
setup_positions_ctrl <- function(positions, proxy, session = get_session()) {
  input <- session$input

  rv <- reactiveVal(positions %||% list())

  state_input <- function() input[[paste0(graph_id(), "-state")]]
  initialized <- function() input[[paste0(graph_id(), "-initialized")]]

  client_positions <- debounce(
    reactive(project_positions(state_input())),
    millis = 250
  )

  # Client -> state: keep reads current with user drags.
  observeEvent(
    client_positions(),
    {
      req(initialized())
      live <- client_positions()
      if (!positions_equal(live, rv())) {
        rv(live)
      }
    },
    label = "positions_from_client"
  )

  # State -> client: external sets move the corresponding nodes.
  observeEvent(
    rv(),
    {
      req(initialized())
      to_push <- positions_diff(rv(), project_positions(state_input()))
      if (length(to_push)) {
        apply_node_positions(to_push, proxy)
      }
    },
    ignoreInit = TRUE,
    label = "positions_to_client"
  )

  rv
}

update_observer <- function(update, board, proxy) {
  observeEvent(
    update(),
    {
      upd <- update()

      if (length(upd$blocks$add)) {
        add_nodes(upd$blocks$add, board$board, proxy)
      }

      if (length(upd$blocks$mod)) {
        relabel_nodes(upd$blocks$mod, proxy)
      }

      if (length(upd$links$add)) {
        blocks <- board_blocks(board$board)
        # If new blocks are added in the same update as new links, for instance
        # during an append action, we need to consider the new blocks that are in
        # upd$blocks$add and not yet in the board. Otherwise, we can possibly end up
        # with non existing target/source nodes.
        if (length(upd$blocks$add)) {
          blocks <- c(blocks, upd$blocks$add)
        }
        add_edges(upd$links$add, blocks, proxy)
      }

      if (length(upd$stacks$add)) {
        add_combos(upd$stacks$add, proxy)
      }

      if (length(upd$stacks$mod)) {
        # `upd$stacks$mod` is a delta list under blockr.core's #175
        # contract; combos need full `stack` objects. See
        # `resolve_mod_deltas()` for the ordering rationale.
        update_combos(
          resolve_mod_deltas(
            upd$stacks$mod,
            board_stacks(board$board),
            blockr.core::update_stack,
            blockr.core::as_stacks
          ),
          board$board,
          proxy
        )
      }

      if (length(upd$stacks$rm)) {
        remove_combos(upd$stacks$rm, proxy)
      }

      if (length(upd$links$rm)) {
        remove_edges(upd$links$rm, proxy = proxy)
      }

      if (length(upd$blocks$rm)) {
        remove_nodes(upd$blocks$rm, proxy = proxy)
      }
    },
    label = "update_observer"
  )
}

actions_observers <- function(actions, proxy) {
  input <- proxy$session$input

  observeEvent(
    input[[paste0(graph_id(), "-batch_delete")]],
    actions[["remove_selected_action"]](
      input[[paste0(graph_id(), "-batch_delete")]]
    ),
    label = "batch_delete"
  )

  observeEvent(
    req(input$added_edge$targetType != "canvas"),
    {
      actions[["draw_link_action"]](input$added_edge)
    },
    label = "draw_link"
  )

  observeEvent(
    input[[paste0(graph_id(), "-copy_selected")]],
    actions[["copy_selected_action"]](
      input[[paste0(graph_id(), "-copy_selected")]]
    ),
    label = "copy_selected"
  )

  observeEvent(
    input[[paste0(graph_id(), "-cut_selected")]],
    actions[["cut_selected_action"]](
      input[[paste0(graph_id(), "-cut_selected")]]
    ),
    label = "cut_selected"
  )

  observeEvent(
    input[[paste0(graph_id(), "-paste_clipboard")]],
    actions[["paste_action"]](
      input[[paste0(graph_id(), "-paste_clipboard")]]
    ),
    label = "paste_clipboard"
  )

  # Append/prepend from canvas drop
  observeEvent(
    req(input$added_edge$targetType == "canvas"),
    {
      edge <- input$added_edge
      req(edge$portType)

      switch(
        edge$portType,
        output = actions[["append_block_action"]](edge$source),
        input = actions[["prepend_block_action"]](edge$source)
      )
    },
    label = "canvas_drop"
  )

  # Append/prepend on port click: FIXME -> disabled due to critical issue
  # with input ports which cannot be clicked :)
  # observeEvent(
  #   input[[paste0(graph_id(), "-selected_port")]],
  #   {
  #     el <- input[[paste0(graph_id(), "-selected_port")]]
  #     req(el$type)

  #     switch(
  #       el$type,
  #       output = actions[["append_block_action"]](from_g6_node_id(el$node)),
  #       input = actions[["prepend_block_action"]](from_g6_node_id(el$node))
  #     )
  #   }
  # )
}

empty_state_observer <- function(board, session) {
  ns <- session$ns

  observeEvent(
    board$board,
    {
      has_blocks <- length(board_blocks(board$board)) > 0
      session$sendCustomMessage(
        "update-empty-state",
        list(
          id = ns("empty-state"),
          show = !has_blocks
        )
      )
    },
    label = "empty_state"
  )
}

# Paint DAG node status badges from a single board-level observer. Each block's
# status (`board$eval[[id]]`) transitively depends on the board-wide needed set,
# so a per-block observer re-derives and re-fires across every node whenever the
# eval set shifts -- O(n^2) during a construction storm, growing with the block
# count. Reading all specs once per flush, diffing against the last-drawn set
# and pushing only the changed nodes in one `g6_update_nodes()` call collapses
# that to a single reactive.
status_badge_observer <- function(board, proxy, session = get_session()) {

  graph_ready <- reactive(
    isTRUE(session$input[[paste0(graph_id(), "-initialized")]]),
    label = "graph_ready"
  )

  specs <- reactive(
    {
      ids <- names(board$blocks)

      if (!length(ids)) {
        return(set_names(list(), character()))
      }

      errors <- block_error_counts(board$conditions(), ids)

      set_names(
        lapply(
          ids,
          function(id) {
            blockr.dock::block_status_badge(
              reval_if(board$eval[[id]]),
              errors[[id]]
            )
          }
        ),
        ids
      )
    },
    label = "status_badges"
  )

  drawn <- reactiveVal(set_names(list(), character()))

  observeEvent(
    list(graph_ready(), specs()),
    {
      req(graph_ready())

      current <- specs()
      last <- drawn()

      changed <- Filter(
        function(id) {
          !isTRUE(is.na(current[[id]])) && !identical(current[[id]], last[[id]])
        },
        names(current)
      )

      if (!length(changed)) {
        return()
      }

      log_trace("dag node badges: repainting {length(changed)} node(s)")

      g6_update_nodes(
        proxy,
        lapply(changed, function(id) badge_node_config(id, current[[id]]))
      )

      for (id in changed) {
        last[[id]] <- current[[id]]
      }

      drawn(last)
    },
    label = "status_badges"
  )
}

# `spec` is a `blockr.dock::block_status_badge()` result: `NULL` clears the
# node's badge, a style list paints a coloured dot. `NA` (dormant) is filtered
# out before reaching here.
badge_node_config <- function(id, spec) {

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

  list(
    id = to_g6_node_id(id),
    style = list(badges = badges)
  )
}

block_error_counts <- function(conditions, ids) {

  errors <- conditions[
    conditions$phase != "status" & conditions$severity == "error", ,
    drop = FALSE
  ]

  errors <- errors[!duplicated(errors[c("block", "id")]), , drop = FALSE]

  as.list(table(factor(errors$block, levels = ids)))
}
