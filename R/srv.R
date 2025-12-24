dag_ext_srv <- function(graph) {
  function(id, board, update, dock, ...) {
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
          graph = graph,
          path = ctx_path,
          ctx = context_menu,
          tools = toolbar,
          session = session
        )

        proxy <- blockr_g6_proxy(session)

        action_args <- c(
          list(board = board, update = update, dock = dock),
          dot_args,
          list(domain = session)
        )

        do.call(
          context_menu_entry_action,
          c(list(context_menu), action_args)
        )

        do.call(
          toolbar_item_action,
          c(list(toolbar), action_args)
        )

        update_observer(update, board, proxy)

        setup_remove_elements_kbd()

        do.call(
          remove_selected_action(
            reactive(req(input[[paste0(graph_id(), "-batch_delete")]])),
            as_module = FALSE
          ),
          action_args
        )

        do.call(
          draw_link_action(
            reactive(
              req(input$added_edge, input$added_edge$targetType != "canvas")
            ),
            as_module = FALSE
          ),
          action_args
        )

        do.call(
          blockr.dock::append_block_action(
            reactive(
              req(
                input$added_edge$source,
                input$added_edge$targetType == "canvas"
              )
            ),
            as_module = FALSE
          ),
          action_args
        )

        observeEvent(
          input[[paste0(graph_id(), "-selected_node")]],
          {
            sel <- input[[paste0(graph_id(), "-selected_node")]]
            evt <- attr(sel, "eventType")

            if (length(sel) == 1L && !identical(evt, "brush_select")) {
              blockr.dock::show_panel(
                from_g6_node_id(sel),
                board$board,
                dock
              )
            }
          }
        )

        empty_state_observer(board, session)

        list(
          state = list(
            graph = reactive(
              input[[paste0(graph_id(), "-state")]]
            )
          ),
          proxy = proxy
        )
      }
    )
  }
}

update_observer <- function(update, board, proxy) {
  observeEvent(
    update(),
    {
      upd <- update()

      if (length(upd$blocks$add)) {
        add_nodes(upd$blocks$add, board$board, proxy)
        # Reposition new node after layout settles
        node_ids <- to_g6_node_id(names(upd$blocks$add))
        later::later(function() {
          updates <- lapply(node_ids, function(id) {
            list(id = id, style = list(x = 400, y = 400))
          })
          g6_update_nodes(proxy, updates)
        }, delay = 0.3)
      }

      if (length(upd$blocks$mod)) {
        update_nodes(upd$blocks$mod, board$board, proxy)
      }

      if (length(upd$links$add)) {
        add_edges(upd$links$add, proxy)
      }

      if (length(upd$stacks$add)) {
        add_combos(upd$stacks$add, proxy)
      }

      if (length(upd$stacks$mod)) {
        update_combos(upd$stacks$mod, board$board, proxy)
      }

      if (length(upd$stacks$rm)) {
        remove_combos(upd$stacks$rm, proxy)
      }

      if (length(upd$links$rm)) {
        remove_edges(upd$links$rm, proxy)
      }

      if (length(upd$blocks$rm)) {
        remove_nodes(upd$blocks$rm, proxy)
      }
    }
  )
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
    }
  )
}
