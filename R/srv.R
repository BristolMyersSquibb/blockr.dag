dag_ext_srv <- function(graph) {
  function(id, board, update, dock, ...) {
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

        context_menu_entry_action(
          context_menu,
          board = board,
          update = update,
          session = session
        )

        toolbar <- toolbar_items(initial_board)

        toolbar_item_action(
          toolbar,
          board = board,
          update = update,
          session = session
        )

        g6_graph <- init_g6(
          board = initial_board,
          graph = graph,
          path = ctx_path,
          ctx = context_menu,
          tools = toolbar,
          session = session
        )

        proxy <- blockr_g6_proxy(session)

        update_observer(update, board, proxy)

        observeEvent(
          TRUE,
          {
            stacks <- board_stacks(board$board)
            has_col <- lgl_ply(stacks, is_dag_stack)

            if (any(!has_col)) {
              cmbs <- g6_graph[["x"]][["data"]][["combos"]]

              cmbs <- set_names(
                chr_xtr(lst_xtr(cmbs, "style"), "fill"),
                chr_xtr(cmbs, "id")
              )

              log_debug(
                "converting stack{?s} {names(stacks)[!has_col]} to ",
                "type 'dag_stack'"
              )

              stack_upd <- Map(
                as_dag_stack,
                stacks[!has_col],
                cmbs[names(stacks)[!has_col]]
              )

              update(list(stacks = list(mod = as_stacks(stack_upd))))
            } else {
              log_debug("no conversions to type 'dag_stack' required")
            }
          },
          once = TRUE
        )

        # TBD: when adding new node, register node validation (see old API).
        # TBD: when adding new blocks call register_node_stack_link (see old
        # API).

        batch_delete_observer(input, update)

        add_edge_observer(board, proxy, update, session)

        observeEvent(
          input[[paste0(graph_id(), "-selected_node")]],
          {
            sel <- input[[paste0(graph_id(), "-selected_node")]]
            if (length(sel) == 1L) {
              blockr.dock::show_panel(sel, board$board, dock)
            }
          }
        )

        list(
          graph = reactive(
            input[[paste0(graph_id(), "-state")]]
          )
        )
      }
    )
  }
}

batch_delete_observer <- function(input, update) {
  # Handle remove any selected element with backspace key
  setup_remove_elements_kbd()

  observeEvent(input[[paste0(graph_id(), "-batch_delete")]], {
    update(
      list(
        blocks = list(rm = input[[paste0(graph_id(), "-selected_node")]]),
        links = list(rm = input[[paste0(graph_id(), "-selected_edge")]]),
        stacks = list(rm = input[[paste0(graph_id(), "-selected_combo")]])
      )
    )
  })
}

update_observer <- function(update, board, proxy) {
  observeEvent(
    update(),
    {
      upd <- update()

      if (length(upd$stacks$rm)) {
        remove_combos(upd$stacks$rm, proxy)
      }

      if (length(upd$links$rm)) {
        remove_edges(upd$links$rm, proxy)
      }

      if (length(upd$blocks$rm)) {
        remove_nodes(upd$blocks$rm, proxy)
      }

      if (length(upd$blocks$add)) {
        add_nodes(upd$blocks$add, board$board, proxy)
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
    }
  )
}

add_edge_observer <- function(board, proxy, update, session) {

  input <- session$input

  observeEvent(
    req(input$added_edge$targetType != "canvas"),
    {
      new <- input$added_edge

      blocks <- board_blocks(board$board)

      inps <- block_input_select(
        blocks[[new$target]],
        new$target,
        board_links(board$board),
        mode = "inputs"
      )

      if (length(inps) == 0L) {
        notify(
          "No inputs are available for block {new$target}.",
          type = "warning"
        )

        remove_edges(new$id, proxy)

        return()
      }

      remove_edges(new$id, proxy)

      new_lnk <- new_link(
        from = new$source,
        to = new$target,
        input = inps[1L]
      )

      update(list(links = list(add = as_links(new_lnk))))
    }
  )

  # Drag edge on canvas to create new block
  append_action <- append_block_action(
    reactive(
      {
        req(input$added_edge$targetType == "canvas")
        input$added_edge$source
      }
    )
  )

  append_action <- append_action(board, update)

  append_action(input, session$output, session)
}
