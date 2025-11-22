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

        toolbar <- toolbar_items(initial_board)

        g6_graph <- init_g6(
          board = initial_board,
          graph = graph,
          path = ctx_path,
          ctx = context_menu,
          tools = toolbar,
          session = session
        )

        proxy <- blockr_g6_proxy(session)

        context_menu_entry_action(
          context_menu,
          board = board,
          update = update,
          proxy = proxy
        )

        toolbar_item_action(
          toolbar,
          board = board,
          update = update,
          proxy = proxy
        )

        update_observer(update, board, proxy)

        stack_conversion_observer(board, g6_graph, update)

        batch_delete_observer(board, update, proxy)

        add_edge_observer(board, proxy, update)

        observeEvent(
          input[[paste0(graph_id(), "-selected_node")]],
          {
            sel <- input[[paste0(graph_id(), "-selected_node")]]
            if (length(sel) == 1L) {
              blockr.dock::show_panel(
                from_g6_node_id(sel),
                board$board,
                dock
              )
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

stack_conversion_observer <- function(board, graph, update) {
  observeEvent(
    TRUE,
    {
      stacks <- board_stacks(board$board)
      has_col <- lgl_ply(stacks, is_dag_stack)

      if (any(!has_col)) {
        cmbs <- graph[["x"]][["data"]][["combos"]]

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
}

batch_delete_observer <- function(board, update, proxy) {

  setup_remove_elements_kbd()

  session <- proxy$session
  input <- session$input

  remove_selected <- remove_selected_action(
    reactive(req(input[[paste0(graph_id(), "-batch_delete")]])),
    as_module = FALSE
  )

  remove_selected(board, update, session)
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

add_edge_observer <- function(board, proxy, update) {

  session <- proxy$session
  input <- session$input

  draw_link <- draw_link_action(
    reactive(req(input$added_edge$targetType != "canvas")),
    as_module = FALSE
  )

  draw_link(board, update, session)

  # Drag edge on canvas to create new block
  append_action <- append_block_action(
    reactive(
      {
        req(input$added_edge$targetType == "canvas")
        from_g6_node_id(input$added_edge$source)
      }
    ),
    as_module = FALSE
  )

  append_action(board, update, session)
}
