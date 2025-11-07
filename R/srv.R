dag_ext_srv <- function(graph) {
  function(id, board, update, dock, ...) {
    moduleServer(
      id,
      function(input, output, session) {
        context_menu <- context_menu_items(
          isolate(board$board)
        )

        ctx_path <- session$registerDataObj(
          name = "context-menu-items",
          data = list(),
          filterFunc = function(data, req) {
            body_bytes <- req$rook.input$read(-1)
            res <- jsonlite::toJSON(
              build_context_menu(
                context_menu,
                board = isolate(board$board),
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
          input = input,
          output = output,
          session = session,
          board = board,
          update = update
        )

        g6_graph <- init_g6(
          board = isolate(board$board),
          graph = graph,
          path = ctx_path,
          ctx = context_menu,
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

        add_edge_observer(input, board, proxy, update)

        observeEvent(
          input[[paste0(graph_id(), "-selected_node")]],
          show_panel(
            input[[paste0(graph_id(), "-selected_node")]],
            board$board,
            dock
          )
        )

        reactive(
          input[[paste0(graph_id(), "-state")]]
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

add_edge_observer <- function(input, board, proxy, update) {
  ns <- proxy$session$ns

  observeEvent(
    input$added_edge,
    {
      new_edg <- input$added_edge

      trg <- board_blocks(board$board)[[new_edg$target]]
      cur <- lnks[lnks$to == new_edg$target]$input

      lnks <- board_links(board$board)
      inps <- setdiff(block_inputs(trg), cur)

      if (is.na(block_arity(trg))) {

        num <- suppressWarnings(as.integer(cur))
        nna <- is.na(num)

        if (all(nna)) {
          inps <- c(inps, "1")
        } else {
          num <- num[!nna]
          inps <- c(inps, as.character(min(setdiff(seq_len(max(num)), num))))
        }

        opts <- list(create = TRUE)

      } else if (length(inps)) {
        opts <- list()
      } else {
        notify(
          "No inputs are available for block {new_edg$target}.",
          type = "warning"
        )

        remove_edges(new_edg$id, proxy)

        return()
      }

      showModal(
        modalDialog(
          title = "Target input",
          tagList(
            selectizeInput(
              ns("added_edge_input"),
              "Block input",
              choices = inps,
              options = opts
            ),
            textInput(
              ns("added_edge_id"),
              label = "Link ID",
              value = rand_names(board_link_ids(board$board))
            )
          ),
          footer = tagList(
            actionButton(
              ns("added_edge_cancel"),
              "Cancel"
            ),
            actionButton(
              ns("added_edge_confirm"),
              "Select input"
            )
          )
        )
      )
    }
  )

  observeEvent(
    input$added_edge_confirm,
    {
      remove_edges(input$added_edge$id, proxy)
      removeModal()
    }
  )

  observeEvent(
    input$added_edge_confirm,
    {
      new_edg <- input$added_edge

      req(
        new_edg$source,
        new_edg$target,
        new_edg$id,
        input$added_edge_input,
        input$added_edge_id
      )

      if (new_edg$id %in% board_link_ids(board$board)) {
        notify(
          "Cannot add edge with existing ID {new_edg$id}.",
          type = "warning"
        )

        return()
      }

      remove_edges(new_edg$id, proxy)

      new_lnk <- new_link(
        from = new_edg$source,
        to = new_edg$target,
        input = input$added_edge_input
      )

      new_lnk <- as_links(set_names(list(new_lnk), input$added_edge_id))

      update(list(links = list(add = new_lnk)))

      removeModal()
    }
  )

  invisible()
}
