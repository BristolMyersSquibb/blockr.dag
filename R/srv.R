dag_ext_srv <- function(graph) {
  function(id, board, update, ...) {
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

        proxy <- init_g6(
          board = isolate(board$board),
          graph = graph,
          path = ctx_path,
          ctx = context_menu,
          session = session
        )

        # TBD: when adding new node, register node validation (see old API).
        # TBD: when adding new blocks call register_node_stack_link (see old API).

        add_edge_observer(input, board, proxy, update)

        update_observer(update, board, proxy)

        reactive(
          input[[paste0(graph_id(), "-state")]]
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
        add_links(upd$links$add, proxy)
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

      lnks <- board_links(board$board)

      inputs <- setdiff(
        block_inputs(trg),
        lnks[lnks$to == new_edg$target]$input
      )

      if (is.na(block_arity(trg))) {

        opts <- list(create = TRUE)

      } else if (length(inputs)) {

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
              choices = inputs,
              options = opts
            ),
            textInput(
              ns("added_edge_id"),
              label = "Link ID",
              value = rand_names(board_link_ids(board$board))
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
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
