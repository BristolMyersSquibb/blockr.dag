new_function <- function(formals = NULL, body = NULL, env = parent.frame()) {

  res <- function() {}

  if (!is.null(formals)) {
    formals(res) <- formals
  }

  if (!is.null(body)) {
    body(res) <- body
  }

  environment(res) <- env

  res
}

new_action <- function(expr) {

  proc_calls <- function(x) {
    if (is.call(x) && identical(x[[1L]], as.symbol("{"))) {
      as.list(x)[-1L]
    } else {
      list(x)
    }
  }

  combine_exprs <- function(x) {
    do.call(
      as.call,
      list(c(as.name("{"), unlst(lapply(x, proc_calls)))),
      quote = TRUE
    )
  }

  body <- list( # nolint: object_usage_linter.
    quote(
      {
        if (is_string(trigger)) {
          trigr_q <- bquote(req(input[[.(trg)]]), list(trg = trigger))
          trigger <- reactive(trigr_q, quoted = TRUE)
        }
      }
    ),
    substitute(expr),
    quote(
      {
        invisible(NULL)
      }
    )
  )

  function(trigger, as_module = TRUE) {

    if (isTRUE(as_module)) {

      function(board, update, proxy) {
        new_function(
          alist(input = , output = , session = ),
          combine_exprs(body)
        )
      }

    } else {

      body <- c(
        quote(
          {
            session <- proxy$session
            input <- session$input
            output <- session$output
          }
        )
      )

      new_function(
        alist(board = , update = , proxy = ),
        combine_exprs(body)
      )
    }
  }
}

add_block_action <- new_action(
  {
    ns <- session$ns
    blk <- reactiveVal()

    observeEvent(
      trigger(),
      {
        blk(NULL)
        showModal(
          create_block_modal(
            mode = "add",
            ns = ns,
            board = board$board
          )
        )
      }
    )

    observeEvent(
      input$add_block_selection,
      {
        req(input$add_block_selection)

        new_blk <- create_block_with_name(
          input$add_block_selection,
          chr_ply(board_blocks(board$board), block_name)
        )

        updateTextInput(
          session,
          "add_block_name",
          value = block_name(new_blk)
        )

        blk(new_blk)
      }
    )

    observeEvent(
      input$add_block_confirm,
      {
        id <- input$add_block_id
        bk <- blk()

        if (!nchar(id) || id %in% board_block_ids(board$board)) {
          notify(
            "Please choose a valid block ID.",
            type = "warning",
            session = session
          )

          return()
        }

        if (!is_block(bk)) {
          notify(
            "Please choose a block type.",
            type = "warning",
            session = session
          )

          return()
        }

        if (!identical(input$add_block_name, block_name(bk))) {
          block_name(bk) <- input$add_block_name
        }

        bk <- as_blocks(set_names(list(bk), id))

        update(list(blocks = list(add = bk)))
        removeModal()
      }
    )
  }
)

append_block_action <- new_action(
  {
    ns <- session$ns
    blk <- reactiveVal()

    observeEvent(
      trigger(),
      {
        blk(NULL)
        showModal(
          create_block_modal(
            mode = "append",
            ns = ns,
            board = board$board
          )
        )
      }
    )

    observeEvent(
      input$append_block_selection,
      {
        req(input$append_block_selection)

        new_blk <- create_block_with_name(
          input$append_block_selection,
          chr_ply(board_blocks(board$board), block_name)
        )

        res <- block_input_select(
          new_blk,
          mode = "update",
          session = session,
          inputId = "append_block_input"
        )

        if (is.null(res)) {
          notify(
            "No inputs are available for the selected block.",
            type = "warning"
          )
          return()
        }

        updateTextInput(
          session,
          "append_block_name",
          value = block_name(new_blk)
        )

        blk(new_blk)
      }
    )

    observeEvent(
      input$append_block_confirm,
      {
        blk_id <- input$append_block_id
        lnk_id <- input$append_link_id

        if (!nchar(blk_id) || blk_id %in% board_block_ids(board$board)) {
          notify(
            "Please choose a valid block ID.",
            type = "warning",
            session = session
          )
          return()
        }

        if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
          notify(
            "Please choose a valid link ID.",
            type = "warning",
            session = session
          )
          return()
        }

        new_blk <- blk()

        if (!is_block(new_blk)) {
          notify(
            "Please choose a block type.",
            type = "warning",
            session = session
          )
          return()
        }

        if (!identical(input$append_block_name, block_name(new_blk))) {
          block_name(new_blk) <- input$append_block_name
        }

        new_blk <- as_blocks(set_names(list(new_blk), blk_id))

        new_lnk <- new_link(
          from = trigger(),
          to = blk_id,
          input = input$append_block_input
        )

        new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

        update(
          list(
            blocks = list(add = new_blk),
            links = list(add = new_lnk)
          )
        )

        removeModal()
      }
    )
  }
)

remove_block_action <- new_action(
  {
    observeEvent(
      trigger(),
      update(list(blocks = list(rm = trigger())))
    )
  }
)

add_link_action <- new_action(
  {
    observeEvent(
      trigger(),
      showModal(link_modal(session$ns, board$board, trigger()))
    )

    observeEvent(
      input$create_link,
      {
        req(input$create_link)

        res <- block_input_select(
          board_blocks(board$board)[[input$create_link]],
          mode = "update",
          session = session,
          inputId = "add_link_input"
        )

        if (is.null(res)) {
          notify(
            "No inputs are available for the selected block.",
            type = "warning"
          )
          return()
        }
      }
    )

    observeEvent(
      input$add_link_confirm,
      {
        lnk_id <- input$add_link_id

        if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
          notify(
            "Please choose a valid link ID.",
            type = "warning",
            session = session
          )

          return()
        }

        new_lnk <- new_link(
          from = trigger(),
          to = input$create_link,
          input = input$add_link_input
        )

        new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

        update(
          list(links = list(add = new_lnk))
        )

        removeModal()
      }
    )
  }
)

draw_link_action <- new_action(
  {
    observeEvent(
      trigger(),
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
  }
)

remove_link_action <- new_action(
  {
    observeEvent(
      trigger(),
      update(list(links = list(rm = trigger())))
    )
  }
)

add_stack_action <- new_action(
  {
    observeEvent(
      trigger(),
      showModal(
        stack_modal(ns = session$ns, board = board$board, mode = "create")
      )
    )

    observeEvent(
      input$stack_confirm,
      {
        stack_id <- input$stack_id
        stack_name <- input$stack_name
        stack_color <- input$stack_color
        selected_blocks <- input$stack_block_selection

        if (
          !nchar(stack_id) ||
            stack_id %in% board_stack_ids(board$board)
        ) {
          notify(
            "Please choose a valid stack ID.",
            type = "warning",
            session = session
          )

          return()
        }

        # Get blocks - use selected blocks if any, otherwise empty
        # character vector
        has_blocks <- length(selected_blocks) > 0 &&
          !is.null(selected_blocks) &&
          any(nchar(selected_blocks) > 0)
        block_ids <- if (has_blocks) {
          selected_blocks[nchar(selected_blocks) > 0]
        } else {
          character()
        }

        # Create stack name - use input if provided, otherwise
        # generate from ID
        if (is.null(stack_name) || !nchar(stack_name)) {
          stack_name <- id_to_sentence_case(stack_id)
        }

        # Create the stack
        new_stk <- new_dag_stack(
          blocks = block_ids,
          name = stack_name,
          color = stack_color
        )

        new_stk <- as_stacks(set_names(list(new_stk), stack_id))

        update(list(stacks = list(add = new_stk)))

        removeModal()
      }
    )
  }
)

edit_stack_action <- new_action(
  {
    ns <- session$ns

    observeEvent(
      trigger(),
      {
        stack <- board_stacks(board$board)[[trigger()]]

        showModal(
          stack_modal(
            ns = ns,
            board = board$board,
            mode = "edit",
            stack = stack,
            stack_id = trigger()
          )
        )
      }
    )

    observeEvent(
      input$edit_stack_confirm,
      {
        id <- trigger()
        stack <- board_stacks(board$board)[[id]]

        blocks <- input$edit_stack_blocks

        if (is.null(blocks)) {
          blocks <- character(0)
        }

        stack_blocks(stack) <- blocks
        stack_color(stack) <- input$edit_stack_color
        stack_name(stack) <- input$edit_stack_name

        stack <- as_stacks(set_names(list(stack), id))

        update(list(stacks = list(mod = stack)))

        removeModal()
      }
    )
  }
)

remove_stack_action <- new_action(
  {
    observeEvent(
      trigger(),
      update(list(stacks = list(rm = trigger())))
    )
  }
)

remove_selected_action <- new_action(
  {
    observeEvent(
      trigger(),
      {
        inp <- proxy$session$input
        update(
          list(
            blocks = list(rm = inp[[paste0(graph_id(), "-selected_node")]]),
            links = list(rm = inp[[paste0(graph_id(), "-selected_edge")]]),
            stacks = list(rm = inp[[paste0(graph_id(), "-selected_combo")]])
          )
        )
      }
    )
  }
)
