#' DAG extension
#'
#' Visualizes the DAG (directed acyclic graph) underlying a board and provides
#' UI elemtnts to manipulate the board,
#'
#' @param graph A `graph` object (or `NULL`)
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @rdname dag
#' @export
new_dag_extension <- function(graph = NULL, ...) {
  blockr.dock::new_dock_extension(
    dag_ext_srv(graph),
    dag_ext_ui,
    name = "DAG",
    class = "dag_extension",
    ...
  )
}

#' @export
context_menu_items.dag_extension <- function(x) {
  list(
    new_context_menu_entry(
      name = "Create edge",
      js = "(value, target, current) => {
        if (current.id === undefined) return;
        const graphId = `${target.closest('.g6').id}`;
        const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
        graph.updateBehavior({
          key: 'create-edge', // Specify the behavior to update
          enable: true,
        });
        // Select node
        graph.setElementState(current.id, 'selected');
        // Disable drag node as it is incompatible with edge creation
        graph.updateBehavior({ key: 'drag-element', enable: false });
        graph.updateBehavior({ key: 'drag-element-force', enable: false });
      }",
      action = NULL, # handled by the 'create-edge' behavior
      condition = function(board, target) {
        target$type == "node"
      },
      id = "create_edge"
    ),
    new_context_menu_entry(
      name = "Remove block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_node")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_node,
          update(list(blocks = list(rm = input$remove_node)))
        )
      },
      condition = function(board, target) {
        target$type == "node"
      }
    ),
    new_context_menu_entry(
      name = "Remove edge",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
            const graphId = `${target.closest('.g6').id}`;
            const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
            graph.removeEdgeData([current.id]);
            graph.draw();
          }",
          ns("remove_edge")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_edge,
          update(list(links = list(rm = input$remove_edge)))
        )
      },
      condition = function(board, target) {
        target$type == "edge"
      }
    ),
    new_context_menu_entry(
      name = "Append block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', current.id, {priority: 'event'});
          }",
          ns("append_block")
        )
      },
      action = function(input, output, session, board, update) {
        ns <- session$ns
        blk <- reactiveVal()

        observeEvent(
          input$append_block,
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

            choices <- block_inputs(new_blk)

            arity <- block_arity(new_blk)

            if (is.na(arity)) {
              opts <- list(create = TRUE)
              choices <- c(choices, "1")
            } else if (identical(arity, 0L)) {
              notify(
                "No inputs are available for the selected block.",
                type = "warning"
              )

              return()
            } else {
              opts <- list()
            }

            updateSelectizeInput(
              session,
              "append_block_input",
              choices = choices,
              options = opts
            )

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
              from = input$append_block,
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
      },
      condition = function(board, target) {
        target$type == "node"
      }
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
      action = function(input, output, session, board, update) {
        ns <- session$ns

        observeEvent(
          input$create_stack,
          {
            showModal(
              stack_modal(
                ns = ns,
                board = board$board,
                mode = "create"
              )
            )
          }
        )

        observeEvent(
          input$stack_confirm,
          {
            stack_id <- input$stack_id
            stack_name <- input$stack_name
            stack_color <- input$stack_color
            selected_blocks <- input$stack_block_selection

            if (!nchar(stack_id) ||
                  stack_id %in% board_stack_ids(board$board)) {
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
      },
      condition = function(board, target) {
        target$type == "canvas"
      }
    ),
    new_context_menu_entry(
      name = "Remove stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_stack")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_stack,
          {
            update(list(stacks = list(rm = input$remove_stack)))
          }
        )
      },
      condition = function(board, target) {
        target$type == "combo"
      }
    ),
    new_context_menu_entry(
      name = "Edit stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id, {priority: 'event'});
          }",
          ns("edit_stack")
        )
      },
      action = function(input, output, session, board, update) {
        ns <- session$ns
        observeEvent(
          input$edit_stack,
          {
            stack <- board_stacks(board$board)[[input$edit_stack]]

            showModal(
              stack_modal(
                ns = ns,
                board = board$board,
                mode = "edit",
                stack = stack,
                stack_id = input$edit_stack
              )
            )
          }
        )

        observeEvent(
          input$edit_stack_confirm,
          {
            id <- input$edit_stack
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
      },
      condition = function(board, target) {
        target$type == "combo"
      }
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
      action = function(input, output, session, board, update) {
        ns <- session$ns
        blk <- reactiveVal()

        observeEvent(
          input$add_block,
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
      },
      condition = function(board, target) {
        target$type == "canvas"
      }
    )
  )
}

create_block_with_name <- function(reg_id, blk_nms, ...) {
  name_fun <- function(nms) {
    function(class) {
      last(make.unique(c(nms, default_block_name(class)), sep = " "))
    }
  }

  create_block(reg_id, ..., block_name = name_fun(blk_nms))
}

new_stack_name <- function(board) {
  existsing <- chr_ply(board_stacks(board), stack_name)
  last(make.unique(c(existsing, default_stack_name()), sep = " "))
}
