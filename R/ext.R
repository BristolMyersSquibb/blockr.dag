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
        observeEvent(
          input$append_block,
          {
            showModal(
              modalDialog(
                title = "Append new block",
                selectInput(
                  ns("new_block"),
                  "Select block to add",
                  choices = list_blocks()
                ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(ns("confirm_append_block"), "Append Block")
                )
              )
            )
          })

          observeEvent(input$confirm_append_block, {
            removeModal()
            new_id <- rand_names(board_block_ids(board$board))
            new_blk <- create_block(input$new_block)
            new_blk <- as_blocks(set_names(list(new_blk), new_id))

            # TBD: build the input by checking the block inputs
            new_link <- as_links(
              new_link(
                from = input$append_block,
                to = new_id,
                input = "data" # replace by correct input
              )
            )

            update(
              list(
                blocks = list(add = new_blk),
                links = list(add = new_link)
              )
            )
          })
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
        observeEvent(
          input$create_stack,
          {

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
              modalDialog(
                title = "Add new block",
                tagList(
                  selectInput(
                    ns("new_block"),
                    label = "Select block to add",
                    choices = c("", list_blocks())
                  ),
                  textInput(
                    ns("block_name"),
                    label = "Block name",
                    placeholder = "Select block to generate default"
                  ),
                  textInput(
                    ns("block_id"),
                    label = "Block ID",
                    value = rand_names(board_block_ids(board$board))
                  )
                ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(ns("confirm_add_block"), "Add Block")
                )
              )
            )
          }
        )

        observeEvent(
          input$new_block,
          {
            req(input$new_block)

            new_blk <- create_block_with_name(
              input$new_block,
              chr_ply(board_blocks(board$board), block_name)
            )

            updateTextInput(
              session,
              "block_name",
              value = block_name(new_blk)
            )

            blk(new_blk)
          }
        )

        observeEvent(
          input$confirm_add_block,
          {
            id <- input$block_id
            bk <- blk()

            if (!nchar(id) || id %in% board_block_ids(board$board)) {
              notify(
                "Please choose valid block IDs.",
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

            if (!identical(input$block_name, block_name(bk))) {
              block_name(bk) <- input$block_name
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

  create_block(reg_id, ..., name = name_fun(blk_nms))
}
